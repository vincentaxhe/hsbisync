module Exe where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket, catch, IOException)
import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.IO
    ( hClose, hFlush, hGetLine, hSetBinaryMode
    , hSetEncoding, utf8, stdout, stdin
    , hSetEcho, hSetBuffering, BufferMode(..)
    )
import System.Process (createProcess, proc, StdStream(..), CreateProcess(..))

import qualified Data.ByteString as BS

import State
    ( FileState(..), FileChange(..), parseStateJSON
    , currentState, lastState, appendAdded, appendDeleted, posixSecs
    )
import Sync (sync, SyncAction(..), resolveConflicts)
import Write
    ( HelperHandle(..), Response(..), sendCommand, readResponse, readJSON
    , hGetExactly, hPutFully, expectOK
    , writeFileVerified, hashBytes, removeFileSafe
    )
import Arg (Root(..))

-- ============================================================================
-- 统一入口
-- ============================================================================

runSync :: String -> Root -> Root -> IO ()
runSync profile root1 root2 = do
    putStrLn "==================================="
    putStrLn "  hsbisync - Bidirectional Sync"
    putStrLn "==================================="

    let (mHost1, path1) = case root1 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        (mHost2, path2) = case root2 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        mTag = Just profile

    -- 并行读取状态
    putStrLn "\nReading states ..."
    ((label1, (last1, cur1)), (label2, (last2, cur2))) <-
        concurrently (readSide mTag mHost1 path1) (readSide mTag mHost2 path2)

    putStrLn $ "  root1 (" ++ label1 ++ "): " ++ show (length cur1) ++ " files (last: " ++ show (length last1) ++ ")"
    putStrLn $ "  root2 (" ++ label2 ++ "): " ++ show (length cur2) ++ " files (last: " ++ show (length last2) ++ ")"

    -- 计算 + 消解形式冲突
    let (pairs1, pairs2) = sync (last1, last2) (cur1, cur2)
        (_, pairs1', pairs2') = resolveConflicts pairs1 pairs2

    showSummary pairs1' pairs2'

    let hasWork = any (isJust . snd) pairs1' || any (isJust . snd) pairs2'
    if not hasWork then putStrLn "\nNothing to do."
    else do
        putStr "\nProceed? [y/N] "
        hFlush stdout
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        answer <- getChar
        hSetEcho stdin True
        let yes = toLower answer == 'y'
        putStrLn [answer]
        if not yes
            then putStrLn "Aborted."
            else do
                -- 执行
                bracket (maybe (return Nothing) (\h -> Just <$> spawnHelper h path1 profile) mHost1) (maybe (return ()) (\hh -> destroyHelper hh)) $ \mHh1 ->
                  bracket (maybe (return Nothing) (\h -> Just <$> spawnHelper h path2 profile) mHost2) (maybe (return ()) (\hh -> destroyHelper hh)) $ \mHh2 -> do
                    let side1 = case mHh1 of Just hh -> Rmt hh; Nothing -> Lcl path1
                        side2 = case mHh2 of Just hh -> Rmt hh; Nothing -> Lcl path2
                    execute mTag side1 side2 pairs1'
                    execute mTag side2 side1 pairs2'

    putStrLn "\nSync complete."

-- ============================================================================
-- 执行端类型
-- ============================================================================

data Side = Lcl FilePath | Rmt HelperHandle

-- ============================================================================
-- 状态读取
-- ============================================================================

readSide :: Maybe String -> Maybe String -> FilePath -> IO (String, ([FileState], [FileState]))
readSide mTag mHost path =
    case mHost of
        Nothing -> do
            st <- (,) <$> lastState path mTag <*> currentState path
            return ("local", st)
        Just host -> do
            hh <- spawnHelper host path (fromMaybe "" mTag)
            st <- (,) <$> getHelperState hh "GETLAST" <*> getHelperState hh "GETSTATE"
            destroyHelper hh
            return ("ssh " ++ host, st)

-- ============================================================================
-- Helper 管理
-- ============================================================================

spawnHelper :: String -> FilePath -> String -> IO HelperHandle
spawnHelper host path profileName = do
    myExe <- getExecutablePath
    let baseArgs = ["--helper", "--passcode=dontrunyourself", "--root=" ++ path]
        tagArgs  = if null profileName then [] else ["--tag=" ++ profileName]
        args     = baseArgs ++ tagArgs
        cmdArgs = (proc "ssh" (host : myExe : args))
            { std_in  = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    (Just hin, Just hout, _, _) <- createProcess cmdArgs
    hSetEncoding hout utf8
    hSetEncoding hin  utf8
    _ <- hGetLine hout
    return $ HelperHandle hin hout

destroyHelper :: HelperHandle -> IO ()
destroyHelper hh = do
    ignoreIO $ sendCommand hh "QUIT"
    ignoreIO $ hClose (hhIn hh)
    ignoreIO $ hClose (hhOut hh)

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = return ()

getHelperState :: HelperHandle -> String -> IO [FileState]
getHelperState hh cmd = do
    result <- readJSON hh cmd
    case result of
        Left err -> error $ cmd ++ " failed: " ++ err
        Right bs -> return $ parseStateJSON bs

-- ============================================================================
-- 执行操作
-- ============================================================================

-- | 执行一侧的所有操作
execute :: Maybe String -> Side -> Side -> [(FileChange, Maybe SyncAction)] -> IO ()
execute mTag self other pairs = do
    let receives   = [ (filename fs, mode fs, posixSecs (mtime fs))
                     | (Added     fs _ _, Just Receive)       <- pairs ]
        removes    = [ (filename fs, d1, d2)
                     | (Del   fs d1 d2, Just Remove)      <- pairs ]
        recordAdds = [ filename fs
                     | (Noop fs _ _, Just RecordAdd)     <- pairs ]
        recordDels = [ filename fs
                     | (Del   fs _ _, Just RecordDelete)  <- pairs ]
    unless (null removes) $ do
        putStrLn "\nDeleting..."
        forM_ removes $ \(relPath, _, _) -> do
            putStrLn $ "  rm " ++ relPath
            delEnd self relPath
            appendEnd self  mTag "DELETE" relPath
            appendEnd other mTag "DELETE" relPath
    unless (null receives) $ do
        putStrLn "\nReceiving..."
        forM_ receives $ \(relPath, srcMode, srcMtime) -> do
            putStrLn $ "  copy " ++ relPath
            copyEndToEnd other self mTag relPath srcMode srcMtime
    unless (null recordAdds) $ do
        putStrLn "\nRecording as added..."
        forM_ recordAdds $ \relPath -> do
            putStrLn $ "  add " ++ relPath
            appendEnd self  mTag "ADD" relPath
            appendEnd other mTag "ADD" relPath
    unless (null recordDels) $ do
        putStrLn "\nRecording as deleted..."
        forM_ recordDels $ \relPath -> do
            putStrLn $ "  del " ++ relPath
            appendEnd self  mTag "DELETE" relPath
            appendEnd other mTag "DELETE" relPath

-- ============================================================================
-- End 操作
-- ============================================================================

delEnd :: Side -> FilePath -> IO ()
delEnd (Lcl root) relPath = removeFileSafe (root </> relPath)
delEnd (Rmt hh)  relPath = do
    res <- expectOK hh ("UNLINK " ++ relPath)
    case res of
        Left err -> error $ "FATAL: remote UNLINK failed: " ++ err
        Right _  -> return ()

appendEnd :: Side -> Maybe String -> String -> FilePath -> IO ()
appendEnd (Lcl root) mTag _cmd relPath =
    case _cmd of
        "DELETE" -> appendDeleted root mTag relPath
        "ADD"    -> appendAdded root mTag relPath
        _        -> return ()
appendEnd (Rmt hh) _mTag cmd relPath = do
    res <- expectOK hh (cmd ++ " " ++ relPath)
    case res of
        Left err -> error $ "FATAL: remote " ++ cmd ++ " failed: " ++ err
        Right _  -> return ()

-- | 跨端复制文件：传输 + 校验（rename 之前） + 记录。
--   校验在目标端 rename 之前完成——不通过则目标文件完全不受影响。
copyEndToEnd :: Side -> Side -> Maybe String -> FilePath -> Int -> Integer -> IO ()
copyEndToEnd src dst mTag relPath srcMode srcMtime = do
    result <- transferContent src dst relPath srcMode srcMtime
    case result of
        Left err -> putStrLn $ "  " ++ err
        Right () -> do
            putStrLn "    verified OK"
            appendEnd dst mTag "ADD" relPath
            appendEnd src mTag "ADD" relPath

-- | 执行文件传输。本地目标使用 'writeFileVerified'（校验通过才 rename），
--   远程目标使用扩展的 RECV 协议（helper 端做同样的事）。
transferContent :: Side -> Side -> FilePath -> Int -> Integer -> IO (Either String ())
transferContent (Lcl srcRoot) (Lcl dstRoot) relPath srcMode srcMtime = do
    let s = srcRoot </> relPath
        d = dstRoot </> relPath
    content <- BS.readFile s
    let expectedHash = hashBytes content
    writeFileVerified d content srcMode srcMtime expectedHash

transferContent (Rmt hhSrc) (Lcl dstRoot) relPath srcMode srcMtime = do
    let d = dstRoot </> relPath
    expectedHash <- hashRemote hhSrc relPath       -- 先拿到期望哈希
    sendCommand hhSrc ("SEND " ++ relPath)
    sz <- readSendSize hhSrc
    hSetBinaryMode (hhOut hhSrc) True
    content <- hGetExactly (hhOut hhSrc) sz
    writeFileVerified d content srcMode srcMtime expectedHash

transferContent (Lcl srcRoot) (Rmt hhDst) relPath srcMode srcMtime = do
    let s = srcRoot </> relPath
    content <- BS.readFile s
    let expectedHash = hashBytes content
    recvVerified hhDst (BS.length content) relPath srcMode srcMtime expectedHash content

transferContent (Rmt hhSrc) (Rmt hhDst) relPath srcMode srcMtime = do
    expectedHash <- hashRemote hhSrc relPath
    sendCommand hhSrc ("SEND " ++ relPath)
    sz <- readSendSize hhSrc
    hSetBinaryMode (hhOut hhSrc) True
    content <- hGetExactly (hhOut hhSrc) sz
    recvVerified hhDst sz relPath srcMode srcMtime expectedHash content

-- | 向远端 helper 发送 RECV 命令（扩展协议：带 mode/mtime/expectedHash），
--   传输数据后等待 helper 端完成写临时文件→设属性→校验→rename 并返回结果。
recvVerified :: HelperHandle -> Int -> FilePath -> Int -> Integer -> String -> BS.ByteString -> IO (Either String ())
recvVerified hh sz relPath mode_ mtime_ expectedHash content = do
    let cmd = "RECV " ++ show sz ++ "," ++ relPath
              ++ " " ++ show mode_ ++ " " ++ show mtime_ ++ " " ++ expectedHash
    sendCommand hh cmd
    resp <- readResponse hh                     -- 第一步：helper 是否准备接收
    case resp of
        RErr msg -> return $ Left ("RECV: " ++ msg)
        ROk _ -> do
            hSetBinaryMode (hhIn hh) True
            hPutFully (hhIn hh) content
            hFlush (hhIn hh)
            resp2 <- readResponse hh            -- 第二步：校验 + rename 结果
            case resp2 of
                ROk _    -> return $ Right ()
                RErr msg -> return $ Left ("verify: " ++ msg)

-- ============================================================================
-- Helper 协议辅助
-- ============================================================================

readSendSize :: HelperHandle -> IO Int
readSendSize hh = do
    line <- hGetLine (hhOut hh)
    case break (== ' ') line of
        ("OK", rest) -> case reads (dropWhile (== ' ') rest) of
            [(n, "")] -> return n
            _         -> error $ "readSendSize: invalid size: " ++ line
        _ -> error $ "readSendSize: expected OK, got: " ++ line

hashRemote :: HelperHandle -> FilePath -> IO String
hashRemote hh relPath = do
    res <- expectOK hh ("HASH " ++ relPath)
    case res of
        Right s  -> return s
        Left err -> error $ "hashRemote failed: " ++ err

-- ============================================================================
-- 其他
-- ============================================================================

showSummary :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)] -> IO ()
showSummary pairs1 pairs2 = do
    let add1 = [ (f, d1, d2) | (Added    fs d1 d2, Just Receive) <- pairs1, let f = filename fs ]
        del1 = [ (f, d1, d2) | (Del  fs d1 d2, Just Remove)  <- pairs1, let f = filename fs ]
        add2 = [ (f, d1, d2) | (Added    fs d1 d2, Just Receive) <- pairs2, let f = filename fs ]
        del2 = [ (f, d1, d2) | (Del  fs d1 d2, Just Remove)  <- pairs2, let f = filename fs ]
        recAdds = [ f | (Noop fs _ _, Just RecordAdd)    <- pairs1, let f = filename fs ]
        recDels = [ f | (Del   fs _ _, Just RecordDelete) <- pairs1, let f = filename fs ]
        conf = nub [ (f, d1, d2) | (Conflict fs d1 d2, Just Unsolvable) <- pairs1 ++ pairs2, let f = filename fs ]

    when (not (null del1)) $ do
        putStrLn "\nroot1 will delete:"
        forM_ del1 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null add1)) $ do
        putStrLn "\nroot1 will receive (root2 -> root1):"
        forM_ add1 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null del2)) $ do
        putStrLn "\nroot2 will delete:"
        forM_ del2 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null add2)) $ do
        putStrLn "\nroot2 will receive (root1 -> root2):"
        forM_ add2 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null recAdds)) $ do
        putStrLn "\nAlready in sync (will record as added):"
        forM_ recAdds $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null recDels)) $ do
        putStrLn "\nAlready deleted on both sides (will record as deleted):"
        forM_ recDels $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null conf)) $ do
        putStrLn "\nConflicts (will skip):"
        forM_ conf $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    let total = length add1 + length del1 + length add2 + length del2 + length recAdds + length recDels
    putStrLn $ "\nTotal: " ++ show total ++ " operations, " ++ show (length conf) ++ " conflicts"