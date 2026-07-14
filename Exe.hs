module Exe where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket, catch, IOException)
import Control.Monad (forM_, unless, when, void)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, removeFile
    , copyFile
    )
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import System.IO
    ( hClose, hFlush, hGetLine, hSetBinaryMode
    , hSetEncoding, utf8, stdout
    )
import System.Posix.Files (setFileTimes, setFileMode)
import System.Posix.Types (EpochTime)
import System.Process (createProcess, proc, StdStream(..), CreateProcess(..))

import qualified Data.ByteString as BS
import Crypto.Hash (hash, SHA512, Digest)

import State
    ( FileState(..), FileChange(..), parseStateJSON
    , currentState, lastState, appendAdded, appendDeleted, posixSecs
    )
import Sync (sync, SyncAction(..), resolveConflicts)
import Write (HelperHandle(..), sendCommand, readJSON, hGetExactly, hPutFully, expectOK)
import Arg (Root(..))

-- ============================================================================
-- 统一入口
-- ============================================================================

runSync :: Root -> Root -> IO ()
runSync root1 root2 = do
    putStrLn "==================================="
    putStrLn "  hsbisync - Bidirectional Sync"
    putStrLn "==================================="

    let (mHost1, path1) = case root1 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        (mHost2, path2) = case root2 of Local p -> (Nothing, p); Remote h p -> (Just h, p)

    -- 并行读取状态
    putStrLn "\nReading states ..."
    ((label1, (last1, cur1)), (label2, (last2, cur2))) <-
        concurrently (readSide mHost1 path1) (readSide mHost2 path2)

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
        answer <- getLine
        unless (map toLower answer `elem` ["y", "yes"]) $ do
            putStrLn "Aborted."
            return ()
        -- 执行
        bracket (maybe (return Nothing) (\h -> Just <$> spawnHelper h path1) mHost1) (maybe (return ()) (\hh -> destroyHelper hh)) $ \mHh1 ->
          bracket (maybe (return Nothing) (\h -> Just <$> spawnHelper h path2) mHost2) (maybe (return ()) (\hh -> destroyHelper hh)) $ \mHh2 -> do
            let side1 = case mHh1 of Just hh -> Rmt hh; Nothing -> Lcl path1
                side2 = case mHh2 of Just hh -> Rmt hh; Nothing -> Lcl path2
            execute side1 side2 pairs1'
            execute side2 side1 pairs2'

    putStrLn "\nSync complete."

-- ============================================================================
-- 执行端类型
-- ============================================================================

data Side = Lcl FilePath | Rmt HelperHandle

-- ============================================================================
-- 状态读取
-- ============================================================================

readSide :: Maybe String -> FilePath -> IO (String, ([FileState], [FileState]))
readSide mHost path =
    case mHost of
        Nothing -> do
            st <- (,) <$> lastState path Nothing <*> currentState path
            return ("local", st)
        Just host -> do
            hh <- spawnHelper host path
            st <- (,) <$> getHelperState hh "GETLAST" <*> getHelperState hh "GETSTATE"
            destroyHelper hh
            return ("ssh " ++ host, st)

-- ============================================================================
-- Helper 管理
-- ============================================================================

spawnHelper :: String -> FilePath -> IO HelperHandle
spawnHelper host path = do
    myExe <- getExecutablePath
    let args = ["--helper", "--passcode=dontrunyourself", "--root=" ++ path]
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
execute :: Side -> Side -> [(FileChange, Maybe SyncAction)] -> IO ()
execute self other pairs = do
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
            appendEnd self  "DELETE" relPath
            appendEnd other "DELETE" relPath
    unless (null receives) $ do
        putStrLn "\nReceiving..."
        forM_ receives $ \(relPath, srcMode, srcMtime) -> do
            putStrLn $ "  copy " ++ relPath
            copyEndToEnd other self relPath srcMode srcMtime
    unless (null recordAdds) $ do
        putStrLn "\nRecording as added..."
        forM_ recordAdds $ \relPath -> do
            putStrLn $ "  add " ++ relPath
            appendEnd self  "ADD" relPath
            appendEnd other "ADD" relPath
    unless (null recordDels) $ do
        putStrLn "\nRecording as deleted..."
        forM_ recordDels $ \relPath -> do
            putStrLn $ "  del " ++ relPath
            appendEnd self  "DELETE" relPath
            appendEnd other "DELETE" relPath

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

appendEnd :: Side -> String -> FilePath -> IO ()
appendEnd (Lcl root) _cmd relPath =
    case _cmd of
        "DELETE" -> appendDeleted root Nothing relPath
        "ADD"    -> appendAdded root Nothing relPath
        _        -> return ()
appendEnd (Rmt hh) cmd relPath = do
    res <- expectOK hh (cmd ++ " " ++ relPath)
    case res of
        Left err -> error $ "FATAL: remote " ++ cmd ++ " failed: " ++ err
        Right _  -> return ()

-- | 跨端复制
--   srcMode / srcMtime 来自源端 FileState，直接应用，无需额外远程查询
copyEndToEnd :: Side -> Side -> FilePath -> Int -> Integer -> IO ()
copyEndToEnd src dst relPath srcMode srcMtime = case (src, dst) of
    -- 本地 → 本地
    (Lcl srcRoot, Lcl dstRoot) -> do
        let s = srcRoot </> relPath
            d = dstRoot </> relPath
        createDirectoryIfMissing True (takeDirectory d)
        copyFile s d
        setFileMode d (fromIntegral srcMode)
        setFileTimes d (fromIntegral srcMtime) (fromIntegral srcMtime)
        h1 <- hashLocal s
        h2 <- hashLocal d
        if h1 == h2
            then do putStrLn "    verified OK"
                    appendEnd dst "ADD" relPath
                    appendEnd src "ADD" relPath
            else putStrLn "    WARNING: verification failed"

    -- 远程 → 本地（远程 SEND，本地直接写文件）
    (Rmt hhSrc, Lcl dstRoot) -> do
        let d = dstRoot </> relPath
        createDirectoryIfMissing True (takeDirectory d)
        sendCommand hhSrc ("SEND " ++ relPath)
        sz <- readSendSize hhSrc
        hSetBinaryMode (hhOut hhSrc) True
        content <- hGetExactly (hhOut hhSrc) sz
        BS.writeFile d content
        setFileMode d (fromIntegral srcMode)
        setFileTimes d (fromIntegral srcMtime) (fromIntegral srcMtime)
        remoteHash <- hashRemote hhSrc relPath
        localHash  <- hashLocal d
        if remoteHash == localHash
            then do putStrLn "    verified OK"
                    appendEnd dst "ADD" relPath
                    appendEnd src "ADD" relPath
            else putStrLn "    WARNING: verification failed"

    -- 本地 → 远程（读本地文件，远程 RECV）
    (Lcl srcRoot, Rmt hhDst) -> do
        let s = srcRoot </> relPath
        content <- BS.readFile s
        let sz = BS.length content
        res <- expectOK hhDst ("RECV " ++ show sz ++ "," ++ relPath)
        case res of
            Left err -> putStrLn $ "  RECV failed: " ++ err
            Right _ -> do
                hSetBinaryMode (hhIn hhDst) True
                hPutFully (hhIn hhDst) content
                hFlush (hhIn hhDst)
                setRemoteStat hhDst relPath (fromIntegral srcMtime) srcMode
                localHash  <- hashLocal s
                remoteHash <- hashRemote hhDst relPath
                if localHash == remoteHash
                    then do putStrLn "    verified OK"
                            appendEnd dst "ADD" relPath
                            appendEnd src "ADD" relPath
                    else putStrLn "    WARNING: verification failed"

    -- 远程 → 远程
    (Rmt hhSrc, Rmt hhDst) -> do
        sendCommand hhSrc ("SEND " ++ relPath)
        sz <- readSendSize hhSrc
        res <- expectOK hhDst ("RECV " ++ show sz ++ "," ++ relPath)
        case res of
            Left err -> putStrLn $ "  RECV failed: " ++ err
            Right _ -> do
                hSetBinaryMode (hhOut hhSrc) True
                hSetBinaryMode (hhIn hhDst)  True
                content <- hGetExactly (hhOut hhSrc) sz
                hPutFully (hhIn hhDst) content
                hFlush (hhIn hhDst)
                setRemoteStat hhDst relPath (fromIntegral srcMtime) srcMode
                h1 <- hashRemote hhSrc relPath
                h2 <- hashRemote hhDst relPath
                if h1 == h2
                    then do putStrLn "    verified OK"
                            appendEnd dst "ADD" relPath
                            appendEnd src "ADD" relPath
                    else putStrLn "    WARNING: verification failed"

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

setRemoteStat :: HelperHandle -> FilePath -> EpochTime -> Int -> IO ()
setRemoteStat hh relPath mt md =
    void $ expectOK hh ("SETSTAT " ++ relPath ++ "," ++ show mt ++ " " ++ show md)

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
        conf = [ (f, d1, d2) | (Conflict fs d1 d2, Just Unsolvable) <- pairs1 ++ pairs2, let f = filename fs ]

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
        putStrLn "\nAlready deleted on both sides (will record):"
        forM_ recDels $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null conf)) $ do
        putStrLn "\nConflicts (will skip):"
        forM_ conf $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    let total = length add1 + length del1 + length add2 + length del2 + length recAdds + length recDels
    putStrLn $ "\nTotal: " ++ show total ++ " operations, " ++ show (length conf) ++ " conflicts"

removeFileSafe :: FilePath -> IO ()
removeFileSafe fp = doesFileExist fp >>= \e -> when e (removeFile fp)

hashLocal :: FilePath -> IO String
hashLocal fp = do
    content <- BS.readFile fp
    return $ show (hash content :: Digest SHA512)
