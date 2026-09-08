module Exe where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket, catch, IOException)
import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (doesFileExist)
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
    ( FileState(..), FileChange(..), parseStateJSON, statRel
    , currentState, lastState, appendAdded, appendDeleted, posixSecs
    )
import Sync (sync, SyncAction(..), resolveConflicts)
import Write
    ( HelperHandle(..), Response(..), sendCommand, readResponse, readJSON
    , hGetExactly, hPutFully, expectOK
    , writeFileVerified, hashBytes, removeFileSafe, getIgnoreRemote
    )
import Arg (Root(..), rootLabel)

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

    -- 阶段 0：明确两端地址
    putStrLn $ "\n  root1 = " ++ rootLabel root1
    putStrLn $ "  root2 = " ++ rootLabel root2
    putStrLn $ "  profile: " ++ profile

    -- 阶段 1：无条件先同步当前 profile 的 ignore 文件（较新一端覆盖较旧一端）。
    -- 两端 ignore 必须先一致，之后各自扫描文件集合才不会出现偏差。
    -- 此阶段之后 ignore 文件不再参与文件同步。
    putStrLn "\n[1/3] Syncing ignore rules ..."
    syncIgnoreFiles profile mHost1 path1 mHost2 path2

    -- 阶段 2：并行读取两端状态（各自按已统一的 ignore 过滤）
    putStrLn "\n[2/3] Reading states ..."
    ((label1, (last1, cur1)), (label2, (last2, cur2))) <-
        concurrently (readSide mTag mHost1 path1) (readSide mTag mHost2 path2)

    putStrLn $ "  root1 (" ++ label1 ++ "): " ++ show (length cur1) ++ " files (last: " ++ show (length last1) ++ ")"
    putStrLn $ "  root2 (" ++ label2 ++ "): " ++ show (length cur2) ++ " files (last: " ++ show (length last2) ++ ")"

    -- 阶段 3：计算差异并实际同步文件。此阶段只处理两端扫描出的普通文件，
    -- ignore 文件已在上面的阶段 1 处理，不会进入这里的记录/删除/传输。
    putStrLn "\n[3/3] Syncing files ..."
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
-- Ignore 规则预同步（同步开始前对齐两端 .bisync/<profile>/ignore）
-- ============================================================================

-- | 若该端为远程，则为其派生一个 helper 执行动作；本地端直接执行。
withSide :: Maybe String -> FilePath -> String -> (Side -> IO a) -> IO a
withSide Nothing path _ k = k (Lcl path)
withSide (Just host) path profile k =
    bracket (spawnHelper host path profile) destroyHelper (k . Rmt)

-- | profile 的 ignore 文件相对路径（相对同步根目录）
ignoreRel :: String -> FilePath
ignoreRel profile = ".bisync" </> profile </> "ignore"

-- | 读取本地 ignore 文件元信息 @(mtime, mode, sha512)@，不存在返回 Nothing
localIgnoreMeta :: FilePath -> String -> IO (Maybe (Integer, Int, String))
localIgnoreMeta root profile = do
    let rel  = ignoreRel profile
        full = root </> rel
    e <- doesFileExist full
    if not e
        then return Nothing
        else do
            fs <- statRel root rel
            content <- BS.readFile full
            return $ Just (posixSecs (mtime fs), mode fs, hashBytes content)

-- | 读取任一端 ignore 文件元信息
sideIgnoreMeta :: Side -> String -> IO (Maybe (Integer, Int, String))
sideIgnoreMeta (Lcl root) profile = localIgnoreMeta root profile
sideIgnoreMeta (Rmt hh)   _        = getIgnoreRemote hh

-- | 对齐两端 ignore 文件（在扫描状态之前调用）
syncIgnoreFiles
    :: String            -- ^ profile
    -> Maybe String      -- ^ root1 host（Nothing=本地）
    -> FilePath         -- ^ root1 路径
    -> Maybe String      -- ^ root2 host
    -> FilePath         -- ^ root2 路径
    -> IO ()
syncIgnoreFiles profile mHost1 path1 mHost2 path2 =
    withSide mHost1 path1 profile $ \side1 ->
      withSide mHost2 path2 profile $ \side2 ->
        syncIgnore profile side1 side2

-- | 两端 ignore 的合并决策：
--   * 都无 → 无事
--   * 仅一端有 → 复制到另一端
--   * 都有且内容相同 → 跳过
--   * 都有但内容不同 → 较新（mtime 大）的一端覆盖另一端；
--     mtime 相同则告警并采用 root1
syncIgnore :: String -> Side -> Side -> IO ()
syncIgnore profile sideA sideB = do
    mA <- sideIgnoreMeta sideA profile
    mB <- sideIgnoreMeta sideB profile
    case (mA, mB) of
        (Nothing, Nothing) ->
            putStrLn "  (no ignore file on root1 or root2)"
        (Just a, Nothing) ->
            copyIgnore profile sideA sideB "root2" a
        (Nothing, Just b) ->
            copyIgnore profile sideB sideA "root1" b
        (Just a, Just b)
            | hashOf a == hashOf b ->
                putStrLn "  ignore identical on root1 and root2"
            | otherwise ->
                case compare (mtOf a) (mtOf b) of
                    GT -> copyIgnore profile sideA sideB "root2" a
                    LT -> copyIgnore profile sideB sideA "root1" b
                    EQ -> do
                        putStrLn "  WARNING: ignore files differ but mtime is equal; using root1"
                        copyIgnore profile sideA sideB "root2" a
  where
    mtOf   (m, _, _) = m
    hashOf (_, _, h) = h

-- | 把 src 端的 ignore 文件同步到 dst 端（经 transferContent 保留 mtime/mode）。
--   只提示哪个 root 被更新了，不区分"较旧/缺失"、不指明来源。
copyIgnore
    :: String -> Side -> Side -> String
    -> (Integer, Int, String)  -- ^ 源端 (mtime, mode, hash)
    -> IO ()
copyIgnore profile src dst dstName (mt, srcMode, _) = do
    result <- transferContent src dst (ignoreRel profile) srcMode mt
    case result of
        Left err -> putStrLn $ "  ERROR: failed to sync ignore file: " ++ err
        Right () -> putStrLn $ "  " ++ dstName ++ " ignore updated"

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
            st <- (,) <$> lastState path mTag <*> currentState path mTag
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
    hSetBinaryMode (hhOut hhSrc) False             -- 恢复文本模式，供后续行协议读取
    hSetEncoding (hhOut hhSrc) utf8                -- 显式重设 utf8（C locale 下二进制开关不恢复编码）
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
    hSetBinaryMode (hhOut hhSrc) False             -- 恢复文本模式
    hSetEncoding (hhOut hhSrc) utf8                -- 显式重设 utf8
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
            hSetBinaryMode (hhIn hh) False       -- 恢复文本模式：后续命令可能含非 ASCII 文件名
            hSetEncoding (hhIn hh) utf8          -- 显式重设 utf8（C locale 下不恢复编码）
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