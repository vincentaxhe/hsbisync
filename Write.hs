module Write where

import Data.Bits ((.&.))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO
    ( Handle, hClose, hFlush
    , hPutStrLn, hSetBinaryMode
    , hGetLine, openTempFile
    )
import System.Posix.Files
    ( getFileStatus, fileMode, modificationTime
    , setFileMode, setFileTimes
    )

import Crypto.Hash (hash, SHA512, Digest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- ============================================================================
-- Types
-- ============================================================================

-- | 连接到 helper 进程的句柄对
data HelperHandle = HelperHandle
    { hhIn  :: !Handle    -- ^ 写入 helper 的 stdin（发命令）
    , hhOut :: !Handle    -- ^ 读取 helper 的 stdout（收响应）
    }

-- | helper 响应
data Response
    = ROk  !String        -- ^ OK + 可选内容
    | RErr !String        -- ^ ERROR + 错误信息
    deriving (Show, Eq)

-- ============================================================================
-- 底层 I/O：确保完整读写
-- ============================================================================

-- | 从 Handle 精确读取 n 字节，保证读满或 EOF
--
--   用于二进制传输（RECV），不会因短读而丢数据
hGetExactly :: Handle -> Int -> IO BS.ByteString
hGetExactly h n = BS.concat . reverse <$> go n []
  where
    go 0     chunks = return chunks
    go remaining chunks = do
        chunk <- BS.hGet h remaining
        let got = BS.length chunk
        if got == 0
            then return chunks
            else go (remaining - got) (chunk : chunks)

-- | 向 Handle 完整写入 ByteString，分块写保证全部写出
--
--   ByteString 的 'hPut' 理论上写全部，但大文件时分块更安全
hPutFully :: Handle -> BS.ByteString -> IO ()
hPutFully h bs = go bs
  where
    chunkSize = 8192   -- 8KB 块
    go b
        | BS.null b = return ()
        | otherwise = do
            let (chunk, rest) = BS.splitAt chunkSize b
            BS.hPut h chunk
            hFlush h
            go rest

-- ============================================================================
-- Helper 协议通信
-- ============================================================================

-- | 向 helper 发送命令
sendCommand :: HelperHandle -> String -> IO ()
sendCommand hh cmd = do
    hPutStrLn (hhIn hh) cmd
    hFlush (hhIn hh)

-- | 读取 helper 响应行
readResponse :: HelperHandle -> IO Response
readResponse hh = do
    line <- hGetLine (hhOut hh)
    return $ case break (== ' ') line of
        ("OK",  rest) -> ROk  (dropWhile (== ' ') rest)
        ("ERROR", msg) -> RErr (dropWhile (== ' ') msg)
        _              -> RErr ("Unexpected response: " ++ line)

-- | 发送命令并期望 OK 响应
expectOK :: HelperHandle -> String -> IO (Either String String)
expectOK hh cmd = do
    sendCommand hh cmd
    resp <- readResponse hh
    return $ case resp of
        ROk msg  -> Right msg
        RErr msg -> Left msg

-- | 发送命令，读取 JSON 响应（用于 GETSTATE/GETLAST/GETDIFF）
readJSON :: HelperHandle -> String -> IO (Either String BL.ByteString)
readJSON hh cmd = do
    sendCommand hh cmd
    resp <- readResponse hh
    case resp of
        RErr msg -> return $ Left msg
        ROk _    -> do
            line <- BS.hGetLine (hhOut hh)
            return $ Right (BL.fromStrict line)

-- ============================================================================
-- 文件属性同步
-- ============================================================================

-- | 从源端获取文件属性，同步到目标端
--
--   流程：GETSTAT(src) → SETSTAT(dst)
syncFileAttrs :: HelperHandle -> HelperHandle -> FilePath -> IO (Either String ())
syncFileAttrs src dst filename = do
    -- 获取源文件属性
    res <- expectOK src ("GETSTAT " ++ filename)
    case res of
        Left err -> return $ Left ("GETSTAT failed: " ++ err)
        Right statStr -> do
            -- statStr = "mtime mode"
            let setCmd = "SETSTAT " ++ filename ++ "," ++ statStr
            res' <- expectOK dst setCmd
            case res' of
                Left err -> return $ Left ("SETSTAT failed: " ++ err)
                Right _  -> return $ Right ()

-- | 本地同步文件属性（不通过 helper 协议）
--
--   直接读源文件 stat，设到目标文件
syncFileAttrsLocal :: FilePath -> FilePath -> IO ()
syncFileAttrsLocal srcPath dstPath = do
    fs <- getFileStatus srcPath
    let mt   = modificationTime fs
        mode = fileMode fs .&. 0o7777
    setFileMode dstPath mode
    setFileTimes dstPath mt mt

-- ============================================================================
-- 文件写入（统一入口：原子写入 + 属性 + 哈希）
-- ============================================================================

-- | 计算内存中 ByteString 的 SHA512 哈希（hex 字符串），无需读磁盘
hashBytes :: BS.ByteString -> String
hashBytes = show . (hash :: BS.ByteString -> Digest SHA512)

-- | 计算本地文件 SHA512 哈希（hex 字符串）
hashFile :: FilePath -> IO String
hashFile fp = do
    content <- BS.readFile fp
    return $ hashBytes content

-- | 原子写入文件：系统分配随机临时文件名，写入成功后原子 rename 到目标路径。
--
--   使用 'openTempFile' 确保临时文件名唯一，多进程并发写同一目标文件也不会冲突。
--   保证并发读者永远看到完整文件——要么旧文件，要么新文件，不会看到写入一半的内容。
--   即使在写入过程中程序崩溃，也只会留下孤儿临时文件，不会破坏目标文件。
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic path content = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    (tmpPath, h) <- openTempFile dir ".tmp"
    BS.hPut h content
    hClose h
    renameFile tmpPath path

-- | 原子写入 + 校验：先写临时文件 → 设属性 → 算哈希比对 → 通过才 rename。
--
--   这是 rsync 的核心安全保证：
--   * 校验在 rename 之前——哈希不匹配则删除临时文件，目标文件完全不受影响
--   * 校验通过才原子替换——新文件从出现的第一刻起内容和属性就完全正确
--   * 不会出现"写完了才发现不对但原文件已经没了"的情况
--
--   返回 @Right ()@ 表示写入和校验均成功，@Left err@ 表示校验失败（原文件未动）。
writeFileVerified
    :: FilePath       -- ^ 目标路径
    -> BS.ByteString  -- ^ 文件内容
    -> Int            -- ^ 目标权限 mode
    -> Integer        -- ^ 目标 mtime（epoch 秒）
    -> String         -- ^ 期望的 SHA512 哈希（hex）
    -> IO (Either String ())
writeFileVerified path content mode_ mtime_ expectedHash = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    (tmpPath, h) <- openTempFile dir ".tmp"
    BS.hPut h content
    hClose h
    -- 属性在 rename 前设好，保证新文件立即可用
    setFileMode tmpPath (fromIntegral mode_)
    setFileTimes tmpPath (fromIntegral mtime_) (fromIntegral mtime_)
    -- 核⼼：校验在 rename 之前，不通过则丢弃临时文件
    let actualHash = hashBytes content
    if actualHash /= expectedHash
        then do
            removeFile tmpPath
            return $ Left "verification failed before commit; target untouched"
        else do
            renameFile tmpPath path
            return $ Right ()

-- | 安全删除文件：不存在则静默跳过，不抛异常
removeFileSafe :: FilePath -> IO ()
removeFileSafe fp = do
    exists <- doesFileExist fp
    if exists then removeFile fp else return ()

-- | 本地写入文件，确保完整落盘后同步属性（保留旧名，兼容已有调用）
--
--   已由 'writeFileAtomic' 替代，保留此别名避免大规模改动。
writeFileLocal :: FilePath -> BS.ByteString -> IO ()
writeFileLocal = writeFileAtomic

-- | 通过 helper 协议在两个 helper 之间复制文件
--
--   流程：
--   1. SEND src → 获取文件大小
--   2. RECV dst → 目标端准备接收
--   3. 管道传输：src.stdout → dst.stdin
--   4. 同步属性：src.GETSTAT → dst.SETSTAT
copyFileRemote :: HelperHandle -> HelperHandle -> FilePath -> IO (Either String ())
copyFileRemote src dst filename = do
    -- 1. 源端 SEND
    sendCommand src ("SEND " ++ filename)
    resp <- readResponse src
    sz <- case resp of
        ROk sizeStr ->
            case reads sizeStr of
                [(n, "")] -> return n
                _         -> return (-1)
        RErr _msg -> return (-1)

    if sz < 0
        then return $ Left "SEND failed"
        else do
            -- 2. 目标端 RECV
            res <- expectOK dst ("RECV " ++ show sz ++ "," ++ filename)
            case res of
                Left err -> return $ Left ("RECV failed: " ++ err)
                Right _ -> do
                    -- 3. 管道传输
                    hSetBinaryMode (hhOut src) True
                    hSetBinaryMode (hhIn dst)  True

                    content <- hGetExactly (hhOut src) sz
                    hPutFully (hhIn dst) content
                    hFlush (hhIn dst)

                    -- 4. 同步属性
                    syncRes <- syncFileAttrs src dst filename
                    return syncRes

-- ============================================================================
-- 文件哈希验证
-- ============================================================================

-- | 对两个 helper 端的同一文件进行哈希对比
--
--   返回：Left err | Right True(相同) | Right False(不同)
compareFileRemote :: HelperHandle -> HelperHandle -> FilePath -> IO (Either String Bool)
compareFileRemote h0 h1 filename = do
    res0 <- expectOK h0 ("HASH " ++ filename)
    res1 <- expectOK h1 ("HASH " ++ filename)
    case (res0, res1) of
        (Left err, _) -> return $ Left ("hash 0: " ++ err)
        (_, Left err) -> return $ Left ("hash 1: " ++ err)
        (Right h0', Right h1') -> return $ Right (h0' == h1')
