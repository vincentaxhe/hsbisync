module Write where

import Control.Exception (IOException, bracket, catch, try)
import Data.Bits ((.&.))
import System.Directory
    ( createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , listDirectory, removeDirectory, removeFile, renameFile
    )
import System.FilePath (takeDirectory, isAbsolute, normalise, (</>))
import System.IO
    ( Handle, IOMode(ReadMode), hClose, hFileSize, hFlush, openBinaryFile
    )
import System.Posix.Files
    ( FileStatus
    , getFileStatus, fileMode, modificationTime
    , setFileMode, setFileTimes
    , getSymbolicLinkStatus, isDirectory, isSymbolicLink
    , readSymbolicLink, createSymbolicLink, removeLink
    , setSymbolicLinkTimesHiRes
    )

import Crypto.Hash
    ( hash, hashInit, hashUpdate, hashFinalize, Context, SHA512(..), Digest )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8With)
import qualified Data.Text.Encoding.Error as TEE (lenientDecode)

import qualified Msg
import qualified Tmp

-- ============================================================================
-- 临时文件脚手架
-- ============================================================================

-- | 在临时文件上干活：创建 → 交给 @act@（写完由调用方 rename 到目标名 = 提交）。
--
--   **不做任何自动清理**：中断（Ctrl-C / 断线 / 断电）留下的临时文件就留在磁盘上，
--   下一次扫描会看到它（文件树保持真实），要不要排除由 ignore 决定，要清就手工删。
--   命名可识别（见 'Tmp'）就是为了让这件事做得成。
withTemp :: FilePath -> (FilePath -> Handle -> IO a) -> IO a
withTemp dir act = do
    (tmpPath, h) <- Tmp.createTemp dir
    act tmpPath h

-- ============================================================================
-- Types
-- ============================================================================

-- | 连接到 helper 进程的句柄对
data HelperHandle = HelperHandle
    { hhIn  :: !Handle    -- ^ 写入 helper 的 stdin（发请求）
    , hhOut :: !Handle    -- ^ 读取 helper 的 stdout（收响应）
    }

-- ============================================================================
-- 底层 I/O：流式（内存 O(1)）
--
--   文件内容**从不整块进内存**：发送端边读边发、接收端边收边写盘，两边各自
--   用增量 SHA512 推进。峰值内存 = 一块 buffer，与文件大小无关。
-- ============================================================================

-- | 每块大小。64 KiB 与管道/ssh 的内部缓冲区同量级：再小就只剩 syscall 开销。
streamChunkSize :: Int
streamChunkSize = 64 * 1024

-- | 增量哈希的一步。
--
--   **必须强制求值**：cryptonite 的 'hashUpdate' 是惰性的，不 @seq@ 的话整条
--   更新链会把每一块的 ByteString 都扣在内存里（实测 2 GB 文件：惰性链常驻
--   1.17 GB，每块 seq 后只有 86 KB）。
hashStep :: Context SHA512 -> BS.ByteString -> Context SHA512
hashStep ctx bs = let ctx' = hashUpdate ctx bs in ctx' `seq` ctx'

-- | 空的 SHA512 上下文
hashInit512 :: Context SHA512
hashInit512 = hashInit

-- | 收尾成 hex 字符串
hashFinish :: Context SHA512 -> String
hashFinish = show . (hashFinalize :: Context SHA512 -> Digest SHA512)

-- | 泵数据：从 @src@ 读最多 @limit@ 字节（'Nothing' = 读到 EOF），写入 @mdst@（若给定），
--   同时（可选）推进 SHA512。返回 @(哈希, 实读字节数)@。
--
--   **读写异常不抛出**，而是返回已经处理完的字节数：调用方靠这个数字补齐载荷，
--   保证帧（"size 字节裸数据"）始终对齐，后面的命令不会错位。
pump
    :: Handle          -- ^ 源
    -> Maybe Handle    -- ^ 目的（'Nothing' = 丢弃）
    -> Bool            -- ^ 是否计算哈希（纯转发时不需要）
    -> Maybe Integer   -- ^ 上限（'Nothing' = 读到 EOF）
    -> IO (String, Integer)
pump src mdst hashing mlimit = go hashInit512 0
  where
    go ctx n = case mlimit of
        Just lim | n >= lim -> return (finish ctx, n)
        _ -> do
            let want = case mlimit of
                    Just lim -> fromIntegral (min (toInteger streamChunkSize) (lim - n))
                    Nothing  -> streamChunkSize
            r <- try (BS.hGetSome src want)
            case r of
                Left e -> do
                    ignoreIOError e
                    return (finish ctx, n)
                Right chunk
                    | BS.null chunk -> return (finish ctx, n)
                    | otherwise -> do
                        w <- case mdst of
                            Nothing -> return (Right ())
                            Just d  -> try (BS.hPut d chunk)
                        case w of
                            Left e -> do
                                ignoreIOError e
                                return (finish ctx, n)
                            Right () ->
                                go (if hashing then hashStep ctx chunk else ctx)
                                   (n + fromIntegral (BS.length chunk))
    finish ctx = if hashing then hashFinish ctx else ""
    ignoreIOError :: IOException -> IO ()
    ignoreIOError _ = return ()

-- | 精确搬运 @n@ 字节（不算哈希，纯转发用）；返回实搬字节数
copyExactly :: Handle -> Handle -> Integer -> IO Integer
copyExactly src dst n = snd <$> pump src (Just dst) False (Just n)

-- | 精确搬运 @n@ 字节，同时算 SHA512；返回 @(哈希, 实搬字节数)@
copyExactlyHash :: Handle -> Maybe Handle -> Integer -> IO (String, Integer)
copyExactlyHash src mdst n = pump src mdst True (Just n)

-- | 排空 @n@ 字节（丢弃载荷）：发送端已经答应的数据必须吃掉，否则它的 stdout
--   会被写满而卡住，后续命令全部错位。
discardExactly :: Handle -> Integer -> IO ()
discardExactly h n = () <$ pump h Nothing False (Just n)

-- | 写 @n@ 个零字节（分块，不会给 1 GB 分配一块内存）。
--
--   用途：发送端读源出错/提前 EOF 时把帧补齐 —— 接收端长度仍然对齐，而哈希必然
--   对不上，于是它自己删临时文件、回错误。这比悄悄截短载荷（会把后面的命令当数据读）安全。
padZeros :: Handle -> Integer -> IO ()
padZeros h = go
  where
    zeros = BS.replicate streamChunkSize 0
    go n
        | n <= 0 = return ()
        | otherwise = do
            let m = min n (toInteger streamChunkSize)
            BS.hPut h (if m >= toInteger streamChunkSize then zeros else BS.take (fromIntegral m) zeros)
            go (n - m)

-- | 从 Handle 精确读取 n 字节，保证读满或 EOF
--
--   只用于**链接内容**这类短内容（文件一律走 'pump' 流式）。
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

-- ============================================================================
-- Helper 协议通信：一行一个规范 JSON（见 Msg）
--
--   * 请求：{"cmd":"…", …}
--   * 响应：{"ok":true,…} / {"ok":false,"error":"…"}
--   * 文件内容不走 JSON：SEND/RECV 的响应里给出 "size"，紧跟其后的就是 <size> 字节裸数据
--   * 控制行全用 ByteString 读/写（不碰 Handle 的文本编码层）——
--     所以不再需要 hSetBinaryMode / hSetEncoding 来回切换（那一对是以前的坑）
-- ============================================================================

-- | 发一条请求（一行规范 JSON）
sendMsg :: HelperHandle -> Msg.Value -> IO ()
sendMsg hh req = do
    BL.hPut (hhIn hh) (Msg.encodeLine req)
    hFlush (hhIn hh)

-- | 读一条响应；@ok=false@ 或格式错都归成 Left
readMsg :: HelperHandle -> IO (Either String Msg.Value)
readMsg hh = do
    line <- B8.hGetLine (hhOut hh)
    return $ case Msg.decodeStrict line of
        Left e  -> Left ("bad response (" ++ e ++ "): " ++ show line)
        Right v -> case Msg.asBool =<< Msg.field "ok" v of
            Just True -> Right v
            _         -> Left (maybe ("remote error: " ++ show line) id (Msg.fieldStr "error" v))

-- | 请求 → 响应（成功时返回响应对象，供调用方取字段）
call :: HelperHandle -> Msg.Value -> IO (Either String Msg.Value)
call hh req = sendMsg hh req >> readMsg hh

-- ============================================================================
-- repo 文件
-- ============================================================================

-- | 覆盖写入 .bisync/\<profile\>/repo（地址写成一行 + 换行）
--
--   显式给出双端地址的 sync 是“权威输入”：直接覆盖，不与旧内容比较
--   （旧内容可能来自上一次同步或手工编辑，不参与决策）。
--
--   repo 是我们自己的数据文件，按“字节 + 显式 UTF-8”处理，不依赖进程 locale。
writeRepoFile :: FilePath -> String -> IO ()
writeRepoFile fp content = do
    createDirectoryIfMissing True (takeDirectory fp)
    BS.writeFile fp (T.encodeUtf8 (T.pack (content ++ "\n")))

-- ============================================================================
-- 文件属性同步
-- ============================================================================

-- | 从源端获取文件属性，同步到目标端
--
--   流程：GETSTAT(src) → SETSTAT(dst)
syncFileAttrs :: HelperHandle -> HelperHandle -> FilePath -> IO (Either String ())
syncFileAttrs src dst filename = do
    r <- call src (Msg.cmd "getstat" [("path", Msg.str filename)])
    case r of
        Left err -> return $ Left ("getstat failed: " ++ err)
        Right v  -> case (Msg.fieldInt "mtime" v, Msg.fieldInt "mode" v) of
            (Just mt, Just md) -> do
                r2 <- call dst (Msg.cmd "setstat" [ ("path",  Msg.str filename)
                                                   , ("mtime", Msg.int mt)
                                                   , ("mode",  Msg.int md) ])
                return $ either (Left . ("setstat failed: " ++)) (const (Right ())) r2
            _ -> return $ Left "getstat: missing mtime/mode"

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
--
--   只用于**已经在内存里的短内容**（链接指向、ignore 文件）；文件一律走 'hashFile'。
hashBytes :: BS.ByteString -> String
hashBytes = show . (hash :: BS.ByteString -> Digest SHA512)

-- | 流式计算本地文件 SHA512（hex 字符串），内存 O(1)
--
--   旧实现是 @BS.readFile@ 后再算：1 GB 文件就是 1 GB 常驻。
hashFile :: FilePath -> IO String
hashFile fp = bracket (openBinaryFile fp ReadMode) hClose $ \h -> do
    sz <- hFileSize h
    fst <$> copyExactlyHash h Nothing sz

-- | 只把 mtime（与 atime）设成给定秒数：不动内容、不动 mode。
--
--   用于“内容实质相同、只差时间戳”时把较旧那端对齐到较新的 mtime：
--   对齐后两端真实文件的 (size, mtime) 一致，记成 added 之后下次运行就是 Unchanged。
--   与落盘写入一致地解析末端链接（作用到链接指向的目标上，保留链接本身）。
setMtimeOnly :: FilePath -> Integer -> IO ()
setMtimeOnly path t = do
    target <- resolveLeafLink path
    setFileTimes target (fromIntegral t) (fromIntegral t)

-- | 原子写入文件：系统分配随机临时文件名，写入成功后原子 rename 到目标路径。
--
--   使用 'openTempFile' 确保临时文件名唯一，多进程并发写同一目标文件也不会冲突。
--   保证并发读者永远看到完整文件——要么旧文件，要么新文件，不会看到写入一半的内容。
--   即使在写入过程中程序崩溃，也只会留下孤儿临时文件，不会破坏目标文件。
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic path content = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    withTemp dir $ \tmpPath h -> do
        BS.hPut h content
        hClose h
        renameFile tmpPath path

-- ============================================================================
-- 目录创建：穿透悬空软链接（让"路径透明"在写文件时真正成立）
-- ============================================================================

-- | 确保目录 @d@ 存在；路径上遇到的**悬空软链接**会被落实为它指向的目录。
--
--   为什么需要：同步是"先同步链接结构、再按路径透明地传文件"。写文件时路径可能
--   穿过一个刚建好、目标还不存在的目录链接，此时：
--   * @createDirectoryIfMissing@ 对链接名本身会 EEXIST（名字已存在但不是目录）；
--   * 再往下（@L/sub@）内核解析链接就会 ENOENT → 写入失败。
--   所以写入前先把链接指向的目录建出来，之后路径自然可达。
--
--   只建目录，**绝不建空文件**：若先造一个空文件，后续 size/mtime 比较全错。
--   链接成环时靠 depth 上限退出并报错。
ensureDirChain :: FilePath -> IO (Either String ())
ensureDirChain = go (0 :: Int)
  where
    go depth d
        | depth > 40 =
            return $ Left ("too many symlink levels while creating directory " ++ d)
        | otherwise = do
            exists <- doesDirectoryExist d
            if exists
                then return (Right ())
                else do
                    let parent = takeDirectory d
                    pRes <- if parent == d then return (Right ()) else go (depth + 1) parent
                    case pRes of
                        Left e -> return (Left e)
                        Right () -> do
                            isLink <- isSymLink d
                            if isLink
                                then do
                                    -- 悬空链接：建出它指向的目录
                                    tgt <- (try (readSymbolicLink d) :: IO (Either IOException FilePath))
                                    case tgt of
                                        Left e ->
                                            return $ Left ("cannot read symlink " ++ d ++ ": " ++ show e)
                                        Right t -> do
                                            let t' = if isAbsolute t
                                                        then t
                                                        else normalise (parent </> t)
                                            go (depth + 1) t'
                                else do
                                    r <- (try (createDirectory d) :: IO (Either IOException ()))
                                    case r of
                                        Right () -> return (Right ())
                                        Left e -> do
                                            -- 竞态：别人刚建好了同名目录
                                            nowDir <- doesDirectoryExist d
                                            if nowDir
                                                then return (Right ())
                                                else return $ Left
                                                    ("cannot create directory " ++ d ++ ": " ++ show e)

-- ============================================================================
-- 目标落地：临时文件 → 校验 → rename
--
--   这是 rsync 的核心安全保证（与文件大小无关，流式同样成立）：
--   * 校验在 rename 之前——哈希不匹配则删临时文件，目标文件完全不受影响；
--   * 校验通过才原子替换——新文件从出现的第一刻起内容和属性就完全正确；
--   * 不会出现"写完了才发现不对但原文件已经没了"的情况。
--
--   目录语义（与 rsync 一致）：
--   * 同步根 @root@ 必须**已存在**——不自动创建，避免 root 写错（或对端路径
--     不存在）时凭空造出一整棵树；
--   * 根**之下**的中间目录（目标文件全路径的父目录）按需**递归**创建，
--     否则往全新的空目录同步时，任何嵌套文件都会因"父目录不存在"而失败。
-- ============================================================================
--
-- | 准备目标：校验同步根、解析末端链接、递归建目录链、建可识别临时文件。
--
--   返回 @(实际目标路径, 临时文件路径, 句柄)@。提交用 'commitReceived'；放弃用
--   'removeQuietly'（临时文件命名规则见 'Tmp'：中断留下的残骸人工清）。
openTargetTemp :: FilePath -> FilePath -> IO (Either String (FilePath, FilePath, Handle))
openTargetTemp root path = do
    rootExists <- doesDirectoryExist root
    if not rootExists
        then return $ Left ("sync root does not exist: " ++ root)
        else do
            -- 目标位置若是"指向文件的链接"：写到它指向的真实文件上，保留链接本身
            target <- resolveLeafLink path
            let dir = takeDirectory target
            mk <- ensureDirChain dir
            case mk of
                Left e -> return (Left e)
                Right () -> do
                    (tmpPath, h) <- Tmp.createTemp dir
                    return (Right (target, tmpPath, h))

-- | 删一个文件，不存在/失败都静默（清理临时文件用）
removeQuietly :: FilePath -> IO ()
removeQuietly fp = removeFile fp `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = return ()

-- | 校验 + 设属性 + 提交（rename）。**校验在 rename 之前**：不通过就删临时文件，
--   目标路径上的旧文件完全不受影响。
commitReceived
    :: FilePath    -- ^ 目标路径
    -> FilePath    -- ^ 临时文件
    -> Int         -- ^ 目标 mode
    -> Integer     -- ^ 目标 mtime（epoch 秒）
    -> String      -- ^ 本端边收边算的哈希
    -> String      -- ^ 发送端边读边算的哈希
    -> IO (Either String ())
commitReceived target tmpPath mode_ mtime_ actual expected
    | actual /= expected = do
        removeQuietly tmpPath
        return $ Left "verification failed before commit; target untouched"
    | otherwise = do
        setFileMode tmpPath (fromIntegral mode_)
        setFileTimes tmpPath (fromIntegral mtime_) (fromIntegral mtime_)
        renameFile tmpPath target
        return (Right ())

-- | 本地文件 → 本地目标（流式，内存 O(1)）。
--
--   不计算哈希：同一批字节既读又写，哈希在这里提供不了任何信息（旧实现的两个
--   哈希都算在同一块内存上，比较恒真）。真正有意义的检查是**源文件没有被截断**：
--   实读字节数小于开始时的 size 就丢弃临时文件 —— 否则会把一个前缀当成完整文件提交。
copyFileStream
    :: FilePath -> FilePath -> FilePath -> Int -> Integer -> IO (Either String ())
copyFileStream srcPath root path mode_ mtime_ = do
    r <- try (openBinaryFile srcPath ReadMode)
    case r of
        Left e -> return $ Left ("cannot read source: " ++ show (e :: IOException))
        Right srcH -> do
            sz0 <- hFileSize srcH
            res <- openTargetTemp root path >>= \prep -> case prep of
                Left e -> return (Left e)
                Right (target, tmpPath, h) -> do
                    n <- copyExactly srcH h sz0
                    hFlush h
                    hClose h
                    if n < sz0
                        then do
                            removeQuietly tmpPath
                            return $ Left "source truncated while copying; target untouched"
                        else do
                            setFileMode tmpPath (fromIntegral mode_)
                            setFileTimes tmpPath (fromIntegral mtime_) (fromIntegral mtime_)
                            renameFile tmpPath target
                            return (Right ())
            hClose srcH
            return res

-- | 安全删除文件：不存在则静默跳过，不抛异常。
--
--   与写入路径对称：目标位置是"指向文件的链接"时删除**它指向的文件**（保留链接），
--   否则删链接只会留下目标、删目标才会真正镜像对端的删除。
removeFileSafe :: FilePath -> IO ()
removeFileSafe fp = do
    target <- resolveLeafLink fp
    exists <- doesFileExist target
    if exists then removeFile target else return ()

-- | 本地写入文件，确保完整落盘后同步属性（保留旧名，兼容已有调用）
--
--   已由 'writeFileAtomic' 替代，保留此别名避免大规模改动。
writeFileLocal :: FilePath -> BS.ByteString -> IO ()
writeFileLocal = writeFileAtomic

-- ============================================================================
-- GETIGNORE — 通过 helper 协议读取远端 ignore 元信息
-- ============================================================================

-- | 通过 helper 协议读取远端 ignore 文件元信息（用于跨端比较）
--
--   协议：GETIGNORE
--   文件存在：OK <mtime> <mode> <sha512hex>
--   文件不存在：OK -
--   返回 @(mtime, mode, sha512hex)@；文件不存在返回 @Nothing@
getIgnoreRemote :: HelperHandle -> IO (Maybe (Integer, Int, String))
getIgnoreRemote hh = do
    r <- call hh (Msg.cmd "getignore" [])
    case r of
        Left err -> error $ "getignore failed: " ++ err
        Right v
            | Just False <- Msg.asBool =<< Msg.field "exists" v -> return Nothing
            | otherwise -> case ( Msg.fieldInt "mtime" v
                                , Msg.fieldInt "mode" v
                                , Msg.fieldStr "hash" v ) of
                (Just mt, Just md, Just h) -> return $ Just (mt, fromIntegral md, h)
                _ -> error $ "getignore: bad response: " ++ show v

-- ============================================================================
-- 符号链接：把链接当成"内容 = 指向字符串"的文件
--
--   链接条目的 size = st_size = 指向字符串长度、mtime = 链接自身 mtime，
--   而"内容"就是指向字符串本身 —— 因此 SEND/RECV/哈希校验/先校验后落地全部复用文件那套。
-- ============================================================================

-- | 路径本身是否为符号链接（lstat，不跟随）。不存在/不可访问 → False
isSymLink :: FilePath -> IO Bool
isSymLink path = do
    r <- (try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus))
    return $ case r of
        Right st -> isSymbolicLink st
        Left _   -> False

-- | 读取符号链接的"内容"：它的指向字符串（UTF-8 字节，可直接参与哈希/SEND/RECV）
readLinkContent :: FilePath -> IO BS.ByteString
readLinkContent path = T.encodeUtf8 . T.pack <$> readSymbolicLink path

-- | 指向字符串的字节 → FilePath（'readLinkContent' 的逆）
linkTargetPath :: BS.ByteString -> FilePath
linkTargetPath = T.unpack . T.decodeUtf8With TEE.lenientDecode

-- | 删除链接本身（unlink），不跟随到目标；返回是否真的删掉了一个链接
removeLinkSafe :: FilePath -> IO Bool
removeLinkSafe path = do
    isLink <- isSymLink path
    if not isLink
        then return False
        else do
            r <- (try (removeLink path) :: IO (Either IOException ()))
            return (either (const False) (const True) r)

-- | 强制移除路径上的占位节点（把真实节点替换成链接之前调用）：
--
--   * 符号链接 → unlink（不跟随，目标内容毫发无损）
--   * 普通文件 → removeFile
--   * 真实目录 → 递归整体删除（目录内部的链接只 unlink，不跟随——避免删到根外）
--   * 不存在 → 静默返回
removeEntryForcibly :: FilePath -> IO ()
removeEntryForcibly path = do
    r <- (try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus))
    case r of
        Left _ -> return ()
        Right st
            | isSymbolicLink st -> quiet (removeLink path)
            | isDirectory st -> do
                entries <- listDirectory path
                mapM_ (removeEntryForcibly . (path </>)) entries
                quiet (removeDirectory path)
            | otherwise -> quiet (removeFile path)
  where
    quiet :: IO () -> IO ()
    quiet action = action `catch` handler
    handler :: IOException -> IO ()
    handler _ = return ()

-- | 解析路径**末端**的符号链接（父目录不动）：返回实际应当读写的目标路径。
--
--   为什么需要：文件阶段对"指向文件的链接"是按**指向的文件**比较的，落地时必须写到
--   目标上——writeFileVerified 用 rename(2) 提交，而 rename 替换的是**链接本身**，
--   直接 rename 到链接名会把链接变成真实文件。链接链逐级解析，有上限保护。
resolveLeafLink :: FilePath -> IO FilePath
resolveLeafLink = go (0 :: Int)
  where
    go depth path
        | depth > 8 = return path
        | otherwise = do
            isLink <- isSymLink path
            if not isLink
                then return path
                else do
                    r <- (try (readSymbolicLink path) :: IO (Either IOException FilePath))
                    case r of
                        Left _ -> return path
                        Right t -> do
                            let p  = takeDirectory path
                                t' = normalise (if isAbsolute t then t else p </> t)
                            -- 指向目录时不再往下解析（文件阶段不会把目录当条目写入）
                            isDir <- doesDirectoryExist t'
                            if isDir then return t' else go (depth + 1) t'

-- | 创建/更新符号链接（先校验后落地，与 'writeFileVerified' 完全同构）：
--
--   1. 清掉目标位置的占位节点（若是真实目录 → 整体删除：这正是"文件夹被链接取代"的落地动作）
--   2. 目标目录里建临时链接 → 设**链接自身**的 mtime（lutimes，不跟随）→ 读回指向做哈希校验
--   3. 校验通过才 rename 到目标名
--
--   mode 参数只为与 'writeFileVerified' 保持同一签名：Linux 上链接权限恒为 0777 且不可设置
--   （没有 lchmod，fchmodat(AT_SYMLINK_NOFOLLOW) 返回 ENOTSUP），而 setFileMode 会跟随链接
--   改到**目标文件**的权限上，属于破坏性操作，绝对不能调用。
writeLinkVerified
    :: FilePath       -- ^ 同步根（必须已存在，不会被自动创建）
    -> FilePath       -- ^ 目标链接全路径（@root </> relPath@）
    -> BS.ByteString  -- ^ 链接指向（UTF-8 字节）
    -> Int            -- ^ 目标权限 mode（链接不适用，忽略）
    -> Integer        -- ^ 链接自身 mtime（epoch 秒）
    -> String         -- ^ 期望的 SHA512（指向字符串的哈希）
    -> IO (Either String ())
writeLinkVerified root path targetBytes _mode mtime_ expectedHash = do
    rootExists <- doesDirectoryExist root
    if not rootExists
        then return $ Left ("sync root does not exist: " ++ root)
        else do
            let dir = takeDirectory path
            mk <- ensureDirChain dir
            case mk of
                Left e -> return $ Left e
                Right () -> do
                    -- 占位节点：真实目录必须整体删除，否则 rename 无法覆盖
                    rm <- (try (removeEntryForcibly path) :: IO (Either IOException ()))
                    case rm of
                        Left e -> return $ Left ("cannot replace " ++ path ++ ": " ++ show e)
                        Right () -> withTemp dir $ \tmpPath h -> do
                            hClose h
                            removeFile tmpPath        -- 释放占位名，改为同名符号链接
                            createSymbolicLink (linkTargetPath targetBytes) tmpPath
                            setSymbolicLinkTimesHiRes tmpPath
                                (fromIntegral mtime_) (fromIntegral mtime_)
                            back <- readLinkContent tmpPath
                            if hashBytes back /= expectedHash
                                then do
                                    quiet (removeLink tmpPath)
                                    return $ Left "verification failed before commit; target untouched"
                                else do
                                    renameFile tmpPath path
                                    return $ Right ()
  where
    quiet :: IO () -> IO ()
    quiet action = action `catch` handler
    handler :: IOException -> IO ()
    handler _ = return ()
