module Helper where

import Control.Exception (ErrorCall, IOException, catch, displayException, try)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO
    ( BufferMode(..), IOMode(ReadMode), hClose, hFileSize, hFlush, hLookAhead
    , hSetBinaryMode, hSetBuffering, openBinaryFile, stdin, stdout
    )

import System.Posix.Files
    ( getFileStatus, fileMode, modificationTime
    , setFileTimes, setFileMode
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import State
    ( FileState(..), StateKind(..)
    , appendAddedKind, appendDeletedKind
    , currentStateWith, currentLinkState, lastStateKindT
    , linkTableOf, scanRelLinks, readAllProfilesIgnore
    , stateBuilder, discardStateUnder
    )
import Write
    ( writeLinkVerified, readLinkContent
    , removeLinkSafe, removeFileSafe, writeRepoFile, setMtimeOnly
    , hGetExactly, hashBytes, hashFile
    , copyExactlyHash, padZeros, openTargetTemp, commitReceived, removeQuietly
    )
import qualified Msg
import qualified LinkTable
import qualified Data.Map as Map
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8With)
import qualified Data.Text.Encoding.Error as TEE (lenientDecode)

-- ============================================================================
-- 入口
-- ============================================================================

-- | 以 helper 模式运行，通过 stdin/stdout 与主控端通信。
--
--   协议：**一行一个规范 JSON**（见 'Msg'）
--     * 请求：{"cmd":"…", …}
--     * 响应：{"ok":true,…} / {"ok":false,"error":"…"}
--     * 文件内容不走 JSON：send/recv 的响应里给出 size，紧跟其后的就是裸字节载荷
--
--   句柄全程二进制（协议是字节流，不经过文本编码层）——既没有
--   hSetBinaryMode/hSetEncoding 来回切换的坑，也不受远端 locale 影响。
runHelper :: FilePath -> Maybe String -> IO ()
runHelper root mTag = do
    -- 注意：**这里绝不创建任何目录**。
    --
    --   以前这里有一句 createDirectoryIfMissing True <root>/.bisync/<profile>，
    --   而它是**递归**创建 —— 等于：只要跟远端握一次手，同步根（连同其父目录、
    --   连同 .bisync）就被建出来了。后果有两层：
    --   * 第 1 步（sync prepare）"检查/创建同步目录"永远查不出"目录不存在"，"created root" 再也
    --     不会出现（用户看到的顺序是：握手 → 说好的创建 → 结果什么都没做）；
    --   * 更糟的是它绕过了"只建一层"的规则（见 'Arg.ensureRoot'），路径写错时
    --     会凭空造出一整棵树。
    --   状态目录该由真正要写它的地方按需创建：'State.writeEntries' /
    --   'State.appendAddedKind' / 'State.appendDeletedKind' / 'writeRepoFile' 都自带
    --   createDirectoryIfMissing，只读路径（当前扫描、读 last、GETIGNORE）不需要它。
    hSetBinaryMode stdout True
    hSetBinaryMode stdin  True
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering

    -- 打招呼 = 自报家门：自己这个**正在运行的文件**的真实路径（Linux 下来自
    -- /proc/self/exe，已解析符号链接）+ 家目录。主控端据此知道"远端这份
    -- hsbisync 到底是哪个文件"，之后直接按绝对路径调用，不用再查 PATH。
    exe  <- getExecutablePath
    home <- fromMaybe "" <$> lookupEnv "HOME"
    reply (Msg.obj [ ("ok",   Msg.JBool True)
                   , ("exe",  Msg.str exe)
                   , ("home", Msg.str home) ])

    commandLoop root mTag

-- | 回一条响应（一行规范 JSON）
reply :: Msg.Value -> IO ()
reply v = BL.hPut stdout (Msg.encodeLine v) >> hFlush stdout

replyOk :: [(String, Msg.Value)] -> IO ()
replyOk kvs = reply (Msg.obj (("ok", Msg.JBool True) : kvs))

replyErr :: String -> IO ()
replyErr e = reply (Msg.obj [("ok", Msg.JBool False), ("error", Msg.str e)])

-- | 命令循环：读一行请求 → 执行 → 回一行响应
commandLoop :: FilePath -> Maybe String -> IO ()
commandLoop root mTag = go
  where
    go = do
        done <- isEOF
        unless done $ do
            line <- B8.hGetLine stdin
            case Msg.decodeStrict line of
                Left e    -> replyErr ("bad request: " ++ e)
                Right req -> (dispatch req `catch` ioErr) `catch` callErr
            go

    ioErr :: IOException -> IO ()
    ioErr e = replyErr (show e)

    callErr :: ErrorCall -> IO ()
    callErr e = replyErr (displayException e)

    dispatch req = case Msg.fieldStr "cmd" req of
        Nothing  -> replyErr "missing \"cmd\""
        Just cmd -> case kindOf req of
            Left e     -> replyErr e
            Right kind -> dispatch' cmd kind req

    -- 需要 "path" 的命令走 withPath；其余（state/last/getignore/putrepo…）不需要
    dispatch' cmd kind req = case cmd of
        "quit"      -> replyOk []
        "state"     -> cmdState     root mTag kind
        "last"      -> cmdLast      root mTag kind
        "getignore" -> cmdGetIgnore root mTag
        "putrepo"   -> cmdPutRepo   root mTag req
        "jail"      -> cmdJail      root
        "setstat"   -> cmdSetStat   root req
        "settime"   -> cmdSetTime   root req
        "add"       -> withPath $ cmdAdd    root mTag kind
        "delete"    -> withPath $ cmdDelete root mTag kind
        "unlink"    -> withPath $ cmdUnlink root mTag kind
        "hash"      -> withPath $ cmdHash   root mTag kind
        "send"      -> withPath $ cmdSend   root mTag kind
        "recv"      -> withPath $ cmdRecv   root mTag kind req
        "getstat"   -> withPath $ cmdGetStat root
        other       -> replyErr ("unknown command: " ++ other)
      where
        withPath k = case Msg.fieldStr "path" req of
            Just p  -> k p
            Nothing -> replyErr "missing \"path\""

    -- "kind"：file（默认）或 link —— 文件与链接在协议层面是同一套命令，
    -- 差别只在"内容是什么"（文件字节 vs 指向字符串）与"怎么落地"。
    kindOf req = case Msg.fieldStr "kind" req of
        Just "link" -> Right LinkKind
        Just "file" -> Right FileKind
        Nothing     -> Right FileKind
        Just other  -> Left ("unknown kind: " ++ other)

isEOF :: IO Bool
isEOF = catch (hLookAhead stdin >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler _ = return True

-- ============================================================================
-- 状态（state = 本次扫描，last = 上次基线）
-- ============================================================================

-- | @{"cmd":"state","kind":…}@ → @{"entries":[…]}@（文件扫描还可能带 @rmdir@，见下）
--
--   文件扫描（'FileKind'）会**顺带清理空目录**（见 'State.currentStateWith'），
--   所以响应会多带一个 @rmdir@：本次**发起过的每一次 rmdir**，形如
--   @[{"相对路径":true}, …]@ —— 数组保证顺序（深 → 浅），元素是单个键值对
--   （键 = 路径，值 = 是否真的删掉）。不只是成功的 —— 失败意味着
--   "判定为空之后必然成功"被打破，那正是主控端要看见的信号。
--
--   一个路径在一次扫描里最多只会发起一次 rmdir，所以键不会重复。
--   **一次都没发起就整个字段都不出现**：老主控端会忽略这个未知键，
--   新主控端把字段缺失当空列表，两个方向都兼容。
--   远端 helper 的 stdout 就是协议，不能自己打印，只能回报给主控端统一展示。
cmdState :: FilePath -> Maybe String -> StateKind -> IO ()
cmdState root mTag kind = do
    (sts, rmdirs) <- currentKind kind root mTag
    replyOk $ [ ("entries", Msg.raw (stateBuilder sts)) ]
           ++ [ ("rmdir", Msg.arr [ Msg.obj [(p, Msg.JBool ok)] | (p, ok) <- rmdirs ])
              | not (null rmdirs) ]

-- | @{"cmd":"last","kind":…}@ → @{"entries":[…]}@（读基线：不扫描、不清理）
--
--   链接（'LinkKind'）还可能多带一个 @tombstones@：本轮读状态时被 @deleted@ 抹掉的
--   条目（同 @entries@ 的数组形状）。主控端用它把「删掉的链接」再传播一步给仍持有
--   同一条链接的对端；文件侧忽略这个字段。一次都没有就整个字段不出现。
cmdLast :: FilePath -> Maybe String -> StateKind -> IO ()
cmdLast root mTag kind = do
    (sts, tombs) <- lastStateKindT kind root mTag
    replyOk $ entries sts
           ++ [ ("tombstones", Msg.raw (stateBuilder tombs))
              | kind == LinkKind, not (null tombs) ]

entries :: [FileState] -> [(String, Msg.Value)]
entries st = [("entries", Msg.raw (stateBuilder st))]

currentKind :: StateKind -> FilePath -> Maybe String -> IO ([FileState], [(FilePath, Bool)])
currentKind FileKind root mTag = currentStateWith True root mTag
currentKind LinkKind root mTag = (\s -> (s, [])) <$> currentLinkState root mTag

-- ============================================================================
-- ADD / DELETE / UNLINK
-- ============================================================================

cmdAdd :: FilePath -> Maybe String -> StateKind -> FilePath -> IO ()
cmdAdd root mTag kind path = do
    appendAddedKind kind root mTag path
    replyOk []

cmdDelete :: FilePath -> Maybe String -> StateKind -> FilePath -> IO ()
cmdDelete root mTag kind path = do
    appendDeletedKind kind root mTag path
    replyOk []

-- | 链接的"内容"：普通仓库读真实链接；链接表模式查表
linkContent :: FilePath -> FilePath -> IO BS.ByteString
linkContent root relPath = do
    mtbl <- linkTableOf root
    case mtbl of
        Nothing -> readLinkContent (root </> relPath)
        Just tbl -> case Map.lookup relPath tbl of
            Nothing -> ioError (userError ("link table: no entry for " ++ relPath))
            Just e  -> return (T.encodeUtf8 (T.pack (LinkTable.entryTarget e)))

-- | 指向字符串的字节 → String（与 'linkContent' 对称）
targetText :: BS.ByteString -> String
targetText = T.unpack . T.decodeUtf8With TEE.lenientDecode

-- | 文件：删的是**目标**（'removeFileSafe' 先解析末端链接，保留链接本身）；
--   链接：只 unlink 链接自己。占位不是链接时回 @notLink@，主控端只告警不删。
cmdUnlink :: FilePath -> Maybe String -> StateKind -> FilePath -> IO ()
cmdUnlink root _mTag kind path = case kind of
    FileKind -> removeFileSafe (root </> path) >> replyOk []
    LinkKind -> do
        mtbl <- linkTableOf root
        case mtbl of
            Nothing -> do
                ok <- removeLinkSafe (root </> path)
                replyOk (if ok then [] else [("notLink", Msg.JBool True)])
            Just tbl -> do
                let (tbl', had) = LinkTable.delEntry path tbl
                when had $ LinkTable.save root tbl'
                replyOk (if had then [] else [("notLink", Msg.JBool True)])

-- ============================================================================
-- HASH
-- ============================================================================

-- | @{"cmd":"hash",…}@ → @{"hash":"…"}@
--
--   文件走**流式**哈希（内存 O(1)）；链接的内容就是指向字符串，很短。
cmdHash :: FilePath -> Maybe String -> StateKind -> FilePath -> IO ()
cmdHash root _mTag kind path = case kind of
    LinkKind -> do
        content <- linkContent root path
        replyOk [("hash", Msg.str (hashBytes content))]
    FileKind -> do
        r <- try (hashFile (root </> path))
        case r of
            Left e -> replyErr ("hash: " ++ show (e :: IOException))
            Right h -> replyOk [("hash", Msg.str h)]

-- ============================================================================
-- SEND：{"ok":true,"size":N} + N 字节裸数据 + {"ok":true,"hash":…}
-- ============================================================================

-- | 发送内容：先报 size，再**流式**发载荷（边读边算哈希），最后补一条哈希尾行。
--
--   读源出错 / 提前 EOF 时用 'padZeros' 把帧补齐 —— 接收端长度仍对齐，而哈希
--   必然对不上，于是它自己删临时文件、回错误（比截短载荷安全得多）。
cmdSend :: FilePath -> Maybe String -> StateKind -> FilePath -> IO ()
cmdSend root _mTag kind path = case kind of
    LinkKind -> do
        content <- linkContent root path
        replyOk [("size", Msg.int (fromIntegral (BS.length content)))]
        BS.hPut stdout content
        hFlush stdout
        replyOk [("hash", Msg.str (hashBytes content))]
    FileKind -> do
        r <- try (openBinaryFile (root </> path) ReadMode)
        case r of
            Left e -> replyErr ("send: " ++ show (e :: IOException))
            Right h -> do
                sz <- hFileSize h
                replyOk [("size", Msg.int sz)]
                (hsh, n) <- copyExactlyHash h (Just stdout) sz
                hClose h
                when (n < sz) $ padZeros stdout (sz - n)
                hFlush stdout
                replyOk [("hash", Msg.str hsh)]

-- ============================================================================
-- RECV：先 {"ok":true} 表示准备好，收完载荷 + 哈希尾行再回结果
-- ============================================================================

-- | @{"cmd":"recv","path":…,"size":…,"mode":…,"mtime":…}@
--
--   **先准备好临时文件再回 OK**：准备失败（目录建不出来 / 根不存在）就直接回错，
--   此时主控端还一个字节都没发 → 协议不会错位，也不需要额外排空。
--   收载荷时边收边写盘边哈希；收完读发送端算的哈希尾行，一致才 rename。
cmdRecv :: FilePath -> Maybe String -> StateKind -> Msg.Value -> FilePath -> IO ()
cmdRecv root mTag kind req path =
    case ( Msg.fieldInt "size" req, Msg.fieldInt "mode" req
         , Msg.fieldInt "mtime" req ) of
        (Just sz, Just md, Just mt) -> case kind of
            FileKind -> recvFile sz (fromIntegral md) mt
            LinkKind -> recvLink sz (fromIntegral md) mt
        _ -> replyErr "recv: missing size/mode/mtime"
  where
    full = root </> path

    -- 文件：边收边写边哈希，内存 O(1)
    recvFile sz md mt = do
        prep <- openTargetTemp root full
        case prep of
            Left e -> replyErr e
            Right (target, tmpPath, h) -> do
                replyOk []                              -- 准备好接收
                (actual, n) <- copyExactlyHash stdin (Just h) sz
                hFlush h
                hClose h
                if n < sz
                    then do
                        removeQuietly tmpPath
                        replyErr "recv: connection closed before payload completed"
                    else do
                        mh <- readHashTrailer
                        case mh of
                            Nothing -> do
                                removeQuietly tmpPath
                                replyErr "recv: missing hash trailer"
                            Just expected -> do
                                r <- commitReceived target tmpPath md mt actual expected
                                either replyErr (const (replyOk [])) r

    -- 链接：内容很短（指向字符串），照旧整块；落地流程不变
    --
    --   **必须先回 OK 再读载荷**（与 recvFile 对称）：主控端发完请求会等这一行，
    --   少了它两边互等，表现为"链接阶段确认后静默卡死"。
    --
    --   链接表模式（jail 型仓库）：不建真实链接，只往表里写一条；
    --   内容照旧由文件阶段写成真实目录 —— 所以这里也不用作废子基线。
    recvLink sz md mt = do
        replyOk []
        content <- hGetExactly stdin (fromIntegral sz)
        mh <- readHashTrailer
        case mh of
            Nothing -> replyErr "recv: missing hash trailer"
            Just expected -> do
                mtbl <- linkTableOf root
                case mtbl of
                    Just tbl
                        | hashBytes content /= expected ->
                            replyErr "verification failed before commit; target untouched"
                        | otherwise -> do
                            LinkTable.save root
                                (LinkTable.putEntry path
                                    (LinkTable.Entry (targetText content)
                                                     (fromIntegral (BS.length content)) mt) tbl)
                            replyOk []
                    Nothing -> do
                        r <- writeLinkVerified root full content md mt expected
                        case r of
                            Right () -> do
                                -- 链接刚建/改：本端该链接子树下的文件基线条目已过期，就地作废
                                discardStateUnder root mTag path
                                replyOk []
                            Left err -> replyErr err

-- | 载荷之后那条哈希尾行：@{"ok":true,"hash":"…"}@
readHashTrailer :: IO (Maybe String)
readHashTrailer = do
    line <- B8.hGetLine stdin
    return $ case Msg.decodeStrict line of
        Right v -> Msg.fieldStr "hash" v
        Left _  -> Nothing

-- ============================================================================
-- GETSTAT / SETSTAT / SETTIME
-- ============================================================================

cmdGetStat :: FilePath -> FilePath -> IO ()
cmdGetStat root path = do
    fs <- getFileStatus (root </> path)
    replyOk [ ("mtime", Msg.int (floor (realToFrac (modificationTime fs) :: Double)))
            , ("mode",  Msg.int (fromIntegral (fileMode fs .&. 0o7777))) ]

cmdSetStat :: FilePath -> Msg.Value -> IO ()
cmdSetStat root req =
    case (Msg.fieldStr "path" req, Msg.fieldInt "mtime" req, Msg.fieldInt "mode" req) of
        (Just path, Just mt, Just md) -> do
            let full = root </> path
            setFileTimes full (fromIntegral mt) (fromIntegral mt)
            setFileMode full (fromIntegral md)
            replyOk []
        _ -> replyErr "setstat: missing path/mtime/mode"

-- | 只把 mtime/atime 对齐到给定秒数（不动内容、不动 mode）。
--
--   用于内容校验钩子：内容实质相同、只差时间戳时，把较旧那端的 mtime 对齐到
--   较新那端，使两端 (size, mtime) 一致（之后记成 added 即彻底收敛）。
cmdSetTime :: FilePath -> Msg.Value -> IO ()
cmdSetTime root req =
    case (Msg.fieldStr "path" req, Msg.fieldInt "mtime" req) of
        (Just path, Just mt) -> setMtimeOnly (root </> path) mt >> replyOk []
        _ -> replyErr "settime: missing path/mtime"

-- ============================================================================
-- GETIGNORE — 读取 .bisync/<profile>/ignore 元信息（供 ignore 预同步比较）
-- ============================================================================

-- | @{"cmd":"getignore"}@ → @{"exists":false}@ 或
--   @{"exists":true,"mtime":…,"mode":…,"hash":…}@
cmdGetIgnore :: FilePath -> Maybe String -> IO ()
cmdGetIgnore root mTag = do
    let ignoreFile = root </> ".bisync" </> fromMaybe "" mTag </> "ignore"
    exists <- doesFileExist ignoreFile
    if not exists
        then replyOk [("exists", Msg.JBool False)]
        else do
            fs <- getFileStatus ignoreFile
            content <- BS.readFile ignoreFile
            replyOk [ ("exists", Msg.JBool True)
                    , ("mtime",  Msg.int (floor (realToFrac (modificationTime fs) :: Double)))
                    , ("mode",   Msg.int (fromIntegral (fileMode fs .&. 0o7777)))
                    , ("hash",   Msg.str (hashBytes content)) ]

-- ============================================================================
-- PUTREPO — 远端把自己的 .bisync/<profile>/repo 写成"登录方（主控端）地址"
-- ============================================================================

-- | @{"cmd":"putrepo","user":<登录方用户名>,"path":<登录方路径>}@
--   让**远端**也拥有 repo 文件，这样远端以后直接 `hsbisync sync <profile>`
--   就能免参反向同步。
--
--     user = 登录方（主控端）本机登录用户名，反向 ssh 时要用这个名字
--     path = 登录方同步根路径；家目录之下是相对路径（不带 ~），否则绝对路径
--            ——具体由主控端按 'formatOwnPath' 决定后传过来
--
--   为什么地址要由 helper 自己拼：只有登录方才可能知道"对面看到的我是谁"，
--   而这个信息就在 sshd 设置的环境变量里：
--       SSH_CONNECTION = "<client_addr> <client_port> <server_addr> <server_port>"
--   因此 host 部分由本端补上，主控端只提供用户名与路径。
--   取不到（非 ssh 场景，如本地测试用的假 ssh）就不写。
--
--   响应：{"addr":…} 或 {"addr":…,"unchanged":true}（已经是这个地址，主控端就不打印）
--         或 {"noclient":true}（取不到登录方地址，只提示）
cmdPutRepo :: FilePath -> Maybe String -> Msg.Value -> IO ()
cmdPutRepo root mTag req =
    case (Msg.fieldStr "user" req, Msg.fieldStr "path" req) of
        (Just user, Just path) | not (null user) && not (null path) -> do
            mip <- loginClientAddr
            case mip of
                Nothing -> replyOk [("noclient", Msg.JBool True)]
                Just ip -> do
                    let repoFile = root </> ".bisync" </> fromMaybe "" mTag </> "repo"
                        addr     = user ++ "@" ++ ip ++ ":" ++ path
                    same <- repoAlready repoFile addr
                    if same
                        then -- 内容没变：不写、也不让主控端打印（第 1 步只报有变化的动作）
                            replyOk [("addr", Msg.str addr), ("unchanged", Msg.JBool True)]
                        else do
                            writeRepoFile repoFile addr
                            replyOk [("addr", Msg.str addr)]
        _ -> replyErr "putrepo: missing user/path"

-- | 现有 repo 文件是否已经是这个地址（比字节，忽略首尾空白/换行）
repoAlready :: FilePath -> String -> IO Bool
repoAlready fp addr = do
    e <- doesFileExist fp
    if not e
        then return False
        else (== B8.pack addr) . B8.strip <$> BS.readFile fp

-- ============================================================================
-- JAIL — 主控端 --jail 指向远端时，只保证表存在（空表）
-- ============================================================================

-- | @{"cmd":"jail"}@ → @{"created":true|false}@
--
--   表已存在 → created=false（主控端不重复提示）。
--   该端还有真实链接 → 报错，让用户先在那台机器上跑 @hsbisync jail@，
--   否则空表会让这些链接判成“消失”并向对端传播删除。
--   链接表是**仓库级**的（@.bisync/link_table.json@），与 profile 无关。
cmdJail :: FilePath -> IO ()
cmdJail root = do
    ok <- LinkTable.exists root
    if ok
        then replyOk [("created", Msg.JBool False)]
        else do
            ign <- readAllProfilesIgnore root
            old <- scanRelLinks Nothing root root ign
            if not (null old)
                then replyErr ("jail: " ++ show (length old)
                    ++ " real symlink(s) still here; run `hsbisync jail` on that host first")
                else do
                    createDirectoryIfMissing True (root </> ".bisync")
                    LinkTable.save root LinkTable.isEmptyTable
                    replyOk [("created", Msg.JBool True)]

-- | 登录方（主控端）地址，取自 sshd 设置的环境变量。
--   SSH_CONNECTION 是现代 sshd 的标准变量；SSH_CLIENT 是旧变量，
--   两者首字段都是客户端地址。
loginClientAddr :: IO (Maybe String)
loginClientAddr = do
    mConn   <- lookupEnv "SSH_CONNECTION"
    mClient <- lookupEnv "SSH_CLIENT"
    return $ listToMaybe [ ip | Just s <- [mConn, mClient], ip : _ <- [words s] ]
