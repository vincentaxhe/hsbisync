module Helper where

import Control.Exception (ErrorCall, IOException, catch, displayException)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO
    ( BufferMode(..), hFlush, hLookAhead
    , hSetBinaryMode, hSetBuffering, stdin, stdout
    )

import System.Posix.Files
    ( getFileStatus, fileMode, modificationTime
    , setFileTimes, setFileMode
    )

import Crypto.Hash (hash, SHA512, Digest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import State
    ( FileState(..), StateKind(..)
    , appendAddedKind, appendDeletedKind
    , currentState, currentLinkState, lastStateKind
    , stateBuilder, discardStateUnder
    )
import Write
    ( writeFileVerified, writeLinkVerified, readLinkContent
    , removeLinkSafe, removeFileSafe, writeRepoFile, setMtimeOnly
    , hGetExactly
    )
import qualified Msg

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
        "setstat"   -> cmdSetStat   root req
        "settime"   -> cmdSetTime   root req
        "add"       -> withPath $ cmdAdd    root mTag kind
        "delete"    -> withPath $ cmdDelete root mTag kind
        "unlink"    -> withPath $ cmdUnlink root kind
        "hash"      -> withPath $ cmdHash   root kind
        "send"      -> withPath $ cmdSend   root kind
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

-- | @{"cmd":"state"|"last","kind":…}@ → @{"entries":[[size,mtime,mode,name],…]}@
cmdState :: FilePath -> Maybe String -> StateKind -> IO ()
cmdState root mTag kind = replyOk . entries =<< currentKind kind root mTag

cmdLast :: FilePath -> Maybe String -> StateKind -> IO ()
cmdLast root mTag kind = replyOk . entries =<< lastStateKind kind root mTag

entries :: [FileState] -> [(String, Msg.Value)]
entries st = [("entries", Msg.raw (stateBuilder st))]

currentKind :: StateKind -> FilePath -> Maybe String -> IO [FileState]
currentKind FileKind root mTag = currentState root mTag
currentKind LinkKind root mTag = currentLinkState root mTag

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

-- | 文件：删的是**目标**（'removeFileSafe' 先解析末端链接，保留链接本身）；
--   链接：只 unlink 链接自己。占位不是链接时回 @notLink@，主控端只告警不删。
cmdUnlink :: FilePath -> StateKind -> FilePath -> IO ()
cmdUnlink root kind path = case kind of
    FileKind -> removeFileSafe (root </> path) >> replyOk []
    LinkKind -> do
        ok <- removeLinkSafe (root </> path)
        replyOk (if ok then [] else [("notLink", Msg.JBool True)])

-- ============================================================================
-- HASH
-- ============================================================================

cmdHash :: FilePath -> StateKind -> FilePath -> IO ()
cmdHash root kind path = do
    content <- kindContent kind root path
    replyOk [("hash", Msg.str (show (hash content :: Digest SHA512)))]

-- | 条目的"内容"：文件 = 文件字节；链接 = 指向字符串
kindContent :: StateKind -> FilePath -> FilePath -> IO BS.ByteString
kindContent FileKind root path = BS.readFile (root </> path)
kindContent LinkKind root path = readLinkContent (root </> path)

-- ============================================================================
-- SEND：{"ok":true,"size":N} + N 字节裸数据
-- ============================================================================

cmdSend :: FilePath -> StateKind -> FilePath -> IO ()
cmdSend root kind path = do
    content <- kindContent kind root path
    replyOk [("size", Msg.int (fromIntegral (BS.length content)))]
    BS.hPut stdout content
    hFlush stdout

-- ============================================================================
-- RECV：两步 —— 先 {"ok":true} 表示准备好，收完载荷再回一条结果
-- ============================================================================

-- | @{"cmd":"recv","path":…,"size":…,"mode":…,"mtime":…,"hash":…}@
--
--   先回 OK 再收数据：失败时目标文件不受影响、协议也不会错位
--   （父进程从第二步的响应里拿到 error）。链接忽略 mode（Linux 上不可设）。
cmdRecv :: FilePath -> Maybe String -> StateKind -> Msg.Value -> FilePath -> IO ()
cmdRecv root mTag kind req path =
    case ( Msg.fieldInt "size" req, Msg.fieldInt "mode" req
         , Msg.fieldInt "mtime" req, Msg.fieldStr "hash" req ) of
        (Just sz, Just md, Just mt, Just h) -> do
            replyOk []                                   -- 准备好接收
            content <- hGetExactly stdin (fromIntegral sz)
            let write = case kind of
                    FileKind -> writeFileVerified
                    LinkKind -> writeLinkVerified
            result <- write root (root </> path) content (fromIntegral md) mt h
            case result of
                Right () -> do
                    -- 链接刚建/改：本端该链接子树下的文件基线条目已过期，就地作废
                    when (kind == LinkKind) $ discardStateUnder root mTag path
                    replyOk []
                Left err -> replyErr err
        _ -> replyErr "recv: missing size/mode/mtime/hash"

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
                    , ("hash",   Msg.str (show (hash content :: Digest SHA512))) ]

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

-- | 登录方（主控端）地址，取自 sshd 设置的环境变量。
--   SSH_CONNECTION 是现代 sshd 的标准变量；SSH_CLIENT 是旧变量，
--   两者首字段都是客户端地址。
loginClientAddr :: IO (Maybe String)
loginClientAddr = do
    mConn   <- lookupEnv "SSH_CONNECTION"
    mClient <- lookupEnv "SSH_CLIENT"
    return $ listToMaybe [ ip | Just s <- [mConn, mClient], ip : _ <- [words s] ]
