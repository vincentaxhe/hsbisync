-- | 远端连接层：只负责"怎么在对面把 hsbisync 跑起来"。
--
--   * 查找：问远端**登录 shell**"hsbisync 在哪"（`command -v hsbisync`）。
--     这是 Linux 下唯一且最根本的办法 —— 不猜任何目录，由真正会去执行它的
--     那个 shell 回答；没有就是没有（直接报错，不做任何兜底猜测）。
--   * 派生/销毁远端 helper 进程（`ssh '<绝对路径>' --helper …`）。
--   * 握手：helper 打完招呼才说明它真的起来了；招呼里还自报自己的真实路径
--     （Linux 下就是 /proc/self/exe）与家目录，此后一律按这个绝对路径直接调用。
module Connect where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_)
import Data.List (nubBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode(..))
import System.FilePath (isAbsolute)
import System.IO (hClose, hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, proc, readProcessWithExitCode )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Arg (Root(..), die)
import Write (HelperHandle(..), sendMsg)
import qualified Msg

-- ============================================================================
-- 查找：问远端的登录 shell
-- ============================================================================

-- | 让远端**登录 shell** 自己回答 hsbisync 在哪（`command -v hsbisync`）。
--
--   这是 Linux 下最根本的查找方式：不猜任何目录，而是问真正会去执行它的那个
--   shell（`command -v` 是 POSIX 规定内建，bash/dash/busybox ash 都有）。
--   走登录 shell（`$SHELL -lc`）而不是非交互 ssh 的默认环境，因为 PATH 是登录
--   会话配置出来的（/etc/profile、~/.profile、~/.bash_profile…），用户"登录后
--   敲 hsbisync"用的就是这份 PATH。
--
--   回答取**最后一行**非空输出：profile 脚本可能先打印别的东西，而命令自己的
--   输出总在最后。答不出绝对路径（没装，或答的是 alias/function/内建）→ Left。
remoteExeLookup :: String -> IO (Either String FilePath)
remoteExeLookup host = do
    (code, out, err) <- readProcessWithExitCode "ssh" [host, remoteCmd] ""
    let answer = lastNonEmpty (lines out)
    return $
        if code == ExitSuccess && isAbsolute answer
            then Right answer
            else Left (note answer err)
  where
    remoteCmd = "\"$SHELL\" -lc " ++ shellQuote lookupCmd
    note answer err = unlines $
        [ "cannot find hsbisync on " ++ host
        , "  asked the remote login shell: $SHELL -lc '" ++ lookupCmd ++ "'"
        , "  it answered: " ++ (if null answer then "(nothing)" else answer)
        ]
        ++ (if null err then [] else ["  stderr: " ++ err])
        ++ [ "  hsbisync must be runnable as `hsbisync` by a login shell on that host"
           , "  (deploy the same version somewhere on that PATH)."
           ]

lookupCmd :: String
lookupCmd = "command -v hsbisync"

-- | 最后一行非空输出（profile 脚本可能先打印东西）
lastNonEmpty :: [String] -> String
lastNonEmpty ls = case filter (not . null) ls of
    [] -> ""
    xs -> last xs

-- | 把字符串包成**一个** shell 单词（单引号包裹，内部单引号按 '\'' 转义）。
--
--   远端命令是交给远端 shell 重新解析的（ssh 收到的只是一条字符串），
--   路径/参数含空格时靠它才不会被拆开。
shellQuote :: String -> String
shellQuote s = "'" ++ concatMap esc s ++ "'"
  where
    esc '\'' = "'\\''"
    esc c    = [c]

-- | 远端 helper 的命令行（ssh 的第二个参数，由远端 shell 解析）。
--   用的是**绝对路径**（登录 shell 查出来的，或 helper 自己报出来的），
--   路径与参数各自包成单个 shell 单词。
remoteHelperCommand :: FilePath -> [String] -> String
remoteHelperCommand exe args = unwords (map shellQuote (exe : args))

-- ============================================================================
-- 握手：helper 自报"我在哪"
-- ============================================================================

-- | 一次握手拿到的远端信息
data RemoteEnv = RemoteEnv
    { reExe  :: Maybe FilePath  -- ^ 远端 hsbisync 的绝对路径
    , reHome :: Maybe String    -- ^ 远端家目录
    }
    deriving (Show, Eq)

-- | 进程级缓存：同一个 host 只问一次、只握手一次。
--   之后所有 helper 派生（withHelpers / readSideK / recordRemoteRepo …）
--   都直接复用这里的绝对路径，不再各查一次 PATH。
remoteEnvs :: MVar (Map String RemoteEnv)
remoteEnvs = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE remoteEnvs #-}

cachedRemoteEnv :: String -> IO (Maybe RemoteEnv)
cachedRemoteEnv host = Map.lookup host <$> readMVar remoteEnvs

rememberRemoteEnv :: String -> RemoteEnv -> IO ()
rememberRemoteEnv host re = modifyMVar_ remoteEnvs (return . Map.insert host re)

-- | 准备阶段：为每个远程端各做一次握手（静默，结果进缓存）；本地端跳过。
--   可重复调用：同一进程内只握手一次（之后不再 ssh）。显示用 'handshakeInfo'。
prepareRemotes :: String -> [Root] -> IO ()
prepareRemotes profile roots =
    mapM_ (uncurry (prepareRemote profile)) (nubBy sameHost [ (h, p) | Remote h p <- roots ])
  where
    sameHost (a, _) (b, _) = a == b

-- | 对单个远端握手一次（静默：结果只进缓存，由调用方决定怎么显示）。
--
--   握手 = 让远端登录 shell 说出 hsbisync 在哪（'remoteExeLookup'），然后用这个
--   绝对路径把它叫起来；它会自报自己的真实路径与家目录。
--   远端没有 hsbisync / 起不来 → 在这里直接报错退出：这是明确的前置条件检查，
--   不是"先试着用、失败了再回头找"。
prepareRemote :: String -> String -> FilePath -> IO ()
prepareRemote profile host rootPath = do
    known <- cachedRemoteEnv host
    case known of
        Just _   -> return ()          -- 同一进程内已经握过手
        Nothing  -> do
            hh <- spawnHelper host rootPath profile
            destroyHelper hh

-- | 握手结果（阶段 1 的显示数据）：每个远程端一项 @(host, 家目录, hsbisync 路径)@。
--
--   只列远程端（本地端没有远端可握手，直接跳过），同一个 host 只出现一次。
--   入口（Main）可能已经握过手（写 repo 需要），这里直接读缓存、不再 ssh；
--   显示与握手分开，是为了保证"先显示两端地址、再显示第 1 步"的顺序稳定。
handshakeInfo :: [Root] -> IO [(String, Maybe String, Maybe FilePath)]
handshakeInfo roots =
    forM (nubBy sameHost [ (h, p) | Remote h p <- roots ]) $ \(h, _) -> do
        env' <- fromMaybe (RemoteEnv Nothing Nothing) <$> cachedRemoteEnv h
        return (h, reHome env', reExe env')
  where
    sameHost (a, _) (b, _) = a == b

-- | 用来启动远端 helper 的绝对路径：
--   ① 缓存里有（之前查过 / helper 自报过）→ 直接用；
--   ② 没有 → 问一次远端的登录 shell 并记进缓存。
remoteHelperPath :: String -> IO FilePath
remoteHelperPath host = do
    cached <- cachedRemoteEnv host
    case cached >>= reExe of
        Just exe -> return exe
        Nothing  -> do
            exe <- either die return =<< remoteExeLookup host
            rememberRemoteEnv host (RemoteEnv (Just exe) (cached >>= reHome))
            return exe

-- ============================================================================
-- helper 进程
-- ============================================================================
spawnHelper :: String -> FilePath -> String -> IO HelperHandle
spawnHelper host path profileName = do
    exe <- remoteHelperPath host
    let helperArgs = ["--helper", "--passcode=dontrunyourself", "--root=" ++ path]
                     ++ (if null profileName then [] else ["--tag=" ++ profileName])
        cmdArgs = (proc "ssh" [host, remoteHelperCommand exe helperArgs])
            { std_in  = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    (Just hin, Just hout, _, _) <- createProcess cmdArgs
    -- 协议是字节流（JSON 行 + 裸载荷）：句柄全程二进制，不过文本编码层
    hSetBinaryMode hin  True
    hSetBinaryMode hout True
    -- helper 若启动即崩溃（版本不一致 / locale 非 UTF-8 / ssh 认证失败），
    -- 这里会读到管道 EOF，给出可读错误而不是裸的 "hGetLine: end of file"。
    line <- B8.hGetLine hout `catch` helperStartFailure host path exe
    rememberGreeting host line
    return $ HelperHandle hin hout

destroyHelper :: HelperHandle -> IO ()
destroyHelper hh = do
    ignoreIO $ sendMsg hh (Msg.cmd "quit" [])
    ignoreIO $ hClose (hhIn hh)
    ignoreIO $ hClose (hhOut hh)

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = return ()

-- | helper 打招呼那一行（自报）→ 进缓存：路径以它为准（比 shell 查出来的权威）。
--   响应形如 @{"ok":true,"exe":…,"home":…}@。
rememberGreeting :: String -> BS.ByteString -> IO ()
rememberGreeting host line =
    case Msg.decodeStrict line of
        Right v | Just selfPath <- Msg.fieldStr "exe" v -> do
            old <- cachedRemoteEnv host
            let mHome = case Msg.fieldStr "home" v of
                            Just h | not (null h) -> Just h
                            _                     -> old >>= reHome
            rememberRemoteEnv host (RemoteEnv (Just selfPath) mHome)
        _ -> return ()

-- | 远端 helper 启动失败时的可读诊断。
--   启动用的是问出来的绝对路径，所以这里不再列一堆猜出来的目录。
helperStartFailure :: String -> FilePath -> FilePath -> IOException -> IO a
helperStartFailure host path exe e = error $ unlines
    [ "cannot start remote helper on " ++ host ++ " (root=" ++ path ++ "): " ++ show e
    , "  remote binary: " ++ exe
    , "  possible causes:"
    , "    * the remote binary is not the same version as the local one"
    , "    * ssh authentication failed (see the remote stderr above)"
    , "    * remote locale is not UTF-8 (ssh non-interactive sessions often have LANG unset),"
    , "      making the helper crash while reading UTF-8 state/ignore files"
    ]
