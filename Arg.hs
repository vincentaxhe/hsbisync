module Arg where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.List (stripPrefix)
import System.Directory
    ( canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getHomeDirectory
    )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (dropTrailingPathSeparator, pathSeparator)
import System.Process (readProcessWithExitCode)

-- ============================================================================
-- Root 类型
-- ============================================================================

-- | 同步根：本地路径或远程 SSH 主机
data Root
    = Local  !FilePath
    | Remote !String !FilePath   -- host, path
    deriving (Show, Eq)

-- | 显示名称：就是用户写出来的地址形式 —— 本地就是路径本身，远程是 host:path。
--   （没有 @local:/path@ 这种地址表示法，本地端不加任何前缀。）
rootLabel :: Root -> String
rootLabel (Local path)       = path
rootLabel (Remote host path) = host ++ ":" ++ path

-- | 从字符串解析 Root
--   "/path"          → Local "/path"
--   "host:/path"     → Remote "host" "/path"
--   "user@host:/path" → Remote "user@host" "/path"
--
--   只支持正常的 host:path（IPv6 不做特殊处理，如需支持请用 hostname 或 ssh 配置别名）。
parseRoot :: String -> Root
parseRoot s =
    case break (== ':') s of
        (h, ':' : p) | not (null h) && '/' `notElem` h -> Remote h p
        _ -> Local s

-- | 本机路径 → 写进对端地址里的形式
--
--   规则（2026-09 与用户确认）：
--   * 位于家目录**之下** → 相对家目录的路径（既不要绝对路径也不要 `~`：
--     ssh 执行命令时的缺省工作目录就是登录用户的家目录，相对路径天然可用）；
--   * 正好是家目录本身 → "."（同样是缺省目录）；
--   * 其它位置 → 绝对路径。
--
--   只适用于 **host:path** 形式的对端地址（远端会把它交给 shell）。
--   本机 Local 路径不经 shell，绝不能缩写成相对路径。
formatOwnPath :: FilePath -> IO String
formatOwnPath p = do
    absP <- canonicalizePath p
    home <- dropTrailingPathSeparator <$> getHomeDirectory
    return $
        if absP == home
            then "."
            else case stripPrefix (home ++ [pathSeparator]) absP of
                    Just rel | not (null rel) -> rel
                    _                         -> absP

-- ============================================================================
-- 远端目录检查
-- ============================================================================

-- | 检查 Root 是否可达且为目录
--   本地：直接 stat
--   远程：ssh host test -d path
checkRoot :: Root -> IO Bool
checkRoot (Local path)       = doesDirectoryExist path
checkRoot (Remote host path) = do
    (code, _, _) <- readProcessWithExitCode "ssh" [host, "test", "-d", path] ""
    return (code == ExitSuccess)

-- | 确保同步根目录存在；不存在则创建**一层**空目录。
--
--   规则（与 rsync 的 "--mkpath" 默认行为相反，故意的）：
--   * 目标地址（同步根）本身不存在 → 只创建它一层，**要求其父目录已存在**；
--     父目录不存在则报错退出，避免路径写错时凭空造出一整棵树；
--   * 根**内部**的中间目录（同步内容的一部分）则由
--     Write.writeFileVerified 递归创建，不受此限制。
--
--   本地用 createDirectoryIfMissing False（父目录缺失即失败）；
--   远程用 ssh mkdir（不加 -p）。
ensureRoot :: Root -> IO ()
ensureRoot (Local path) = do
    isDir <- doesDirectoryExist path
    if isDir
        then return ()
        else do
            isFile <- doesFileExist path
            when isFile $
                die $ path ++ ": exists but is not a directory"
            r <- (try (createDirectoryIfMissing False path)
                    :: IO (Either IOException ()))
            case r of
                Left _ -> die $ path ++ ": cannot create target root\n"
                    ++ "  (only one level is created: the parent directory must already exist)"
                Right () -> putStrLn $ "  created root: " ++ path
ensureRoot (Remote host path) = do
    let label = host ++ ":" ++ path
    (code, _, _) <- readProcessWithExitCode "ssh" [host, "test", "-d", path] ""
    if code == ExitSuccess
        then return ()
        else do
            -- 不加 -p：只创建目标地址这一层
            (code', _, err) <- readProcessWithExitCode "ssh" [host, "mkdir", path] ""
            when (code' /= ExitSuccess) $
                die $ label ++ ": cannot create remote directory: " ++ err
                   ++ "  (only one level is created: the parent directory must exist)"
            putStrLn $ "  created root: " ++ label

-- | 严格要求 Root 已存在且为目录（不自动创建）。
--
--   用于 profile 免参同步（hsbisync sync <profile>）：对端地址必须真实存在；
--   只有显式两端地址的 sync 才通过 'ensureRoot' 自动创建目录。
requireRoot :: Root -> IO ()
requireRoot root = do
    ok <- checkRoot root
    unless ok $
        die $ rootLabel root ++ ": target does not exist\n"
           ++ "  (auto-create only happens with `hsbisync sync <profile> <root1> <root2>`)"

-- ============================================================================
-- 命令行解析
-- ============================================================================

-- | 解析命令行参数，验证两个 Root 都可达
parseArgs :: IO (Root, Root)
parseArgs = getArgs >>= parseArgsFrom

-- | 从字符串列表解析两个 Root 并验证可达性
parseArgsFrom :: [String] -> IO (Root, Root)
parseArgsFrom args =
    case args of
        [r1, r2] -> do
            let root1 = parseRoot r1
                root2 = parseRoot r2
            ok1 <- checkRoot root1
            ok2 <- checkRoot root2
            case (ok1, ok2) of
                (True,  True)  -> return (root1, root2)
                (False, True)  -> die $ rootLabel root1 ++ ": not accessible"
                (True,  False) -> die $ rootLabel root2 ++ ": not accessible"
                _              -> die $ rootLabel root1 ++ " and " ++ rootLabel root2 ++ ": not accessible"
        _ ->
            die "Usage: hsbisync <root1> <root2>\n  root = /local/path  or  [user@]host:/remote/path"

-- | 验证 profile 名是否合法：不能为空，不能含路径分隔符
validateProfile :: String -> IO ()
validateProfile name
    | null name                 = die "Profile name cannot be empty"
    | '/' `elem` name           = die "Profile name cannot contain '/'"
    | '\\' `elem` name          = die "Profile name cannot contain '\\'"
    | name == "." || name == ".." = die "Profile name cannot be '.' or '..'"
    | otherwise                 = return ()

-- | 从字符串列表解析 profile + 两个 Root，并确保两个同步根目录都存在。
--   [profile, r1, r2] → (profile, root1, root2)
--   目录不存在时自动创建空目录（创建成功才继续）。
parseSyncThree :: [String] -> IO (String, Root, Root)
parseSyncThree [profile, r1, r2] = do
    validateProfile profile
    let root1 = parseRoot r1
        root2 = parseRoot r2
    ensureRoot root1
    ensureRoot root2
    return (profile, root1, root2)
parseSyncThree _ = die "Usage: hsbisync sync <profile> <root1> <root2>"

-- | 打印错误并退出
die :: String -> IO a
die msg = do
    putStrLn $ "Error: " ++ msg
    exitFailure
