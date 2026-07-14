module Arg where

import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.Process (readProcessWithExitCode)

-- ============================================================================
-- Root 类型
-- ============================================================================

-- | 同步根：本地路径或远程 SSH 主机
data Root
    = Local  !FilePath
    | Remote !String !FilePath   -- host, path
    deriving (Show, Eq)

-- | 显示名称
rootLabel :: Root -> String
rootLabel (Local path)       = "local:" ++ path
rootLabel (Remote host path) = host ++ ":" ++ path

-- | 从字符串解析 Root
--   "/path"          → Local "/path"
--   "host:/path"     → Remote "host" "/path"
--   "user@host:/path" → Remote "user@host" "/path"
parseRoot :: String -> Root
parseRoot s =
    case break (== ':') s of
        (h, ':' : p) | not (null h) && '/' `notElem` h -> Remote h p
        _ -> Local s

-- | 检查 Root 是否可达且为目录
--   本地：直接 stat
--   远程：ssh host test -d path
checkRoot :: Root -> IO Bool
checkRoot (Local path)       = doesDirectoryExist path
checkRoot (Remote host path) = do
    (code, _, _) <- readProcessWithExitCode "ssh" [host, "test", "-d", path] ""
    return (code == ExitSuccess)

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

-- | 从字符串列表解析 profile + 两个 Root 并验证可达性
--   [profile, r1, r2] → (profile, root1, root2)
parseSyncThree :: [String] -> IO (String, Root, Root)
parseSyncThree [profile, r1, r2] = do
    validateProfile profile
    let root1 = parseRoot r1
        root2 = parseRoot r2
    ok1 <- checkRoot root1
    ok2 <- checkRoot root2
    case (ok1, ok2) of
        (True,  True)  -> return (profile, root1, root2)
        (False, True)  -> die $ rootLabel root1 ++ ": not accessible"
        (True,  False) -> die $ rootLabel root2 ++ ": not accessible"
        _              -> die $ rootLabel root1 ++ " and " ++ rootLabel root2 ++ ": not accessible"
parseSyncThree _ = die "Usage: hsbisync sync <profile> <root1> <root2>"

-- | 打印错误并退出
die :: String -> IO a
die msg = do
    putStrLn $ "Error: " ++ msg
    exitFailure
