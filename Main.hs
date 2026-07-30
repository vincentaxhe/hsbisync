module Main where

import Control.Exception (bracket, try, displayException, SomeException)
import Control.Monad (forM_, unless)
import Data.Maybe (isNothing)
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, listDirectory
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T (unpack, pack)

import Arg (Root(..), parseRoot, parseSyncThree)
import Exe (runSync, spawnHelper, destroyHelper)
import Helper (runHelper)
import Write (HelperHandle(..), putRepoRemote, putIgnoreRemote)
import State
    ( FileState(..), DiffState(..)
    , readLastState, currentState, diffOneSide
    , readEntries, bsLines
    )

-- ============================================================================
-- 入口
-- ============================================================================

main :: IO ()
main = do
    args <- getArgs
    case args of
        "--helper" : rest  -> helperMain rest
        ["--help"]         -> showHelp
        ["log", profile]   -> cmdLog profile
        ["state", profile] -> cmdState profile
        "profile" : rest   -> cmdProfile rest
        "sync" : rest      -> syncCmd rest
        _                  -> showHelp

showHelp :: IO ()
showHelp = putStrLn $ unlines
    [ "hsbisync - Bidirectional File Synchronization"
    , ""
    , "Usage:"
    , "  hsbisync sync <profile> [<root1> <root2>]"
    , "    With 1 arg:  read peer from .bisync/<profile>/repo and sync"
    , "    With 3 args: sync two directories using <profile>"
    , "  hsbisync log <profile>             Show pending changes"
    , "  hsbisync state <profile>           Preview changes (read-only)"
    , "  hsbisync profile                   List all profiles"
    , "  hsbisync profile <name>            Show profile details"
    , "  hsbisync profile <name> <addr> [--repo <a>] [--ignore <r>] ..."
    , "                                      Configure profile at multiple paths"
    , "  hsbisync --help                    Show this help"
    , ""
    , "Root formats:"
    , "  /local/path"
    , "  [user@]host:/remote/path"
    ]

-- ============================================================================
-- Helper 模式
-- ============================================================================

helperMain :: [String] -> IO ()
helperMain args
    | "--passcode=dontrunyourself" `notElem` args =
        putStrLn "ERROR: helper must be invoked with --passcode=dontrunyourself"
    | otherwise =
        let rootPath = findArg "--root=" args
            tagVal   = findArg "--tag=" args
        in if null rootPath
            then putStrLn "Usage: hsbisync --helper --passcode=dontrunyourself --root=<path> [--tag=<profile>]"
            else runHelper rootPath (if null tagVal then Nothing else Just tagVal)

-- | 从参数列表中查找以 prefix 开头的参数，返回去掉 prefix 后的值（取首个匹配）
findArg :: String -> [String] -> String
findArg prefix args =
    case filter (\a -> take (length prefix) a == prefix) args of
        (a:_) -> dropPrefix prefix a
        []    -> ""

dropPrefix :: String -> String -> String
dropPrefix prefix s
    | take (length prefix) s == prefix = drop (length prefix) s
    | otherwise = s

-- ============================================================================
-- sync 子命令
-- ============================================================================

-- | hsbisync sync <profile> [<root1> <root2>]
--   一个参数：读 .bisync/<profile>/repo 获取对端地址
--   三个参数：profile + 双端直接同步
syncCmd :: [String] -> IO ()
syncCmd [profile] = syncFromRepo profile
syncCmd args@[_, _, _] = do
    (profile, root1, root2) <- parseSyncThree args
    runSync profile root1 root2
syncCmd _ = die "Usage: hsbisync sync <profile> [<root1> <root2>]"

-- ============================================================================
-- 参数解析工具
-- ============================================================================

-- | 将 \\n 转义序列转换为实际换行符
unescapeNewlines :: String -> String
unescapeNewlines = go
  where
    go [] = []
    go ('\\':'n':xs) = '\n' : go xs
    go (x:xs)        = x   : go xs

-- ============================================================================
-- sync (profile only) — 读 .bisync/<profile>/repo，与记录的对端同步
-- ============================================================================

syncFromRepo :: String -> IO ()
syncFromRepo profile = do
    cwd <- getCurrentDirectory
    let repoFile = cwd </> ".bisync" </> profile </> "repo"
    exists <- doesFileExist repoFile
    unless exists $
        die $ "No .bisync/" ++ profile ++ "/repo found in " ++ cwd
           ++ "\nCreate it manually with the peer address, e.g.:"
           ++ "\n  echo 'user@host:/path' > " ++ repoFile

    peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
    let root1 = Local cwd
        root2 = parseRoot peer
    runSync profile root1 root2

-- ============================================================================
-- log 子命令
-- ============================================================================

cmdLog :: String -> IO ()
cmdLog profile = do
    cwd <- getCurrentDirectory
    let dir = cwd </> ".bisync" </> profile
    added   <- readEntries (dir </> "added")
    deleted <- readDeleted  (dir </> "deleted")

    if null added && null deleted
        then putStrLn "No pending changes."
        else do
            let total = length added + length deleted
            putStrLn $ "Last sync recorded (" ++ show total ++ " entries):"
            unless (null added) $ do
                putStrLn $ "\n  Added " ++ show (length added) ++ " files:"
                forM_ added $ \fs -> putStrLn $ "    " ++ filename fs
            unless (null deleted) $ do
                putStrLn $ "\n  Deleted " ++ show (length deleted) ++ " files:"
                forM_ deleted $ \f -> putStrLn $ "    " ++ f

readDeleted :: FilePath -> IO [FilePath]
readDeleted fp = do
    exists <- doesFileExist fp
    if not exists then return []
    else do
        raw <- BS.readFile fp
        return $ filter (not . null) $ map (T.unpack . T.decodeUtf8) $ bsLines raw

-- ============================================================================
-- state 子命令（只读预览）
-- ============================================================================

cmdState :: String -> IO ()
cmdState profile = do
    cwd <- getCurrentDirectory
    prev <- readLastState cwd (Just profile)
    cur  <- currentState cwd (Just profile)

    let diffs = diffOneSide prev cur
        changed = [ (ds, fs) | (ds, fs) <- diffs, ds /= Unchanged ]

    putStrLn $ "Last merged state: " ++ show (length prev) ++ " files"
    putStrLn $ "Current directory:  " ++ show (length cur)  ++ " files"

    if null changed
        then putStrLn "No changes."
        else do
            putStrLn $ "\nChanges (" ++ show (length changed) ++ " entries):\n"
            forM_ changed $ \(ds, fs) ->
                putStrLn $ "  " ++ pad 8 (show ds) ++ "  " ++ filename fs

-- ============================================================================
-- profile 子命令
-- ============================================================================

-- | hsbisync profile                                — 列出所有 profile
--   hsbisync profile <name>                         — 显示指定 profile
--   hsbisync profile <name> <addr> [flags] [<addr> [flags] ...]
--     flags: --repo <addr>  --ignore <rules>         — 在多个路径下配置 profile
cmdProfile :: [String] -> IO ()
cmdProfile [] = cmdProfileList
cmdProfile [name] = cmdProfileShow name
cmdProfile (name : rest) =
    case parseBlocks rest of
        []  -> die "Usage: hsbisync profile <name> <addr> [--repo <addr>] [--ignore <rules>] ..."
        blks -> forM_ blks $ \(path, mRepo, mIgnore) ->
            cmdProfileConfig name path mRepo mIgnore

-- | 将参数列表解析为多个地址块 [(path, mRepo, mIgnore)]
--   flags 只属于紧邻的前一个地址，遇到下一个地址停止消费
parseBlocks :: [String] -> [(FilePath, Maybe String, Maybe String)]
parseBlocks [] = []
parseBlocks (p : xs)
    | p == "--repo" || p == "--ignore" = []  -- 孤立标志，忽略
    | otherwise =
        let (flags, rest) = parseFlags xs
        in (p, lookup "--repo" flags, lookup "--ignore" flags) : parseBlocks rest

-- | 只消费前导的 --key value 对，遇到非 flag 参数立即停止
parseFlags :: [String] -> ([(String, String)], [String])
parseFlags [] = ([], [])
parseFlags (a:b:xs)
    | a == "--repo" || a == "--ignore" =
        let (more, rest) = parseFlags xs in ((a, b) : more, rest)
parseFlags rest = ([], rest)

-- | 列出 .bisync/ 下所有 profile
cmdProfileList :: IO ()
cmdProfileList = do
    cwd <- getCurrentDirectory
    let bisyncDir = cwd </> ".bisync"
    exists <- doesDirectoryExist bisyncDir
    if not exists
        then putStrLn "No .bisync directory found."
        else do
            entries <- listDirectory bisyncDir
            let profiles = filter (not . isHidden) entries
            if null profiles
                then putStrLn "No profiles found."
                else do
                    putStrLn "Profiles:"
                    forM_ profiles $ \name ->
                        showProfile bisyncDir name

-- | 显示单个 profile 详情（从 CWD）
cmdProfileShow :: String -> IO ()
cmdProfileShow name = do
    cwd <- getCurrentDirectory
    let bisyncDir = cwd </> ".bisync"
    exists <- doesDirectoryExist (bisyncDir </> name)
    if not exists
        then putStrLn $ "Profile '" ++ name ++ "' not found."
        else showProfile bisyncDir name

-- | 在指定路径下创建/更新 profile 配置文件
--
--   path 可以是本地路径或 user@host:/path 格式的远程路径。
--   远程路径通过 helper 协议写入，写入失败会报错。
cmdProfileConfig :: String -> FilePath -> Maybe String -> Maybe String -> IO ()
cmdProfileConfig name path mRepo mIgnore
    | isNothing mRepo && isNothing mIgnore = do
        putStrLn $ "[" ++ path ++ "]"
        putStrLn "  WARNING: no --repo or --ignore specified, nothing to write"
    | otherwise = do
    case parseRoot path of
        Local localPath ->
            writeProfileLocal name localPath mRepo mIgnore
        Remote host remotePath -> do
            putStrLn $ "[" ++ host ++ ":" ++ remotePath ++ "]"
            result <- (try :: IO () -> IO (Either SomeException ())) $
                bracket (spawnHelper host remotePath name)
                        destroyHelper
                        $ \hh -> writeProfileRemote hh mRepo mIgnore
            case result of
                Left e  -> putStrLn $ "  ERROR: " ++ displayException e
                Right () -> return ()

-- | 本地写入 profile 配置
--
--   要求 path 必须是已存在的目录，只在其下创建 .bisync/<name>/
writeProfileLocal :: String -> FilePath -> Maybe String -> Maybe String -> IO ()
writeProfileLocal name path mRepo mIgnore = do
    exists <- doesDirectoryExist path
    unless exists $
        die $ "Path does not exist: " ++ path
    let profDir = path </> ".bisync" </> name
    createDirectoryIfMissing True profDir
    putStrLn $ "[" ++ path ++ "]"
    case mRepo of
        Just addr -> do
            writeFile (profDir </> "repo") addr
            putStrLn $ "  repo: " ++ addr ++ "  [OK]"
        Nothing -> return ()
    case mIgnore of
        Just rules -> do
            writeFile (profDir </> "ignore") (unescapeNewlines rules)
            putStrLn $ "  ignore: " ++ rules ++ "  [OK]"
        Nothing -> return ()

-- | 通过 helper 协议远程写入 profile 配置
writeProfileRemote :: HelperHandle -> Maybe String -> Maybe String -> IO ()
writeProfileRemote hh mRepo mIgnore = do
    case mRepo of
        Just addr -> do
            result <- putRepoRemote hh (T.encodeUtf8 (T.pack addr))
            case result of
                Left err -> die $ "Failed to write remote repo: " ++ err
                Right () -> putStrLn $ "  repo: " ++ addr ++ "  [OK]"
        Nothing -> return ()
    case mIgnore of
        Just rules -> do
            let content = unescapeNewlines rules
            result <- putIgnoreRemote hh (T.encodeUtf8 (T.pack content))
            case result of
                Left err -> die $ "Failed to write remote ignore: " ++ err
                Right () -> putStrLn $ "  ignore: " ++ rules ++ "  [OK]"
        Nothing -> return ()

-- | 显示单个 profile 的 repo 和 ignore 内容
showProfile :: FilePath -> String -> IO ()
showProfile bisyncDir name = do
    let repoFile   = bisyncDir </> name </> "repo"
        ignoreFile = bisyncDir </> name </> "ignore"
    hasRepo   <- doesFileExist repoFile
    hasIgnore <- doesFileExist ignoreFile

    peerText <- if hasRepo
        then do
            peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
            return $ "repo: " ++ peer
        else return "repo: -"

    ignoreLines <- if hasIgnore
        then lines <$> readFile ignoreFile
        else return []

    putStrLn $ "  " ++ name
    putStrLn $ "    " ++ peerText
    if null ignoreLines
        then putStrLn "    ignore: -"
        else do
            putStrLn "    ignore:"
            forM_ ignoreLines $ \l ->
                putStrLn $ "      " ++ l

-- | 文件名是否以 '.' 开头
isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

-- ============================================================================
-- 工具
-- ============================================================================

die :: String -> IO a
die msg = do
    putStrLn $ "Error: " ++ msg
    exitFailure
