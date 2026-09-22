module Main where

import Control.Monad (forM_, unless)
import System.Directory
    ( doesDirectoryExist, doesFileExist
    , getCurrentDirectory, listDirectory
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T (unpack)

import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)

import Arg (Root(..), parseRoot, parseSyncThree)
import Exe (RootSource(..), printBanner, runSync)
import Helper (runHelper)
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
    -- 全进程编码固定为 UTF-8，一行搞定，之后所有代码都按"文件名与内容都是
    -- UTF-8"来写（本地与远端 helper 是同一入口，行为一致）：
    --   * setFileSystemEncoding — 文件名、argv（中文文件名）
    --   * setLocaleEncoding     — 文件内容与句柄默认编码（readFile/TIO.readFile/
    --                             putStrLn），不再受 LANG/LC_ALL=C 影响
    -- 顺序很重要：必须在创建句柄、读参数之前执行。
    setFileSystemEncoding utf8
    setLocaleEncoding utf8
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
    , "    With 3 args: sync two roots and record the peer into repo file(s)"
    , "                 (both ends are recorded when the peer is reached via ssh)"
    , "  hsbisync log <profile>             Show pending changes"
    , "  hsbisync state <profile>           Preview changes (read-only)"
    , "  hsbisync profile                   List all profiles"
    , "  hsbisync profile <name>            Show profile details"
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
    -- 标题总是最前面（任何检查/握手/建目录之前）
    printBanner
    (profile, root1, root2) <- parseSyncThree args
    -- 后面全交给 runSync 的 7 步：握手 → 建目录+写 repo → ignore → 链接 → 状态 → 计算 → 执行
    runSync RootsGiven profile root1 root2
syncCmd _ = die "Usage: hsbisync sync <profile> [<root1> <root2>]"

-- ============================================================================
-- sync (profile only) — 读 .bisync/<profile>/repo，与记录的对端同步
-- ============================================================================

-- | 只给 profile：对端地址从本仓库的 @.bisync/\<profile\>/repo@ 读。
--   这种模式下目录**不创建、repo 也不重写**：第 2 步只检查两端目录存在。
syncFromRepo :: String -> IO ()
syncFromRepo profile = do
    printBanner                                 -- 标题最快，连读 repo 之前就出
    cwd <- getCurrentDirectory
    let repoFile = cwd </> ".bisync" </> profile </> "repo"
    exists <- doesFileExist repoFile
    unless exists $
        die $ "No .bisync/" ++ profile ++ "/repo found in " ++ cwd
           ++ "\nCreate it manually with the peer address, e.g.:"
           ++ "\n  echo 'user@host:/path' > " ++ repoFile

    peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
    let root1 = Local cwd          -- 当前目录，必然存在
        root2 = parseRoot peer
    runSync RootsFromRepo profile root1 root2

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
-- profile 子命令（只读：列出 / 显示某个 profile 的 repo 与 ignore）
-- ============================================================================

-- | hsbisync profile            — 列出所有 profile
--   hsbisync profile <name>     — 显示指定 profile
--
--   注：repo/ignore 不再由 profile 命令直接写入。
--   repo 改为在 hsbisync sync <profile> <root1> <root2> 时自动记录；
--   ignore 则靠每次 sync 前的 ignore 预同步在两端对齐。
cmdProfile :: [String] -> IO ()
cmdProfile []     = cmdProfileList
cmdProfile [name] = cmdProfileShow name
cmdProfile _      = die "Usage: hsbisync profile [<name>]"

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
