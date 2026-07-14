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

import Arg (Root(..), parseRoot, parseSyncThree)
import Exe (runSync)
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
    args <- getArgs
    case args of
        "--helper" : rest -> helperMain rest
        ["--help"]        -> showHelp
        ["log", profile]  -> cmdLog profile
        ["state", profile] -> cmdState profile
        ["profile"]        -> cmdProfile
        "sync" : rest     -> syncCmd rest
        _                 -> showHelp

showHelp :: IO ()
showHelp = putStrLn $ unlines
    [ "hsbisync - Bidirectional File Synchronization"
    , ""
    , "Usage:"
    , "  hsbisync sync <profile> [<root1> <root2>]"
    , "    With 3 args: sync two directories using <profile>"
    , "    With 1 arg:  read peer from .bisync/<profile>/repo and sync"
    , "  hsbisync log <profile>             Show pending changes"
    , "  hsbisync state <profile>           Preview changes (read-only)"
    , "  hsbisync profile                   List profiles and their peers"
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
--   一个参数：读 .bisync/<profile>/repo 获取对端地址（须手动创建）
--   三个参数：profile + 双端直接同步（不写 repo，repo 须手动维护）
syncCmd :: [String] -> IO ()
syncCmd [profile] = syncFromRepo profile
syncCmd args@[_, _, _] = do
    (profile, root1, root2) <- parseSyncThree args
    runSync profile root1 root2
syncCmd _ = die "Usage: hsbisync sync <profile> [<root1> <root2>]"

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
    cur  <- currentState cwd

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
-- profile 子命令：列出 .bisync/ 下所有 profile 及其对端地址
-- ============================================================================

cmdProfile :: IO ()
cmdProfile = do
    cwd <- getCurrentDirectory
    let bisyncDir = cwd </> ".bisync"
    exists <- doesDirectoryExist bisyncDir
    if not exists
        then putStrLn "No .bisync directory found. Run 'hsbisync sync <profile> ...' first."
        else do
            entries <- listDirectory bisyncDir
            let profiles = filter (not . isHidden) entries
            if null profiles
                then putStrLn "No profiles found."
                else do
                    putStrLn "Profiles:"
                    forM_ profiles $ \name -> do
                        let isDir = doesDirectoryExist (bisyncDir </> name)
                            -- quick check: if it's a directory, it's a profile
                        isProfile <- isDir
                        if not isProfile
                            then return ()
                            else do
                                let repoFile = bisyncDir </> name </> "repo"
                                hasRepo <- doesFileExist repoFile
                                if hasRepo
                                    then do
                                        peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
                                        putStrLn $ "  " ++ name ++ "  →  " ++ peer
                                    else
                                        putStrLn $ "  " ++ name ++ "  (no peer recorded)"

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
