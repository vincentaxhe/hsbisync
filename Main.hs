module Main where

import Control.Monad (forM_, unless)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T (unpack)

import Arg (Root(..), parseRoot, parseArgsFrom)
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
        ["log"]           -> cmdLog
        ["state"]         -> cmdState
        "sync" : rest     -> syncCmd rest
        _                 -> showHelp

showHelp :: IO ()
showHelp = putStrLn $ unlines
    [ "hsbisync - Bidirectional File Synchronization"
    , ""
    , "Usage:"
    , "  hsbisync sync [<root1> <root2>]   Sync two directories"
    , "  hsbisync log                       Show .bisync state files"
    , "  hsbisync state                     Preview changes (read-only)"
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
helperMain ["--passcode=dontrunyourself", rootArg] =
    runHelper (dropPrefix "--root=" rootArg) Nothing
helperMain _ =
    putStrLn "Usage: hsbisync --helper --passcode=dontrunyourself --root=<path>"

dropPrefix :: String -> String -> String
dropPrefix prefix s
    | take (length prefix) s == prefix = drop (length prefix) s
    | otherwise = s

-- ============================================================================
-- sync 子命令
-- ============================================================================

-- | hsbisync sync [<root1> <root2>]
--   无参数：读 .bisync/repo；两参数：直接同步
syncCmd :: [String] -> IO ()
syncCmd []          = syncFromRepo
syncCmd [r1, r2]    = do
    (root1, root2) <- parseArgsFrom [r1, r2]
    runSync root1 root2
syncCmd _           = die "Usage: hsbisync sync [<root1> <root2>]"

-- ============================================================================
-- sync (no args) — 读 .bisync/repo，与记录的对端同步
-- ============================================================================

syncFromRepo :: IO ()
syncFromRepo = do
    cwd <- getCurrentDirectory
    let repoFile = cwd </> ".bisync" </> "repo"
    exists <- doesFileExist repoFile
    unless exists $
        die $ "No .bisync/repo found in " ++ cwd
           ++ "\nRun 'hsbisync <root1> <root2>' first to record the peer."

    peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
    let root1 = Local cwd
        root2 = parseRoot peer
    runSync root1 root2

-- ============================================================================
-- log 子命令
-- ============================================================================

cmdLog :: IO ()
cmdLog = do
    cwd <- getCurrentDirectory
    let dir = cwd </> ".bisync"
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

cmdState :: IO ()
cmdState = do
    cwd <- getCurrentDirectory
    prev <- readLastState cwd Nothing
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

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

-- ============================================================================
-- 工具
-- ============================================================================

die :: String -> IO a
die msg = do
    putStrLn $ "Error: " ++ msg
    exitFailure
