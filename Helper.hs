module Helper where

import Control.Exception (catch, IOException)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Char (toUpper)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO
    ( BufferMode(..), hFlush, hLookAhead
    , hSetBinaryMode, hSetBuffering, hSetEncoding, utf8, stdin, stdout
    )
import GHC.IO.Encoding (setFileSystemEncoding)
import System.Posix.Files
    ( getFileStatus, fileMode, modificationTime
    , setFileTimes, setFileMode
    )

import Crypto.Hash (hash, SHA512, Digest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import State
    ( diffState, appendAdded, appendDeleted
    , currentState, lastState, stateJSON
    )

-- ============================================================================
-- 入口
-- ============================================================================

-- | 以 helper 模式运行，通过 stdin/stdout 与主控端通信
runHelper :: FilePath -> Maybe String -> IO ()
runHelper root mTag = do
    let stateDir = root </> ".bisync" </> maybe "" id mTag
    createDirectoryIfMissing True stateDir

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    setFileSystemEncoding utf8   -- SSH 远程 locale 通常是 C，强制 UTF-8 读文件名

    putStrLn "OK"
    hFlush stdout

    commandLoop root mTag

-- | 命令循环
commandLoop :: FilePath -> Maybe String -> IO ()
commandLoop root mTag = go
  where
    go = do
        done <- isEOF
        unless done $ do
            line <- getLine
            let (cmd, params) = break (== ' ') line
                params'       = dropWhile (== ' ') params
            case map toUpper cmd of
                "QUIT"    -> putStrLn "OK" >> hFlush stdout
                "GETDIFF" -> cmdGetDiff  root mTag
                "GETSTATE"-> cmdGetState root mTag
                "GETLAST" -> cmdGetLast  root mTag
                "ADD"     -> cmdAdd      root mTag params'
                "UNLINK"  -> cmdUnlink   root params'
                "DELETE"  -> cmdDelete   root mTag params'
                "HASH"    -> cmdHash     root params'
                "SEND"    -> cmdSend     root params'
                "GETSTAT" -> cmdGetStat  root params'
                "SETSTAT" -> cmdSetStat  root params'
                "RECV"    -> cmdRecv     root params'
                _         -> putStrLn ("ERROR Unknown command: " ++ cmd)
            hFlush stdout
            go

isEOF :: IO Bool
isEOF = catch (hLookAhead stdin >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler _ = return True

-- ============================================================================
-- GETDIFF
-- ============================================================================

cmdGetDiff :: FilePath -> Maybe String -> IO ()
cmdGetDiff root mTag = do
    json <- diffState root mTag
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc json 10)
    hFlush stdout

-- | GETSTATE — 返回当前文件状态列表（JSON）
cmdGetState :: FilePath -> Maybe String -> IO ()
cmdGetState root _mTag = do
    st <- currentState root
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc (stateJSON st) 10)
    hFlush stdout

-- | GETLAST — 返回上次同步状态列表（JSON）
cmdGetLast :: FilePath -> Maybe String -> IO ()
cmdGetLast root mTag = do
    st <- lastState root mTag
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc (stateJSON st) 10)
    hFlush stdout

-- ============================================================================
-- ADD / UNLINK / DELETE
-- ============================================================================

cmdAdd :: FilePath -> Maybe String -> String -> IO ()
cmdAdd root mTag filename = do
    appendAdded root mTag filename
    putStrLn "OK"

cmdUnlink :: FilePath -> String -> IO ()
cmdUnlink root filename = do
    catch (removeFile (root </> filename)) handler
    putStrLn "OK"
  where
    handler :: IOException -> IO ()
    handler e = putStrLn ("ERROR " ++ show e)

cmdDelete :: FilePath -> Maybe String -> String -> IO ()
cmdDelete root mTag filename = do
    appendDeleted root mTag filename
    putStrLn "OK"

-- ============================================================================
-- HASH
-- ============================================================================

cmdHash :: FilePath -> String -> IO ()
cmdHash root filename = do
    content <- BS.readFile (root </> filename)
    let digest = hash content :: Digest SHA512
    putStrLn $ "OK " ++ show digest

-- ============================================================================
-- SEND / RECV
-- ============================================================================

cmdSend :: FilePath -> String -> IO ()
cmdSend root filename = do
    content <- BS.readFile (root </> filename)
    let sz = BS.length content
    putStrLn $ "OK " ++ show sz
    hFlush stdout
    hSetBinaryMode stdout True
    BS.hPut stdout content
    hFlush stdout

cmdRecv :: FilePath -> String -> IO ()
cmdRecv root params =
    case break (== ',') params of
        (sizeStr, ',' : filename) ->
            case reads sizeStr of
                [(sz, "")] -> do
                    let fullPath = root </> filename
                        dstDir   = takeDirectory fullPath
                        tmpPath  = dstDir </> "." ++ takeFileName filename ++ ".tmp"
                    createDirectoryIfMissing True dstDir
                    putStrLn "OK"
                    hFlush stdout
                    hSetBinaryMode stdin True
                    content <- BS.hGet stdin sz
                    BS.writeFile tmpPath content
                    renameFile tmpPath fullPath
                _ -> putStrLn "ERROR Invalid RECV size"
        _ -> putStrLn "ERROR Invalid RECV params"

-- ============================================================================
-- GETSTAT / SETSTAT
-- ============================================================================

cmdGetStat :: FilePath -> String -> IO ()
cmdGetStat root filename = do
    fs <- getFileStatus (root </> filename)
    let mt   = modificationTime fs
        mode = fileMode fs .&. 0o7777
    putStrLn $ "OK " ++ show mt ++ " " ++ show mode

cmdSetStat :: FilePath -> String -> IO ()
cmdSetStat root params = do
    let (filename, rest) = break (== ',') params
        (mtimeStr, modeStr) = break (== ' ') (drop 1 rest)
    case (reads mtimeStr, reads (dropWhile (== ' ') modeStr)) of
        ([(mt, "")], [(md, "")]) -> do
            let fullPath = root </> filename
            setFileTimes fullPath (fromIntegral (mt :: Integer)) (fromIntegral (mt :: Integer))
            setFileMode fullPath (fromIntegral (md :: Integer))
            putStrLn "OK"
        _ -> putStrLn "ERROR Invalid SETSTAT params"
