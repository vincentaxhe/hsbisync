module Helper where

import Control.Exception (catch, IOException)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Char (toUpper)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), takeDirectory)
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
import Write (writeFileVerified)

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

-- | RECV 命令（扩展协议）：接收文件并在 rename 之前校验。
--
--   协议格式：RECV <size>,<filename> <mode> <mtime> <expectedHash>
--   其中 filename 是 size 之后到倒数第 3 个空格之前的所有内容（可含空格），
--   mode、mtime、expectedHash 是最后 3 个以空格分隔的字段。
--
--   流程：
--   1. 解析参数，回复 "OK" 表示准备接收
--   2. 读取 <size> 字节二进制数据
--   3. 写临时文件 + 设 mode/mtime + 算哈希比对
--   4. 通过 → rename + 回复 "OK"；不通过 → 删临时文件 + 回复 "ERROR ..."
cmdRecv :: FilePath -> String -> IO ()
cmdRecv root params =
    case break (== ',') params of
        (sizeStr, ',' : rest) ->
            let revWords = reverse (words rest)
            in case revWords of
                (hashStr : mtimeStr : modeStr : revFilename) ->
                    case (reads sizeStr, reads modeStr, reads mtimeStr) of
                        ([(sz, "")], [(md, "")], [(mt, "")]) -> do
                            let filename = unwords (reverse revFilename)
                                fullPath = root </> filename
                                dstDir   = takeDirectory fullPath
                            createDirectoryIfMissing True dstDir
                            putStrLn "OK"
                            hFlush stdout
                            hSetBinaryMode stdin True
                            content <- BS.hGet stdin sz
                            hSetBinaryMode stdin False
                            result <- writeFileVerified fullPath content md mt hashStr
                            case result of
                                Right () -> putStrLn "OK"
                                Left err -> putStrLn ("ERROR " ++ err)
                            hFlush stdout
                        _ -> putStrLn "ERROR Invalid RECV params"
                _ -> putStrLn "ERROR Invalid RECV params"
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
