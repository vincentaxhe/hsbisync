module Helper where

import Control.Exception (catch, IOException)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO
    ( BufferMode(..), hFlush, hLookAhead
    , hSetBinaryMode, hSetBuffering, hSetEncoding, utf8, stdin, stdout
    )

import System.Posix.Files
    ( getFileStatus, fileMode, modificationTime
    , setFileTimes, setFileMode
    )

import Crypto.Hash (hash, SHA512, Digest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import State
    ( StateKind(..)
    , diffState, appendAddedKind, appendDeletedKind
    , currentState, currentLinkState, lastStateKind
    , stateJSON, discardStateUnder
    )
import Write (writeFileVerified, writeLinkVerified, readLinkContent, removeLinkSafe)

-- ============================================================================
-- 入口
-- ============================================================================

-- | 以 helper 模式运行，通过 stdin/stdout 与主控端通信
runHelper :: FilePath -> Maybe String -> IO ()
runHelper root mTag = do
    let stateDir = root </> ".bisync" </> maybe "" id mTag
    createDirectoryIfMissing True stateDir

    -- 全局编码（setFileSystemEncoding/setLocaleEncoding）已在 Main.main 入口统一
    -- 钉死为 UTF-8；这里只需保证本协议所用句柄是行缓冲 + UTF-8。
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8

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
                "GETIGNORE"-> cmdGetIgnore root mTag
                -- 链接阶段（与文件阶段一一对应）
                "GETLINKSTATE" -> cmdGetLinkState root mTag
                "GETLINKLAST"  -> cmdGetLinkLast  root mTag
                "LINKADD"      -> cmdLinkAdd    root mTag params'
                "LINKDEL"      -> cmdLinkDel    root mTag params'
                "LINKHASH"     -> cmdLinkHash   root params'
                "LINKSEND"     -> cmdLinkSend   root params'
                "LINKRECV"     -> cmdLinkRecv   root mTag params'
                "LINKUNLINK"   -> cmdLinkUnlink root params'
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
cmdGetState root mTag = do
    st <- currentState root mTag
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc (stateJSON st) 10)
    hFlush stdout

-- | GETLAST — 返回上次同步状态列表（JSON）
cmdGetLast :: FilePath -> Maybe String -> IO ()
cmdGetLast root mTag = do
    st <- lastStateKind FileKind root mTag
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc (stateJSON st) 10)
    hFlush stdout

-- | GETLINKSTATE — 当前链接状态列表（JSON，格式与 GETSTATE 同）
cmdGetLinkState :: FilePath -> Maybe String -> IO ()
cmdGetLinkState root mTag = do
    st <- currentLinkState root mTag
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc (stateJSON st) 10)
    hFlush stdout

-- | GETLINKLAST — 上次链接状态列表（JSON）
cmdGetLinkLast :: FilePath -> Maybe String -> IO ()
cmdGetLinkLast root mTag = do
    st <- lastStateKind LinkKind root mTag
    putStrLn "OK"
    BL.hPutStr stdout (BL.snoc (stateJSON st) 10)
    hFlush stdout

-- ============================================================================
-- ADD / UNLINK / DELETE
-- ============================================================================

cmdAdd :: FilePath -> Maybe String -> String -> IO ()
cmdAdd root mTag filename = do
    appendAddedKind FileKind root mTag filename
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
    appendDeletedKind FileKind root mTag filename
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
cmdSend root filename = sendContent (BS.readFile (root </> filename))

-- | LINKSEND — 发送链接"内容"（指向字符串），帧格式与 SEND 完全相同
cmdLinkSend :: FilePath -> String -> IO ()
cmdLinkSend root filename = sendContent (readLinkContent (root </> filename))

-- | 通用 SEND：内容由调用方提供（文件 → 读字节，链接 → 读指向字符串）
sendContent :: IO BS.ByteString -> IO ()
sendContent getContent = do
    content <- getContent
    let sz = BS.length content
    putStrLn $ "OK " ++ show sz
    hFlush stdout
    hSetBinaryMode stdout True
    BS.hPut stdout content
    hFlush stdout
    hSetBinaryMode stdout False                  -- 恢复文本模式，供后续行响应输出
    hSetEncoding stdout utf8                     -- C locale 下二进制开关不会恢复编码，显式重设

-- | RECV 命令（扩展协议）：接收文件并在 rename 之前校验。
--
--   协议格式：RECV <size>,<filename> <mode> <mtime> <expectedHash>
--   其中 filename 是 size 之后到倒数第 3 个空格之前的所有内容（可含空格），
--   mode、mtime、expectedHash 是最后 3 个以空格分隔的字段。
--
--   流程：
--   1. 解析参数，回复 "OK" 表示准备接收
--   2. 读取 <size> 字节二进制数据
--   3. 写临时文件 + 设 mode/mtime + 算哈希比对（目录：root 必须存在，
--      root 之下的中间目录由 'writeFileVerified' 递归创建）
--   4. 通过 → rename + 回复 "OK"；不通过 → 删临时文件 + 回复 "ERROR ..."
--
--   注意：先收完数据再校验目录，协议不会因为目录问题而错位——
--   失败时目标文件不受影响，父进程会从第二步响应里拿到 ERROR。
cmdRecv :: FilePath -> String -> IO ()
cmdRecv root params = recvContent (const (return ())) params writeFileVerified root

-- | LINKRECV — 收链接"内容"（指向字符串），落地为符号链接。
--
--   落地成功后顺手作废本端在该链接子树下的文件基线条目（见 'discardStateUnder'）：
--   链接此前在本端不可达，那些条目已过期，必须当作"从未有过"，
--   否则 diff 会得出 Deleted，让对端删掉真实内容。
cmdLinkRecv :: FilePath -> Maybe String -> String -> IO ()
cmdLinkRecv root mTag params =
    recvContent (discardStateUnder root mTag) params writeLinkVerified root

-- | 通用 RECV：收 <size> 字节，交给 writer 落地（先校验后 rename）。
--
--   writer 共享同一签名，所以文件（writeFileVerified）与链接
--   （writeLinkVerified，忽略 mode）可以用完全相同的协议实现。
--   onCommit 在落地成功后调用（拿到相对路径），供链接阶段作废过期基线。
recvContent
    :: (FilePath -> IO ())   -- ^ 落地成功后的回调（相对路径）
    -> String
    -> (FilePath -> FilePath -> BS.ByteString -> Int -> Integer -> String -> IO (Either String ()))
    -> FilePath
    -> IO ()
recvContent onCommit params writer root =
    case break (== ',') params of
        (sizeStr, ',' : rest) ->
            let revWords = reverse (words rest)
            in case revWords of
                (hashStr : mtimeStr : modeStr : revFilename) ->
                    case (reads sizeStr, reads modeStr, reads mtimeStr) of
                        ([(sz, "")], [(md, "")], [(mt, "")]) -> do
                            let filename = unwords (reverse revFilename)
                                fullPath = root </> filename
                            putStrLn "OK"
                            hFlush stdout
                            hSetBinaryMode stdin True
                            content <- BS.hGet stdin sz
                            hSetBinaryMode stdin False
                            hSetEncoding stdin utf8   -- C locale 下二进制开关不恢复编码，显式重设
                            result <- writer root fullPath content md mt hashStr
                            case result of
                                Right () -> do
                                    onCommit filename
                                    putStrLn "OK"
                                Left err -> putStrLn ("ERROR " ++ err)
                            hFlush stdout
                        _ -> putStrLn "ERROR Invalid RECV params"
                _ -> putStrLn "ERROR Invalid RECV params"
        _ -> putStrLn "ERROR Invalid RECV params"

-- | LINKUNLINK — 只删链接本身（unlink），不跟随到目标
--
--   占位不是链接时回 "OK NOTLINK"，主控端只告警、不强行删。
cmdLinkUnlink :: FilePath -> String -> IO ()
cmdLinkUnlink root filename = do
    ok <- removeLinkSafe (root </> filename)
    putStrLn (if ok then "OK" else "OK NOTLINK")

-- ============================================================================
-- 链接阶段：ADD / DELETE / HASH 的对应命令
-- ============================================================================

cmdLinkAdd :: FilePath -> Maybe String -> String -> IO ()
cmdLinkAdd root mTag filename = do
    appendAddedKind LinkKind root mTag filename
    putStrLn "OK"

cmdLinkDel :: FilePath -> Maybe String -> String -> IO ()
cmdLinkDel root mTag filename = do
    appendDeletedKind LinkKind root mTag filename
    putStrLn "OK"

-- | LINKHASH — 链接"内容"（指向字符串）的 SHA512
cmdLinkHash :: FilePath -> String -> IO ()
cmdLinkHash root filename = do
    content <- readLinkContent (root </> filename)
    let digest = hash content :: Digest SHA512
    putStrLn $ "OK " ++ show digest

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

-- ============================================================================
-- GETIGNORE — 读取 .bisync/<profile>/ignore 元信息（供 ignore 预同步比较）
-- ============================================================================

-- | GETIGNORE 命令：读取 .bisync/<profile>/ignore 的元信息（供跨端比较）
--
--   协议：GETIGNORE
--   文件存在：OK <mtime> <mode> <sha512hex>
--   文件不存在：OK -
cmdGetIgnore :: FilePath -> Maybe String -> IO ()
cmdGetIgnore root mTag = do
    let ignoreFile = root </> ".bisync" </> fromMaybe "" mTag </> "ignore"
    exists <- doesFileExist ignoreFile
    if not exists
        then putStrLn "OK -"
        else do
            fs <- getFileStatus ignoreFile
            content <- BS.readFile ignoreFile
            let mt     = modificationTime fs
                mode   = fileMode fs .&. 0o7777
                digest = hash content :: Digest SHA512
            putStrLn $ "OK " ++ show mt ++ " " ++ show mode ++ " " ++ show digest
