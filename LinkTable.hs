-- | 「链接表」：jail 型仓库里，软链接不落成真实链接，只在这张表里记一行。
--
--   * 表文件：@<同步根>/.bisync/link_table.json@（**仓库级，不按 profile 分**）
--     （@.bisync@ 在根那一层就被跳过、不参与同步 → 天然是本端本地配置）。
--   * **存在性就是模式**：文件在 = 这个仓库是 jail 型（链接只认表）；
--     删掉文件 = 回到普通仓库（按 lstat 扫描真实链接）。
--   * 链接是**仓库的属性**，不是 profile 的属性：一个仓库不可能同时是 jail 和非 jail，
--     否则“某个 profile 先同步过、另一个 profile 不知道哪些路径其实是链接”就会出问题
--     —— 所以表放在 @.bisync/@ 下，所有 profile 共用一张。
--   * 表里每条记 @target@ / @size@ / @mtime@：
--     @size@ 就是链接的 @st_size@（= 指向字符串的字节长度），@mtime@ 是链接自身
--     的 mtime。两个都必须存 —— 它们是链接比较键 @(size, mtime)@ 的两半，
--     而 jail 端没有 lstat 可查。
--
--   格式（一行规范 JSON；@{}@ 就是空表）：
--
--   > {"conf":{"target":"/home/vincent/.config/alacritty","size":31,"mtime":1758300001}}
--
--   写入一律 **temp + rename**：半截文件会让下一轮判错。
module LinkTable
    ( Entry(..)
    , Table
    , tablePath
    , exists
    , isEmptyTable
    , load
    , save
    , putEntry
    , delEntry
    ) where

import Control.Exception (IOException, try)
import Data.Map (Map)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

import qualified Msg

-- | 表里的一条：链接的指向、@size@（= 指向字节长度）、链接自身 mtime（epoch 秒）
data Entry = Entry
    { entryTarget :: String
    , entrySize   :: Integer
    , entryMtime  :: Integer
    }
    deriving (Show, Eq)

type Table = Map String Entry

-- | 表文件路径：**仓库级**（@.bisync/link_table.json@），所有 profile 共用
--
tablePath :: FilePath -> FilePath
tablePath root = root </> ".bisync" </> "link_table.json"

-- | 该端是不是 jail 型仓库（只看文件在不在）
exists :: FilePath -> IO Bool
exists root = doesFileExist (tablePath root)

-- | 空表（新建时用）
isEmptyTable :: Table
isEmptyTable = Map.empty

-- ============================================================================
-- 编解码
-- ============================================================================

encode :: Table -> BL.ByteString
encode tbl = Msg.encode (Msg.obj (map row (Map.toAscList tbl)))
  where
    row (name, e) =
        ( name
        , Msg.obj [ ("target", Msg.str (entryTarget e))
                  , ("size",   Msg.int (entrySize e))
                  , ("mtime",  Msg.int (entryMtime e))
                  ] )

-- | 解析失败 → Left（宁可报错，也不把表当空表：那会把链接全判成"消失"）
decode :: BS.ByteString -> Either String Table
decode raw = case Msg.decodeStrict raw of
    Left e -> Left ("bad link table: " ++ e)
    Right (Msg.JObj kvs) -> Map.fromList <$> mapM row kvs
    Right _ -> Left "bad link table: top level must be an object"
  where
    row (nameB, val) = do
        let name = B8.unpack nameB
        tgt <- maybe (Left ("bad link table: " ++ name ++ ": missing target"))
                     Right (Msg.fieldStr "target" val)
        sz  <- maybe (Left ("bad link table: " ++ name ++ ": missing size"))
                     Right (Msg.fieldInt "size" val)
        mt  <- maybe (Left ("bad link table: " ++ name ++ ": missing mtime"))
                     Right (Msg.fieldInt "mtime" val)
        return (name, Entry tgt sz mt)

-- ============================================================================
-- 读写
-- ============================================================================

-- | 读表。文件不存在 → 'isEmptyTable'；内容坏 → Left。
load :: FilePath -> IO (Either String Table)
load root = do
    let p = tablePath root
    ok <- doesFileExist p
    if not ok
        then return (Right isEmptyTable)
        else do
            raw <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
            return $ case raw of
                Left e  -> Left (p ++ ": " ++ show e)
                Right b -> decode b

-- | 原子写表（temp + rename，同目录保证 rename 是原子的）
save :: FilePath -> Table -> IO ()
save root tbl = do
    let p   = tablePath root
        dir = takeDirectory p
        tmp = p ++ ".tmp"
    createDirectoryIfMissing True dir
    r <- try (BL.writeFile tmp (encode tbl)) :: IO (Either IOException ())
    case r of
        Left e -> ioError e
        Right () -> do
            rn <- try (renameFile tmp p) :: IO (Either IOException ())
            case rn of
                Right () -> return ()
                Left e   -> do
                    _ <- try (removeFile tmp) :: IO (Either IOException ())
                    ioError e

-- | 增/改一条
putEntry :: String -> Entry -> Table -> Table
putEntry = Map.insert

-- | 删一条（返回新表与"是否真的删掉了"）
delEntry :: String -> Table -> (Table, Bool)
delEntry name tbl =
    let found = Map.member name tbl
    in (Map.delete name tbl, found)
