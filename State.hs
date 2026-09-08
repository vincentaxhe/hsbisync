module State where

import Control.Exception (catch, IOException)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , listDirectory, removeFile, renameFile
    )
import System.FilePath (takeFileName, (</>), takeDirectory)
import System.OsPath (encodeFS)
import System.Posix.Files
    ( getFileStatus, fileSize, fileMode, modificationTime
    )
import Text.JSON

import Ignore (Ignore, parse, ignores)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text as T (pack, unpack)

-- | 空的 gitignore 规则集
emptyIgnore :: Ignore
emptyIgnore = parse (T.pack "")

-- ============================================================================
-- Data types
-- ============================================================================

-- | 文件状态
--
--   本地和远程都归一化到此类型：
--   * 本地：@EpochTime → UTCTime@（通过 'posixSecondsToUTCTime'）
--   * 远程：JSON 中的 epoch 秒整数 → 'UTCTime'（通过 json）
--   两者产生等价的 'Eq' 实例，'Sync.sync' 可直接比较。
data FileState = FileState
    { size     :: !Integer
    , mtime    :: !UTCTime
    , mode     :: !Int
    , filename :: !FilePath
    } deriving (Show, Eq)

-- | diff 条目
data DiffEntry = DiffEntry
    { deState :: !String
    , deSize  :: !Integer
    , deMtime :: !Integer
    , deFile  :: !FilePath
    } deriving (Show, Eq)
-- | 单侧文件状态变化
-- | 单侧文件差异（5 种）
--
--   * 'None'  — 该侧从未有过此文件
--   * 'Unchanged'    — 文件存在且未变
--   * 'New'     — 新增
--   * 'Updated'  — 修改
--   * 'Deleted'  — 删除
data DiffState = None | Unchanged | New | Updated | Deleted
    deriving (Show, Eq)

-- | 文件变更决策，携带双方各自的 DiffState
--
--   * 'Added'     — 本方应接收。root1Diff/root2Diff 说明两侧变化
--   * 'Del'   — 本方应删除
--   * 'Conflict'  — 双方冲突
--   * 'Noop' — 无变化
data FileChange
    = Noop FileState DiffState DiffState
    | Added     FileState DiffState DiffState
    | Del   FileState DiffState DiffState
    | Conflict  FileState DiffState DiffState
    deriving (Show, Eq)

-- ============================================================================
-- 获取当前文件状态（扫描 root 目录）
-- ============================================================================

-- | 从 @.bisync/<profile>/ignore@ 读取忽略规则
readProfileIgnore :: FilePath -> Maybe String -> IO Ignore
readProfileIgnore root mTag = do
    let ignoreFile = root </> ".bisync" </> fromMaybe "" mTag </> "ignore"
    exists <- doesFileExist ignoreFile
    if exists
        then parse <$> TIO.readFile ignoreFile
        else return emptyIgnore

-- | 扫描目录，返回所有文件的相对路径（深度优先）
--
--   不做隐藏文件过滤：所有文件/目录（含以 @.@ 开头的）都参与同步，
--   需要排除的条目通过 ignore 规则（@.bisync/<profile>/ignore@）过滤。
--   仅在同步根目录下额外跳过工具自身状态目录 @.bisync@。
scanFiles :: FilePath -> Maybe String -> IO [FilePath]
scanFiles root mTag = do
    ign <- readProfileIgnore root mTag
    scanRel' root root ign

-- | 扫描 root 目录，返回所有文件的 FileState 列表（相对路径）
--
--   等同于 @scanFiles + statRel@
currentState :: FilePath -> Maybe String -> IO [FileState]
currentState root mTag = do
    ign <- readProfileIgnore root mTag
    files <- scanRel' root root ign
    mapM (statRel root) files

-- | 内部递归扫描（兼容旧接口，不使用任何忽略规则）
scanRel :: FilePath -> FilePath -> IO [FilePath]
scanRel root dir = scanRel' root dir emptyIgnore

-- | 递归扫描，使用给定的 gitignore 规则过滤所有条目
--
--   不过滤隐藏文件（以 @.@ 开头的条目同样参与同步）。
--   当 @dir@ 为同步根目录时，额外跳过工具自身状态目录 @.bisync@，
--   其余排除一律交给 ignore 规则。
scanRel' :: FilePath -> FilePath -> Ignore -> IO [FilePath]
scanRel' root dir ignoreRules = do
    entries <- listDirectory dir
    let atRoot  = dir == root
        visible = [ e | e <- entries
                      , not (atRoot && takeFileName e == ".bisync") ]

    -- 判断每个条目是否为目录
    let fullPaths  = [dir </> e | e <- visible]
    isDirFlags <- mapM doesDirectoryExist fullPaths

    -- 将路径转为相对于 root 的形式，供 gitignore 匹配
    let relPaths = map (makeRelative root) fullPaths
    osPaths  <- mapM encodeFS relPaths
    let ignored  = zipWith (ignores ignoreRules) osPaths isDirFlags
        kept     = [ (fp, isDir)
                   | (fp, isDir, ign) <- zip3 fullPaths isDirFlags ignored
                   , not ign ]

    let dirs  = [d | (d, True ) <- kept]
        files = [f | (f, False) <- kept]

    subFiles <- concat <$> mapM (\d -> scanRel' root d ignoreRules) dirs
    let relFiles = map (makeRelative root) files
    return (subFiles ++ relFiles)

-- | 获取单个文件的 FileState（填充 size、mtime、mode）
statRel :: FilePath -> FilePath -> IO FileState
statRel root relPath = do
    let fullPath = root </> relPath
    fs <- getFileStatus fullPath
    let sz = fromIntegral (fileSize fs)
        mt = posixSecondsToUTCTime (realToFrac (modificationTime fs))
        md = fromIntegral (fileMode fs) .&. 0o7777
    return $ FileState sz mt md relPath

-- ============================================================================
-- 获取上次同步状态（从 .bisync/ 目录读取）
-- ============================================================================

-- | 从 @.bisync/@ 目录读取上次同步状态（只读，不写回不删文件）
--
--   自动合并 @state@、@added@、@deleted@ 三个文件：
--   * @state@ — 上次同步时的基准状态
--   * @added@ — 上次同步后新增的文件（覆盖/追加到 state）
--   * @deleted@ — 上次同步后删除的文件（从 state 中移除）
--
--   与 'lastState' 不同，此函数不修改任何文件。
readLastState :: FilePath -> Maybe String -> IO [FileState]
readLastState root mTag = do
    let stateDir   = root </> ".bisync" </> fromMaybe "" mTag
        stateFile   = stateDir </> "state"
        addedFile   = stateDir </> "added"
        deletedFile = stateDir </> "deleted"

    st <- readEntries stateFile

    hasAdded <- doesFileExist addedFile
    st' <- if hasAdded
        then do adds <- readEntries addedFile; return $ mergeAdds adds st
        else return st

    hasDeleted <- doesFileExist deletedFile
    if hasDeleted
        then do
            raw <- BS.readFile deletedFile
            let dels = map (T.unpack . T.decodeUtf8) (bsLines raw)
            return $ filter (\(FileState _ _ _ fn) -> fn `notElem` dels) st'
        else return st'

-- | 从 @.bisync/@ 目录读取上次同步状态，合并后写回并清理
--
--   内部调用 'readLastState'，然后写回 @state@ 并删除 @added@ / @deleted@。
lastState :: FilePath -> Maybe String -> IO [FileState]
lastState root mTag = do
    let stateDir   = root </> ".bisync" </> fromMaybe "" mTag
        stateFile   = stateDir </> "state"
        addedFile   = stateDir </> "added"
        deletedFile = stateDir </> "deleted"

    st <- readLastState root mTag

    hasAdded <- doesFileExist addedFile
    hasDeleted <- doesFileExist deletedFile
    when (hasAdded || hasDeleted) $ do
        writeEntries stateFile st
        when hasAdded   $ removeFileSilent addedFile
        when hasDeleted $ removeFileSilent deletedFile

    return st

-- | 读取 state/added 文件（原始字节读，回避编码问题）
--
--   严格模式：空文件返回 []，非空则每行必须能解析，否则报错退出。
readEntries :: FilePath -> IO [FileState]
readEntries fp = do
    exists <- doesFileExist fp
    if not exists then return []
    else do
        raw <- BS.readFile fp
        let lineStrs = filter (not . null)
                     $ map (T.unpack . T.decodeUtf8)
                     $ bsLines raw
        return $ map parseLineStrict lineStrs

-- | 解析一行 "size,mtime,filename"，失败则 error 退出
--   mode 不存储在 state 文件中，解析时填 0（仅用于 diff 比较，不参与 sameFile）
parseLineStrict :: String -> FileState
parseLineStrict line =
    case break (== ',') line of
        (sizeStr, ',' : rest) ->
            case break (== ',') rest of
                (mtimeStr, ',' : file) ->
                    case (reads sizeStr, reads mtimeStr) of
                        ([(sz, "")], [(mt, "")]) ->
                            FileState
                                sz
                                (posixSecondsToUTCTime (realToFrac (mt :: Integer)))
                                0
                                file
                        _ -> bad
                _ -> bad
        _ -> bad
  where
    bad = error $ "parseLineStrict: invalid state line: " ++ line

-- | 写入 state 文件（原始字节写入，必须成功否则退出）
writeEntries :: FilePath -> [FileState] -> IO ()
writeEntries fp entries = do
    createDirectoryIfMissing True (takeDirectory fp)
    let tmpFp = fp ++ ".tmp"
    let content = BS.concat
            [ T.encodeUtf8 (T.pack (show (size fs) ++ "," ++ show (posixSecs (mtime fs)) ++ ","))
              <> T.encodeUtf8 (T.pack (filename fs))
              <> BS.singleton 10
            | fs <- entries ]
    BS.writeFile tmpFp content
    renameFile tmpFp fp
    -- 确认写入成功
    written <- BS.readFile fp
    when (BS.length written /= BS.length content) $
        error $ "writeEntries: verification failed for " ++ fp

-- | 合并 added 条目到 state：同文件名则覆盖，否则追加
mergeAdds :: [FileState] -> [FileState] -> [FileState]
mergeAdds adds st =
    let addMap = [(filename a, a) | a <- adds]
        replaced = map (\fs -> case lookup (filename fs) addMap of
                                  Just a  -> a
                                  Nothing -> fs) st
        newFiles = [a | a <- adds, filename a `notElem` map filename st]
    in replaced ++ newFiles

-- ============================================================================
-- Helpers
-- ============================================================================

-- | 按换行分割 ByteString（纯字节，不涉及编码）
bsLines :: BS.ByteString -> [BS.ByteString]
bsLines bs
    | BS.null bs = []
    | otherwise =
        let (line, rest) = BS.break (== 10) bs
            rest' = BS.dropWhile (== 10) rest
        in line : bsLines rest'

-- | monadic partition
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []     = return ([], [])
partitionM p (x:xs) = do
    yes <- p x
    (ys, ns) <- partitionM p xs
    return $ if yes then (x:ys, ns) else (ys, x:ns)

-- | 静默删文件
removeFileSilent :: FilePath -> IO ()
removeFileSilent fp = catch (removeFile fp) handler
  where
    handler :: IOException -> IO ()
    handler _ = return ()

-- | 计算相对路径
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative root path =
    let r = dropTrailingSlash root
    in dropTrailingSlash $ drop (length r + 1) path
  where
    dropTrailingSlash p
        | null p        = p
        | last p == '/' = init p
        | otherwise     = p

-- ============================================================================
-- Helper 复用：diff 计算 + added/deleted 追加
-- ============================================================================

-- | 计算上次状态与当前状态的差异（供 Helper 的 GETDIFF 命令使用）
--   返回 JSON 字节串
diffState :: FilePath -> Maybe String -> IO BL.ByteString
diffState root mTag = do
    prev <- lastState root mTag
    cur  <- currentState root mTag

    let diffs = diffOneSide prev cur
    return $ diffJSON [mkDiffEntry ds fs | (ds, fs) <- diffs]

mkDiffEntry :: DiffState -> FileState -> DiffEntry
mkDiffEntry ds fs = DiffEntry (show ds) (size fs) (posixSecs (mtime fs)) (filename fs)

-- | 追加一条记录到 @.bisync/added@（写入必须成功）
appendAdded :: FilePath -> Maybe String -> FilePath -> IO ()
appendAdded root mTag relPath = do
    let stateDir = root </> ".bisync" </> fromMaybe "" mTag
    createDirectoryIfMissing True stateDir
    let addedFp = stateDir </> "added"
    fs <- getFileStatus (root </> relPath)
    let sz = fileSize fs
        mt = modificationTime fs
        line = T.encodeUtf8 (T.pack (show sz ++ "," ++ show mt ++ ","))
            <> T.encodeUtf8 (T.pack relPath)
            <> BS.singleton 10
    BS.appendFile addedFp line
    -- 确认写入
    content <- BS.readFile addedFp
    when (not (BS.isSuffixOf line content)) $
        error $ "appendAdded: verification failed for " ++ addedFp

-- | 追加一条记录到 @.bisync/deleted@（写入必须成功）
appendDeleted :: FilePath -> Maybe String -> FilePath -> IO ()
appendDeleted root mTag relPath = do
    let stateDir = root </> ".bisync" </> fromMaybe "" mTag
    createDirectoryIfMissing True stateDir
    let deletedFp = stateDir </> "deleted"
    let line = T.encodeUtf8 (T.pack relPath) <> BS.singleton 10
    BS.appendFile deletedFp line
    -- 确认写入
    content <- BS.readFile deletedFp
    when (not (BS.isSuffixOf line content)) $
        error $ "appendDeleted: verification failed for " ++ deletedFp

-- | 比较两个 FileState 是否相同（基于 size 和整数秒级 mtime）
--   mode 不参与比较——权限变更不触发重新同步
sameFile :: FileState -> FileState -> Bool
sameFile a b =
    size a == size b &&
    posixSecs (mtime a) == posixSecs (mtime b)

-- | UTCTime → 整数 POSIX 秒
posixSecs :: UTCTime -> Integer
posixSecs = floor . utcTimeToPOSIXSeconds

-- | 计算单侧差异：last vs cur → [(DiffState, FileState)]
diffOneSide :: [FileState] -> [FileState] -> [(DiffState, FileState)]
diffOneSide prev cur =
    let prevMap  = [(filename fs, fs) | fs <- prev]
        curNames = map filename cur
        lookupPrev name = lookup name prevMap

        curDiffs = map (\fs ->
            case lookupPrev (filename fs) of
                Nothing     -> (New,    fs)
                Just prevFs -> if sameFile fs prevFs
                    then (Unchanged, fs)
                    else (Updated,   fs)
            ) cur

        delDiffs = [ (Deleted, fs)
                   | fs <- prev
                   , filename fs `notElem` curNames
                   ]
    in curDiffs ++ delDiffs

-- ============================================================================
-- JSON 编解码（json 包，纯 Haskell，无 C++ 依赖）
-- ============================================================================

-- | FileState 的 JSON 实例
instance JSON FileState where
    showJSON (FileState sz mt md fn) =
        makeObj [ ("size",     showJSON sz)
                , ("mtime",    showJSON (posixSecs mt))
                , ("mode",     showJSON md)
                , ("filename", showJSON fn)
                ]

    readJSON (JSObject obj) = do
        sz <- valFromObj "size" obj
        mt <- valFromObj "mtime" obj
        md <- valFromObj "mode" obj
        fn <- valFromObj "filename" obj
        return $ FileState sz (posixSecondsToUTCTime (realToFrac (mt :: Integer))) md fn
    readJSON _ = Error "FileState: expected object"

-- | DiffEntry 的 JSON 实例
instance JSON DiffEntry where
    showJSON (DiffEntry st sz mt fn) =
        makeObj [ ("deState", showJSON st)
                , ("deSize",  showJSON sz)
                , ("deMtime", showJSON mt)
                , ("deFile",  showJSON fn)
                ]

    readJSON (JSObject obj) = do
        st <- valFromObj "deState" obj
        sz <- valFromObj "deSize" obj
        mt <- valFromObj "deMtime" obj
        fn <- valFromObj "deFile" obj
        return $ DiffEntry st sz mt fn
    readJSON _ = Error "DiffEntry: expected object"

-- | 将 FileState 列表编码为 JSON（供 GETSTATE/GETLAST 使用）
stateJSON :: [FileState] -> BL.ByteString
stateJSON = BL.fromStrict . T.encodeUtf8 . T.pack . encode

-- | 从 JSON 解析 FileState 列表
parseStateJSON :: BL.ByteString -> [FileState]
parseStateJSON bs = case decode (T.unpack (T.decodeUtf8 (BL.toStrict bs))) of
    Ok fs   -> fs
    Error e -> error $ "parseStateJSON: invalid JSON: " ++ e

-- | 将 diff 列表编码为 JSON
diffJSON :: [DiffEntry] -> BL.ByteString
diffJSON = BL.fromStrict . T.encodeUtf8 . T.pack . encode


