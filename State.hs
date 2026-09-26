module State where

import Control.Exception (catch, IOException, try)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import System.Directory
    ( createDirectoryIfMissing, doesFileExist
    , listDirectory, removeFile, renameFile
    )
import System.FilePath (takeFileName, (</>), takeDirectory, dropTrailingPathSeparator)
import System.IO (hPutStrLn, stderr)
import System.OsPath (encodeFS)
import System.Posix.Files
    ( FileStatus
    , getFileStatus, getSymbolicLinkStatus, fileSize, fileMode, modificationTime
    , deviceID, fileID, isDirectory, isSymbolicLink
    )
import System.Posix.Types (DeviceID, FileID)
import Ignore (Ignore, parse, ignores)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Msg

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
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
    } deriving (Show, Eq, Ord)

-- | 单侧文件差异（4 种）
--
--   * 'Unchanged'  — 文件存在且未变
--   * 'New'        — 新增
--   * 'Updated'    — 修改
--   * 'Deleted'    — 删除
data DiffState = Unchanged | New | Updated | Deleted
    deriving (Show, Eq)

-- | 单侧文件在比较中的状态（精确 5 种，无冗余）
--
--   自身 4 种差异 + "该侧从未有过此文件"（Absent），
--   两者组合恰好覆盖 classifyOne 的全部输入域，
--   5×5 = 25 种双侧组合全部合法可达，无需 catch-all。
--
--   所有构造子都携带 'FileState'：即使是 'Absent'（该侧从未有过），
--   元信息（size=0, mtime=epoch）仍可用于 FileChange 输出与显示。
data SideState
    = Absent      !FileState      -- ^ 该侧从未有过此文件
    | SNew        !FileState      -- ^ 新增
    | SUpdated    !FileState      -- ^ 修改
    | SUnchanged  !FileState      -- ^ 未变
    | SDeleted    !FileState      -- ^ 删除
    deriving (Show, Eq, Ord)

-- | 文件变更决策，携带双方各自的 SideState
--
--   * 'Added'     — 本方应接收。root1Diff/root2Diff 说明两侧变化
--   * 'Del'   — 本方应删除
--   * 'Conflict'  — 双方冲突
--   * 'Noop' — 无变化
data FileChange
    = Noop FileState SideState SideState
    | Added     FileState SideState SideState
    | Del   FileState SideState SideState
    | Conflict  FileState SideState SideState
    deriving (Show, Eq)

-- ============================================================================
-- 获取当前文件状态（扫描 root 目录）
-- ============================================================================

-- | 从 @.bisync/<profile>/ignore@ 读取忽略规则
--
--   文件名与文件内容一律按 UTF-8 处理（进程入口已固定编码，见 Main.main）。
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
    scanRelFrom [] root root ign

-- | 扫描 root 目录，返回所有文件的 FileState 列表（相对路径）
--
--   等同于 @scanFiles + statRel@。
--   起始时用同步根自身的 (dev,ino) 播种环检测，这样 @link -> .@ 这类
--   指回根目录的链接在第 1 层就会被切断。
currentState :: FilePath -> Maybe String -> IO [FileState]
currentState root mTag = do
    ign <- readProfileIgnore root mTag
    mRootId <- dirIdent root
    let seed = maybe [] (:[]) mRootId
    files <- scanRelFrom seed root root ign
    mapM (statRel root) files

-- | 内部递归扫描（兼容旧接口，不使用任何忽略规则）
scanRel :: FilePath -> FilePath -> IO [FilePath]
scanRel root dir = scanRel' root dir emptyIgnore

-- | 递归扫描，使用给定的 gitignore 规则过滤所有条目
--
--   不过滤隐藏文件（以 @.@ 开头的条目同样参与同步），唯一例外是同步根目录下的
--   工具状态目录 @.bisync@；其余一律交给 ignore 规则 —— 包括工具自己的临时文件：
--   扫描只看文件树的真相，要排除什么就写进 ignore（见 'Tmp'）。
scanRel' :: FilePath -> FilePath -> Ignore -> IO [FilePath]
scanRel' root dir ignoreRules = scanRelFrom [] root dir ignoreRules

-- ============================================================================
-- 扫描：文件阶段（跟随软链接，内容透明）
-- ============================================================================

-- | 条目分类（文件阶段）：**完全按普通文件路径看待**
--
--   跟随 stat，所以指向目录的链接就是目录（进去取内容）、指向文件的链接就是文件
--   （size/mtime 取自目标、写入时穿透到目标），无需单独的"链接"分类；
--   唯一的例外是悬空链接（目标不存在）：那里没有内容可比，整条跳过
--   （链接本身仍由链接阶段负责）。
data EntryKind
    = SkipEntry   -- ^ 悬空链接 / 扫描期间消失：整条跳过
    | DirEntry    -- ^ 目录：递归进去取内容（目录自身不记条目）
    | FileEntry   -- ^ 文件：记为条目（size/mtime 取自跟随后的目标）
    deriving (Eq, Show)

classifyEntry :: FilePath -> IO EntryKind
classifyEntry path = do
    lst <- (try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus))
    case lst of
        Left _ -> return SkipEntry
        Right _ -> do
            tgt <- (try (getFileStatus path) :: IO (Either IOException FileStatus))
            case tgt of
                Left _   -> return SkipEntry
                Right tg -> return (if isDirectory tg then DirEntry else FileEntry)

-- | 目录解析后的身份 (dev, ino)，跟随软链接；不存在返回 Nothing
--   用于切断软链接环（例如 @link -> .@ 这类指回祖先目录的链接）
dirIdent :: FilePath -> IO (Maybe (DeviceID, FileID))
dirIdent path = do
    r <- (try (getFileStatus path) :: IO (Either IOException FileStatus))
    return $ case r of
        Right st -> Just (deviceID st, fileID st)
        Left _   -> Nothing

-- | 递归扫描（文件阶段）：返回文件相对路径
--
--   软链接在这里就是普通路径（stat 跟随、写入穿透到目标）；
--   悬空链接扫不到东西，diff 自然得到 Deleted / None，不做特殊处理。
scanRelFrom
    :: [(DeviceID, FileID)]   -- ^ 当前路径上各级目录（解析后）的身份，用于环检测
    -> FilePath -> FilePath -> Ignore -> IO [FilePath]
scanRelFrom visited root dir ignoreRules = do
    entries <- listDirectory dir
    let atRoot  = dir == root
        visible = [ e | e <- entries
                      , not (atRoot && takeFileName e == ".bisync") ]

    let fullPaths  = [dir </> e | e <- visible]
    kinds <- mapM classifyEntry fullPaths

    -- 将路径转为相对于 root 的形式，供 gitignore 匹配
    let relPaths = map (makeRelative root) fullPaths
    osPaths  <- mapM encodeFS relPaths
    let isDirFlag k = case k of DirEntry -> True; _ -> False
        ignored  = zipWith (ignores ignoreRules) osPaths (map isDirFlag kinds)
        kept     = [ (fp, k)
                   | (fp, k, ign) <- zip3 fullPaths kinds ignored
                   , k /= SkipEntry, not ign ]

    let dirs  = [ d | (d, DirEntry)  <- kept ]
        files = [ f | (f, FileEntry) <- kept ]

    subs <- mapM (descend visited root ignoreRules) dirs
    let relFiles = map (makeRelative root) files
    return (concat subs ++ relFiles)
  where
    descend vs rt ign d = do
        mIdent <- dirIdent d
        case mIdent of
            Just ident
                | ident `elem` vs -> do
                    hPutStrLn stderr $ "WARNING: symlink loop skipped: " ++ d
                    return []
                | otherwise -> scanRelFrom (ident : vs) rt d ign
            Nothing -> return []   -- 竞态：扫描期间消失

-- | 获取单个文件的 FileState（跟随软链接：size/mtime 取自目标）
statRel :: FilePath -> FilePath -> IO FileState
statRel root relPath = do
    let fullPath = root </> relPath
    fs <- getFileStatus fullPath
    let sz = fromIntegral (fileSize fs)
        mt = posixSecondsToUTCTime (realToFrac (modificationTime fs))
        md = fromIntegral (fileMode fs) .&. 0o7777
    return $ FileState sz mt md relPath

-- ============================================================================
-- 扫描：链接阶段（不跟随，只收链接本身）
-- ============================================================================

-- | 链接扫描的条目分类
--
--   注意：指向目录的链接**不**递归进去——链接阶段只负责把链接结构弄一致，
--   链接下的内容由文件阶段（跟随链接）处理。
data LinkEntryKind
    = LinkEntry       -- ^ 指向文件的链接，或悬空链接：都是一条链接条目
    | LinkDirEntry    -- ^ 指向目录的链接：一条链接条目，不递归
    | RealDirEntry    -- ^ 真实目录：递归
    | OtherEntry      -- ^ 普通文件/消失的条目：跳过
    deriving (Eq, Show)

classifyLinkEntry :: FilePath -> IO LinkEntryKind
classifyLinkEntry path = do
    lst <- (try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus))
    case lst of
        Left _ -> return OtherEntry
        Right st
            | isSymbolicLink st -> do
                tgt <- (try (getFileStatus path) :: IO (Either IOException FileStatus))
                return $ case tgt of
                    Right tg | isDirectory tg -> LinkDirEntry
                    _                         -> LinkEntry
            | isDirectory st -> return RealDirEntry
            | otherwise      -> return OtherEntry

-- | 递归收集软链接（不跟随：目录链接不进入），返回相对路径
--
--   与文件阶段同口径：跳过根目录下的 @.bisync@、按 ignore 规则过滤
--   （目录链接按"目录"参与匹配，使 @dir/@ 这类规则能命中）。
scanRelLinks :: FilePath -> FilePath -> Ignore -> IO [FilePath]
scanRelLinks root dir ignoreRules = do
    entries <- listDirectory dir
    let atRoot  = dir == root
        visible = [ e | e <- entries
                      , not (atRoot && takeFileName e == ".bisync") ]

    let fullPaths = [dir </> e | e <- visible]
    kinds <- mapM classifyLinkEntry fullPaths

    let relPaths = map (makeRelative root) fullPaths
    osPaths <- mapM encodeFS relPaths
    let isDirFlag k = case k of LinkDirEntry -> True; RealDirEntry -> True; _ -> False
        ignored  = zipWith (ignores ignoreRules) osPaths (map isDirFlag kinds)
        kept     = [ (fp, k)
                   | (fp, k, ign) <- zip3 fullPaths kinds ignored
                   , k /= OtherEntry, not ign ]

    let links = [ l | (l, k) <- kept, k == LinkEntry || k == LinkDirEntry ]
        dirs  = [ d | (d, RealDirEntry) <- kept ]

    subs <- concat <$> mapM (\d -> scanRelLinks root d ignoreRules) dirs
    return (map (makeRelative root) links ++ subs)

-- | 扫描同步目录中所有"可见"软链接（相对路径）
scanLinks :: FilePath -> Maybe String -> IO [FilePath]
scanLinks root mTag = do
    ign <- readProfileIgnore root mTag
    scanRelLinks root root ign

-- | 扫描所有软链接的 FileState 列表（相对路径）
currentLinkState :: FilePath -> Maybe String -> IO [FileState]
currentLinkState root mTag = do
    links <- scanLinks root mTag
    mapM (statLinkRel root) links

-- | 链接条目的元信息：lstat（不跟随）
--
--   size = st_size = 指向字符串长度，因此判定键 "(长度, 链接自身 mtime)" 就是
--   现有 'sameFile' 那套比较，完全不需要新比较函数。
statLinkRel :: FilePath -> FilePath -> IO FileState
statLinkRel root relPath = do
    fs <- getSymbolicLinkStatus (root </> relPath)
    let sz = fromIntegral (fileSize fs)
        mt = posixSecondsToUTCTime (realToFrac (modificationTime fs))
        md = fromIntegral (fileMode fs) .&. 0o7777
    return $ FileState sz mt md relPath

-- ============================================================================
-- 获取上次同步状态（从 .bisync/ 目录读取）
-- ============================================================================

-- ============================================================================
-- 状态文件：文件条目 / 链接条目
-- ============================================================================

-- | 状态文件种类
--
--   链接条目**完全复用**文件条目的机制与行格式（@size,mtime,path@）：
--   链接的 size = st_size = 指向字符串长度、mtime = 链接自身 mtime，
--   而指向字符串本身作为"内容"在传输阶段传递，不落在状态文件里。
data StateKind = FileKind | LinkKind
    deriving (Eq, Show)

-- | 各类别使用的三个文件名
kindFileNames :: StateKind -> (FilePath, FilePath, FilePath)
kindFileNames FileKind = ("state",      "added",      "deleted")
kindFileNames LinkKind = ("link_state", "link_added", "link_deleted")

-- | 取某个条目的元信息（相对路径 → FileState）
--
--   * 文件条目：跟随软链接（内容语义）
--   * 链接条目：lstat（链接自身语义）
statKindRel :: StateKind -> FilePath -> FilePath -> IO FileState
statKindRel FileKind = statRel
statKindRel LinkKind = statLinkRel

-- | 从状态目录读取上次同步状态（只读，不写回不删文件）
--
--   自动合并 state / added / deleted 三个文件（链接阶段对应
--   @link_state@ / @link_added@ / @link_deleted@）：
--   * state   — 上次同步时的基准状态
--   * added   — 上次同步后新增的条目（覆盖/追加到 state）
--   * deleted — 上次同步后删除的条目（从 state 中移除）
--
--   与 'lastStateKind' 不同，此函数不修改任何文件。
readLastStateKind :: StateKind -> FilePath -> Maybe String -> IO [FileState]
readLastStateKind kind root mTag = do
    let stateDir = root </> ".bisync" </> fromMaybe "" mTag
        (stateName, addedName, deletedName) = kindFileNames kind
        stateFile   = stateDir </> stateName
        addedFile   = stateDir </> addedName
        deletedFile = stateDir </> deletedName

    st <- readEntries stateFile

    hasAdded <- doesFileExist addedFile
    st' <- if hasAdded
        then do adds <- readEntries addedFile; return $ mergeAdds adds st
        else return st

    hasDeleted <- doesFileExist deletedFile
    if hasDeleted
        then do
            raw <- BS.readFile deletedFile
            let dels = Set.fromList (map (T.unpack . T.decodeUtf8) (bsLines raw))
            return $ filter (\fs -> not (Set.member (filename fs) dels)) st'
        else return st'

-- | 从状态目录读取上次同步状态，合并后写回并清理
--
--   内部调用 'readLastStateKind'，然后写回 state 并删除 added / deleted。
lastStateKind :: StateKind -> FilePath -> Maybe String -> IO [FileState]
lastStateKind kind root mTag = do
    let stateDir = root </> ".bisync" </> fromMaybe "" mTag
        (stateName, addedName, deletedName) = kindFileNames kind
        stateFile   = stateDir </> stateName
        addedFile   = stateDir </> addedName
        deletedFile = stateDir </> deletedName

    st <- readLastStateKind kind root mTag

    hasAdded <- doesFileExist addedFile
    hasDeleted <- doesFileExist deletedFile
    when (hasAdded || hasDeleted) $ do
        writeEntries stateFile st
        when hasAdded   $ removeFileSilent addedFile
        when hasDeleted $ removeFileSilent deletedFile

    return st

-- | 兼容旧签名（文件条目）
readLastState :: FilePath -> Maybe String -> IO [FileState]
readLastState = readLastStateKind FileKind

lastState :: FilePath -> Maybe String -> IO [FileState]
lastState = lastStateKind FileKind

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
--
--   用 Map/Set 索引（O((|st|+|adds|) log n)，不再每次线性重找）：
--   * 「同名覆盖」来自 Map，重复名字**保留先出现的那条**
--     （与旧的关联表 @lookup@ 语义一致）
--   * 追加部分先按名字去重：否则 added 里的重复行会在 state 里留两条同名记录
mergeAdds :: [FileState] -> [FileState] -> [FileState]
mergeAdds adds st =
    let addM    = indexStates adds
        stName  = nameSet st
        newOnes = [ a | a <- dedupOn filename adds, not (Set.member (filename a) stName) ]
    in map (\fs -> Map.findWithDefault fs (filename fs) addM) st ++ newOnes

-- ============================================================================
-- Helpers
-- ============================================================================

-- ============================================================================
-- 索引 / 去重（全部用 Map/Set：不要再用 linear lookup / notElem / nub）
-- ============================================================================

-- | 文件名 → 条目。同名重复时**保留先出现的那条**（与关联表 @lookup@ 语义一致）
indexStates :: [FileState] -> Map.Map FilePath FileState
indexStates = Map.fromListWith (\_ old -> old) . map (\fs -> (filename fs, fs))

-- | 一批条目占用哪些文件名
nameSet :: [FileState] -> Set.Set FilePath
nameSet = Set.fromList . map filename

-- | 顺序保持的去重（O(n log n)，替代 @Data.List.nub@ 的 O(n²)）：
--   按给定键判重，保留每个键**第一次**出现的那条
--
--   注意：**必须保序**（root1 的顺序在前），所以不能用 @Map.keys@ / @Set.toList@。
dedupOn :: Ord k => (a -> k) -> [a] -> [a]
dedupOn key = go Set.empty
  where
    go _    []       = []
    go seen (x : xs) | Set.member (key x) seen = go seen xs
                     | otherwise               = x : go (Set.insert (key x) seen) xs

dedup :: Ord a => [a] -> [a]
dedup = dedupOn id

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
-- Helper 复用：added/deleted 追加
-- ============================================================================

-- | 追加一条记录到 added / link_added（写入必须成功）
--
--   元信息按 kind 取：文件条目跟随软链接，链接条目用 lstat。
appendAddedKind :: StateKind -> FilePath -> Maybe String -> FilePath -> IO ()
appendAddedKind kind root mTag relPath = do
    let stateDir = root </> ".bisync" </> fromMaybe "" mTag
        (_, addedName, _) = kindFileNames kind
        addedFp = stateDir </> addedName
    createDirectoryIfMissing True stateDir
    fs <- statKindRel kind root relPath
    let line = T.encodeUtf8 (T.pack (show (size fs) ++ "," ++ show (posixSecs (mtime fs)) ++ ","))
            <> T.encodeUtf8 (T.pack relPath)
            <> BS.singleton 10
    BS.appendFile addedFp line
    -- 确认写入
    content <- BS.readFile addedFp
    when (not (BS.isSuffixOf line content)) $
        error $ "appendAdded: verification failed for " ++ addedFp

-- | 追加一条记录到 deleted / link_deleted（写入必须成功）
appendDeletedKind :: StateKind -> FilePath -> Maybe String -> FilePath -> IO ()
appendDeletedKind kind root mTag relPath = do
    let stateDir = root </> ".bisync" </> fromMaybe "" mTag
        (_, _, deletedName) = kindFileNames kind
        deletedFp = stateDir </> deletedName
    createDirectoryIfMissing True stateDir
    let line = T.encodeUtf8 (T.pack relPath) <> BS.singleton 10
    BS.appendFile deletedFp line
    -- 确认写入
    content <- BS.readFile deletedFp
    when (not (BS.isSuffixOf line content)) $
        error $ "appendDeleted: verification failed for " ++ deletedFp

-- | 兼容旧签名（文件条目）
appendAdded :: FilePath -> Maybe String -> FilePath -> IO ()
appendAdded = appendAddedKind FileKind

appendDeleted :: FilePath -> Maybe String -> FilePath -> IO ()
appendDeleted = appendDeletedKind FileKind

-- | 刚在本端建/改了链接 @linkRel@：把本端文件状态里该链接**子树**下的基线条目作废。
--
--   为什么：链接此前在本端不可达（目标不存在、或本端原本是真实目录），扫描看不到
--   链接下的内容，那些基线条目已经过期。若不作废，diff 会得出 Deleted，进而让
--   **对端删掉自己的真实内容**；作废后这里对文件阶段就是"从未有过"（None），
--   走接收，等文件传过来（顺带把链接目标目录建出来）再以 added 记回正确状态。
--
--   只有一个副作用要注意：它会把合并后的基线写回 state（与 lastStateKind 一致）。
discardStateUnder :: FilePath -> Maybe String -> FilePath -> IO ()
discardStateUnder root mTag linkRel = do
    st <- lastStateKind FileKind root mTag
    let (stateName, _, _) = kindFileNames FileKind
        stateFile = root </> ".bisync" </> fromMaybe "" mTag </> stateName
        prefix  = dropTrailingPathSeparator linkRel
        under f = f == prefix || take (length prefix + 1) f == prefix ++ "/"
        dropped = [ filename fs | fs <- st, under (filename fs) ]
        kept    = [ fs | fs <- st, not (under (filename fs)) ]
    when (not (null dropped)) $ do
        writeEntries stateFile kept
        hPutStrLn stderr $ "  (baseline dropped under new symlink " ++ linkRel
            ++ ": " ++ show (length dropped) ++ " entries)"

-- | 比较两个 FileState 是否相同（基于 size 和整数秒级 mtime）
--   mode 不参与比较——权限变更不触发重新同步
sameFile :: FileState -> FileState -> Bool
sameFile a b =
    size a == size b &&
    posixSecs (mtime a) == posixSecs (mtime b)

-- | UTCTime → 整数 POSIX 秒
posixSecs :: UTCTime -> Integer
posixSecs = floor . utcTimeToPOSIXSeconds

-- | 计算单侧差异：last vs cur → [SideState]
--
--   索引化：一侧 O(n log n)，不再逐条线性查 prev / 线性 notElem 扫 cur
--   （n 大时那是 O(n²)，每次运行都要跑）。
diffOneSide :: [FileState] -> [FileState] -> [SideState]
diffOneSide prev cur =
    let prevM  = indexStates prev
        curSet = nameSet cur

        curDiffs = map (\fs ->
            case Map.lookup (filename fs) prevM of
                Nothing     -> SNew fs
                Just prevFs -> if sameFile fs prevFs
                    then SUnchanged fs
                    else SUpdated fs
            ) cur

        delDiffs = [ SDeleted fs
                   | fs <- prev
                   , not (Set.member (filename fs) curSet)
                   ]
    in curDiffs ++ delDiffs

-- ============================================================================
-- 状态列表的消息编解码（见 Msg：一行一个规范 JSON）
-- ============================================================================

-- | 状态列表 → 消息值（**位置式数组**，省掉每条重复的键名）：
--
--   > [[size,mtime,mode,name], ...]
--
--   设计取舍（2026-09-22 与用户定）：
--   * 不用二进制 JSON（CBOR）—— 实测它只省字节（-21%），不省 CPU；
--   * 批量数据用**数组**（对象形状 88.8 B/条 → 数组 55.8 B/条，解码也快一倍）；
--   * 名字只在这里转一次（'Msg.bytes' 接 UTF-8 字节），不再 String↔Text 反复转。
stateValue :: [FileState] -> Msg.Value
stateValue = Msg.arr . map one
  where
    one fs = Msg.arr [ Msg.int (size fs)
                     , Msg.int (posixSecs (mtime fs))
                     , Msg.int (fromIntegral (mode fs))
                     , Msg.bytes (T.encodeUtf8 (T.pack (filename fs)))
                     ]

-- | 状态列表的**流式编码**：直接写进 Builder，不建 `[Value]` 中间树。
--
--   输出字节与 'stateValue' 完全一致（同一套规范形状），只是省掉每条 4 个
--   `Value` 节点的分配——这就是 aeson `toEncoding` 那套“不建中间结构”的思路，
--   在 bytestring 上自己写只要十来行，不用引依赖。
stateBuilder :: [FileState] -> B.Builder
stateBuilder sts = B.word8 91 <> go sts <> B.word8 93
  where
    go []       = mempty
    go [x]      = one x
    go (x : xs) = one x <> B.word8 44 <> go xs
    one fs = B.word8 91
          <> Msg.intBuilder (size fs)                <> B.word8 44
          <> Msg.intBuilder (posixSecs (mtime fs))   <> B.word8 44
          <> Msg.intBuilder (fromIntegral (mode fs)) <> B.word8 44
          <> Msg.strBuilder (T.encodeUtf8 (T.pack (filename fs)))
          <> B.word8 93

-- | 从消息值解析状态列表（形状见 'stateValue'）
parseStateValue :: Msg.Value -> [FileState]
parseStateValue v = case Msg.asArr v of
    Nothing -> error "parseStateValue: expected an array"
    Just xs -> map oneEntry xs
  where
    oneEntry e = case Msg.asArr e of
        Just [a, b, c, d]
            | Just sz <- Msg.asInt a
            , Just mt <- Msg.asInt b
            , Just md <- Msg.asInt c
            , Just nm <- Msg.asStr d ->
                FileState sz (posixSecondsToUTCTime (fromIntegral mt))
                          (fromIntegral md) (T.unpack (T.decodeUtf8 nm))
        _ -> error "parseStateValue: expected [size,mtime,mode,name]"

