module Main where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, when)
import System.Directory
    ( doesDirectoryExist, doesFileExist
    , getCurrentDirectory, listDirectory
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Posix.Files (readSymbolicLink)

import Data.List (sort)

import qualified Data.Map as Map

import Data.Time (UTCTime, TimeZone)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)

import Arg (Root(..), parseRoot, parseSyncThree)
import Exe (RootSource(..), printBanner, runSync)
import Helper (runHelper)
import State
    ( FileState(..), DiffState(..), StateKind(..)
    , currentState, currentLinkState, diffOneSide
    , readLastStateKind, indexStates, posixSecs
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
        ["diff", profile]  -> cmdDiff profile
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
    , "  hsbisync diff <profile>            Preview local changes (read-only)"
    , "                                      files + symlinks, with size/mtime delta"
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
    -- 后面全交给 runSync 的 6 步：
    -- sync prepare（目录 + 握手 + repo）→ ignore → 链接 → 状态 → 计算 → 执行
    runSync RootsGiven profile root1 root2
syncCmd _ = die "Usage: hsbisync sync <profile> [<root1> <root2>]"

-- ============================================================================
-- sync (profile only) — 读 .bisync/<profile>/repo，与记录的对端同步
-- ============================================================================

-- | 只给 profile：对端地址从本仓库的 @.bisync/\<profile\>/repo@ 读。
--   这种模式下目录**不创建、repo 也不重写**：第 1 步（sync prepare）只检查两端目录存在。
--
--   缺东西分两级报（用户 2026-09-23 定）：
--   * @.bisync/\<profile\>@ 目录**本身不存在** → 这是 profile 名写错了，不是
--     "忘了建 repo"：直接把这里**真实存在**的 profile 列出来；
--   * 目录在、@repo@ 不在 → 这时才提示手工写一行对端地址。
syncFromRepo :: String -> IO ()
syncFromRepo profile = do
    printBanner                                 -- 标题最快，连读 repo 之前就出
    cwd <- getCurrentDirectory
    let profDir  = cwd </> ".bisync" </> profile
        repoFile = profDir </> "repo"

    hasDir <- doesDirectoryExist profDir
    unless hasDir $ do
        names <- existingProfiles (cwd </> ".bisync")
        die $ "Profile '" ++ profile ++ "' not found in " ++ cwd ++ "\n"
           ++ case names of
                  [] -> "No profiles found."
                  _  -> "Profiles:\n" ++ unlines [ "  " ++ n | n <- names ]

    exists <- doesFileExist repoFile
    unless exists $
        die $ "No .bisync/" ++ profile ++ "/repo found in " ++ cwd
           ++ "\nCreate it manually with the peer address, e.g.:"
           ++ "\n  echo 'user@host:/path' > " ++ repoFile

    peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
    let root1 = Local cwd          -- 当前目录，必然存在
        root2 = parseRoot peer
    runSync RootsFromRepo profile root1 root2

-- | @.bisync/@ 下现有的 profile 名（按名字排序；目录不存在就是空）
existingProfiles :: FilePath -> IO [String]
existingProfiles bisyncDir = do
    hasDir <- doesDirectoryExist bisyncDir
    if not hasDir
        then return []
        else sort . filter (not . isHidden) <$> listDirectory bisyncDir

-- ============================================================================
-- diff 子命令（只读预览）
-- ============================================================================

-- | @hsbisync diff <profile>@ — 只读预览：**本地**基线与当前目录的差异。
--
--   两棵树都看，各自与各自的基线对比（互不干扰），顺序为**先链接树后文件树**：
--   * 软链接树 — 基线 @link_state@/link_added/... vs 'currentLinkState'（不跟随）
--   * 文件树   — 基线 @state@/added/deleted       vs 'currentState'（跟随软链接）
--
--   为什么两棵都要：链接的“内容”是**指向字符串**、且链接自身的 mtime 也是被同步的
--   属性，只报文件树时“链接被指向”这类变化完全不可见。
--
--   除状态与路径外，每条变更还会**缩进几行标出真正变化的维度**
--   （长度 / mtime 的旧值 → 新值）。目的就是让「一百多条全报 Updated，
--   可我根本没批量改过这些文件」这类怀疑一眼能查：
--   * 只差 mtime 的那批，看新值是不是**集中/相同**的时刻 ——
--     是，则说明整棵树被 @cp -r@ / @git checkout@ / 重装之类的操作覆盖过；
--   * 只差长度（文件 = size，链接 = 指向字符串长度）的，才是内容真的变了。
--
--   时间按**本地时区**打印（与 @ls -l@ / @stat@ 一致，方便直接对照）。
--   全程只读：不写 state，也不删 added/deleted（用的是 'readLastStateKind'）。
cmdDiff :: String -> IO ()
cmdDiff profile = do
    cwd <- getCurrentDirectory
    tz  <- getCurrentTimeZone
    -- 顺序：先软链接树（结构层，条数少），再文件树（内容层，条数多）
    diffOneTree tz cwd profile LinkKind
    diffOneTree tz cwd profile FileKind

-- | 打印一棵树的差异（文件树 / 软链接树）
--
--   基线取 @.bisync/\<profile\>@ 下对应的一对三个文件（只读）；
--   当前状态现场扫描（文件跟随软链接，链接 lstat 不跟随）。
diffOneTree :: TimeZone -> FilePath -> String -> StateKind -> IO ()
diffOneTree tz cwd tag kind = do
    prev <- readLastStateKind kind cwd (Just tag)
    cur  <- case kind of
        FileKind -> currentState     cwd (Just tag)
        LinkKind -> currentLinkState cwd (Just tag)

    let prevM = indexStates prev
        -- (DiffState, 条目, 基线)：
        --   New     → 条目 = 当前，基线 = Nothing
        --   Updated → 条目 = 当前，基线 = 上一版
        --   Deleted → 条目本身就是基线那条（diffOneSide 的约定），基线 = 同一条
        changed = [ (ds, fs, Map.lookup (filename fs) prevM)
                  | (ds, fs) <- diffOneSide prev cur
                  , ds /= Unchanged
                  ]

    putStrLn ""
    putStrLn $ "== " ++ treeLabel kind ++ " =="
    putStrLn $ "Last merged state: " ++ show (length prev) ++ " " ++ noun kind
    putStrLn $ "Current tree:      " ++ show (length cur)  ++ " " ++ noun kind

    if null changed
        then putStrLn "No changes."
        else do
            putStrLn $ "\nChanges (" ++ show (length changed) ++ " entries):"
            case updatedBreakdown kind changed of
                Nothing -> return ()
                Just s  -> putStrLn $ "  Updated: " ++ s
            putStrLn ""
            forM_ changed $ \(ds, fs, mPrev) -> do
                putStrLn $ "  " ++ pad 8 (show ds) ++ "  " ++ filename fs
                forM_ (detailLines tz kind ds fs mPrev) $ \l ->
                    putStrLn $ detailPad ++ l
                when (kind == LinkKind) $ do
                    -- 链接当前指向哪里：只有链接**还在**（New/Updated）才读得到；
                    -- 已删除的读不到——基线只存长度+mtime，不存目标字符串。
                    mTgt <- linkTarget (cwd </> filename fs)
                    forM_ mTgt $ \t -> putStrLn $ detailPad ++ pad 8 "target" ++ "-> " ++ t

-- | 该棵树的标题
treeLabel :: StateKind -> String
treeLabel FileKind = "Files Tree"
treeLabel LinkKind = "Symlinks Tree"

-- | 计数单位
noun :: StateKind -> String
noun FileKind = "files"
noun LinkKind = "links"

-- | 长度维度在两类条目里的名字：
--   * 文件 — @size@ = st_size = 字节数
--   * 链接 — @len@  = st_size = **指向字符串长度**（状态文件里存的就是它）
--   分开叫是为了避免看到 “size 12 → 20” 以为链接本身有多少字节内容。
sizeWord :: StateKind -> String
sizeWord FileKind = "size"
sizeWord LinkKind = "len"

-- | 读软链接指向（不跟随；路径不存在/不是链接 → Nothing）
linkTarget :: FilePath -> IO (Maybe FilePath)
linkTarget fp = do
    r <- try (readSymbolicLink fp) :: IO (Either IOException FilePath)
    return $ either (const Nothing) Just r

-- | 明细行的缩进宽度：与上面 @"  " ++ pad 8 …"  ++ "  " ++ 路径@ 的路径列对齐
--   （2 + 8 + 2 = 12），明细自己也用同一个 8 栏写标签。
detailIndent :: Int
detailIndent = 12

detailPad :: String
detailPad = replicate detailIndent ' '

-- | 明细行：只列**真正不同**的维度，没变的维度不打（上百条时逐条打全就是噪声）。
--   * 'New'     → 该条目现在的 长度 / mtime
--   * 'Deleted' → 它当时（基线里）的长度 / mtime
--   * 'Updated' → 长度 / mtime 各一行，**只打变了的那些**
detailLines :: TimeZone -> StateKind -> DiffState -> FileState -> Maybe FileState -> [String]
detailLines tz kind ds fs mPrev = case (ds, mPrev) of
    (New, _)          -> attrs fs
    (Deleted, Just p) -> attrs p
    (Updated, Just p) -> sizeLine p fs ++ mtimeLine p fs
    _                 -> []
  where
    -- New / Deleted 没有“旧→新”可比，就把当前（或当时）两个维度写在同一行
    attrs x = [ pad 8 (sizeWord kind) ++ show (size x)
                ++ ", mtime " ++ fmtTime tz (mtime x) ]
    sizeLine p x
        | size p == size x = []
        | otherwise        = [ pad 8 (sizeWord kind) ++ show (size p) ++ " → " ++ show (size x) ]
    mtimeLine p x
        | posixSecs (mtime p) == posixSecs (mtime x) = []
        | otherwise = [ pad 8 "mtime" ++ fmtTime tz (mtime p)
                        ++ " → " ++ fmtTime tz (mtime x) ]

-- | UTCTime → 本地时区的 @YYYY-MM-DD HH:MM:SS@
fmtTime :: TimeZone -> UTCTime -> String
fmtTime tz = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utcToLocalTime tz

-- | 'Updated' 条目的细分统计：只差 mtime / 只差长度 / 两者都差。
--
--   没有 Updated 条目时返回 'Nothing'（不打印这一行）。
--   由 'sameFile' 的定义，三者之和必然等于 Updated 条数。
updatedBreakdown :: StateKind -> [(DiffState, FileState, Maybe FileState)] -> Maybe String
updatedBreakdown kind changed =
    case [ (p, fs) | (Updated, fs, Just p) <- changed ] of
        []  -> Nothing
        ups ->
            let w         = sizeWord kind
                mtimeOnly = length [ () | (p, fs) <- ups, size p == size fs ]
                sizeOnly  = length [ () | (p, fs) <- ups
                                       , size p /= size fs
                                       , posixSecs (mtime p) == posixSecs (mtime fs) ]
                both      = length ups - mtimeOnly - sizeOnly
            in Just $ show (length ups) ++ " total, "
                    ++ show mtimeOnly ++ " mtime-only, "
                    ++ show sizeOnly  ++ " " ++ w ++ "-only, "
                    ++ show both      ++ " " ++ w ++ "+mtime"

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
    hasDir <- doesDirectoryExist bisyncDir
    if not hasDir
        then putStrLn "No .bisync directory found."
        else do
            names <- existingProfiles bisyncDir
            if null names
                then putStrLn "No profiles found."
                else do
                    putStrLn "Profiles:"
                    forM_ names $ \name ->
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
