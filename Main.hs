module Main where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM_, unless, when)
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
import Exe
    ( RootSource(..), convertFromJail, convertToJail
    , printBanner, runSync, setAutoConfirm, setJailTargets, setUnjailTargets
    )
import Help (showHelp)
import Helper (runHelper)
import State
    ( FileState(..), DiffState(..), SideState(..), StateKind(..)
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
    args0 <- getArgs
    -- --jail <root> / --unjail <root>（都可重复）：本次同步显式地在该端
    -- 建立 / 删除 .bisync/link_table.json（存在性就是 jail 开关）。
    -- 用空格分隔而不是 --jail=<root>：路径里带引号/空格时省得和 shell 转义拧。
    (args, jailSpecs, unjailSpecs) <- case splitRepoModes args0 of
        Left e  -> die e
        Right r -> return r
    setJailTargets jailSpecs
    setUnjailTargets unjailSpecs
    -- --no-confirm：所有确认自动答 y（脚本 / 无终端时用）
    let (syncArgs, noConfirm) = splitNoConfirm args
    setAutoConfirm noConfirm
    -- 每个子命令**先按参数个数**校验：数量不对就直接给出该子命令的正确用法，
    -- 多余参数也算错误（不静默忽略）。只有数量正确时才谈得上"实质性的误用"。
    case syncArgs of
        "--helper" : rest  -> helperMain rest
        "help" : rest
            | null rest    -> showHelp
            | otherwise    -> die "Usage: hsbisync help"
        "--help" : rest
            | null rest    -> showHelp
            | otherwise    -> die "Usage: hsbisync --help"
        "diff" : rest      -> cmdDiff rest
        "profile" : rest   -> cmdProfile rest
        "jail" : rest     -> cmdConvert True rest
        "unjail" : rest   -> cmdConvert False rest
        "sync" : rest      -> syncCmd rest
        _                  -> showHelp

-- | 完整说明文在 Help.hs（单一维护点）；此处只负责分发。

-- | 从参数里抽出 @--jail@ / @--unjail@ 标记（都可重复）：它后面紧跟的那个
--   路径就是 Jail 端 / 要被显式删掉链接表的那一端。
--
--   只移除开关本身，**路径仍留在位置参数里** —— 所以：
--
--   > sync p --jail A B          -- ✓ A 建表，两端是 A 和 B
--   > sync p --jail A --unjail B -- ✓ A 建表、B 删表
--   > sync p --jail A --jail B   -- ✓ 两端都建表
--   > sync p A B --jail A        -- ✗ 去掉开关后有三个路径 → 参数个数校验报错
--
--   路径里的空格/引号完全由 shell 负责，程序不做任何拼接。
--   同一个根同时出现在两个列表里由 Exe.applyRepoModes 报错。
splitRepoModes :: [String] -> Either String ([String], [String], [String])
splitRepoModes = go [] [] []
  where
    go pos js us [] = Right (reverse pos, reverse js, reverse us)
    go _ _ _ ["--jail"] = Left "--jail requires a path: --jail <root>"
    go _ _ _ ["--unjail"] = Left "--unjail requires a path: --unjail <root>"
    go pos js us ("--jail" : x : rest) = go (x : pos) (x : js) us rest
    go pos js us ("--unjail" : x : rest) = go (x : pos) js (x : us) rest
    go pos js us (a : rest) = go (a : pos) js us rest

-- | 抽掉无值的 @--no-confirm@（可以出现在任意位置）
splitNoConfirm :: [String] -> ([String], Bool)
splitNoConfirm xs =
    (filter (/= "--no-confirm") xs, "--no-confirm" `elem` xs)

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
-- jail / unjail 子命令：在当前目录做仓库形态转换
-- ============================================================================

-- | @hsbisync jail@ / @hsbisync unjail@
--
--   只作用于**当前目录**（就是同步根本端）。已经是目标形态则什么都不做。
--   转换只做一次；之后同步流程根据 link_table.json 在不在自动选口径。
--
--   链接表是**仓库级**的（@.bisync/link_table.json@），与 profile 无关；
--   旧命令行的 @<profile>@ 仍然接受（兼容），但直接忽略并提示一句。
cmdConvert :: Bool -> [String] -> IO ()
cmdConvert toJail args = case args of
    []  -> go
    [p] -> do
        putStrLn $ "  note: the link table is repo-wide (.bisync/link_table.json);"
                ++ " profile " ++ p ++ " is ignored"
        go
    _   -> die ("Usage: hsbisync " ++ verb)
  where
    go = do
        cwd <- getCurrentDirectory
        if toJail then convertToJail cwd else convertFromJail cwd
    verb = if toJail then "jail" else "unjail"

-- ============================================================================
-- sync 子命令
-- ============================================================================

-- | hsbisync sync <profile> [<root1> <root2>]
--   一个参数：读 .bisync/<profile>/repo 获取对端地址
--   三个参数：profile + 双端直接同步
syncCmd :: [String] -> IO ()
syncCmd [profile] = syncFromRepo profile
syncCmd args@[_, _, _] = do
    -- 先把命令行解析并验完（profile 名可能非法），确认无误才打横幅：
    -- 横幅 = "同步流程已经开始" 的标志，前提都没过就看见三行 ===== 很误导
    -- （用户 2026-10-01 定）。
    (profile, root1, root2) <- parseSyncThree args
    printBanner
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
--
--   **两个前提都在横幅之前查**（用户 2026-10-01 定）：横幅是"同步流程已开始"的标志，
--   要等 profile 与 repo 都确认无误才打 —— 名字写错却先看到三行 ===== 很误导。
syncFromRepo :: String -> IO ()
syncFromRepo profile = do
    cwd <- getCurrentDirectory
    profDir <- requireProfileDir cwd profile           -- 前提 1：profile 存在
    let repoFile = profDir </> "repo"

    exists <- doesFileExist repoFile                   -- 前提 2：repo 存在
    unless exists $
        die $ ".bisync/" ++ profile ++ "/repo does not exist\n"
           ++ "Create it manually with the peer address, e.g.:"
           ++ "\n  echo 'user@host:/path' > .bisync/" ++ profile ++ "/repo"

    printBanner                                        -- 前提全过 → 进入同步流程
    peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
    let root1 = Local cwd          -- 当前目录，必然存在
        root2 = parseRoot peer
    runSync RootsFromRepo profile root1 root2

-- | 检查 @.bisync/\<profile\>@ 目录存在，返回该目录；不存在就报错退出。
--
--   **sync 与 diff 共用**：profile 名写错（目录本身不存在）不该被当成"什么都还没同步"，
--   所以两者都用同一套警示，并列出这里**真实存在**的 profile。
--   "目录在、repo 不在"是另一回事（只有 sync 需要 repo），在那个函数里单独报。
--
--   提示只说 @.bisync/\<profile\>@ 这个**相对路径**（用户 2026-10-01 定）：
--   命令本来就只认当前目录，报绝对路径既长又没有任何额外信息。
requireProfileDir :: FilePath -> String -> IO FilePath
requireProfileDir cwd profile = do
    let profDir = cwd </> ".bisync" </> profile
    hasDir <- doesDirectoryExist profDir
    unless hasDir $ do
        names <- existingProfiles (cwd </> ".bisync")
        die $ ".bisync/" ++ profile ++ " does not exist\n"
           ++ case names of
                  [] -> "No profiles found."
                  _  -> "Profiles:\n" ++ unlines [ "  " ++ n | n <- names ]
    return profDir

-- | @.bisync/@ 下现有的 profile 名（按名字排序；目录不存在就是空）
--
--   只认**子目录**：@.bisync@ 下面还有仓库级的配置文件（@ignore@、
--   @link_table.json@），它们不是 profile。
existingProfiles :: FilePath -> IO [String]
existingProfiles bisyncDir = do
    hasDir <- doesDirectoryExist bisyncDir
    if not hasDir
        then return []
        else do
            entries <- listDirectory bisyncDir
            dirs <- filterM (doesDirectoryExist . (bisyncDir </>))
                            [ e | e <- entries, not (isHidden e) ]
            return (sort dirs)

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
--
--   profile 目录不存在直接报错（'requireProfileDir'，与 @sync \<profile\>@ 同一套警示）：
--   否则"名字写错"会被伪装成"基线为空 → 整棵树都是 New"。
cmdDiff :: [String] -> IO ()
cmdDiff [profile] = diffProfile profile
cmdDiff _         = die "Usage: hsbisync diff <profile>"

-- | 实际执行 diff（参数个数已由 'cmdDiff' 校验）
diffProfile :: String -> IO ()
diffProfile profile = do
    cwd <- getCurrentDirectory
    -- 与 sync 一样先查 profile 目录：不存在就是名字写错（而不是"基线为空"），
    -- 否则这里会把整棵树当成 New 全列一遍，看着像正常输出。
    _   <- requireProfileDir cwd profile
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
        FileKind -> currentState     cwd
        LinkKind -> currentLinkState cwd

    let prevM = indexStates prev
        -- (DiffState, 条目, 基线)：
        --   New     → 条目 = 当前，基线 = Nothing
        --   Updated → 条目 = 当前，基线 = 上一版
        --   Deleted → 条目本身就是基线那条（diffOneSide 的约定），基线 = 同一条
        changed = [ (toDiffState ss, ssFS ss, Map.lookup (filename (ssFS ss)) prevM)
                  | ss <- diffOneSide prev cur
                  , toDiffState ss /= Unchanged
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
-- profile 子命令（只读：列出 / 显示各 profile 的 repo 与仓库级 ignore）
-- ============================================================================

-- | hsbisync profile — 列出所有 profile（每个带 repo 详情，另外单独列一次 ignore）。
--
--   没有"只显示某一个"的模式：列表输出本身就包含全部信息，没必要多一个入口。
--   注：repo/ignore 不再由 profile 命令直接写入 ——
--   repo 在 hsbisync sync <profile> <root1> <root2> 时自动记录；
--   ignore 靠每次 sync 前的 ignore 预同步在两端对齐。
cmdProfile :: [String] -> IO ()
cmdProfile [] = cmdProfileList
cmdProfile _  = die "Usage: hsbisync profile"

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
            unless (null names) $ do
                putStrLn "Profiles:"
                forM_ names $ \name ->
                    showProfile bisyncDir name
            -- ignore 是仓库级的（.bisync/ignore），所有 profile 共用 → 只列一次
            showRepoIgnore bisyncDir

-- | 仓库级 ignore（@.bisync/ignore@）
showRepoIgnore :: FilePath -> IO ()
showRepoIgnore bisyncDir = do
    let ignoreFile = bisyncDir </> "ignore"
    hasIgnore <- doesFileExist ignoreFile
    ignoreLines <- if hasIgnore
        then lines <$> readFile ignoreFile
        else return []
    putStrLn "Ignore (repo-wide):"
    if null ignoreLines
        then putStrLn "    -"
        else forM_ ignoreLines $ \l -> putStrLn $ "    " ++ l

-- | 显示单个 profile 的 repo
showProfile :: FilePath -> String -> IO ()
showProfile bisyncDir name = do
    let repoFile = bisyncDir </> name </> "repo"
    hasRepo <- doesFileExist repoFile
    peerText <- if hasRepo
        then do
            peer <- filter (not . (`elem` "\n\r")) <$> readFile repoFile
            return $ "repo: " ++ peer
        else return "repo: -"
    putStrLn $ "  " ++ name
    putStrLn $ "    " ++ peerText

-- | 文件名是否以 '.' 开头
isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False

-- | SideState → DiffState（仅用于 diff 显示，Absent 映射为 New）
toDiffState :: SideState -> DiffState
toDiffState (SNew _)      = New
toDiffState (SUpdated _)  = Updated
toDiffState (SUnchanged _) = Unchanged
toDiffState (SDeleted _)  = Deleted
toDiffState (Absent _)    = New

-- | SideState → FileState
ssFS :: SideState -> FileState
ssFS (SNew fs)      = fs
ssFS (SUpdated fs)  = fs
ssFS (SUnchanged fs) = fs
ssFS (SDeleted fs)  = fs
ssFS (Absent fs)    = fs

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

-- ============================================================================
-- 工具
-- ============================================================================

die :: String -> IO a
die msg = do
    putStrLn $ "Error: " ++ msg
    exitFailure
