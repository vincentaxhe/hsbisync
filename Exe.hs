module Exe where

import Control.Concurrent.Async (concurrently)
import Control.Exception (ErrorCall, IOException, bracket, catch, displayException, try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>))
import System.IO
    ( IOMode(ReadMode), hClose, hFileSize, hFlush, openBinaryFile, stdout, stdin
    , hSetEcho, hSetBuffering, BufferMode(..)
    , hReady
    )
import System.Posix.User (getEffectiveUserName)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Connect (destroyHelper, handshakeInfo, prepareRemotes, spawnHelper)
import State
    ( FileState(..), FileChange(..), StateKind(..)
    , parseStateValue, statRel
    , currentStateWith, currentLinkState
    , lastStateKind, appendAddedKind, appendDeletedKind, discardStateUnder, posixSecs
    , indexStates, dedup
    )
import Sync (sync, SyncAction(..), resolveConflicts, SideStats(..), sideStateLabel)
import Write
    ( HelperHandle(..), call, readMsg, sendMsg, hGetExactly
    , writeLinkVerified, readLinkContent
    , hashBytes, hashFile, removeFileSafe, removeLinkSafe, getIgnoreRemote, setMtimeOnly
    , writeRepoFile
    , copyFileStream, copyExactly, copyExactlyHash, discardExactly, padZeros
    , openTargetTemp, commitReceived, removeQuietly
    )
import qualified Msg
import Arg
    ( Root(..)
    , ensureRoot, formatOwnPath, requireRoot, rootLabel
    )

-- ============================================================================
-- 统一入口
-- ============================================================================

-- | 两个同步地址从哪来 —— 决定第 1 步（sync prepare）做什么
--
--   * 'RootsGiven'   —— 命令行写明了两个地址（hsbisync sync <profile> <r1> <r2>）：
--     缺目录就创建，并把对端地址写进双方的 repo，供以后免参同步；
--   * 'RootsFromRepo' —— 只给了 profile（地址来自本仓库的 repo 文件）：
--     不建、不写，只要求两端目录都真实存在。
data RootSource = RootsGiven | RootsFromRepo
    deriving (Eq, Show)

-- | 程序标题：总是最先打印（在任何检查、握手、建目录之前）
printBanner :: IO ()
printBanner = do
    putStrLn "==================================="
    putStrLn "  hsbisync - Bidirectional Sync"
    putStrLn "==================================="

-- | 同步主流程：标题之后一共 **6 步**
--
--   0. （不编号）先明确两端地址与 profile
--   1. sync prepare：确保同步需要的东西 + 与远端握手 + 写 repo（合成一步，见下）
--   2. 同步 ignore 规则（无条件先对齐，之后 ignore 不再参与文件同步）
--   3. 同步软链接结构（**闸门**：这一步没达成一致，4~6 步整体跳过）
--   4. 读两端状态
--   5. 计算同步动作（diff + 冲突消解 + 额外内容校验）
--   6. 列动作、确认、执行（真正的传输与记录）
--
--   第 1 步内部顺序固定为 **目录 → 握手 → repo**，三者是真依赖：
--   * 握手 = "按远端同步根把一个 helper ssh 起来"，所以根得先就位；
--   * 写远端 repo 又要用握手拿到的 helper 绝对路径。
--   可见"确保同步需要的东西"与"握手"本就是同一件事的两半：以前把握手单列成
--   第 1 步、把目录放第 2 步，等于要求"先握手、后建目录"，依赖是反的。
--
--   走到第 2 步时保证：两个同步目录一定存在（第 1 步要么建了、要么查了）。
runSync :: RootSource -> String -> Root -> Root -> IO ()
runSync srcKind profile root1 root2 = do
    let (mHost1, path1) = case root1 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        (mHost2, path2) = case root2 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        mTag = Just profile

    -- 阶段 0：先明确两端地址（本地端就是一个路径，没有 local: 这种写法）
    putStrLn $ "\n  root1 = " ++ rootLabel root1
    putStrLn $ "  root2 = " ++ rootLabel root2
    putStrLn $ "  profile: " ++ profile

    -- 阶段 1：sync prepare —— **目录 → 握手 → repo**（依赖原因见函数注释）
    --
    --   目录：写明地址 → 缺目录就建（只建一层，见 'ensureRoot'）；
    --         只给 profile → 只查两端目录存在，不建不写（缺了就是错误退出）。
    --   握手：问出远端 helper 的真实路径并记进缓存，此后所有远端调用都复用它。
    --   repo：只有写明地址的模式才写（免参同步时 repo 是权威输入，不碰）。
    --
    --   输出（用户 2026-09-23 定）：**按 root1 / root2 side 分组**，每组只列真的发生了
    --   的动作（@create … [create OK]@ / @repo = … [write OK]@），ssh 端另报
    --   @helper@ / @home@；所以这三件事都先静默做完、把结果收集起来，
    --   最后交给 'showPrepare' 一次打印。
    putStrLn "\n[1/6] Sync prepare ..."
    (created1, created2) <- case srcKind of
        RootsGiven -> do
            c1 <- ensureRoot root1
            c2 <- ensureRoot root2
            return (c1, c2)
        RootsFromRepo -> do
            requireRoot root1
            requireRoot root2
            return (False, False)
    prepareRemotes profile [root1, root2]
    handshakes <- handshakeInfo [root1, root2]
    repos <- case srcKind of
        RootsGiven    -> persistRepo profile root1 root2
        RootsFromRepo -> return (Nothing, Nothing)
    showPrepare root1 root2 (created1, created2) handshakes repos

    -- 阶段 2：无条件先同步当前 profile 的 ignore 文件（较新一端覆盖较旧一端）。
    -- 两端 ignore 必须先一致，之后各自扫描文件集合才不会出现偏差。
    -- 此阶段之后 ignore 文件不再参与文件同步。
    -- 输出：只在**真的复制了**才多一行；两端都没有 / 两边本来就一样 → 除了标题不说别的。
    putStrLn "\n[2/6] Syncing ignore rules ..."
    syncIgnoreFiles profile mHost1 path1 mHost2 path2

    -- 阶段 3：同步软链接（先让两端的链接结构完全一致）。
    -- 输出：只有真有动作（要用户确认）时才列清单 + 提问；结构调整一致时静默。
    --
    -- 返回 False = 链接结构**没**谈成（还有未消解的冲突、有动作没落地，或用户在确认处拒绝）
    -- → **不进文件阶段**。理由：文件阶段的比较键只有"相对路径 + (size,mtime)"，
    -- 它默认两端同一路径指同一个对象；链接没对齐时这个前提不成立 —— 跟随链接扫出来
    -- 的两份内容会被当成"同一个逻辑路径的差异"互相纠正，而落地是穿透末端链接写到
    -- 目标上（可能是同步根之外、且是用户还没决定的那一边），等于替用户做了决定。
    linksAgreed <- syncLinkPhase profile mTag mHost1 path1 mHost2 path2
    if linksAgreed
        then do
            syncFilePhases profile mTag mHost1 path1 mHost2 path2
            putStrLn "\nSync complete."
        else
            putStrLn "\nSymlinks not in sync; stopping before the file phase."

-- | 第 4~6 步：读状态 → 计算动作 → 执行。
--
--   **前提：两端链接结构已一致**（由 'syncLinkPhase' 把关）。链接没对齐时这里的
--   一切比较都失去依据 —— 见 'runSync' 阶段 3 的注释。
syncFilePhases
    :: String -> Maybe String -> Maybe String -> FilePath -> Maybe String -> FilePath -> IO ()
syncFilePhases profile mTag mHost1 path1 mHost2 path2 = do
    -- 阶段 4：并行读取两端状态（各自按已统一的 ignore 过滤）
    --
    --   悬空链接不做特殊处理：扫描跟随链接时那里自然"什么都没有"，
    --   diff 该得到 Deleted 就得到 Deleted（删除动作本身有确认关卡）。
    --   输出（用户 2026-09-22）：不统计条数 —— 想看变化看第 5 步的初步统计，
    --   这一步只留阶段标题。
    putStrLn "\n[4/6] Reading states ..."
    ((_label1, (last1, cur1), rmdir1), (_label2, (last2, cur2), rmdir2)) <-
        concurrently (readSideK SKFile mTag mHost1 path1) (readSideK SKFile mTag mHost2 path2)

    -- 第 4 步的收尾：扫描本来就要进入/查看/离开每一个目录，空判定是顺带得出的，
    -- 所以"清理空目录"就发生在这里（只用 rmdir，绝不递归删）。
    -- 两端各清各的：空不空只有本端盘上的真相说了算。
    showRmdirResults "root1" rmdir1
    showRmdirResults "root2" rmdir2

    -- 名字索引：名字 → 当前状态。后面（内容校验）只做 O(log n) 查询，
    -- 不再逐条线性重找（那是 O(n²)）。惰性：不会走到的路径就不会建索引。
    let idx1 = indexStates cur1
        idx2 = indexStates cur2

    -- 阶段 5：计算最终动作（diff + 形式冲突消解 + 额外内容校验）。
    -- 只处理两端扫描出的普通文件；ignore 文件已在阶段 2、软链接已在阶段 3 处理。
    putStrLn "\n[5/6] Computing sync actions ..."
    -- 计算 + 消解形式冲突（同 size 同整数秒 mtime 的一对直接判作已同步）
    let (stats1, pairs1, stats2, pairs2) = sync (last1, last2) (cur1, cur2)
        (_, pairs1', pairs2') = resolveConflicts pairs1 pairs2

    -- 初步统计：两端各自相对自己基线的状态变化（冲突消解 / 内容校验之前），
    -- 一端一行、四种都列（含 0；'None' = 该侧从未有过，不计）。
    showStats "root1" stats1
    showStats "root2" stats2

    -- 初步结果：两端动作合并、按种类计数（同样是**内容校验之前**的口径）。
    -- **恒定打印**（用户 2026-09-22）：就算五种全是 0，这一行也要在。
    showActionResult pairs1' pairs2'

    let hasWork = any (isJust . snd) pairs1' || any (isJust . snd) pairs2'
    if not hasWork
        then putStrLn "\nNothing to do."
        else withHelpers mHost1 path1 mHost2 path2 profile $ \side1 side2 -> do
                -- 额外内容校验：冲突的、或要传输的，只要两端 size 相同就静默核对内容；
                -- 一致的“只更新属性并记录”，不一致的维持原判（见 'verifySameSizeCandidates'）。
                (pairs1'', pairs2'') <- verifySameSizeCandidates idx1 idx2 side1 side2 pairs1' pairs2'

                -- 阶段 6：列出最终动作、确认、执行（真正的传输与记录）
                putStrLn "\n[6/6] Sync and record files ..."
                showSummary pairs1'' pairs2''

                -- 只剩冲突时没有任何要执行的东西（'execute' 对冲突本就是空操作）
                -- → 不用确认，直接跑到底退出（用户 2026-09-23 定）。
                yes <- if hasApplicableAction pairs1'' pairs2''
                           then askYesNo "\nProceed? [y/N] "
                           else return True
                if not yes
                    then putStrLn "Aborted."
                    else do
                        -- 文件阶段不设闸门（它就是最后一步）：执行失败会在上面自己报出来。
                        _ <- execute SKFile mTag side1 side2 pairs1''
                        _ <- execute SKFile mTag side2 side1 pairs2''
                        return ()

-- | 第 1 步（sync prepare）的输出：**按 root1 / root2 side 分组**，
--   每组只列真的发生了的动作：
--
--     [1/6] Sync prepare ...
--       at root1 side:
--         create vincent@192.168.1.140:Sync  [create OK]
--         helper = /usr/local/bin/hsbisync
--         home   = /home/vincent
--         repo = vincent@192.168.1.160:Sync  [write OK]
--       at root2 side:
--         repo = vincent@192.168.1.140:Sync  [write OK]
--
--   三件事决定每一行（用户 2026-09-23 定）：
--   * **哪一端** —— 分组标题 @at root1 side:@ / @at root2 side:@，不再拿地址当行首
--     标签：远端根常写成 @host:目录名@，再拼上格式里的冒号就成了 @host:目录: repo = …@
--     那样的双冒号，读不出来；
--   * **是不是 ssh** —— 只有远程端才报 @helper@ / @home@：那是握手唯一有意义的产物
--     （远端在跑哪份二进制、家目录在哪），本地端没有远端信息可报；
--   * **有没有动作** —— 同步目录**真建了**才 @create <路径>  [create OK]@、
--     repo **真写了**才 @repo = <地址>  [write OK]@；
--     没建、没写、本地端又无动作 → 连 @at rootN side:@ 标题都不出现。
showPrepare
    :: Root -> Root
    -> (Bool, Bool)                              -- ^ 两端是否各建了同步目录
    -> [(String, Maybe String, Maybe FilePath)]  -- ^ 握手结果（只含远程端）
    -> (Maybe String, Maybe String)              -- ^ 两端各写进 repo 的地址（Nothing = 没写）
    -> IO ()
showPrepare root1 root2 (created1, created2) handshakes (repo1, repo2) =
    mapM_ showSide [ ("root1", root1, created1, repo1)
                   , ("root2", root2, created2, repo2) ]
  where
    showSide (which, root, created, mRepo) = do
        let actions = concat
                [ [ "create " ++ rootLabel root ++ "  [create OK]" | created ]
                , handshakeLines root
                , [ "repo = " ++ a ++ "  [write OK]" | Just a <- [mRepo] ]
                ]
        unless (null actions) $ do
            putStrLn $ "  at " ++ which ++ " side:"
            mapM_ (putStrLn . ("    " ++)) actions

    -- 远程端报 helper / home（本地端没有远端可握手 → 一行都没有）。
    -- 握手结果最多两条（一个 host 一条），直接找。
    handshakeLines root = case root of
        Local _    -> []
        Remote h _ -> case [ (mHome, mExe) | (host, mHome, mExe) <- handshakes, host == h ] of
            ((mHome, mExe) : _) ->
                [ "helper = " ++ fromMaybe "(not reported)" mExe
                , "home   = " ++ fromMaybe "?" mHome
                ]
            [] -> []

-- ============================================================================
-- 校验钩子：文件比较阶段，冲突 / 要传输 + 两端 size 相同 → 立刻核对内容
-- ============================================================================

-- | FileChange 的文件名（不管哪个构造子）
changeName :: FileChange -> FilePath
changeName fc = case fc of
    Noop fs _ _     -> filename fs
    Added fs _ _    -> filename fs
    Del fs _ _      -> filename fs
    Conflict fs _ _ -> filename fs

-- | 文件比较阶段的钩子：把"有冲突的"和"要传输的"文件里**两端 size 相同**的拿出来，
--   安静地逐个校对两端内容（SHA512），然后把**确实一致**的按来时路分类列出：
--
--     Extra content check ...
--     Not real conflict:      <- 本来报冲突，实际内容一致（属性已静默对齐、只记 added）
--     - x.txt
--     No need transfer:       <- 本来要传输，实际内容一致（同上）
--     - y.txt
--
--   * **一致**（列表里的那些）——不传输：把**较旧那端**的 mtime 对齐到较新那端
--     （本地 'setMtimeOnly' / 远端 SETTIME），然后把两端都记成 added（'RecordAdd'），
--     它们会出现在下文的"已同步（纯纪录）"一段里。
--     对齐 + 记录之后，两端真实文件的 (size, mtime) 与各自的基线都一致，
--     下一次运行两边都是 Unchanged，这件事彻底消失。
--   * **不一致**（读不到也一样）——原样保留、**完全不提**：该传的照旧传、
--     该报的冲突照旧报，只在下文对应段落里出现。
--
--   为什么要做：mtime 不同但内容相同的文件（git checkout、cp 未保留时间戳、换机器
--   重建…）在纯 (size,mtime) 判定下会被当成"要传"甚至"冲突"，白跑一趟、白问一次。
--   只比 size 相同的对；size 不同必然不同内容，不浪费时间读字节。
--
--   只在**文件阶段**做；链接阶段的"内容"是指向字符串，另有判定。
--   调用点在展示摘要与确认之前，所以用户看到的是已经校完的最终结果。
verifySameSizeCandidates
    :: Map.Map FilePath FileState             -- ^ root1 当前状态索引（'indexStates'）
    -> Map.Map FilePath FileState             -- ^ root2 当前状态索引
    -> Side -> Side
    -> [(FileChange, Maybe SyncAction)]          -- ^ pairs1
    -> [(FileChange, Maybe SyncAction)]          -- ^ pairs2
    -> IO ([(FileChange, Maybe SyncAction)], [(FileChange, Maybe SyncAction)])
verifySameSizeCandidates idx1 idx2 side1 side2 pairs1 pairs2 = do
    let names = dedup $ [ filename fs | (Added fs _ _, Just Receive)      <- pairs1 ++ pairs2 ]
                    ++ [ filename fs | (Conflict fs _ _, Just Unsolvable) <- pairs1 ++ pairs2 ]
        conflictNames = Set.fromList [ filename fs | (Conflict fs _ _, _) <- pairs1 ++ pairs2 ]
        sameSize n = case (Map.lookup n idx1, Map.lookup n idx2) of
            (Just a, Just b) -> size a == size b
            _                -> False
        sized = [ (n, if Set.member n conflictNames then CandConflict else CandTransfer)
                | n <- names, sameSize n ]
    if null sized
        then return (pairs1, pairs2)
        else do
            -- 额外内容校验：两端 size 相同的逐个安静校对（一致的同时把属性对齐），
            -- 再把**确实一致**的按来时路分类列出——它们既不是真冲突、也不需要传输。
            putStrLn "\n  Extra content check ..."
            hFlush stdout
            confirmed <- filterM (\(n, _) -> identicalAndAlign idx1 idx2 side1 side2 n) sized
            let same = map fst confirmed
            showGroupFor same confirmed
            if null same
                then return (pairs1, pairs2)
                else do
                    -- 冲突在两侧各有一条；转换后只让其中一侧发起记录即可
                    -- （'execute' 的 RecordAdd 同时记两端，两边都转会在 added 里留重复行）
                    let sameSet     = Set.fromList same
                        conflicts1S = Set.fromList [ filename fs | (Conflict fs _ _, _) <- pairs1 ]
                        pairs1'' = map (markRecordAdd sameSet) pairs1
                        pairs2'' = [ p | p@(fc, _) <- map (markRecordAdd sameSet) pairs2
                                       , not (Set.member (changeName fc) sameSet
                                              && Set.member (changeName fc) conflicts1S) ]
                    return (pairs1'', pairs2'')

-- | 把校验结果按“本来是冲突”/“本来要传”分两组打印（一致的那些）
showGroupFor :: [FilePath] -> [(FilePath, CandKind)] -> IO ()
showGroupFor same confirmed = do
    let showGroup lbl ns = unless (null ns) $ do
            putStrLn $ "  " ++ lbl ++ ":"
            forM_ ns $ \n -> putStrLn $ "  - " ++ n
    unless (null same) $ do
        showGroup "Not real conflict" [ n | (n, CandConflict) <- confirmed ]
        showGroup "No need transfer" [ n | (n, CandTransfer) <- confirmed ]

-- | 校验钩子里候选的来源：本来就报冲突的 / 本来要传输的
--   （用来把"校验后一致"的文件分成两组显示：will not conflict / will not transfer）
data CandKind = CandConflict | CandTransfer
    deriving (Eq, Show)

-- | 核对一对文件：内容相同 → 把**较旧那端** mtime 对齐到较新那端并返回 True；
--   内容不同 / 读不到 → 维持原样返回 False。两种都不打印（结果只在摘要里体现）。
identicalAndAlign :: Map.Map FilePath FileState -> Map.Map FilePath FileState
                  -> Side -> Side -> FilePath -> IO Bool
identicalAndAlign idx1 idx2 side1 side2 name =
    case (Map.lookup name idx1, Map.lookup name idx2) of
        (Just fs1, Just fs2) -> do
            mh1 <- hashSideContent side1 name
            mh2 <- hashSideContent side2 name
            case (mh1, mh2) of
                (Just h1, Just h2)
                    | h1 == h2 -> do
                        let t1    = posixSecs (mtime fs1)
                            t2    = posixSecs (mtime fs2)
                            newer = max t1 t2
                        when (t1 /= t2) $
                            setMtimeOnlySide (if t1 < t2 then side1 else side2) name newer
                        return True
                _ -> return False
        _ -> return False

-- | 把"实质相同"的一对改成"已同步、只需记录"（'execute' 会同时记两端）
markRecordAdd :: Set.Set FilePath -> (FileChange, Maybe SyncAction) -> (FileChange, Maybe SyncAction)
markRecordAdd names (Conflict fs d1 d2, _)
    | Set.member (filename fs) names = (Noop fs d1 d2, Just RecordAdd)
markRecordAdd names (Added fs d1 d2, _)
    | Set.member (filename fs) names = (Noop fs d1 d2, Just RecordAdd)
markRecordAdd _ other = other

-- | 取某一端文件内容的 SHA512（本地流式哈希，远端用 hash 命令）；读不到返回 Nothing
--
--   本地旧实现是 @BS.readFile@ 后再算（大文件整块进内存），现在走 'hashFile' 分块。
hashSideContent :: Side -> FilePath -> IO (Maybe String)
hashSideContent (Lcl root) relPath =
    fmap Just (hashFile (root </> relPath)) `catch` ioFail
  where
    ioFail :: IOException -> IO (Maybe String)
    ioFail _ = return Nothing

hashSideContent (Rmt hh) relPath = do
    res <- call hh (Msg.cmd "hash" [ ("kind", kindName FileKind)
                                   , ("path", Msg.str relPath) ])
    return $ case res of
        Right v -> Msg.fieldStr "hash" v
        Left _  -> Nothing

-- | 把某一端文件的 mtime 对齐到给定秒数（失败只告警：不影响后续传输/记录）
setMtimeOnlySide :: Side -> FilePath -> Integer -> IO ()
setMtimeOnlySide (Lcl root) relPath t = setMtimeOnly (root </> relPath) t
setMtimeOnlySide (Rmt hh) relPath t = do
    res <- call hh (Msg.cmd "settime" [ ("path",  Msg.str relPath)
                                      , ("mtime", Msg.int t) ])
    case res of
        Left err -> putStrLn $ "  WARNING: settime failed for " ++ relPath ++ ": " ++ err
        Right _  -> return ()

-- | 读一个按键作为确认（不回显），并把**已经可读**的该行剩余输入丢掉。
--
--   为什么要清行：同步流程里会问不止一次（链接阶段、文件阶段），而 getChar 只取走
--   一个字节，剩下的回车会被下一个提示当成"取消"。
--
--   但清行绝对不能阻塞：终端可能把按键即时送达（没有跟回车），此时阻塞等回车
--   会让人以为"要按两次 y"。所以只看 hReady —— 有就丢，没有就立刻返回。
--   EOF 视为 No（管道输入不足时不会抛异常挂掉）。
askYesNo :: String -> IO Bool
askYesNo prompt = do
    putStr prompt
    hFlush stdout
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    answer <- getChar `catch` eofAsNo
    hSetEcho stdin True
    putStrLn [answer]
    drainAvailable
    return (toLower answer == 'y')
  where
    eofAsNo :: IOException -> IO Char
    eofAsNo _ = return 'n'

    drainAvailable = do
        -- hReady 在 handle 已 EOF 时是抛异常而不是返回 False；EOF 就当"没有可丢的"。
        ready <- hReady stdin `catch` eofAsFalse
        if not ready
            then return ()
            else do
                c <- getChar `catch` eofAsNewline
                if c == '\n' then return () else drainAvailable

    eofAsFalse :: IOException -> IO Bool
    eofAsFalse _ = return False

    eofAsNewline :: IOException -> IO Char
    eofAsNewline _ = return '\n'

-- ============================================================================
-- 链接同步阶段
-- ============================================================================

-- | 同步两端软链接的结构。
--
--   链接被当作"内容 = 指向字符串"的文件：
--   * size = st_size = 指向字符串长度，mtime = 链接自身 mtime
--     → 判定与消解直接复用文件阶段那一整套（'sameFile' / 'classifyOne' / 'resolveConflicts'）；
--   * 指向字符串作为"内容"走同一套 SEND/RECV/哈希校验/先校验后落地；
--   * 自己一套 state 文件（@link_state@ / @link_added@ / @link_deleted@，行格式相同）。
--
--   先做这一步的目的是：之后文件阶段只按"路径"读写（对链接完全透明）。
--
--   输出（用户 2026-09-22 定）：**只说动作** —— 没有任何动作时除了阶段标题一个字不输出；
--   有动作才由 'showSummary' 列出并问一次确认（不需要像文件阶段那样统计前后条数）。
--
--   返回值 = **链接结构是否已达成一致**（决定 'runSync' 要不要继续文件阶段）：
--   * 本来就一致（没有任何动作）→ 'True'
--   * 只剩冲突（没有能执行的动作）→ 'False'（不提问、不起 helper，列完清单就结束）
--   * 用户拒绝执行 → 'False'
--   * 有动作没落地（删除失败 / 链接没建上）→ 'False'
--   * 执行完仍有未消解冲突 → 'False'
--
--   即"真正一致" = 该做的都做成了 **且** 一条冲突都不剩。
syncLinkPhase
    :: String -> Maybe String
    -> Maybe String -> FilePath -> Maybe String -> FilePath -> IO Bool
syncLinkPhase profile mTag mHost1 path1 mHost2 path2 = do
    putStrLn "\n[3/6] Syncing symlinks ..."
    ((_label1, (last1, cur1), _), (_label2, (last2, cur2), _)) <-
        concurrently (readSideK SKLink mTag mHost1 path1) (readSideK SKLink mTag mHost2 path2)

    let (_, pairs1, _, pairs2) = sync (last1, last2) (cur1, cur2)
        (_, pairs1', pairs2') = resolveConflicts pairs1 pairs2
        hasWork = any (isJust . snd) pairs1' || any (isJust . snd) pairs2'

    -- 输出约定（用户 2026-09-22）：**只有真的有动作（要确认）时才列出来并提问**。
    -- 不统计两端软链接的前后条数（那是文件阶段的口径，链接阶段不需要），
    -- 链接结构本来就一致时这里一个字都不说。
    if not hasWork
        then return True
        else do
            showSummary pairs1' pairs2'
            -- 只剩冲突（要跳过的那些）时没有要执行的动作：不必确认，也不用起 helper，
            -- 列完清单直接结束（用户 2026-09-23 定，与文件阶段口径一致）——但结构没谈成，
            -- 返回 False，文件阶段不做。
            if not (hasApplicableAction pairs1' pairs2')
                then return False
                else do
                    yes <- askYesNo "\nApply symlink changes? [y/N] "
                    if not yes
                        then do
                            putStrLn "Aborted (symlink phase)."
                            return False
                        else do
                            applied <- withHelpers mHost1 path1 mHost2 path2 profile $ \side1 side2 -> do
                                ok1 <- execute SKLink mTag side1 side2 pairs1'
                                discardLocal side1 mTag pairs1'
                                ok2 <- execute SKLink mTag side2 side1 pairs2'
                                discardLocal side2 mTag pairs2'
                                return (ok1 && ok2)
                            unless applied $
                                putStrLn "  WARNING: symlink changes not fully applied."
                            -- 真正一致 = 该做的都落地了 **且** 没留下未消解冲突
                            return (applied && not (hasUnresolved pairs1' pairs2'))

-- | 链接阶段刚在某端建/改了一个链接：把该端这个链接**子树**下的文件基线条目作废。
--
--   链接此前在该端不可达（目标不存在、或那里原本是真实目录），扫描看不到链接下的
--   内容，所以那些条目已过期——作废后文件阶段对它们是 None（"从未有过"）而不是
--   Deleted，于是走接收而不是让对端删数据。文件落地时会顺带把链接目标目录建出来，
--   然后再以 added 记回正确状态。
--
--   远端那一端由 helper 在 LINKRECV 落地后就地完成（同一函数）。
discardLocal :: Side -> Maybe String -> [(FileChange, Maybe SyncAction)] -> IO ()
discardLocal (Lcl root) mTag pairs =
    forM_ [ filename fs | (Added fs _ _, Just Receive) <- pairs ] $
        \relPath -> discardStateUnder root mTag relPath
discardLocal (Rmt _) _ _ = return ()

-- | 按需为两端派生 helper，在其生命周期内执行动作（文件/链接阶段共用）
withHelpers
    :: Maybe String -> FilePath -> Maybe String -> FilePath -> String
    -> (Side -> Side -> IO a) -> IO a
withHelpers mHost1 path1 mHost2 path2 profile k =
    bracket (maybe (return Nothing) (\h -> Just <$> spawnHelper h path1 profile) mHost1)
            (maybe (return ()) (\hh -> destroyHelper hh))
            $ \mHh1 ->
      bracket (maybe (return Nothing) (\h -> Just <$> spawnHelper h path2 profile) mHost2)
              (maybe (return ()) (\hh -> destroyHelper hh))
              $ \mHh2 -> do
                  let side1 = case mHh1 of Just hh -> Rmt hh; Nothing -> Lcl path1
                      side2 = case mHh2 of Just hh -> Rmt hh; Nothing -> Lcl path2
                  k side1 side2

-- ============================================================================
-- Ignore 规则预同步（同步开始前对齐两端 .bisync/<profile>/ignore）
-- ============================================================================

-- | 若该端为远程，则为其派生一个 helper 执行动作；本地端直接执行。
withSide :: Maybe String -> FilePath -> String -> (Side -> IO a) -> IO a
withSide Nothing path _ k = k (Lcl path)
withSide (Just host) path profile k =
    bracket (spawnHelper host path profile) destroyHelper (k . Rmt)

-- | profile 的 ignore 文件相对路径（相对同步根目录）
ignoreRel :: String -> FilePath
ignoreRel profile = ".bisync" </> profile </> "ignore"

-- | 读取本地 ignore 文件元信息 @(mtime, mode, sha512)@，不存在返回 Nothing
localIgnoreMeta :: FilePath -> String -> IO (Maybe (Integer, Int, String))
localIgnoreMeta root profile = do
    let rel  = ignoreRel profile
        full = root </> rel
    e <- doesFileExist full
    if not e
        then return Nothing
        else do
            fs <- statRel root rel
            content <- BS.readFile full
            return $ Just (posixSecs (mtime fs), mode fs, hashBytes content)

-- | 读取任一端 ignore 文件元信息
sideIgnoreMeta :: Side -> String -> IO (Maybe (Integer, Int, String))
sideIgnoreMeta (Lcl root) profile = localIgnoreMeta root profile
sideIgnoreMeta (Rmt hh)   _        = getIgnoreRemote hh

-- | 对齐两端 ignore 文件（在扫描状态之前调用）
syncIgnoreFiles
    :: String            -- ^ profile
    -> Maybe String      -- ^ root1 host（Nothing=本地）
    -> FilePath         -- ^ root1 路径
    -> Maybe String      -- ^ root2 host
    -> FilePath         -- ^ root2 路径
    -> IO ()
syncIgnoreFiles profile mHost1 path1 mHost2 path2 =
    withSide mHost1 path1 profile $ \side1 ->
      withSide mHost2 path2 profile $ \side2 ->
        syncIgnore profile side1 side2

-- | 两端 ignore 的合并决策：
--   * 都无 → 无事（**静默**）
--   * 仅一端有 → 复制到另一端
--   * 都有且内容相同 → 跳过（**静默**）
--   * 都有但内容不同 → 较新（mtime 大）的一端覆盖另一端；
--     mtime 相同则告警并采用 root1
--
--   输出约定（用户 2026-09-22）：**只有真的做了动作才打印**（即复制那一行）。
--   "两边都没有"和"两边一样"都不是动作，一个字都不说。
syncIgnore :: String -> Side -> Side -> IO ()
syncIgnore profile sideA sideB = do
    mA <- sideIgnoreMeta sideA profile
    mB <- sideIgnoreMeta sideB profile
    case (mA, mB) of
        (Nothing, Nothing) ->
            return ()
        (Just a, Nothing) ->
            copyIgnore profile sideA sideB "root2" a
        (Nothing, Just b) ->
            copyIgnore profile sideB sideA "root1" b
        (Just a, Just b)
            | hashOf a == hashOf b ->
                return ()
            | otherwise ->
                case compare (mtOf a) (mtOf b) of
                    GT -> copyIgnore profile sideA sideB "root2" a
                    LT -> copyIgnore profile sideB sideA "root1" b
                    EQ -> do
                        putStrLn "  WARNING: ignore files differ but mtime is equal; using root1"
                        copyIgnore profile sideA sideB "root2" a
  where
    mtOf   (m, _, _) = m
    hashOf (_, _, h) = h

-- | 把 src 端的 ignore 文件同步到 dst 端（经 transferContent 保留 mtime/mode）。
--   只提示哪个 root 被更新了，不区分"较旧/缺失"、不指明来源。
copyIgnore
    :: String -> Side -> Side -> String
    -> (Integer, Int, String)  -- ^ 源端 (mtime, mode, hash)
    -> IO ()
copyIgnore profile src dst dstName (mt, srcMode, _) = do
    result <- transferContent SKFile src dst (ignoreRel profile) srcMode mt
    case result of
        Left err -> putStrLn $ "  ERROR: failed to sync ignore file: " ++ err
        Right () -> putStrLn $ "  " ++ dstName ++ " ignore updated"

-- ============================================================================
-- 执行端类型
-- ============================================================================

data Side = Lcl FilePath | Rmt HelperHandle

-- ============================================================================
-- 状态读取
-- ============================================================================

-- | 同步阶段种类：文件阶段 / 链接阶段
--
--   两个阶段只差三件"内容来源与落地方式"：取内容（读文件 / 读指向字符串）、
--   落地（写文件 / 建链接）、状态文件（state / link_state）。
--   diff/消解/传输/校验/摘要全部共用同一套代码。
data SyncKind = SKFile | SKLink
    deriving (Eq, Show)

kindState :: SyncKind -> StateKind
kindState SKFile = FileKind
kindState SKLink = LinkKind

kindVerb :: SyncKind -> String
kindVerb SKFile = "copy"
kindVerb SKLink = "link"

kindDeleteVerb :: SyncKind -> String
kindDeleteVerb SKFile = "rm"
kindDeleteVerb SKLink = "unlink"

-- | 协议里的 "kind" 字段：文件与链接共用同一套命令（helper 侧只差"内容是什么/怎么落地"）
kindName :: StateKind -> Msg.Value
kindName FileKind = Msg.str "file"
kindName LinkKind = Msg.str "link"

-- | 读取一端的状态：(label, (上次状态, 当前状态), 本次扫描发起过的每一次 rmdir)
--
--   悬空链接没有特殊处理：扫描只是扫不到那里的东西。
--   第三项只对**文件**扫描有意义：链接扫描不清理（见 'State.currentStateWith'）。
readSideK
    :: SyncKind -> Maybe String -> Maybe String -> FilePath
    -> IO (String, ([FileState], [FileState]), [(FilePath, Bool)])
readSideK kind mTag mHost path =
    case mHost of
        Nothing -> do
            l <- lastStateKind (kindState kind) path mTag
            (c, rmdirs) <- case kind of
                SKFile -> currentStateWith True path mTag
                SKLink -> (\s -> (s, [])) <$> currentLinkState path mTag
            return ("local", (l, c), rmdirs)
        Just host -> do
            hh <- spawnHelper host path (fromMaybe "" mTag)
            (l, _)       <- getHelperStates hh "last"  (kindState kind)
            (c, rmdirs)  <- getHelperStates hh "state" (kindState kind)
            destroyHelper hh
            return ("ssh " ++ host, (l, c), rmdirs)

-- | 读对端状态：which = "state"（本次扫描）或 "last"（上次基线）
--
--   "state" 的响应里还可能带 @rmdir@：本次**发起过的每一次 rmdir**，形如
--   @[{"相对路径": 是否成功}, …]@（**数组**保序，深 → 浅）；
--   "last" 只读基线，不带这个字段 → 空列表。
getHelperStates :: HelperHandle -> String -> StateKind -> IO ([FileState], [(FilePath, Bool)])
getHelperStates hh which kind = do
    r <- call hh (Msg.cmd which [("kind", kindName kind)])
    case r of
        Left err -> error $ which ++ " failed: " ++ err
        Right v  -> case Msg.field "entries" v of
            Just e  -> return (parseStateValue e, Msg.fieldBools "rmdir" v)
            Nothing -> error $ which ++ ": response has no \"entries\""

-- ============================================================================
-- Helper 管理
--
--   派生/销毁/握手都在 Connect.hs（"怎么在对面跑起 hsbisync"单独一层）。
-- ============================================================================

-- ============================================================================
-- 执行操作
-- ============================================================================

-- | 记录类型：ADD / DELETE
--
data Rec = RecAdd | RecDelete

-- | 这批动作里有没有**真正要执行**的东西。
--
--   'Unsolvable'（冲突）虽然也记在 @Just@ 里，但它是"发现了冲突、跳过"的标记，
--   'execute' 对它什么都不做 —— 所以**只有冲突**时不需要用户确认，直接跑完退出
--   （用户 2026-09-23 定）。与 'Nothing'（无动作）一起都算"没有可执行的"。
hasApplicableAction
    :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)] -> Bool
hasApplicableAction pairs1 pairs2 = any applicable (pairs1 ++ pairs2)
  where
    applicable (_, Just Unsolvable) = False
    applicable (_, Just _)          = True
    applicable (_, Nothing)         = False

-- | 这批动作里**还有没有未消解的冲突**（@Conflict … Just Unsolvable@）。
--
--   链接阶段用它判断"结构到底有没有谈成"：冲突在 pairs1 / pairs2 里各有一条，
--   只要还剩一条，两端链接结构就仍有分歧 → 'syncLinkPhase' 返回 'False'，
--   文件阶段整体不做（见 'runSync' 阶段 3）。
hasUnresolved
    :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)] -> Bool
hasUnresolved pairs1 pairs2 = any unresolved (pairs1 ++ pairs2)
  where
    unresolved (Conflict _ _ _, Just Unsolvable) = True
    unresolved _                                 = False

-- | 执行一侧的所有操作（文件/链接阶段共用，动作按 kind 区分）。
--
--   顺序固定为：**先"纯纪录"的动作**（只写状态、不动文件内容），**再删除，最后传输**
--   —— 纯纪录是双方内容已经一致、只补状态的事，先落地它，确认环节里出现过的东西
--   就不会因为后面的动作而变卦。
--
--   返回值 = **每个动作都真的落地了**（删除真删了、链接真建上了）。
--   链接阶段用它当闸门的一部分：只要有一个动作没落地，两端链接结构就仍不一致
--   → 文件阶段不做（见 'syncLinkPhase' / 'runSync' 阶段 3）。
--   没落地的动作**不写记录**：记录等于断言"这件事发生了"，写了会让基线撒谎、
--   把分歧在下一轮隐掉；留原样才能下一轮重新发现它。
execute :: SyncKind -> Maybe String -> Side -> Side -> [(FileChange, Maybe SyncAction)] -> IO Bool
execute kind mTag self other pairs = do
    let receives   = [ (filename fs, mode fs, posixSecs (mtime fs))
                     | (Added     fs _ _, Just Receive)       <- pairs ]
        removes    = [ (filename fs, d1, d2)
                     | (Del   fs d1 d2, Just Remove)      <- pairs ]
        recordAdds = [ filename fs
                     | (Noop fs _ _, Just RecordAdd)     <- pairs ]
        recordDels = [ filename fs
                     | (Del   fs _ _, Just RecordDelete)  <- pairs ]
    unless (null recordAdds) $ do
        putStrLn "\nRecording as added..."
        forM_ recordAdds $ \relPath -> do
            putStrLn $ "  add " ++ relPath
            appendEnd kind self  mTag RecAdd relPath
            appendEnd kind other mTag RecAdd relPath
    unless (null recordDels) $ do
        putStrLn "\nRecording as deleted..."
        forM_ recordDels $ \relPath -> do
            putStrLn $ "  del " ++ relPath
            appendEnd kind self  mTag RecDelete relPath
            appendEnd kind other mTag RecDelete relPath
    okRemoves <- if null removes
        then return True
        else do
            putStrLn "\nDeleting then record..."
            and <$> forM removes (\(relPath, _, _) -> do
                putStrLn $ "  " ++ kindDeleteVerb kind ++ " " ++ relPath
                ok <- delEnd kind self relPath
                if ok
                    then do
                        appendEnd kind self  mTag RecDelete relPath
                        appendEnd kind other mTag RecDelete relPath
                    -- 没删成 → **不写记录**：留原样，下一轮才会重新发现这个分歧
                    else return ()
                return ok)
    okReceives <- if null receives
        then return True
        else do
            putStrLn "\nReceiving then record..."
            and <$> forM receives (\(relPath, srcMode, srcMtime) -> do
                putStrLn $ "  " ++ kindVerb kind ++ " " ++ relPath
                copyEndToEnd kind other self mTag relPath srcMode srcMtime)
    return (okRemoves && okReceives)

-- ============================================================================
-- End 操作
-- ============================================================================

-- | 删除一侧的条目。返回值 = **真的删了**（对文件来说：删动作没抛异常就算成功）。
--
--   链接阶段：只删链接本身（unlink），不跟随到目标；占位不是链接则**不算成功**
--   （扫描→执行之间本地被改成了真实文件/目录）——不能当成"已对齐"放行。
delEnd :: SyncKind -> Side -> FilePath -> IO Bool
delEnd SKFile (Lcl root) relPath = do
    removeFileSafe (root </> relPath)
    return True
delEnd SKFile (Rmt hh)  relPath = do
    res <- call hh (Msg.cmd "unlink" [("kind", kindName FileKind), ("path", Msg.str relPath)])
    case res of
        Left err -> error $ "FATAL: remote unlink failed: " ++ err
        Right _  -> return True
delEnd SKLink (Lcl root) relPath = do
    ok <- removeLinkSafe (root </> relPath)
    unless ok $ putStrLn $ "  WARNING: not a symlink, left untouched: " ++ relPath
    return ok
delEnd SKLink (Rmt hh) relPath = do
    res <- call hh (Msg.cmd "unlink" [("kind", kindName LinkKind), ("path", Msg.str relPath)])
    case res of
        Left err -> error $ "FATAL: remote unlink (link) failed: " ++ err
        Right v  -> case Msg.asBool =<< Msg.field "notLink" v of
            Just True -> do
                putStrLn $ "  WARNING: not a symlink, left untouched: " ++ relPath
                return False
            _         -> return True

appendEnd :: SyncKind -> Side -> Maybe String -> Rec -> FilePath -> IO ()
appendEnd kind (Lcl root) mTag rec relPath =
    case rec of
        RecDelete -> appendDeletedKind (kindState kind) root mTag relPath
        RecAdd    -> appendAddedKind   (kindState kind) root mTag relPath
appendEnd kind (Rmt hh) _mTag rec relPath = do
    let cmd = case rec of RecAdd -> "add"; RecDelete -> "delete"
    res <- call hh (Msg.cmd cmd [ ("kind", kindName (kindState kind))
                                , ("path", Msg.str relPath) ])
    case res of
        Left err -> error $ "FATAL: remote " ++ cmd ++ " failed: " ++ err
        Right _  -> return ()

-- | 跨端复制文件：传输 + 校验（rename 之前） + 记录。
--   校验在目标端 rename 之前完成——不通过则目标文件完全不受影响。
--   返回值 = 是否真的落地（没落地就不写记录，见 'execute'）。
copyEndToEnd :: SyncKind -> Side -> Side -> Maybe String -> FilePath -> Int -> Integer -> IO Bool
copyEndToEnd kind src dst mTag relPath srcMode srcMtime = do
    result <- transferContent kind src dst relPath srcMode srcMtime
    case result of
        Left err -> do
            putStrLn $ "  " ++ err
            return False
        Right () -> do
            putStrLn "    verified OK"
            appendEnd kind dst mTag RecAdd relPath
            appendEnd kind src mTag RecAdd relPath
            return True

-- | 执行内容传输。**文件一律流式**（一块 64 KiB，内存 O(1)，与是不是 ssh 无关）：
--
--     * 本地 → 本地：读源文件、直接写目标临时文件（不进内存），rename 提交；
--     * 本地 → 远端：发 recv（size/mode/mtime）→ 流式灌载荷 → 补哈希尾行 → 读结果；
--     * 远端 → 本地：send 拿 size → 流式收进目标临时文件（边收边算哈希）→
--       读发送端的哈希尾行比对 → 通过才 rename；
--     * 远端 → 远端：本端只搬运、不落盘，把发送端算的哈希尾行转给接收端。
--
--   载荷格式（文件与链接相同）：请求 → @{"ok":true,…}@ → size 字节裸数据 →
--   一条 @{"ok":true,"hash":…}@ 尾行 → 结果。哈希由**发送端边读边算**、接收端边收边算，
--   收完才比对 —— 所以不需要先单独读一遍文件拿哈希（旧实现为此多了一次整文件读取）。
transferContent :: SyncKind -> Side -> Side -> FilePath -> Int -> Integer -> IO (Either String ())
transferContent SKFile = transferFile
transferContent SKLink = transferLink

-- ============================================================================
-- 文件：流式
-- ============================================================================

transferFile :: Side -> Side -> FilePath -> Int -> Integer -> IO (Either String ())
transferFile (Lcl srcRoot) (Lcl dstRoot) relPath srcMode srcMtime =
    copyFileStream (srcRoot </> relPath) dstRoot (dstRoot </> relPath) srcMode srcMtime

transferFile (Lcl srcRoot) (Rmt hhDst) relPath srcMode srcMtime = do
    r <- try (openBinaryFile (srcRoot </> relPath) ReadMode)
    case r of
        Left e -> return $ Left ("cannot read source: " ++ show (e :: IOException))
        Right srcH -> do
            sz <- hFileSize srcH
            res <- remoteRecvFrom hhDst relPath sz srcMode srcMtime
                       (copyExactlyHash srcH (Just (hhIn hhDst)) sz)
            hClose srcH
            return res

transferFile (Rmt hhSrc) (Lcl dstRoot) relPath srcMode srcMtime = do
    es <- remoteSend hhSrc SKFile relPath
    case es of
        Left e -> return (Left e)
        Right sz -> do
            prep <- openTargetTemp dstRoot (dstRoot </> relPath)
            case prep of
                Left e -> do
                    -- 源端已经答应了 sz 字节：必须吃掉，否则它的 stdout 被写满会卡住
                    drainRemoteSend hhSrc sz
                    return (Left e)
                Right (target, tmpPath, h) -> do
                    (actual, n) <- copyExactlyHash (hhOut hhSrc) (Just h) sz
                    hFlush h
                    hClose h
                    if n < sz
                        then do
                            removeQuietly tmpPath
                            return $ Left "connection closed before payload completed; target untouched"
                        else do
                            eh <- remoteHashTrailer hhSrc
                            case eh of
                                Left e -> removeQuietly tmpPath >> return (Left e)
                                Right expected ->
                                    commitReceived target tmpPath srcMode srcMtime actual expected

transferFile (Rmt hhSrc) (Rmt hhDst) relPath srcMode srcMtime = do
    es <- remoteSend hhSrc SKFile relPath
    case es of
        Left e -> return (Left e)
        Right sz -> do
            eb <- remoteRecvBegin hhDst SKFile relPath sz srcMode srcMtime
            case eb of
                Left e -> drainRemoteSend hhSrc sz >> return (Left e)
                Right () -> do
                    n <- copyExactly (hhOut hhSrc) (hhIn hhDst) sz
                    eh <- remoteHashTrailer hhSrc
                    finishRemoteRecv hhDst sz n (either (const "") id eh)

-- ============================================================================
-- 链接：内容是"指向字符串"（几十字节），整块传输；载荷格式与文件一致
-- ============================================================================

transferLink :: Side -> Side -> FilePath -> Int -> Integer -> IO (Either String ())
transferLink (Lcl srcRoot) (Lcl dstRoot) relPath srcMode srcMtime = do
    content <- readLinkContent (srcRoot </> relPath)
    writeLinkVerified dstRoot (dstRoot </> relPath) content srcMode srcMtime
        (hashBytes content)

transferLink (Lcl srcRoot) (Rmt hhDst) relPath srcMode srcMtime = do
    content <- readLinkContent (srcRoot </> relPath)
    let hsh = hashBytes content
        sz  = toInteger (BS.length content)
    eb <- remoteRecvBegin hhDst SKLink relPath sz srcMode srcMtime
    case eb of
        Left e -> return (Left e)
        Right () -> do
            BS.hPut (hhIn hhDst) content
            hFlush (hhIn hhDst)
            finishRemoteRecv hhDst sz sz hsh

transferLink (Rmt hhSrc) (Lcl dstRoot) relPath srcMode srcMtime = do
    es <- remoteSend hhSrc SKLink relPath
    case es of
        Left e -> return (Left e)
        Right sz -> do
            content <- hGetExactly (hhOut hhSrc) (fromIntegral sz)
            eh <- remoteHashTrailer hhSrc
            case eh of
                Left e -> return (Left e)
                Right expected ->
                    writeLinkVerified dstRoot (dstRoot </> relPath) content srcMode srcMtime expected

transferLink (Rmt hhSrc) (Rmt hhDst) relPath srcMode srcMtime = do
    es <- remoteSend hhSrc SKLink relPath
    case es of
        Left e -> return (Left e)
        Right sz -> do
            eb <- remoteRecvBegin hhDst SKLink relPath sz srcMode srcMtime
            case eb of
                Left e -> drainRemoteSend hhSrc sz >> return (Left e)
                Right () -> do
                    content <- hGetExactly (hhOut hhSrc) (fromIntegral sz)
                    eh <- remoteHashTrailer hhSrc
                    let hsh = either (const "") id eh
                    BS.hPut (hhIn hhDst) content
                    hFlush (hhIn hhDst)
                    finishRemoteRecv hhDst sz sz hsh

-- ============================================================================
-- 传输协议：recv / send
--
--   请求与响应仍是一行规范 JSON；载荷是紧跟响应之后的裸字节，长度由 "size" 决定。
--   载荷之后**再补一条** @{"ok":true,"hash":…}@ —— 哈希要边读边算，没法在请求里先给。
-- ============================================================================

-- | recv 第一阶段：发请求，等接收端说"准备好"。
--   失败时一个字节都还没发（接收端也是先准备临时文件再回 OK），协议不会错位。
remoteRecvBegin
    :: HelperHandle -> SyncKind -> FilePath -> Integer -> Int -> Integer
    -> IO (Either String ())
remoteRecvBegin hh kind relPath sz mode_ mtime_ = do
    r <- call hh (Msg.cmd "recv" [ ("kind",  kindName (kindState kind))
                                 , ("path",  Msg.str relPath)
                                 , ("size",  Msg.int sz)
                                 , ("mode",  Msg.int (fromIntegral mode_))
                                 , ("mtime", Msg.int mtime_) ])
    return $ case r of
        Left e  -> Left ("recv: " ++ e)
        Right _ -> Right ()

-- | recv 第二阶段：载荷已写完（不足 sz 时这里补零，保持帧对齐）→ 哈希尾行 → 读结果
finishRemoteRecv :: HelperHandle -> Integer -> Integer -> String -> IO (Either String ())
finishRemoteRecv hh sz n hsh = do
    er <- try (do
            when (n < sz) $ padZeros (hhIn hh) (sz - n)
            hFlush (hhIn hh)
            sendMsg hh (Msg.obj [("ok", Msg.JBool True), ("hash", Msg.str hsh)])
            r <- readMsg hh
            return $ either (Left . ("verify: " ++)) (const (Right ())) r)
    return $ either (\e -> Left ("transport: " ++ show (e :: IOException))) id er

-- | 本地内容 → 远端：发 recv 请求，再跑 @payload@（往 hhIn 写正好 sz 字节），然后收尾
remoteRecvFrom
    :: HelperHandle -> FilePath -> Integer -> Int -> Integer
    -> IO (String, Integer)          -- ^ 载荷动作：(本端算出的哈希, 实发字节数)
    -> IO (Either String ())
remoteRecvFrom hh relPath sz mode_ mtime_ payload = do
    eb <- remoteRecvBegin hh SKFile relPath sz mode_ mtime_
    case eb of
        Left e -> return (Left e)
        Right () -> do
            (hsh, n) <- payload
            finishRemoteRecv hh sz n hsh

-- | 让远端发送内容：响应里的 size 决定紧随其后的载荷长度
remoteSend :: HelperHandle -> SyncKind -> FilePath -> IO (Either String Integer)
remoteSend hh kind relPath = do
    r <- call hh (Msg.cmd "send" [ ("kind", kindName (kindState kind))
                                 , ("path", Msg.str relPath) ])
    return $ case r of
        Left e -> Left ("send: " ++ e)
        Right v -> case Msg.fieldInt "size" v of
            Just sz -> Right sz
            Nothing -> Left "send: response has no \"size\""

-- | 读发送端载荷之后那条哈希尾行
remoteHashTrailer :: HelperHandle -> IO (Either String String)
remoteHashTrailer hh = do
    r <- readMsg hh
    return $ case r of
        Left e  -> Left ("send: " ++ e)
        Right v -> maybe (Left "send: trailer has no \"hash\"") Right (Msg.fieldStr "hash" v)

-- | 不吃掉源端的载荷就必须排空：它已经答应了 sz 字节，不读走会把管道写满而卡死，
--   之后所有命令都会错位。
drainRemoteSend :: HelperHandle -> Integer -> IO ()
drainRemoteSend hh sz = do
    discardExactly (hhOut hh) sz
    _ <- readMsg hh
    return ()

-- ============================================================================
-- 其他
-- ============================================================================

-- | 第 4 步的收尾：本次扫描期间**发起过的每一次 rmdir**，按 root 分别列出。
--
--   * 成功的 → 列在 "removed empty directories" 下面（正常情况就是全部）；
--   * **失败的单独告警**：判定为空之后 rmdir 本该必然成功，出现失败说明
--     "判定"与"事实"不一致（竞态：判定与 rmdir 之间进了新条目），或文件系统拒绝
--     （挂载点 EBUSY / 权限不足 / 只读文件系统）—— 这是值得看一眼的信号，不静默吞掉。
--
--   顺序是深 → 浅（子目录先被删掉，父目录才可能变空）。什么都没发起就一个字不说。
showRmdirResults :: String -> [(FilePath, Bool)] -> IO ()
showRmdirResults which rs = do
    let ok  = [ p | (p, True)  <- rs ]
        bad = [ p | (p, False) <- rs ]
    unless (null ok) $ do
        putStrLn $ "\n" ++ which ++ " removed empty directories:"
        forM_ ok $ \d -> putStrLn $ "  - " ++ d
    unless (null bad) $ do
        putStrLn $ "\nWARNING: " ++ which ++ " could not remove empty directories:"
        forM_ bad $ \d -> putStrLn $ "  - " ++ d

-- | 第 5 步的初步统计：一端一行，四种状态都列（含 0）。
--
--   两端地址第 0 步已经列过（@root1 = …@ / @root2 = …@），这里不再重复，只写
--   @root1@ / @root2@（用户 2026-09-22 要求）。
--   统计内容是**每侧相对自己基线**的状态变化（'diffOneSide' 的直接结果），
--   早于冲突消解与内容校验，所以叫"初步"。
--   四种 = new / updated / unchanged / deleted；'None' 表示该侧从未有过该条目，
--   不属于它的"自身状态变化"，故不列。
showStats :: String -> SideStats -> IO ()
showStats which s = putStrLn $
    "  " ++ which ++ ": new "       ++ show (statNew s)
        ++ ", updated "   ++ show (statUpdated s)
        ++ ", unchanged " ++ show (statUnchanged s)
        ++ ", deleted "   ++ show (statDeleted s)

-- | 第 5 步的**初步结果**：把两端算出来的动作按种类计数（紧跟在两端状态统计之后）。
--
--   种类就是 'SyncAction' 的 5 个构造子（无动作的 'Nothing' 不是种类，不计）：
--   receive（要接收）/ delete（要删除）/ conflict（冲突，跳过）/
--   record-add（已同步，只记 added）/ record-delete（两端已删，只记 deleted）。
--   两端**合并计数**、五种都列（含 0）。计数按**名字去重**：一个真冲突在 pairs1 /
--   pairs2 里各有一条（同一个文件），它只算 1 个（与 'showSummary' 的 "N conflicts" 同口径）。
--
--   这是"初步"的：内容校验（接下来的 Extra content check）会把一部分
--   receive / conflict 改成 record-add，所以这里的数字可能比第 6 步的摘要注意大。
--   恒定打印（全 0 也打），位置在两端状态统计之后、`Nothing to do.` / 内容校验之前。
showActionResult
    :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)] -> IO ()
showActionResult pairs1 pairs2 = do
    let entries = [ (changeName fc, a) | (fc, Just a) <- pairs1 ++ pairs2 ]
        n a = length (dedup [ f | (f, a') <- entries, a' == a ])
    putStrLn $ "  result: " ++ show (n Receive)       ++ " receive"
                 ++ ", "      ++ show (n Remove)         ++ " delete"
                 ++ ", "      ++ show (n Unsolvable)    ++ " conflict"
                 ++ ", "      ++ show (n RecordAdd)      ++ " record-add"
                 ++ ", "      ++ show (n RecordDelete)   ++ " record-delete"

showSummary :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)] -> IO ()
showSummary pairs1 pairs2 = do
    let add1 = [ (f, d1, d2) | (Added    fs d1 d2, Just Receive) <- pairs1, let f = filename fs ]
        del1 = [ (f, d1, d2) | (Del  fs d1 d2, Just Remove)  <- pairs1, let f = filename fs ]
        add2 = [ (f, d1, d2) | (Added    fs d1 d2, Just Receive) <- pairs2, let f = filename fs ]
        del2 = [ (f, d1, d2) | (Del  fs d1 d2, Just Remove)  <- pairs2, let f = filename fs ]
        recAdds = dedup [ f | (Noop fs _ _, Just RecordAdd)    <- pairs1 ++ pairs2, let f = filename fs ]
        recDels = dedup [ f | (Del  fs _ _, Just RecordDelete) <- pairs1 ++ pairs2, let f = filename fs ]
        conf = dedup [ (f, d1, d2) | (Conflict fs d1 d2, Just Unsolvable) <- pairs1 ++ pairs2, let f = filename fs ]

    when (not (null del1)) $ do
        putStrLn "\nroot1 will delete:"
        forM_ del1 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ sideStateLabel d1 ++ " r2:" ++ sideStateLabel d2 ++ "]"
    when (not (null add1)) $ do
        putStrLn "\nroot1 will receive (root2 -> root1):"
        forM_ add1 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ sideStateLabel d1 ++ " r2:" ++ sideStateLabel d2 ++ "]"
    when (not (null del2)) $ do
        putStrLn "\nroot2 will delete:"
        forM_ del2 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ sideStateLabel d1 ++ " r2:" ++ sideStateLabel d2 ++ "]"
    when (not (null add2)) $ do
        putStrLn "\nroot2 will receive (root1 -> root2):"
        forM_ add2 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ sideStateLabel d1 ++ " r2:" ++ sideStateLabel d2 ++ "]"
    when (not (null recAdds)) $ do
        putStrLn "\nAlready in sync (will record as added):"
        forM_ recAdds $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null recDels)) $ do
        putStrLn "\nAlready deleted (will record as deleted):"
        forM_ recDels $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null conf)) $ do
        putStrLn "\nConflicts (will skip):"
        forM_ conf $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ sideStateLabel d1 ++ " r2:" ++ sideStateLabel d2 ++ "]"
    let total = length add1 + length del1 + length add2 + length del2 + length recAdds + length recDels
    putStrLn $ "\nTotal: " ++ show total ++ " operations, " ++ show (length conf) ++ " conflicts"

-- ============================================================================
-- repo：显式双端同步时把对端地址写下来（供以后 hsbisync sync <profile> 免参同步）
-- ============================================================================

-- | 把对端地址强制写进双方的 @.bisync/\<profile\>/repo@，返回**两端各自写进去的地址**
--   （'Nothing' = 内容没变、没写；展示统一由 'showPrepare' 做）。
--
--   既然命令里已给出完整路径，它就是权威输入：直接覆盖，不与旧内容比较。
--   * 两端都是本地 → 两端互写对方的绝对路径（本机 Local 路径不经 shell，
--     不能用相对路径）
--   * 一端本地一端远程 → 本地端写 @host:远端路径@（沿用输入的原样路径）；
--     远程端写 @本机用户@本机IP:本机路径@ —— IP 由远端 helper 从 sshd 的
--     SSH_CONNECTION 里取，路径按家目录简写（见 'recordRemoteRepo'）
--   * 两端都远程 → 都跳过（互相的地址在对方看来未必可用）
persistRepo :: String -> Root -> Root -> IO (Maybe String, Maybe String)
persistRepo profile r1 r2 =
    case (r1, r2) of
        (Local p1, Local p2) -> do
            a1 <- canonicalizePath p1
            a2 <- canonicalizePath p2
            w1 <- writeRepo profile p1 a2
            w2 <- writeRepo profile p2 a1
            return (w1, w2)
        (Local p1, Remote h2 p2) -> do
            w1 <- writeRepo profile p1 (remoteAddr r2)
            w2 <- recordRemoteRepo h2 p2 profile p1
            return (w1, w2)
        (Remote h1 p1, Local p2) -> do
            w2 <- writeRepo profile p2 (remoteAddr r1)
            w1 <- recordRemoteRepo h1 p1 profile p2
            return (w1, w2)
        (Remote _ _, Remote _ _) -> return (Nothing, Nothing)
  where
    -- 仅在 r1/r2 为 Remote 时调用
    remoteAddr (Remote h p) = h ++ ":" ++ p
    remoteAddr (Local _)    = ""

-- | 把对端地址写进某个本地仓库：**只在内容真会变时**才写，返回写进去的地址。
--
--   第 1 步（sync prepare）的约定：只报有实际变化的动作 —— 所以先看一眼现有 repo，
--   已经是这个地址就什么都不做（返回 'Nothing'，'showPrepare' 也就不提这一行）。
writeRepo :: String -> FilePath -> String -> IO (Maybe String)
writeRepo profile localPath addr = do
    let fp = localPath </> ".bisync" </> profile </> "repo"
        want = B8.pack addr
    same <- do
        e <- doesFileExist fp
        if not e
            then return False
            else (== want) . B8.strip <$> BS.readFile fp
    if same
        then return Nothing
        else do
            writeRepoFile fp addr
            return (Just addr)

-- ============================================================================
-- repo 记录：让远端也写下"本机（登录方）地址"
-- ============================================================================

-- | 让远端 helper 把它自己的 .bisync/<profile>/repo 写成**本机（登录方）地址**：
--
--     <本机用户名>@<本机IP>:<本机路径>
--
--   * IP 只有对面知道（sshd 把 SSH_CONNECTION 放进远端进程的环境里），
--     所以 host 部分由 helper 补上，本机只提供用户名与路径；
--   * 用户名取本机当前登录用户 —— 反向 ssh 到本机时用的就是这个名字；
--   * 路径取本机同步根，按家目录简写规则 'formatOwnPath' 处理
--     （家目录之下写相对路径，其余写绝对路径）。
--
--   这样远端也具备 repo，在远端直接 `hsbisync sync <profile>` 即可反向免参同步。
--   repo 只是"下次免参同步"的缓存，所以任何失败都只提示、不中断同步。
--
--   成功且内容真的变了才返回 @Just 地址@；没变（helper 回 unchanged）或没写成
--   都返回 'Nothing' —— 打印由 'showPrepare' 统一做（只报有实际变化的动作）。
recordRemoteRepo :: String -> FilePath -> String -> FilePath -> IO (Maybe String)
recordRemoteRepo host remotePath profile ownPath = do
    r <- tryOurErrors $
            bracket (spawnHelper host remotePath profile) destroyHelper $ \hh -> do
                user <- getEffectiveUserName
                path <- formatOwnPath ownPath
                call hh (Msg.cmd "putrepo" [ ("user", Msg.str user)
                                           , ("path", Msg.str path) ])
    case r of
        Left err         -> note ("not written (" ++ err ++ ")")
        Right (Left err) -> note ("not written (" ++ err ++ ")")
        Right (Right v)
            | Just True <- Msg.asBool =<< Msg.field "noclient" v ->
                note "not written: cannot tell which address the peer sees us as (SSH_CONNECTION unset)"
            | Just addr <- Msg.fieldStr "addr" v ->
                -- 远端 repo 内容没变时 helper 会回 unchanged：没写 → 不报（第 1 步只报有变化的动作）
                if isUnchanged v
                    then return Nothing
                    else return (Just addr)
            | otherwise -> note "not written: unexpected response"
  where
    -- 失败/无法判定都只是提示（repo 只是缓存）：打完继续，这一端就没写得成的 repo
    note msg = putStrLn ("  NOTE: " ++ host ++ ": repo " ++ msg) >> return Nothing

    isUnchanged v = case Msg.asBool =<< Msg.field "unchanged" v of
        Just True -> True
        _         -> False

-- | 只捕获"我们自己的"失败（IO 异常 + 我们 error 出来的 ErrorCall），
--   不吞 Ctrl-C 之类的异步异常。Left 是可读的失败原因。
tryOurErrors :: IO a -> IO (Either String a)
tryOurErrors act = (Right <$> act) `catch` ioToLeft `catch` callToLeft
  where
    ioToLeft :: IOException -> IO (Either String a)
    ioToLeft e = return (Left (show e))

    callToLeft :: ErrorCall -> IO (Either String a)
    callToLeft = return . Left . displayException