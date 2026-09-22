module Exe where

import Control.Concurrent.Async (concurrently)
import Control.Exception (ErrorCall, IOException, bracket, catch, displayException)
import Control.Monad (filterM, forM_, unless, when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>))
import System.IO
    ( hFlush, stdout, stdin
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
    , currentState, currentLinkState
    , lastStateKind, appendAddedKind, appendDeletedKind, discardStateUnder, posixSecs
    , indexStates, dedup
    )
import Sync (sync, SyncAction(..), resolveConflicts)
import Write
    ( HelperHandle(..), call, readMsg, hGetExactly, hPutFully
    , writeFileVerified, writeLinkVerified, readLinkContent
    , hashBytes, removeFileSafe, removeLinkSafe, getIgnoreRemote, setMtimeOnly
    , writeRepoFile
    )
import qualified Msg
import Arg
    ( Root(..)
    , ensureRoot, formatOwnPath, requireRoot, rootLabel
    )

-- ============================================================================
-- 统一入口
-- ============================================================================

-- | 两个同步地址从哪来 —— 决定第 2 步做什么
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

-- | 同步主流程：标题之后一共 **7 步**
--
--   0. （不编号）先明确两端地址与 profile
--   1. 握手：问出远端 hsbisync 在哪、让它自报真实路径与家目录
--   2. 同步目录与 repo：写明地址 → 建目录 + 写 repo；只给 profile → 只检查存在
--   3. 同步 ignore 规则（无条件先对齐，之后 ignore 不再参与文件同步）
--   4. 同步软链接结构
--   5. 读两端状态
--   6. 计算同步动作（diff + 冲突消解 + 额外内容校验）
--   7. 列动作、确认、执行（真正的传输与记录）
--
--   走到第 3 步时保证：两个同步目录一定存在（第 2 步要么建了、要么查了）。
runSync :: RootSource -> String -> Root -> Root -> IO ()
runSync srcKind profile root1 root2 = do
    let (mHost1, path1) = case root1 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        (mHost2, path2) = case root2 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        mTag = Just profile

    -- 阶段 0：先明确两端地址（本地端就是一个路径，没有 local: 这种写法）
    putStrLn $ "\n  root1 = " ++ rootLabel root1
    putStrLn $ "  root2 = " ++ rootLabel root2
    putStrLn $ "  profile: " ++ profile

    -- 阶段 1：与远端握手（hsbisync 在远端的位置 + 它自报的真实路径与家目录）。
    -- 总在最前面：第 2 步写远端 repo 要用到它自报的绝对路径。命中缓存时不重复 ssh。
    prepareRemotes profile [root1, root2]
    infos <- handshakeInfo [root1, root2]
    let showHandshake :: (String, Maybe String, Maybe FilePath) -> IO ()
        showHandshake (host, mHome, mExe) = do
            putStrLn $ "\n[1/7] Handshake " ++ host ++ " ..."
            putStrLn $ "  home     = " ++ fromMaybe "?" mHome
            putStrLn $ "  hsbisync = " ++ fromMaybe "(not reported)" mExe
    mapM_ showHandshake infos

    -- 阶段 2：确保同步需要的东西就绪（同步目录 / .bisync/<profile> / repo）
    --
    --   写明地址 → 缺目录就建（只建一层，见 'ensureRoot'）+ 写 repo；
    --   只给 profile → 只查两端目录存在，不建不写。
    --   **只打印有实际变化的动作**：检查无误就静默（.bisync/<profile> 由写 repo 顺手建）。
    putStrLn "\n[2/7] Ensure sync needs ..."
    case srcKind of
        RootsGiven -> do
            ensureRoot root1
            ensureRoot root2
            persistRepo profile root1 root2
        RootsFromRepo -> do
            requireRoot root1
            requireRoot root2

    -- 阶段 3：无条件先同步当前 profile 的 ignore 文件（较新一端覆盖较旧一端）。
    -- 两端 ignore 必须先一致，之后各自扫描文件集合才不会出现偏差。
    -- 此阶段之后 ignore 文件不再参与文件同步。
    putStrLn "\n[3/7] Syncing ignore rules ..."
    syncIgnoreFiles profile mHost1 path1 mHost2 path2

    -- 阶段 4：同步软链接（先让两端的链接结构完全一致）
    syncLinkPhase profile mTag mHost1 path1 mHost2 path2

    -- 阶段 5：并行读取两端状态（各自按已统一的 ignore 过滤）
    --
    --   悬空链接不做特殊处理：扫描跟随链接时那里自然"什么都没有"，
    --   diff 该得到 Deleted 就得到 Deleted（删除动作本身有确认关卡）。
    putStrLn "\n[5/7] Reading states ..."
    ((label1, (last1, cur1)), (label2, (last2, cur2))) <-
        concurrently (readSideK SKFile mTag mHost1 path1) (readSideK SKFile mTag mHost2 path2)

    putStrLn $ "  root1 (" ++ label1 ++ "): " ++ show (length cur1) ++ " files (last: " ++ show (length last1) ++ ")"
    putStrLn $ "  root2 (" ++ label2 ++ "): " ++ show (length cur2) ++ " files (last: " ++ show (length last2) ++ ")"

    -- 名字索引：名字 → 当前状态。后面（内容校验）只做 O(log n) 查询，
    -- 不再逐条线性重找（那是 O(n²)）。惰性：不会走到的路径就不会建索引。
    let idx1 = indexStates cur1
        idx2 = indexStates cur2

    -- 阶段 6：计算最终动作（diff + 形式冲突消解 + 额外内容校验）。
    -- 只处理两端扫描出的普通文件；ignore 文件已在阶段 3、软链接已在阶段 4 处理。
    putStrLn "\n[6/7] Computing sync actions ..."
    -- 计算 + 消解形式冲突（同 size 同整数秒 mtime 的一对直接判作已同步）
    let (pairs1, pairs2) = sync (last1, last2) (cur1, cur2)
        (_, pairs1', pairs2') = resolveConflicts pairs1 pairs2

    let hasWork = any (isJust . snd) pairs1' || any (isJust . snd) pairs2'
    if not hasWork
        then putStrLn "\nNothing to do."
        else withHelpers mHost1 path1 mHost2 path2 profile $ \side1 side2 -> do
                -- 额外内容校验：冲突的、或要传输的，只要两端 size 相同就静默核对内容；
                -- 一致的“只更新属性并记录”，不一致的维持原判（见 'verifySameSizeCandidates'）。
                (pairs1'', pairs2'') <- verifySameSizeCandidates idx1 idx2 side1 side2 pairs1' pairs2'

                -- 阶段 7：列出最终动作、确认、执行（真正的传输与记录）
                putStrLn "\n[7/7] Sync and record files ..."
                showSummary pairs1'' pairs2''

                yes <- askYesNo "\nProceed? [y/N] "
                if not yes
                    then putStrLn "Aborted."
                    else do
                        execute SKFile mTag side1 side2 pairs1''
                        execute SKFile mTag side2 side1 pairs2''

    putStrLn "\nSync complete."

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

-- | 取某一端文件内容的 SHA512（本地读字节，远端用 hash 命令）；读不到返回 Nothing
hashSideContent :: Side -> FilePath -> IO (Maybe String)
hashSideContent (Lcl root) relPath =
    fmap Just (hashBytes <$> BS.readFile (root </> relPath)) `catch` ioFail
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
syncLinkPhase
    :: String -> Maybe String
    -> Maybe String -> FilePath -> Maybe String -> FilePath -> IO ()
syncLinkPhase profile mTag mHost1 path1 mHost2 path2 = do
    -- 阶段 3：同步软链接（先让两端的链接结构完全一致）
    -- 阶段 4：同步软链接（先让两端的链接结构完全一致）
    putStrLn "\n[4/7] Syncing symlinks ..."
    ((label1, (last1, cur1)), (label2, (last2, cur2))) <-
        concurrently (readSideK SKLink mTag mHost1 path1) (readSideK SKLink mTag mHost2 path2)

    putStrLn $ "  root1 (" ++ label1 ++ "): " ++ show (length cur1) ++ " symlinks (last: " ++ show (length last1) ++ ")"
    putStrLn $ "  root2 (" ++ label2 ++ "): " ++ show (length cur2) ++ " symlinks (last: " ++ show (length last2) ++ ")"

    let (pairs1, pairs2) = sync (last1, last2) (cur1, cur2)
        (_, pairs1', pairs2') = resolveConflicts pairs1 pairs2
        hasWork = any (isJust . snd) pairs1' || any (isJust . snd) pairs2'

    if not hasWork
        then putStrLn "  (symlink structures already identical)"
        else do
            showSummary pairs1' pairs2'
            yes <- askYesNo "\nApply symlink changes? [y/N] "
            if not yes
                then putStrLn "Aborted (symlink phase)."
                else withHelpers mHost1 path1 mHost2 path2 profile $ \side1 side2 -> do
                        execute SKLink mTag side1 side2 pairs1'
                        discardLocal side1 mTag pairs1'
                        execute SKLink mTag side2 side1 pairs2'
                        discardLocal side2 mTag pairs2'

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
    -> (Side -> Side -> IO ()) -> IO ()
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
--   * 都无 → 无事
--   * 仅一端有 → 复制到另一端
--   * 都有且内容相同 → 跳过
--   * 都有但内容不同 → 较新（mtime 大）的一端覆盖另一端；
--     mtime 相同则告警并采用 root1
syncIgnore :: String -> Side -> Side -> IO ()
syncIgnore profile sideA sideB = do
    mA <- sideIgnoreMeta sideA profile
    mB <- sideIgnoreMeta sideB profile
    case (mA, mB) of
        (Nothing, Nothing) ->
            putStrLn "  (no ignore file on root1 or root2)"
        (Just a, Nothing) ->
            copyIgnore profile sideA sideB "root2" a
        (Nothing, Just b) ->
            copyIgnore profile sideB sideA "root1" b
        (Just a, Just b)
            | hashOf a == hashOf b ->
                putStrLn "  ignore identical on root1 and root2"
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
-- | 本地读取"内容"：文件读字节，链接读指向字符串
kindReadLocal :: SyncKind -> FilePath -> IO BS.ByteString
kindReadLocal SKFile = BS.readFile
kindReadLocal SKLink = readLinkContent

-- | 本地落地"内容"：写文件 / 建链接（都是先校验后 rename）
kindWriteLocal
    :: SyncKind -> FilePath -> FilePath -> BS.ByteString -> Int -> Integer -> String
    -> IO (Either String ())
kindWriteLocal SKFile = writeFileVerified
kindWriteLocal SKLink = writeLinkVerified

-- | 读取一端的状态：(label, (上次状态, 当前状态))
--
--   悬空链接没有特殊处理：扫描只是扫不到那里的东西。
readSideK
    :: SyncKind -> Maybe String -> Maybe String -> FilePath
    -> IO (String, ([FileState], [FileState]))
readSideK kind mTag mHost path =
    case mHost of
        Nothing -> do
            l <- lastStateKind (kindState kind) path mTag
            c <- case kind of
                SKFile -> currentState path mTag
                SKLink -> currentLinkState path mTag
            return ("local", (l, c))
        Just host -> do
            hh <- spawnHelper host path (fromMaybe "" mTag)
            l <- getHelperStates hh "last"  (kindState kind)
            c <- getHelperStates hh "state" (kindState kind)
            destroyHelper hh
            return ("ssh " ++ host, (l, c))

-- | 读对端状态：which = "state"（本次扫描）或 "last"（上次基线）
getHelperStates :: HelperHandle -> String -> StateKind -> IO [FileState]
getHelperStates hh which kind = do
    r <- call hh (Msg.cmd which [("kind", kindName kind)])
    case r of
        Left err -> error $ which ++ " failed: " ++ err
        Right v  -> case Msg.field "entries" v of
            Just e  -> return (parseStateValue e)
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

-- | 执行一侧的所有操作（文件/链接阶段共用，动作按 kind 区分）。
--
--   顺序固定为：**先"纯纪录"的动作**（只写状态、不动文件内容），**再删除，最后传输**
--   —— 纯纪录是双方内容已经一致、只补状态的事，先落地它，确认环节里出现过的东西
--   就不会因为后面的动作而变卦。
execute :: SyncKind -> Maybe String -> Side -> Side -> [(FileChange, Maybe SyncAction)] -> IO ()
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
    unless (null removes) $ do
        putStrLn "\nDeleting then record..."
        forM_ removes $ \(relPath, _, _) -> do
            putStrLn $ "  " ++ kindDeleteVerb kind ++ " " ++ relPath
            delEnd kind self relPath
            appendEnd kind self  mTag RecDelete relPath
            appendEnd kind other mTag RecDelete relPath
    unless (null receives) $ do
        putStrLn "\nReceiving then record..."
        forM_ receives $ \(relPath, srcMode, srcMtime) -> do
            putStrLn $ "  " ++ kindVerb kind ++ " " ++ relPath
            copyEndToEnd kind other self mTag relPath srcMode srcMtime

-- ============================================================================
-- End 操作
-- ============================================================================

delEnd :: SyncKind -> Side -> FilePath -> IO ()
delEnd SKFile (Lcl root) relPath = removeFileSafe (root </> relPath)
delEnd SKFile (Rmt hh)  relPath = do
    res <- call hh (Msg.cmd "unlink" [("kind", kindName FileKind), ("path", Msg.str relPath)])
    case res of
        Left err -> error $ "FATAL: remote unlink failed: " ++ err
        Right _  -> return ()
-- 链接阶段：只删链接本身（unlink），不跟随到目标；占位不是链接则不动
delEnd SKLink (Lcl root) relPath = do
    ok <- removeLinkSafe (root </> relPath)
    unless ok $ putStrLn $ "  WARNING: not a symlink, left untouched: " ++ relPath
delEnd SKLink (Rmt hh) relPath = do
    res <- call hh (Msg.cmd "unlink" [("kind", kindName LinkKind), ("path", Msg.str relPath)])
    case res of
        Left err -> error $ "FATAL: remote unlink (link) failed: " ++ err
        Right v  -> case Msg.asBool =<< Msg.field "notLink" v of
            Just True -> putStrLn $ "  WARNING: not a symlink, left untouched: " ++ relPath
            _         -> return ()

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
copyEndToEnd :: SyncKind -> Side -> Side -> Maybe String -> FilePath -> Int -> Integer -> IO ()
copyEndToEnd kind src dst mTag relPath srcMode srcMtime = do
    result <- transferContent kind src dst relPath srcMode srcMtime
    case result of
        Left err -> putStrLn $ "  " ++ err
        Right () -> do
            putStrLn "    verified OK"
            appendEnd kind dst mTag RecAdd relPath
            appendEnd kind src mTag RecAdd relPath

-- | 执行内容传输。本地目标使用 'writeFileVerified' / 'writeLinkVerified'
--   （校验通过才 rename），远程目标使用扩展的 RECV/LINKRECV 协议
--   （helper 端做同样的事）。文件与链接只有"读哪来的内容/怎么落地"不同。
transferContent :: SyncKind -> Side -> Side -> FilePath -> Int -> Integer -> IO (Either String ())
transferContent kind (Lcl srcRoot) (Lcl dstRoot) relPath srcMode srcMtime = do
    let s = srcRoot </> relPath
        d = dstRoot </> relPath
    content <- kindReadLocal kind s
    let expectedHash = hashBytes content
    kindWriteLocal kind dstRoot d content srcMode srcMtime expectedHash

transferContent kind (Rmt hhSrc) (Lcl dstRoot) relPath srcMode srcMtime = do
    let d = dstRoot </> relPath
    expectedHash <- hashRemote kind hhSrc relPath         -- 先拿到期望哈希
    content <- recvRemoteContent kind hhSrc relPath
    kindWriteLocal kind dstRoot d content srcMode srcMtime expectedHash

transferContent kind (Lcl srcRoot) (Rmt hhDst) relPath srcMode srcMtime = do
    let s = srcRoot </> relPath
    content <- kindReadLocal kind s
    let expectedHash = hashBytes content
    recvVerified kind hhDst (BS.length content) relPath srcMode srcMtime expectedHash content

transferContent kind (Rmt hhSrc) (Rmt hhDst) relPath srcMode srcMtime = do
    expectedHash <- hashRemote kind hhSrc relPath
    content <- recvRemoteContent kind hhSrc relPath
    recvVerified kind hhDst (BS.length content) relPath srcMode srcMtime expectedHash content

-- | 向远端 helper 发 recv（带 size/mode/mtime/hash），传输数据后等它完成
--   写临时文件 → 设属性 → 校验 → rename，并返回结果。
--   链接阶段 mode 不适用（helper 端忽略）。
recvVerified
    :: SyncKind -> HelperHandle -> Int -> FilePath -> Int -> Integer -> String
    -> BS.ByteString -> IO (Either String ())
recvVerified kind hh sz relPath mode_ mtime_ expectedHash content = do
    r <- call hh (Msg.cmd "recv" [ ("kind",  kindName (kindState kind))
                                  , ("path",  Msg.str relPath)
                                  , ("size",  Msg.int (fromIntegral sz))
                                  , ("mode",  Msg.int (fromIntegral mode_))
                                  , ("mtime", Msg.int mtime_)
                                  , ("hash",  Msg.str expectedHash) ])
    case r of
        Left err -> return $ Left ("recv: " ++ err)
        Right _  -> do
            hPutFully (hhIn hh) content     -- 载荷：size 字节裸数据
            hFlush (hhIn hh)
            r2 <- readMsg hh                -- 第二步：校验 + rename 结果
            return $ either (Left . ("verify: " ++)) (const (Right ())) r2

-- ============================================================================
-- Helper 协议辅助
-- ============================================================================

-- | 让远端发来某条目内容：send 的响应带 size，紧跟其后的就是裸数据
recvRemoteContent :: SyncKind -> HelperHandle -> FilePath -> IO BS.ByteString
recvRemoteContent kind hh relPath = do
    r <- call hh (Msg.cmd "send" [ ("kind", kindName (kindState kind))
                                  , ("path", Msg.str relPath) ])
    case r of
        Left err -> error $ "send failed: " ++ err
        Right v  -> case Msg.fieldInt "size" v of
            Just sz -> hGetExactly (hhOut hh) (fromIntegral sz)
            Nothing -> error "send: response has no \"size\""

hashRemote :: SyncKind -> HelperHandle -> FilePath -> IO String
hashRemote kind hh relPath = do
    r <- call hh (Msg.cmd "hash" [ ("kind", kindName (kindState kind))
                                  , ("path", Msg.str relPath) ])
    case r of
        Left err -> error $ "hash failed: " ++ err
        Right v  -> case Msg.fieldStr "hash" v of
            Just h  -> return h
            Nothing -> error "hash: response has no \"hash\""

-- ============================================================================
-- 其他
-- ============================================================================

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
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null add1)) $ do
        putStrLn "\nroot1 will receive (root2 -> root1):"
        forM_ add1 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null del2)) $ do
        putStrLn "\nroot2 will delete:"
        forM_ del2 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null add2)) $ do
        putStrLn "\nroot2 will receive (root1 -> root2):"
        forM_ add2 $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    when (not (null recAdds)) $ do
        putStrLn "\nAlready in sync (will record as added):"
        forM_ recAdds $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null recDels)) $ do
        putStrLn "\nAlready deleted on both sides (will record as deleted):"
        forM_ recDels $ \f ->
            putStrLn $ "  - " ++ f
    when (not (null conf)) $ do
        putStrLn "\nConflicts (will skip):"
        forM_ conf $ \(f, d1, d2) ->
            putStrLn $ "  - " ++ f ++ "  [r1:" ++ show d1 ++ " r2:" ++ show d2 ++ "]"
    let total = length add1 + length del1 + length add2 + length del2 + length recAdds + length recDels
    putStrLn $ "\nTotal: " ++ show total ++ " operations, " ++ show (length conf) ++ " conflicts"

-- ============================================================================
-- repo：显式双端同步时把对端地址写下来（供以后 hsbisync sync <profile> 免参同步）
-- ============================================================================

-- | 把对端地址强制写进双方的 @.bisync/\<profile\>/repo@。
--
--   既然命令里已给出完整路径，它就是权威输入：直接覆盖，不与旧内容比较。
--   * 两端都是本地 → 两端互写对方的绝对路径（本机 Local 路径不经 shell，
--     不能用相对路径）
--   * 一端本地一端远程 → 本地端写 @host:远端路径@（沿用输入的原样路径）；
--     远程端写 @本机用户@本机IP:本机路径@ —— IP 由远端 helper 从 sshd 的
--     SSH_CONNECTION 里取，路径按家目录简写（见 'recordRemoteRepo'）
--   * 两端都远程 → 都跳过（互相的地址在对方看来未必可用）
persistRepo :: String -> Root -> Root -> IO ()
persistRepo profile r1 r2 =
    case (r1, r2) of
        (Local p1, Local p2) -> do
            a1 <- canonicalizePath p1
            a2 <- canonicalizePath p2
            writeRepo profile p1 a2
            writeRepo profile p2 a1
        (Local p1, Remote h2 p2) -> do
            writeRepo profile p1 (remoteAddr r2)
            recordRemoteRepo h2 p2 profile p1
        (Remote h1 p1, Local p2) -> do
            writeRepo profile p2 (remoteAddr r1)
            recordRemoteRepo h1 p1 profile p2
        (Remote _ _, Remote _ _) -> return ()
  where
    -- 仅在 r1/r2 为 Remote 时调用
    remoteAddr (Remote h p) = h ++ ":" ++ p
    remoteAddr (Local _)    = ""

-- | 把对端地址写进某个本地仓库：**只在内容真会变时**才写 + 打印。
--
--   第 2 步的约定（用户 2026-09-22）：只打印有实际变化的动作，检查无误就静默 ——
--   所以先看一眼现有 repo，已经是这个地址就什么都不做。
writeRepo :: String -> FilePath -> String -> IO ()
writeRepo profile localPath addr = do
    let fp = localPath </> ".bisync" </> profile </> "repo"
        want = B8.pack addr
    same <- do
        e <- doesFileExist fp
        if not e
            then return False
            else (== want) . B8.strip <$> BS.readFile fp
    if same
        then return ()
        else do
            writeRepoFile fp addr
            putStrLn $ "  " ++ localPath ++ ": repo = " ++ addr ++ "  [OK]"

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
recordRemoteRepo :: String -> FilePath -> String -> FilePath -> IO ()
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
                -- 远端 repo 内容没变时 helper 会回 unchanged：静默（第 2 步只报有变化的动作）
                if isUnchanged v
                    then return ()
                    else putStrLn $ "  " ++ host ++ ":" ++ remotePath ++ ": repo = " ++ addr ++ "  [OK]"
            | otherwise -> note "not written: unexpected response"
  where
    note msg = putStrLn $ "  NOTE: " ++ host ++ ":" ++ remotePath ++ ": repo " ++ msg

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