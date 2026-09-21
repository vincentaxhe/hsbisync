module Exe where

import Control.Concurrent.Async (concurrently)
import Control.Exception (ErrorCall, IOException, bracket, catch, displayException)
import Control.Monad (filterM, forM_, unless, when)
import Data.Char (toLower)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.IO
    ( hClose, hFlush, hGetLine, hSetBinaryMode
    , hSetEncoding, utf8, stdout, stdin
    , hSetEcho, hSetBuffering, BufferMode(..)
    , hReady
    )
import System.Process (createProcess, proc, StdStream(..), CreateProcess(..))
import System.Posix.User (getEffectiveUserName)

import qualified Data.ByteString as BS

import State
    ( FileState(..), FileChange(..), StateKind(..)
    , parseStateJSON, statRel
    , currentState, currentLinkState
    , lastStateKind, appendAddedKind, appendDeletedKind, discardStateUnder, posixSecs
    )
import Sync (sync, SyncAction(..), resolveConflicts)
import Write
    ( HelperHandle(..), Response(..), sendCommand, readResponse, readJSON
    , hGetExactly, hPutFully, expectOK
    , writeFileVerified, writeLinkVerified, readLinkContent
    , hashBytes, removeFileSafe, removeLinkSafe, getIgnoreRemote, setMtimeOnly
    )
import Arg (Root(..), formatOwnPath, rootLabel)

-- ============================================================================
-- 统一入口
-- ============================================================================

runSync :: String -> Root -> Root -> IO ()
runSync profile root1 root2 = do
    putStrLn "==================================="
    putStrLn "  hsbisync - Bidirectional Sync"
    putStrLn "==================================="

    let (mHost1, path1) = case root1 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        (mHost2, path2) = case root2 of Local p -> (Nothing, p); Remote h p -> (Just h, p)
        mTag = Just profile

    -- 阶段 0：明确两端地址
    putStrLn $ "\n  root1 = " ++ rootLabel root1
    putStrLn $ "  root2 = " ++ rootLabel root2
    putStrLn $ "  profile: " ++ profile

    -- 阶段 1：无条件先同步当前 profile 的 ignore 文件（较新一端覆盖较旧一端）。
    -- 两端 ignore 必须先一致，之后各自扫描文件集合才不会出现偏差。
    -- 此阶段之后 ignore 文件不再参与文件同步。
    putStrLn "\n[1/4] Syncing ignore rules ..."
    syncIgnoreFiles profile mHost1 path1 mHost2 path2

    -- 阶段 2：同步软链接（先让两端的链接结构完全一致）
    syncLinkPhase profile mTag mHost1 path1 mHost2 path2

    -- 阶段 3：并行读取两端状态（各自按已统一的 ignore 过滤）
    --
    --   悬空链接不做特殊处理：扫描跟随链接时那里自然"什么都没有"，
    --   diff 该得到 Deleted 就得到 Deleted（删除动作本身有确认关卡）。
    putStrLn "\n[3/4] Reading states ..."
    ((label1, (last1, cur1)), (label2, (last2, cur2))) <-
        concurrently (readSideK SKFile mTag mHost1 path1) (readSideK SKFile mTag mHost2 path2)

    putStrLn $ "  root1 (" ++ label1 ++ "): " ++ show (length cur1) ++ " files (last: " ++ show (length last1) ++ ")"
    putStrLn $ "  root2 (" ++ label2 ++ "): " ++ show (length cur2) ++ " files (last: " ++ show (length last2) ++ ")"

    -- 阶段 4：计算差异并实际同步文件。此阶段只处理两端扫描出的普通文件，
    -- ignore 文件已在上面的阶段 1 处理、软链接已在阶段 2 处理。
    putStrLn "\n[4/4] Syncing files ..."
    -- 计算 + 消解形式冲突（同 size 同整数秒 mtime 的一对直接判作已同步）
    let (pairs1, pairs2) = sync (last1, last2) (cur1, cur2)
        (_, pairs1', pairs2') = resolveConflicts pairs1 pairs2

    let hasWork = any (isJust . snd) pairs1' || any (isJust . snd) pairs2'
    if not hasWork
        then putStrLn "\nNothing to do."
        else withHelpers mHost1 path1 mHost2 path2 profile $ \side1 side2 -> do
                -- 校验钩子（在展示冲突/传输请求与确认**之前**）：冲突的、或要传输的，
                -- 只要两端 size 相同就立刻核对两端内容；结果只有两种——维持，
                -- 或者“只更新属性并记录”（见 'verifySameSizeCandidates'）。
                (pairs1'', pairs2'') <- verifySameSizeCandidates cur1 cur2 side1 side2 pairs1' pairs2'

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

-- | 某个文件在某一侧的当前状态（没扫到就是 Nothing）
stateOf :: [FileState] -> FilePath -> Maybe FileState
stateOf sts name = find ((== name) . filename) sts

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
    :: [FileState] -> [FileState]                -- ^ 两端当前状态（cur1 / cur2）
    -> Side -> Side
    -> [(FileChange, Maybe SyncAction)]          -- ^ pairs1
    -> [(FileChange, Maybe SyncAction)]          -- ^ pairs2
    -> IO ([(FileChange, Maybe SyncAction)], [(FileChange, Maybe SyncAction)])
verifySameSizeCandidates cur1 cur2 side1 side2 pairs1 pairs2 = do
    let names = nub $ [ filename fs | (Added fs _ _, Just Receive)      <- pairs1 ++ pairs2 ]
                    ++ [ filename fs | (Conflict fs _ _, Just Unsolvable) <- pairs1 ++ pairs2 ]
        conflictNames = [ filename fs | (Conflict fs _ _, _) <- pairs1 ++ pairs2 ]
        sameSize n = case (stateOf cur1 n, stateOf cur2 n) of
            (Just a, Just b) -> size a == size b
            _                -> False
        sized = [ (n, if n `elem` conflictNames then CandConflict else CandTransfer)
                | n <- names, sameSize n ]
    if null sized
        then return (pairs1, pairs2)
        else do
            -- 额外内容校验：两端 size 相同的逐个安静校对（一致的同时把属性对齐），
            -- 再把**确实一致**的按来时路分类列出——它们既不是真冲突、也不需要传输。
            putStrLn "\n  Extra content check ..."
            hFlush stdout
            confirmed <- filterM (\(n, _) -> identicalAndAlign cur1 cur2 side1 side2 n) sized
            let same = map fst confirmed
                showGroup lbl ns = unless (null ns) $ do
                    putStrLn $ "  " ++ lbl ++ ":"
                    forM_ ns $ \n -> putStrLn $ "  - " ++ n
            unless (null same) $ do
                showGroup "Not real conflict" [ n | (n, CandConflict) <- confirmed ]
                showGroup "No need transfer" [ n | (n, CandTransfer) <- confirmed ]
            if null same
                then return (pairs1, pairs2)
                else do
                    -- 冲突在两侧各有一条；转换后只让其中一侧发起记录即可
                    -- （'execute' 的 RecordAdd 同时记两端，两边都转会在 added 里留重复行）
                    let conflicts1 = [ filename fs | (Conflict fs _ _, _) <- pairs1 ]
                        pairs1'' = map (markRecordAdd same) pairs1
                        pairs2'' = [ p | p@(fc, _) <- map (markRecordAdd same) pairs2
                                       , not (changeName fc `elem` same
                                              && changeName fc `elem` conflicts1) ]
                    return (pairs1'', pairs2'')

-- | 校验钩子里候选的来源：本来就报冲突的 / 本来要传输的
--   （用来把"校验后一致"的文件分成两组显示：will not conflict / will not transfer）
data CandKind = CandConflict | CandTransfer
    deriving (Eq, Show)

-- | 核对一对文件：内容相同 → 把**较旧那端** mtime 对齐到较新那端并返回 True；
--   内容不同 / 读不到 → 维持原样返回 False。两种都不打印（结果只在摘要里体现）。
identicalAndAlign :: [FileState] -> [FileState] -> Side -> Side -> FilePath -> IO Bool
identicalAndAlign cur1 cur2 side1 side2 name =
    case (stateOf cur1 name, stateOf cur2 name) of
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
markRecordAdd :: [FilePath] -> (FileChange, Maybe SyncAction) -> (FileChange, Maybe SyncAction)
markRecordAdd names (Conflict fs d1 d2, _)
    | filename fs `elem` names = (Noop fs d1 d2, Just RecordAdd)
markRecordAdd names (Added fs d1 d2, _)
    | filename fs `elem` names = (Noop fs d1 d2, Just RecordAdd)
markRecordAdd _ other = other

-- | 取某一端文件内容的 SHA512（本地读字节，远端用 HASH 命令）；读不到返回 Nothing
hashSideContent :: Side -> FilePath -> IO (Maybe String)
hashSideContent (Lcl root) relPath =
    fmap Just (hashBytes <$> BS.readFile (root </> relPath)) `catch` ioFail
  where
    ioFail :: IOException -> IO (Maybe String)
    ioFail _ = return Nothing

hashSideContent (Rmt hh) relPath = do
    res <- expectOK hh ("HASH " ++ relPath)
    return $ case res of
        Right h -> Just h
        Left _  -> Nothing

-- | 把某一端文件的 mtime 对齐到给定秒数（失败只告警：不影响后续传输/记录）
setMtimeOnlySide :: Side -> FilePath -> Integer -> IO ()
setMtimeOnlySide (Lcl root) relPath t = setMtimeOnly (root </> relPath) t
setMtimeOnlySide (Rmt hh) relPath t = do
    res <- expectOK hh ("SETTIME " ++ show t ++ "," ++ relPath)
    case res of
        Left err -> putStrLn $ "  WARNING: SETTIME failed for " ++ relPath ++ ": " ++ err
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
    putStrLn "\n[2/4] Syncing symlinks ..."
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

kindHashCmd, kindSendCmd, kindRecvCmd, kindAddCmd, kindDelCmd :: SyncKind -> String
kindHashCmd SKFile = "HASH"
kindHashCmd SKLink = "LINKHASH"
kindSendCmd SKFile = "SEND"
kindSendCmd SKLink = "LINKSEND"
kindRecvCmd SKFile = "RECV"
kindRecvCmd SKLink = "LINKRECV"
kindAddCmd  SKFile = "ADD"
kindAddCmd  SKLink = "LINKADD"
kindDelCmd  SKFile = "DELETE"
kindDelCmd  SKLink = "LINKDEL"

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
            l <- getHelperLast hh (kindState kind)
            c <- getHelperState hh (kindState kind)
            destroyHelper hh
            return ("ssh " ++ host, (l, c))

-- | 读取对端上次状态（GETLAST / GETLINKLAST：纯数组）
getHelperLast :: HelperHandle -> StateKind -> IO [FileState]
getHelperLast hh kind = do
    result <- readJSON hh (cmdOf kind)
    case result of
        Left err -> error $ cmdOf kind ++ " failed: " ++ err
        Right bs -> return $ parseStateJSON bs
  where
    cmdOf FileKind = "GETLAST"
    cmdOf LinkKind = "GETLINKLAST"

-- | 读取对端当前状态（GETSTATE / GETLINKSTATE：都是纯数组）
getHelperState :: HelperHandle -> StateKind -> IO [FileState]
getHelperState hh kind = do
    result <- readJSON hh (cmdOf kind)
    case result of
        Left err -> error $ cmdOf kind ++ " failed: " ++ err
        Right bs -> return $ parseStateJSON bs
  where
    cmdOf FileKind = "GETSTATE"
    cmdOf LinkKind = "GETLINKSTATE"

-- ============================================================================
-- Helper 管理
-- ============================================================================

spawnHelper :: String -> FilePath -> String -> IO HelperHandle
spawnHelper host path profileName = do
    myExe <- getExecutablePath
    let baseArgs = ["--helper", "--passcode=dontrunyourself", "--root=" ++ path]
        tagArgs  = if null profileName then [] else ["--tag=" ++ profileName]
        args     = baseArgs ++ tagArgs
        cmdArgs = (proc "ssh" (host : myExe : args))
            { std_in  = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    (Just hin, Just hout, _, _) <- createProcess cmdArgs
    hSetEncoding hout utf8
    hSetEncoding hin  utf8
    -- helper 若启动即崩溃（远端缺 hsbisync / locale 非 UTF-8 / ssh 认证失败），
    -- 这里会读到管道 EOF，给出可读错误而不是裸的 "hGetLine: end of file"。
    _ <- hGetLine hout `catch` helperStartFailure host path
    return $ HelperHandle hin hout

-- | 远端 helper 启动失败时的可读诊断
helperStartFailure :: String -> FilePath -> IOException -> IO String
helperStartFailure host path e = error $ unlines
    [ "cannot start remote helper on " ++ host ++ " (root=" ++ path ++ "): " ++ show e
    , "  possible causes:"
    , "    * remote hsbisync missing or not the same version"
    , "    * ssh authentication failed (see ssh output above)"
    , "    * remote locale is not UTF-8 (ssh non-interactive sessions often have LANG unset),"
    , "      making the helper crash while reading UTF-8 state/ignore files"
    ]

destroyHelper :: HelperHandle -> IO ()
destroyHelper hh = do
    ignoreIO $ sendCommand hh "QUIT"
    ignoreIO $ hClose (hhIn hh)
    ignoreIO $ hClose (hhOut hh)

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = return ()

-- ============================================================================
-- 执行操作
-- ============================================================================

-- | 记录类型：ADD / DELETE
--
data Rec = RecAdd | RecDelete

-- | 执行一侧的所有操作（文件/链接阶段共用，动作按 kind 区分）
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
    unless (null removes) $ do
        putStrLn "\nDeleting..."
        forM_ removes $ \(relPath, _, _) -> do
            putStrLn $ "  " ++ kindDeleteVerb kind ++ " " ++ relPath
            delEnd kind self relPath
            appendEnd kind self  mTag RecDelete relPath
            appendEnd kind other mTag RecDelete relPath
    unless (null receives) $ do
        putStrLn "\nReceiving..."
        forM_ receives $ \(relPath, srcMode, srcMtime) -> do
            putStrLn $ "  " ++ kindVerb kind ++ " " ++ relPath
            copyEndToEnd kind other self mTag relPath srcMode srcMtime
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

-- ============================================================================
-- End 操作
-- ============================================================================

delEnd :: SyncKind -> Side -> FilePath -> IO ()
delEnd SKFile (Lcl root) relPath = removeFileSafe (root </> relPath)
delEnd SKFile (Rmt hh)  relPath = do
    res <- expectOK hh ("UNLINK " ++ relPath)
    case res of
        Left err -> error $ "FATAL: remote UNLINK failed: " ++ err
        Right _  -> return ()
-- 链接阶段：只删链接本身（unlink），不跟随到目标；占位不是链接则不动
delEnd SKLink (Lcl root) relPath = do
    ok <- removeLinkSafe (root </> relPath)
    unless ok $ putStrLn $ "  WARNING: not a symlink, left untouched: " ++ relPath
delEnd SKLink (Rmt hh) relPath = do
    res <- expectOK hh ("LINKUNLINK " ++ relPath)
    case res of
        Left err -> error $ "FATAL: remote LINKUNLINK failed: " ++ err
        Right "NOTLINK" ->
            putStrLn $ "  WARNING: not a symlink, left untouched: " ++ relPath
        Right _ -> return ()

appendEnd :: SyncKind -> Side -> Maybe String -> Rec -> FilePath -> IO ()
appendEnd kind (Lcl root) mTag rec relPath =
    case rec of
        RecDelete -> appendDeletedKind (kindState kind) root mTag relPath
        RecAdd    -> appendAddedKind   (kindState kind) root mTag relPath
appendEnd kind (Rmt hh) _mTag rec relPath = do
    let cmd = case rec of RecAdd -> kindAddCmd kind; RecDelete -> kindDelCmd kind
    res <- expectOK hh (cmd ++ " " ++ relPath)
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
    sendCommand hhSrc (kindSendCmd kind ++ " " ++ relPath)
    sz <- readSendSize hhSrc
    hSetBinaryMode (hhOut hhSrc) True
    content <- hGetExactly (hhOut hhSrc) sz
    hSetBinaryMode (hhOut hhSrc) False                    -- 恢复文本模式，供后续行协议读取
    hSetEncoding (hhOut hhSrc) utf8                       -- 显式重设 utf8（C locale 下二进制开关不恢复编码）
    kindWriteLocal kind dstRoot d content srcMode srcMtime expectedHash

transferContent kind (Lcl srcRoot) (Rmt hhDst) relPath srcMode srcMtime = do
    let s = srcRoot </> relPath
    content <- kindReadLocal kind s
    let expectedHash = hashBytes content
    recvVerified kind hhDst (BS.length content) relPath srcMode srcMtime expectedHash content

transferContent kind (Rmt hhSrc) (Rmt hhDst) relPath srcMode srcMtime = do
    expectedHash <- hashRemote kind hhSrc relPath
    sendCommand hhSrc (kindSendCmd kind ++ " " ++ relPath)
    sz <- readSendSize hhSrc
    hSetBinaryMode (hhOut hhSrc) True
    content <- hGetExactly (hhOut hhSrc) sz
    hSetBinaryMode (hhOut hhSrc) False                    -- 恢复文本模式
    hSetEncoding (hhOut hhSrc) utf8                       -- 显式重设 utf8
    recvVerified kind hhDst sz relPath srcMode srcMtime expectedHash content

-- | 向远端 helper 发送 RECV/LINKRECV 命令（扩展协议：带 mode/mtime/expectedHash），
--   传输数据后等待 helper 端完成写临时文件→设属性→校验→rename 并返回结果。
--   链接阶段 mode 不适用（helper 端忽略）。
recvVerified
    :: SyncKind -> HelperHandle -> Int -> FilePath -> Int -> Integer -> String
    -> BS.ByteString -> IO (Either String ())
recvVerified kind hh sz relPath mode_ mtime_ expectedHash content = do
    let cmd = kindRecvCmd kind ++ " " ++ show sz ++ "," ++ relPath
              ++ " " ++ show mode_ ++ " " ++ show mtime_ ++ " " ++ expectedHash
    sendCommand hh cmd
    resp <- readResponse hh                     -- 第一步：helper 是否准备接收
    case resp of
        RErr msg -> return $ Left (kindRecvCmd kind ++ ": " ++ msg)
        ROk _ -> do
            hSetBinaryMode (hhIn hh) True
            hPutFully (hhIn hh) content
            hFlush (hhIn hh)
            hSetBinaryMode (hhIn hh) False       -- 恢复文本模式：后续命令可能含非 ASCII 文件名
            hSetEncoding (hhIn hh) utf8          -- 显式重设 utf8（C locale 下不恢复编码）
            resp2 <- readResponse hh            -- 第二步：校验 + rename 结果
            case resp2 of
                ROk _    -> return $ Right ()
                RErr msg -> return $ Left ("verify: " ++ msg)

-- ============================================================================
-- Helper 协议辅助
-- ============================================================================

readSendSize :: HelperHandle -> IO Int
readSendSize hh = do
    line <- hGetLine (hhOut hh)
    case break (== ' ') line of
        ("OK", rest) -> case reads (dropWhile (== ' ') rest) of
            [(n, "")] -> return n
            _         -> error $ "readSendSize: invalid size: " ++ line
        _ -> error $ "readSendSize: expected OK, got: " ++ line

hashRemote :: SyncKind -> HelperHandle -> FilePath -> IO String
hashRemote kind hh relPath = do
    res <- expectOK hh (kindHashCmd kind ++ " " ++ relPath)
    case res of
        Right s  -> return s
        Left err -> error $ "hashRemote failed: " ++ err

-- ============================================================================
-- 其他
-- ============================================================================

showSummary :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)] -> IO ()
showSummary pairs1 pairs2 = do
    let add1 = [ (f, d1, d2) | (Added    fs d1 d2, Just Receive) <- pairs1, let f = filename fs ]
        del1 = [ (f, d1, d2) | (Del  fs d1 d2, Just Remove)  <- pairs1, let f = filename fs ]
        add2 = [ (f, d1, d2) | (Added    fs d1 d2, Just Receive) <- pairs2, let f = filename fs ]
        del2 = [ (f, d1, d2) | (Del  fs d1 d2, Just Remove)  <- pairs2, let f = filename fs ]
        recAdds = nub [ f | (Noop fs _ _, Just RecordAdd)    <- pairs1 ++ pairs2, let f = filename fs ]
        recDels = nub [ f | (Del  fs _ _, Just RecordDelete) <- pairs1 ++ pairs2, let f = filename fs ]
        conf = nub [ (f, d1, d2) | (Conflict fs d1 d2, Just Unsolvable) <- pairs1 ++ pairs2, let f = filename fs ]

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
                expectOK hh ("PUTREPO " ++ user ++ "," ++ path)
    case r of
        Left err         -> note ("not written (" ++ err ++ ")")
        Right (Left err) -> note ("not written (" ++ err ++ ")")
        Right (Right "NOCLIENT") ->
            note "not written: cannot tell which address the peer sees us as (SSH_CONNECTION unset)"
        Right (Right addr) ->
            putStrLn $ "  " ++ host ++ ":" ++ remotePath ++ ": repo = " ++ addr ++ "  [OK]"
  where
    note msg = putStrLn $ "  NOTE: " ++ host ++ ":" ++ remotePath ++ ": repo " ++ msg

-- | 只捕获"我们自己的"失败（IO 异常 + 我们 error 出来的 ErrorCall），
--   不吞 Ctrl-C 之类的异步异常。Left 是可读的失败原因。
tryOurErrors :: IO a -> IO (Either String a)
tryOurErrors act = (Right <$> act) `catch` ioToLeft `catch` callToLeft
  where
    ioToLeft :: IOException -> IO (Either String a)
    ioToLeft e = return (Left (show e))

    callToLeft :: ErrorCall -> IO (Either String a)
    callToLeft = return . Left . displayException