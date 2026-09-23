module Sync where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import State (FileState(..), FileChange(..), DiffState(..), sameFile, diffOneSide, dedup)

-- | 同步操作
data SyncAction = Receive | Remove | Unsolvable | RecordAdd | RecordDelete
    deriving (Show, Eq)

-- | 单侧 diff 的**初步统计**（给输出的第 5 步用）：每种状态各多少条。
--
--   'DiffState' 共 5 种，但 'None' 是"该侧从未有过"——它不是这一侧的
--   **自身状态变化**（不在 'diffOneSide' 的输出里），所以这里只有 4 个计数。
data SideStats = SideStats
    { statNew       :: !Int
    , statUpdated   :: !Int
    , statUnchanged :: !Int
    , statDeleted   :: !Int
    }
    deriving (Show, Eq)

-- | 统计一份 diff（'diffOneSide' 的输出）里各状态条数
--
--   一次线性遍历（严格累加，避免大列表堆 thunk）。
diffStats :: [(DiffState, FileState)] -> SideStats
diffStats = foldl' step (SideStats 0 0 0 0)
  where
    step (SideStats n u k d) (st, _) = case st of
        New       -> SideStats (n + 1) u k d
        Updated   -> SideStats n (u + 1) k d
        Unchanged -> SideStats n u (k + 1) d
        Deleted   -> SideStats n u k (d + 1)
        None      -> SideStats n u k d

-- | 双向同步核心逻辑
--
--   先对两侧各自调用 'diffOneSide' 计算单侧差异（New / Updated / Unchanged / Deleted），
--   再在 'classifyOne' 中组合两侧的差异，产生每侧的 FileChange。
--   None 表示该侧从未有过此文件，仅出现在 classifyOne 的输出中。
--
--   返回：两端各自的 @('SideStats', [(FileChange, Maybe SyncAction)])@ ——
--   统计是**初步**的（diff 的直接结果，冲突消解与内容校验之前），
--   @Just Receive@ = 从对方接收，@Just Remove@ = 删除本地，@Just Unsolvable@ = 冲突。
sync :: ([FileState], [FileState]) -> ([FileState], [FileState])
     -> ( SideStats, [(FileChange, Maybe SyncAction)]    -- ^ root1：初步统计 + 动作
        , SideStats, [(FileChange, Maybe SyncAction)] )  -- ^ root2：初步统计 + 动作
sync (last1, last2) (cur1, cur2) =
    let diff1 = diffOneSide last1 cur1
        diff2 = diffOneSide last2 cur2
        dm1 = indexDiffs diff1
        dm2 = indexDiffs diff2
        -- 顺序保持的去重（每个名字只在 root1 优先的位置出现一次）
        allFiles = dedup (map (filename . snd) diff1 ++ map (filename . snd) diff2)
        results = map (classifyOne (flip Map.lookup dm1) (flip Map.lookup dm2)) allFiles
        changes1 = [c1 | (c1, _) <- results]
        changes2 = [c2 | (_, c2) <- results]
    in ( diffStats diff1, zip changes1 (map toAct changes1)
       , diffStats diff2, zip changes2 (map toAct changes2) )

-- | 消解形式冲突 → 实质冲突
--
--   可自动消解的三类：
--   * New+New 或 Updated+Updated 且内容相同 → RecordAdd（不传输只 ADD）
--   * Deleted+Deleted → RecordDelete（不删文件只 DELETE）
--   其余 Conflict 保留为 Unsolvable。
resolveConflicts
    :: [(FileChange, Maybe SyncAction)] -> [(FileChange, Maybe SyncAction)]
    -> ( [(FilePath, FileState)]             -- ^ 可自动消解（供 summary 展示）
       , [(FileChange, Maybe SyncAction)]    -- ^ pairs1：消解的 → Record
       , [(FileChange, Maybe SyncAction)]    -- ^ pairs2：消解的 → 移除
       )
resolveConflicts pairs1 pairs2 =
    let conflicts1 = [(fc, fs) | (fc@(Conflict fs _ _), _) <- pairs1]
        conflicts2 = [(fc, fs) | (fc@(Conflict fs _ _), _) <- pairs2]

        -- root2 侧索引：文件名 → (本侧 DiffState, 本侧 FileState)
        -- （`Conflict fs _ d2` 里 fs 与元组第二项同一个值，所以不再重复取出）
        m2 = Map.fromListWith (\_ old -> old)
                 [ (filename fs, (d2, fs)) | (Conflict fs _ d2, _) <- conflicts2 ]

        -- New/Updated/Unchanged + sameFile → RecordAdd
        --   * New+New / Updated+Updated / New+Updated → 双方内容相同则 ADD
        --   * Unchanged+New / New+Unchanged → 一侧未变一侧新增，
        --     内容相同则在新侧记 ADD（消解为已同步）
        --
        -- 注意：每个 Conflict 都携带 (root1状态, root2状态) 两个 DiffState。
        -- d1 来自 pairs1（root1 侧），d2 必须来自 pairs2（root2 侧），
        -- 不能两边都取第一字段，否则 Unchanged+New 会被误判成两边都 Unchanged。
        resolvedAdd = [ (name, fs1)
                      | (Conflict fs1 d1 _, _) <- conflicts1, let name = filename fs1
                      , d1 `elem` [New, Updated, Unchanged]
                      , Just (d2, fs2) <- [Map.lookup name m2]
                      , d2 `elem` [New, Updated, Unchanged]
                      , not (d1 == Unchanged && d2 == Unchanged)
                      , sameFile fs1 fs2
                      ]

        -- Deleted+Deleted → RecordDelete
        resolvedDel = [ name
                      | (Conflict fs1 Deleted Deleted, _) <- conflicts1, let name = filename fs1
                      , Just (Deleted, _) <- [Map.lookup name m2]
                      ]

        addNames = Set.fromList (map fst resolvedAdd)
        delNames = Set.fromList resolvedDel
        resolvedNames = Set.union addNames delNames
        keepInPairs2 (Conflict fs _ _, _) = not (Set.member (filename fs) resolvedNames)
        keepInPairs2 _                    = True

        -- pairs1：过期条目（本端删了、对端从未有过）→ 纯记录
        --
        --   这里**绝不能**变成 'Remove'：那个路径如今可能是指向别处的链接
        --   （实测就是"指向目录的链接"），删下去会穿透链接动到真数据。
        --   'RecordDelete' 只往 deleted 文件里写一行，下轮读状态时把该条目滤掉。
        convert1 (Noop fs Deleted None, Nothing) = (Del fs Deleted None, Just RecordDelete)
        convert1 (Conflict fs d1 d2, _)
            | Set.member (filename fs) delNames = (Del fs d1 d2, Just RecordDelete)
            | Set.member (filename fs) addNames = (Noop fs d1 d2, Just RecordAdd)
        convert1 other = other

        -- pairs2 同上（对称）
        convert2 (Noop fs None Deleted, Nothing) = (Del fs None Deleted, Just RecordDelete)
        convert2 other = other

    in (resolvedAdd, map convert1 pairs1, map convert2 (filter keepInPairs2 pairs2))
toAct :: FileChange -> Maybe SyncAction
toAct (Noop _ _ _)  = Nothing
toAct (Added     _ _ _)  = Just Receive
toAct (Del   _ _ _)  = Just Remove
toAct (Conflict  _ _ _)  = Just Unsolvable
-- | 单侧 diff 的索引：文件名 → (DiffState, FileState)（同名重复保留先出现的）
indexDiffs :: [(DiffState, FileState)] -> Map.Map FilePath (DiffState, FileState)
indexDiffs = Map.fromListWith (\_ old -> old) . map (\(d, fs) -> (filename fs, (d, fs)))

-- | 分类单个文件的双侧状态，产生两侧各自的 FileChange
--
--   输入是两侧各自通过 'diffOneSide' 预计算出的 diff 查找函数：
--   * @Nothing@          = 该侧从未有过此文件
--   * @Just (New, fs)@     = 新增
--   * @Just (Updated, fs)@  = 修改
--   * @Just (Unchanged, fs)@    = 未变
--   * @Just (Deleted, fs)@  = 删除
--
--   5×5 = 25 种组合全部显式枚举，编译器可验证穷尽性。
classifyOne
    :: (FilePath -> Maybe (DiffState, FileState))
    -> (FilePath -> Maybe (DiffState, FileState))
    -> FilePath -> (FileChange, FileChange)
classifyOne dm1 dm2 name =
    let nop = Noop dummy None None
    in case (dm1 name, dm2 name) of

        -- 都不存在
        (Nothing, Nothing) -> (nop, nop)

        ------------------------------------------------------------
        -- 仅 side1 有变化 → side2 需行动
        ------------------------------------------------------------
        (Just (New,    fs1), Nothing) -> (Noop fs1 New None,    Added     fs1 New None)
        (Just (Updated, fs1), Nothing) -> (Noop fs1 Updated None, Added     fs1 Updated None)
        (Just (Unchanged,   fs1), Nothing) -> (Noop fs1 Unchanged None,   Added     fs1 Unchanged New)

        -- side1 的基线里有、现在扫不到了，而 side2 **从未有过**这个条目：
        -- 没有任何东西需要传播，但 side1 那条**过期的基线记录必须清掉**，否则每轮
        -- 都会报一次 `deleted 1` 却没有任何动作（`Nothing to do.`）——永不自愈。
        -- 实际碰到过：条目当初是"指向文件的链接"（按文件记录），后来改成"指向目录的
        -- 链接"，文件阶段只把后者当目录（不记条目）→ 从此永远"删除"。
        -- 编码成 @Noop fs1 Deleted None@，由 'resolveConflicts' 转成 RecordDelete
        -- （纯记录：只改状态，**不动文件**）。
        (Just (Deleted, fs1), Nothing) -> (Noop fs1 Deleted None, nop)

        ------------------------------------------------------------
        -- 仅 side2 有变化 → side1 需行动（对称）
        ------------------------------------------------------------
        (Nothing, Just (New,    fs2)) -> (Added     fs2 None New,    Noop fs2 None New)
        (Nothing, Just (Updated, fs2)) -> (Added     fs2 None Updated, Noop fs2 None Updated)
        (Nothing, Just (Unchanged,   fs2)) -> (Added     fs2 New Unchanged,      Noop fs2 None Unchanged)

        -- 同上（对称）：side2 的过期条目，由 'resolveConflicts' 转成 RecordDelete
        (Nothing, Just (Deleted, fs2))  -> (nop, Noop fs2 None Deleted)

        ------------------------------------------------------------
        -- 双方都有变化
        ------------------------------------------------------------
        (Just (New,    fs1), Just (New,    fs2)) -> (Conflict fs1 New New,      Conflict fs2 New New)
        (Just (New,    fs1), Just (Updated, fs2)) -> (Conflict fs1 New Updated,    Conflict fs2 New Updated)
        (Just (New,    fs1), Just (Unchanged,   fs2)) -> (Conflict fs1 New Unchanged,      Conflict fs2 New Unchanged)
        (Just (New,    fs1), Just (Deleted, fs2)) -> (Conflict fs1 New Deleted,    Conflict fs2 New Deleted)

        (Just (Updated, fs1), Just (New,    fs2)) -> (Conflict fs1 Updated New,    Conflict fs2 Updated New)
        (Just (Updated, fs1), Just (Updated, fs2)) -> (Conflict fs1 Updated Updated, Conflict fs2 Updated Updated)
        (Just (Updated, fs1), Just (Unchanged,   _))   -> (Noop fs1 Updated Unchanged,  Added     fs1 Updated Unchanged)
        (Just (Updated, fs1), Just (Deleted, fs2)) -> (Conflict fs1 Updated Deleted, Conflict fs2 Updated Deleted)

        (Just (Unchanged,   fs1), Just (New,    fs2)) -> (Conflict fs1 Unchanged New,      Conflict fs2 Unchanged New)
        (Just (Unchanged,   _),   Just (Updated, fs2)) -> (Added     fs2 Unchanged Updated,  Noop fs2 Unchanged Updated)
        (Just (Unchanged,   fs1), Just (Unchanged,   fs2)) -> (Noop fs1 Unchanged Unchanged,    Noop fs2 Unchanged Unchanged)
        (Just (Unchanged,   fs1), Just (Deleted, _))   -> (Del   fs1 Unchanged Deleted,  Noop fs1 Unchanged Deleted)

        (Just (Deleted, fs1), Just (New,    fs2)) -> (Conflict fs1 Deleted New,    Conflict fs2 Deleted New)
        (Just (Deleted, fs1), Just (Updated, fs2)) -> (Conflict fs1 Deleted Updated, Conflict fs2 Deleted Updated)
        (Just (Deleted, _),   Just (Unchanged,   fs2)) -> (Noop fs2 Deleted Unchanged,  Del   fs2 Deleted Unchanged)
        (Just (Deleted, _),   Just (Deleted, _))   -> (Conflict dmy Deleted Deleted, Conflict dmy Deleted Deleted)

        -- diffOneSide 永不返回 None，此分支仅满足 exhaustiveness 检查
        _ -> error $ "classifyOne: unexpected state for " ++ name
  where
    dummy = FileState 0 (posixSecondsToUTCTime 0) 0 name
    dmy   = dummy
