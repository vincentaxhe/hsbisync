{-# LANGUAGE BangPatterns #-}
module Sync where

import qualified Data.Map as Map
import qualified Data.Set as Set
import State (FileState(..), FileChange(..), SideState(..), sameFile, diffOneSide, dedup)

-- | 同步操作
data SyncAction = Receive | Remove | Unsolvable | RecordAdd | RecordDelete
    deriving (Show, Eq)

-- | 单侧 diff 的**初步统计**（给输出的第 5 步用）：每种状态各多少条。
--
--   'SideState' 共 5 种，但 'Absent' 是"该侧从未有过"——它不是这一侧的
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
diffStats :: [SideState] -> SideStats
diffStats = foldl' step (SideStats 0 0 0 0)
  where
    step !acc st = case st of
        SNew       _ -> acc { statNew       = statNew acc + 1 }
        SUpdated   _ -> acc { statUpdated   = statUpdated acc + 1 }
        SUnchanged _ -> acc { statUnchanged = statUnchanged acc + 1 }
        SDeleted   _ -> acc { statDeleted   = statDeleted acc + 1 }
        Absent     _ -> acc

-- | 双向同步核心逻辑
--
--   先对两侧各自调用 'diffOneSide' 计算单侧差异（'SNew' / 'SUpdated' / 'SUnchanged' / 'SDeleted'），
--   再在 'classifyOne' 中组合两侧的差异，产生每侧的 FileChange。
--   'Absent' 表示该侧从未有过此文件，由 'classifyOne' 在查不到文件名时自然产生。
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
        dm1 = indexSideStates diff1
        dm2 = indexSideStates diff2
        lookupSide dm name = case Map.lookup name dm of
            Just ss -> ss
            Nothing -> Absent (dummyFS name)
        -- 顺序保持的去重（每个名字只在 root1 优先的位置出现一次）
        allFiles = dedup (map sideStateName diff1 ++ map sideStateName diff2)
        results = map (\name -> classifyOne (lookupSide dm1 name) (lookupSide dm2 name)) allFiles
        changes1 = [c1 | (c1, _) <- results]
        changes2 = [c2 | (_, c2) <- results]
    in ( diffStats diff1, zip changes1 (map toAct changes1)
       , diffStats diff2, zip changes2 (map toAct changes2) )

-- | SideState 列表按文件名索引（同名重复保留先出现的）
indexSideStates :: [SideState] -> Map.Map FilePath SideState
indexSideStates = Map.fromListWith (\_ old -> old) . map (\ss -> (sideStateName ss, ss))

-- | 为 Absent 构造占位 FileState（size=0, mtime=epoch, mode=0）
dummyFS :: FilePath -> FileState
dummyFS name = FileState 0 (read "1970-01-01 00:00:00 UTC") 0 name

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

        -- root2 侧索引：文件名 → (本侧 SideState, 本侧 FileState)
        -- （`Conflict fs _ d2` 里 fs 与元组第二项同一个值，所以不再重复取出）
        m2 = Map.fromListWith (\_ old -> old)
                 [ (filename fs, (d2, fs)) | (Conflict fs _ d2, _) <- conflicts2 ]

        -- SNew/SUpdated/SUnchanged + sameFile → RecordAdd
        --   * New+New / Updated+Updated / New+Updated → 双方内容相同则 ADD
        --   * Unchanged+New / New+Unchanged → 一侧未变一侧新增，
        --     内容相同则在新侧记 ADD（消解为已同步）
        --
        -- 注意：每个 Conflict 都携带 (root1状态, root2状态) 两个 SideState。
        -- d1 来自 pairs1（root1 侧），d2 必须来自 pairs2（root2 侧），
        -- 不能两边都取第一字段，否则 Unchanged+New 会被误判成两边都 Unchanged。
        resolvedAdd = [ (name, fs1)
                      | (Conflict fs1 d1 _, _) <- conflicts1, let name = filename fs1
                      , isPresent d1
                      , Just (d2, fs2) <- [Map.lookup name m2]
                      , isPresent d2
                      , not (isUnchanged d1 && isUnchanged d2)
                      , sameFile fs1 fs2
                      ]

        -- SDeleted+SDeleted → RecordDelete
        resolvedDel = [ name
                      | (Conflict fs1 (SDeleted _) (SDeleted _), _) <- conflicts1, let name = filename fs1
                      , Just (SDeleted _, _) <- [Map.lookup name m2]
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
        convert1 (Noop fs (SDeleted _) (Absent _), Nothing) = (Del fs (SDeleted fs) (Absent fs), Just RecordDelete)
        convert1 (Conflict fs d1 d2, _)
            | Set.member (filename fs) delNames = (Del fs d1 d2, Just RecordDelete)
            | Set.member (filename fs) addNames = (Noop fs d1 d2, Just RecordAdd)
        convert1 other = other

        -- pairs2 同上（对称）
        convert2 (Noop fs (Absent _) (SDeleted _), Nothing) = (Del fs (Absent fs) (SDeleted fs), Just RecordDelete)
        convert2 other = other

    in (resolvedAdd, map convert1 pairs1, map convert2 (filter keepInPairs2 pairs2))

isPresent :: SideState -> Bool
isPresent (SNew _)      = True
isPresent (SUpdated _)  = True
isPresent (SUnchanged _) = True
isPresent _             = False

isUnchanged :: SideState -> Bool
isUnchanged (SUnchanged _) = True
isUnchanged _              = False
toAct :: FileChange -> Maybe SyncAction
toAct (Noop _ _ _)  = Nothing
toAct (Added     _ _ _)  = Just Receive
toAct (Del   _ _ _)  = Just Remove
toAct (Conflict  _ _ _)  = Just Unsolvable

-- | 从 SideState 提取文件名
sideStateName :: SideState -> FilePath
sideStateName ss = case ss of
    Absent fs     -> filename fs
    SNew fs       -> filename fs
    SUpdated fs   -> filename fs
    SUnchanged fs -> filename fs
    SDeleted fs   -> filename fs

-- | 分类单个文件的双侧状态，产生两侧各自的 FileChange
--
--   输入是两侧各自的 'SideState'（精确 5 种）：
--   * 'Absent'      = 该侧从未有过此文件
--   * 'SNew'        = 新增
--   * 'SUpdated'    = 修改
--   * 'SUnchanged'  = 未变
--   * 'SDeleted'    = 删除
--
--   5×5 = 25 种组合全部显式枚举，无 catch-all，编译器验证穷尽性。
classifyOne
    :: SideState -> SideState -> (FileChange, FileChange)
classifyOne s1 s2 = case (s1, s2) of

    -- 都不存在
    (Absent fs1, Absent fs2) -> (Noop fs1 s1 s2, Noop fs2 s1 s2)

    ------------------------------------------------------------
    -- 仅 side1 有变化 → side2 需行动
    ------------------------------------------------------------
    (SNew fs1,      Absent fs2) -> (Noop fs1 s1 s2,    Added fs2 s1 s2)
    (SUpdated fs1,  Absent fs2) -> (Noop fs1 s1 s2,    Added fs2 s1 s2)
    (SUnchanged fs1,Absent fs2) -> (Noop fs1 s1 s2,    Added fs2 s1 s2)

    -- side1 的基线里有、现在扫不到了，而 side2 **从未有过**这个条目：
    -- 没有任何东西需要传播，但 side1 那条**过期的基线记录必须清掉**，否则每轮
    -- 都会报一次 `deleted 1` 却没有任何动作（`Nothing to do.`）——永不自愈。
    -- 实际碰到过：条目当初是"指向文件的链接"（按文件记录），后来改成"指向目录的
    -- 链接"，文件阶段只把后者当目录（不记条目）→ 从此永远"删除"。
    -- 编码成 @Noop fs1 (SDeleted fs1) (Absent fs2)@，由 'resolveConflicts' 转成 RecordDelete
    -- （纯记录：只改状态，**不动文件**）。
    (SDeleted fs1, Absent fs2) -> (Noop fs1 s1 s2, Noop fs2 s1 s2)

    ------------------------------------------------------------
    -- 仅 side2 有变化 → side1 需行动（对称）
    ------------------------------------------------------------
    (Absent fs1, SNew fs2)      -> (Added fs1 s1 s2, Noop fs2 s1 s2)
    (Absent fs1, SUpdated fs2)  -> (Added fs1 s1 s2, Noop fs2 s1 s2)
    (Absent fs1, SUnchanged fs2)-> (Added fs1 s1 s2, Noop fs2 s1 s2)

    -- 同上（对称）：side2 的过期条目，由 'resolveConflicts' 转成 RecordDelete
    (Absent fs1, SDeleted fs2)  -> (Noop fs1 s1 s2, Noop fs2 s1 s2)

    ------------------------------------------------------------
    -- 双方都有变化
    ------------------------------------------------------------
    (SNew fs1,      SNew fs2)       -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SNew fs1,      SUpdated fs2)   -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SNew fs1,      SUnchanged fs2) -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SNew fs1,      SDeleted fs2)   -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)

    (SUpdated fs1,  SNew fs2)       -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SUpdated fs1,  SUpdated fs2)   -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SUpdated fs1,  SUnchanged fs2) -> (Noop fs1 s1 s2,     Added fs2 s1 s2)
    (SUpdated fs1,  SDeleted fs2)   -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)

    (SUnchanged fs1, SNew fs2)       -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SUnchanged _, SUpdated fs2)   -> (Added fs2 s1 s2,    Noop fs2 s1 s2)
    (SUnchanged fs1, SUnchanged fs2) -> (Noop fs1 s1 s2,     Noop fs2 s1 s2)
    (SUnchanged fs1, SDeleted fs2)   -> (Del fs1 s1 s2,      Noop fs2 s1 s2)

    (SDeleted fs1, SNew fs2)       -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SDeleted fs1, SUpdated fs2)   -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
    (SDeleted _, SUnchanged fs2) -> (Noop fs2 s1 s2,     Del fs2 s1 s2)
    (SDeleted fs1, SDeleted fs2)   -> (Conflict fs1 s1 s2, Conflict fs2 s1 s2)
