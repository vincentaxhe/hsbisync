module Sync where

import Data.List (nub, find)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import State (FileState(..), FileChange(..), DiffState(..), sameFile, diffOneSide)

-- | 同步操作
data SyncAction = Receive | Remove | Unsolvable | RecordAdd | RecordDelete
    deriving (Show, Eq)

-- | 双向同步核心逻辑
--
--   先对两侧各自调用 'diffOneSide' 计算单侧差异（New / Updated / Unchanged / Deleted），
--   再在 'classifyOne' 中组合两侧的差异，产生每侧的 FileChange。
--   None 表示该侧从未有过此文件，仅出现在 classifyOne 的输出中。
--
--   返回两侧的 @(FileChange, Maybe SyncAction)@ 列表
--   * @Nothing@ = 无需操作
--   * @Just Receive@ = 从对方接收
--   * @Just Deleted@ = 删除本地
--   * @Just Unsolvable@ = 冲突
sync :: ([FileState], [FileState]) -> ([FileState], [FileState])
     -> ([(FileChange, Maybe SyncAction)], [(FileChange, Maybe SyncAction)])
sync (last1, last2) (cur1, cur2) =
    let diff1 = diffOneSide last1 cur1
        diff2 = diffOneSide last2 cur2
        dm1 = lookupDiff diff1
        dm2 = lookupDiff diff2
        allFiles = nub $ map (filename . snd) diff1 ++ map (filename . snd) diff2
        results = map (classifyOne dm1 dm2) allFiles
        changes1 = [c1 | (c1, _) <- results]
        changes2 = [c2 | (_, c2) <- results]
    in ( zip changes1 (map toAct changes1)
       , zip changes2 (map toAct changes2) )

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

        -- New/Updated + sameFile → RecordAdd
        resolvedAdd = [ (name, fs1)
                      | ((Conflict fs1 d1 _), fs1') <- conflicts1, let name = filename fs1
                      , d1 `elem` [New, Updated]
                      , (Conflict fs2 d2 _, _) <- conflicts2
                      , filename fs2 == name, d2 `elem` [New, Updated]
                      , sameFile fs1' fs2
                      ]

        -- Deleted+Deleted → RecordDelete
        resolvedDel = [ name
                      | (Conflict fs1 Deleted Deleted, _) <- conflicts1, let name = filename fs1
                      , (Conflict fs2 Deleted Deleted, _) <- conflicts2
                      , filename fs2 == name
                      ]

        resolvedNames = map fst resolvedAdd ++ resolvedDel
        keepInPairs2 (Conflict fs _ _, _) = filename fs `notElem` resolvedNames
        keepInPairs2 _                    = True

        convert ((Conflict fs d1 d2), _)
            | filename fs `elem` resolvedDel                                      = (Del fs d1 d2, Just RecordDelete)
            | filename fs `elem` map fst resolvedAdd, d1 `elem` [New, Updated]     = (Noop fs d1 d2, Just RecordAdd)
        convert other = other

    in (resolvedAdd, map convert pairs1, filter keepInPairs2 pairs2)
toAct :: FileChange -> Maybe SyncAction
toAct (Noop _ _ _)  = Nothing
toAct (Added     _ _ _)  = Just Receive
toAct (Del   _ _ _)  = Just Remove
toAct (Conflict  _ _ _)  = Just Unsolvable
-- | 从 diff 列表构建文件名 → Maybe (DiffState, FileState) 的查找函数
lookupDiff :: [(DiffState, FileState)] -> FilePath -> Maybe (DiffState, FileState)
lookupDiff diffs name = find (\(_, fs) -> filename fs == name) diffs

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
        (Just (Deleted, _),    Nothing) -> (nop, nop)

        ------------------------------------------------------------
        -- 仅 side2 有变化 → side1 需行动（对称）
        ------------------------------------------------------------
        (Nothing, Just (New,    fs2)) -> (Added     fs2 None New,    Noop fs2 None New)
        (Nothing, Just (Updated, fs2)) -> (Added     fs2 None Updated, Noop fs2 None Updated)
        (Nothing, Just (Unchanged,   fs2)) -> (Added     fs2 New Unchanged,      Noop fs2 None Unchanged)
        (Nothing, Just (Deleted, _))    -> (nop, nop)

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
