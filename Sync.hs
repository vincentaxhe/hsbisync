module Sync where

import Data.List (nub, find)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import State (FileState(..), FileChange(..), DiffState(..), sameFile)

-- | 同步操作
data SyncAction = Receive | Remove | Unsolvable | RecordAdd | RecordDelete
    deriving (Show, Eq)

-- | 双向同步核心逻辑
--
--   返回两侧的 @(FileChange, Maybe SyncAction)@ 列表
--   * @Nothing@ = 无需操作
--   * @Just Receive@ = 从对方接收
--   * @Just Delete@ = 删除本地
--   * @Just Unsolvable@ = 冲突
sync :: ([FileState], [FileState]) -> ([FileState], [FileState])
     -> ([(FileChange, Maybe SyncAction)], [(FileChange, Maybe SyncAction)])
sync (last1, last2) (cur1, cur2) =
    let allFiles = nub $ map filename last1 ++ map filename last2
                      ++ map filename cur1 ++ map filename cur2
        lm1 = lookupFS last1; lm2 = lookupFS last2
        cm1 = lookupFS cur1;   cm2 = lookupFS cur2
        results = map (classifyOne lm1 lm2 cm1 cm2) allFiles
        changes1 = [c1 | (c1, _) <- results]
        changes2 = [c2 | (_, c2) <- results]
    in ( zip changes1 (map toAct changes1)
       , zip changes2 (map toAct changes2) )

-- | 消解形式冲突 → 实质冲突
--
--   可自动消解的三类：
--   * New+New 或 Update+Update 且内容相同 → RecordAdd（不传输只 ADD）
--   * Delete+Delete → RecordDelete（不删文件只 DELETE）
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

        -- New/Update + sameFile → RecordAdd
        resolvedAdd = [ (name, fs1)
                      | ((Conflict fs1 d1 _), fs1') <- conflicts1, let name = filename fs1
                      , d1 `elem` [New, Update]
                      , (Conflict fs2 d2 _, _) <- conflicts2
                      , filename fs2 == name, d2 `elem` [New, Update]
                      , sameFile fs1' fs2
                      ]

        -- Delete+Delete → RecordDelete
        resolvedDel = [ name
                      | (Conflict fs1 Delete Delete, _) <- conflicts1, let name = filename fs1
                      , (Conflict fs2 Delete Delete, _) <- conflicts2
                      , filename fs2 == name
                      ]

        resolvedNames = map fst resolvedAdd ++ resolvedDel
        keepInPairs2 (Conflict fs _ _, _) = filename fs `notElem` resolvedNames
        keepInPairs2 _                    = True

        convert ((Conflict fs d1 d2), _)
            | filename fs `elem` resolvedDel                                      = (Deleted fs d1 d2, Just RecordDelete)
            | filename fs `elem` map fst resolvedAdd, d1 `elem` [New, Update]     = (Unchanged fs d1 d2, Just RecordAdd)
        convert other = other

    in (resolvedAdd, map convert pairs1, filter keepInPairs2 pairs2)
toAct :: FileChange -> Maybe SyncAction
toAct (Unchanged _ _ _)  = Nothing
toAct (Added     _ _ _)  = Just Receive
toAct (Deleted   _ _ _)  = Just Remove
toAct (Conflict  _ _ _)  = Just Unsolvable
classifyOne
    :: (FilePath -> Maybe FileState) -> (FilePath -> Maybe FileState)
    -> (FilePath -> Maybe FileState) -> (FilePath -> Maybe FileState)
    -> FilePath -> (FileChange, FileChange)
classifyOne lm1 lm2 cm1 cm2 name =
    let nop  = Unchanged dummy None None
    in case (lm1 name, lm2 name, cm1 name, cm2 name) of

        --- 都不存在
        (Nothing, Nothing, Nothing, Nothing) -> (nop, nop)

        ------------------------------------------------------------
        -- 仅 root1 变化 → root2 需行动
        ------------------------------------------------------------

        -- root1 新增，root2 无变化
        (Nothing, Nothing, Just c1, Nothing) ->
            ( Unchanged c1 New None
            , Added     c1 New None )

        -- root1 修改（双方 last 一致），root2 未变
        (Just l1, Just l2, Just c1, Just c2)
            | sameFile l1 l2 && not (sameFile c1 l1) && sameFile c2 l2 ->
            ( Unchanged c1 Update None
            , Added     c1 Update None )

        -- root1 修改（仅 root1 有 last），root2 无
        (Just l1, Nothing, Just c1, Nothing) | not (sameFile c1 l1) ->
            ( Unchanged c1 Update None
            , Added     c1 Update None )

        -- root1 未变，root2 无记录 → root2 应接收
        (Just l1, Nothing, Just c1, Nothing) | sameFile c1 l1 ->
            ( Unchanged c1 None None
            , Added     c1 None New )

        -- root1 删除，root2 未变
        (Just l1, Just l2, Nothing, Just c2) | sameFile l1 l2 && sameFile c2 l2 ->
            ( Unchanged c2 Delete None
            , Deleted   c2 Delete None )

        -- root2 删除，root1 未变
        (Just l1, Just l2, Just c1, Nothing) | sameFile l1 l2 && sameFile c1 l1 ->
            ( Deleted   c1 None Delete
            , Unchanged c1 None Delete )

        -- root1 新增，root2 曾经有但删了 → conflict
        (Nothing, Just _, Just c1, Nothing) ->
            ( Unchanged c1 New Delete
            , Conflict  c1 New Delete )

        ------------------------------------------------------------
        -- 仅 root2 变化 → root1 需行动（对称）
        ------------------------------------------------------------

        (Nothing, Nothing, Nothing, Just c2) ->
            ( Added     c2 None New
            , Unchanged c2 None New )

        (Just l1, Just l2, Just c1, Just c2) | sameFile l1 l2 && sameFile c1 l1 && not (sameFile c2 l2) ->
            ( Added     c2 None Update
            , Unchanged c2 None Update )

        (Nothing, Just l2, Nothing, Just c2) | not (sameFile c2 l2) ->
            ( Added     c2 None Update
            , Unchanged c2 None Update )

        (Just l1, Just l2, Just c1, Nothing) | sameFile l1 l2 && sameFile c1 l1 ->
            ( Deleted   c1 None Delete
            , Unchanged c1 None Delete )

        (Just _, Nothing, Nothing, Just c2) ->
            ( Conflict  c2 Delete New
            , Unchanged c2 Delete New )

        ------------------------------------------------------------
        -- 双方都有变化
        ------------------------------------------------------------

        -- 两边都 new → 冲突（执行阶段比较后相同的可自动消解）
        (Nothing, Nothing, Just c1, Just c2) ->
            ( Conflict c1 New New
            , Conflict c2 New New )

        -- 两边都 update → 冲突（执行阶段比较后相同的可自动消解）
        (Just l1, Just l2, Just c1, Just c2) | sameFile l1 l2 && not (sameFile c1 l1) && not (sameFile c2 l2) ->
            ( Conflict c1 Update Update
            , Conflict c2 Update Update )

        -- 两边都 delete → 形式冲突，可消解为 RecordDelete
        (Just _, Just _, Nothing, Nothing) ->
            ( Conflict dummy Delete Delete
            , Conflict dummy Delete Delete )

        (Just l1, Just l2, Nothing, Just c2) | sameFile l1 l2 && not (sameFile c2 l2) ->
            ( Conflict c2 Delete Update
            , Conflict c2 Delete Update )

        (Just l1, Just l2, Just c1, Nothing) | sameFile l1 l2 && not (sameFile c1 l1) ->
            ( Conflict c1 Update Delete
            , Conflict c1 Update Delete )

        ------------------------------------------------------------
        -- 都没变
        ------------------------------------------------------------
        (Just _, Just _, Just c1, Just c2) | sameFile c1 c2 ->
            ( Unchanged c1 None None
            , Unchanged c2 None None )

        -- 兜底
        _ -> (nop, nop)

  where dummy = FileState 0 (posixSecondsToUTCTime 0) 0 name

lookupFS :: [FileState] -> FilePath -> Maybe FileState
lookupFS fs name = find (\f -> filename f == name) fs
