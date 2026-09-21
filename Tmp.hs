-- | 临时文件的命名约定 —— 只负责"起个可识别的名字 + 原子创建"，别的什么都不做。
--
--   为什么还需要一个模块：原子写入（先写临时文件、校验通过再 rename 到目标名）是
--   不可省的；而临时文件名必须**可识别**，否则中断留下的残骸和真人文件无法区分，
--   你既没法用一条 ignore 规则排除它，也没法用一条 find 手工清掉它。
--
--   **设计取舍（用户 2026-09-22 定）**：工具**不自动清理、也不自动忽略**临时文件。
--   文件树保持真实 —— 扫描看到什么就是什么，只有 ignore 规则能影响同步内容。
--   于是：
--   * 中断留下的临时文件会一直被看见（也可能被同步），这是**已知且接受**的行为；
--   * 想排除它，在你的 ignore 文件里加一行（gitignore 语法，任意层级匹配）：
--         .hsbisync-tmp*
--   * 想清理它，手工跑：
--         find <root> -name '.hsbisync-tmp*' -delete
--
--   生成的名字形如 @.hsbisync-tmp\<pid\>-\<n\>.tmp@：隐藏文件、固定前缀、
--   pid 与序号由 'openTempFile' 填（天然唯一，多进程并发写同一目录也不会撞名）。
module Tmp
    ( createTemp
    ) where

import System.IO (Handle, hSetBinaryMode, openTempFile)

-- | 公共前缀（隐藏文件，不会跟真人文件撞名）
tmpPrefix :: String
tmpPrefix = ".hsbisync-tmp"

-- | 后缀
tmpSuffix :: String
tmpSuffix = ".tmp"

-- | 交给 'openTempFile' 的模板：生成名 = @前缀 ++ \<本进程 pid\> ++ "-" ++ \<序号\> ++ 后缀@
tmpTemplate :: String
tmpTemplate = tmpPrefix ++ tmpSuffix

-- | 原子创建临时文件（@O_EXCL@：同名已存在时 openTempFile 会自己换名字重试）。
--   属性（mode/mtime）由调用方在 rename 之前设好，这里只负责名字与独占创建。
createTemp :: FilePath -> IO (FilePath, Handle)
createTemp dir = do
    (path, h) <- openTempFile dir tmpTemplate
    hSetBinaryMode h True
    return (path, h)
