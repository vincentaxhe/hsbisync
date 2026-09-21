-- | 临时文件：**一条命名约定 + 一个"没确认成功就删"的规则**（刻意保持最小）。
--
--   1. **名字**：@.hsbisync-tmp\<pid\>-\<n\>.tmp@（隐藏 + 固定前缀，一眼认出是我们的）。
--      老写法 @openTempFile dir ".tmp"@ 生成的是 @\<pid\>-\<n\>.tmp@，跟真人文件无法区分。
--   2. **生命周期**：临时文件只有一个成功出口 = 校验通过后 rename 到目标名；
--      其它任何情况（校验不过、写失败、Ctrl-C、断线）都走 'Write.withTemp' 的
--      @onException@ 把它删掉 —— 就是"没确认成功就删"，不留半成品。
--
--   扫描端只认一条：**我们的临时文件不是内容**（'isTempName'），永不参与同步。
--   这不是"多一层防护"，而是"内容"的定义 —— 否则残骸会被当成新文件传给对端（反流）。
--
--   诚实说明做不到的部分：SIGKILL、断电、远端 helper 被 sshd 杀掉时，进程没有机会
--   运行任何清理代码，残骸会留在磁盘上。此时靠上面那条"不是内容"的规则保证它不参与
--   同步（不反流、不污染对端）；要回收空间就手工删：
--   @find \<root\> -name '.hsbisync-tmp*' -delete@。
--   明确**不做断点续传**：残骸没有价值，一律丢弃。
module Tmp
    ( createTemp
    , isTempName
    , discardTemp
    ) where

import Control.Exception (IOException, catch)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (removeFile)
import System.FilePath (takeFileName)
import System.IO (Handle, hClose, hSetBinaryMode, openTempFile)

-- | 公共前缀（隐藏文件，不会跟真人文件撞名）
tmpPrefix :: String
tmpPrefix = ".hsbisync-tmp"

-- | 后缀
tmpSuffix :: String
tmpSuffix = ".tmp"

-- | 交给 'openTempFile' 的模板：生成名 = @前缀 ++ \<本进程 pid\> ++ "-" ++ \<序号\> ++ 后缀@
--   （pid 由 openTempFile 自己填，天然唯一，天然可识别）
tmpTemplate :: String
tmpTemplate = tmpPrefix ++ tmpSuffix

-- | 原子创建临时文件（@O_EXCL@：同名已存在时 openTempFile 会自己换名字重试）。
--   属性（mode/mtime）由调用方在 rename 之前设好，这里只负责名字与独占创建。
createTemp :: FilePath -> IO (FilePath, Handle)
createTemp dir = do
    (path, h) <- openTempFile dir tmpTemplate
    hSetBinaryMode h True
    return (path, h)

-- | 是不是我们自己的临时文件（前缀 + 后缀，中间非空）
isTempName :: FilePath -> Bool
isTempName path =
    let n = takeFileName path
    in tmpPrefix `isPrefixOf` n
       && tmpSuffix `isSuffixOf` n
       && length n > length tmpPrefix + length tmpSuffix

-- | 丢掉临时文件（best-effort）：清理动作本身绝不能变成一个新的失败
discardTemp :: FilePath -> Handle -> IO ()
discardTemp tmp h = do
    ignore (hClose h)
    ignore (removeFile tmp)

ignore :: IO () -> IO ()
ignore action = action `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = return ()
