不需要调用系统命令（`mv`/`chmod`/`touch` 那种 shell 脚本方式只是上一个回答给的一种"能干活就行"的示例）。`System.Directory.renameFile`、`copyFile` 和 `System.Posix.Files.setFileMode`/`setFileTimes` 本身就是直接的 syscall 封装（`rename(2)`、`chmod(2)`、`utimensat(2)`），不经过 shell，比 SSH 里拼一段脚本更干净、更不容易有转义问题——你现在的做法（走自己写的 helper 协议）方向是对的。

但你这句话里有个需要先纠正的认知：

> "能保证一致的，会校验的"

**哈希校验和原子性是两件不同的事，校验不能替代原子性。** 校验只能告诉你"写完之后对不对"，它不能阻止其他进程在写入过程中看到一个**写了一半、权限不对**的文件；而且如果校验失败了，在你现在的代码里，**原来那个好文件已经被覆盖没了**，没有回滚的余地。这才是原子写真正要解决的问题，跟"最终结果对不对"是两码事。

## 具体看你四条分支

**1. `Lcl → Lcl` 分支——实际上基本没问题**

```haskell
copyFile s d
setFileMode d (fromIntegral srcMode)
setFileTimes d ...
```

`directory` 包的 `copyFile` 文档写明了："If the destination file already exists, it is replaced atomically"，而且它本身就会**从源文件拷贝权限**。所以这条分支的核心部分（内容+权限）其实已经是原子的，你后面再调一次 `setFileMode` 是冗余操作（不会错，但没必要），`setFileTimes` 那个窗口期只影响 mtime，不影响能不能正常读/执行，属于无害的小瑕疵。这条分支不用大改。

**2. `Rmt → Lcl` 分支——这是真正有问题的地方**

```haskell
BS.writeFile d content
setFileMode d (fromIntegral srcMode)
setFileTimes d ...
```

`BS.writeFile` 是**原地截断再写**，完全不是原子操作。如果 `d` 恰好是一个正在运行的可执行文件，Linux 内核会保护它——直接以写模式打开一个正在执行的二进制会返回 `ETXTBSY`（Text file busy），所以这种情况下大概率是直接报错，不会静默写坏。但如果 `d` 是别的文件（配置、数据、库文件、或者还没启动但即将被启动的程序），这个写入过程完全没有保护：
- 写到一半时，任何读这个文件的进程会看到不完整内容
- 如果传输中途失败或者后面 hash 校验不通过，你已经把原来能用的文件毁掉了，**校验在这里只是"事后报警"，没有"事前防护"**

**3/4. `Lcl → Rmt` 和 `Rmt → Rmt`——取决于你看不到的 `RECV` 实现**

这两条分支的落盘动作实际发生在远端 helper 进程里（也就是同一个二进制以 `--helper` 方式跑起来后处理 `RECV` 命令的那部分代码，不在你贴的 `Exe.hs` 里，应该在别的模块）。既然 helper 也是你自己写的同一个程序，这部分代码你是能改的，需要确认它内部是不是也做了"写临时文件+校验+rename"，如果不是，同样的问题会在远端复现。

## 修复方式：把校验放到 rename 之前，而不是之后

核心改动：**先写临时文件 → 设权限/时间戳 → 算 hash 比对 → 比对通过才 rename，不通过就丢弃临时文件、原文件保持不动。**

```haskell
import System.IO (openBinaryTempFile, hClose)
import System.Directory (renameFile, doesFileExist, removeFile)
import System.Posix.Files (setFileMode, setFileTimes)
import System.FilePath (takeDirectory, takeFileName)
import Crypto.Hash (hash, SHA512, Digest)
import qualified Data.ByteString as BS

-- | 写入内容，设置权限/时间戳，校验通过才原子替换目标文件；
--   校验失败则清理临时文件，目标文件完全不受影响。
writeFileVerified
    :: FilePath   -- ^ 最终目标路径
    -> BS.ByteString  -- ^ 要写入的内容
    -> Int        -- ^ 目标权限（如 0o755）
    -> Integer    -- ^ 目标 mtime（epoch seconds）
    -> String     -- ^ 期望的哈希（来自源端，事先算好的）
    -> IO (Either String ())
writeFileVerified path content mode mtime expectedHash = do
    let dir = takeDirectory path
    (tmpPath, h) <- openBinaryTempFile dir (takeFileName path ++ ".tmp")
    BS.hPut h content
    hClose h

    -- 权限/时间戳在 rename 之前设好，保证一旦可见，属性一定是对的
    setFileMode tmpPath (fromIntegral mode)
    setFileTimes tmpPath (fromIntegral mtime) (fromIntegral mtime)

    -- 校验放在 rename 之前：只校验临时文件，不动目标文件
    let actualHash = show (hash content :: Digest SHA512)
    if actualHash /= expectedHash
        then do
            removeFile tmpPath
            return $ Left "verification failed before commit; target untouched"
        else do
            renameFile tmpPath path   -- 原子生效，此时内容和属性都已经是对的
            return $ Right ()
```

对应改 `copyEndToEnd` 里 `Rmt → Lcl` 那段：

```haskell
(Rmt hhSrc, Lcl dstRoot) -> do
    let d = dstRoot </> relPath
    createDirectoryIfMissing True (takeDirectory d)
    sendCommand hhSrc ("SEND " ++ relPath)
    sz <- readSendSize hhSrc
    hSetBinaryMode (hhOut hhSrc) True
    content <- hGetExactly (hhOut hhSrc) sz

    remoteHash <- hashRemote hhSrc relPath   -- 源端先给出期望哈希
    result <- writeFileVerified d content srcMode srcMtime remoteHash

    case result of
        Right () -> do
            putStrLn "    verified OK"
            appendEnd dst mTag "ADD" relPath
            appendEnd src mTag "ADD" relPath
        Left err -> putStrLn ("    WARNING: " ++ err)
```

区别就是：原来是"写→设属性→算本地 hash→和远端 hash 比→打日志"（校验只是旁观者，不影响已经发生的覆盖），现在是"写临时文件→设属性→算 hash→比对→**只有比对通过才让它生效**"（校验直接决定要不要 commit，目标文件在校验失败时完全没被碰过）。

`Lcl → Rmt`、`Rmt → Rmt` 这两条分支同理，需要去改远端 helper 里 `RECV` 命令的处理逻辑，思路一样：临时文件名（带 `.tmp` 或随机后缀）、写完设属性、算 hash 返回给发起方比对（或者由 helper 自己拿发起方传来的期望 hash 比对），比对通过再 `rename`，不通过就删掉临时文件并返回错误响应，不要去碰目标路径。


是的，完全一样——道理不区分"本地"还是"远程"，`writeFileVerified` 这套逻辑本质上就是**rsync 真实的底层实现方式**，不是我编出来近似的东西。rsync 默认（非 `--inplace`）模式下，对每个文件的处理确实就是：

1. 在**目标文件同一目录**下 `mkstemp()` 开一个随机名临时文件（这点很关键，见下面解释）
2. 写入内容
3. `chmod`/`utime` 设权限和时间戳
4. `rename()` 覆盖到最终路径

跟你现在这个 `writeFileVerified` 一模一样，只是 rsync 是 C 写的，你是 Haskell 写的。

## 把 `Lcl → Lcl` 也统一成这套逻辑

你之前的 `Lcl → Lcl` 分支用 `copyFile`（本身也是原子替换）是可以的，但既然要"像 rsync"，不如把所有分支都收敛到同一个 `writeFileVerified`，逻辑上更一致、也不用记两套语义：

```haskell
(Lcl srcRoot, Lcl dstRoot) -> do
    let s = srcRoot </> relPath
        d = dstRoot </> relPath
    createDirectoryIfMissing True (takeDirectory d)
    content <- BS.readFile s
    let expectedHash = show (hash content :: Digest SHA512)
    result <- writeFileVerified d content srcMode srcMtime expectedHash
    case result of
        Right () -> do
            putStrLn "    verified OK"
            appendEnd dst mTag "ADD" relPath
            appendEnd src mTag "ADD" relPath
        Left err -> putStrLn ("    WARNING: " ++ err)
```

这样四个分支（本地/远程 × 本地/远程）最终都收口到同一个"落盘函数"，差别只在于内容是从本地读的还是从 socket/pipe 收的，写入这一步完全统一。

## 有一个细节必须保证：临时文件要和目标同目录（同文件系统）

这是 rsync 能做到原子替换的**前提条件**，不是可选项。`rename(2)` 只有在**源和目标在同一个文件系统**上才是原子的，跨文件系统会返回 `EXDEV`，这时候：

- 命令行 `mv` 会自动降级成"复制+删除"（这就不再是原子的了）
- Haskell 的 `renameFile` **不会自动降级**，会直接抛异常

你现在的写法：

```haskell
let dir = takeDirectory path
(tmpPath, h) <- openBinaryTempFile dir (takeFileName path ++ ".tmp")
```

临时文件建在 `takeDirectory path`，也就是目标文件所在目录，这一点已经做对了——只要你的部署环境没有把某个子目录单独挂载成别的文件系统（比如 `/opt/myapp/bin` 是个单独挂载点），这个前提就一直成立。如果你不确定，可以在启动时检查一下，或者干脆约定"临时文件永远建在目标同目录"这条规则不许破例。

## 如果想更贴近 rsync 的"可靠性"级别，可以再加一步 fsync

rsync 本身默认不强制 fsync（有 `--fsync` 参数才会），但如果你想比 rsync 默认行为更保险（比如同步的是关键的可执行文件，担心断电丢数据），可以在 `hClose h` 之前对临时文件的底层 fd 显式 `fsync`：

```haskell
import System.Posix.IO (handleToFd)
import System.Posix.Unistd (fileSynchronise)  -- 具体模块名视 unix 包版本而定

BS.hPut h content
fd <- handleToFd h        -- 转成底层 fd 才能 fsync
fileSynchronise fd
hClose h
```

这一步保证内容真正落盘（不只是进了内核页缓存），配合 rename 才是完整的"崩溃安全"写入。是否需要看你的场景对断电这种极端情况的容忍度，纯粹为了"不打断正在运行的程序"其实不需要这步，fsync 解决的是另一个正交问题（持久性）。

## 需要澄清一点：这复刻的是 rsync 的"安全性"，不是它的"增量传输"

要说清楚这套东西**做到了什么**、**没做到什么**：

- ✅ 做到了：目标文件永远只有"旧的完整版本"或"新的完整版本"两种状态可见，不会出现半成品；校验失败绝不影响原文件；正在运行的程序不受打断
- ❌ 没做到：rsync 真正省带宽的核心是**滚动校验和 + 块级 diff 算法**（只传输文件里变化的那部分数据块，而不是整个文件）。你现在的实现（以及你之前 `Sync.hs`/`State.hs` 里基于整文件哈希判断要不要传输）走的是"整文件比对、要传就整个传"的路线，这在文件不大或者改动比例高时没问题，但如果以后要同步很大的文件且只改了一小部分，真正的带宽优化需要另外实现 rsync 算法本身（rolling checksum + block matching），这是完全独立的一块工作，和"原子写入"这个话题不冲突但也不是一回事。

如果你目前的场景文件不算特别大（比如程序二进制、配置文件），现在这套"整文件传输 + 原子落盘 + 校验"已经足够安全可靠，不需要再上真正的 rsync delta 算法。