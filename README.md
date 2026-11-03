# hsbisync

A bidirectional file sync tool written in Haskell.

## 构建

需要 [stack](https://docs.haskellstack.org/)（`stack.yaml` 固定 `lts-24.52` → GHC 9.10.3）：

```sh
stack build --copy-bins --local-bin-path .
```

## 在树莓派上安装

GitHub Actions 会在打 `v*` 标签时产出 `aarch64-linux` 的静态包，例如：

```sh
curl -LO https://github.com/vincentaxhe/hsbisync/releases/latest/download/hsbisync-v0.1.0-aarch64-linux.tar.gz
tar xzf hsbisync-v0.1.0-aarch64-linux.tar.gz
sudo install -m 755 hsbisync /usr/local/bin/
hsbisync --help
```

- 产物在 Ubuntu 22.04 arm64 上构建（glibc 2.35），可在 **Raspberry Pi OS 64 位 Bookworm / Trixie** 上直接运行。
- 若提示缺库：`sudo apt install libgmp10`。
- 32 位 Raspberry Pi OS（armhf）**没有**产物，需要自行在本机 `stack build`。

## 远端 helper

同步的每一端都要有**同一版本**的 `hsbisync` 二进制（协议会随版本变化），
部署到对端 PATH 里的任意目录即可（例如 `/usr/local/bin`）。
