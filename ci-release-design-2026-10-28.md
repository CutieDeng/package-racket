# tag 驱动自动出包：CI 设计讨论稿（2026-10-28）

## 0. 现状（已核对代码与仓库）

| 环节 | 现在怎么做 | 自动化程度 |
|---|---|---|
| 源码包 `racket-minimal-<ver>-src.tgz` | 本地：`package-racket --target brew --target source-release --racket-root <本地已装包的 checkout>`，staging 拷 `racket/collects`、`racket/src` 和 28 个包的**源码**（`<root>/pkgs/<name>` 或 `<root>/racket/share/pkgs/<name>`），写 `share/links.rktd`、`share/pkgs/pkgs.rktd`、`share/self-catalog`，确定性 tar，REST 上传到 `CutieDeng/racket` release `v<ver>`（`secret/ghtoken.rktd`，`replace-release-asset #t`） | 手工 |
| Homebrew | 同一次运行写 tap `Formula/racket@9.rb`（url + sha256 + formula-version）；推 tap main → `publish.yml` 在 macos-26 / ubuntu x64 / ubuntu arm 容器出 bottle，传到 tap 自己的 release，回写 bottle 块 | 推 tap 手工，其后自动 |
| deb / rpm / windows | 本地 `deb-spec`/`rpm-spec`/`windows-portable-ci` 按 `*-ci-config.rktd` 重生成脚手架（源码包 URL + sha256 钉死）；推各仓 main → `build-deb.yml`/`build-rpm.yml`/`build-windows-portable.yml` 构建并传到各仓 release | 推仓手工，其后自动 |
| 版本号 | `package-config.rktd`（source-version / formula-version）+ deb/rpm/windows/source-release 五个 config 里各写一遍 release-tag、release 计数 | 手工，六处 |
| 运行 package-racket 的 Racket | 需带 `tstring` 包的 Racket（本机 brew racket@9） | — |

事实：`CutieDeng/racket` 已有 tag v9.2.5…v9.3.5（release 承载源码包）；28 个 profile 包里 13 个在 racket 仓 `pkgs/` 内（base、racket-lib、tstring、racket-tstring、xrepl-lib、expeditor-lib、parser-tools-lib、at-exp-lib、sandbox-lib、compiler-lib、zo-lib、racket-index、net-lib），**15 个不在**（readline-lib、scribble-text-lib、syntax-color-lib、option-contract-lib、scheme-lib、rackunit-lib、testing-util-lib、errortrace-lib、source-syntax、scribble-lib、scribble-html-lib、srfi-lite-lib、compatibility-lib、planet-lib、draw-lib），本地靠 checkout 里 `raco pkg install` 过的 `racket/share/pkgs`。README 提到的 Rhombus profile 在代码里不存在（`grep rhombus` 为 0），README 需更正。`gh` 不在本机，但 CI runner 自带。

## 1. 目标

```
git push pri-github v9.3-dev
git tag v9.3.12 && git push pri-github v9.3.12
```
之后无人干预：源码包上 release → tap/deb/rpm/win 四仓自动得到新版本并各自出包。再次打 tag 或重跑必须幂等。

## 2. 设计

### 2.1 一个编排工作流，放在 CutieDeng/racket
`.github/workflows/release-packages.yml`，`on: push: tags: ['v*']`（另加 `workflow_dispatch` 输入 tag 供重跑）。由 package-racket 生成（新目标 `release-ci`），和其他仓一样"生成物不手改"。放在 racket 仓而不是 package-racket 的理由：tag 事件就在这里，同仓 `GITHUB_TOKEN` 直接能写自己的 release，少一个跨仓 dispatch 令牌。

### 2.2 作业
**A. source（ubuntu-24.04，≈5 min）**
1. checkout racket@tag；checkout `CutieDeng/package-racket@main` 到子目录。
2. 断言：tag `vX.Y.Z` == `racket_version.h` == `pkgs/base/info.rkt`，否则失败（这次 9.3.12 就漏过 info.rkt）。
3. 取可运行 package-racket 的 Racket：`brew install cutiedeng/racket/racket@9`（上一版 bottle，含 tstring；Linux 用 `ghcr.io/homebrew/brew` 容器）。回退：`make base` + 装树内 `pkgs/tstring`（≈20 min），由 `workflow_dispatch` 开关选。
4. 取 15 个外部包：新文件 `profile-lock.rktd`（包名 → 来源 URL + sha1），`raco pkg install --no-setup --checksum <sha1>` 到临时 `racket/share/pkgs`；sha1 与 self-catalog 复用同一算法。lock 由本地现有 root 一次生成，之后升级外部包 = 改 lock 并提交。
5. `package-racket --target brew --target source-release --token-env GITHUB_TOKEN`（需新增 `--token-env`，现在只读文件）。产出 tgz、sha256、Formula；release 不存在则创建（`gh release create --verify-tag`）。
6. 产出物 `source-meta.json`（version、formula-version、sha256、url）作为后续作业的唯一输入。

**B. fan-out（needs A，≈2 min）**
用细粒度 PAT `PACKAGING_PUSH_TOKEN`（contents:write，仅 homebrew-racket / deb-racket / rpm-racket / win-racket 四仓）：
1. tap：写 Formula（A 已生成），提交推 main → 现有 `publish.yml` 出 bottle。
2. deb-racket / rpm-racket / win-racket：以 A 的 version/sha256 覆盖 `*-ci-config.rktd` 里的 release-tag / release-name / 源码 URL，运行 `deb-spec`、`rpm-spec`、`windows-portable-ci` 重生成，提交推 main → 各自工作流出包。
3. 全部提交是"重生成"：无差异就不提交、不触发，天然幂等。
4. 同时把改过的六个 config 提交回 package-racket（版本号单一来源见 2.3）。

**C. status（可选）**：在 racket release 页追加一段"下游构建"链接；不等待下游（各仓自己报状态）。

### 2.3 版本号推导（去掉六处手写）
- racket tag `vX.Y.Z` → source-version `X.Y.Z`，formula-version `X.Y.Z.1`，deb/rpm release `1`，下游 release-tag `vX.Y.Z`。
- 只改打包、不改源码：在 **package-racket** 打 tag `pkg-X.Y.Z.N` → 跳过 A（复用已有源码包与 sha256），只跑 B，formula `X.Y.Z.N`、deb/rpm release `N`、rpm tag `vX.Y.Z-rN`（现有 `-r2` 命名）。
- `-rc` 后缀的 tag：只跑 A 且 release 标 prerelease，不推下游。
- `package-config.rktd` 等文件改为 CI 回写的产物，本地不再手改。

### 2.4 令牌
- `GITHUB_TOKEN`（racket 仓）：写自己的 release。
- `PACKAGING_PUSH_TOKEN`：细粒度 PAT，四个下游仓 contents:write；仅此一个新秘密。
- 下游仓继续用各自的 `GITHUB_TOKEN` 上传 release（现状不变）。

### 2.5 需要在 package-racket 里加的东西
1. `--token-env NAME`（`source-release`/`apt-release`），保留 `--token-file`。
2. `profile-lock.rktd` + 目标 `profile-fetch`（校验 sha1，写到 `--racket-root` 的 `racket/share/pkgs`）。
3. 目标 `release-plan`：输入 tag，输出六个 config 的新值（或直接改写），供 B 用。
4. 目标 `release-ci`：生成 `release-packages.yml`，复用现有 YAML 校验器。
5. README 删掉 Rhombus profile 描述或把包加回 profile（二选一）。

### 2.6 验收
1. 在 fork 上用 `v9.3.12-rc1` 跑 A：release 为 prerelease，tgz sha256 与本地 `brew` 流程产物逐字节相同（确定性 tar 保证）。
2. 打 `v9.3.12`：A+B 全跑；四个下游仓各出现一次由 bot 推送的提交；bottle/deb/rpm/zip 各自 release 到位；`brew upgrade`、`apt install`、`dnf install` 冒烟。
3. 重跑同一 tag：A 原位替换资产，B 零提交，下游不重复构建。
4. `pkg-9.3.12.2`：只跑 B，formula 9.3.12.2。

### 2.7 开销与风险
- 墙钟：A 5 min；下游并行，bottle 3 runner 各 30–40 min，deb/rpm 矩阵 20–40 min，windows ≈40 min；端到端约 1 h。
- 引导依赖：A 用上一版 bottle 跑 package-racket；若 tstring 语法或 package-racket 用到新 Racket 特性，需走回退路径（构建 checkout）。
- 外部包：lock 钉 sha1，官方 catalog 下架旧版会使 A 失败（显式失败，不会悄悄换内容）。
- 历史：本次 v9.3-dev 重写过提交，推送用 force；tag 应打在推送后的最终提交上。

## 3. 决定（2026-10-28 用户定）
1. **编排放 package-racket 仓**：racket 仓只保留一个极小的 `notify-packaging.yml`（tag 推送 → `repository_dispatch` 到 package-racket，令牌为细粒度 PAT，actions:write）。打包逻辑、版本推导、下游写入全部在 package-racket 的 `release.yml`（`repository_dispatch` + `workflow_dispatch(tag)` + `push: tags: pkg-*`）。
2. **引导 Racket**：任何我们私有 racket 系的 bottle 都可以（不必是上一版）；CI 用 tap 的 `racket@9` bottle 跑 package-racket。
3. **版本**：正常只抬 Z（racket tag `vX.Y.Z` → formula `X.Y.Z.1`、deb/rpm release 1）；N 只在"源码不变、package-racket 变"时抬（package-racket tag `pkg-X.Y.Z.N`，只跑 fan-out）。
4. **外部包**：profile 需要的外部包直接内置到 racket 源仓（`pkgs/`，与 expeditor/xrepl/parser-tools 同一做法），不做 lock/fetch；非 profile 的外部包不内置。已落地：racket `386ca58323` 把 15 个包搬进 `pkgs/`（9.3.5 打包根里的版本），Makefile PKGS/REQUIRED_PKGS 同步。

## 4. 实施清单（按序）
1. racket：`386ca58323`（已做）；验证 package-racket 的 `brew` 流程从 `pkgs/` 取齐 28 个包。
2. package-racket：`--token-env`；`release-plan`（tag → 六个 config 的值并改写）；`release-ci` 生成 `.github/workflows/release.yml`（本仓）与 `notify-packaging.yml`（racket 仓）；README 删 Rhombus 描述。
3. 秘密：package-racket 仓 `PACKAGING_PUSH_TOKEN`（contents:write：racket、homebrew-racket、deb-racket、rpm-racket、win-racket）；racket 仓 `PACKAGING_DISPATCH_TOKEN`（actions:write：package-racket）。可以是同一枚 PAT。
4. 验收：`v9.3.12-rc1`（只 A，prerelease）→ `v9.3.12`（A+B）→ 重跑幂等 → `pkg-9.3.12.2`（只 B）。

## 5. 实施状态（2026-10-28）
- racket `386ca58323`：15 个 profile 包内置到 `pkgs/`；`6182d6bbc3`：`.github/workflows/notify-packaging.yml`（生成物）。
- package-racket `1a44ba6`：`--token-env`、`release-plan`、`release-config.rktd`、同步测试改为从 formula-version 推导；`2764fc6`：`release-ci`、`--create-release`/`--prerelease`、`--deb-repo-root`/`--windows-repo-root`、README；`9294eb2`：生成的 `release.yml`。测试 55/55。
- 验证过：`brew` 流程从新树 staging 20/20 默认包（文档包随 `--with-docs`）；`release-plan` 对临时拷贝改写/回读/幂等；两份 workflow 经 ruby YAML 校验与内容断言。
- **未做（需要你）**：① 建细粒度 PAT：contents:write 于 racket、homebrew-racket、deb-racket、rpm-racket、win-racket，actions:write 于 package-racket；存为 package-racket 仓 secret `PACKAGING_PUSH_TOKEN`，racket 仓 secret `PACKAGING_DISPATCH_TOKEN`（可同一枚）。② 推送 package-racket main、racket v9.3-dev（force）。③ 验收序列：`v9.3.12-rc1` → `v9.3.12` → 重跑 → `pkg-9.3.12.2`。
- 已知限制：CI 的源码包由 GNU tar 产出，与本机 bsdtar 的字节可能不同（各自确定性）；首次 rc 跑前无法在本地验证 `Homebrew/actions/setup-homebrew` 容器内 `brew install racket@9` 的耗时。

## 6. tag 命名空间（2026-10-29 改）
`v*` 可能撞上游（上游 racket 用 `vX.Y`、`vX.Y.Z.W`，fork 里也带着这些 tag；一旦 `git push --tags` 就会误触发）。改为 fork 专用前缀：源码发布 `release-X.Y.Z`、候选 `release-X.Y.Z-rcN`、打包改版 `pkg-X.Y.Z.N`（package-racket 仓）。`release-plan` 明确拒绝 `v*`；notify 只监听 `release-*`；GitHub release、源码包 URL、bottle 根 URL、deb/rpm/win 的 release tag 全部用 `release-X.Y.Z`（rpm 为 `release-X.Y.Z-rN`）。旧的 v9.3.5 等 release 原样保留，旧 Formula/脚手架继续指向旧 URL。

## 7. 试跑记录（2026-10-29，tag `release-9.3.12-rc1`，racket 9baf1c00e6）

事件链：racket 推 tag → `notify packaging`（40848669980 成功）→ `repository_dispatch racket-tag` → package-racket `release packages`。rc 路径 = plan 不提交配置 + 只做 A（源码 release，prerelease），跳过下游再生与推送。

| 次 | run | 失败步 | 根因 | 修复（package-racket 提交） |
|---|---|---|---|---|
| 1 | 40845…(手动 dispatch) | Set up Homebrew | setup-homebrew 的 token 与 checkout 的 extraheader 重复 `Authorization` | 818a076：先 checkout，setup-homebrew 不给 token，推送前 unset extraheader |
| 2 | — | brew install | brew 容器内 runner 临时目录 root 属主，EACCES | 4b1cbfd：去容器，ubuntu-24.04 + Linuxbrew |
| 3 | — | Checkout racket source | rc 去 checkout 不存在的 `release-9.3.12` | 5a179c5：候选用自身 tag |
| 4 | 40848688898 | Build the source archive | rc 不提交 plan 改写，release job 新 checkout main 仍是 9.3.5 配置 → `source-version 9.3.5 does not match 9.3.12` | 6a404d8：release job 在自己的 checkout 上重跑 `release-plan`（幂等；也消除正式 tag 下 checkout 早于 plan 提交的竞态） |
| 5 | 40849586359 | Build（上传资产） | 真错误被 `raise-user-error` 把消息当格式串吞掉（`tag ~g not allowed`） | a350eb3：190 处改为 `"~a"` 传参 |
| 6 | 40850501892 | Build（上传资产） | `rktcrypto_rsa_verify_msg: implementation not found`：引导 bottle 9.3.5.1 的 librktcrypto keep 表（38 项）无此符号，Linux 链接器丢弃该归档成员，in-tree TLS 无法验 uploads.github.com 的 RSA 证书；api.github.com 为 ECDSA 故建 release 成功。9.3.12 已导出（lan-11 x86-64 `nm -D` 确认，keep 表 126 项，541fd38089 加入） | 7cc7575：`--http-client curl`，release job 全部 GitHub API 走 curl(1)；token 经 0600 文件 `-H @file` |
| 7 | 40851850919 | — | **成功**：plan 41 s，release 2 min 27 s | — |

结果（已验证）：
- `https://github.com/CutieDeng/racket/releases/tag/release-9.3.12-rc1`，prerelease，资产 `racket-minimal-9.3.12-src.tgz` 21 350 301 B，sha256 `82fc5372…86639c`，GitHub digest 与本地一致。
- 该 sha256 与 run 5/6 本地构建值相同（确定性 tar 成立）。
- Formula 写入 tap checkout 但 rc 不推送；bottle root `homebrew-racket/releases/download/release-9.3.12-rc1` 尚无 bottle（rc 不产 bottle）。

未做：`release-9.3.12` 正式 tag（A+B，推 tap/deb/rpm/win）、重跑幂等、`pkg-9.3.12.2`。
racket 侧待办：无（9.3.12 已含 keep 表修复）；以后引导 bottle ≥ 9.3.12 后 `--http-client curl` 仍保留，作为引导栈与被发布树解耦的保险。
