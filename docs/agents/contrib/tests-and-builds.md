# Focused tests and builds

```sh
node scripts/contrib.mjs prepare --package midgard-node --plan
node scripts/contrib.mjs prepare --package midgard-node --execute
node scripts/contrib.mjs test --package midgard-node --file tests/validation-worker-pool.test.ts
node scripts/contrib.mjs test --package midgard-node --file tests/database.test.ts --seed 42
node scripts/contrib.mjs build --package midgard-sdk
node scripts/contrib.mjs native --package midgard-node
node scripts/contrib.mjs boundary --package midgard-core
```

File selectors are package-relative explicit test paths. The runner resolves
that package's Vitest, supplies emulator mode for node/node-tools and test mode
for other packages, derives the package's pretest
builds, requires the selected blueprint stamp and local test Postgres where
applicable, and assigns an invocation-specific disposable database family.
Both file and test ordering use the recorded seed. A name selector records
filtered assertions separately from skipped selected assertions.

Place watcher test checkouts on the project filesystem outside `/tmp`: its
funding recovery suites exercise the production guard against temporary durable
stores. Keep that guard intact when choosing a worktree location.

`--source-only` explicitly omits compiled prerequisites. Use it for a suite
whose entire execution stays inside the source resolver; it cannot establish
compiled-worker behavior. A zero-test selection, missing/inconsistent report,
setup error, changed input or changed protected artifact fails. Selected skips
or todos produce exit 3, rather than a complete pass.

Package `build` scripts now enter the same guard. The original commands live
under `build:contrib-raw` as the guard's implementation. The three Dockerfiles
also call these recipes inside their isolated copied build trees; the demo
Docker context contains neither repository tooling nor Git. Native build
recipes likewise enter `contrib native`. Build freshness binds the checkout,
source/dependency/configuration/lock/native/generated-input closure and emitted
file contents; timestamps cannot establish freshness. Doctor and preflight use
the same dist verdict. Inputs conservatively include dependency tests and
fixtures, so some unrelated edits can require an extra rebuild.

The closure also binds the installed packages
(`demo/node_modules/.pnpm/lock.yaml`), the variables a recipe names, and the
dist bytes of every workspace package a build source imports. A dist can be
fresh only when all of these hold (`scripts/contrib/build-inputs.mjs`):

- `build:contrib-raw` is an `&&` sequence of `tsup` (without `--onSuccess`,
  `--config`, `--tsconfig` or watch flags) and `node [flags] script.mjs|.cjs|.js`
  (without flags that load or evaluate other code or read an env file), each
  optionally preceded by `NAME=value`. Words may be quoted and may expand
  `$NAME`, `${NAME}` or `${NAME:-literal}`; each such name is bound. Any other
  program (`pnpm`, `npx`, `tsx`, `sh`, a `.ts` script) or shell syntax is
  refused, with the reason.
- The tsup config (which sets no `onSuccess` or `tsconfig`) and node scripts,
  and every local module they import, are closure files that import only
  `fs`, `fs/promises`, `path`, `url`, `crypto`, `util` and `os` (and `tsup`),
  load no code dynamically, and read `process.env` only by literal name; each
  name is bound. `child_process`, `worker_threads`, `vm`, `net`, `http` and
  every other builtin are refused.
- A tsup public directory (`--publicDir`, or `publicDir` in the config as a
  string literal or `true`) is inside the closure.
- The nearest `tsconfig.json` of the package and of every workspace package its
  sources inline, its `extends` chain, and its `include`, `files`,
  `references`, `baseUrl`, `paths` and `typeRoots` targets are closure files or
  installed packages, and no relative import in the package's or an inlined
  package's `src` leaves the closure.
- `ESBUILD_BINARY_PATH` is unset, and `NODE_OPTIONS` loads no code
  (`--require`, `-r`, `--import`, loaders and the like).
- After the build, the trace (`scripts/contrib/build-trace.cjs`, preloaded into
  every Node process of the recipe except the corepack executable the build
  `PATH` resolves) accounts for everything the build did. Every file read
  through `node:fs`, every copy, rename, link or symlink source (recursively for `cp`),
  every module loaded (ES modules included, through `module.registerHooks`)
  and every input esbuild's metafile lists is a closure input, an installed
  package, a dist of the package or a compiled dependency, or a file the
  build wrote before it read it. The only child process allowed is esbuild's
  installed service binary; any other spawn, an esbuild context, or a Node
  without module hooks is untraced. Every `tsup` the recipe runs must appear
  as a process that reports an esbuild bundle, and every recipe script as a
  process. Otherwise the build succeeds but the dist stays unstamped: the
  stamp file holds the reasons instead, so the next check reports it missing
  and rebuilds.
- A `node_modules` above the checkout (TypeScript includes every ancestor's
  `@types`) leaves every dist unstamped; the reason, and the doctor and
  preflight fix, name the directory to remove.

Not bound: existence probes and directory listings that read no file contents,
package-manager configuration (`.npmrc`) read by the exempt launcher, the
files the esbuild binary reads for its own use (it reports what it bundled),
and variables tsup or esbuild read internally (`PATH` included).
Building a fresh dist is a verified no-op: it prints `fresh: skipped` and writes a receipt with
`"status": "fresh"` that points at the receipt of the build that made the dist.
`--force`, or `MIDGARD_CONTRIB_FORCE_BUILD=1` for builds reached through
`pnpm run build`, rebuilds the requested package anyway.

The Go helper disables ambient Git stamping with `-buildvcs=false`: linked
worktrees and source archives must not discover an unrelated ancestor repository.
Its native receipt records source, compiler and binary identities instead.
Watcher TypeScript rebuilds preserve `dist/native`; only the declared Go binary
is excluded from JavaScript output identities. Native receipts still bind its
bytes. Clean reproduction rechecks both owners after all artifact checks, so
a later build or generator cannot silently erase an earlier native result.

Guarded children use Corepack to select each project's declared package manager,
including bare `pnpm --dir ...` inside nested recipes. The demo pins pnpm 9 and
the docs site pins pnpm 10; an ambient executable cannot select their version.
Corepack must be available. Source folders named `build`, `target` or `dist`
remain inputs; generated outputs are excluded only at their owning project.

Guarded invocations hold an exclusive workspace resource while consuming or
publishing compiled artifacts. Build steps also enter a host-wide memory-heavy
queue. Nested guarded commands inherit ownership only when their token and
Linux process ancestry match. This serializes guarded consumers in one
checkout. Raw compiler/test commands and external editors can bypass the
lease; the final digest check catches resulting changes. It does not freeze
external processes or make mutable dist paths immutable.

Run `node scripts/contrib/enroll-builds.mjs --write` after adding a package
build recipe. CI checks enrollment. Do not invoke a raw recipe as contributor
verification. [script: scripts/contrib/enroll-builds.mjs]
