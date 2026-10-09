# Focused tests and builds

```sh
node scripts/contrib.mjs prepare --package midgard-node --plan
node scripts/contrib.mjs prepare --package midgard-node --execute
node scripts/contrib.mjs test --package midgard-node --file tests/validation-worker-pool.test.ts
node scripts/contrib.mjs test --package midgard-node --file tests/database.test.ts --seed 42
node scripts/contrib.mjs test --package midgard-watcher --maxWorkers 2
node scripts/contrib.mjs test --package midgard-node --related demo/midgard-core/src/canonical-json.ts
node scripts/contrib.mjs build --package midgard-sdk
node scripts/contrib.mjs native --package midgard-node
node scripts/contrib.mjs boundary --package midgard-core
```

`contrib test` is how suites run, one file or a whole package. It runs the
package's own `test` script, the command CI runs: the environment that script
sets (`NODE_ENV=emulator` for node and node-tools, `NODE_ENV=test` where it
sets none), its Vitest flags and, for a whole-package run without `--name`, its
plain-Node preludes (`scripts/contrib/vitest-command.mjs`). A script flag the
runner does not understand is refused, not dropped. The caller may add only
Vitest's `--maxWorkers N`, `--exclude GLOB` (repeatable) and
`--disableConsoleIntercept`, spelled as Vitest spells them.

`--file` takes package-relative test paths; without one, the run is the whole
package. Before running, `vitest list` collects the files with the same
configuration and flags: a named file the config or an `--exclude` drops, or a
filter that also selects other files, is refused, and the receipt then
requires the report to cover exactly those files. The runner derives the
package's pretest builds, makes the blueprint ready where the package reads it
(copied from a checkout with identical inputs and profile, else built with
`deployment:build`), requires the local test Postgres where the package's
tests read it, and assigns an invocation-specific disposable database family,
dropped when the run ends. Both file and test ordering use the recorded seed.
A name selector records filtered assertions separately from skipped selected
assertions.

`--related PATH` (repeatable, exclusive with `--file`) runs the test files a
set of changed files reaches (`scripts/contrib/reached.mjs`). A child process
asks the package's own Vitest for its import graph from source, rooted at every
test, setup file and script of the package and every script outside the
packages, so edges into other packages, worker entries and spawned scripts are
graphed; Vitest's own `related` misses the last two, and the workspace bundles
hide the first. On top of imports, a module reaches a file it names in a
string literal (a fixture, a worker entry, a JSON input), the files under a
directory it lists, and a package's `dist` when a built input of its closure
changed. A `.ak` change also reaches every reader of
`onchain/aiken/plutus.json`. Whatever cannot be decided widens: a module the
probe cannot analyse always runs, a data file nothing names, a manifest, a
lockfile, a Vitest or TypeScript configuration or the shared test harness runs
the whole package, and so does a change reaching a package whose plain-Node
preludes are not graphed. A failed probe runs the whole package. Nothing
reached prints `no test reached; nothing to run` and exits 0 with a
`midgard-contrib-reach/v1` result; otherwise the receipt carries `reach` (the
changed files, `whole`, the reached files and why). A file named only through
a computed path with no part of its name spelled is the one edge it cannot
see.

The terminal gets a short verdict on stderr (counts, then each failed test or
file that failed to load with its first message line, the log and the receipt)
and a compact JSON summary on stdout; `--output FILE` keeps the full receipt,
and the run directory holds the logs and Vitest's JSON report.

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
  refused, with the reason. The package defines no `prebuild:contrib-raw` or
  `postbuild:contrib-raw` script, and the build runs pnpm with pre/post
  scripts disabled, `/bin/sh` as the script shell and no shell emulator.
- The tsup config (which sets no `onSuccess` or `tsconfig`) and node scripts,
  and every local module they import, are closure files that import only
  `fs`, `fs/promises`, `path`, `url`, `crypto`, `util` and `os` (and `tsup`),
  load no code dynamically (no `eval`, `Function`, global object,
  `constructor`, `_load`, `getBuiltinModule`, bindings, `dlopen`,
  `mainModule`, `createRequire`, or `require`/`import` without a literal
  specifier), name no network global (`fetch`, `WebSocket`, `EventSource`,
  `XMLHttpRequest`), and read `process.env` only by literal name; each name
  is bound. These scans skip comments, and the network scan also skips string
  text. `child_process`, `worker_threads`, `vm`, `net`, `http` and every
  other builtin are refused.
- A tsup public directory (`--publicDir`, or `publicDir` in the config as a
  string literal or `true`) is inside the closure.
- The nearest `tsconfig.json` of the package and of every workspace package its
  sources inline, its `extends` chain, and its `include`, `files`,
  `references`, `baseUrl`, `paths` and `typeRoots` targets are closure files or
  installed packages, and no relative import in the package's or an inlined
  package's `src` leaves the closure.
- `ESBUILD_BINARY_PATH` is unset, and `NODE_OPTIONS` loads no code
  (`--require`, `-r`, `--import`, loaders and the like). `NODE_OPTIONS` is
  split as Node splits it, so a quoted path with a space is one argument.
- After the build, the trace (`scripts/contrib/build-trace.cjs`, preloaded into
  every Node process and worker of the recipe except the corepack executable
  the build `PATH` resolves) accounts for everything the build did. Every
  file opened with read access (`r+`, `a+` and `O_RDWR` included, and
  `openAsBlob`), every copy, rename, link or symlink source (recursively for
  `cp`), every module loaded (ES modules included, recorded by
  `module.registerHooks` as their own kind) and every input esbuild's
  metafile lists is a closure input, an installed package, a dist of the
  package or a compiled dependency, or a file the build created before it
  read it. Only a successful truncating create (`w`, `writeFile` without an
  append flag, a copy or rename destination) counts as created; an append or
  a failed create does not. The default is to refuse: anything the tracer
  cannot classify is recorded as untraced and leaves the dist unstamped. That
  covers any child process but esbuild's installed service binary, a worker
  given its own `execArgv` or an environment that changes the trace or
  `NODE_OPTIONS`, any network connection or use of `fetch`, `WebSocket` or
  `EventSource`, a Node option that loads other code, an esbuild context, an
  fs call on a path it cannot name, and a Node without module hooks. Every
  traced process must report at least one module load, every `tsup` the
  recipe runs must appear as a process that reports an esbuild bundle, and
  every recipe script must appear as a process. Otherwise the build succeeds
  but the dist stays unstamped: the stamp file holds the reasons instead, so
  the next check reports it missing and rebuilds.
- A `node_modules/@types` above the checkout (TypeScript includes every
  ancestor's) leaves every dist unstamped. The reason names it, and the
  doctor and preflight fix advise moving it aside if nothing else needs it,
  then rebuilding. The fix never deletes anything outside the repository.

The guard catches mistakes and ordinary build code, not deliberate evasion.
It does not bind:

- existence probes, `stat`, `readlink` and directory listings, which read no
  file contents;
- package-manager configuration (`.npmrc`) beyond the pinned settings, which
  the exempt launcher reads;
- the files the esbuild binary reads for its own use (it reports what it
  bundled);
- variables tsup or esbuild read internally (`PATH` included).

Installed packages are bound by the install record, not scanned. Code in
them that bypasses Node's fs and module layers (a native addon, an internal
binding, a substituted `process.env`) is outside the trace, as is build code
written to defeat the static scan in ways it does not pattern-match.
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

Guarded invocations hold an exclusive workspace resource while they write
compiled artifacts (dist, native outputs, the blueprint). `contrib test` holds
it only for that preparation and releases it before the suites start, so test
runs in one checkout proceed side by side. Build steps also enter a host-wide
memory-heavy queue. Nested guarded commands inherit ownership only when their
token and Linux process ancestry match. A build, blueprint copy, raw compiler
or editor that changes a prerequisite under a running suite is caught by the
final digest check, which fails the receipt. It does not freeze external
processes or make mutable dist paths immutable.

Run `node scripts/contrib/enroll-builds.mjs --write` after adding a package
build recipe. CI checks enrollment. Do not invoke a raw recipe as contributor
verification. [script: scripts/contrib/enroll-builds.mjs]
