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
that package's Vitest, supplies emulator mode, derives the package's pretest
builds, requires the selected blueprint stamp and local test Postgres where
applicable, and assigns an invocation-specific disposable database family.
Both file and test ordering use the recorded seed. A name selector records
filtered assertions separately from skipped selected assertions.

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

The Go helper disables ambient Git stamping with `-buildvcs=false`: linked
worktrees and source archives must not discover an unrelated ancestor repository.
Its native receipt records source, compiler and binary identities instead.

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
