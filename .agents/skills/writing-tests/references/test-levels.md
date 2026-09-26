# Test levels: commands, prerequisites, limits

Read the section for the level you picked in
[SKILL.md](../SKILL.md#3-pick-the-cheapest-level-that-catches-it). Commands
run from the repository root. Facts are as of 2026-09-25.

What each level's evidence establishes, for fault proofs in particular, is
tabulated in
[docs/fault-proofs/testing-status.md](../../../../docs/fault-proofs/testing-status.md).

## 1. Aiken unit test

- **Where:** a `test` block in the module that owns the predicate, under
  `onchain/aiken/lib` or `onchain/aiken/validators`.
- **Prerequisite:** the pinned fork compiler.
  `node onchain/aiken/scripts/pinned-compiler.mjs` exits 0 only when `aiken`
  (or `MIDGARD_AIKEN_BIN`) reports the `AIKEN_FORK_VERSION` pinned in the
  workflows. Stock v1.1.22 has an unsound decoder
  (`onchain/aiken/scripts/pinned-compiler.mjs:3-10`).
- **Focused run:**

  ```bash
  node onchain/aiken/scripts/run-focused-check.mjs <module> <test_name> [<test_name> ...]
  ```

  `<module>` is the source path under `lib/` or `validators/` without `.ak`
  (`midgard/bounded-blob-v1.test` or its underscore spelling). The runner
  asserts the pinned compiler, collects from that module only, and fails
  unless exactly as many tests as names passed. The selector forms, dotted
  modules and trace builds are in
  [aiken-contract-build](../../aiken-contract-build/SKILL.md#focused-checks).

- **CI:** `Aiken CI/Compile and run the Aiken test suite with the pinned fork`
  runs the full `aiken check`.
- **Does not prove:** that the off-chain builder produces a transaction the
  validator accepts, or that the deployed parameterization is right. That is
  level 4.

## 2. TypeScript unit test

- **Where:** the package that owns the logic. `midgard-core`, `midgard-sdk`,
  `midgard-validation`, `lucid-midgard`, `midgard-fault-proofs` and
  `midgard-watcher` have no database in their vitest setup;
  `da-committee-node`'s global setup only owns a temporary directory.
- **Focused run:**

  ```bash
  pnpm --dir demo/<package> exec vitest run tests/<file>.test.ts -t "<test name>"
  ```

  One `midgard-core` case ran in 0.4 s on 2026-09-25. For the watcher use
  `pnpm --dir demo/midgard-watcher test tests/<file>.test.ts`, which keeps the
  package's `MALLOC_MMAP_THRESHOLD_` setting.

- **Check the count.** A `-t` pattern that matches nothing exits 0 with every
  test skipped. A file path that matches nothing exits 1.
- **CI:** one step per package in `Midgard Node CI`, for example
  `Build, typecheck, and test Midgard SDK`, `Test lucid-midgard` and
  `Build and test Midgard core DA transport` (which runs the whole
  `midgard-core` suite).
- **Does not prove:** cross-language agreement (level 3) or on-chain
  acceptance (level 4).

## 3. Golden-vector channel

A channel is one generator, a JSON fixture that a vitest suite recomputes, and
a generated Aiken test module that the fork re-verifies. It pins TypeScript
and Aiken to the same bytes, and each channel's negative vectors keep a
verifier that accepts everything from passing
(`demo/midgard-validation/scripts/generate-cek-core-step-v1-goldens.mjs:1-39`,
shared plumbing in `demo/midgard-core/scripts/golden-channel.mjs`).

- **Add a case:** add the vector to the channel's vector source (for example
  `demo/midgard-validation/tests/fixtures/cek-core-step-v1.vectors.mjs`), run
  the generator without `--check`, and commit both outputs. Never hand-edit a
  generated fixture or `.ak` module.
- **Check:**

  ```bash
  pnpm --dir demo/<package> run fixtures:<channel>:check
  ```

  Then run the generated Aiken module through the level 1 runner.

- **Channels CI checks** (`Midgard Node CI`, one step each):
  `midgard-core` `native-tx-field-access-v1`, `native-tx-field-items-v1`,
  `native-tx-vector-v1`; `midgard-sdk` `native-tx-carriage-wire-v1`,
  `canonical-decodability-v1`, `committed-field-shape-v1`;
  `midgard-validation` `ordered-collection-boundary-aiken`,
  `nested-boundary-aiken`, `cek-core-step-v1`,
  `validation-auxiliary-witness-v1`; `midgard-node` `transition-trace-abi`.
- **Blind spot:** `lucid-midgard fixtures:native-compact:check` and
  `midgard-node fixtures:transaction-root-v1:check` exist but no workflow runs
  them.

## 4. lucid-evolution emulator scenario

- **Where:** `*-emulator.test.ts` in `midgard-node`, `midgard-validation` and
  `midgard-watcher`; `submit-init-emulator*` and the lifecycle suites in
  `midgard-fault-proofs`.
- **Prerequisite: a blueprint built from the current source.** The suites
  read `onchain/aiken/plutus.json`, which is build output and untracked. Build
  it the way CI does:

  ```bash
  pnpm --dir demo deployment:build preprod-testing
  ```

  This refuses any `aiken` on `PATH` but the fork pinned in
  `.github/workflows/aiken-ci.yml`
  (`demo/scripts/deployment-profiles.mjs:260-270`). Rebuild after any `.ak`
  change; nothing checks that an existing `plutus.json` matches the source.
  Some suites accept `MIDGARD_REAL_BLUEPRINT_PATH` to point elsewhere.

- **Focused run:**

  ```bash
  NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/<file>-emulator.test.ts
  pnpm --dir demo/midgard-fault-proofs exec vitest run tests/<file>.test.ts
  ```

  Several node emulator files also touch Postgres (level 5 prerequisites
  apply). The named node lanes (`test:operator-exit:emulator`,
  `test:tx-prep:emulator`, ...) are in `demo/midgard-node/package.json`.

- **Cost:** the whole fault-proof suite took 20.5 minutes at eight forks on a
  32-CPU host on 2026-09-21 (`demo/midgard-fault-proofs/vitest.config.ts:13-23`).
  Run one file.
- **Rules:** local UPLC evaluation on every completion, refusals asserted as
  script failures, times relative to the emulator
  ([SKILL.md](../SKILL.md#6-time-and-fees-in-emulator-tests)).
- **Does not prove:** real provider response shapes, submit lag against a
  real node, or chain-sync behavior. Emulator ledger rules also lag the real
  ledger at times: lucid 0.6.5 was needed before the emulator refused a
  reference input that is also spent (`967b1698b`). When the emulator is more
  permissive than the ledger, update the dependency rather than skipping the
  negative.

## 5. Postgres-backed node suite

- **Where:** `demo/midgard-node/tests` (and `demo/midgard-node-tools/tests`,
  which reuses the shard scheme under its own prefix). The files that touch
  Postgres are inventoried in the comment at the top of
  `demo/midgard-node/vitest.config.ts`; add a new one there. `[review]`
- **Prerequisites:**
  - A server on `127.0.0.1:5433` with `synchronous_commit=on`.
    `scripts/start-test-postgres.sh status` reports whether one is
    listening; `start` starts one with those flags. The server may be shared
    with other checkouts, so leave a running one alone.
  - Built `dist/` for files that spawn plain `node` children or load
    `dist/` (crash probes, `tests/validation-worker-pool.test.ts:41`).
    `pnpm --dir demo/midgard-node run pretest` builds `lucid-midgard`,
    `midgard-sdk` and the node; `pnpm test` runs it for you.
- **Focused run:**

  ```bash
  NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/<file>.test.ts
  ```

  Global setup creates and migrates one database per worker
  (`<prefix>_w0`..`<prefix>_wN`) before any file runs and builds the native
  owner binary with cargo (`tests/global-setup.ts:187-197`).

- **Knobs** (`tests/test-env.ts`, `tests/global-setup.ts`):
  - `MIDGARD_TEST_DATABASE_PREFIX` names the shard family. It exists so
    `midgard-node-tools` (`midgard_tools_test`) never shares a database with
    the node suite (`tests/test-env.ts:42-47`). The default `midgard_test`
    names are the same for every checkout on the server, so two checkouts
    running the node suite at once share shards.
  - `MIDGARD_SKIP_DB_TESTS=1` skips provisioning for a focused run of a file
    that never touches Postgres. A file that does touch it then fails.
  - `MIDGARD_SKIP_NATIVE_BUILD=1` skips the cargo build; the two files that
    need the binary skip with a printed reason.
  - `MIDGARD_NODE_TEST_FORKS` caps parallel files; `MIDGARD_NODE_TEST_BAIL`
    opts into bail for one run and is never committed.
- **CI:** `Midgard Node CI/Test Midgard node`, after
  `Build Midgard node and migrate isolated CI database`.
- **Does not prove:** behavior against real Ogmios, Kupo or a real node.

## 6. Devnet and live acceptance

Reach for this only when the regression lives in runtime composition,
transport, provider behavior or recovery on real services. Use
[midgard-e2e-acceptance](../../midgard-e2e-acceptance/SKILL.md) before
operating services, and
[demo/midgard-node-tools/devnet/AGENTS.md](../../../../demo/midgard-node-tools/devnet/AGENTS.md)
for devnet configuration. A live run found the Ogmios v6 `queryNetwork/tip`
shape that every test double had wrong (`b4a427d61`, `9eabcdc0c`); once found,
the durable guard is a lower-level test whose double answers the real shape.

## Whole-workspace runs

`pnpm --dir demo test` runs `demo/scripts/run-test-lanes-v1.mjs`, which gates
every lane on the workspace typecheck. CI runs the per-package steps instead.
`pnpm --dir demo run test:tx-prep:local` is the transaction-preparation
ladder: SDK, then node, then emulator.
