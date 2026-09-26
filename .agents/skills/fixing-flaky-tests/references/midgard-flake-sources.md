# Where Midgard flakes come from

Each entry gives the symptom you see, the mechanism, the evidence in this
tree, and the fix. Checked against the tree on 2026-09-25. Mistakes that make
a test _wrong_ rather than intermittent live in the sibling catalogue,
[mistakes-we-make.md](../../writing-tests/references/mistakes-we-make.md).

## Timing and clocks

### A Lucid instance created mid-test

- **Symptom:** `validity start or end too far in the past` from the evaluator,
  only in some orderings or only after an earlier step advanced time.
- **Mechanism:** on an emulator provider, Lucid takes its slot configuration
  from the emulator at the moment the instance is created: `zeroTime` is
  `provider.now()` and `zeroSlot` is `provider.slot` (lucid-evolution 0.6.5,
  `resolveSlotConfig` in the package's `dist/index.js`). A second instance
  created after the emulator moved cannot express any earlier time. The
  message comes from the `@lucid-evolution/uplc` 0.2.23 evaluator.
- **Fix:** build every Lucid instance for a scenario before advancing time, or
  derive bounds from the instance you build with and clamp the lower bound at
  its `zeroSlot`, as `exitValidityWindow` does
  (`demo/midgard-node/src/transactions/operators/exit.ts:186-213`). `[review]`

### Wall-clock time in an emulator test

- **Symptom:** a validity window, deadline or stage check that passes on a
  fast run and fails on a slow one.
- **Mechanism:** the emulator clock starts at `Date.now()` when it is
  constructed (the `Emulator` constructor in `@lucid-evolution/provider`
  0.2.4) and then moves only when the test advances it. A bound computed from
  `Date.now()` later in the test drifts from the emulator's clock by however
  long the test has run, which depends on machine load.
- **Evidence:** `b27a8d5c8` (a commit window opened ahead of the emulator's
  clock; the fix advances the emulator to the window's first slot);
  `daf9fad3d` (a devnet journey slept by the wall clock while the node checks
  validity against its ledger tip; the fix waits for the tip).
- **Fix:** express every time as an offset from `emulator.now()` or the
  instance's slot config, and advance the emulator explicitly. Rules for
  production lower bounds are in
  [transaction-finalization.md](../../../../docs/agents/transaction-finalization.md#validity-windows).
  `[review]`

### Real timers racing a budget

- **Symptom:** the wrong failure classification under full-suite load
  (`all_peers_failed` where `deadline_exceeded` was expected), never alone.
- **Evidence:** `15b30754d` added a `clock` seam (`now`, `setTimeout`,
  `clearTimeout`) to `WatcherPublicDaClientV1` so the test drives time
  instead of racing the global timer queue.
- **Fix:** inject the clock and decide by state, not by which timer fires
  first. `[review]`

## Postgres on 5433

### Shard names shared across checkouts

- **Symptom:** rows vanish mid-assertion, a migration error such as
  `schema_version_ahead`, or `DROP DATABASE` refused because another session
  is connected, when two checkouts or worktrees run node suites at once.
- **Mechanism:** every checkout names its shards
  `${MIDGARD_TEST_DATABASE_PREFIX ?? "midgard_test"}_w${VITEST_POOL_ID}`
  (`demo/midgard-node/tests/test-env.ts:46-53`) on the same server. The shard
  scheme keeps files of one run apart; nothing keeps two runs apart. Global
  setup recreates a shard whose schema belongs to another migration set with a
  plain `DROP` (`demo/midgard-node/tests/global-setup.ts:65-71,93-98`), which
  fails loudly if the other run is connected and succeeds under it if it is
  between files. `midgard-node-tools` uses its own family, `midgard_tools_test`,
  unless the variable is already set
  (`demo/midgard-node-tools/vitest.config.ts:20`).
- **Fix:** give a second checkout its own prefix for a single-package run:
  `MIDGARD_TEST_DATABASE_PREFIX=midgard_test_<worktree> pnpm --dir demo/midgard-node exec vitest run ...`.
  Setting it for the whole lane runner gives node and node-tools the same
  family again. `[review]` Nothing detects two runs sharing a shard.

### `synchronous_commit` is an environment difference, not a flake

- **Symptom:** a `SHOW synchronous_commit` assertion reads `off`
  (`demo/midgard-node/tests/database.test.ts:3054`,
  `demo/midgard-node/tests/tx-admissions-claim-load.test.ts:112-114`). It is
  red on every run on one server and green in CI.
- **Mechanism:** the assertion proves the write-behind's
  `SET LOCAL synchronous_commit = off`
  (`demo/midgard-node/src/database/txAdmissions.ts:1101,1188`) stays
  transaction-local. It needs the session default to be `on`. Global setup
  sets it per shard (`demo/midgard-node/tests/global-setup.ts:62`) and
  `scripts/start-test-postgres.sh:99` starts the server with it on; a
  database that skipped global setup takes the server's default.
- **Evidence:** `81c7ec871` added the per-database override after local
  servers started with the global `off` false-redded these tests.
- **Fix:** run through global setup, or start the server with
  `scripts/start-test-postgres.sh`, which leaves an existing server on the
  port untouched (`scripts/start-test-postgres.sh:9`). Do not change the
  assertion. `[runtime: createShard in demo/midgard-node/tests/global-setup.ts]`

## Resources

### wasm linear memory

- **Symptom:** `EvaluatorError: unreachable`, a wasm trap that looks like a
  validator rejection, in the last tests of a large emulator file.
- **Mechanism:** the uplc and CML evaluators allocate wasm linear memory
  outside the V8 heap, and it never shrinks. Through
  `@lucid-evolution/uplc` 0.2.22 each evaluation also leaked, so one long file
  hit the ~4 GiB wasm32 ceiling (`e04836c7c`). uplc 0.2.23 and lucid 0.6.4
  fixed both leaks and the heap guard was removed (`a0a56e280`); the pins are
  uplc 0.2.23 and lucid 0.6.5 as of 2026-09-25.
- **Fix:** keep one fresh process per file, which
  `isolatedForksPool` provides (`demo/midgard-test-support/vitest.js:36-70`),
  and split a file whose heaviest journey needs more. If a run dies on
  memory, lower the fork count; raising `heapMb` only moves the wall
  (`demo/midgard-test-support/vitest.js:50-53`).
  `[runtime: isolatedForksPool]`

### A loaded machine

- **Symptom:** a timeout, or `Timeout calling onTaskUpdate`, that tracks load
  rather than code. Another session's devnet, a parallel suite, or a
  `cargo` build competes for the same cores.
- **Evidence:** `ac1b630e1` (a row measuring ~22 s locally timed out at 60 s
  in 2 of 4 CI runs); `e7b91208e` (a test at 236,718 ms against a 240,000 ms
  budget); the watcher config's comment on worker RPC starvation
  (`demo/midgard-watcher/vitest.config.ts:41-46`).
- **Fix:** record the load with every measurement (`uptime`, `nproc`,
  `ps -eo pid,ni,pcpu,etime,args --sort=-pcpu | head`). A failure that only
  appears at a load average above the core count is a budget question; see
  the timeout row in the masking table. `[review]`

### Port collisions

- **Symptom:** `EADDRINUSE`, or a test talking to a service it never started.
- **Evidence:** test servers bind port 0 and read the assigned port
  (`demo/midgard-node/tests/helpers/local-l1-observation.ts:480`,
  `demo/midgard-watcher/tests/support/state-queue-observation-fixture.ts:338`).
  Separately, the node test defaults point the L1 provider at
  `127.0.0.1:1337` and `127.0.0.1:1442`
  (`demo/midgard-node/tests/test-env.ts:66-67`), the Ogmios and Kupo ports a
  local devnet publishes (`demo/midgard-node/docker-compose.kupmios.yaml:113,169`),
  so a test that reaches the provider without a double gets a different
  answer depending on whether a devnet is up.
- **Fix:** bind port 0; never pick a "free" port and bind it later. A test
  that must not reach L1 gets a double or an unroutable URL. `[review]`

## Vitest file parallelism

- **Symptom:** a file passes alone and fails in the full run, or passes with
  `MIDGARD_NODE_TEST_FORKS=1` and fails with the default of 4
  (`demo/midgard-node/tests/test-env.ts:14`).
- **Mechanism:** files run in parallel forks. Files that clear whole tables
  are safe only because each fork has its own shard; the list of
  database-touching files is kept by hand in
  `demo/midgard-node/vitest.config.ts:10-48`. A new file that picks its own
  database, a fixed port, a shared temp path or a module-level singleton
  outside the process breaks that.
- **Fix:** find the shared resource and give each fork its own. Running with
  one fork is a diagnostic, not a fix. `[review]`
