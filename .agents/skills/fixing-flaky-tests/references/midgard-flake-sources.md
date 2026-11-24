# Where Midgard flakes come from

Each entry gives the symptom you see, the mechanism, the evidence in this
tree, and the fix. Checked against the tree on 2026-11-23. Mistakes that make
a test _wrong_ rather than intermittent live in the sibling catalogue,
[mistakes-we-make.md](../../writing-tests/references/mistakes-we-make.md).

## Timing and clocks

### A Lucid instance created mid-test

- **Symptom:** `validity start or end too far in the past` from the evaluator,
  only in some orderings or only after an earlier step advanced time.
- **Mechanism:** on an emulator provider, Lucid takes its slot configuration
  from the emulator at the moment the instance is created: `zeroTime` is
  `provider.now()` and `zeroSlot` is `provider.slot` (lucid-evolution 0.8.9,
  the package's `dist/index.js`). A second instance created after the
  emulator moved cannot express any earlier time. The message comes from the
  `@lucid-evolution/uplc` evaluator.
- **Fix:** build every Lucid instance for a scenario before advancing time, or
  derive bounds from the instance you build with and clamp the lower bound at
  its `zeroSlot`, as `exitValidityWindow` does
  (`demo/midgard-node/src/transactions/operators/exit.resolve-operator-script-refs-program.ts`).
  `[review]`

### Wall-clock time in an emulator test

- **Symptom:** a validity window, deadline or stage check that passes on a
  fast run and fails on a slow one.
- **Mechanism:** the emulator clock starts at `Date.now()` when it is
  constructed (the `Emulator` constructor in `@lucid-evolution/provider`
  0.4.8) and then moves only when the test advances it. A bound computed from
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
  `clearTimeout`) to a DA client so the test drove time instead of racing the
  global timer queue. The lesson holds for any budgeted retry loop.
- **Fix:** inject the clock and decide by state, not by which timer fires
  first. `[review]`

## Postgres on 5433

### Shard names shared across checkouts

- **Symptom:** rows vanish mid-assertion, a migration error such as
  `schema_version_ahead`, or `DROP DATABASE` refused because another session
  is connected, when two raw runs of the node suites share a checkout, or share
  one `MIDGARD_TEST_DATABASE_PREFIX`.
- **Mechanism:** shards are named `<prefix>_w${VITEST_POOL_ID}` on the same
  server. The prefix is `MIDGARD_TEST_DATABASE_PREFIX` when set, otherwise
  `midgard_test` in the main checkout and `midgard_test_<hash>` in a linked
  worktree (`defaultTestDatabasePrefix` in
  `demo/midgard-node/tests/test-env.ts`). `midgard-node-tools` uses the
  `midgard_tools_test` family the same way
  (`demo/midgard-node-tools/vitest.config.ts`). The scheme keeps files of one
  run and different checkouts apart; nothing keeps two runs in one checkout
  apart. Global setup recreates a shard whose schema belongs to another
  migration set with a plain `DROP` (`dropShard` in
  `demo/midgard-node/tests/global-setup.ts`), which fails loudly if the other
  run is connected and succeeds under it if it is between files.
- **Fix:** run suites through `node scripts/contrib.mjs test`, which gives
  every invocation its own family, `midgard_contrib_<checkout hash>_<random>`,
  and drops it when the run ends (`scripts/contrib/databases.mjs`). A raw
  `vitest run` still shares the checkout's family; nothing detects two raw
  runs sharing a shard.

### `synchronous_commit` is an environment difference, not a flake

- **Symptom:** a `SHOW synchronous_commit` assertion reads `off`
  (`demo/midgard-node/tests/database.test/ledger.ts`,
  `demo/midgard-node/tests/tx-admissions-claim-load.test.ts`). It is red on
  every run on one server and green in CI.
- **Mechanism:** the assertion proves the write-behind's
  `SET LOCAL synchronous_commit = off`
  (`demo/midgard-node/src/database/txAdmissions.admit-reserved-batch.ts`)
  stays transaction-local. It needs the session default to be `on`. Global
  setup sets it per shard (`createShard` in
  `demo/midgard-node/tests/global-setup.ts`) and
  `scripts/start-test-postgres.sh` starts the server with it on; a database
  that skipped global setup takes the server's default.
- **Evidence:** `81c7ec871` added the per-database override after local
  servers started with the global `off` false-redded these tests.
- **Fix:** run through global setup, or start the server with
  `scripts/start-test-postgres.sh`, which leaves an existing server on the
  port untouched (the script's header). Do not change the
  assertion. `[runtime: createShard in demo/midgard-node/tests/global-setup.ts]`

## Resources

### wasm linear memory

- **Symptom:** `EvaluatorError: unreachable`, a wasm trap that looks like a
  validator rejection, in the last tests of a large emulator file.
- **Mechanism:** the uplc and CML evaluators allocate wasm linear memory
  outside the V8 heap, and it never shrinks. Through
  `@lucid-evolution/uplc` 0.2.22 each evaluation also leaked, so one long file
  hit the ~4 GiB wasm32 ceiling (`e04836c7c`). uplc 0.2.23 and lucid 0.6.4
  fixed both leaks (`a0a56e280`); the pins are uplc 0.5.3 and lucid 0.8.9 as
  of 2026-11-23.
- **Fix:** keep one fresh process per file, which
  `isolatedForksPool` provides (`demo/midgard-test-support/vitest.js`), and
  split a file whose heaviest journey needs more. If a run dies on memory,
  lower the fork count; raising `heapMb` only moves the wall (the comment
  above `isolatedForksPool`).
  `[runtime: isolatedForksPool]`

### A loaded machine

- **Symptom:** a timeout, or `Timeout calling onTaskUpdate`, that tracks load
  rather than code. Another session's devnet, a parallel suite, or a
  `cargo` build competes for the same cores.
- **Evidence:** `ac1b630e1` (a row measuring ~22 s locally timed out at 60 s
  in 2 of 4 CI runs); `e7b91208e` (a test at 236,718 ms against a 240,000 ms
  budget); the watcher config's comment on worker RPC starvation
  (`demo/midgard-watcher/vitest.config.ts`).
- **Fix:** record the load with every measurement (`uptime`, `nproc`,
  `ps -eo pid,ni,pcpu,etime,args --sort=-pcpu | head`). A failure that only
  appears at a load average above the core count is a budget question; see
  the timeout row in the masking table. `[review]`

### Port collisions

- **Symptom:** `EADDRINUSE`, or a test talking to a service it never started.
- **Evidence:** test servers bind port 0 and read the assigned port
  (`demo/midgard-node/tests/settlement-worker-heap.test.ts`,
  `demo/midgard-watcher/tests/fault-proofs/watcher-installed-journey.test.ts`).
- **Fix:** bind port 0; never pick a "free" port and bind it later. A test
  that must not reach L1 gets a double or an unroutable URL. `[review]`

## Vitest file parallelism

- **Symptom:** a file passes alone and fails in the full run, or passes with
  `MIDGARD_NODE_TEST_FORKS=1` and fails with the default of 4
  (`demo/midgard-node/tests/test-env.ts`).
- **Mechanism:** files run in parallel forks. Files that clear whole tables
  are safe only because each fork has its own shard, which the `setupFiles`
  entry `demo/midgard-node/tests/pin-database-shard.ts` assigns to every file;
  there is no list to maintain. A new file that picks its own database, a
  fixed port, a shared temp path or a module-level singleton outside the
  process breaks that.
- **Fix:** find the shared resource and give each fork its own. Running with
  one fork is a diagnostic, not a fix. `[review]`
