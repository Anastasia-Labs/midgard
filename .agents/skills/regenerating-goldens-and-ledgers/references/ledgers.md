# Script budgets and fit ledgers

A fit ledger records a measured cost, not a derived value, so "regenerate"
means "measure again". The measurement is only as good as the tree and compiler
it ran on.

## Script budgets

`demo/midgard-core/src/script-budgets.json` is the one table of execution-unit
ceilings. An entry names an Aiken test as `module/test`; with `minusControl` it
judges that test minus a control test of the same module, and with
`env: "testnet"` it runs under that Aiken environment. The table is not a
channel: a ceiling is not something a generator writes, and a reading that
drops needs no edit.

`onchain/aiken/scripts/script-budgets.mjs` (run from the repository root, with
the aiken `demo/flake.lock` pins) judges every reading `<=` its ceiling:

```sh
node onchain/aiken/scripts/script-budgets.mjs check [--module <aiken module>]...
node onchain/aiken/scripts/script-budgets.mjs tighten [--module <aiken module>]...
node onchain/aiken/scripts/script-budgets.mjs raise <name> --reason <why>
```

CI judges the whole table from the complete test shards
[ci: Aiken CI/Hold every script budget to its ceiling]. Locally, `check` runs a
focused `aiken check` per budgeted module, so `--module` keeps it to the
modules you touched. `node scripts/preflight.mjs` plans exactly those.

- **Over its ceiling.** Shrink the script. If the cost is accepted, `raise`
  lifts that one ceiling to the reading plus 1% and records `--reason` in
  `raisedBecause`. Accepting a regression is a decision for whoever owns the
  lane [review]. `raise` refuses a budget that is not over its ceiling, and a
  ceiling above the basis.
- **Missing.** A renamed or deleted test, or a control that no longer exists.
  Rename the entry, or delete it with the test.
- **Now fits.** An `aboveBasis` entry records work that cannot fit an L1
  transaction. A reading inside the basis means the claim is stale: drop
  `aboveBasis` and bring the ceiling under the basis, after checking whatever
  relied on the infeasibility.
- **Out of band.** A `growth` claim holds the difference between two budgets
  inside a band, in both directions. Movement either way means the scaling the
  claim describes changed.

`tighten` only lowers ceilings, to the reading plus 1%. Production reads some
ceilings through `scriptBudget()` (the native-script scan planner and the
tx-order mint estimator), so changing one of those moves a production cost:
run the consumer's tests in the same change [review].

## Fit ledgers

`docs/fault-proofs/size-plans/*-fit-ledger.json` (45 tracked files) record
Van Rossem fit measurements. Each is stamped with the `blueprintSha256` and
`compilerVersion` it was measured against.
[`docs/fault-proofs/size-plans/README.md`](../../../../docs/fault-proofs/size-plans/README.md)
is the authority. Its rules:

- The `*-fit-ledger.test.ts` readers were removed on 2026-09-09. Most of these
  files are recorded evidence and writer outputs. Nothing compares them with
  the current build. `verifyMeasuredFitLedger`
  (`demo/midgard-fault-proofs/tests/support/measured-fit-ledger.ts:85`) would,
  but as of 2026-09-25 it has no caller.
- Two ledgers are still held to the current build:
  `transition-trace-forced-window-fit-ledger.json` (transition subvariants
  suite) and `transition-trace-workflow-fit-ledger.json` (installed
  transition-trace lifecycle suite). Each suite requires the recorded
  `blueprintSha256` to equal the SHA-256 of the current blueprint, and the
  scenario and row roster to equal a fresh run's; the budgets themselves may
  differ (`demo/midgard-fault-proofs/tests/support/pinned-fit-ledger.ts`)
  [ci: Midgard Node CI/Test fault-proof tooling]. Any
  change that moves blueprint bytes makes both stale.
- Regenerate against a freshly built blueprint with
  `pnpm --dir demo/midgard-fault-proofs fit:regenerate <ledger>` (or `--all`;
  `--list` shows each ledger's owning test files). It finds the owning files
  in the test sources, runs exactly those under `MIDGARD_WRITE_FIT_LEDGER=1`
  with a fragment directory and run token it makes for the run, and keeps the
  ledger only if every owning file passed and rewrote it; otherwise it puts
  the ledger back and exits 1. It sets `MIDGARD_FIT_FRAGMENT_DIR` and
  `MIDGARD_FIT_MEASUREMENT_RUN` itself. Thirteen checked-in ledgers have no
  suite writer, and the command refuses each with the reason
  (`LEDGERS_WITHOUT_SUITE_WRITER` in
  `demo/midgard-fault-proofs/scripts/fit-ledger-owners.mjs`)
  [ci: Midgard Node CI/Test the fit-ledger regeneration script].
- Without the flag the writer throws `FitLedgerWriteRefusedError` for every
  checked-in ledger, so no ledger changes; only the caller-named outputs
  (`MIN_ADA_FIT_LEDGER_PATH`, `TRANSITION_TRACE_FIT_LEDGER_PATH`) write without
  it [runtime: writeVanRossemFitLedger].
- Never replace a blueprint digest without re-measuring the transactions. A
  digest edited by hand makes an old measurement claim a new build [review].
- Do not recommit a historical report just because a test run rewrote it.

Example of a real re-pin: `1d85e6db3` (2026-09-12), "Re-pin the
transition-trace forced-window fit ledger to the decision 0007 blueprint".
