# Script budgets and Van Rossem fit

Neither execution cost nor Van Rossem fit is committed as a reading. Execution
costs are held to ceilings, and fit is checked as each transaction is
submitted, so a change that makes a script cheaper or smaller needs no edit
here.

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

## Van Rossem fit has no ledger

There are no fit ledgers. Every transaction a fault-proof emulator test submits
is held to the Van Rossem limits (16,384 signed bytes, 16.5M memory, 10G CPU)
at submission, and a publication (zero execution units) to the 15,872-byte
publication target, by the suite's submit guard
(`demo/midgard-fault-proofs/tests/support/emulator/van-rossem-submit-guard.ts`,
limits in `demo/midgard-fault-proofs/src/proof-fit/van-rossem.ts`). A
transaction over a limit fails the test that submitted it, naming the axis and
the negative margin. There is nothing to regenerate.
