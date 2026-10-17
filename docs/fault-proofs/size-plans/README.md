# Fault-proof fit evidence

This directory keeps JSON files that current tests read as regression inputs.
It does not archive successful runs. Measurements belong to their recorded source,
compiler, blueprint, and shape; retaining a file does not make it current.

## Verification contracts

| Contract             | Executable authority                                   | What it establishes                                                                                       |
| -------------------- | ------------------------------------------------------ | --------------------------------------------------------------------------------------------------------- |
| Fresh-run comparison | `fit:regenerate --check` ([below](#checking-a-ledger)) | Reruns each ledger's owning tests and compares the checked-in ledger with the one they write, row by row. |
| Historical snapshot  | None                                                   | A ledger no test writes is recorded evidence only; it does not establish current-build fit.               |

The `*-fit-ledger.test.ts` readers were removed on 2026-09-09: they compared
a saved ledger to a transcribed copy of itself (or to a stale blueprint digest)
and could not fail for any production reason. The live lifecycle suites that
write these ledgers are the executable authority for fit: they assert every
margin on fresh measurements and close with a fail-closed coverage check, and
`fit:regenerate --check` (below) holds each ledger they write to their fresh
run. A green snapshot cannot satisfy [release acceptance](../execution-plan.md).

## Regenerating a ledger

Regenerate a ledger against a freshly built blueprint with one command from
the repository root:

```sh
pnpm --dir demo/midgard-fault-proofs fit:regenerate mint-authorization-workflow-fit-ledger.json
```

It takes ledger file names or paths, or `--all` for every checked-in ledger a
test writes; `--list` prints each ledger's owning test files and runs nothing.
It finds the owning files in the test sources
([`fit-ledger-owners.mjs`](../../../demo/midgard-fault-proofs/scripts/fit-ledger-owners.mjs)),
then runs exactly those files, every case, in one Vitest run under
`MIDGARD_WRITE_FIT_LEDGER=1`, with a fragment directory and run token it makes
for that run and removes afterwards. It keeps a ledger only when every owning
file passed with no skipped case and the run rewrote the ledger. Otherwise it
puts the ledger back as it was and exits 1. It also puts back any other file
in this directory the run changed. The stale-blueprint refusal applies as in
any Vitest run. Execution budgets vary between runs of one blueprint, so a
rewrite without a blueprint change is noise: do not commit it.

## Checking a ledger

Check one ledger, or every ledger a test writes, against a fresh run of its
owners:

```sh
pnpm --dir demo/midgard-fault-proofs fit:regenerate --check transition-trace-workflow-fit-ledger.json
pnpm --dir demo/midgard-fault-proofs fit:regenerate --check --all
```

The check runs the owners exactly as a regeneration does, with one more
variable, `MIDGARD_FIT_LEDGER_CHECK_DIR`, set to a scratch directory:
`writeVanRossemFitLedger` writes a ledger bound for this directory there
instead, so a check never rewrites a checked-in file. It then compares each
checked-in ledger with the fresh one
([`fit-ledger-differences.mjs`](../../../demo/midgard-fault-proofs/scripts/fit-ledger-differences.mjs)).
Every field counts except the execution budgets (`memoryUnits`, `cpuUnits`),
the margins derived from them, `blueprintSha256` and `ledgerSha256`: rows,
their order, names, kinds, shapes, signed bytes and the byte and publication
margins must all match. A field only one side has is a difference, so a ledger
still carrying a dropped field such as `compilerVersion` is stale. The check
exits 1 and names each difference when a ledger is stale, when an owning file
failed, skipped a case or did not write its ledger, or when the run changed a
file in this directory; any such file is put back. It needs no pinned
blueprint hash or compiler version. CI runs `--check --all` in the
`midgard-fault-proofs fit ledgers` job.

A re-run has to build the same transactions for the comparison to mean
anything. Each owning test file calls `fixFitLedgerOwnerEmulator()`
([`fit-ledger-owner-emulator.ts`](../../../demo/midgard-fault-proofs/tests/support/emulator/fit-ledger-owner-emulator.ts))
when it loads, so its emulators start at a fixed time and its wallets come
from fixed seed phrases, one per role. Two runs then write the same ledger
byte for byte, execution budgets included. Other suites keep random wallets
and a wall-clock start.

Every checked-in ledger is written through `writeVanRossemFitLedger`, which
throws `FitLedgerWriteRefusedError` unless `MIDGARD_WRITE_FIT_LEDGER=1` is set,
so an ordinary test run never rewrites one. Split ledgers and measured-fit
fragments also need `MIDGARD_FIT_FRAGMENT_DIR` and a fresh
`MIDGARD_FIT_MEASUREMENT_RUN` token. The command sets all three for its run;
never set them by hand. Two variables instead name an output path, and naming
it is the opt-in, so they need no flag: `MIN_ADA_FIT_LEDGER_PATH` (read by
`min-ada-wrongful-rejection-lifecycle.test.ts`) and
`TRANSITION_TRACE_FIT_LEDGER_PATH` (read by the three
`submit-init-emulator-transition-trace-final*.test.ts` files). Of those three,
`-final.test.ts` writes the named path itself; `-final-many-assets` and
`-final-deep-deposit` write a sibling next to it, `<name>-many-assets<ext>` and
`<name>-deep-deposit<ext>`. Some producers regenerate only historical tables.
Never replace a blueprint digest without remeasuring the transactions.

Two ledgers are measured by several files that each run a disjoint part of one
case table: `value-not-preserved-fit-ledger.json` by
`value-conservation-lifecycle.test.ts` and its `-forced-assets` and
`-non-forced-assets` siblings, and `mint-authorization-workflow-fit-ledger.json`
by `mint-authorization-installed-lifecycle.test.ts` and its `-native-wide`,
`-native-deep`, `-reference` and `-field-maxima` siblings. Each part stores its
rows in the run's fragment directory once all its cases pass, and the last part
of the run to pass merges them into the ledger
(`tests/support/split-fit-ledger.ts`) in case order: value conservation's rows
are numbered as one file running every case in order numbers them, and mint
authorization's keep their `<scenario>/<stage>/...` names. A run that misses a
part, or a part with a failed or filtered-out case, leaves the ledger untouched.

No test writes twelve of the checked-in ledgers, and the command refuses
each of them with the reason. `invalid-signature-wrongful-rejection-v1-fit-ledger.json`
comes from `scripts/write-invalid-signature-fit-ledger.mjs`, which reads the
log of a passing run, and `min-ada-wrongful-rejection-v1-fit-ledger.json` from
the path `MIN_ADA_FIT_LEDGER_PATH` names. The other ten belong to families
whose suites record measured-fit fragments
(`tests/support/measured-fit-ledger.ts`) that nothing merges into a ledger,
because `verifyMeasuredFitLedger` has no caller. The list is
`LEDGERS_WITHOUT_SUITE_WRITER` in `fit-ledger-owners.mjs`; its test fails when
the list and the test sources disagree.

## Fresh acceptance artifacts

Run the normal testnet build and the complete registered lifecycle scenarios
under the [publication and execution rules](00-primer.md). Capture current
results with the release artifact. Publication rows alone do not prove execution;
atomic maximum rows do not prove registered lifecycle provenance.

ScriptSources, Phase-A, LOP publication and transition test writers can emit
measurement reports. Most are run outputs, not repository regression inputs. Do
not recommit reports merely because a test regenerates a historical path.

The blueprint hash covers the exact build, including its compiler preamble.
Fit ledgers do not carry a separate compiler version label.

[Availability challenge](availability-challenge.md) is the remaining active
publication plan. Implemented resolver decisions live in
[ADR 0003](../decisions/0003-publishable-semantic-resolvers.md) and
[ADR 0004](../decisions/0004-checkpointed-ledger-output-facts.md).
