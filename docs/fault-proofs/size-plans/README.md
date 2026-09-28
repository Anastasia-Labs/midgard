# Fault-proof fit evidence

This directory keeps JSON files that current tests read as regression inputs.
It does not archive successful runs. Measurements belong to their recorded source,
compiler, blueprint, and shape; retaining a file does not make it current.

## Verification contracts

| Contract                                      | Executable authority                                                                                                | What it establishes                                                                                      |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| Complete live resolver publication comparison | [Resolver publication test](../../../demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts) | Rebuilds measurements and compares the full saved ledger, including current blueprint/compiler identity. |
| Historical snapshot consistency               | Family tests listed below                                                                                           | Checks saved rows/digests or a hardcoded measurement table; does not establish current-build fit.        |

The `*-fit-ledger.test.ts` readers were removed on 2026-09-09: they compared
a saved ledger to a transcribed copy of itself (or to a stale blueprint digest)
and could not fail for any production reason. The live lifecycle suites that
write these ledgers are the only executable authority for fit: they assert
every margin on fresh measurements and close with a fail-closed coverage
check. The JSON files here remain as recorded evidence and as writer outputs;
a `--check` regenerator is the intended replacement for drift detection.
A green snapshot cannot satisfy [release acceptance](../execution-plan.md).

Find a literal consumer from the repository root with:

```sh
rg --fixed-strings '<ledger filename>' demo/midgard-fault-proofs/tests
```

Inspect the owning lifecycle test before regeneration. Every checked-in ledger
is written through `writeVanRossemFitLedger`, which throws
`FitLedgerWriteRefusedError` unless `MIDGARD_WRITE_FIT_LEDGER=1` is set, so an
ordinary test run never rewrites one. Two variables instead name an output
path, and naming it is the opt-in, so they need no flag:
`MIN_ADA_FIT_LEDGER_PATH` (read by
`min-ada-wrongful-rejection-lifecycle.test.ts`) and
`TRANSITION_TRACE_FIT_LEDGER_PATH` (read by the three
`submit-init-emulator-transition-trace-final*.test.ts` files). Of those three,
`-final.test.ts` writes the named path itself; `-final-many-assets` and
`-final-deep-deposit` write a sibling next to it, `<name>-many-assets<ext>` and
`<name>-deep-deposit<ext>`. Some producers regenerate only historical tables.
Never replace a blueprint digest without remeasuring the transactions.

## Fresh acceptance artifacts

Run the normal testnet build and the complete registered lifecycle scenarios
under the [publication and execution rules](00-primer.md). Capture current
results with the release artifact. Publication rows alone do not prove execution;
atomic maximum rows do not prove registered lifecycle provenance.

ScriptSources, Phase-A, LOP publication and transition test writers can emit
measurement reports. Most are run outputs, not repository regression inputs. Do
not recommit reports merely because a test regenerates a historical path.

Two transition-trace ledgers are active regression inputs, each checked by the
suite that measures it:

| Ledger                                           | Suite                                                       |
| ------------------------------------------------ | ----------------------------------------------------------- |
| `transition-trace-forced-window-fit-ledger.json` | `submit-init-emulator-transition-trace-subvariants.test.ts` |
| `transition-trace-workflow-fit-ledger.json`      | `transition-trace-installed-lifecycle.test.ts`              |

Without the write flag, the suite requires the saved ledger's blueprint and
compiler identity to match the current build, its digest to match its body, and
its scenario and row roster to match the fresh run. Execution budgets differ
between runs of one blueprint, so row values are not compared. Regenerate a
ledger by running its complete suite with `MIDGARD_WRITE_FIT_LEDGER=1`, then
rerun it without the flag to verify the saved evidence. Never change a blueprint
digest without fresh lifecycle measurements.

[Availability challenge](availability-challenge.md) is the remaining active
publication plan. Implemented resolver decisions live in
[ADR 0003](../decisions/0003-publishable-semantic-resolvers.md) and
[ADR 0004](../decisions/0004-checkpointed-ledger-output-facts.md).
