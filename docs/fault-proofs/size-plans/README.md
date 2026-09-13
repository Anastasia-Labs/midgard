# Fault-proof fit evidence

This directory keeps JSON files that current tests read as regression inputs.
It does not archive successful runs. Measurements belong to their recorded source,
compiler, blueprint, and shape; retaining a file does not make it current.

## Verification contracts

| Contract                                      | Executable authority                                                                                                                                                                                                                                              | What it establishes                                                                                                                                 |
| --------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| Complete live resolver publication comparison | [Resolver publication test](../../../demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts)                                                                                                                                               | Rebuilds measurements and compares the full saved ledger, including current blueprint/compiler identity.                                            |
| Historical snapshot consistency               | Family tests listed below                                                                                                                                                                                                                                         | Checks saved rows/digests or a hardcoded measurement table; does not establish current-build fit.                                                   |

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

Inspect the owning lifecycle test before
regeneration: most producers use `MIDGARD_WRITE_FIT_LEDGER=1`, while some have
family-specific output variables or regenerate only historical tables. Never
replace a blueprint digest without remeasuring the transactions.

## Fresh acceptance artifacts

Run the normal testnet build and the complete registered lifecycle scenarios
under the [publication and execution rules](00-primer.md). Capture current
results with the release artifact. Publication rows alone do not prove execution;
atomic maximum rows do not prove registered lifecycle provenance.

ScriptSources, Phase-A, LOP publication and transition test writers can emit
measurement reports, but no current test reads their former checked-in snapshots.
Those reports are run outputs, not repository regression inputs. Transition
installed/subvariant suites currently write them on ordinary runs; other writers
are opt-in. Do not recommit generated reports merely because running a test
recreates a deleted historical path. The live measurements and assertions remain
in their owning tests.

[Availability challenge](availability-challenge.md) is the remaining active
publication plan. Implemented resolver decisions live in
[ADR 0003](../decisions/0003-publishable-semantic-resolvers.md) and
[ADR 0004](../decisions/0004-checkpointed-ledger-output-facts.md).
