# Fault-proof fit evidence

How a fault-proof transaction is shown to fit the Van Rossem limits, and the
one measurement summary this directory keeps.

## Fit is enforced at submission

Every transaction a fault-proof emulator test submits is judged before the
emulator sees it. The suite's setup file
[`van-rossem-submit-guard.ts`](../../../demo/midgard-fault-proofs/tests/support/emulator/van-rossem-submit-guard.ts)
refuses a transaction over 16,384 signed bytes, 16.5M memory units or 10G CPU
units, and a publication (a transaction with zero execution units) over the
15,872-byte publication target, which keeps a 512-byte reserve below the
maximum. The refusal fails the test that submitted the transaction and names
the axis and its negative margin. The limits live in
[`src/proof-fit/van-rossem.ts`](../../../demo/midgard-fault-proofs/src/proof-fit/van-rossem.ts).
The traced-refusal run is exempt, because its validators carry traces by
design; the plain run is the one that proves fit.

There are no fit ledgers. The per-family fit ledger JSON files, their
writers and the `fit:regenerate` command were removed: the guard checks every
transaction of every run against the current build, which a saved ledger could
only restate.

| Contract                                                                                         | Executable authority                                                                                                                                                                          |
| ------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Every emulator transaction is within the Van Rossem maximum, every publication within the target | [Submit guard](../../../demo/midgard-fault-proofs/tests/support/emulator/van-rossem-submit-guard.ts) and its [test](../../../demo/midgard-fault-proofs/tests/van-rossem-submit-guard.test.ts) |
| A family fits at its consensus-maximum shape                                                     | The family's lifecycle suite, which submits that shape from Init to conviction and removal                                                                                                    |
| Resolver publications keep the 512-byte reserve                                                  | [Resolver publication test](../../../demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts)                                                                           |

A green run cannot satisfy [release acceptance](../execution-plan.md) by
itself.

## Fresh acceptance artifacts

Run the normal testnet build and the complete registered lifecycle scenarios
under the [publication and execution rules](00-primer.md). Capture current
results with the release artifact. Publication rows alone do not prove execution;
atomic maximum rows do not prove registered lifecycle provenance.

ScriptSources, Phase-A, LOP publication and transition test writers can emit
measurement reports. They are run outputs, not repository regression inputs. Do
not commit them.

[Availability challenge](availability-challenge.md) is the remaining active
publication plan; its [measurement summary](availability-challenge-fit.json)
records the measurements that plan cites. Implemented resolver decisions live in
[ADR 0003](../decisions/0003-publishable-semantic-resolvers.md) and
[ADR 0004](../decisions/0004-checkpointed-ledger-output-facts.md).
