# Fault-proof architecture decisions

Accepted and proposed architectural decisions for Midgard fault proofs live in
this directory. A decision records protocol intent and acceptance criteria; it
does not activate a consensus change by itself. Consensus-affecting decisions
still require an exact profile/version update, implementation, generated
evidence, and the normal release gate.

| Decision                                                                                                        | Status                                                                               |
| --------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| [0001 — Authenticated field-preimage carriage](0001-reference-input-field-evidence.md)                          | Implemented in canonical V1                                                          |
| [0002 — Bounded non-interactive proof threads](0002-bounded-proof-threads.md)                                   | Accepted; implemented in the current catalogue; signed-intent persistence superseded |
| [0003 — Publishable semantic resolvers](0003-publishable-semantic-resolvers.md)                                 | Accepted; validation and transition resolver decomposition implemented               |
| [0004 — Checkpointed ledger-output proof facts](0004-checkpointed-ledger-output-facts.md)                       | Accepted; implemented for ScriptSources and ResolveInputs                            |
| [0005 — Canonical compressed-prefix MPF root mutation](0005-canonical-mpf-root-mutation.md)                     | Accepted; implemented in production exclusion and mutation consumers                 |
| [0006 — DA attestation owns transaction-size admission](0006-da-attestation-transaction-size-admission.md)      | Accepted                                                                             |
| [0007 — The operator owns committed user-event validity](0007-operator-owned-event-validity.md)                 | Accepted                                                                             |
| [0009 — doubleWithdraw owns a block's payable same-output pair](0009-double-withdraw-owns-payable-pair.md)      | Accepted                                                                             |
| [0010 — The NoIdx families own a same-block missing output index](0010-no-idx-owns-same-block-missing-index.md) | Accepted                                                                             |
