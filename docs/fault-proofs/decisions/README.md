# Fault-proof architecture decisions

Accepted and proposed architectural decisions for Midgard fault proofs live in
this directory. A decision records protocol intent and acceptance criteria; it
does not activate a consensus change by itself. Consensus-affecting decisions
still require an exact profile/version update, implementation, generated
evidence, and the normal release gate.

| Decision                                                                                                   | Status                      |
| ---------------------------------------------------------------------------------------------------------- | --------------------------- |
| [0001 — Authenticated field-preimage carriage](0001-reference-input-field-evidence.md)                     | Implemented in canonical V1 |
| [0002 — Bounded non-interactive proof threads](0002-bounded-proof-threads.md)                              | Accepted; implemented       |
| [0003 — Publishable semantic resolvers](0003-publishable-semantic-resolvers.md)                            | Accepted; implemented       |
| [0004 — Checkpointed ledger-output proof facts](0004-checkpointed-ledger-output-facts.md)                  | Accepted; implemented       |
| [0005 — Canonical compressed-prefix MPF root mutation](0005-canonical-mpf-root-mutation.md)                | Accepted; implemented       |
| [0006 — DA attestation owns transaction-size admission](0006-da-attestation-transaction-size-admission.md) | Accepted; implemented       |
