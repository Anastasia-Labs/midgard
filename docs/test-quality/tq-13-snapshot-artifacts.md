# TQ-13 — Review snapshot contracts and preserve coverage gates

Status: Proposed
Last reviewed: 2026-09-07

- **Audit sections**: §2
- **Rules**: R5
- **Blocked by**: —

## Scope and work

1. Trace `MIDGARD_VALIDATION_EVIDENCE` through package scripts and CI before
   changing its gate. A declared optional evidence lane is not an orphan merely
   because the variable is absent from default CI.
2. Review other large fixtures by their consumers. Public API export rosters
   can protect a public contract; package `engines.node` assertions can protect
   the declared floor. Testing the executing Node version is an additional,
   different property. Neither assertion is automatically redundant.

Cross-language golden vectors and fixed-input digest vectors remain in scope
only for provenance review, not deletion as duplicated output.

## Verification and acceptance

Run the affected snapshot consumer and its relevant evidence lane. Do not claim
unchanged coverage merely because the remaining tests pass.
