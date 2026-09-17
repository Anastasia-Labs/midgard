# TQ-13 — Review snapshot contracts and preserve coverage gates

Status: Proposed
Last reviewed: 2026-09-07

- **Audit sections**: §2.1, §2
- **Rules**: R5
- **Blocked by**: —

## Current behavior

`demo/midgard-validation/tests/resolver-proof-fit-sweep.test.ts` compares
`artifact.unfit` with `resolver-proof-fit-sweep-v1.unfit-pin.json`. Equality
notices new gaps, closed gaps, identity changes, and reason-text changes. It is
therefore a regression gate as well as a maintenance cost. The surrounding
row checks prevent unmeasured rows from claiming an accepted measurement, but
do not independently prevent the unfit set from growing.

The earlier claim that the pin only punishes progress was incorrect. Retain
this current verifier input until a replacement preserves its coverage.

## Scope and work

1. Separate the stable category/index identity and measured/unmeasured status
   from explanatory prose. Compare the former where it is the contract; keep
   the explanations available to reviewers.
2. If replacing exact equality with an allowed-gap set, reject new gaps and
   removed measurements. Decide explicitly whether a closed gap requires an
   accompanying reviewed update. Do not derive the expected set from the same
   observed artifact, which would eliminate the regression check.
3. Keep the current row validity, non-empty measurement, and budget checks.
   A saved artifact still needs current build/evaluator provenance before it
   establishes release readiness (TQ-01).
4. Trace `MIDGARD_VALIDATION_EVIDENCE` through package scripts and CI before
   changing its gate. A declared optional evidence lane is not an orphan merely
   because the variable is absent from default CI.
5. Review other large fixtures by their consumers. Public API export rosters
   can protect a public contract; package `engines.node` assertions can protect
   the declared floor. Testing the executing Node version is an additional,
   different property. Neither assertion is automatically redundant.

Cross-language golden vectors and fixed-input digest vectors remain in scope
only for provenance review, not deletion as duplicated output.

## Verification and acceptance

Run the affected snapshot consumer and its relevant evidence lane. Confirm that
an unexpected additional unmeasured row and loss of a required measured row are
rejected by the replacement. Confirm that the documented treatment of a closed
gap works. Preserve required input files until these checks are in place.

Do not claim unchanged coverage merely because the remaining tests pass.
