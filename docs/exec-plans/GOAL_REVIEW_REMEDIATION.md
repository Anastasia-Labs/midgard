# Remaining capability review verification

Status: Active

Last reviewed: 2026-09-07 (remaining requirements; no release acceptance rerun).

The completed July review findings and remediation diary have been removed.
Two remaining verification requirements belong to the current
[capability acceptance contract](GOAL_SPEC.md):

- **Necessity revalidation (RF-055):** repeat the retained measured proof-fit
  checks against the frozen release source, compiler, blueprint, applied
  parameters, and fixtures. Existing scanner pins and the CG1 exclusion do
  not establish current acceptance. Resolve their actual discrepancies;
  updating a digest or restoring removed bookkeeping is not verification.
- **Manifest-bound candidate production (RF-079):** after the real release
  identity is available, exercise the production candidate emitter with a
  fresh benchmark database and assert that its identity is manifest-bound.
  Safe integer-count conversion and a skipped/default candidate do not prove
  that production route.

Recheck generated native transaction fixtures after codec changes and run the
narrow behavioral, type, and build checks required by the affected packages.
Current coverage and release blockers live in
[the proof coverage matrix](../fault-proofs/coverage-matrix.md) and
[testnet readiness](../public_testnet_readiness.md). No historical green review
row substitutes for those checks.
