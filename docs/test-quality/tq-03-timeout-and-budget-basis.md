# TQ-03 — Calibrate timeouts and state each execution-budget basis

Status: Proposed
Last reviewed: 2026-09-07 (configuration and ledger review)

Audit: §12, §10, §7.1. Rule: R9. Related: TQ-15.

## Problem and current evidence

The fault-proof Vitest configuration and shared preset do not set a package
`testTimeout`. Suites without a local override can inherit Vitest's default,
while others explicitly allow long emulator runs. A timeout is a hang-detection
allowance, not measured runtime or a sum of expected CI duration.

The zero-input saved-ledger suite checks the hard 16.5M memory / 10G CPU
limits; the network-id saved-ledger suite checks 13.2M / 8G, a 20% headroom
basis. These are not automatically contradictory measurements of the same
contract. The issue is whether the suite clearly states and enforces the
required hard-fit or reliability-margin claim.

`van-rossem-fit-ledger.ts` already exports hard limits. Check the current
profile and GOAL_SPEC §3.3 before choosing the reliability margin. Do not
invent an existing `GOAL_SPEC_EXECUTION_BASIS_V1` export or replace distinct
normative limits with a single unexplained number.

## Proposed work

1. Inventory effective timeouts, including package presets and per-test
   overrides. Measure actual CI runtime before selecting defaults or reducing
   long allowances. Keep justified emulator and optional operator exceptions.
2. Establish package defaults where they simplify the real suite mix. Separate
   unit and emulator configuration when one default would weaken hang
   detection for short tests or make long scenarios flaky.
3. Name hard-fit and reliability-margin bases distinctly. Reuse declared
   constants for incidental arithmetic and retain independent tests of the
   normative values. Each report identifies which claim it proves.
4. Identify live measurement producers before moving benchmark work. Preserve
   script execution, publication, persistence and semantic controls; saved
   ledger rows are not replacements for current measurements.
5. Review Aiken cost-reading cases individually. A fixture-shape assertion can
   protect benchmark geometry; move it only with an explicit runnable producer
   and preserved consumers. Diagnostic logging is not categorically forbidden.

## Acceptance

- Effective timeouts and exceptions have a measured rationale; no claimed
  runtime reduction is based only on declared maximums.
- Every cost verdict names its profile and hard-fit or margin basis, with
  current producing identity where required.
- Reorganized benchmarks remain executable and required coverage remains in
  its CI lane. Do not remove assertions solely because today's producer
  already guarantees the checked property.
- Report actual commands and results, including omitted expensive lanes.
