# TQ-05 — Match negative-test assertions to their named failure

Status: Proposed
Last reviewed: 2026-11-23 (cited tests)

Rule: R3. Related: TQ-04, TQ-06, TQ-10.

## Problem and current evidence

A bare `toThrow()` proves that a call throws. That is adequate for some public
refusal contracts, but cannot by itself establish a named validator or exact
failure mode. Likewise, regex length is not a measure of discrimination. Review
the scenario, fixture, preceding controls and exception source together.
Candidate searches must distinguish `.not.toThrow()` and inspect
multiline or helper-based assertions before reporting totals.

## Candidates requiring individual decisions

- `devnet-stack-native-block-admission.test.ts` uses `arrayContaining` with one matcher:
  that checks at least one matching element, not all eight identifiers. If the
  contract requires all IDs, assert each expected member and cardinality.
- `not.toEqual(arrayContaining([a,b,c,d]))` rejects the complete conjunction,
  not every forbidden member individually. Confirm the intended exclusion
  contract before strengthening it.
- Decoder refusals, timeout aborts, lifecycle status and unique-token checks
  should assert the relevant public result where their titles promise it.
  A fixture helper that verifies useful shared preconditions is not inherently
  an invalid location for assertions.

## Proposed work

1. Prioritize tests whose specific named claim exceeds their current oracle.
   Reuse helpers for common boundaries without forcing every refusal through
   one generic helper or inventing a new public error API unnecessarily.
2. For a table of independent negative cases, ensure every case executes and
   checks its own result. An exception around the whole loop can stop later
   cases from running.
3. Add successful fixture controls where setup failures could satisfy the
   negative claim. Use structured codes when available; retain intentional
   message contracts and legitimate broad refusal tests.
4. Keep shared assertion helpers explicit and report failures with scenario
   context. Separate data-only builders where doing so improves attribution;
   do not ban assertions from all test support.

## Acceptance

- Each changed test checks its stated boundary and all listed cases run.
- Known membership/exclusion matcher mistakes are corrected where the intended
  contract is confirmed.
- No blanket zero-bare-throw quota, regex-length rule, or zero-`expect` rule
  substitutes for semantic review.
- Report the focused commands and actual outcomes. This card itself does not
  claim that any validator defect has been reproduced.
