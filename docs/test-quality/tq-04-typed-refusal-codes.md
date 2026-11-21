# TQ-04 — Prefer structured refusal codes for structured error contracts

Status: Proposed
Last reviewed: 2026-09-07 (error API review)

Audit: §4. Rules: R3, R4. Related: TQ-05, TQ-06.

## Problem and current evidence

An error matcher can be too broad for the failure named by its test, or too
coupled to incidental wording. Neither every prose assertion nor every
alternation is defective: user-facing diagnostics and a deliberately allowed
set of refusals can be observable contracts.

`demo/midgard-core/src/codec/errors.ts` already defines
`MidgardTxCodecErrorCodes` as an `as const` object, its value-union type, and a
`MidgardTxCodecError` carrying `code` and optional `detail`. It is not a
TypeScript enum and does not need to become one. Phase A tests already assert
rejection codes. Importing the vocabulary constant is not the only valid way
to test the public code values.

Candidate searches in the older audit count textual matches, not adjudicated
weak assertions. Recheck current source before quoting a package total or
rewriting an alternation whose accepted outcomes may be intentional.

## Proposed work

1. Identify the failure each scenario claims and the structured information
   currently exposed at that boundary. Keep existing stable vocabularies.
2. Assert `code`, cause, phase, or relevant state when those are public
   contracts. Add a new error surface only where callers need that distinction;
   do not introduce an API merely to avoid every prose assertion.
3. Review broad alternations individually. A test naming a single guard needs
   evidence of that guard; a test intentionally accepting multiple valid
   boundary refusals may retain a documented allowed set.
4. Preserve diagnostic wording tests where wording, redaction, a required
   field, or an operator explanation is the deliverable. Numeric thresholds
   may also need direct value checks; neither form automatically replaces the
   other.
5. Migrate in small packages or families with negative controls and ordinary
   successful scenarios. A changed refusal may reveal an incorrect old claim,
   but a newly red test requires investigation before calling it a finding.

## Acceptance

- Structured refusal tests use the stable code contract where available and
  relevant; existing `as const` APIs remain valid.
- Each changed matcher detects the failure its test names without accepting
  unrelated setup failures.
- Intentional diagnostic and allowed-set assertions remain covered.
- Any TQ-19 automation detects demonstrated weak patterns with documented
  exceptions; it does not ban all regexes or mandate enums/cardinality rules.
