# T10 Local Validation

## Scope

Add local Phase A and optional Phase B validation preflight after extracting
node validation semantics into shared packages.

## Dependencies

- T07 canonicalization.
- T08 balancing.
- T09 scripts for complete coverage.
- Architecture doc `07`.

## Deliverables

- Use shared `@al-ft/midgard-core` for native tx codec/types/order helpers.
- Shared `@al-ft/midgard-validation` package for Phase A/Phase B validators,
  reject-code types, and validation reports, or an equivalent shared validation
  module accepted by the repository.
- Node imports the shared validation source.
- `lucid-midgard` imports the shared validation source for preflight.
- `validate("phase-a")`.
- `validate("phase-b")` when pre-state is available.
- `complete({ localValidation })`.
- `CompleteTx.validate`, `validateSafe`, and `validateProgram` expose local
  preflight reports without mutating completion metadata.
- Validation reports.
- Reject-code preserving errors.

## Acceptance Criteria

- Local Phase A agrees with node Phase A for fixture bytes.
- Local Phase B agrees with node Phase B for fixture bytes and pre-state.
- Node and `lucid-midgard` consume the same validation implementation, not two
  independent mirrored implementations.
- Budget enforcement defaults to true.
- Missing pre-state is explicit.
- Phase B recomputes `scriptIntegrityHash` from `redeemerTxWitsRoot` and
  canonical `scriptLanguageViews` after resolving inline and reference scripts.
- Missing or extraneous language views preserve node reject-code discipline and
  reject with the shared validation error path.
- Result names and docs use "local preflight" terminology and never imply final
  acceptance.
- Reject codes and reason details are preserved exactly from shared validation.
- Regression guards prove node validation modules remain shims over
  `@al-ft/midgard-validation` and `lucid-midgard` does not define mirrored
  Phase A/B runners.

## Current Implementation Notes

- `@al-ft/midgard-validation` owns Phase A, Phase B, reject codes, script
  source resolution, context construction, and local script evaluation.
- `demo/midgard-node/src/validation/*` re-exports the shared package for Midgard
  native validation.
- `lucid-midgard` calls the shared validators for both
  `complete({ localValidation })` and `CompleteTx.validate(...)`.
- `CompleteTx.validate("phase-b")` requires explicit `localPreState`; budget
  enforcement defaults to true.
- Local preflight reports are marked non-authoritative and do not imply final
  node acceptance.

## Required Tests

- Shared package unit tests for Phase A/Phase B rules.
- Phase A accept/reject fixtures.
- Phase B accept/reject fixtures.
- Budget exceeded rejection.
- Value not preserved rejection.
- Input not found rejection.
- Error code preservation.
- Node conformance tests proving unchanged reject codes after extraction.
- `lucid-midgard` conformance tests proving local preflight reports match node
  validation for fixture bytes and pre-state.

## Non-Goals

- Do not implement an independent mirrored validator in `lucid-midgard` as the
  production path.
- Do not mark locally validated transactions as final.
- Do not bypass node validation.
