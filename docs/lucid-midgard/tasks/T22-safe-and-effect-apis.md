# T22 Safe and Effect APIs

## Scope

Expose non-throwing and `Effect`-based APIs for fallible builder, signing,
provider, validation, submission, and status operations where they improve typed
error handling and cancellation.

This task is being delivered incrementally. The current signed-off subset covers
builder completion, local output chaining, submission, and status polling:
`completeSafe`, `completeProgram`, `chainSafe`, `chainProgram`, `submitSafe`,
`submitProgram`, `statusSafe`, `statusProgram`, `awaitStatusSafe`, and
`awaitStatusProgram`. Safe and Effect variants for signing, provider reads, and
standalone local validation remain planned follow-up work and are still marked
planned in the API parity map.

Promise APIs may remain for ergonomics, but production-grade call paths should
have an auditable way to receive structured errors as values.

## Dependencies

- T03 core types and structured errors.
- T04 provider client.
- T05 wallet and signers.
- T07 finalization and canonicalization.
- T10 local validation.
- T11 submit, status, and chain.
- T18 sign builder layer.

## Deliverables

- `Effect` variants for completion, signing, submission, status polling, and
  local validation.
- Safe Promise variants that return a stable discriminated result type, such as
  `MidgardResult<T, E> = { ok: true; value: T } | { ok: false; error: E }`.
- Error union types covering builder, provider, signing, validation, and status
  failures, with stable discriminants, serializable fields, retryability, and
  preserved node reject-code fields where applicable.
- Clear taxonomy for expected operational failures versus programmer defects;
  safe and Effect APIs return expected failures but do not convert programmer
  defects into successful results.
- Preservation of node reject codes inside Effect and safe-result errors.
- Cancellation and interruption behavior for provider reads, submission, local
  validation, status polling, and wallet/signing operations. Non-cancellable
  boundaries must be documented explicitly and must not leak timers or hidden
  retries.
- Provider adapter helpers for Effect-native providers where practical.
- Documentation showing when to use Promise, safe, and Effect APIs.

## Acceptance Criteria

- Expected failures are returned as typed error values by safe and Effect APIs.
- Promise APIs are thin adapters over the same checked behavior.
- Effect APIs are lazy and perform no provider or wallet calls until run.
- Status polling can be interrupted without leaving hidden timers running.
- Structured error payloads remain serializable for CLI and service logs.
- Production API paths do not use `Effect.orDie`, unchecked `unsafeRun`, or
  defect-catching to hide validation or provider failures.
- CI includes a static guard against `Effect.orDie`, unchecked `unsafeRun`, and
  broad defect-catching in production `lucid-midgard` source.
- Local validation errors preserve authoritative node reject codes verbatim.

## Required Tests

- Effect success and failure for completion.
- Effect success and failure for signing.
- Effect success, duplicate admission, conflict, and rejection status paths.
- Safe-result tests for insufficient funds, provider payload errors, invalid
  signatures, and local validation rejection.
- Laziness test proving provider calls happen only when an Effect is run.
- Interruption or timeout test for status polling.
- Cancellation propagation tests for provider reads, submission, local
  validation, and wallet/signing Effect wrappers where cancellation is supported.
- Static guard test for forbidden unsafe Effect patterns in production source.
- Type tests for exposed error channels.

## Current Implementation Notes

- `TxBuilder.completeSafe(...)` and `TxBuilder.chainSafe(...)` return
  `MidgardResult<T, LucidMidgardError>`.
- `TxBuilder.completeProgram(...)` and `TxBuilder.chainProgram(...)` return lazy
  `Effect` values over the same completion/chaining paths.
- `CompleteTx.submitSafe(...)`, `CompleteTx.submitProgram(...)`,
  status helpers, submitted status helpers, and top-level
  `LucidMidgard.txStatus*`/`awaitTx*` helpers use the same
  `MidgardResult`/`Effect` convention.
- Submit admission payloads are validated before `SubmittedTx` construction and
  preserve durable admission semantics without implying validation acceptance:
  status is limited to the node admission enum, `duplicate` is required, and
  HTTP `202`/`200` must match new/duplicate admission semantics.
- Safe and Effect wrappers catch structured `LucidMidgardError` failures as
  expected operational errors. Unexpected defects are rethrown instead of being
  converted into successful safe-result values.
- The default CI workflow runs `lucid-midgard` typecheck and tests when
  lucid-midgard/core/validation code changes.
- The production source is guarded against `Effect.orDie`, unchecked
  `unsafeRun`, and broad defect-catching helpers.

## Non-Goals

- No rewrite of the fluent builder into an Effect-only DSL.
- No conversion of programmer defects into successful safe results.
- No validation bypass or downgraded strictness for safe APIs.
- No compatibility layer for legacy `safeRun` wrappers unless it delegates to
  the new typed surface.
