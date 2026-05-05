# T06 Builder Fluent API

## Scope

Implement the initial fluent `newTx()` API and immutable builder state.

## Dependencies

- T03 core types.
- T04 provider interface.
- T05 wallet interface.
- Architecture docs `01` and `03`.

## Deliverables

- `LucidMidgard.new(...)`.
- Wallet selection methods.
- `newTx()`.
- `collectFrom`, `readFrom`, `addSigner`.
- `pay.ToAddress`, `pay.ToContract`, `pay.ToProtectedAddress`.
- `validFrom`, `validTo`.
- Builder inspection/debug snapshot.

## Acceptance Criteria

- Public calls produce deterministic builder state.
- Duplicate spend/reference inputs fail.
- Spend/reference overlap fails.
- Output authored order is preserved.
- Protected output intent is explicit in state.

## Required Tests

- Fluent API smoke examples.
- Immutability or snapshot safety tests.
- Duplicate input/reference rejection.
- Output ordering test.
- Protected output state test.

## Non-Goals

- No script redeemer indexing beyond state placeholders.
- No balancing or completion in this task unless needed for smoke tests.
