# T03 Core Types

## Scope

Define stable TypeScript types for user-facing and internal Midgard builder
data.

## Dependencies

- T01.
- T02 for codec types.

## Deliverables

- `Address`, `OutRef`, `MidgardUtxo`, `Assets`, `ValueLike`.
- Script, datum, redeemer, and output option types.
- Protocol parameter types.
- Structured error base classes.
- Utility conversion functions around CML assets and outputs.

## Acceptance Criteria

- Types can represent all fields required by simple transfers and future
  script/mint/observer transactions.
- Outref ordering helper follows lexicographic `txHash`, then `outputIndex`.
- Output types distinguish ordinary and protected outputs.
- Errors are serializable enough for CLI display.

## Required Tests

- Outref parse/format/order tests.
- Asset normalization and arithmetic tests.
- Output option validation tests.
- Error shape tests.

## Non-Goals

- No provider calls.
- No transaction completion.
