# T04 Provider Client

## Scope

Implement the provider abstraction and an HTTP provider for Midgard node APIs.

## Dependencies

- T03 core types.
- Architecture docs `04` and `08`.

## Deliverables

- `MidgardProvider` interface.
- `MidgardNodeProvider`.
- UTxO fetch by address.
- UTxO fetch by outref.
- Submit native transaction.
- Protocol parameter/current-slot retrieval from `GET /protocol-info`.
- Explicit constructor fallback for nodes that do not expose
  `GET /protocol-info`.
- Transaction status lookup through `GET /tx-status`.

## Stable Node Endpoints

The provider must target these stable node endpoints:

- `GET /utxos?address=...`
- `GET /utxo?txOutRef=...`
- `POST /utxos?by-outrefs`
- `POST /submit`
- `GET /tx-status?tx_hash=...`
- `GET /protocol-info`

`GET /protocol-info` is the provider source for:

- `apiVersion`
- `network`
- `midgardNativeTxVersion`
- `currentSlot`
- `protocolFeeParameters.minFeeA`
- `protocolFeeParameters.minFeeB`
- `submissionLimits.maxSubmitTxCborBytes`
- `validation.strictnessProfile`

## Acceptance Criteria

- UTxO responses validate `outref` and output bytes before returning.
- Submit preserves durable admission semantics and status codes.
- Backlog, tx-id conflict, and invalid payload errors are structured.
- `getProtocolParameters()` and `getCurrentSlot()` use `GET /protocol-info`
  when available.
- If `GET /protocol-info` is unavailable, provider construction must fail
  unless explicit fallback values were supplied.
- Fallback-derived values are exposed as diagnostics and are never silently
  treated as node-derived production values.
- Missing status support produces capability errors, not silent no-ops.

## Required Tests

- Mock HTTP UTxO success and malformed payload.
- Mock HTTP by-outref success and missing UTxO.
- Protocol info success.
- Protocol info unavailable without fallback fails closed.
- Protocol info unavailable with explicit fallback reports fallback source.
- Current slot is parsed from protocol info as an integer string.
- Transaction status preserves node reject code and detail.
- Submit success `202`.
- Duplicate submit `200`.
- Conflict `409`.
- Backlog full `503`.
- Transport error retry classification.

## Non-Goals

- No direct database provider in this task.
- No wallet signing.
