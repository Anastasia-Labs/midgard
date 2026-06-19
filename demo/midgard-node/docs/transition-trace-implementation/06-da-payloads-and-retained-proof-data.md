# Task 06: DA Payloads And Retained Proof Data

## Goal

Upgrade DA payloads and local pending-finalization storage so challengers can
reconstruct source roots, trace roots, event-to-step roots, and proof witnesses
from data keyed by `header_hash`.

This task is complete when DA committee validation checks every new root and
count in the header against the advertised payload.

## Files To Update

SDK DA schema:

- `demo/midgard-sdk/src/da-payload.ts`

Node DA payload construction:

- `demo/midgard-node/src/workers/commit-block-header/da-payload.ts`
- `demo/midgard-node/src/database/daPayloads.ts`
- `demo/midgard-node/src/database/migrations/sql/0007_da_payloads.sql`
- `demo/midgard-node/src/database/pendingBlockFinalizations.ts`
- `demo/midgard-node/src/workers/commit-block-header/pending-journal.ts`

DA committee validation:

- `demo/da-committee-node/src/da/payload.ts`
- `demo/da-committee-node/src/domain.ts`
- `demo/da-committee-node/src/coordinator/submitter-reconciler.ts`
- DA committee database migrations, if present.

Tests:

- `demo/midgard-node/tests/da-payload.test.ts`
- `demo/midgard-node/tests/da-attestation.test.ts`
- `demo/midgard-node/tests/e2e-da-gates.test.ts`
- `demo/da-committee-node/tests/payload.test.ts`

## Payload Schema

Introduce a new DA payload version:

```text
DaPayloadBodyV2 {
  header_hash
  header

  utxos
  withdrawals
  forced_transactions
  transactions
  deposits

  transition_trace
  event_to_step

  counts
}
```

Retain full enough source payloads to derive the committed roots:

- UTxO members for the final UTxO root or enough delta data plus base proof to
  reconstruct it deterministically.
- Withdrawal source events.
- Forced transaction source events.
- Normal L2 transaction compact values and full tx CBOR where runtime or proof
  construction requires it.
- Deposit source events.
- Transition trace members.
- Event-to-step members.

## Root Checks

DA validators must recompute and compare:

```text
utxos_root
withdrawals_root
forced_transactions_root
transactions_root
deposits_root
transition_trace_root
event_to_step_root
```

They must also compare:

```text
withdrawal_count
forced_transaction_count
l2_transaction_count
deposit_count
total_event_count
transition_step_count
```

## Database Changes

Pending finalization and DA payload storage need columns or JSON fields for:

- `forced_transactions_root`
- `transition_trace_root`
- `event_to_step_root`
- all event counts
- retained forced transaction members
- retained transition trace members
- retained event-to-step members

Prefer normalized member tables for proof-critical data if the existing pending
finalization journal uses normalized tables for other roots.

## Implementation Notes

- DA payloads remain keyed by `header_hash`.
- DA validation must fail closed on missing trace data.
- Do not rely on operator HTTP endpoints as the only way to fetch proof data.
- If full transaction CBOR is retained separately from `MidgardTxCompact`, the
  root must still be computed from the compact value used by `transactions_root`.

## Tests And Verification

Add tests for:

- DA payload v2 root recomputation
- mismatch on each new root
- mismatch on each count
- missing trace member
- missing event-to-step member
- forced transaction member included in payload
- DA committee rejects a payload whose header root differs from recomputed trace
  root

Run:

```sh
cd demo && pnpm run test -- da-payload
cd demo && pnpm run test -- da-attestation
cd demo && pnpm run test -- e2e-da-gates
cd demo && pnpm run typecheck
```

## Exit Criteria

- A challenger can reconstruct all committed roots from DA.
- DA committee attestations cover trace commitments, not just old source roots.
- Header, payload, and retained local journal data have the same root surface.

