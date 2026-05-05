# T11 Submit, Status, and Chain

## Scope

Implement signed transaction submission, status polling, and transaction
chaining ergonomics.

## Dependencies

- T04 provider client.
- T05 wallet/signers.
- T07 finalization.
- T08 balancing.

## Deliverables

- `CompleteTx.sign.withWallet().complete()`.
- `CompleteTx.submit()` for witness-complete signed `CompleteTx` values.
- `SubmittedTx.awaitStatus({ until: ... })`.
- Chain helper for using produced outputs in subsequent builders.
- Durable admission metadata.

## Acceptance Criteria

- Submit sends native bytes, not Cardano bytes.
- Duplicate admission is surfaced distinctly.
- Tx id conflict is hard failure.
- Status polling distinguishes admission from validation acceptance.
- Chained outputs use authored output indexes.

## Required Tests

- Sign and submit success with mock provider.
- Duplicate submit.
- Conflict submit.
- Status accepted.
- Status rejected with code/detail.
- Chain two locally built transactions and verify dependent outref.

## Non-Goals

- No block finality guarantees unless provider exposes them.
- No automatic rebuild during submit retry.
