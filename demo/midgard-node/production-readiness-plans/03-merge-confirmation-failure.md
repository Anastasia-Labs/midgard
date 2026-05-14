# P0 Blocker 3: Do Not Finalize Local Merge State After L1 Confirmation Failure

Status: immediate local-divergence bug fixed; durable confirmation-unknown
recovery still open.

The previous version of this document described a current bug where the merge
path caught `TxConfirmError`, logged it, returned success, and then mutated
`confirmed_ledger` and `blocks`. That is no longer the current implementation.
`TxConfirmError` now fails the merge effect before local finalization.

The remaining production gap is durable merge confirmation recovery: the merge
path still uses the generic all-in-one `handleSignSubmit`, does not persist a
pending merge record before provider submission, and has no startup/readiness
model for a merge whose submission may have reached L1 but whose confirmation
was not durably observed.

## Current Behavior

The merge transaction builder is owned by
[`src/transactions/state-queue/merge-to-confirmed-state.ts`](../src/transactions/state-queue/merge-to-confirmed-state.ts).
The module comment still says the flow applies transactions to
`ConfirmedLedgerDB` and removes the merged block from `BlocksDB` before
submission
([lines 7-16](../src/transactions/state-queue/merge-to-confirmed-state.ts#L7-L16)).
That comment is stale. The current executable flow submits and confirms first,
then runs local finalization.

Submission and confirmation are delegated to `handleSignSubmit`
([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1431-1435](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1431-L1435)).
`handleSignSubmit` signs, submits, waits for confirmation with timeout/retry,
reconciles wallet UTxOs, pauses, and returns the tx hash
([`src/transactions/utils.ts` lines 336-380](../src/transactions/utils.ts#L336-L380)).
Its confirmation timeout constants are currently 90 seconds, one retry, and a
5 second poll interval
([`src/transactions/utils.ts` lines 29-31](../src/transactions/utils.ts#L29-L31)).

Current failure handling:

- `onSubmitFailure` fails the merge effect, so local finalization does not run
  on submit failure
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1403-1413](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1403-L1413)).
- `onConfirmFailure` logs and then fails with a new `TxConfirmError`; it no
  longer returns success
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1417-1429](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1417-L1429)).
- The "Merge transaction submitted, updating the db..." branch is reached only
  after `handleSignSubmit` succeeds
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1431-1438](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1431-L1438)).

Current local finalization:

- starts a `MutationJobsDB.Kind.ConfirmedMergeFinalization` job with header hash
  and replay counts
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1440-1452](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1440-L1452));
- clears spent confirmed UTxOs, inserts produced confirmed UTxOs, and clears the
  merged block from `BlocksDB` inside one SQL transaction
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1453-1486](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1453-L1486));
- marks the mutation job completed after the SQL transaction
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` line 1487](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1487));
- marks the job failed if local finalization fails
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1488-1495](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1488-L1495)).

Startup refuses unfinished local mutation jobs, including failed or running
`confirmed_merge_finalization` jobs
([`src/commands/listen.ts` lines 76-91](../src/commands/listen.ts#L76-L91)).
Readiness exposes unfinished mutation jobs through
`unfinished_local_mutation_jobs:<count>`
([`src/commands/listen-router.ts` lines 457-458](../src/commands/listen-router.ts#L457-L458),
[`src/commands/listen-router.ts` lines 521-532](../src/commands/listen-router.ts#L521-L532)).

There is currently no `PendingMergeFinalizationsDB`, `pending_merge_*` schema,
merge confirmation-unknown status, or merge recovery fiber.

## What Has Landed

The old immediate local divergence path has been closed for `TxConfirmError`:

```text
handleSignSubmit succeeds
  -> local merge finalization may run

handleSignSubmit fails with TxSubmitError or TxConfirmError
  -> merge effect fails
  -> local merge finalization is not reached
```

Confirmed merge local finalization is also safer than before because its SQL
domain effects are transactional and are wrapped by `local_mutation_jobs`. A
crash or failure during local finalization should now leave an unfinished local
mutation job that blocks startup/readiness instead of silently continuing as if
finalization completed.

## Remaining Production Gap

The remaining problem is not the old "catch confirmation failure and continue"
bug. The remaining problem is the absence of a durable pending merge recovery
model.

The unsafe crash/ambiguity windows are:

1. The node signs and submits inside `handleSignSubmit` without first persisting
   a merge-specific pending record containing the signed tx, tx hash, expected
   inputs, expected outputs, header hashes, validity interval, and replay
   counts.
2. A provider may accept the tx, then the process may crash or `awaitTx` may
   time out before the merge caller records durable state.
3. After restart, the node has no pending merge row telling it whether to query
   L1 for the exact tx, re-submit the exact signed tx while valid, finalize
   locally after verified L1 evidence, or fail closed as invariant-failed.
4. Readiness has no merge-specific reason for confirmation unknown or local
   merge finalization pending, apart from generic unfinished
   `local_mutation_jobs` after local finalization has already started.

Also note a generic helper gap: `handleSignSubmit` awaits a boolean result from
`lucid.awaitTx`, but the current helper does not branch on an explicit `false`
result before returning success
([`src/transactions/utils.ts` lines 344-375](../src/transactions/utils.ts#L344-L375)).
The merge-specific submit path should treat any non-positive confirmation
result as confirmation unknown and must not enter local finalization.

## Target Invariants

1. `confirmed_ledger` must only reflect a merge after verified L1 evidence shows
   the merge advanced confirmed state to the expected header.
2. `BlocksDB.clearBlock(headerHash)` must only run after the same verified L1
   evidence.
3. A `TxConfirmError` after merge submission must never be converted into
   success for the purpose of local finalization. This immediate fix has landed.
4. Every submitted-or-possibly-submitted merge that has not been locally
   finalized must have a durable pending merge record before the merge effect
   returns.
5. The merge tx hash, body fingerprint, signed tx CBOR, validity interval, and
   expected L1 outputs must be durable before the provider submit call begins.
6. At most one active pending merge may exist at a time.
7. Recovery must be idempotent. Re-running recovery after crash may either
   observe already-finalized state or complete the remaining local steps.
8. The recovery proof must bind all of these values: merge tx hash, consumed
   confirmed-state outref, consumed header-node outref, expected merged header
   hash, expected previous confirmed header hash, expected updated confirmed
   state datum, expected confirmed-state output, expected settlement output,
   expected settlement unit, and signed tx body fingerprint.
9. Confirmation unknown is not a success state. It is an active degraded state
   that blocks further local merge finalization and is exposed through readiness
   and metrics.
10. Local UPLC evaluation remains mandatory. No merge plan may set
    `.complete({ localUPLCEval: false })`.

## Durable Pending Merge Recovery Model

Add a merge-specific journal instead of overloading
`pending_block_finalizations`. Block finalization and merge finalization have
different safety predicates and different recovery payloads.

Proposed table: `pending_merge_finalizations`.

Required columns:

- `merge_id BYTEA PRIMARY KEY`: deterministic key, preferably
  `hash(expected_header_hash || merge_tx_hash || confirmed_input_tx_hash ||
  confirmed_input_index || header_input_tx_hash || header_input_index)`.
- `expected_header_hash BYTEA NOT NULL`.
- `previous_confirmed_header_hash BYTEA NOT NULL`.
- `merge_tx_hash BYTEA NOT NULL UNIQUE`.
- `signed_tx_cbor BYTEA NOT NULL`.
- `tx_body_cbor BYTEA NOT NULL` or `tx_body_hash BYTEA NOT NULL`.
- `valid_from_unix_time_ms BIGINT NOT NULL`.
- `invalid_hereafter_slot BIGINT` or equivalent nullable upper-bound field.
- `confirmed_input_tx_hash BYTEA NOT NULL`.
- `confirmed_input_output_index INTEGER NOT NULL`.
- `header_input_tx_hash BYTEA NOT NULL`.
- `header_input_output_index INTEGER NOT NULL`.
- `expected_confirmed_output_index INTEGER NOT NULL`.
- `expected_confirmed_datum_cbor BYTEA NOT NULL` or a datum hash plus enough
  bytes to verify exactly.
- `expected_confirmed_output_cbor BYTEA NOT NULL`.
- `expected_settlement_output_index INTEGER NOT NULL`.
- `expected_settlement_datum_cbor BYTEA NOT NULL`.
- `expected_settlement_output_cbor BYTEA NOT NULL`.
- `expected_settlement_unit TEXT NOT NULL`.
- `block_tx_count INTEGER NOT NULL`.
- `spent_outref_count INTEGER NOT NULL`.
- `produced_utxo_count INTEGER NOT NULL`.
- `status TEXT NOT NULL`.
- `last_error TEXT`.
- `abandonment_evidence JSONB`.
- `observed_confirmed_at_ms BIGINT`.
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.

Statuses:

- `prepared`: local UPLC evaluation has passed and the exact signed tx, tx hash,
  validity interval, expected outputs, consumed outrefs, and replay payload
  fingerprints are persisted before provider submission.
- `submitted_confirmation_unknown`: submit completed or may have completed, but
  confirmation failed, timed out, crashed, or returned a non-positive result; no
  local finalization has run.
- `confirmed_local_finalization_pending`: L1 evidence is verified, local
  finalization has not completed.
- `finalized`: verified L1 evidence and local DB finalization are complete.
- `abandoned`: explicit recovery proved the submitted tx did not land and the
  canonical state advanced incompatibly. This must require evidence and an audit
  event.
- `invariant_failed`: recovery found a mismatch that requires operator action.

Required constraints:

- unique partial index allowing only one active row in `prepared`,
  `submitted_confirmation_unknown`, `confirmed_local_finalization_pending`, or
  `invariant_failed`;
- unique index on `merge_tx_hash`;
- status check constraint;
- indexes on `status`, `expected_header_hash`, and `updated_at`;
- no foreign key to `blocks.header_hash` if `BlocksDB.clearBlock` removes that
  row during finalization.

Required audit table:

- `pending_merge_finalization_events(merge_id, from_status, to_status,
  event_type, evidence_json, created_at)`.

## Confirmation Unknown Handling

On a merge attempt:

1. Fetch confirmed state and first queued block from L1.
2. Decode block payloads and derive spent outrefs and produced UTxOs.
3. Build the merge tx with `localUPLCEval: true`.
4. Sign the transaction or extend the submit helper with merge lifecycle hooks
   so the exact signed tx CBOR is available to the journal before submit.
5. Persist a `prepared` pending merge record with tx hash, signed tx CBOR, body
   fingerprint, validity interval, expected consumed inputs, reference-input
   set, output indexes, expected output CBOR/datum fingerprints, settlement
   unit, replay counts, and block header identity before provider submission.
6. Submit the exact signed transaction represented by the prepared row.
7. If submit is proven rejected before provider acceptance, abandon only with
   explicit evidence. If acceptance is ambiguous, keep the row active.
8. If confirmation fails, times out, crashes, or returns `false`, update the row
   to `submitted_confirmation_unknown`, record error evidence, emit metrics, and
   fail the merge effect before local finalization.
9. If confirmation returns successfully, verify the tx hash and merge-specific
   L1 state evidence before marking
   `confirmed_local_finalization_pending`; then run local finalization.

Recovery loop behavior:

- For `prepared`, inspect L1 by `merge_tx_hash` and by state-queue datum. If
  validity is still live, re-submit only the exact signed tx from the journal.
- For `submitted_confirmation_unknown`, query by tx hash and verify state-queue
  confirmed-state datum. State proof is stronger than provider tx lookup.
- Treat L1 as confirmed only when the confirmed-state node has the expected
  header hash, previous header hash, output datum/assets, settlement output, and
  queue topology.
- If confirmed, transition to `confirmed_local_finalization_pending` and run the
  local finalizer.
- If not found and validity may still be live, keep the row active and retry.
- If canonical state advanced incompatibly, abandon only through explicit
  evidence.
- If provider state is ambiguous, keep `submitted_confirmation_unknown`; do not
  finalize or abandon.

## Local Finalization Gating

Refactor local finalization into a recovery-callable function that requires a
verified pending merge record and verified L1 observation:

```text
verified L1 merge evidence
  -> load/recompute replay payload
  -> compare pending record fingerprints/counts
  -> finalize confirmed ledger and BlocksDB in one SQL transaction
  -> mark pending merge finalized
  -> mark local mutation job completed
```

The current SQL transaction around confirmed ledger and block cleanup has
landed, but the function is not yet driven by a pending merge journal.

The finalizer must:

- run only for `confirmed_local_finalization_pending`;
- rederive spent and produced UTxOs from immutable block payloads or use the
  persisted snapshot;
- verify that current local `BlocksDB` rows still correspond to
  `expected_header_hash`, unless this is an idempotent retry after a verified
  previous clear;
- verify exact CBOR/address/assets equality on already-present produced UTxOs;
- verify missing spent rows only when the pending merge state proves an
  identical previous attempt progressed far enough;
- mark pending merge `finalized` in the same transactional local-finalization
  boundary or in a provably recoverable next step.

## Observability And Readiness

Current landed observability:

- merge local finalization failures increment
  `merge_local_finalization_failures_total`
  ([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1496-1509](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1496-L1509));
- unfinished local mutation jobs appear in `/readyz` as
  `unfinished_local_mutation_jobs:<count>`.

Still required:

- `merge_confirmation_unknown_count`;
- `merge_pending_recovery_count`;
- `merge_recovery_success_count`;
- `merge_recovery_invariant_failure_count`;
- `pending_merge_finalization_active`;
- `pending_merge_finalization_age_ms`;
- `pending_merge_local_finalization_pending`;
- status-labeled metric for pending merge state;
- structured logs with `merge_tx_hash`, `expected_header_hash`,
  `previous_confirmed_header_hash`, consumed input outrefs, status transition,
  and error class.

Readiness must fail while any active pending merge is unresolved or locally
unfinalized. Add reasons:

- `merge_confirmation_unknown`;
- `merge_prepared_unresolved`;
- `merge_local_finalization_pending`;
- `merge_invariant_failed`.

Startup must hydrate active pending merge state before the merge fiber can
submit another merge. The merge fiber should no-op or run recovery when a
pending merge exists; it must not build a new merge on top of unresolved local
state.

## Tests And Fault Injection

Existing coverage:

- merge preflight/error-code tests cover error-code extraction, missing payload
  diagnostics, redeemer seed index derivation, and native tx payload decoding
  ([`tests/merge-error-codes.test.ts` lines 28-175](../tests/merge-error-codes.test.ts#L28-L175));
- emulator happy path covers deposit-only commit, confirmation, local
  finalization recovery, merge to confirmed state, burned header UTxO absence,
  and settlement creation
  ([`tests/deposit-flow-emulator.test.ts` lines 1660-1845](../tests/deposit-flow-emulator.test.ts#L1660-L1845)).

Missing coverage:

- `TxConfirmError` path does not call `ConfirmedLedgerDB.clearUTxOs`,
  `ConfirmedLedgerDB.insertMultiple`, or `BlocksDB.clearBlock`;
- explicit `awaitTx === false` is treated as confirmation unknown and does not
  run local finalization;
- pending merge state machine allows only valid transitions and enforces one
  active pending merge;
- readiness returns not ready for active `prepared`,
  `submitted_confirmation_unknown`, `confirmed_local_finalization_pending`, and
  `invariant_failed` merge states;
- restart after confirmation unknown hydrates pending merge and recovers from
  verified L1 evidence;
- crash after provider submit but before confirmation observation does not cause
  a second distinct merge tx;
- crash after confirmed-ledger clears but before inserts either rolls forward
  with exact verification or fails closed;
- provider cannot find tx by hash but state-queue confirmed datum has advanced;
  recovery uses state proof and finalizes;
- canonical state advances incompatibly; pending merge is not finalized and is
  only abandoned through explicit evidence.

## Concrete Checklist

Landed:

- [x] Replace logging-only `TxConfirmError` handling with effect failure before
      local finalization.
- [x] Ensure the normal local finalization branch is unreachable after
      `TxConfirmError`.
- [x] Keep submit failure as a no-local-finalization failure path.
- [x] Wrap confirmed merge local finalization in `MutationJobsDB`.
- [x] Run confirmed-ledger clear, confirmed-ledger insert, and
      `BlocksDB.clearBlock` in one SQL transaction.
- [x] Fail startup/readiness when a confirmed merge finalization job remains
      unfinished.
- [x] Preserve mandatory local UPLC evaluation in merge transaction completion.

Still required:

- [ ] Add `PendingMergeFinalizationsDB` with schema, status constants, and
      transition helpers.
- [ ] Add one-active-pending-merge database constraint.
- [ ] Persist signed tx CBOR, tx hash, tx body fingerprint, validity interval,
      expected outputs, and `prepared` pending merge records before L1
      submission.
- [ ] Split or extend the submit helper so merge has journaled submit and
      confirmation phases.
- [ ] Treat explicit `awaitTx === false` as confirmation unknown.
- [ ] Persist `submitted_confirmation_unknown` before failing confirmation
      unknown attempts.
- [ ] Recover `prepared` rows without submitting a second distinct merge tx.
- [ ] Refactor local merge finalization into an idempotent recovery-safe helper
      driven by verified pending merge records.
- [ ] Verify L1 evidence before `confirmed_ledger` mutation or
      `BlocksDB.clearBlock`.
- [ ] Mark pending merge `finalized` only after local finalization commits.
- [ ] Hydrate active pending merge state on startup.
- [ ] Block or recover before any new merge attempt when active pending merge
      exists.
- [ ] Add readiness reasons for pending merge states.
- [ ] Add merge confirmation/recovery metrics and structured logs.
- [ ] Add unit tests for error semantics and state transitions.
- [ ] Add emulator/fault-injection tests for confirmation timeout, restart
      recovery, partial local finalization, provider ambiguity, and incompatible
      canonical advancement.
- [ ] Document operator recovery behavior and required evidence for abandoning a
      pending merge.
