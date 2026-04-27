# P0 Blocker 3: Do Not Finalize Local Merge State After L1 Confirmation Failure

## Problem Statement

The merge path can currently mutate local confirmed state after the node fails to
confirm that the merge transaction landed on L1. That violates the production L2
requirement that local confirmed state must be a projection of canonical L1
evidence, not an optimistic guess.

The required production behavior is:

- a merge may be submitted before the node has durable confirmation;
- local merge finalization must not run until the node has verified L1 evidence
  for that exact merge;
- confirmation-unknown states must be durable, observable, and recoverable after
  restart;
- readiness must fail or degrade while a merge is unresolved or locally
  unfinalized;
- recovery must roll forward from verified L1 evidence, never rewrite silently or
  preserve legacy behavior.

This plan is intentionally scoped to `demo/midgard-node` and must not add
compatibility shims, dual behavior, or unsafe operator toggles.

## Current Behavior

The merge transaction builder is owned by
[`src/transactions/state-queue/merge-to-confirmed-state.ts`](../src/transactions/state-queue/merge-to-confirmed-state.ts).
The module comment says the flow applies transactions to `ConfirmedLedgerDB` and
removes the merged block from `BlocksDB` as part of the merge workflow
([lines 9-16](../src/transactions/state-queue/merge-to-confirmed-state.ts#L9-L16)).

The actual merge flow:

- fetches the current confirmed state and first queued block from L1
  ([lines 501-507](../src/transactions/state-queue/merge-to-confirmed-state.ts#L501-L507));
- fetches that block's local transaction payloads from `BlocksDB` and
  `ImmutableDB`
  ([lines 513-520](../src/transactions/state-queue/merge-to-confirmed-state.ts#L513-L520));
- decodes the block transactions into spent outrefs and produced UTxOs before
  submission
  ([lines 541-565](../src/transactions/state-queue/merge-to-confirmed-state.ts#L541-L565));
- builds the updated confirmed-state datum from the first queued block header
  ([lines 605-621](../src/transactions/state-queue/merge-to-confirmed-state.ts#L605-L621));
- completes the merge transaction with local UPLC evaluation enabled
  ([lines 1333-1341](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1333-L1341)).

Submission and confirmation are delegated to `handleSignSubmit`
([lines 1404-1408](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1404-L1408)).
`handleSignSubmit` signs, submits, waits for confirmation with timeout/retry, and
returns the tx hash only after confirmation
([`src/transactions/utils.ts` lines 336-380](../src/transactions/utils.ts#L336-L380)).
Its confirmation timeout is currently 90 seconds with one retry
([`src/transactions/utils.ts` lines 29-31](../src/transactions/utils.ts#L29-L31)).
The helper is too coarse for durable merge recovery because the caller cannot
journal the signed tx, provider submission, and confirmation phases separately.
A crash inside `handleSignSubmit` after provider submission but before the merge
caller observes a `TxConfirmError` would leave only whatever was persisted before
the helper call. Also treat a provider `awaitTx` result of `false` as
confirmation failure; confirmation success must require an explicit trusted
positive result plus the merge-specific L1 datum checks below.

The unsafe behavior is the merge-specific `TxConfirmError` handler:

- `onSubmitFailure` fails the merge effect, so local finalization does not run on
  submit failure
  ([lines 1388-1398](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1388-L1398));
- `onConfirmFailure` only logs the error and returns success
  ([lines 1402-1403](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1402-L1403));
- execution then logs "Merge transaction submitted, updating the db..." and runs
  local finalization
  ([lines 1409-1436](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1409-L1436)).

The local finalization body deletes spent UTxOs from `confirmed_ledger`, inserts
produced UTxOs into `confirmed_ledger`, and clears the merged block from
`BlocksDB`
([lines 1413-1436](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1413-L1436)).
If this runs after confirmation failure and the L1 transaction never landed, the
node's confirmed ledger diverges from canonical state.

The merge fiber repeats this action and catches all causes as warnings
([`src/fibers/merge.ts` lines 67-81](../src/fibers/merge.ts#L67-L81)).
Therefore a confirmation failure currently looks operationally recoverable even
though the local DB may already have advanced.

Existing block commitment code has the pattern merge needs but does not yet have:

- durable pending block finalization records with statuses and a single-active
  invariant
  ([`src/database/pendingBlockFinalizations.ts` lines 30-46](../src/database/pendingBlockFinalizations.ts#L30-L46),
  [lines 113-169](../src/database/pendingBlockFinalizations.ts#L113-L169));
- startup hydration into in-memory recovery refs
  ([`src/commands/listen-startup.ts` lines 211-250](../src/commands/listen-startup.ts#L211-L250));
- readiness failure while local finalization is pending
  ([`src/commands/readiness.ts` lines 65-67](../src/commands/readiness.ts#L65-L67));
- confirmation worker semantics for unresolved, confirmed, and stale submitted
  blocks
  ([`src/workers/confirm-block-commitments.ts` lines 153-207](../src/workers/confirm-block-commitments.ts#L153-L207));
- explicit worker outputs for "submitted but awaiting local finalization" and
  "submitted but awaiting confirmation"
  ([`src/workers/utils/commit-block-header.ts` lines 45-62](../src/workers/utils/commit-block-header.ts#L45-L62)).

## Target Invariants

1. `confirmed_ledger` must only reflect a merge after verified L1 evidence shows
   the merge advanced confirmed state to the expected header.
2. `BlocksDB.clearBlock(headerHash)` must only run after the same verified L1
   evidence.
3. A `TxConfirmError` after merge submission must never be converted into success
   for the purpose of local finalization.
4. Every submitted-or-possibly-submitted merge that has not been locally
   finalized must have a durable pending merge record before the merge effect
   returns.
5. The merge tx hash, body fingerprint, signed tx CBOR, validity interval, and
   expected L1 outputs must be durable before the provider submit call begins.
   This closes the crash window between submission and caller-side error
   handling and gives recovery enough evidence to query or resubmit the exact
   same transaction when that is still valid.
6. At most one active pending merge may exist at a time. A new merge attempt must
   fail closed or no-op while an active pending merge exists.
7. Recovery must be idempotent. Re-running the recovery after crash may either
   observe "already finalized" or complete the remaining local steps; it must not
   duplicate UTxOs, delete unrelated UTxOs, or clear an unrelated block.
8. The recovery proof must bind all of these values:
   `merge_tx_hash`, consumed confirmed-state outref, consumed header-node outref,
   expected merged header hash, expected previous confirmed header hash, expected
   updated confirmed-state datum, expected confirmed-state output address/assets,
   expected settlement output index, expected settlement datum, expected
   settlement asset unit, and the signed tx body fingerprint.
9. Confirmation unknown is not a success state. It is an active degraded state
   that blocks further local merge finalization and is exposed through readiness
   and metrics.
10. Local UPLC evaluation remains mandatory. No merge plan may set
   `.complete({ localUPLCEval: false })`.

## Error Semantics

Introduce a merge-specific error taxonomy in
`merge-to-confirmed-state.ts` or a small adjacent module:

- `MergeSubmitError`: submit failed before the node has evidence that the tx was
  accepted. Preserve current no-local-finalization behavior.
- `MergeConfirmationUnknownError`: submit returned a tx hash, but `awaitTx`
  failed, timed out, or returned an untrusted/no-confirmation outcome. Persist a
  pending merge record, then fail the merge attempt so the fiber logs and repeats
  without local finalization.
- `MergeConfirmationRejectedError`: L1 evidence proves the merge did not land and
  the expected first queued block remains unmerged or was superseded. Mark the
  pending merge abandoned only through explicit recovery logic with evidence.
- `MergeLocalFinalizationError`: L1 evidence proves the merge landed, but local DB
  finalization failed. Keep the pending merge active and fail readiness until
  recovery succeeds.
- `MergeInvariantError`: any mismatch between pending record, L1 datum, header
  hash, outrefs, settlement unit, or local replay payload. This is fail-closed and
  operator-visible; do not auto-abandon.

The immediate bug fix is to replace the current logging-only
`onConfirmFailure` with a failure path. The production fix also records the
durable pending merge before returning the failure. The local finalization block
must be reachable only from the confirmed branch.

## Durable Pending Merge Recovery Model

Add a merge-specific journal instead of overloading
`pending_block_finalizations`. Block finalization and merge finalization have
different safety predicates and different recovery payloads.

Proposed table: `pending_merge_finalizations`.

Required columns:

- `merge_id BYTEA PRIMARY KEY`: deterministic key, preferably
  `hash(expected_header_hash || merge_tx_hash || confirmed_input_tx_hash ||
  confirmed_input_index || header_input_tx_hash || header_input_index)`.
- `expected_header_hash BYTEA NOT NULL`: header being merged into confirmed
  state.
- `previous_confirmed_header_hash BYTEA NOT NULL`: confirmed-state header before
  the merge.
- `merge_tx_hash BYTEA NOT NULL UNIQUE`: tx hash/body hash computed before
  provider submission. It must be present even in `prepared` so recovery can
  inspect L1 after a crash during submit/confirm.
- `signed_tx_cbor BYTEA NOT NULL`: exact signed transaction bytes to support
  deterministic audit and safe re-submit while the validity interval is live.
- `tx_body_cbor BYTEA NOT NULL` or `tx_body_hash BYTEA NOT NULL`: exact body
  fingerprint used to verify the signed tx and the pending record agree.
- `valid_from_unix_time_ms BIGINT NOT NULL`.
- `invalid_hereafter_slot BIGINT` or equivalent nullable upper-bound field if
  the transaction body contains one.
- `confirmed_input_tx_hash BYTEA NOT NULL`.
- `confirmed_input_output_index INTEGER NOT NULL`.
- `header_input_tx_hash BYTEA NOT NULL`.
- `header_input_output_index INTEGER NOT NULL`.
- `expected_confirmed_output_index INTEGER NOT NULL`.
- `expected_confirmed_datum_hash BYTEA` or `expected_confirmed_datum_cbor BYTEA
  NOT NULL`: enough to verify the L1 output exactly.
- `expected_confirmed_output_cbor BYTEA NOT NULL` or explicit
  address/assets/datum fields for the confirmed-state output.
- `expected_settlement_output_index INTEGER NOT NULL`.
- `expected_settlement_datum_cbor BYTEA NOT NULL`.
- `expected_settlement_output_cbor BYTEA NOT NULL` or explicit
  address/assets/datum fields for the settlement output.
- `expected_settlement_unit TEXT NOT NULL`.
- `block_tx_count INTEGER NOT NULL`.
- `spent_outref_count INTEGER NOT NULL`.
- `produced_utxo_count INTEGER NOT NULL`.
- `status TEXT NOT NULL`.
- `last_error TEXT`.
- `abandonment_evidence JSONB` or equivalent nullable structured evidence field
  for explicit `abandoned` transitions.
- `observed_confirmed_at_ms BIGINT`.
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.

Statuses:

- `prepared`: local UPLC evaluation has passed and the exact signed tx, tx hash,
  validity interval, expected outputs, consumed outrefs, and replay payload
  fingerprints are persisted before provider submission.
- `submitted_confirmation_unknown`: submit completed but confirmation failed or
  timed out; no local finalization has run.
- `confirmed_local_finalization_pending`: L1 evidence is verified, local
  finalization has not completed.
- `finalized`: verified L1 evidence and local DB finalization are complete.
- `abandoned`: explicit recovery proved the submitted tx did not land and the
  canonical state advanced incompatibly. This must require evidence and an audit
  event entry.
- `invariant_failed`: recovery found a mismatch that requires operator action.

Indexes and constraints:

- unique partial index allowing only one active row in statuses `prepared`,
  `submitted_confirmation_unknown`, `confirmed_local_finalization_pending`, or
  `invariant_failed`;
- unique index on `merge_tx_hash`;
- check constraint for valid statuses;
- indexes on `status`, `expected_header_hash`, and `updated_at`;
- no foreign key to `blocks.header_hash` if `BlocksDB.clearBlock` removes that
  row during finalization; keep the pending record as the audit trail.

Optional child tables:

- `pending_merge_spent_outrefs(merge_id, tx_hash, output_index, ordinal)`;
- `pending_merge_produced_utxos(merge_id, out_ref, cbor, ordinal)`.

Required audit table:

- `pending_merge_finalization_events(merge_id, from_status, to_status,
  event_type, evidence_json, created_at)`: append-only transition history for
  prepared, submitted, confirmed, finalized, abandoned, and invariant-failed
  events. Do not rely only on mutable `last_error` or process logs for evidence.

The preferred first implementation can avoid copying full UTxO payloads if
recovery always recomputes from immutable block tx payloads and verifies counts
and roots against the pending record. Copying produced UTxOs is safer for
recoverability if future retention could remove immutable payloads before merge
recovery. If retention remains uncertain, store the produced UTxO CBOR snapshot
in the pending merge journal.

## Confirmation Unknown Handling

On a merge attempt:

1. Fetch confirmed state and first queued block from L1.
2. Decode block payloads and derive `preflightSpentOutRefs` and
   `preflightProducedUTxOs`.
3. Build the merge tx with `localUPLCEval: true`.
4. Sign the transaction or extend the submit helper with merge lifecycle hooks
   so the exact signed tx CBOR is available to the journal before submit.
5. Persist a `prepared` pending merge record with the tx hash, signed tx CBOR,
   body fingerprint, validity interval, expected consumed inputs, reference-input
   set, output indexes, expected output CBOR/datum fingerprints, settlement unit,
   replay counts, and block header identity before provider submission.
6. Submit the exact signed transaction represented by the prepared row. Do not
   call the current all-in-one `handleSignSubmit` from merge unless it has been
   refactored so journal writes happen before provider submit and immediately
   after submit returns.
7. If `TxSubmitError` occurs before provider acceptance is known, keep the
   prepared record and classify it through recovery unless the provider error is
   deterministic proof that the transaction was rejected before admission. Only
   then mark the row `abandoned` with evidence or delete a never-submitted
   prepared row through an explicit audited path. Do not mutate confirmed ledger
   or `BlocksDB`.
8. If submit returns accepted/submitted but confirmation fails, or if `awaitTx`
   resolves to `false`, update the row to
   `submitted_confirmation_unknown`, record `last_error` and a transition event,
   set an in-memory pending-merge flag, emit metrics, and fail the effect with
   `MergeConfirmationUnknownError`.
9. If confirmation returns successfully, verify the returned tx hash matches the
   pending row and verify the merge-specific L1 evidence before marking
   `confirmed_local_finalization_pending`; then run local finalization.

`handleSignSubmit` may remain available to flows with weaker recovery needs, but
merge must use a journal-aware submit path. That path must preserve the existing
early-validity retry behavior from `submitSignedTxWithRecovery` without
collapsing submit and confirm into an unjournaled critical section.

Recovery loop behavior:

- Poll active pending merge rows.
- For `prepared`, first inspect L1 by `merge_tx_hash` and by state-queue datum.
  If the tx is already reflected on L1, advance to
  `confirmed_local_finalization_pending`. If the validity interval is still live
  and the signed tx CBOR exactly matches the prepared row, a recovery action may
  re-submit that same signed tx. If L1 proves the tx did not land and can no
  longer land, abandon only with explicit evidence. If provider state is
  ambiguous, keep the row active.
- For `submitted_confirmation_unknown`, query L1 for the tx hash and also inspect
  the state queue confirmed-state node. Providers can fail to find a tx by hash
  even when state has advanced; state-queue datum verification is the stronger
  protocol predicate.
- Treat L1 as confirmed only when the confirmed-state node now has
  `headerHash == expected_header_hash`,
  `prevHeaderHash == previous_confirmed_header_hash`, the expected confirmed
  output address/assets/datum, the expected settlement output datum/assets, and
  the old header node is no longer the first queued block in the way the merge
  transaction required.
- Use the same finality/stability predicate chosen for production block
  confirmation before local finalization. A mere provider acknowledgement is not
  enough if it can still be rolled back under the configured L1 stability window.
- If confirmed, transition to `confirmed_local_finalization_pending` and run
  local finalization.
- If not found and the transaction validity interval may still be live, keep the
  row active and retry.
- If L1 proves canonical state advanced past the expected header without that
  merge, transition only through explicit `abandoned` recovery with evidence in
  `last_error` or an audit table.
- If provider state is ambiguous, keep `submitted_confirmation_unknown`; do not
  finalize or abandon.

## Local Finalization Gating

Refactor local finalization into a function that requires a verified pending
merge record and verified L1 observation:

```text
verified L1 merge evidence
  -> load/recompute replay payload
  -> compare pending record fingerprints/counts
  -> finalize confirmed ledger and BlocksDB
  -> mark pending merge finalized
```

The finalizer must:

- run only for `confirmed_local_finalization_pending`;
- rederive spent and produced UTxOs from the block payloads or use the persisted
  snapshot;
- validate that the current local `BlocksDB` entry still corresponds to
  `expected_header_hash` unless the operation is an idempotent retry after
  `BlocksDB.clearBlock`;
- make the DB transition transactional where PostgreSQL tables are involved:
  confirmed ledger clears/inserts, `BlocksDB.clearBlock`, and journal status
  update should commit together;
- avoid parallel writes that can leave `confirmed_ledger`, `blocks`, and the
  journal in contradictory states;
- treat partial local finalization as recoverable only when the journal remains
  active and enough payload is durable to re-run or verify completion.

If existing confirmed-ledger operations are not idempotent, add idempotent
variants or perform precondition checks in the same transaction. For example:

- deleting a missing spent outref is acceptable only if the produced set and
  journal status prove a previous identical recovery attempt completed far enough;
- inserting an already-present produced UTxO must verify exact CBOR/address/assets
  equality, not silently overwrite;
- clearing an already-cleared block must verify that the pending merge is the
  reason it was cleared.

## Schema Changes

This fix needs schema support. Because the broader readiness report identifies
the lack of versioned migrations as a separate blocker, implement this schema
through the repository's chosen migration system once that blocker is addressed.
Until then, the implementation should still be written as an explicit,
auditable schema addition rather than an implicit silent rewrite.

Minimum schema deliverables:

- `pendingMergeFinalizations.ts` database module with `createTable`,
  `prepare`, `markSubmittedConfirmationUnknown`,
  `markConfirmedLocalFinalizationPending`, `markFinalized`,
  `markAbandonedWithEvidence`, `markInvariantFailed`, `appendEvent`, and
  `retrieveActive`.
- migration or explicit startup schema creation for `pending_merge_finalizations`
  plus the append-only transition event table and optional payload child tables.
- database init wiring in
  [`src/database/init.ts`](../src/database/init.ts#L31-L43).
- startup hydration that sets merge-specific globals/readiness input from any
  active pending merge record.

Do not alter `pending_block_finalizations` semantics to cover merge unless the
implementation first proves the combined state machine remains clear and
auditable. A separate table is safer and easier to audit.

## Observability And Readiness

Add merge-specific operational signals:

- counter: `merge_confirmation_unknown_count`;
- counter: `merge_pending_recovery_count`;
- counter: `merge_recovery_success_count`;
- counter: `merge_recovery_invariant_failure_count`;
- gauge: `pending_merge_finalization_active`;
- gauge: `pending_merge_finalization_age_ms`;
- gauge: `pending_merge_local_finalization_pending`;
- gauge or status-labeled metric exposing active pending merge status
  (`prepared`, `submitted_confirmation_unknown`,
  `confirmed_local_finalization_pending`, `invariant_failed`);
- structured logs with `merge_tx_hash`, `expected_header_hash`,
  `previous_confirmed_header_hash`, consumed input outrefs, status transition,
  and error class.

Readiness must fail while any active pending merge is unresolved or locally
unfinalized. Extend the readiness input and reasons with:

- `merge_confirmation_unknown`;
- `merge_prepared_unresolved`;
- `merge_local_finalization_pending`;
- `merge_invariant_failed`.

Startup must hydrate active pending merge state before the merge fiber can submit
another merge. The merge fiber should no-op or run recovery when a pending merge
exists; it must not build a new merge on top of unresolved local state.

## Tests And Fault Injection

Unit tests:

- `onConfirmFailure` path persists a pending merge and does not call
  `ConfirmedLedgerDB.clearUTxOs`, `ConfirmedLedgerDB.insertMultiple`, or
  `BlocksDB.clearBlock`.
- `awaitTx` resolving `false` is treated as confirmation unknown and does not
  run local finalization.
- `TxSubmitError` path keeps or abandons the active `prepared` record according
  to explicit provider evidence; it never silently deletes a possibly submitted
  tx hash.
- pending merge state machine allows only valid transitions and enforces one
  active pending merge.
- readiness returns not ready for active
  `prepared`, `submitted_confirmation_unknown`,
  `confirmed_local_finalization_pending`, and `invariant_failed` merge states.
- recovery verification rejects mismatched header hash, previous confirmed header
  hash, confirmed output datum/assets, settlement output datum/assets, consumed
  outrefs, settlement unit, tx body fingerprint, signed tx CBOR, and replay
  counts.

Integration/emulator tests:

- crash after `prepared` insert but before provider submit; recovery either
  resubmits the exact signed tx while valid or abandons only with explicit
  never-landed evidence after it can no longer land.
- crash after provider submit but before marking
  `submitted_confirmation_unknown`; recovery uses the prepared tx hash and L1
  state proof to avoid a second distinct merge.
- inject `lucid.awaitTx` timeout after successful submit; assert local confirmed
  ledger and `BlocksDB` remain unchanged, pending merge is active, and readiness
  fails.
- restart after confirmation unknown; hydrate pending merge; observe L1 confirmed
  state; run recovery; assert local finalization completes and readiness recovers.
- restart after L1 confirmed but before local finalization; recovery finalizes
  idempotently.
- crash/fail after confirmed-ledger clears but before inserts; recovery either
  rolls forward with exact verification or fails closed with
  `merge_invariant_failed`.
- provider cannot find tx by hash but state-queue confirmed datum has advanced;
  recovery uses state proof and finalizes.
- canonical state advances incompatibly; pending merge is not finalized and is
  only abandoned through explicit evidence.
- repeated merge fiber ticks during active pending merge do not submit a second
  merge.

Fault injection points:

- after `prepared` record insert and before submit;
- after submit returns and before confirmation await;
- inside `awaitTx` timeout/failure;
- after marking `confirmed_local_finalization_pending`;
- after each local finalization batch;
- after `BlocksDB.clearBlock`;
- before marking pending merge `finalized`.

## Rollout Steps

1. Add the pending merge journal schema and database module.
2. Add merge readiness inputs, startup hydration, and metrics with no behavior
   change except reporting active pending merge rows if present.
3. Split or extend the submit helper so merge can persist the signed tx, tx hash,
   and prepared journal row before provider submission and record submit/confirm
   transitions immediately.
4. Refactor local merge finalization into a separately callable, recovery-safe
   function.
5. Persist `prepared` before submit and gate new merge attempts when an active
   pending merge exists.
6. Change `TxConfirmError` and explicit `awaitTx === false` handling to persist
   `submitted_confirmation_unknown` and fail the merge effect before local
   finalization.
7. Add recovery loop/action for active pending merge rows.
8. Make local finalization transition journaled and transactional.
9. Add unit, integration, and fault-injection coverage.
10. Run the emulator deposit/commit/merge flow and recovery tests.
11. Enable readiness failure for pending merge states as a hard production
    default.

## Risks And Open Questions

- The current local finalization uses multiple DB operations outside a single
  explicit transaction. This plan should be coordinated with the atomic/journaled
  local-finalization blocker from the production readiness report.
- Recovery needs a robust way to verify the exact confirmed-state output datum on
  L1. If provider APIs do not expose enough tx/output detail by tx hash, recovery
  must verify via state-queue traversal and datum decoding.
- The pending journal must retain enough replay payload to survive retention. If
  `ImmutableDB` or `BlocksDB` can be swept before merge recovery, store produced
  UTxO snapshots in the pending merge tables.
- Validity interval expiry creates an ambiguous window: a tx may be absent
  because it expired, because the provider is stale, or because the tx landed and
  query failed. Recovery must keep ambiguity active until canonical state proves
  the outcome.
- The implementation must define whether merge recovery runs in the existing
  merge fiber, a new merge-confirmation fiber, or startup plus periodic merge
  recovery. A dedicated recovery action called before normal merge submission is
  simplest and avoids another always-on worker.
- Existing metrics use Effect metrics in-process. If operators need durable audit
  history, add an audit table for pending merge transitions instead of relying
  only on logs.

## Concrete Checklist

- [ ] Add `PendingMergeFinalizationsDB` with schema, status constants, and
      transition helpers.
- [ ] Add one-active-pending-merge database constraint.
- [ ] Persist signed tx CBOR, tx hash, tx body fingerprint, validity interval,
      expected outputs, and `prepared` pending merge records before L1
      submission.
- [ ] Split or extend the submit helper so merge has journaled submit and
      confirmation phases.
- [ ] Replace logging-only `TxConfirmError` handling with durable
      `submitted_confirmation_unknown` plus effect failure.
- [ ] Treat explicit `awaitTx === false` as confirmation unknown.
- [ ] Ensure local finalization is unreachable after confirmation failure.
- [ ] Recover `prepared` rows without submitting a second distinct merge tx.
- [ ] Refactor local merge finalization into an idempotent recovery-safe helper.
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
