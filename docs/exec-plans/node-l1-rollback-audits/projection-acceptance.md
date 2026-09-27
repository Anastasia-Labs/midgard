# §4.D projection acceptance criteria (node-l1-rollback-redesign.md)

Path convention: paths without a prefix are under `demo/midgard-node/src/`; test paths are under `demo/midgard-node/tests/`.
Data kinds (plan §4.A–C): **A** = slot-tagged L1 facts (rolled back by one truncation). **B** = append-only operator intents. **C** = content-addressed data (foreign DA payloads, tx CBOR).
Heads: **local**, **landed**, **settled** (depth ≥ `confirmationDepth`: 30 in preprod-public/mainnet, 3 in local-devnet/preprod-testing, `config/deployments/*.yaml:4`), **confirmed** (merged and at depth).
Cost classes: **O(1)**, **O(queue)** = state-queue length, **O(k)** = blocks above the fork point, **O(W)** = one window, **O(C)** = blocks since the confirmed head, **O(N)** = full ledger.

## Plan-vs-code discrepancies found (fix the plan before ticketing)

| # | Plan says | Code says |
|---|---|---|
| X1 | `finalizeCommittedBlockLocally` sits in merge-to-confirmed-state | It is `workers/utils/commit-submission.ts:175-380`, called from `workers/commit-block-header/submission.ts:1881` |
| X2 | Tail refusal is at `commit-block-header.ts:852` | It is `workers/commit-block-header.ts:986-1003`. It fires for **any** abandoned journal with a non-null `CORRECTION_TRANSITION_DIGEST`, which includes replacement digests, not only correction removals |
| X3 | Deposit cutoff uses `new Date()` | The production path already uses `slotToUnixTime(change.after.head.slot)` (`services/event-history-runtime.ts:196-197`). `new Date()` survives only in `fibers/project-deposits-to-mempool-ledger.ts:158-160`, reached from the CLI `project-deposits-once` (`index.ts:2377`) and `commands/reconcile.ts:762`. `projectDepositsToMempoolLedgerFiber` (`:187`) has no production caller |
| X4 | Topology health is a "readiness gate" | `/readyz` (`commands/listen-router.ts:1376ff`) does not consult topology. The only gates are `/init` 409 (`listen-router.ts:1952`) and snapshot fetch failure (`services/state-queue-topology.ts:320-327`). The address-wide summary (`:85-110`) checks only invalid/root/tail counts. Orphan, cycle and missing checks exist only in the linked walk (`:149-224`) |
| X5 | "Permanent never-reuse of event keys across retired **and orphaned** origins" | The canonical set only holds incarnations with `placement != null` (`l1-event-history-provenance.ts:157-197`, check at `:264-268`). An orphaned incarnation does not block reuse, and `l1-event-history-owner-rollback-emulator.test.ts:5` asserts that a "fresh incarnation of the same deposit ID" is admitted. `event_history_incarnations` has no DELETE anywhere, so the key set is already never pruned |
| X6 | Post-finality correction rollback = integrity halt | Today it is recorded and **restored**: `services/state-queue-correction-observer.ts:551-585` (assertRollbackPermitted → revokeTerminal → restoreAfterRollback). It halts only for architecture_g once a rewind has run (`:360-398`) |
| X7 | Tier 2 `restoreRetainedRoot` is at 1567-1660 | `services/mpf-native-owner/service.ts:1567-1650`. `recover` is at `:1438-1470`, `restoreCanonicalRoot` at `:1534-1565` |
| X8 | tx-status exposes the head level (D2) | `commands/tx-status.ts:49-138` returns committed / awaiting_local_recovery / pending_commit / … with `committedMeaning: immutable_db_inclusion_not_confirmed_ledger_merge` (`:56,73`). There is no head field. The handler is `listen-router.ts:1100-1150` |

---

## H. Heads (local / landed / settled / confirmed)

| Row | Criteria |
|---|---|
| Inputs | **A**: state-queue UTxOs with slot and depth, and the follower tip. **B**: own commit intents (local head only). **A**: confirmed-state UTxO (confirmed head). |
| Output | `heads{local, landed, settled, confirmed}`. Each is a (header_hash, end_time, slot) keyed by fact-store tip point. |
| Today | There is no single structure. local = `fibers/speculative-commit-state.ts` (294). landed ≈ `summarizeStateQueueTopology` tail (`services/state-queue-topology.ts:85-110`). settled ≈ `markObservedWaitingStability`/`markFinalized` depth gating (`database/pendingBlockFinalizations.ts:2466,2546`). confirmed = `confirmed_ledger` + merge finalization (`transactions/state-queue/merge-to-confirmed-state.ts:1232-1349`). |
| Invariants | I1 confirmed ≤ settled ≤ landed ≤ local, compared by chain position. I2 landed is a pure function of A at tip T: two nodes with equal A agree byte-for-byte. I3 settled = the deepest landed ancestor with depth ≥ `confirmationDepth` (manifest value, `services/event-history-runtime.ts:85-88`). I4 local ≠ landed ⇔ exactly one live own commit intent in B (E loop, "one active own commit"). I5 after truncation at slot s, every head with slot > s is recomputed, and none is kept from cache. |
| Rollback | Recomputed. landed/settled are O(queue). local is O(1) (drop a dead intent). confirmed is unaffected below the finality depth; a change there triggers the incident (§8). |
| Acceptance | Property test with the fork simulator (built on `tests/helpers/history-rollback-transport.ts` `makeRollbackHistoryTransport`, using `fast-check` already in package.json): for random fork sequences, I1–I5 hold and heads equal the heads of a fresh replay. **Negative:** inject a cached landed head after truncation and assert the check fails. Adapt `pending-finalization.test.ts`, `confirmation-finalization-race.test.ts`. |
| Depends | P1 fact store (A). |

## 1. Landed queue + topology health

| Row | Criteria |
|---|---|
| Inputs | **A**: every UTxO at the state-queue address and policy, with slot. Keyed on the landed head. |
| Output | `landed_queue` = ordered linked list root→tail (NodeKey, header_hash, datum), plus `topology_health{healthy, reason, invalid, orphans, roots, tails, cycle}`. Keyed by fact-store tip point. |
| Today | `services/state-queue-topology.ts` (394): summary `:85-110`, reasons `:61-80`, linked walk with cycle/dup/missing checks `:149-224`, `MAX_OPERATIONAL_STATE_QUEUE_NODES=10_000` `:140`, snapshot `:281-371`. Polling source: `workers/utils/confirm-block-commitments.ts:137-147`. |
| Invariants | I1 healthy ⇒ exactly one root, one tail, and the linked walk from the root visits every valid policy UTxO exactly once (no orphans, no cycle). I2 queue length ≤ `MAX_OPERATIONAL_STATE_QUEUE_NODES`, otherwise unhealthy (not truncated). I3 malformed policy UTxO (bad datum or NFT) ⇒ unhealthy with a named reason. I4 unhealthy ⇒ `/readyz` not-ready **and** the E loop proposes no commit or merge. I5 the output is a pure function of A at the tip. |
| Rollback | Recomputed from A, O(queue). |
| Acceptance | Extend `state-queue-topology.test.ts:84-200` (it already has "fails closed when a linked unit is missing, duplicated, or cyclic", `:200`) with: (a) an orphan node that has a valid datum but is unreachable from the root ⇒ unhealthy; today the summary passes this. (b) A `/readyz` route test asserting not-ready on unhealthy; adapt `readiness-history-frontier-route.test.ts`. **Negative:** healthy queue ⇒ ready, and a truncation that removes the tail recomputes a healthy queue. |
| Depends | H, P1. Owner Q5 (halt vs warn). |

## 2. Event set per window (+ never-reuse key set)

| Row | Criteria |
|---|---|
| Inputs | **A**: deposit/withdrawal/forced-tx outputs with inclusion slot, and list-transition retirements (§3.5). Keyed on landed head plus window [start,end]. |
| Output | `event_set(window)` = {event_id, kind, inclusion_time, outRef, retired_at?}. `event_key_set` = {(kind,key), (kind,idCbor)} is append-only and never pruned. |
| Today | `l1-event-history-provenance.ts` (358): `historyIncarnationId` `:66-78`, canonical set `:157-197`, reuse refusal `:264-268`. Journal validation `database/eventHistoryJournal.ts:171-196`. Retention `eventHistoryJournal.ts ~897-990` (horizon = `automaticRecoveryMaxDepth`). Deletable: `database/eventHistoryMaterialization.ts` (411), `eventHistoryReplayReceipts.ts` (363), `eventHistoryLedgerReceipts.ts` (268), `eventHistoryLedgerRepair.ts` (331), `eventHistoryRecoveryPlans.ts` (573), `services/event-history-producer.ts` (242), `services/event-history-recovery.ts` (317), `l1-event-history-initialization.ts` (141, already dead). |
| Invariants | I1 an event is in `event_set(w)` iff its inclusion_time ∈ w and its origin output is on the canonical chain at the tip. I2 at most one canonical origin per (kind,key) and per (kind,idCbor) (`eventHistoryJournal.ts:171-196`). I3 a key that has ever had a live or retired canonical origin is never readmitted (`provenance.ts:264-268`). Orphan semantics depend on owner Q1. I4 `event_key_set` rows are never deleted; there is a test on the SQL surface. I5 `inclusion_time` = `slotToUnixTime(slot)` of the origin tx and never wall-clock. |
| Rollback | Recomputed incrementally. Truncate A above the fork, then re-derive windows touching slots > fork: O(k). Key set: entries whose origin is now orphaned are marked orphaned, never deleted. |
| Acceptance | Adapt `l1-event-history-provenance.test.ts:81-186` and `l1-event-history-owner-rollback-emulator.test.ts`. (a) Fork removes a deposit origin, then the re-created ID is admitted or refused per Q1. (b) A retired origin's key being resubmitted ⇒ refused. **Negative:** the same key under a different kind is admitted. Simulator property: the event set equals a replay from activation over A. |
| Depends | H, P1. Owner Q1. |

## 3. Event status (+ T2 carry-forward)

| Row | Criteria |
|---|---|
| Inputs | **A**: landed queue (1), event set (2). **B**: own signed block members. **C**: foreign DA payloads by header hash. Keyed on landed head. |
| Output | `event_status(event_id)` ∈ {awaiting, included(own,header), included(foreign,header), carried_forward(window)}, plus the head level (D2). Keyed by event_id at the fact-store tip. |
| Today | Mutable status columns: deposits `awaiting/projected/consumed` (`database/deposits.ts:37-41`), withdrawals and forced `awaiting/projected/finalized` (`withdrawals.ts:48-52`, `forcedTransactions.ts:65-69`). Set by `markProjectedByEventIds` in `fibers/block-confirmation.ts:221-275`. T2: `workers/t2-foreign-event-reconciliation.ts` (780) `resolveT2ForeignEventEvidence` `:145-297`; the whole-window rule `:180-184`; `verifyForeignPayload` (8 roots + counts) `:76-107`; a foreign-present candidate returns `AwaitingForeignDa "foreign_event_present_requires_finalization"` `:270-282`; absent ⇒ projected `:608-633`. State in `database/foreignTipReconciliations.ts` (1061). |
| Invariants | I1 included(h) ⇔ the event is a member of block h, h is on the landed chain or is the one live own intent, and h's roots were verified (own: B; foreign: C verified against all 8 roots). I2 a foreign block with any non-empty deposit/forced/withdrawal root and no verified C ⇒ no event in its window may be declared omitted. It **defers**, never guesses (plan §4.C). I3 due-but-omitted events in a verified foreign window are carried_forward to the next window. They are never dropped and never double-included. I4 `VerifiedEmpty` is never upgraded (write-once C). I5 the status reports its head level (landed/settled/confirmed). |
| Rollback | Recomputed per affected window: O(k·W). C is unaffected (content-addressed). |
| Acceptance | Adapt `t2-foreign-event-reconciliation.test.ts:127-193` and `foreign-tip-reconciliation.test.ts`. (a) A foreign block with a non-empty withdrawals root but missing DA ⇒ the deposit stays awaiting (not omitted). (b) A verified foreign block that omits a due deposit ⇒ carried_forward. (c) A fork removes the foreign block ⇒ the event returns to awaiting. **Negative:** a payload with one root mismatched ⇒ refused and nothing projected. |
| Depends | 1, 2, C store (P1), B store (P3). Owner Q4. |

## 4. Deposit spendability in the working mempool ledger

| Row | Criteria |
|---|---|
| Inputs | **A**: event set (2) and follower tip slot. **D**: event status (3). Keyed on the landed head plus the one live own intent. |
| Output | The set of spendable deposit UTxOs in the working ledger, keyed by outRef/source_event_id. |
| Today | `spendablePredicate` = `source_event_id IS NULL OR deposits.projected_header_hash IS NOT NULL` (`database/mempoolLedger.ts:176-181`), `retrieveSpendable` `:184-204`. Publish: `publishProjectedDeposits` (`fibers/block-confirmation.ts:278-299`) and revive republish (`:713-721`). Projection: `fibers/project-deposits-to-mempool-ledger.ts` (197): `reconcileAlreadyProjectedDeposits` `:37-110`, `projectAwaitingDeposits(upTo)` `:112-138`. Due query: `deposits.ts:256-271`. |
| Invariants | I1 a deposit is spendable iff its status is included(h) with h landed or the live own intent. I2 the cutoff is `slotToUnixTime(follower_tip.slot)`, and no code path calls `new Date()` (closes X3). I3 a deposit consumed by an included tx is not spendable. I4 after rollback removes h, the deposit is not spendable, and txs spending it are rejected transitively and batch-atomically (`services/state-queue-correction-ledger-restore.ts:57-62`). |
| Rollback | Recomputed from event status, O(deposits in affected windows). |
| Acceptance | Adapt `deposit-flow-emulator-confirmation-journal.test.ts`, `deposit-flow-emulator-recovery-invalidation.test.ts`, `mempool-ledger-cache.test.ts`. (a) Deposit included ⇒ spendable; fork removes the block ⇒ not spendable and a dependent tx rejected with `E_REWIND_REOPENED_DEPOSIT_INPUT`. **Negative:** a deposit with inclusion_time > tip time stays unspendable even when wall-clock is later (fake clock ahead of the tip). A grep/lint test: no `new Date()` in the projection. |
| Depends | 3. |

## 5. Landed-block local effects

| Row | Criteria |
|---|---|
| Inputs | **B**: signed block members (tx CBOR in C) of a landed header. **D**: L2 ledger at that header's root (9). Keyed on header_hash (landed). |
| Output | Per header_hash: immutable rows, BlocksDB rows, mempool/processed_mempool removal, withdrawal ledger effects plus deposit consumption, the DA payload with CEK sidecars, CEK ownership release, and a tx-MPF reset. Idempotent by header_hash. |
| Today | `workers/utils/commit-submission.ts:175-380` (606 lines): skip already-committed `:196-243`; DA payload from `materializeConfirmedLedgerSnapshot` `:260-273`; one tx for immutable, BlocksDB and mempool clear `:311-331`; processed clear `:333-337`; `applyFinalizedWithdrawalLedgerEffects` `:136-172`; DA upsert + CEK release `:338-348`; outbox + `transactionsMpf.resetToEmpty()` `:366-371`; recovery `:382-502`. Guarded by the root checks in `submission.ts:1843-1879`. Undo: `services/state-queue-correction-recovery.ts:593-660` (reinsert), `services/state-queue-correction-ledger-restore.ts:313,528-530,568`, `services/native-mpf-local-finalization.ts` (87). |
| Invariants | I1 the effects for h exist iff h is landed or the live own intent (plan §4.E-5). I2 applying the effects for h twice is a no-op. I3 the DA payload for h equals the ledger materialized at h's MPF root, with byte-equal roots and counts. I4 when h leaves the landed chain, all effects are removed, and its txs return to the mempool or are rejected transitively (`state-queue-correction-ledger-restore.ts:30-62`). I5 CEK ownership is released only for txs in a landed h. |
| Rollback | Recomputed per header after truncation, O(k · block size). |
| Acceptance | Adapt `failed-local-finalization-correction-emulator.test.ts:39`, `native-mpf-local-finalization.test.ts:277-360`, `mempool-ledger-effects.test.ts`, `deposit-flow-emulator-merge-payout.test.ts`. (a) Crash between the immutable insert and the DA upsert, then restart ⇒ the projection converges with no duplicates. (b) Fork removes h ⇒ immutable/BlocksDB rows gone, members back in the mempool, and a dependent withdrawal rejected. **Negative:** a revived landed h (replacement won) produces its DA payload before the attestation timeout (§4.E-8). |
| Depends | 9, B (P3), 3. |

## 6. Withdrawal classification + forced-tx verdicts as block content (B)

| Row | Criteria |
|---|---|
| Inputs | **B** only for existing blocks (signed content). For a new or replacement build: **D** ledger at the base root (9) and event set (2). Keyed by header_hash. |
| Output | `block_content(h).withdrawal_classifications[event_id]` (Validity) and `.forced_verdicts[event_id]`. Immutable once signed. |
| Today | Mutable rows: classification at build `workers/utils/mpf/process.ts:450-482` (`classifyWithdrawal` in `mpf/withdrawal-classification.ts:78`, guarded by `CLASSIFICATION_REVISION`); Validity `database/withdrawals.ts:56-65`; reopen `~606-652`; restore `restoreCorrectedClassification` `:660-716`; per-journal columns `pendingBlockFinalizations.ts:117-122`. Forced: `setProofClassifications` `database/forcedTransactions.ts:452-520` from `mpf/process.ts:545-570` via `mpf/event-window.ts:441`. It is not keyed by header. |
| Invariants | I1 the classification for (h,event) is read from B and never recomputed for a signed h. I2 a replacement block re-derives the classification against its own base root, and the result is stored under the new header only. I3 two headers with different verdicts for the same event may coexist in B, and the landed one wins by derivation. I4 no mutation of the withdrawal or forced row changes a signed block's content (drop `CLASSIFICATION_REVISION` and `restoreCorrectedClassification`). |
| Rollback | Unaffected (B is append-only). Re-derived only when building a replacement, O(W). |
| Acceptance | Adapt `withdrawal-classification*.test.ts`, `forced-transactions.test.ts:263`, `signed-intent-replacement-revival-emulator.test.ts:119,231,316`. (a) Block h classifies W as valid; a replacement h' at a different base classifies W as invalid; h lands ⇒ W effect = valid (from h's content). **Negative:** mutating the row after signing changes neither h's payload nor its roots. |
| Depends | B store (P3), 9. |

## 7. Refuse to build on a correction-removed tail

| Row | Criteria |
|---|---|
| Inputs | **A**: landed queue (1) and admitted corrections (8). **B**: own intents. Keyed on the landed tail. |
| Output | `build_base_admissible(tail) : {ok} | {defer, reason}`. |
| Today | `workers/commit-block-header.ts:986-1003`, keyed on journal `Abandoned` + `CORRECTION_TRANSITION_DIGEST != null` (set by `markCorrectedAfterStateQueueRemoval`, `pendingBlockFinalizations.ts:2732-2770`). Revival excludes correction (`services/canonical-journal-recovery.ts:356-361`). |
| Invariants | I1 no commit intent is created whose base is a header removed by an admitted correction. I2 a landed own block abandoned by **replacement** is followed or revived, not refused (§4.E-1). This fixes X2: the current check also refuses replacement digests. I3 the decision is a pure function of A plus B at the tip. I4 a refusal is a defer, not a halt, and the loop retries on the next A change. |
| Rollback | Recomputed, O(1) per tail. |
| Acceptance | Adapt `commit-block-header-state-queue-tail.test.ts` and `state-queue-correction-reinclusion.test.ts`. (a) Correction admitted that removes tail h ⇒ no build on h, defer. **Negative:** replacement-abandoned own block h that landed ⇒ loop follows h (revives) and builds on it. Adapt `signed-intent-replacement-revival-emulator.test.ts`. |
| Depends | 1, 8, B (P3). |

## 8. Correction admission + post-finality incident

| Row | Criteria |
|---|---|
| Inputs | **A**: state-queue removal txs from `l1_txs` with depth, and the landed queue (1). Keyed on the settled head. |
| Output | `admitted_corrections` = {removal tx, removed headers, admitted_at_depth}, plus `incident` (halt flag + evidence). Keyed by removal tx id. |
| Today | `services/state-queue-correction-observer.ts` (1603): `reconcileStateQueueCorrectionObserver` `:416-671`; admission at depth ≥ `requiredFinalityDepth` `:623-650`; post-admission rollback restore `:551-585` (`postFinalityRollbackIncidents`); architecture_g-only rewind halt `:360-398`; Kupo source `:1403`, depths `:1430,1545`. Wiring: `fibers/attestation-timeout-correction.ts:76-148`, `services/event-history-runtime.ts:79-90` (`requiredFinalityDepth` = manifest `confirmationDepth`). Undo: `services/state-queue-correction-rewind.ts` (899), `state-queue-correction-recovery.ts` (682). |
| Invariants | I1 a correction is admitted iff its removal tx has depth ≥ `confirmationDepth` on the canonical chain. I2 admission is a pure function of A, and two nodes with equal A admit the same set. I3 an admitted correction whose removal tx later leaves the canonical chain ⇒ integrity halt (plan) with a recorded incident. Engine-uniform, see Q2. I4 below admission depth, the pending correction has no effect on any other projection. |
| Rollback | Recomputed from A, O(k). Admitted entries below depth are unaffected. A change to them is the incident (halt), not a recompute. |
| Acceptance | Adapt `state-queue-correction-observer.test.ts:422-616`: rewrite "records and atomically restores a correction rolled back after admission" (`:508`) to expect a halt. Keep the refusal test `:562`. (a) Removal at depth confirmationDepth−1 ⇒ not admitted; at depth confirmationDepth ⇒ admitted. (b) Fork removes an admitted removal ⇒ halt, with the incident persisted and no L2 writes after it. **Negative:** fork removes a **non-admitted** removal ⇒ no incident. |
| Depends | 1, H. Owner Q2. |

## 9. L2 ledger MPF roots, tier 1 and tier 2

| Row | Criteria |
|---|---|
| Inputs | **B/C**: per-block event logs and members. **A**: landed chain (1). **D**: confirmed root (10). Keyed on header_hash along the landed chain. |
| Output | `mpf_root(h)` for every h in the horizon, the resident retained-root set, and the ledger at the working root. |
| Today | `services/mpf-native-owner/service.ts` (2001): `recover` = one block `:1438-1470`; `restoreCanonicalRoot` `:1534-1565`; `restoreRetainedRoot` (full index walk + child restart + `__root__` write) `:1567-1650`; `FULL_INDEX_MAX_RECORDS=2_000_000` `:36`, `FULL_INDEX_MAX_BYTES=512MiB` `:35`. Node-key `del` is a no-op and only ROOT_KEY is deleted (`mpf/root-view-store.ts ~596-615`). Callers: `history-dependent-recovery.ts:34`, `block-commitment.ts:211,235`, `native-mpf-startup.ts:180`, `native-mpf-local-finalization.ts:83`. Only architecture_g has an owner (`services/config.ts:255,1083-1088`). |
| Invariants | I1 for every landed h within the horizon, `mpf_root(h)` equals h's signed ledger root. I2 tier 1: rewind to a resident root is `SwitchRoot` O(1); a non-resident target is refused (then tier 2), never approximated. I3 tier 2: `restoreCanonicalRoot(confirmed)` followed by replay of the saved event logs from confirmed to the target yields the target's signed root, with a check after each block. I4 mark/sweep deletes only nodes unreachable from {current, horizon roots, confirmed, pending replay roots}, and runs only with no generation open. I5 resident records ≤ 2M; exceeding that is a surfaced error, not silent eviction. |
| Rollback | Tier 1 rewound O(1) (resident). Tier 2 rewound O(C) replay plus O(N) restore. |
| Acceptance | Adapt `mpf-native-canonical-recovery.test.ts:131-230` and `mpf-native-promotion-recovery.test.ts`. (a) Fork of depth 3 inside the horizon ⇒ SwitchRoot, root equal and no restart. (b) A correction at maturity depth removes a block below the horizon ⇒ tier-2 replay reaches the target root. (c) Sweep then re-open every retained root ⇒ all readable. **Negative:** SwitchRoot to a swept root ⇒ refused, and tier 2 then succeeds. A replay log with one tampered event ⇒ root mismatch halt. |
| Depends | B/C (P3), 1, 10. Owner Q3. |

## 10. confirmed_ledger

| Row | Criteria |
|---|---|
| Inputs | **A**: the on-chain confirmed-state UTxO (header hash + root). **D**: ledger deltas (5/9). Keyed on the confirmed head. |
| Output | The `confirmed_ledger` table, equal to the ledger at the confirmed header's root. |
| Today | `transactions/state-queue/confirmed-ledger-snapshot.ts` (428): delta chain `:276-299`, suffix `:308-340`, snapshot `:342-352`, apply with base and final root checks `:383-428`. `database/confirmedLedger.ts` (34). Merge finalize `merge-to-confirmed-state.ts:122-168,1232-1349` runs after a single merge-tx confirmation (`:1200-1212`) with no inverse. |
| Invariants | I1 the root of `confirmed_ledger` equals the root in the on-chain confirmed-state datum at a depth ≥ `confirmationDepth`. I2 the merge effects (clearBlock, deposit consumed, withdrawal/forced finalized) apply only once the merge tx is at settled depth, not on first sighting. I3 the table only moves forward. A backward move is the incident (halt), as in §8. I4 apply is atomic, and the base root is checked before the delta. |
| Rollback | Unaffected below depth. Above depth, the merge is not yet applied, so there is nothing to undo. |
| Acceptance | Adapt `confirmed-ledger-snapshot.test.ts:152-287`, `reconcile-merge-complete.test.ts`, `merge-readiness.test.ts`. (a) Merge tx at depth d−1 ⇒ confirmed_ledger unchanged; at d ⇒ applied. (b) Fork removes a sub-depth merge ⇒ no inverse is needed and the state is unchanged. **Negative:** a delta with a wrong base root ⇒ refused atomically. |
| Depends | H, 9. |

## W. Reconcile-loop wallet view

| Row | Criteria |
|---|---|
| Inputs | **A**: operator-address UTxOs at the tip. **B**: live intents (spent inputs + predicted change). Keyed on the fact-store tip plus the live-intent set. |
| Output | `wallet_view` = A-UTxOs − inputs of live intents + predicted change of live intents. The single owner of the Lucid override. |
| Today | `operator-wallet-view.ts` (169, `makeOperatorWalletView` `:38`, `applySubmittedTxToOperatorWalletView` `:138`, staleness `:162`). Frozen override `transactions/utils.ts:536-590` (`reconcileWalletUtxosFromSignedTx`, called `:1422`). Ad-hoc overrides in `transactions/reference-publication.ts:292-793`, `reference-scripts.ts:525`, `reference-script-sweep.ts:1095`. |
| Invariants | I1 the view is recomputed from A + B on every loop pass, and no override outlives one build. I2 an input spent by a dead intent reappears; the change from a dead intent disappears. I3 no two live intents spend the same wallet input. I4 before any build, the previous override is cleared (the known "Missing vkey witness"/"Could not spend UTxO" hazard). |
| Rollback | Recomputed, O(wallet UTxOs). |
| Acceptance | Adapt `operator-wallet-view.test.ts:72-143` and `wallet-hygiene.test.ts`. (a) Intent dies after a fork ⇒ its inputs are spendable again and the next build succeeds. **Negative:** a stale pinned view (a live intent's change never landed) ⇒ the build does not select that change. |
| Depends | H, B (P3), E loop. |

---

## (1) Dependency DAG — ticket order

```
T0  P1 fact store (A) + C store                        [prereq, not a projection]
T1  H  heads                                    ← T0
T2  1  landed queue + topology (+ /readyz gate) ← T1
T3  2  event set + key set                      ← T1
T4  8  correction admission + incident          ← T2
T5  B  intent store (P3)                        ← T0   [prereq]
T6  3  event status (+T2 carry-forward)         ← T2, T3, T5, C
T7  4  deposit spendability                     ← T6
T8  9  MPF roots tier 1/2                       ← T2, T5
T9  6  classification/verdicts as B content     ← T5, T8
T10 7  correction-removed-tail refusal          ← T2, T4, T5
T11 10 confirmed_ledger                         ← T1, T8
T12 5  landed-block local effects               ← T6, T8, T9
T13 W  wallet view                              ← T1, T5, E loop
```
Parallelizable: {T2,T3}, then {T4,T6,T8}, then {T7,T9,T10,T11}. The critical path is T0→T1→T2→T8→T9→T12. The simulator (fast-check + `makeRollbackHistoryTransport`) must land with T1, since every later acceptance test uses it.

## (2) Deletable once ALL projections exist (`wc -l` at HEAD 32d87e2ca + working tree)

| File | Lines | Replaced by |
|---|---|---|
| database/eventHistoryLedgerReceipts.ts | 268 | 2 |
| database/eventHistoryLedgerRepair.ts | 331 | 2 |
| database/eventHistoryRecoveryPlans.ts | 573 | 2 |
| database/eventHistoryReplayReceipts.ts | 363 | 2 |
| database/eventHistoryMaterialization.ts | 411 | 2 |
| services/event-history-producer.ts | 242 | 2 |
| services/event-history-recovery.ts | 317 | 2 |
| l1-event-history-initialization.ts | 141 | dead today |
| services/state-queue-correction-rewind.ts | 899 | 8, 9 |
| services/state-queue-correction-recovery.ts | 682 | 5 |
| services/state-queue-correction-ledger-restore.ts | 578 | 5 (rejection logic re-homed to E-5) |
| services/native-mpf-local-finalization.ts | 87 | 9 |
| fibers/project-deposits-to-mempool-ledger.ts | 197 | 4 |
| database/pendingBlockFinalizations.ts | 2874 | H + B |
| fibers/block-confirmation.ts | 890 | H, 3, 5 |
| workers/confirm-block-commitments.ts | 407 | 1 |
| workers/utils/confirm-block-commitments.ts | 222 | 1 |
| services/history-signed-header-recovery.ts | 453 | E loop |
| services/signed-intent-canonical-coverage.ts | 250 | E loop |
| database/eventHistoryCanonicalCoverage.ts | 239 | E loop |
| services/history-expired-intent-release.ts | 822 | E-2 (logic re-homed) |
| services/history-dependent-recovery.ts | 59 | 9 |
| services/history-pending-backoff.ts | 108 | E-7 |
| services/history-recovery-state-queue.ts | 114 | 1 |
| services/canonical-journal-recovery.ts | 453 | E-1 (logic re-homed) |
| database/mutationJobs.ts | 212 | 10 |
| fibers/speculative-commit-state.ts | 294 | H (local) |
| workers/utils/commit-submission.ts | 606 | 5 (logic re-homed) |
| workers/commit-block-header/pending-journal.ts | 485 | B |
| database/foreignTipReconciliations.ts | 1061 | 3 |
| **Total** | **14,638** | |

Not deleted: `database/stateQueueMutationLeases.ts` (476; the lock is kept per §4.E). `services/state-queue-topology.ts` (394), `services/state-queue-correction-observer.ts` (1603), `workers/t2-foreign-event-reconciliation.ts` (780) and `operator-wallet-view.ts` (169) are **rewritten in place** as projections 1/8/3/W, so their line counts shrink but the files survive. The subtotals match the P0 audit (P4 4,389; P3 10,584 including the lease table).

## (3) Owner questions (D1–D3 not re-asked)

1. **Orphaned-origin key reuse (X5).** Today a deposit ID whose origin was orphaned by a fork may be readmitted as a fresh incarnation (`l1-event-history-provenance.ts:157-197,264-268`; the test `l1-event-history-owner-rollback-emulator.test.ts:5` asserts it). The plan says never-reuse across orphaned origins. Which is intended?
2. **Post-finality correction rollback (X6).** Should it be a uniform integrity halt for all engines, replacing today's record-and-restore (`state-queue-correction-observer.ts:551-585`)?
3. **MPF engines.** Do legacy, overlay and event_flat (`services/config.ts:255,1083-1088`, default legacy) survive the redesign? If they do, projection 9 needs per-block roots for each of them. If not, the tier-1/2 design covers architecture_g only.
4. **Foreign inclusion of our candidates.** Should an event included by a verified foreign block become included(foreign) and be released? Today it blocks as `foreign_event_present_requires_finalization` (`t2-foreign-event-reconciliation.ts:270-282`).
5. **Topology failure policy (X4).** Should unhealthy topology (orphan or malformed policy UTxO, cycle, over-cap) make `/readyz` fail and stop proposals (a halt), or only warn while serving reads?
