# P0 deletion/replacement audit: demo/midgard-node/src (PARTIAL)

Status: PARTIAL. Line counts, production callers and the L1-read call-site inventory are
mechanical and complete (grep with relative-import resolution; tests excluded; barrel
namespaces resolved separately). The component, phase and "uncovered" columns are the
auditor's TENTATIVE classification from module names, headers and caller graph. Three deep-read
sub-audits (event-history / block lifecycle / ingestion+correction+merge) were still running when
this was written and are NOT folded in. Nothing here was built or run.

Legend: A fact store, B intent store, C foreign DA cache, D projection, E reconcile loop.

## Tentative line totals by deletable phase
| Phase | Lines | Modules |
|---|---|---|
| P2 (L1 reads onto fact store) | 9,560 | l1-event-history-* (15 files), l1-ledger-snapshot, eventHistoryJournal(+Codec), event-history-owner/runtime, fetch-and-insert-* x3, user-event-barrier-refresher, user-event-ingestion, state-queue-topology, l1-tx-order-carriage, operator-wallet-view |
| P3 (intent store + reconcile loop) | 10,584 | pendingBlockFinalizations, block-confirmation, confirm-block-commitments x2, history-signed-header-recovery, signed-intent-canonical-coverage, eventHistoryCanonicalCoverage, history-expired-intent-release, history-dependent-recovery, history-pending-backoff, history-recovery-state-queue, canonical-journal-recovery, mutationJobs, stateQueueMutationLeases, speculative-commit-state, commit-submission, pending-journal, foreignTipReconciliations, event-history-producer, event-history-recovery |
| P4 (per-block roots) | 4,389 | eventHistoryLedgerReceipts, LedgerRepair, RecoveryPlans, ReplayReceipts, Materialization, state-queue-correction-rewind/recovery/ledger-restore, native-mpf-local-finalization, project-deposits-to-mempool-ledger |
| keep / reshape to pure proposer | rest | block-commitment, speculative-commit-builder, merge, merge-to-confirmed-state, operator-watchdog(+policy), attestation-timeout-correction, commit-block-header + submission/build/da-payload*, t2-foreign-event-reconciliation (C), transactions/utils, scheduler-refresh, history-commit-window (modify: horizon lag d), deposits/withdrawals/forcedTransactions DB (become D views), eventHistorySubmissions (B), eventHistoryAuthority, correction-observer (reads move to A) |

Audited set total (incl. keep): 47,021 lines.

## Module table
| Path | Lines | Production callers | Component | Phase |
|---|---|---|---|---|
| l1-event-history-activation.ts | 87 | l1-event-history-initialization, -list-replay | A | P2 |
| l1-event-history-block-stage.ts | 128 | -ledger-projection, -list-replay | A | P2 |
| l1-event-history-chain.ts | 304 | -source, services/event-history-owner, signed-intent-canonical-coverage | A (ChainSync client; reuse) | P2 (moves) |
| l1-event-history-entries.ts | 234 | eventHistoryMaterialization, fetch-and-insert-deposit/withdrawal | A/D | P2 |
| l1-event-history-initialization.ts | 141 | NONE (tests only) — dead in production today | delete | now |
| l1-event-history-ledger-projection.ts | 84 | -initialization, -projection | D | P2 |
| l1-event-history-list-replay.ts | 303 | eventHistoryReplayReceipts, event-history-owner | A+D | P2 |
| l1-event-history-projection.ts | 23 | eventHistoryJournal, -list-replay, -transport, event-history-owner | D | P2 |
| l1-event-history-provenance.ts | 358 | eventHistoryJournal(+Codec), Materialization, -entries, -initialization, -list-replay, event-history-owner | unknown (authority/provenance check; plan silent) | keep logic |
| l1-event-history-reference.ts | 88 | -list-replay, event-history-owner | A | P2 |
| l1-event-history-snapshot.ts | 110 | -source | A (bootstrap) | P2 |
| l1-event-history-source.ts | 448 | 19 modules (journal, recovery plans, coverage, owner, runtime, correction-rewind, ...) | A | P2 |
| l1-event-history-transaction.ts | 285 | -reference, -source, -transition, signed-intent-canonical-coverage | A (l1_txs) | P2 |
| l1-event-history-transition.ts | 552 | eventHistoryJournal, -activation, -block-stage, -ledger-projection, -provenance | A+D | P2 |
| l1-event-history-transport.ts | 289 | event-history-owner/runtime, history-expired-intent-release, history-signed-header-recovery | A | P2 |
| l1-ledger-snapshot.ts | 224 | 17 modules | A (bootstrap snapshot) | P2 (reuse pattern) |
| services/event-history-owner.ts | 1,080 | LedgerRepair, Materialization, producer, runtime, globals, history-commit-window, expired-intent-release | A (follower) | P2 |
| services/event-history-runtime.ts | 217 | commands/listen.ts | E | P2/P3 |
| services/event-history-producer.ts | 242 | 23 modules (deposits, withdrawals, txAdmissions, pendingBlockFinalizations, all fibers, commit worker, merge) | D (read API) | P3 |
| services/event-history-recovery.ts | 317 | owner, producer, runtime, dependent/expired/signed-header recovery, native-mpf-startup, correction-rewind | E | P3 |
| services/history-commit-window.ts | 53 | commit-block-header, build-unsigned-tx, submission | keep (add horizon lag d) | keep |
| services/history-signed-header-recovery.ts | 453 | event-history-owner, runtime | delete (whichever-lands-wins) | P3 |
| services/history-dependent-recovery.ts | 59 | expired-intent-release, signed-header-recovery, correction-rewind | E | P3 |
| services/history-expired-intent-release.ts | 822 | event-history-runtime | E + B pruning rule | P3 |
| services/history-pending-backoff.ts | 108 | event-history-owner | E | P3 |
| services/history-recovery-state-queue.ts | 114 | history-signed-header-recovery | E | P3 |
| services/signed-intent-canonical-coverage.ts | 250 | eventHistoryCanonicalCoverage, signed-header-recovery | delete | P3 |
| services/canonical-journal-recovery.ts | 453 | commands/listen-startup, block-confirmation, expired-intent-release | E (restart = same reset path) | P3 |
| database/eventHistoryAuthority.ts | 419 | 17 modules | unknown (authority material; plan silent) | keep? |
| database/eventHistoryCanonicalCoverage.ts | 239 | history-signed-header-recovery | delete | P3 |
| database/eventHistoryJournal.ts | 1,605 | CanonicalCoverage, RecoveryPlans, owner, dependent/expired/signed-header recovery, correction-rewind | A (replaces undo records/incarnations) | P2 |
| database/eventHistoryJournalCodec.ts | 117 | eventHistoryJournal | A | P2 |
| database/eventHistoryLedgerReceipts.ts | 268 | txAdmissions | D (per-block roots) | P4 |
| database/eventHistoryLedgerRepair.ts | 331 | Materialization, runtime, signed-header-recovery | D | P4 |
| database/eventHistoryMaterialization.ts | 411 | runtime, signed-header-recovery | D | P4 |
| database/eventHistoryRecoveryPlans.ts | 573 | commands/mpf-audit, dependent/expired/signed-header recovery, correction-observer/recovery/rewind | E/D | P4 |
| database/eventHistoryReplayReceipts.ts | 363 | eventHistoryJournal, event-history-owner | D | P4 |
| database/eventHistorySubmissions.ts | 211 | commands/submit-withdrawal, transactions/event-history-submission | B (own list-insert txs) | keep |
| database/pendingBlockFinalizations.ts | 2,874 | direct: stateQueueMutationLeases, mpf/process, mpf/validation-trace, expired-intent-release, signed-header-recovery, native-mpf-local-finalization, correction-rewind; via PendingBlockFinalizationsDB: 24 files incl. block-commitment, speculative-commit-builder, block-confirmation, commit worker, da-payload(-backfill), merge-to-confirmed-state, listen-router/startup, reconcile | B (signed bytes) + D (status) | P3 |
| database/foreignTipReconciliations.ts | 1,061 | via ForeignTipReconciliationsDB: block-commitment, speculative-commit-builder, t2-foreign-event-reconciliation, listen-router | C + D | P3 |
| database/forcedTransactions.ts | 646 | 18 files (mpf/*, commit worker, da-payload, event-roots, block-commitment/confirmation, merge, canonical-journal-recovery, correction-recovery, t2, event-settlement-proof) | D over A | P2 (ingest) / keep (views) |
| database/deposits.ts | 440 | 27 files | D over A | P2 (ingest) / keep (views) |
| database/withdrawals.ts | 824 | 24 files | D over A | P2 (ingest) / keep (views) |
| database/mutationJobs.ts | 212 | pendingBlockFinalizations, expired-intent-release, signed-header-recovery, merge-to-confirmed-state, commit-submission, listen-router/startup, reconcile, merge | E | P3 |
| database/stateQueueMutationLeases.ts | 476 | block-commitment, speculative-commit-builder, pending-journal, listen-router, commit worker, attestation-timeout-correction, reconcile, merge, mpf-audit, expired/signed-header recovery, correction-rewind | E | P3 |
| fibers/block-confirmation.ts | 890 | fibers/index (listen), expired-intent-release | E + D | P3 |
| fibers/block-commitment.ts | 1,439 | fibers/index, speculative-commit-builder | E proposer | keep (reshape) |
| fibers/speculative-commit-builder.ts | 1,253 | block-confirmation, fibers/index, expired/signed-header recovery, correction-rewind | E proposer | keep (reshape) |
| fibers/speculative-commit-state.ts | 294 | block-commitment, builder, barrier-refresher, globals, commit worker, submission | D (local head) | P3 |
| fibers/user-event-barrier-refresher.ts | 114 | fibers/index | A (follower tip replaces barrier) | P2 |
| fibers/user-event-ingestion.ts | 130 | fetch-and-insert-* x3 | A | P2 |
| fibers/fetch-and-insert-deposit-utxos.ts | 136 | commands/reconcile, state-reconciliation, fibers/index, barrier-refresher, submit-deposit, commit worker, submission | A | P2 |
| fibers/fetch-and-insert-withdrawal-utxos.ts | 85 | commands/fetch-withdrawals-once, state-reconciliation, fibers/index, barrier-refresher, commit worker, submission | A | P2 |
| fibers/fetch-and-insert-tx-order-utxos.ts | 661 | fibers/index, barrier-refresher, commit worker, submission | A (+l1_txs) | P2 |
| fibers/project-deposits-to-mempool-ledger.ts | 197 | commands/reconcile, fibers/index, runtime, signed-header-recovery | D | P4 |
| fibers/attestation-timeout-correction.ts | 499 | fibers/index | E proposer | keep (reshape) |
| services/attestation-timeout-observation.ts | 69 | attestation-timeout-correction | D | keep |
| fibers/merge.ts | 816 | commands/reconcile, fibers/index | E proposer | keep (reshape) |
| fibers/operator-watchdog.ts | 407 | fibers/index | E proposer | keep (reshape) |
| fibers/operator-watchdog-policy.ts | 194 | fibers/index, watchdog, operators/status | keep | keep |
| workers/confirm-block-commitments.ts | 407 | worker spawned by block-confirmation.ts:380 | E + D (landed/settled) | P3 |
| workers/utils/confirm-block-commitments.ts | 222 | block-confirmation, confirm worker | D | P3 |
| workers/utils/commit-submission.ts | 606 | commit-block-header/submission | E + B | P3 |
| workers/commit-block-header.ts | 3,325 | worker spawned by block-commitment:977, speculative-commit-builder:492; also imported by commands/reconcile, index, mpf-commit-candidate-probe | keep (builder) | keep (reshape) |
| workers/commit-block-header/submission.ts | 1,890 | commit-block-header | E + B | P3 (submission half) |
| workers/commit-block-header/pending-journal.ts | 485 | submission | B | P3 |
| workers/commit-block-header/state-queue.ts | 329 | builder, commit worker, build-unsigned-tx, pending-journal, submission | A reads | P2 |
| workers/t2-foreign-event-reconciliation.ts | 780 | commit-block-header | C | keep |
| services/state-queue-correction-observer.ts | 1,603 | commands/availability-challenge-source, services/index, correction-recovery/rewind | A (Kupo reads) + keep (detection) | P2 (reads) |
| services/state-queue-correction-recovery.ts | 682 | expired-intent-release, services/index, correction-rewind | D | P4 |
| services/state-queue-correction-rewind.ts | 899 | runtime, expired-intent-release | D | P4 |
| services/state-queue-correction-ledger-restore.ts | 578 | correction-recovery | D (two-tier roots) | P4 |
| services/state-queue-topology.ts | 394 | listen-router, listen-startup, block-commitment, merge, retention-sweeper, speculative-commit-builder, native-mpf-startup, initialization | A+D | P2 |
| l1-tx-order-carriage.ts | 1,194 | availability-challenge-source, fetch-and-insert-tx-order, -chain, -source, -transport, l1-ledger-snapshot, correction-observer | A (l1_txs) | P2 |
| operator-wallet-view.ts | 169 | merge-to-confirmed-state, build-unsigned-tx, submission, scheduler-refresh | A (own wallet) + B (in-flight spends) | P2/P3 |
| transactions/utils.ts (handleSignSubmit :1314, submitSignedTxWithRecovery :945, awaitExactTransactionConfirmation :378) | 1,641 | 22 files (every L1 tx builder) | E (resubmit exact bytes) + keep (signing) | P3 (confirmation half) |
| transactions/state-queue/merge-to-confirmed-state.ts | 1,460 | fibers/merge | keep builder; local finalization -> D | P4 (finalization) |
| services/native-mpf-local-finalization.ts | 87 | block-commitment | D | P4 |
| workers/utils/scheduler-refresh.ts | 1,622 | block-commitment, commit worker, build-unsigned-tx | A reads + keep | P2 (reads) |
| local-ledger-slot.ts | 449 | 15 files | keep (Ogmios submit-slot) | keep |

## Control-plane holds and mutation leases
- `withL1ControlPlane` / `withL1ControlPlaneIfAvailable` / `withL1ControlPlaneWaitTimeout` defined at services/globals.ts:371/431/489 (held variant :443), with wait/hold timers, acquisition and timeout metrics and a max-hold timeout.
- Takers: fibers/block-commitment, block-confirmation, merge, operator-watchdog, speculative-commit-builder, user-event-barrier-refresher, user-event-ingestion, commands/listen-router, services/history-signed-header-recovery.
- `state_queue_mutation_leases` (database/stateQueueMutationLeases.ts, 476) and `local_mutation_jobs` (database/mutationJobs.ts, 212); schema in database/migrations/index.ts.
- Plan E deletes these. Open item: ingestion fibers also take the hold, so it serializes DB ingestion as well as L1 submission; E must also serialize wallet-UTxO selection across all own txs, not only commits.

## L1 read call sites (Kupo / provider / Ogmios reads)
179 call-site lines (full file:line list: l1reads-union.txt in this scratchpad); 140 outside commands/ and index.ts. The count includes both primitive reads (`utxosAt*`, `utxosByOutRef`, `utxoByUnit`, `wallet().getUtxos()`, `awaitTx*`, Kupo HTTP `/matches` `/checkpoints`, Ogmios `queryLedgerState/*` `queryNetwork/*` `findIntersection` `nextBlock`) and named helper invocations that wrap them (`SDK.fetch*Program`, `fetchStateQueueSnapshotProgram`, `fetchReferenceScriptUtxos*`, `fetchActiveOperatorUtxos`, `fetchKupo*`), so a few logical reads appear twice (wrapper plus inner call).
By file (count): correction-observer 12+, l1-tx-order-carriage 9+, initialization 7, reference-scripts ~13, reference-publication 6, da-attestation 5, state-reconciliation 5, commit-block-header/state-queue 4, transactions/utils 4, merge-to-confirmed-state 5, event-history transport/chain 4 each, reserve-payout 7, availability-challenge 4, scheduler-refresh 6, merge.ts 3, block-commitment 2, attestation-timeout-correction 3, fetch-and-insert-* 5, barrier-refresher 3, commit worker 4, confirm-block-commitments 3, others 1–3.
Plan-relevant: services/native-ledger.ts makes a `NativeLedgerKupmios` whose reward-account reads come from a local cardano-node ledger query, a third L1 read path that the plan does not mention.

## Behaviours not covered by the plan (tentative, pending deep read)
1. **Every own L1 tx besides commits.** Plan E steps 2–4 cover commits (tail-spending) only. The node also signs and submits merge, DA attestation (transactions/da-attestation.ts, from da/startup.ts), attestation-timeout correction, operator register/exit/takeover, reserve payout, reference-script publication and sweep, script-reward registration, PHAS membership, availability-challenge registration, and event-history list inserts (transactions/event-history-submission.ts with its own durable journal database/eventHistorySubmissions.ts). All of these go through transactions/utils.ts `submitSignedTxWithRecovery` and `handleSignSubmit`. Each needs a "can it still land" rule, and none has a tail-nonce analogue.
2. **Operator wallet UTxO contention and Lucid's pinned override view.** operator-wallet-view.ts plus handleSignSubmit pin predicted change via overrideUTxOs. The plan puts the own wallet in A but never says how in-flight own spends (B) are subtracted from the wallet view, or how the held change of a dead intent is released.
3. **Tx-order carriage needs more than outputs.** l1-tx-order-carriage.ts (1,194 lines) walks ChainSync from a Kupo creation point, rejects on rollback ("Rollbacks fail the read; they never widen it", :805), and resolves datum hashes through Kupo `?resolve_hashes`. The plan's `l1_txs(hash, slot, body, redeemers)` omits witnesses, datum-hash preimages and the ancestor-point lookup.
4. **Spend-attribution reads for correction/fraud observation.** state-queue-correction-observer.ts uses Kupo `/matches/*@tx` and spend lookups (:842, :889, :1205, :1435–1561) to find who spent a queue or correction-lock outref and the historical outputs. The plan's `spent_tx` covers the spender id, but the observer also needs the spender's full outputs and resolved datums at historical points. The same applies to commands/availability-challenge-source.ts.
5. **Event-history list provenance and authority.** l1-event-history-provenance.ts and database/eventHistoryAuthority.ts (777 lines, 17+ importers) verify list-node provenance and authority material. A plain slot-tagged UTxO store does not reproduce this, and the plan never says where it lives.
6. l1-event-history-initialization.ts is already dead in production (tests only).
7. The horizon-lag change in history-commit-window.ts (D1 pairing) has no owning phase in section 7.
8. Kupo also serves `/checkpoints` for reference publication (transactions/reference-publication-provider.ts:62). Removing Kupo needs a replacement for that too.
