# Recovery callers and commit chaining — read-only investigation

Branch: colll78/canonical-v1-watcher-l1-source-checkpoint (HEAD 32d87e2ca plus the uncommitted working tree).
Paths are relative to `demo/midgard-node/` unless they start with `docs/` or `demo/`.
Plan: `docs/exec-plans/node-l1-rollback-redesign.md`. §1 is at lines 11-37, §4.B at 159-181, §4.E at 235-283 and §9 at 334-350.

## Headline

1. **§9 item 1 does not hold.** The coverage path is live in production. There are two ways in:
   - `prepareSignedHeaderRecovery` runs unconditionally on every pending reconciliation (`src/services/event-history-runtime.ts:139-150`).
   - `signedHeaderRecoveryHoldSlot` sets the retention hold on every journal append (`src/services/event-history-owner.ts:600-625`).

   Its scope has no equivalent in expired-intent release. That scope is a landed (Observed or Finalized), deposit-only own block that was later rolled back together with its deposits' funding. Deleting the coverage path means §4.E step 1 must first cover rollback of *landed* own blocks.
2. **The expired-intent release only runs under `MPF_ENGINE=architecture_g`** (`event-history-runtime.ts:79-90, 155-168, 184-194`). The default engine is `legacy` (`src/config.ts:1088`), and no tracked compose or deployment config sets `MPF_ENGINE`. Every commit records a signed intent whatever the engine is. Under `legacy`, nothing resolves an expired signed commit, so the active-journal guard blocks all further commits. This is a latent wedge (details in Q1a).
3. **§9 item 4: no regression.** "One active own commit" is exactly today's invariant. One wording fix is needed: §4.B says a child is "built only after its parent has landed". The speculative builder, which is optional and off by default, builds *before* the parent lands and submits only after. The text should say "signed/submitted only after".
4. **Q3:** there is no evidence anywhere in the repo that `t1-recover.sh` or the T1 drill has ever passed. This matches plan line 318.

---

## Q1a — Production call paths

### Expired-intent release (`src/services/history-expired-intent-release.ts`, 822 lines)
Production chain:
- `src/index.ts:49` (import) → `:453` `runNode`
- → `src/commands/listen.ts:238` `makeProductionEventHistoryOwner`
- → `src/services/event-history-runtime.ts`:
  - `preparePendingReconciliation` (`:115-169`) → `prepareExpiredIntentRelease` (`:155-168`), gated on `rewindAuthority`.
  - `reconcile` (`:180-215`) → `expiredIntentReleaseDisposition` (`:184-194`), with the same gate.
- `rewindAuthority` is set only when `MPF_ENGINE === "architecture_g"` and the manifest is present (`event-history-runtime.ts:79-90`).
- The owner retries pending reconciliation at `src/services/event-history-owner.ts:540-556`.

There are no CLI callers. Test-only callers:
- `tests/helpers/history-production-owner-lifecycle.ts:291`
- `tests/helpers/production-history-binding.ts:33`
- `tests/event-history-ready-append.test.ts`

All three go through `makeProductionEventHistoryOwner`.

### Coverage / signed-header recovery (`src/services/history-signed-header-recovery.ts`, 453 lines)
There are two production entries, both reached from `listen.ts:238`:
1. `prepareSignedHeaderRecovery` at `event-history-runtime.ts:139-150`. This entry is **unconditional**, with no engine gate, and it runs *before* expired release in the same pass.
2. `signedHeaderRecoveryHoldSlot`:
   - Imported at `event-history-owner.ts:47` and called on every append at `:600-625`, where `holdSlot` is passed to `Journal.append`.
   - Consumed by the retention logic in `src/database/eventHistoryJournal.ts:879-889` and `:919-940`.
   - Surfaced on health output as `eventHistoryRetentionHold` at `src/commands/listen-router.ts:1589-1595` and `:1714`.

   The owner's retention state is at `event-history-owner.ts:107-117` (type), `:228`, `:809-845` (`noteRetention`) and `:981-982`.

There are no CLI callers. No test calls `prepareSignedHeaderRecovery` directly. It runs, as a no-op, only inside the production-owner test helpers listed above.

What the coverage path does:
- Candidates are non-abandoned headers whose deposit members have non-`origin_canonical` incarnations (`:78-89`).
- It requires exactly one candidate (`:175`), whose status is Finalized or ObservedWaitingStability and which is deposit-only (`:182-194`).
- There must be no other non-terminal journal and an empty processed mempool (`:202-206`), plus `covered_absent` evidence (`:220-239`).
- It then:
  - validates the state queue
  - abandons the journal
  - calls `materializeCanonicalHistory` with `AuthorizedHistoryHeaderRetirement` (`src/database/eventHistoryLedgerRepair.ts:16-18, 175-187`)
  - updates the MPF marker
  - reconciles the deposit projection.

### The legacy-engine wedge
Every commit records a signed intent:
- `submitWithDurableIntent` at `src/workers/commit-block-header/submission.ts:687-703`, used at `:1065` and `:1614`.
- This calls `recordSignedIntent` at `src/database/pendingBlockFinalizations.ts:2356`.

No other path clears an expired signed commit:
- The confirmation worker abandons an expired commit only when `intendedTxHash == null` (`src/workers/confirm-block-commitments.ts:320`).
- `StaleUnconfirmedRecoveryOutput` only logs "Signed commit intent is unresolved…" and leaves it to the history owner (`src/fibers/block-confirmation.ts:772-784`, log at `:781`).

The history owner's release is gated to `architecture_g`. So under the default `legacy` engine (`config.ts:1088`), an expired signed commit stays in an active status forever. The active-journal guard (`pendingBlockFinalizations.ts:1996-2012`) then refuses every later commit.

`MPF_ENGINE` is not set in any tracked config:
- `git grep` over `config/`, the compose files and `operator-compose.mjs` finds nothing.
- `.env.example:278` has it commented out.

I did not verify which engine the preprod runs used.

## Q1b — Can both paths act on the same journal? Which wins?

**No. Their preconditions do not overlap:**
- Coverage acts only on Finalized or ObservedWaitingStability (landed) rows, and only when no other non-terminal journal exists (`history-signed-header-recovery.ts:182-206`).
- Expired release acts only on the single active row that is *unlanded*, meaning PendingSubmission, SubmittedLocalFinalizationPending or SubmittedUnconfirmed (`history-expired-intent-release.ts:107-111, 144-167`).

A journal satisfies at most one of these, so neither path "wins" on a shared input.

Ordering and interlocks:
- In one pass, the signed-header path runs first (`event-history-runtime.ts:139-150` before `:155-168`).
- A retained `signed_header` recovery plan for a different header makes expired release defer (`history-expired-intent-release.ts:532-541`).
- Under `legacy`, only the coverage path is live.

## Q1c — What is dead if the coverage path is deleted (`wc -l`)

### Whole source files — 1,056 lines

| File | Lines |
|---|---|
| `src/services/history-signed-header-recovery.ts` | 453 |
| `src/services/signed-intent-canonical-coverage.ts` | 250 |
| `src/database/eventHistoryCanonicalCoverage.ts` | 239 |
| `src/services/history-recovery-state-queue.ts` (only importer is the signed-header path) | 114 |
| **Total** | **1,056** |

### Partial source removals (approximate)
- `event-history-runtime.ts:21, 94, 101, 138-150`.
- `event-history-owner.ts`: about 60 lines — the import at `:47`, the hold at `:600-625`, and retention state and accessor at `:107-117, :228, :809-845, :981-982`.
- `eventHistoryJournal.ts`: about 35 lines — `holdSlot` at `:879-881`, `RetentionHold` at `:884-889`, and the hold branch in `retain()` at `:919-940`.
- `listen-router.ts:1589-1595, 1714`: about 8 lines.
- `eventHistoryLedgerRepair.ts`: about 15 lines — `AuthorizedHistoryHeaderRetirement` at `:16-18, 175-187`, whose only other importer is the signed-header path.

### Whole test files — 1,089 lines

| Test file | Lines |
|---|---|
| `tests/signed-intent-canonical-coverage.test.ts` | 508 |
| `tests/event-history-canonical-coverage.test.ts` | 403 |
| `tests/history-recovery-state-queue.test.ts` | 178 |
| **Total** | **1,089** |

### Partial test removals
- `tests/event-history-retired-membership.test.ts:759-930`, about 170 lines.
- `tests/history-source-owner-retention.test.ts`: the case at `:124` ("holds retention visibly").
- `tests/event-history-journal.test.ts`: `loadCanonicalHistoryCoverage` uses at `:1946, 2128, 2181`.
- `tests/helpers/history-journal-retention.ts` (8 lines).
- One reference each in `readiness-history-frontier-route.test.ts` and `event-history-ready-append.test.ts`.

### Must NOT be deleted: shared with expired release
- `src/database/eventHistoryRecoveryPlans.ts`:
  - `SIGNED_HEADER_RECOVERY_DOMAIN` (`:42`)
  - `prepareRetainedNativeHistoryRecoveryPlan` (`:261`)
  - `retainedPreparedRecoveryPlan`, whose kind is `"signed_header"` (`:329-355`). The name is misleading, because expired release uses it too.
- `src/services/history-dependent-recovery.ts`.
- The check `retained?.kind === "signed_header"` in `src/services/state-queue-correction-rewind.ts:690`.
- `assertNoUnreconciledSignedSubmission` (`pendingBlockFinalizations.ts:1762-1783`, called at `submission.ts:882-885, 1408-1411`). It is not part of the coverage path, and the plan keeps it.

## Q1d — `history-expired-intent-release.ts` against §4.E steps 1–4

| §4.E step | Status | Present | Gap |
|---|---|---|---|
| 1. Follow the landed chain | PARTIAL | <ul><li>`decide` (`:403-494`) handles *landed*: it calls `recordConfirmedPendingBlock` (`:627-661`).</li><li>Revive applies only to "replacement" abandonments (`:447-459`; `canonical-journal-recovery.ts:186-189`), so correction-abandoned journals are never revived.</li><li>Revive integrity checks: no member tx in immutable (`canonical-journal-recovery.ts:221-231`), and a CAS on the MPF marker (`:281-295`).</li><li>Integrity halts: active row SubmittedUnconfirmed (`:462-468`) or a blocking sibling (`:470-486`).</li></ul> | <ul><li>Runs only at or after the TTL and only for *unlanded* rows. Before the TTL, "landed" is a single Kupo sighting by the confirmation fiber.</li><li>A landed row that is rolled back later is out of scope; that is the coverage path's job.</li><li>A second revive site exists: `block-confirmation.ts:359, 680-705` (`reviveCanonicalPayloadJournalFromWorkerSnapshot`).</li><li>All checks are at tip depth; there is no k-depth.</li></ul> |
| 2. Can it still land? | MINIMAL | <ul><li>Checkpoint head slot ≥ TTL (`:227`, rechecked at `:544`).</li><li>Tail-slot occupancy stands in for "inputs spent" (`:426-445`), but only after the TTL.</li><li>Defers if a correction removed the base (`:420-425, 616-626`) or a correction rewind is owed (`:527-530`).</li></ul> | <ul><li>The slot comes from the lagging journaled checkpoint, not the wall-clock slot.</li><li>No checks for reference inputs, dependency intents, `valid_from`, the append fence or expired unattested suffix, whether the scheduler still names us, or root equality.</li></ul> |
| 3. Resubmit the exact bytes | ABSENT | <ul><li>"Never replaced when undecodable or TTL-less" is present (`signedTtl` `:120-133`; reported once at `:221-226`).</li><li>The lost-response case waits via `assertNoUnreconciledSignedSubmission`.</li></ul> | <ul><li>Nothing in the node resubmits `signed_tx_cbor` (grep of its users).</li><li>`BadInputs` and `OutsideValidityInterval` are classified only inside one submit attempt (`src/transactions/utils.ts:60-115`).</li></ul> |
| 4. Replacement on the same tail | PARTIAL (delegated) | <ul><li>On replace or revive, the module:<ul><li>reopens the members (`reincludeStateQueueCorrectedBlocks`)</li><li>abandons the row (`abandonLocalBlockFinalization`)</li><li>releases the lease</li><li>CASes the MPF marker (`:779-792`)</li><li>revives if needed (`:796`)</li></ul></li><li>The normal commit fiber then builds from a fresh L1 tail (`src/fibers/block-commitment.ts:864-871`), reusing the whole build path: turn, reserves, EWMA and DA cap.</li></ul> | <ul><li>The replacement spends D only if D is still the tail. If a foreign block took the slot, it builds on the new tail.</li><li>Replacement is not pinned to D.</li><li>Nothing is resubmitted.</li></ul> |

---

## Q2a — The "one active own commit" guard

Today no second commit is built on the confirmed tail, signed or submitted while the previous one is unconfirmed. Six layers enforce this:

1. **Submit sets local-finalization pending.** Production submission emits only `SubmittedAwaitingConfirmationOutput` (`submission.ts:818, 1337`). The handler for it (`block-commitment.ts:1228`) sets `LOCAL_FINALIZATION_PENDING=true` (`:1244`).
2. **The commit fiber holds while pending.**
   - It refreshes from L1 only if `!LOCAL_FINALIZATION_PENDING` (`block-commitment.ts:864`).
   - `resolveAuthoritativeLocalFinalizationPreflight` (`:129-176`) forces pending=true whenever an active submitted journal exists.
   - The pre-lease scheduler plan short-circuits (`:388-391`).
3. **The worker refuses to build.**
   - `canBuildOnConfirmedBlock` requires `!localFinalizationPending` (`src/workers/commit-block-header.ts:1793-1795`).
   - `shouldDeferCommitSubmission` (`src/workers/commit-block-planner.ts:705-709`, called at `commit-block-header.ts:2078-2090`) returns NothingToCommit.
   - After landing, the worker runs only `recoverLocalFinalizationAgainstConfirmedBlock` (`:2092-2110` → `markFinalized`, `commit-submission.ts:488`). The next build happens on a later tick.
4. **Hard DB guard.** `preparePendingSubmission` refuses while another row is in `ACTIVE_STATUSES` (`pendingBlockFinalizations.ts:1996-2012`). `ACTIVE_STATUSES` (`:140-145`) includes ObservedWaitingStability.
5. **Lost-response guard.** `assertNoUnreconciledSignedSubmission` (`pendingBlockFinalizations.ts:1762-1783`, called at `submission.ts:882-885, 1408-1411`) refuses while a PendingSubmission row with `intended_tx_hash` exists.
6. **Cross-process lease.** Work runs under the `"block_commitment"` lease in `state_queue_mutation_leases` (`tryWithLease`).

## Q2b — What "landed" means, and the minimum interval

**"Landed" is a single Kupo sighting at depth 0:**
- `confirm-block-commitments.ts:281` runs the targeted `probeSubmittedTx`, then falls back to a state-queue scan. The full-scan cadence is `TARGETED_MISS_FULL_SCAN_INTERVAL_MS=20_000` (`src/workers/utils/confirm-block-commitments.ts:69, 85`).
- A sighting moves the row to ObservedWaitingStability (`observeConfirmedPendingBlock`). That status is still active.
- Local finalization then moves it to Finalized.
- The next commit does not wait for any depth or stability.

**Minimum interval between two own commits** is the sum of:
- the parent's L1 inclusion (activeSlotsCoeff 0.05 and slotLength 1 give about 20 s expected; `demo/midgard-node-tools/devnet/preprod/configuration.json:15-16`)
- a confirmation poll (`WAIT_BETWEEN_BLOCK_CONFIRMATION` default 2000 ms, `config.ts:490-492`)
- a commit tick for local finalization (`WAIT_BETWEEN_BLOCK_COMMITMENT` default 1000 ms, `config.ts:487-489`)
- the next tick's build, sign and submit.

These timers are **not** inter-commit waits:
- The 30/20/10 s reserves (`src/workers/utils/history-commit-window.ts:31-52`) are the minimum budget left before each commit's *own* end time at pre-witness, pre-build and pre-submit. The generic budgets are 6/3/2 min (`src/workers/utils/commit-end-time.ts:19-21`).
- `UNCONFIRMED_BLOCK_MAX_AGE_MS` (default 180_000, `config.ts:560-562`) only triggers a warning (`confirm-block-commitments.ts:331-334`).

## Q2c — Speculative L2 build: where it happens, and whether the retention preserves it

**It exists, but it is optional and off by default:**
- `SPECULATIVE_COMMIT_BUILD` defaults to false (`config.ts:493-495`; `.env.example:97`).
- The README and the decisions doc keep it off (`README.md:349-351`; `docs/midgard/decisions/benchmark-gated-runtime-options.md:9`).
- Its fibers are wired only when it is enabled (`listen.ts:421-431`), and it requires a non-legacy engine (`config.ts:1089-1100`).

**Build happens before the parent lands:**
- `runSpeculativeCommitBuilderOnce` (`src/fibers/speculative-commit-builder.ts:670-800`) builds a child on the active submitted journal.
- It sets the base outref to `${submittedTxHash}#0` (`commit-block-header.ts:1822-1824`) and defers database writes (`:2286`).
- It stays provider-free and unsigned until an instruction arrives (`awaitSpeculativeInstruction`, `:2495-2505`).

**Submit happens after the parent lands:**
- `SuccessfulConfirmationOutput` offers `COMMIT_SUBMIT_WAKE_QUEUE` (`block-confirmation.ts:753-762`).
- `submitSpeculativeCandidateOnConfirmation` (`speculative-commit-builder.ts:996-1182`) takes the lease and re-reads the L1 tail. It invalidates the candidate (T2) unless tail == base (`decideSpeculativeInstructionForLiveTip` `:273-295`; worker check `commit-block-header.ts:2508-2524`).
- It local-finalizes the parent first (`instruction.localFinalizationBlock`, `:2525` onward), so the active-journal guard (Q2a layer 4) still holds.

**Does the retention preserve it?** Yes, in substance: speculative mode still keeps one active own commit. §4.B's wording ("built only after its parent has landed", lines 159-181) would literally forbid the speculative prebuild. It should read "signed/submitted only after its parent has landed".

## Q2d — Does the retention regress throughput or latency?

**No.** The rule is exactly today's invariant (Q2a), both with and without speculative build (Q2c). The only fix needed is the §4.B wording ("built" → "signed/submitted"), so speculative prebuild is not written out of the design.

---

## Q3 — Has `t1-recover.sh` or a similar drill ever passed?

**No evidence of a passing live run.**

What exists:
- The script is `demo/midgard-node-tools/devnet/phase4-process/scripts/t1-recover.sh`. Its last commit is a46ec6222 (2026-09-10), and it requires the local-devnet tokens (`:13-27`).
- The drill is specified in:
  - the phase4-process README (`:170-200`)
  - `docs/PHASE4_PIPELINED_COMMIT_PROCESS_ACCEPTANCE.md:45, 70-101`
  - `e2e-pipelined-commit-process-acceptance.ts:1975, 2089-2170`
  - `src/commands/phase4-t1-recovery.ts`.
- The harness forces `SPECULATIVE_COMMIT_BUILD=true` (`pipelined-commit-process-harness.ts:401`) and sets no `MPF_ENGINE`.
- Only offline checks exist: the verifier `verify-phase4-pipelined-process-summary.mjs`, plus `phase4-t1-recovery.test.ts` and the verifier tests, which run on fixture attestations.

What is missing:
- No `summary.json` or attestation from a real run is committed.
- No CI workflow references the drill.
- `docs/public_testnet_readiness.md` has no entry for it.
- The "T1–T7 … passed" matches in the forced-inclusion docs are speculative-invalidation reason codes, not this drill.

This agrees with plan line 318 ("never yet evidenced passing").
