# Archive pass 2

Base: `3027be19cb3bc740f83af0e155e34ab38c7cf894`. Review date: 2026-10-01.

## Outcome and scope

Independent second review of A4 replay-archive retention. This pass found two displaced integration paths and a removal-age recovery gap;
the final inspected fixes close all three first-pass findings and all three
findings recorded below. No remaining confirmed or plausible archive defect was
found in the final scope. Closure is based on source tracing and independent
focused runs, not acceptance of an implementation report. No implementation files were edited
by this review; this report is uncommitted.

Read the full current versions of watcher replay-transcript store, lifecycle,
operation doors, absence witnesses, completion, archive-validation capture, application construction,
deployment authority, supervisor runner-completion helper and runtime retirement
hook/bootstrap wiring. Caller tracing covered transcript capture currency,
classifier decision identity, completed raw-L1 verification, supervisor queue
finish/recovery, historical proof progress, state-queue source/runtime, history
recovery, native coordinator and bounded post-finality recovery. New files were
read directly because tracked diffs omit them. The broader dirty funding,
availability, node, node-tools and public-DA changes are outside this review.

Instructions read: root and demo AGENTS.md (there is no nested watcher AGENTS.md),
consensus-review skill and all twelve lenses, fraud-proof and DA invariant files,
production-l2.md, verification.md, local-test-environment skill and running-suite
reference, and writing-reports-and-prs skill. Relevant gates are the focused
archive/deployment and supervisor/progress/execution suites below. Broad preflight
and package builds belong to the parent review; this reviewer did not run them.

## Ranked findings discovered in this pass

```text
[F4] CONFIRMED major
Where: demo/midgard-watcher/src/fault-proofs/fault-proof-progress-authority.ts:207-210,399-404;
       demo/midgard-watcher/src/fault-proofs/fault-proof-supervisor.create-supervisor.ts:365-380,407-415,633-714
Defect: A durable completed journal can lose its only path to canonical
        verification before its archive operation pin is finished.
Trace: An operator commits a real validation fault -> the runner uses its capture
       (application currentChallenge) and persists a completed journal -> process
       stops before the new runner-completion verifier calls archive
       completeOperation -> restart's progress initialize skips completed entries
       -> the target has already been removed, so no live fault decision is
       available -> production recoverExisting(null) admits no jobs and historical
       progress supplies no reconciliation -> proof_started=1/completed_slot=NULL
       remains pinned forever -> repeated events can exhaust the bounded archive
       and stop classification. A second trace needs no restart: fresh canonical
       verifier returns pending/checkpoint_changed -> supervisor marks its queue
       job finished -> historical progress skips the completed objective on every
       later native observation -> no pending-to-applicable retry is scheduled.
Reach: Real faults are adversary-controlled; restart is supported runtime behavior
       and provider checkpoint changes are a typed production pending outcome.
       The new helper closes normal success but does not close these paths.
Lens: Replacement guards; both polarities; gates that cannot fail.
```

```text
[F5] CONFIRMED major
Where: demo/midgard-watcher/src/runtime/replay-transcript-retirement.ts:42-55 (initial pass-2 source);
       demo/midgard-watcher/src/runtime/chain-coordinator.create-coordinator.ts:592-629;
       demo/midgard-watcher/src/runtime/chain-coordinator.canonical-path-from-history.ts:44
Defect: Relative 2,160-block finality checkpoints and an absolute 240-block
        archive refresh grid can permanently miss one another.
Trace: Last touched finalized anchor is block 10081 -> quiet deliveries beyond
       this anchor fail retirement ready(), which requires delivered blockNo <=
       durable finalized blockNo -> coordinator advances the durable anchor only
       at 12241, 14401, ... (2,160-block relative cadence) -> those checkpoints
       are never divisible by 240 -> refresh is skipped at each checkpoint ->
       on-grid deliveries continue failing ready() -> expiry never advances the
       retirement clock during the quiet interval -> later archive admission can
       fail at its storage ceiling before a new fault finalizes.
Reach: An operator chooses when to stop committing rollup blocks; the last touched
       Cardano block need not be on an absolute 240-block grid. 2,160 is a multiple
       of 240, so no later relative checkpoint repairs this offset.
Lens: Ledger facts; replacement guards; gates that cannot fail.
```

```text
[F6] CONFIRMED major
Where: demo/midgard-watcher/src/storage/replay-transcript-lifecycle.ts:351-374 (inclusion-depth fix inspected during pass 2);
       demo/midgard-watcher/src/storage/replay-transcript-completion.ts:89-97;
       demo/midgard-watcher/src/l1/rollback-engine/recovery.evaluate-watcher-post-finality-recovery-internal.ts:204-216
Defect: Aging the original inclusion beyond the supported rollback depth does
        not prevent a recent removal from rolling back after archive retirement.
Trace: A faulty header was included at block 10 and remains live through 3999 ->
       its successful proof removes it at 4000 -> sparse canonical blocks reach
       4010 only after more than the actual signed retention duration from the
       removal slot -> every time horizon, Idle lock and absence check passes,
       and 4010-10 exceeds 2,160 -> whole transcript identity is deleted -> an
       authenticated supported post-finality replacement with common ancestor
       3999 has rollback depth 11, restoring the same old target while its replay
       original is gone. Never-selected descendants pruned by an ancestor have
       the same trace without a completion terminal.
Reach: Supported post-finality recovery bounds block-number depth, not elapsed
       slots. Terminal observedAt stores removal slot/hash but no block number.
       No existing check binds retirement to the removal/absence block's age.
       Public DA retention is already elapsed; reconstructing deleted originals
       is not guaranteed. Ranked major for recovery/proof liveness loss; no direct
       validator acceptance or value-theft trace is claimed.
Lens: Anchoring; ledger facts; replacement cancellation/recovery guards.
```

## Pass-1 closure and final fix status

- F1: CLOSED, including its F4 displacement. The fresh runner calls the canonical
  verifier before the completion cache and markCompleted callback. Startup now
  retains completed journal objectives, and historical progress permits their
  read-only reconciliation until canonical verification succeeds. A removed
  target with a completed journal is verified after restart; a first pending
  verification is retried on the next observation. The two new objective cases
  use the actual terminal verifier after the controlled pending seam and assert
  no runner, funding allocation or submission. This review's 13-objective and
  2-runner-door tests passed.
- F2: CLOSED for the reported never-selected descendant path. Durable
  classification_complete is privately admitted and bound to the current archive
  head; proof_started remains separately pinned until canonical completion.
  The final seven classification tests passed, including old inclusion/recent
  absence, restart, rollback, live reappearance and witness substitution.
  Interrupted captures that never reach final classification stay pinned.
- F3: CLOSED, including F5. Refresh uses elapsed progress since the last
  authenticated source observation, so off-grid 2,160-block authority checkpoints
  qualify. Ordinary quiet deliveries ahead of the durable anchor defer without
  resetting the absence witness. Recovery/work suspension still clears it.
  The final five runtime tests passed, including off-grid and intervening-quiet
  regressions. A temporary fix reset witnesses on every behind-anchor delivery;
  that displacement was reported and removed before this final run.
- F6: CLOSED. The store persists the first eligible authenticated absence point
  with the whole pin-set digest. Deletion requires strictly more than 2,160
  blocks beyond that point as well as all signed time horizons and inclusion
  recovery depth. Startup, rollback, live reappearance and ineligible dependencies
  clear witnesses without touching transcript originals. Changed pin sets restart
  absence age; witness corruption fails closed. The seven storage lifecycle and
  five runtime regression tests pin these cases.

## Lens coverage

1. Parameter trust: clean. The window is derived from the verified signed
   deployment authority; no new on-chain parameter revalidation appears.
2. Always-succeeds scripts: clean. This scope changes no validator parameter
   application, script loader, yield handshake or validator arm.
3. Decoders/pinned compiler: clean within this scope. Archive head identities,
   row and byte digests and lifecycle checksums are independently checked;
   capture semantically replays persisted bytes. No new Aiken output was built.
4. Value conservation: clean. Retirement changes no value movement or economics.
5. Anchoring: clean after F6 closure. Exact bytes/identity and signed retention
   remain bound; continuous authenticated absence ages beyond supported recovery.
6. Reference scripts: clean. No new proof step, loader or inline attach changes.
7. Both polarities: clean after F4 closure. Fresh success/pending and historical
   completed-journal restart/pending tests pin both authority dispositions.
8. Gates that cannot fail: clean after F4/F5 closure. Final tests drive historical
   reconciliation, off-grid refresh and interleaved ordinary quiet delivery; the
   lifecycle negatives refuse early deletion and retain original bytes.
9. Budgets: no new on-chain work. SQLite ceilings bound rows/bytes/pins; maximum
   archive sweep wall time and storage-ceiling recovery were not measured.
10. TypeScript/Aiken twins: clean. Lifecycle SQLite metadata has no Aiken twin;
    no canonical codec or proof computation was replaced in this scope.
11. Ledger facts: clean after F5/F6 closure. Relative checkpoints advance exact
    admitted observations, and both inclusion/absence depth guard sparse-block
    time expiry. A native checkpoint hash is unique across block numbers.
12. Replacement guards: clean after F4/F6 closure. Main success, historical
    completed-journal restart, pending retries and absence invalidation preserve
    the canonical completion/retention boundaries. Reconciliation permits still
    refuse before_preflight and before_submit.

## Independent checks and observations

All commands ran from `/home/gumbo/midgard-hub/midgard` on 2026-10-01,
concurrently with the parent preflight/build work. No package build was launched.

1. `PATH=/tmp/midgard-toolchain-20261001:$PATH node scripts/doctor.mjs --json`
   — exit 1 under the sandbox: blueprint stamp fresh, Node 22.22.2 matched,
   dependencies and hooks present; native Aiken/child Node/pnpm/Postgres probes
   returned EPERM. This is environment access failure, not a passed doctor.
2. `PATH=/tmp/midgard-toolchain-20261001:$PATH env MALLOC_MMAP_THRESHOLD_=131072 MIDGARD_WATCHER_FORKS=2 pnpm --dir demo/midgard-watcher exec vitest run tests/storage/replay-transcript-store.test.ts tests/storage/replay-transcript-classification-retirement.test.ts tests/runtime/replay-transcript-retirement.test.ts tests/fault-proofs/fault-proof-supervisor-runner-completion.test.ts tests/runtime/deployment-identity.test.ts --reporter=default --reporter=json --outputFile=/tmp/archive-pass2-focused.json`
   — native escalation, exit 0; 5 files, 48 collected/passed, no skipped tests,
   23.03s. This run predates off-grid/depth/absence/completed-recovery fixes.
3. `PATH=/tmp/midgard-toolchain-20261001:$PATH env MALLOC_MMAP_THRESHOLD_=131072 MIDGARD_WATCHER_FORKS=2 pnpm --dir demo/midgard-watcher exec vitest run tests/fault-proofs/fault-proof-progress-authority.test.ts tests/fault-proofs/fault-proof-supervisor-completed-resume.test.ts tests/fault-proofs/fault-proof-objective-progress.test.ts tests/fault-proofs/fault-proof-execution.test.ts --reporter=default --reporter=json --outputFile=/tmp/archive-pass2-recovery.json`
   — native escalation, exit 1; 4 files, 39 collected, 38 passed, 1 failed,
   no skipped tests, 44.30s. The coalesced-generation test at objective-progress
   line 380 expected one completion verification, observed two after fresh-success
   verification was added. This is an expectation requiring review, not a
   pre-existing red; the author was notified.
4. Isolated `/tmp/archive-pass2-removal-depth.test.ts` probe, same toolchain,
   `vitest run /tmp/archive-pass2-removal-depth.test.ts --config /tmp/archive-pass2.vitest.config.mts --reporter=default --reporter=json --outputFile=/tmp/archive-pass2-removal-depth.json`
   — initial config load exited 1 before collection because bundled repository
   test-support imports contain script shebangs; corrected dynamic-import config
   started but produced no completed test result and was stopped (exit 130).
   Neither result is evidence for F6. No implementation mutation or fix red-check
   was performed in the shared tree.

5. `PATH=/tmp/midgard-toolchain-20261001:$PATH env MALLOC_MMAP_THRESHOLD_=131072 MIDGARD_WATCHER_FORKS=2 pnpm --dir demo/midgard-watcher exec vitest run tests/storage/replay-transcript-store.test.ts tests/storage/replay-transcript-classification-retirement.test.ts tests/runtime/replay-transcript-retirement.test.ts tests/fault-proofs/fault-proof-supervisor-runner-completion.test.ts tests/runtime/deployment-identity.test.ts tests/fault-proofs/fault-proof-progress-authority.test.ts tests/fault-proofs/fault-proof-supervisor-completed-resume.test.ts tests/fault-proofs/fault-proof-objective-progress.test.ts tests/fault-proofs/fault-proof-execution.test.ts --reporter=default --reporter=json --outputFile=/tmp/archive-pass2-final.json`
   — native escalation, exit 1; 9 files, 95 collected, 94 passed, 1 failed,
   no skipped tests, 79.92s. The intervening-quiet runtime regression reused
   a synthetic block hash across two block numbers: refresh correctly did not
   re-query that same hash and then refused the mismatched block number. The
   fixture was changed to give each checkpoint a distinct hash; no production
   exact-point gate was relaxed.
6. `PATH=/tmp/midgard-toolchain-20261001:$PATH env MALLOC_MMAP_THRESHOLD_=131072 MIDGARD_WATCHER_FORKS=2 pnpm --dir demo/midgard-watcher exec vitest run tests/storage/replay-transcript-store.test.ts tests/storage/replay-transcript-classification-retirement.test.ts tests/runtime/replay-transcript-retirement.test.ts tests/fault-proofs/fault-proof-objective-progress.test.ts --reporter=default --reporter=json --outputFile=/tmp/archive-pass2-final-rerun.json`
   — native escalation, exit 0; 4 files, 39 collected/passed, no skipped tests,
   54.22s. All changed fixture/objective tests were rerun after the correction.
   Together with the five unaffected files in run 5 this independently covers
   all 95 final focused tests; this is not a claimed single 95-test green run.
7. `PATH=/tmp/midgard-toolchain-20261001:$PATH node --version` and
   `PATH=/tmp/midgard-toolchain-20261001:$PATH pnpm --version`
   — each exit 0; observed Node v22.22.2 and pnpm 9.15.4.

## Residuals, rulings and limitations

Open confirmed/plausible findings in the final inspected archive scope: none.
Uncertain/incomplete capture and legacy pins are intentionally not retired.
Restart/recovery conservatively resets absence age, so reclamation can be delayed
by at least 2,161 further canonical blocks. Operational capacity remains fail
closed. Maximum-capacity sweep timing is unmeasured; the SQLite archive test
verifies reclaimed capacity and allocated-page reuse at a small configured
ceiling. The existing cumulative 2,048 workflow-directory recovery bound remains
unchanged; retaining completed reconciliation objectives does not remove that
pre-existing operational bound.
No new fault-proof-family coverage gap is proposed for remaining-gaps.md.

Not checked: broad watcher/preflight gates, format/lint/typecheck/build, live
devnet acceptance, on-chain emulator reruns, actual supported post-finality
replacement after archive deletion, maximum-size timing, and unrelated changes
in the shared dirty tree. Parent/author reports are not counted as independently
run evidence. User node-tools work and plutus.json were preserved. No approval
or owner ruling was inferred; no external messages, commits or pushes were made.
