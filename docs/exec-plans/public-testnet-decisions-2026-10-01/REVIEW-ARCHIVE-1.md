# Archive pass 1

Base: `3027be19cb3bc740f83af0e155e34ab38c7cf894`. Review date: 2026-10-01.

Scope: the full working versions of watcher replay-transcript store, lifecycle,
completion, deployment authority, archive-validation capture, application
construction and runtime retirement hookup; the changed archive tests and
fixture. New files were read in full because an ordinary tracked diff omits
them. Caller tracing additionally covered the decision bridge, target selection,
supervisor, proof progress authority, transcript replay, raw-L1 completed
verification, workflow orchestrator, queue observation source, history recovery
and coordinator delivery. The review used the complete consensus-review skill,
its twelve lenses, fraud-proof invariants and DA invariants. No implementation
edits were made by this review.

The working tree changed during the review. The final caller inspection includes
the runtime hookup at `watcher-runtime.create-watcher-runtime.ts:648` and the
corrected Idle-lock predicate at `replay-transcript-lifecycle.ts:318-319`.
Findings below concern paths still present in that inspected version.

## Ranked findings

```text
[F1] CONFIRMED major
Where: demo/midgard-watcher/src/storage/replay-transcript-completion.ts:44-58;
       demo/midgard-watcher/src/fault-proofs/fault-proof-supervisor.create-supervisor.ts:360-380;
       demo/midgard-watcher/src/fault-proofs/fault-proof-progress-authority.ts:207-210
Defect: A normal successful runner completion never finishes its durable
        replay-transcript pin, and recovery omits that already-completed journal.
Trace: An operator commits a validation fault -> classification archives its
       transcript and creates a NULL completed_slot pin
       (fault-proof-application.create-application.ts:608-628,
       fault-proof-application.archive-validation-capture.ts:34-45,
       replay-transcript-lifecycle.ts:209-226) -> its selected proof runs and
       reaches release finality -> the workflow returns kind "completed"
       (demo/midgard-fault-proofs/src/workflow/orchestrator.run-admitted-fraud-proof-workflow.ts:670-672,743-780)
       -> supervisor remembers completion and removes the objective without
       calling dependencies.verifyCompleted (:360-380) -> the only production
       completeOperation caller, verifyCompletedWatcherReplayTranscriptWorkflow
       (:44-58), has not run -> same-generation coalesced completion can be
       accepted from the completedValidations cache (:271-276) -> restart also
       skips completed journals (fault-proof-progress-authority.ts:207-210)
       -> retirement refuses the NULL pin forever
       (replay-transcript-lifecycle.ts:317) -> repeating such successful proofs
       consumes archive rows/bytes until a later classification throws the
       capacity error (replay-transcript-store.ts:357-366), stopping proof
       intake despite the old operations being finished.
Reach: A malicious operator can supply real validation faults; honest successful
       proofs are the trigger, rather than forged completion input. The runner
       performs its own canonical terminal verification, but that verification
       has no replayTranscriptStore and therefore does not update these pins.
       A changed-generation coalesced invocation can happen to call the new
       wrapper; an uncontended completion has no such guaranteed invocation.
Lens: Replacement paths keep every guard; gates that cannot fail; both polarities.
```

```text
[F2] CONFIRMED major
Where: demo/midgard-watcher/src/fault-proofs/fault-proof-application.archive-validation-capture.ts:34-45;
       demo/midgard-watcher/src/fault-proofs/fault-decision-bridge.create-bridge.ts:180-367,456-459;
       demo/midgard-watcher/src/storage/replay-transcript-lifecycle.ts:317
Defect: Decisions captured but never selected receive permanent operation pins
        with no admitted cancellation or supersession completion path.
Trace: An operator puts two provable faulty headers in the authenticated queue,
       with a validationTraceDispute fault in the descendant -> bridge classifies
       every candidate header before selecting a target
       (fault-decision-bridge.create-bridge.ts:180-367) -> descendant capture
       durably pins its own decisionDigest before target selection
       (fault-proof-application.create-application.ts:608-628,
       fault-proof-application.archive-validation-capture.ts:34-45) -> Idle
       target selection chooses only the first fault
       (fault-decision-bridge.selected-target.ts:252-260) -> retainDecisionAuthorities
       drops the descendant's private capture
       (fault-proof-application.create-application.ts:507-515) -> proof of the
       ancestor prunes its descendants without proving each descendant
       (onchain/aiken/validators/state-queue.ak:651-698) -> the descendant has
       no workflow journal ending in completed, which the sole admitted pin
       completion door requires
       (replay-transcript-completion.ts:44-58;
       demo/midgard-fault-proofs/src/workflow/completed-verification.ts:46-83)
       -> its completed_slot remains NULL despite target absence, Idle lock and
       every retention horizon passing -> retireExpired always skips it (:317)
       -> repeating these queues eventually fills the archive and makes later
       validation-fault classification fail closed on capacity
       (replay-transcript-store.ts:357-366).
Reach: The queue may contain descendants before an earlier fraud proof takes
       the correction lock. Classification deliberately scans all headers and
       admission allows genuine validation faults. Descendant pruning is the
       supported correction path; nothing requires a separate completed proof
       for the descendant. Ordinary classification retirement after a rollback
       or a generation change can likewise strand a pin created before the
       final currency fence, although the descendant trace alone proves this
       finding.
Lens: Replacement paths keep every guard, including cancellation/removal exits.
```

```text
[F3] CONFIRMED major
Where: demo/midgard-watcher/src/runtime/replay-transcript-retirement.ts:40-52;
       demo/midgard-watcher/src/runtime/state-queue-runtime.ts:271-278
Defect: Quiet finalized canonical progress cannot advance the archive retirement
        clock because the hook requires a fresh queue observation at that exact
        quiet block, and queue observations are not refreshed on that path.
Trace: A completed transcript's target is removed at a touched block N before
       its completion retention horizon -> source.observe publishes its latest
       finalized queue observation at N
       (authenticated-state-queue-observation.create-watcher-state-queue-observation-source.ts:248)
       -> later canonical blocks are quiet because they spend no tracked queue
       or user-event output -> queue onFinalized returns without source.observe
       (state-queue-runtime.ts:271-278), retaining latestFinalized at N -> even
       after every retention deadline passes, the hooked sweep compares the
       observation at N with finalized nativeBlock N+k and returns at the
       mismatch (replay-transcript-retirement.ts:40-52) -> expired complete
       chains cannot be reclaimed during the quiet interval -> at a full archive
       a new validation fault can fail its inclusion classification on capacity
       before that block's finalization can create a fresh queue observation
       (chain-coordinator.create-coordinator.ts:725-732;
       state-queue-runtime.ts:243-249;
       replay-transcript-store.ts:357-366), losing live proof intake.
Reach: An operator controls whether it submits further rollup blocks. A quiet
       Cardano interval after correction is a supported runtime shape; quiet
       block coverage authenticates unchanged queue relevance but the new hook
       does not use that authenticated progress. The hook is now wired in
       watcher-runtime.create-watcher-runtime.ts:648-656, so this is a live
       integration defect rather than an unused helper observation.
Lens: Ledger facts rather than builder assumptions; replacement paths preserve
      timeout and reclamation behavior.
```

All three findings are ranked major because their direct effect is watcher
liveness at a reachable storage bound; this review found no direct dishonest
on-chain acceptance or value theft in the changed archive code.

## Lenses

1. Parameter trust: clean. Retention comes from the signed deployment manifest;
   there is no new on-chain parameter check or datum treated as a deployment
   parameter.
2. Always-succeeds scripts: clean. No script loading, application arity, yield
   handshake or validator arm changed in this scope.
3. Decoders and pinned compiler: clean for archive integrity. Current CBOR head
   identity, bytes digest and record digest are checked; persisted transcript
   bytes are independently replayed before challenge admission. No new Aiken
   decoder or compiler output was introduced here.
4. Value conservation: clean. Archive lifecycle makes no token, payout, fee or
   mint/burn change.
5. Anchoring rather than preimage: clean. Capture binds the exact admitted header,
   inclusion identity, deployment and fresh replay semantics; archived bytes
   cannot directly mint replay authority.
6. Reference scripts: clean. No new fault-proof step or inline attachment enters
   through these changes; existing deployment binding remains on the application
   path.
7. Both polarities: F1 and F2. The lifecycle unit scenario manually completes
   every pin and therefore does not cover ordinary successful-run completion or
   a never-selected captured decision.
8. Gates that cannot fail: F1. The new complete-operation guard is bypassed by
   the supervisor's uncontended success/cache path. The initial retirement test
   also used a null correction lock that production initialized state cannot
   supply; that fixture and predicate were corrected during this review.
9. Execution and size budgets: clean within measured claims. No new on-chain
   transaction work or execution-ledger row changed. Archive iteration is bounded
   by declared storage/pin ceilings; wall time at the largest archive shape was
   not measured, so no performance verdict is claimed.
10. TypeScript and Aiken twins: clean. No consensus codec or proof computation twin
    is replaced. Transcript lifecycle encoding is local SQLite metadata.
11. Ledger facts rather than builder assumptions: F3. The cached queue point is
    not the current native finalized point during quiet canonical progress.
12. Replacement paths keep every guard: F1, F2 and F3. Completion, discarded
    decisions and quiet reclamation do not all reach the new lifecycle transitions.

## Checks and evidence

Gates relevant to this TypeScript scope: watcher format-check, lint, typecheck,
build and watcher tests selected by preflight; focused archive/deployment tests;
completion and runtime integration scenarios. No golden generator or Aiken
execution-ledger channel is selected by these archive-only edits. Required full
change verification remains with the change owner; this pass did not repeat the
full suite.

- 2026-10-01, repository root: `env PATH=/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH node scripts/doctor.mjs`:
  exit 1 in the restricted sandbox; blueprint stamp was fresh, Node matched,
  dependencies and hooks were present. Compiler/Postgres probes encountered
  EPERM and the dist/pnpm probes could not execute. No compiler/Postgres failure
  is inferred from those sandbox results.
- 2026-10-01, repository root, elevated execution:
  `env PATH=/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH MALLOC_MMAP_THRESHOLD_=131072 /home/gumbo/.nvm/versions/node/v22.22.2/bin/node /home/gumbo/.cache/node/corepack/v1/pnpm/9.15.4/bin/pnpm.cjs --dir demo/midgard-watcher exec vitest run tests/storage/replay-transcript-store.test.ts tests/runtime/deployment-identity.test.ts`:
  first run collected 41 tests, 40 passed and 1 failed, exit 1. The retirement
  scenario failed `expected 1 to be +0` while the shared Idle predicate/fixture
  were being updated. This is recorded as an unstable-snapshot run, not a final
  passing gate or a claimed red-check of a particular fix.
- 2026-10-01, repository root, elevated execution, final focused rerun:
  `env PATH=/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH MALLOC_MMAP_THRESHOLD_=131072 /home/gumbo/.nvm/versions/node/v22.22.2/bin/node /home/gumbo/.cache/node/corepack/v1/pnpm/9.15.4/bin/pnpm.cjs --dir demo/midgard-watcher exec vitest run tests/storage/replay-transcript-store.test.ts tests/runtime/deployment-identity.test.ts`:
  41 collected and 41 passed (14 archive, 27 deployment identity), exit 0.
  The shared checkout had concurrent activity; elapsed time was 43.65 seconds.
  This verifies the updated lifecycle unit cases and deployment loader, not
  the missing completion/selection/quiet-progress integration paths in F1-F3.
- An isolated `/tmp/archive-pass1/idle-lock.test.ts` probe used the same Node/pnpm
  versions. Its first config load exited 1 before test collection because
  bundling the repository's test-support import encountered script shebangs.
  The corrected scratch config run was interrupted with exit 130 after it
  produced no completed test result. Neither is evidence for a finding. No
  repository test or implementation file was changed by those probes.

## Observed closure during this pass

The initial lifecycle predicate skipped every non-null CorrectionLock, including
the authentic initialized `Idle` lock. The final source refuses null and non-Idle
locks instead, and the fixture now models a real Idle lock. Its runtime caller
was also absent initially and is present in the final inspected source. These
two initial observations are structurally closed and are not ranked open
findings. The final focused rerun result is recorded separately below.

## Residual risks and limitations

F1-F3 remain open in the last inspected implementation. Close F1 by coupling
every admitted canonical successful runner terminal to archive completion before
the supervisor removes/caches its objective. Close F2 with an authenticated
terminal lifecycle for never-selected, superseded or canonically removed
dependencies that preserves active evidence and every retention horizon. Close
F3 by binding the cached queue state to serialized authenticated quiet progress
before supplying the retirement clock. These are operational evidence-retention
gaps, not new fault-proof-family coverage gaps, so this report proposes no
remaining-gaps.md family entry.

Not checked: full watcher suite, preflight, format/lint/typecheck/build, live
devnet acceptance, on-chain emulator reruns, maximum-size archive timing, a
storage-ceiling stress test, or the unrelated funding/runtime changes in the
broader dirty tree. The review traced the reachable sources and control flow;
it did not execute an end-to-end adversarial queue containing both ancestor and
descendant faults. No fixes were authored here, so there is no claimed fix
red-check or pass-2 verdict.
