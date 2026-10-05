# Archive pass 3

Base: `3027be19cb3bc740f83af0e155e34ab38c7cf894`. Review date: 2026-10-01.

## Outcome and scope

Final adversarial review found no new confirmed or plausible defect in the A4
archive batch. F1-F6 from the preceding two reports are CLOSED for their stated
traces. This is a source-review and focused-test conclusion, not a claim that
live acceptance or the parent's broad preflight was independently repeated.

Read the current tracked diff against the base and the new files directly:
replay-transcript store, lifecycle, operation doors, completion and absence;
validation archive capture and application construction; verified deployment
authority; supervisor runner-completion admission, supervisor scheduling and
progress authority; runtime retirement and bootstrap wiring; and the completed
journal reconciliation-permit change. Read the related archive, classification,
runtime retirement, completion, deployment, capture, application, progress and
execution tests. Caller tracing covered capture currency, bridge selection and
generation fences, canonical raw-L1 completed verification, state-queue source
and runtime, serialized coordinator delivery and post-finality recovery, history
recovery, and reconciliation-only actuation checkpoints.

Instructions read: root and demo AGENTS.md; reviewing-consensus-changes and the
entire twelve-lens reference; DA, fraud-proof and state-queue invariant references;
production-l2.md and verification.md; local-test-environment and running-suites;
writing-reports-and-prs. Earlier reports supplied finding traces and required
closure verdicts, not implementation rationale. Broader dirty funding, node,
node-tools, public-DA, deployment-profile and availability work was excluded,
apart from the archive runtime wiring needed to trace this change. No
implementation file was edited, build launched, commit made, or external message
sent by this pass.

Relevant gates: watcher format/lint/typecheck/build and the watcher suite selected
by preflight, plus the focused archive, application and supervisor/progress tests
below. The parent was already running broad preflight; this pass did not repeat
its builds or use its database prefix. No canonical codec, validator parameter,
golden generator or Aiken execution-ledger change is part of this archive scope.

## Prior-finding verdicts

| Finding | Verdict | Final source and regression evidence |
| --- | --- | --- |
| F1: ordinary successful completion leaves its pin pending | CLOSED | `fault-proof-supervisor.create-supervisor.ts:367` routes a fresh completed runner through `admitWatcherProofRunnerCompletion`. Its verifier finishes the archive operation through `replay-transcript-completion.ts:78-90` before the applicable callback caches completion and removes the objective. Helper tests cover delayed applicability, pending and refusal; objective-progress covers ordinary completion and coalesced generations. |
| F2: never-selected descendant classifications remain pinned forever | CLOSED | Application classification calls the admitted current-capture classification door; obtaining the challenge separately marks `proof_started`. `replay-transcript-lifecycle.ts:338-347` permits a fully classified never-used pin to age out, while a proof-used pin remains pending until canonical completion. The classification-retirement suite covers never-used retirement, forged/head-substituted admission and started-proof retention. Interrupted or unknown work remains conservatively retained. |
| F3: quiet canonical progress never supplies a retirement clock | CLOSED | `replay-transcript-retirement.ts:63-94` acquires an exact local observation and authenticates the unchanged queue through the production source door when the source point is sufficiently behind. Exact hash, slot and block-number equality is still required at `:97-102`. Runtime retirement tests cover this refresh reaching real SQLite reclamation and recovery/unfinished-work refusal. |
| F4: completed-journal restart or pending verification loses its reconciliation path | CLOSED | `fault-proof-progress-authority.ts:207-213` restores completed journals; `:381-413` no longer skips them on later admitted observations. The reconciliation controller accepts the exact validated signed completed journal, without granting preflight/submission. Objective-progress now tests a removed target after restart with both immediate and first-pending canonical verification; both reach verification without runner invocation or funding queries. |
| F5: relative finality checkpoints never intersect an absolute refresh grid | CLOSED | Refresh uses elapsed blocks from the admitted source observation, not absolute modulus (`replay-transcript-retirement.ts:63-68`). Ordinary deliveries beyond durable finality defer without clearing continuous absence (`:53-59`). Runtime tests cover an off-grid checkpoint and an intermediate ordinary quiet delivery between two durable checkpoints. |
| F6: an old inclusion can be deleted while its recent removal is recoverable | CLOSED | Retirement now also requires the exact current pin set's absence witness to age by strictly more than `postFinalityRecoveryDepth` newer blocks (`replay-transcript-absence.ts:66-95`, lifecycle `:362-391`). The witness is reset on store opening, rollback, unsafe runtime readiness and live/incomplete identity observations. Classification tests pin refusal at depth 2,160, retirement at 2,161, restart/rollback/live resets and corrupt-witness refusal. |

These verdicts close the reported traces. They do not claim a live native-chain
replacement was executed after deletion or that the new never-selected flow was
driven by an end-to-end adversarial ancestor/descendant queue in this pass.

## Guard pairing and attack results

- Exact archive identity remains the deployment, header and full inclusion point.
  Lifecycle registration compares every inclusion field to the admitted header
  and binds the retention window and verified deployment to that manifest
  (`replay-transcript-lifecycle.ts:212-244`). Classification compares the admitted
  decision, header and current transcript head; terminal completion uses the
  exact recorded decision identity and freshly authenticated raw-L1 facts.
- Replacing permanent retention adds permission only after every pin is known
  and finished or fully classified never-used. Unknown legacy chains, partial
  classification and proof-used unfinished operations continue to block
  reclamation. A completed marker alone grants no completion authority.
- Every original time guard remains: signed end-time retention, inclusion-time
  retention, and completion-time retention. Old-inclusion block depth remains,
  and the new absence-age check adds recent-removal recovery protection.
- Atomic deletion still audits the full chain, then deletes transcript versions,
  head, lifecycle pins and absence in one immediate transaction. The trigger
  refusal test verifies that a later deletion error preserves the whole chain
  and both operation pins.
- Startup clears unsupported old absence witnesses; serialized rollback clears
  them before queue rewind. Runtime pending recovery, quarantine/incidents,
  unfinished/queued/active/blocked work prevents a sweep. A quiet delivery ahead
  of the next durable checkpoint only defers; it does not erase valid same-branch
  absence history. Fresh capture I/O has readiness and queue-currency fences.
- Historical completed-journal permits remain reconciliation-only: exact decision,
  journal identity, prepared evidence and signed attempt checks stay in the
  controller. `assertPermit` still revokes reconciliation at `before_preflight`
  and `before_submit`. Completed verification does not open a signer, allocate
  funding, submit a transaction or rewrite the completed journal.

One investigated candidate was dropped: restoring completed journals can fill
the 2,048-objective startup bound, but the unchanged `exactWorkflowDirectories`
already caps all workflow directories at 2,048 before this change. The candidate
does not establish a newly introduced archive regression. That existing
operational bound was not stress-tested here.

## Twelve lenses

1. **Parameter trust — clean.** Retention is resolved from the verified signed
   deployment authority. No validator parameter check or datum-as-parameter
   substitution was added.
2. **Always-succeeds scripts — clean.** No script loader, parameter arity,
   validator arm or yield handshake changed in this scope.
3. **Decoders and pinned compiler — clean within scope.** Canonical header
   encoding, archive identity, row bytes/record digests, lifecycle checksums and
   absence checksums fail closed; persisted replay bytes must independently
   replay before obtaining a challenge. No Aiken result was produced here.
4. **Value conservation — clean.** This batch changes evidence lifecycle and
   read-only reconciliation scheduling, not amounts, rewards, fees or mint/burn.
5. **Anchoring — clean.** Manifest, inclusion identity, authenticated header,
   current replay capture and raw canonical terminal remain distinct authority
   checks. Absence is tied to the current pin digest and ages by native block
   depth; old inclusion age alone no longer authorizes deletion.
6. **Reference scripts — clean.** Archive maintenance and completed verification
   introduce no fault-proof step or inline witness. Existing application roster
   preflight remains exercised by the focused application test.
7. **Both polarities — clean for the reported fixes.** Focused tests cover safe
   reclamation and refusal for recent absence, live targets, pending operations,
   locks, recovery, corrupt pins/witnesses and transactional failure. Completed
   reconciliation covers both immediate applicability and first-pending retry.
8. **Gates that cannot fail — no confirmed defect.** Fresh completion must pass
   the verifier before its callback; unfinished restored objectives remain
   indexed. Negative archive tests target the changed doors and transaction
   refusal. This pass did not independently mutate fixes for red-checks.
9. **Execution and size budgets — no on-chain increase.** Declared archive
   rows/bytes/chains/pins remain bounded and capacity fails closed. Maximum-size
   sweep timing and storage-ceiling recovery throughput were not measured.
10. **TypeScript/Aiken twins — clean.** These local lifecycle tables have no
    Aiken twin; no canonical consensus codec or proof computation was replaced.
11. **Ledger facts — clean for the reported composition.** The clock comes from
    the exact authenticated finalized native point, not wall time or an absolute
    block grid. Recovery protection uses newer native blocks, not elapsed slots.
12. **Replacement paths retain guards — clean for reviewed exits.** The guard
    pairings above cover fresh completion, completed crash recovery, pending
    verification, never-used removal, startup, rollback and quiet reclamation;
    no replacement path grants new submission authority.

## Independent checks

All runs below were from `/home/gumbo/midgard-hub/midgard`, on 2026-10-01,
concurrent with the parent's broad preflight. JSON outputs were read for test
counts; no parent report is counted as independent test evidence.

1. `env PATH=/tmp/midgard-toolchain-20261001:$PATH MIDGARD_TEST_DATABASE_PREFIX=midgard_archive_review3 node scripts/doctor.mjs --json`
   — exit 1 under the restricted sandbox. Node 22.22.2 matched, dependencies and
   executable hooks were present, and the blueprint stamp matched current inputs
   and the pinned compiler. Native compiler/Postgres and child Node/pnpm probes
   returned EPERM. This is not a passed doctor or an inferred tool failure.
2. Native escalation:
   `env PATH=/tmp/midgard-toolchain-20261001:$PATH MALLOC_MMAP_THRESHOLD_=131072 MIDGARD_REAL_BLUEPRINT_PATH=/tmp/midgard-decisions-blueprint-20261001/onchain/aiken/plutus.json MIDGARD_WATCHER_FORKS=2 MIDGARD_TEST_DATABASE_PREFIX=midgard_archive_review3 pnpm --dir demo/midgard-watcher exec vitest run tests/storage/replay-transcript-store.test.ts tests/storage/replay-transcript-classification-retirement.test.ts tests/runtime/replay-transcript-retirement.test.ts tests/fault-proofs/fault-proof-supervisor-runner-completion.test.ts tests/runtime/deployment-identity.test.ts tests/fault-proofs/fault-proof-progress-authority.test.ts tests/fault-proofs/fault-proof-supervisor-completed-resume.test.ts tests/fault-proofs/fault-proof-objective-progress.test.ts tests/fault-proofs/fault-proof-execution.test.ts --reporter=default --reporter=json --outputFile=/tmp/archive-pass3-focused.json`
   — exit 0; 9 files, 95 tests collected/passed, 0 skipped; 87.58 seconds.
3. Native escalation:
   `env PATH=/tmp/midgard-toolchain-20261001:$PATH MALLOC_MMAP_THRESHOLD_=131072 MIDGARD_REAL_BLUEPRINT_PATH=/tmp/midgard-decisions-blueprint-20261001/onchain/aiken/plutus.json MIDGARD_WATCHER_FORKS=2 MIDGARD_TEST_DATABASE_PREFIX=midgard_archive_review3_capture pnpm --dir demo/midgard-watcher exec vitest run tests/fault-proofs/replay-transcript-capture.test.ts tests/fault-proofs/fault-proof-application.test.ts tests/fault-proofs/fault-proof-application-decision-time.test.ts --reporter=default --reporter=json --outputFile=/tmp/archive-pass3-capture.json`
   — exit 0; 3 files, 30 tests collected/passed, 0 skipped; 62.23 seconds. The
   two capture cases exercise cold replay admission for normal and forced Plutus
   decisions, opaque-handle refusal and history-generation retirement.

## Residuals and limits

No new residual defect was established; no fault-proof-family gap entry is
proposed. Uncertain/legacy evidence stays pinned, storage ceilings still require
safe operational intervention, and repeated restarts or active recovery can
delay reclamation by restarting absence age. Those are conservative retention
properties, not permission to delete unresolved evidence. Existing state-queue
SQ5/SQ6 owner questions and known codec gaps are unchanged by this archive batch.

Not independently run: full preflight or watcher suite; package format, lint,
typecheck or build; live devnet/acceptance; on-chain emulator or Aiken gates;
maximum-capacity archive timing; a live supported post-finality replacement after
deletion; a complete ancestor/descendant selection-and-pruning scenario; or a
fix mutation/red-check. The parent was already running broad verification and
requested no concurrent builds or implementation mutations. Focused tests use
controlled reader seams for runtime composition and cannot substitute for live
native recovery acceptance. This review authored only this report.
