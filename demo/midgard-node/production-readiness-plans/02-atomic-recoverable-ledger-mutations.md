# P0 Blocker 2: Atomic And Recoverable Ledger Mutations

Status: partial implementation landed. The old "implementation plan only"
status is stale.

Current code has a minimal durable local mutation job table, startup and
readiness fail-closed behavior for unfinished jobs, richer pending block
finalization journals, and job wrappers around block and merge local
finalization. It does not yet have the full production recovery system described
by this blocker: generalized multi-phase replay, MPF root manifests,
quarantine records, operator recovery commands, or durable pending merge
confirmation recovery.

Owner scope: `demo/midgard-node` local ledger mutation flow across mempool
acceptance, block commitment/finalization, merge finalization, PostgreSQL
tables, and LevelDB-backed MPF state.

Production standard: the node must never silently continue from a mixed local
ledger state. If a mutation cannot be proven complete, replayable, or
quarantined, startup and readiness must fail closed.

## Problem Statement

`midgard-node` still mutates local state across independently durable stores:

- PostgreSQL tables such as `tx_admissions`, `mempool`, `mempool_ledger`,
  `mempool_tx_deltas`, `processed_mempool`, `immutable`, `blocks`,
  `confirmed_ledger`, `deposits_utxos`, `withdrawal_utxos`,
  `pending_block_finalizations`, and `local_mutation_jobs`.
- LevelDB-backed MPF stores used to derive L2 UTxO and transaction roots.
- In-memory refs that gate worker behavior and readiness.

Several flows now use SQL transactions or local mutation job rows, but the node
still lacks a single durable recovery model that can prove SQL and MPF state are
both at an expected pre-state or post-state after a crash. For a production L2,
that generalized recovery model remains required because local state determines
which L2 transactions are spendable, which roots are committed to L1, which
blocks are eligible for merge, and which deposits or withdrawals have been
projected.

Every local mutation must eventually prove one of these outcomes:

- the mutation did not happen and can be retried from its pre-state;
- the mutation happened and can be marked complete after verification;
- the mutation cannot be proven safe and the node must quarantine and stop
  admitting/finalizing traffic.

## Landed Implementation

### Minimal Local Mutation Jobs

Migration 0003 creates `local_mutation_jobs` with `job_id`, `kind`, `status`,
optional `plan_hash`, JSON payload, attempts, error metadata, and timestamps
([`src/database/migrations/sql/0003_local_mutation_jobs.sql` lines 1-21](../src/database/migrations/sql/0003_local_mutation_jobs.sql#L1-L21)).
It is included in the migration manifest as version 3 and listed in application
table/index manifests
([`src/database/migrations/index.ts` lines 35-41](../src/database/migrations/index.ts#L35-L41),
[`src/database/migrations/index.ts` lines 75-96](../src/database/migrations/index.ts#L75-L96),
[`src/database/migrations/index.ts` line 125](../src/database/migrations/index.ts#L125)).

`MutationJobsDB` exposes the current repository API:

- closed kinds: `local_block_finalization` and
  `confirmed_merge_finalization`
  ([`src/database/mutationJobs.ts` lines 11-14](../src/database/mutationJobs.ts#L11-L14));
- statuses: `running`, `completed`, and `failed`
  ([`src/database/mutationJobs.ts` lines 18-22](../src/database/mutationJobs.ts#L18-L22));
- `start`, `markCompleted`, `markFailed`, `retrieveUnfinished`, and
  `countUnfinished`
  ([`src/database/mutationJobs.ts` lines 52-151](../src/database/mutationJobs.ts#L52-L151)).

This is a fail-closed job marker, not the full multi-stage journal originally
planned in this document. It records enough to block startup and readiness when
a known finalization job is unfinished, but it does not store canonical plan
CBOR, SQL postcondition markers, MPF roots, or quarantine evidence.

### Startup And Readiness Guards

Startup now refuses to serve when any local mutation job is not completed
([`src/commands/listen.ts` lines 72-91](../src/commands/listen.ts#L72-L91)).
Readiness counts unfinished jobs and returns reason
`unfinished_local_mutation_jobs:<count>`
([`src/commands/listen-router.ts` lines 454-457](../src/commands/listen-router.ts#L454-L457),
[`src/commands/listen-router.ts` lines 521-532](../src/commands/listen-router.ts#L521-L532)).

There is no automatic replay runner yet. Current production behavior is
fail-closed until an explicit recovery path exists.

### Mempool Acceptance

The tx queue processor now persists accepted and rejected admissions through
`TxAdmissionsDB` instead of writing `MempoolDB` and `TxRejectionsDB` directly
from the fiber
([`src/fibers/tx-queue-processor.ts` lines 486-510](../src/fibers/tx-queue-processor.ts#L486-L510)).
Accepted admissions wrap `MempoolDB.insertMultiple` and admission status updates
in one SQL transaction
([`src/database/txAdmissions.ts` lines 341-369](../src/database/txAdmissions.ts#L341-L369)).
Rejected admissions insert byte-matching rejection metadata and update admission
status in one SQL transaction
([`src/database/txAdmissions.ts` lines 387-456](../src/database/txAdmissions.ts#L387-L456)).

`MempoolDB.insertMultiple` itself now wraps tx rows, produced UTxOs, deltas,
spent input deletes, consumed deposit markers, and address history in
`sql.withTransaction`
([`src/database/mempool.ts` lines 61-103](../src/database/mempool.ts#L61-L103)).

Remaining gap: mempool acceptance is still not represented as a
`local_mutation_jobs` kind or recoverable local ledger mutation job. The local
CLI transfer path still bypasses durable admissions and writes rejections or
accepted mempool rows directly
([`src/commands/submit-l2-transfer.ts` lines 949-976](../src/commands/submit-l2-transfer.ts#L949-L976)).

### Deposit Projection

Startup no longer suppresses deposit catch-up or projection failures. The node
maps those failures to `DatabaseInitializationError` before serving traffic
([`src/commands/listen.ts` lines 92-119](../src/commands/listen.ts#L92-L119)).

Standalone deposit projection has stricter SQL behavior than the old plan
described. Reconciliation rejects mismatched existing projected rows, and
awaiting-deposit projection runs the mempool-ledger reconciliation and deposit
status update in one SQL transaction
([`src/fibers/project-deposits-to-mempool-ledger.ts` lines 28-77](../src/fibers/project-deposits-to-mempool-ledger.ts#L28-L77),
[`src/fibers/project-deposits-to-mempool-ledger.ts` lines 79-105](../src/fibers/project-deposits-to-mempool-ledger.ts#L79-L105)).

Remaining gap: deposit projection still has no `local_mutation_jobs` boundary,
plan hash, replay state, or MPF manifest link.

### Pending Block Finalization Journals

Pending block finalization journals are now richer than the original plan
assumed.

The active status set is:

- `pending_submission`
- `submitted_local_finalization_pending`
- `submitted_unconfirmed`
- `observed_waiting_stability`

Terminal statuses are `finalized` and `abandoned`
([`src/database/pendingBlockFinalizations.ts` lines 54-70](../src/database/pendingBlockFinalizations.ts#L54-L70)).

Migration 0005 refuses to migrate active old-format pending journals, then adds
state-queue base metadata, base roots, expected roots, block start time, and
durable payload fields for deposit, withdrawal, and tx members
([`src/database/migrations/sql/0005_pending_finalization_journal_payloads.sql` lines 1-90](../src/database/migrations/sql/0005_pending_finalization_journal_payloads.sql#L1-L90)).
`PendingBlockFinalizationsDB` stores payload CBOR, SHA-256, source table, source
id, source timestamp, and ordinal for each member
([`src/database/pendingBlockFinalizations.ts` lines 43-52](../src/database/pendingBlockFinalizations.ts#L43-L52),
[`src/database/pendingBlockFinalizations.ts` lines 330-386](../src/database/pendingBlockFinalizations.ts#L330-L386)).
It enforces one active journal through a partial unique index
([`src/database/pendingBlockFinalizations.ts` lines 387-398](../src/database/pendingBlockFinalizations.ts#L387-L398)).

Commit submission writes the pending journal before L1 submit:

- deposit/withdrawal-only path
  ([`src/workers/commit-block-header.ts` lines 686-729](../src/workers/commit-block-header.ts#L686-L729));
- tx-backed path
  ([`src/workers/commit-block-header.ts` lines 819-884](../src/workers/commit-block-header.ts#L819-L884)).

Startup asserts active journal payloads are complete, hydrates submitted tx
state and local-finalization flags from the active journal, and can revive
abandoned canonical payload-bearing journals for local finalization recovery
([`src/commands/listen-startup.ts` lines 71-122](../src/commands/listen-startup.ts#L71-L122),
[`src/commands/listen-startup.ts` lines 260-323](../src/commands/listen-startup.ts#L260-L323),
[`src/commands/listen-startup.ts` lines 332-380](../src/commands/listen-startup.ts#L332-L380)).

### Local Block Finalization

`successfulSubmissionProgram` wraps local block finalization in
`MutationJobsDB.Kind.LocalBlockFinalization`
([`src/workers/utils/commit-submission.ts` lines 45-84](../src/workers/utils/commit-submission.ts#L45-L84),
[`src/workers/utils/commit-submission.ts` lines 269-321](../src/workers/utils/commit-submission.ts#L269-L321)).
The local recovery path uses the same job wrapper after loading replay payload
from the pending journal
([`src/workers/utils/commit-submission.ts` lines 323-433](../src/workers/utils/commit-submission.ts#L323-L433)).

The SQL portion of `finalizeCommittedBlockLocally` now runs immutable inserts,
block linkage, mempool clears, processed-mempool keyed clears, and withdrawal
ledger effects inside `sql.withTransaction`
([`src/workers/utils/commit-submission.ts` lines 216-257](../src/workers/utils/commit-submission.ts#L216-L257)).
After that SQL transaction, the transactions MPF root is reset to empty
([`src/workers/utils/commit-submission.ts` line 258](../src/workers/utils/commit-submission.ts#L258)).

Remaining gaps:

- the MPF reset is still outside the SQL transaction and has no root manifest;
- a failed local block finalization leaves a failed/running job that blocks
  startup/readiness, but no automatic replay runner exists;
- the broader `block_root_preparation` job and staged-root promotion model have
  not landed.

### MPF Root Preparation

The old file name `src/workers/utils/mpt.ts` is stale. The current MPF code is
in `src/workers/utils/mpf.ts`.

`processMpfs` still mutates active transaction and ledger MPFs while deriving
candidate roots. It snapshots transaction and ledger roots before applying
batches and resets both on apply failure
([`src/workers/utils/mpf.ts` lines 1140-1163](../src/workers/utils/mpf.ts#L1140-L1163)).
The commit worker additionally wraps the whole database operation in
`withMpfRootTransaction(ledgerMpf, ...)`, which resets only the ledger MPF root
when the wrapped effect fails
([`src/workers/utils/mpf.ts` lines 1303-1317](../src/workers/utils/mpf.ts#L1303-L1317),
[`src/workers/commit-block-header.ts` lines 1442-1445](../src/workers/commit-block-header.ts#L1442-L1445)).

Remaining gap: there is no SQL `local_mpf_roots` manifest, staged root role, or
active root pointer. MPF roots are still persisted by root markers in the MPF
store, not by a recoverable SQL control plane.

### Merge Local Finalization

The immediate merge confirmation bug is fixed and is detailed in
[03-merge-confirmation-failure.md](./03-merge-confirmation-failure.md). In this
blocker, the relevant local mutation fact is that confirmed merge local
finalization now uses `MutationJobsDB.Kind.ConfirmedMergeFinalization` and wraps
confirmed-ledger deletes/inserts plus `BlocksDB.clearBlock` in one SQL
transaction
([`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1440-1495](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1440-L1495)).

Remaining gap: there is no durable pending merge confirmation journal or merge
recovery loop. If confirmation is unknown, local finalization is blocked, but
the node does not yet persist enough merge-specific evidence to recover after
restart.

### Genesis And Bootstrap

Genesis startup is no longer defaulted on for test networks. It runs only when
`RUN_GENESIS_ON_STARTUP` is true and the network is not mainnet
([`src/commands/startup-policy.ts` lines 1-12](../src/commands/startup-policy.ts#L1-L12),
[`tests/startup-policy.test.ts` lines 4-16](../tests/startup-policy.test.ts#L4-L16)).

Remaining production gap: when enabled, startup still forks `Genesis.program` as
a background daemon and suppresses its failure
([`src/commands/listen.ts` lines 135-153](../src/commands/listen.ts#L135-L153)).
`makeMpfs` still auto-seeds the ledger MPF from configured genesis UTxOs when
the persisted ledger root is empty
([`src/workers/utils/mpf.ts` lines 155-191](../src/workers/utils/mpf.ts#L155-L191)).
That behavior must remain disabled for production and should be replaced by an
explicit bootstrap job if production genesis seeding is ever required.

## Target Invariants

These invariants remain the production target. Some are partially enforced by
the landed minimal jobs and pending block journals, but none should be treated
as fully complete until the remaining gaps are closed.

1. Every local ledger mutation has one durable `job_id` before any externally
   visible local state changes.
2. Each job records its kind, inputs, deterministic plan hash, expected
   pre-state fingerprints, expected post-state fingerprints, status, attempt
   counters, and operator-visible error metadata.
3. SQL domain mutations for one job are committed in one PostgreSQL transaction
   whenever they are logically part of the same state transition.
4. MPF mutations are deterministic, idempotent, root-verified, and linked to the
   SQL journal by `job_id`.
5. A job is complete only after SQL postconditions, MPF postconditions, and any
   required L1 evidence all verify.
6. Replaying the same job after a crash either reaches the same complete state
   or quarantines the node. It must not produce a different root or silently
   skip a conflicting row.
7. SQL rows and MPF roots must never be advanced based on stale in-memory refs
   alone. Refs are caches of durable state, not authority.
8. Startup must process or fail closed on unfinished jobs before workers admit
   new txs, build new blocks, finalize local blocks, or merge blocks.
9. Readiness must be false while any job is unfinished, retrying recovery, or
   quarantined.
10. Any destructive recovery operation, MPF rebuild, or quarantine override must
    require an explicit operator command and durable audit record.
11. No `demo/midgard-node` backward-compatibility shim, legacy dual path, or
    silent migration should be introduced.

## Required Production Model Still Open

The following model is still the right target for this blocker.

### General Job Kinds

The current `local_mutation_jobs` table only covers:

- `local_block_finalization`
- `confirmed_merge_finalization`

Production recovery still needs durable boundaries for:

- `mempool_acceptance_batch`
- `deposit_projection_batch`
- `genesis_bootstrap`
- `block_root_preparation`
- `block_local_finalization`
- `merge_local_finalization`
- `mpt_rebuild` or `mpf_rebuild`

The existing table can either evolve into the full journal or be replaced by an
explicitly migrated `local_ledger_mutation_jobs` table. Do not add compatibility
shims for old pre-launch job shapes.

### General Job State Machine

The current states are only `running`, `completed`, and `failed`. The target
recovery state machine still needs durable, monotonic phases such as:

```text
planned
sql_applied
mpf_applying
mpf_applied
verified
complete
quarantined
abandoned
```

`running`/`failed` is enough to fail closed, but not enough to replay or prove
which cross-store phase committed.

### SQL Mutation Pattern

Every SQL phase should use this shape:

```text
retry whole transaction on serialization failure before any MPF phase:
  BEGIN SERIALIZABLE;
  SELECT job row FOR UPDATE;
  VERIFY job.status is planned or recoverable;
  VERIFY SQL preconditions still match;
  APPLY domain writes with strict conflict checks;
  INSERT job SQL postcondition marker;
  UPDATE job SET status = 'sql_applied', sql_applied_at = now();
  COMMIT;
if commit result is ambiguous:
  reconnect and inspect job row plus SQL marker before deciding whether to replay
```

Required conflict behavior:

- `ON CONFLICT DO NOTHING` is acceptable only when the existing row is proven
  byte-identical to the planned row.
- If an outref, tx id, header link, or event id already exists with different
  bytes or ownership, the job must fail and quarantine.
- Deletes must record expected deleted row counts or deleted row hashes when
  absence would change replay semantics.
- Batch member order must be stored explicitly where ledger ordering matters.
- PostgreSQL advisory locks are required around job execution and recovery.

### MPF Mutation Pattern

Every MPF phase should use this shape:

```text
SELECT job row FOR UPDATE;
VERIFY job.status is sql_applied or mpf_applying;
VERIFY current root is either expected_pre_root or expected_post_root;
IF current root is expected_post_root:
  mark mpf_applied;
ELSE:
  UPDATE job SET status = 'mpf_applying';
  APPLY deterministic MPF batch;
  READ root;
  VERIFY root equals expected_post_root;
  RECORD post-root and mark mpf_applied;
```

Important constraints:

- MPF plans must be idempotent: `put(k, same_v)` and `del(k)` can be replayed
  safely, but conflicting `put(k, different_v)` must quarantine.
- Block root preparation should use staged roots or copy-on-write namespaces.
  Active roots must not advance until the finalization job has required L1
  evidence.
- If current root is neither the pre-root nor the post-root in the job, startup
  must quarantine. Do not reseed or guess.

### Root Manifest

Still required: SQL root manifest tables that record MPF roots per trie and
generation with roles such as `staged`, `active`, `retired`, and `abandoned`.
The active pointer must be updated only after post-root verification. Repeated
root hashes across generations must be allowed, but active pointers must never
reference staged or retired rows.

### Recovery And Quarantine

Startup recovery must eventually run under a node-wide PostgreSQL advisory lock.
For each unfinished job, it must verify plan hashes, SQL postconditions, MPF
roots, and required L1 evidence, then roll forward or quarantine. It must never
delete, reseed, truncate, or rebuild automatically.

Current code stops at the safer interim behavior: fail startup/readiness when an
unfinished `local_mutation_jobs` row exists.

## Observability And Tests

Landed or partially landed coverage:

- `deposit-flow-emulator.test.ts` checks an active pending-finalization journal
  after block submission, transition to `observed_waiting_stability`, recovery
  through `SuccessfulLocalFinalizationRecoveryOutput`, and journal finalization
  ([`tests/deposit-flow-emulator.test.ts` lines 1515-1623](../tests/deposit-flow-emulator.test.ts#L1515-L1623)).
- The same emulator file covers the happy-path merge into confirmed state and
  settlement creation
  ([`tests/deposit-flow-emulator.test.ts` lines 1660-1845](../tests/deposit-flow-emulator.test.ts#L1660-L1845)).
- `readiness.test.ts` covers the base `local_finalization_pending` readiness
  reason
  ([`tests/readiness.test.ts` lines 80-100](../tests/readiness.test.ts#L80-L100)).
- `startup-policy.test.ts` covers explicit genesis startup gating
  ([`tests/startup-policy.test.ts` lines 4-16](../tests/startup-policy.test.ts#L4-L16)).

Missing coverage that should be added:

- direct `MutationJobsDB` transition tests, including idempotent `start`,
  `markCompleted`, `markFailed`, and unfinished counts;
- startup refusal with unfinished local mutation jobs;
- readiness HTTP response with `unfinished_local_mutation_jobs:<count>`;
- local block finalization crash/failure injection before and after SQL commit,
  before and after MPF reset, and before job completion;
- merge local finalization crash/failure injection inside the SQL transaction
  and before `markCompleted`;
- MPF root mismatch and missing root marker fail-closed tests;
- operator recovery/quarantine tests once those paths exist.

## Concrete Checklist

Landed:

- [x] Add migration 0003 for `local_mutation_jobs`.
- [x] Add `MutationJobsDB` with current kind/status constants and start,
      complete, fail, retrieve unfinished, and count unfinished helpers.
- [x] Wire `local_mutation_jobs` into the migration and application table
      manifests.
- [x] Refuse startup when unfinished local mutation jobs exist.
- [x] Include unfinished local mutation jobs in readiness output and readiness
      failure reasons.
- [x] Wrap local block finalization and local block finalization recovery in
      `MutationJobsDB`.
- [x] Store richer pending block finalization journal metadata and member
      payloads.
- [x] Fail closed when an active pending block journal has incomplete durable
      payload members.
- [x] Wrap confirmed merge local finalization in `MutationJobsDB`.
- [x] Run confirmed merge local SQL effects in one SQL transaction.
- [x] Make startup deposit catch-up/projection failures fatal before serving.
- [x] Gate genesis startup behind explicit `RUN_GENESIS_ON_STARTUP` and never
      run it on mainnet.

Still required:

- [ ] Add or evolve to a full multi-phase local ledger mutation journal with
      canonical plan bytes, plan hashes, SQL markers, MPF roots, attempt counts,
      and quarantine evidence.
- [ ] Add durable append-only mutation events.
- [ ] Add strict byte-identical conflict helpers for tx rows, ledger rows, block
      rows, and event-origin rows.
- [ ] Add SQL fingerprint helpers for mempool, mempool ledger, processed
      mempool, confirmed ledger, blocks, and pending finalization rows.
- [ ] Add MPF root manifest read/write helpers with staged, active, retired, and
      abandoned roles.
- [ ] Add active-root pointer enforcement so staged, retired, or abandoned roots
      cannot become active except through verified promotion.
- [ ] Add MPF executor that checks pre-root/post-root and quarantines unknown
      roots.
- [ ] Add recovery runner guarded by a PostgreSQL advisory lock.
- [ ] Replay or quarantine unfinished jobs before worker startup. Until then,
      preserve fail-closed startup behavior.
- [ ] Represent mempool acceptance as a durable mutation job, or otherwise prove
      admission SQL transactions are sufficient for production recovery.
- [ ] Route local CLI transfer persistence through the same durable admission or
      mutation boundary as HTTP submission.
- [ ] Convert standalone deposit projection into a durable mutation job or
      remove it from normal runtime.
- [ ] Replace optional background genesis seeding with an explicit bootstrap job
      or operator command.
- [ ] Remove automatic ledger MPF genesis reseeding from normal production
      startup.
- [ ] Replace direct active-MPF mutation in `processMpfs` with staged root
      preparation and manifest-backed promotion.
- [ ] Include deferred mempool-to-processed-mempool transfer in a recoverable
      block-root-preparation job.
- [ ] Link local block finalization to verified staged roots and promote only
      roots for the confirmed header.
- [ ] Add durable pending merge confirmation recovery as tracked in
      [03-merge-confirmation-failure.md](./03-merge-confirmation-failure.md).
- [ ] Add metrics and durable audit events for every job stage.
- [ ] Add fault-injection tests for every crash point listed above.
- [ ] Add operator commands to inspect, replay, quarantine, and explicitly
      rebuild MPF state.
- [ ] Document production migration and rollback procedures.
