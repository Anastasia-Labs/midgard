# P0 Blocker 2: Atomic And Recoverable Ledger Mutations

Status: implementation plan only. No code changes are included in this
document.

Owner scope: `demo/midgard-node` local ledger mutation flow across mempool
acceptance, block commitment/finalization, merge finalization, PostgreSQL
tables, and LevelDB-backed MPT state.

Production standard: the node must never silently continue from a mixed local
ledger state. If a mutation cannot be proven complete, replayable, or
quarantined, startup and readiness must fail closed.

## Problem Statement

`midgard-node` currently mutates ledger state through multiple independently
durable stores:

- PostgreSQL tables such as `mempool`, `mempool_ledger`,
  `mempool_tx_deltas`, `processed_mempool`, `immutable`, `blocks`,
  `confirmed_ledger`, `deposits_utxos`, and `pending_block_finalizations`.
- LevelDB-backed MPT stores used to derive L2 UTxO and transaction roots.
- In-memory refs that gate worker behavior and readiness.

Several production-critical flows perform multi-store mutations without one
durable transaction boundary. PostgreSQL transactions alone cannot atomically
commit LevelDB/MPT mutations. LevelDB/MPT checkpointing alone cannot roll back
PostgreSQL state. A crash, process kill, disk error, provider failure, or
database restart between awaits can leave SQL and MPT state disagreeing.

For a production L2, this is a correctness blocker. Local state determines which
transactions are spendable, which roots are committed to L1, which blocks are
eligible for merge, and which deposits have been projected. The node needs a
durable mutation journal, deterministic replay plans, and startup recovery that
can prove one of these outcomes for every mutation:

- the mutation did not happen and can be retried from its pre-state;
- the mutation happened and can be marked complete after verification;
- the mutation cannot be proven safe and the node must quarantine and stop
  admitting/finalizing traffic.

## Current Behavior

The existing production readiness report identifies this as P0 blocker 2:
`PRODUCTION_READINESS_REPORT.md:144` through
`PRODUCTION_READINESS_REPORT.md:252`. The report explicitly notes that SQL and
LevelDB cannot be made atomic by a normal SQL transaction alone and recommends a
durable journal (`PRODUCTION_READINESS_REPORT.md:210` to
`PRODUCTION_READINESS_REPORT.md:248`).

### Mempool Acceptance

The tx queue processor drains an in-memory queue and keeps a process-local
pending buffer before validation (`src/fibers/tx-queue-processor.ts:387` to
`src/fibers/tx-queue-processor.ts:443`). Accepted and rejected results are then
persisted as separate effects:

- rejections are inserted into `tx_rejections` at
  `src/fibers/tx-queue-processor.ts:521` to
  `src/fibers/tx-queue-processor.ts:529`;
- accepted transactions are inserted through `MempoolDB.insertMultiple` at
  `src/fibers/tx-queue-processor.ts:539` to
  `src/fibers/tx-queue-processor.ts:543`;
- the cached in-memory UTxO state is updated only after SQL effects return at
  `src/fibers/tx-queue-processor.ts:552` to
  `src/fibers/tx-queue-processor.ts:554`.

`MempoolDB.insertMultiple` itself mutates several tables sequentially:

- inserts transaction rows into `mempool` at `src/database/mempool.ts:63` to
  `src/database/mempool.ts:68`;
- inserts produced UTxOs into `mempool_ledger` at
  `src/database/mempool.ts:80` to `src/database/mempool.ts:82`;
- stores per-transaction deltas in `mempool_tx_deltas` at
  `src/database/mempool.ts:82`;
- deletes spent UTxOs from `mempool_ledger` and marks consumed deposit event ids
  at `src/database/mempool.ts:83` to `src/database/mempool.ts:85`;
- writes address history at `src/database/mempool.ts:86` to
  `src/database/mempool.ts:87`.

There is no single SQL transaction around those table mutations. A failure can
leave transaction rows without ledger deltas, produced UTxOs without consumed
inputs, or deposit status out of sync with `mempool_ledger`.

The local CLI transfer path has the same issue. After local validation it writes
rejections through `TxRejectionsDB.insertMany` and accepted transactions through
`MempoolDB.insertMultiple` (`src/commands/submit-l2-transfer.ts:888` to
`src/commands/submit-l2-transfer.ts:915`). This bypasses the queue processor and
must not remain as an independent mutation path.

Standalone deposit projection is another local ledger mutation path:
`projectDepositsToMempoolLedger` reconciles already projected deposits, projects
new awaiting deposits, mutates `mempool_ledger` and `deposits_utxos`, then bumps
an in-memory version ref (`src/fibers/project-deposits-to-mempool-ledger.ts:27`
to `src/fibers/project-deposits-to-mempool-ledger.ts:119`). Startup currently
logs and suppresses projection failures (`src/commands/listen.ts:81` to
`src/commands/listen.ts:88`). That behavior can hide a partial local ledger
mutation and must be journaled or removed from normal startup.

Genesis startup seeding also mutates local ledger state directly. On non-mainnet
startup, `Genesis.program` is forked as a daemon (`src/commands/listen.ts:90` to
`src/commands/listen.ts:108`), inserts configured genesis UTxOs directly into
`mempool_ledger`, increments the in-memory mempool ledger version, and swallows
database errors as "already exists" (`src/genesis.ts:25` to
`src/genesis.ts:64`). Separately, `makeMpts` may seed the ledger MPT when the
root is empty. These two genesis paths are not atomic with each other and must
be replaced by an explicit fresh-node bootstrap job or disabled for production
startup.

### MPT Root Preparation

The commit worker opens the persistent ledger and mempool tries in
`makeMpts` (`src/workers/utils/mpt.ts:86` to
`src/workers/utils/mpt.ts:124`). If the ledger trie root is empty it currently
seeds genesis UTxOs (`src/workers/utils/mpt.ts:100` to
`src/workers/utils/mpt.ts:119`), which is unsafe unless SQL also proves this is
a fresh node.

`processMpts` prepares block roots and also mutates durable state:

- it can project deposits into SQL inside `resolveIncludedDepositEntriesForWindow`
  (`src/workers/utils/mpt.ts:177` to `src/workers/utils/mpt.ts:266`);
- it can drop malformed mempool txs and insert rejections concurrently at
  `src/workers/utils/mpt.ts:398` to `src/workers/utils/mpt.ts:408`;
- it mutates both mempool and ledger tries concurrently at
  `src/workers/utils/mpt.ts:411` to `src/workers/utils/mpt.ts:414`;
- it then reads roots at `src/workers/utils/mpt.ts:416` to
  `src/workers/utils/mpt.ts:417`.

The worker wraps `databaseOperationsProgram` with `withTrieTransaction`, but only
for `ledgerTrie` (`src/workers/commit-block-header.ts:1058` to
`src/workers/commit-block-header.ts:1060`). The mempool trie and SQL writes made
inside the same logical operation are not covered by the ledger trie checkpoint.
`withTrieTransaction` itself checkpoints, commits, or reverts one trie
(`src/workers/utils/mpt.ts:459` to `src/workers/utils/mpt.ts:475`).

When no prior committed block is available, the commit worker computes roots and
then transfers processed tx payloads from `mempool` into `processed_mempool`
through `skippedSubmissionProgram` (`src/workers/commit-block-header.ts:948` to
`src/workers/commit-block-header.ts:959`; `src/workers/utils/commit-submission.ts:232`
to `src/workers/utils/commit-submission.ts:271`). That transfer is a ledger
mutation because it changes which txs are available for later commitment and
must be part of the same recoverable block-preparation job.

### Block Submission And Local Finalization

Before commit submission, the worker records a pending block finalization:

- deposit-only path at `src/workers/commit-block-header.ts:536` to
  `src/workers/commit-block-header.ts:541`;
- tx-backed path at `src/workers/commit-block-header.ts:637` to
  `src/workers/commit-block-header.ts:644`.

On successful submission, the pending record is marked submitted at
`src/workers/commit-block-header.ts:717` to
`src/workers/commit-block-header.ts:724`.

Local block finalization currently fans out SQL and MPT effects concurrently in
`finalizeCommittedBlockLocally`:

- immutable tx insert, block link insert, and mempool SQL clear are run in
  nested `Effect.all` calls at `src/workers/utils/commit-submission.ts:120` to
  `src/workers/utils/commit-submission.ts:151`;
- `ProcessedMempoolDB.clear` and `mempoolTrie.delete()` are run concurrently
  with those SQL effects at `src/workers/utils/commit-submission.ts:148` to
  `src/workers/utils/commit-submission.ts:149`;
- `mempoolTrie.delete()` closes and removes the LevelDB directory at
  `src/workers/utils/mpt.ts:706` to `src/workers/utils/mpt.ts:717`.

After that, `successfulSubmissionProgram` marks deposits projected and marks
local finalization complete (`src/workers/utils/commit-submission.ts:183` to
`src/workers/utils/commit-submission.ts:189`). These are separate effects after
the local finalization fan-out.

The existing `pending_block_finalizations` table is a useful partial journal,
but it is scoped to submitted block lifecycle, not to every SQL/MPT mutation
step. It tracks active statuses (`src/database/pendingBlockFinalizations.ts:30`
to `src/database/pendingBlockFinalizations.ts:46`) and enforces a single active
pending block through a partial unique index
(`src/database/pendingBlockFinalizations.ts:158` to
`src/database/pendingBlockFinalizations.ts:169`).

### Pending Finalization Startup And Confirmation

Startup hydrates in-memory refs from the active pending block finalization
(`src/commands/listen-startup.ts:211` to
`src/commands/listen-startup.ts:243`). It does not inspect a general
multi-store mutation journal because none exists.

The confirmation worker observes pending blocks and can set
`LOCAL_FINALIZATION_PENDING` plus `AVAILABLE_LOCAL_FINALIZATION_BLOCK` when a
confirmed block needs local recovery (`src/fibers/block-confirmation.ts:266` to
`src/fibers/block-confirmation.ts:280`). The tx queue processor pauses while
`LOCAL_FINALIZATION_PENDING` is true (`src/fibers/tx-queue-processor.ts:367` to
`src/fibers/tx-queue-processor.ts:385`). Readiness only exposes this as the
single reason `local_finalization_pending` (`src/commands/readiness.ts:168` to
`src/commands/readiness.ts:170`).

### Merge Finalization

The merge flow submits a merge transaction and then mutates confirmed SQL state.
Confirmation failure is currently logged and swallowed at
`src/transactions/state-queue/merge-to-confirmed-state.ts:1402` to
`src/transactions/state-queue/merge-to-confirmed-state.ts:1407`, which is a
separate P0 issue. For this plan, the local mutation risk is that
`finalizeLocalMergeProgram` performs confirmed ledger deletions, confirmed
ledger insertions, and block clearing as separate effects:

- clear spent confirmed UTxOs at
  `src/transactions/state-queue/merge-to-confirmed-state.ts:1418` to
  `src/transactions/state-queue/merge-to-confirmed-state.ts:1423`;
- insert produced confirmed UTxOs at
  `src/transactions/state-queue/merge-to-confirmed-state.ts:1424` to
  `src/transactions/state-queue/merge-to-confirmed-state.ts:1431`;
- clear the merged block from `BlocksDB` at
  `src/transactions/state-queue/merge-to-confirmed-state.ts:1432` to
  `src/transactions/state-queue/merge-to-confirmed-state.ts:1435`.

`ConfirmedLedgerDB.clearUTxOs` and `ConfirmedLedgerDB.insertMultiple` delegate
to generic ledger helpers (`src/database/confirmedLedger.ts:300` to
`src/database/confirmedLedger.ts:309`). The insert helper uses
`ON CONFLICT DO NOTHING` (`src/database/utils/ledger.ts:425` to
`src/database/utils/ledger.ts:437`), so replay is not strict enough to detect an
existing outref with conflicting bytes unless additional checks are added.

## Target Invariants

These invariants define correctness for the implementation.

1. Every local ledger mutation has one durable `job_id` before any externally
   visible local state changes.
2. Each job records its kind, inputs, deterministic plan hash, expected pre-state
   fingerprints, expected post-state fingerprints, status, attempt counters, and
   operator-visible error metadata.
3. SQL domain mutations for one job are committed in one serializable PostgreSQL
   transaction whenever they are logically part of the same state transition.
4. MPT mutations are deterministic, idempotent, root-verified, and linked to the
   SQL journal by `job_id`.
5. A job is complete only after SQL postconditions, MPT postconditions, and any
   required L1 evidence all verify.
6. Replaying the same job after a crash either reaches the same complete state or
   quarantines the node. It must not produce a different root or silently skip a
   conflicting row.
7. SQL rows and MPT roots must never be advanced based on stale in-memory refs
   alone. Refs are caches of durable state, not authority.
8. Startup must process unfinished jobs before workers admit new txs, build new
   blocks, finalize local blocks, or merge blocks.
9. Readiness must be false while any job is unfinished, retrying recovery, or
   quarantined.
10. Any destructive recovery operation, MPT rebuild, or quarantine override must
    require an explicit operator command and durable audit record.
11. No demo/midgard-node backward-compatibility shim, legacy dual path, or silent
    migration should be introduced. If existing local state cannot satisfy the
    new invariants, fail closed and require explicit migration or rebuild.
12. Every code path that mutates local ledger state must use the journal
    boundary. This includes HTTP submission, the tx queue processor, local CLI
    transfer submission, standalone deposit projection, genesis/bootstrap
    seeding, block preparation, block local finalization, and merge local
    finalization.
13. Candidate roots produced before L1 submission or before L1 confirmation are
    staged evidence, not active local state. Active root promotion requires the
    corresponding durable SQL job, verified postconditions, and required L1
    evidence.

## Transaction And Journal Model

Introduce one general local mutation journal rather than several unrelated
ad-hoc recovery flags.

### Job Kinds

Use a closed set of job kinds:

- `mempool_acceptance_batch`: validation result persistence for a tx batch. This
  covers `tx_rejections`, `mempool`, `mempool_ledger`,
  `mempool_tx_deltas`, deposit consumed status, and address history.
- `deposit_projection_batch`: standalone projection/reconciliation of L1 deposit
  events into the local mempool ledger. This covers `deposits_utxos`,
  `mempool_ledger`, address history if added for deposits, and the durable
  mempool-ledger fingerprint. If standalone projection is removed from the
  runtime, this kind is needed only for explicit operator projection commands.
- `genesis_bootstrap`: explicit fresh-node local bootstrap for configured
  genesis UTxOs and initial MPT root manifests. This is allowed only when SQL
  ledger tables, MPT root manifests, pending jobs, and relevant L1 protocol
  state prove a fresh deployment. It must not be a forked background startup
  task and must not treat conflicts as success.
- `block_root_preparation`: deterministic SQL and MPT preparation before commit
  submission. This covers deposit projection/reconciliation, malformed tx
  rejection/drop, mempool transaction root preparation, candidate UTxO root
  preparation, and any mempool-to-processed-mempool transfer used while waiting
  for prior block confirmation.
- `block_local_finalization`: post-submission or post-confirmation local block
  finalization. This covers `immutable`, `blocks`, `mempool`,
  `processed_mempool`, mempool MPT clearing, deposit projection final status,
  and `pending_block_finalizations` status changes.
- `merge_local_finalization`: post-confirmed merge local finalization. This
  covers `confirmed_ledger`, `blocks`, any confirmed-state root/fingerprint
  metadata, and merge lifecycle status.
- `mpt_rebuild`: explicit offline rebuild from SQL. This must never be part of
  normal startup unless an unfinished job already proves the rebuild was
  requested and authorized.

### Job Statuses

Use a monotonic state machine:

```text
planned
sql_applied
mpt_applying
mpt_applied
verified
complete
quarantined
abandoned
```

Rules:

- `planned`: durable plan exists. No domain mutation has been proven.
- `sql_applied`: all SQL domain mutations and SQL postcondition markers were
  committed in one PostgreSQL transaction.
- `mpt_applying`: SQL is complete and this node has started the non-transactional
  MPT phase. Recovery must inspect roots before deciding what happened.
- `mpt_applied`: MPT roots match expected post-roots and are durably recorded in
  SQL.
- `verified`: all cross-store postconditions and required L1 evidence were
  verified.
- `complete`: terminal success.
- `quarantined`: terminal fail-closed state requiring explicit operator action.
- `abandoned`: terminal state only for jobs whose external precondition failed
  before local mutation became canonical, for example commit signing failed
  before submission and before block local finalization.

Do not use "best effort cleanup" as a status. If cleanup is needed, it is a new
planned job with its own postconditions.

### Plan Payload

Each job stores a canonical plan payload. The payload must be deterministic and
must be hashed before mutation. Use RFC 8949 deterministic CBOR or another
existing canonical encoder already used in the node. JSONB can be retained for
operator readability, but hashes must be over canonical bytes.

Plan payload fields:

- `job_id`
- `kind`
- `schema_version`
- `created_by`: worker or command name
- `correlation_id`: request id, validation batch id, header hash, merge tx id,
  or recovery command id
- `inputs`: tx ids, header hash, block end time, deposit event ids, spent
  outrefs, produced outrefs, submitted tx hash, expected L1 evidence
- `sql_plan_hash`
- `mpt_plan_hash`
- `preconditions`: table row fingerprints, expected MPT roots, pending
  finalization status, and L1 evidence requirements
- `postconditions`: expected table row fingerprints, expected MPT roots, pending
  finalization status, and L1 evidence requirements
- `idempotency_keys`: unique keys used to prevent duplicate job creation

### SQL Mutation Pattern

Every SQL phase should use this shape:

```text
retry whole transaction on serialization failure before any MPT phase:
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

Do not split one logical domain transition across independent table helper
calls unless the caller wraps them in the same SQL transaction. Existing helpers
can be refactored into transaction-aware helpers that accept a `SqlClient` or
execute under the caller's `sql.withTransaction`.

Required conflict behavior:

- `ON CONFLICT DO NOTHING` is acceptable only when the existing row is proven
  byte-identical to the planned row.
- If an outref, tx id, header link, or deposit event id already exists with
  different bytes or different ownership, the job must fail and quarantine.
- Deletes must record expected deleted row counts or deleted row hashes when
  absence would change replay semantics.
- Batch member order must be stored explicitly where ledger ordering matters.
- Serialization failures must restart the whole SQL phase from the locked job
  row. Do not retry only the failed statement after partial in-transaction
  effects.
- PostgreSQL advisory locks are required around job execution and recovery. The
  lock key must be stable and documented, with one global mutation lock for
  cross-store state plus optional per-job locks for operator inspection.

### MPT Mutation Pattern

Every MPT phase should use this shape:

```text
SELECT job row FOR UPDATE;
VERIFY job.status is sql_applied or mpt_applying;
VERIFY current root is either expected_pre_root or expected_post_root;
IF current root is expected_post_root:
  mark mpt_applied;
ELSE:
  UPDATE job SET status = 'mpt_applying';
  APPLY deterministic MPT batch;
  READ root;
  VERIFY root equals expected_post_root;
  RECORD post-root and mark mpt_applied;
```

Important constraints:

- MPT plans must be idempotent: `put(k, same_v)` and `del(k)` can be replayed
  safely, but conflicting `put(k, different_v)` must quarantine.
- Filesystem deletion of the mempool trie must be removed from normal
  finalization. Replace `mempoolTrie.delete()` with a deterministic batch
  clearing planned tx keys, or with a generation/snapshot model where the active
  root is switched only after SQL records the new generation.
- If current root is neither the pre-root nor the post-root in the job, startup
  must quarantine. Do not guess and do not reseed.
- `block_root_preparation` should not mutate the active production roots before
  the plan is durable or before the block is allowed to affect local authority.
  Prefer staging tries or copy-on-write roots. If the MPT library cannot stage
  both ledger and mempool roots safely, use separate LevelDB namespaces per job.
  Record candidate roots as `staged`; promote them to `active` only from the
  finalization job after required L1 evidence and SQL postconditions verify.
- Abandoned staged roots must be marked `retired` or `abandoned` in SQL before
  any filesystem cleanup. Cleanup itself is an explicit maintenance job, not a
  recovery side effect.

## SQL Vs MPT Consistency Model

SQL is the durable control plane and audit source. MPT is the deterministic
materialized state used for root derivation and proof generation. Neither is
allowed to silently override the other.

### Source Of Truth

- Job state, lifecycle decisions, operator recovery status, and readiness state
  come from SQL.
- Domain rows in SQL are authoritative for APIs that expose transaction status,
  ledger rows, address history, pending finalizations, and merge status.
- MPT roots are authoritative only when linked to a completed job in SQL and
  verified against that job's plan.

### Root Manifest

Add a SQL root manifest table that records durable MPT roots per trie and
generation. The table must support staged candidate roots as well as active
roots because block preparation can compute roots before L1 evidence makes them
authoritative:

```sql
CREATE TABLE local_mpt_roots (
  trie_name TEXT NOT NULL,
  generation BIGINT NOT NULL,
  root_hash TEXT NOT NULL,
  backing_store_id TEXT NOT NULL,
  root_role TEXT NOT NULL,
  last_job_id UUID NOT NULL REFERENCES local_ledger_mutation_jobs(job_id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  activated_at TIMESTAMPTZ,
  retired_at TIMESTAMPTZ,
  PRIMARY KEY (trie_name, generation),
  UNIQUE (trie_name, generation, root_hash),
  CHECK (root_role IN ('staged', 'active', 'retired', 'abandoned'))
);
```

Add a single-row active pointer table:

```sql
CREATE TABLE local_mpt_active_roots (
  trie_name TEXT PRIMARY KEY,
  generation BIGINT NOT NULL,
  root_hash TEXT NOT NULL,
  last_job_id UUID NOT NULL REFERENCES local_ledger_mutation_jobs(job_id),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  FOREIGN KEY (trie_name, generation, root_hash)
    REFERENCES local_mpt_roots(trie_name, generation, root_hash)
);
```

Do not enforce uniqueness on `(trie_name, root_hash)`: the empty mempool root or
another byte-identical root can legitimately recur across generations. Repeated
roots are safe only when the generation, backing store, job id, and role are
explicitly recorded.

Only `root_role = 'active'` rows may be referenced by
`local_mpt_active_roots`; enforce this with a migration-owned trigger or an
equivalent constrained update helper tested at the database boundary. Candidate
roots created by `block_root_preparation` stay `staged` until
`block_local_finalization` promotes them after L1 evidence.

Startup must verify that:

- the active root row exists for every required trie after bootstrap;
- the LevelDB-backed trie opens successfully;
- the trie root equals `local_mpt_active_roots.root_hash`;
- any non-empty SQL state with an empty/missing trie fails closed unless an
  explicit rebuild job exists.

### Fingerprints

For SQL tables whose full row set defines ledger state, store deterministic
fingerprints:

- `mempool_ledger` fingerprint: ordered by raw `outref`, hash
  `outref || output || address || source_event_id`.
- `confirmed_ledger` fingerprint: ordered by raw `outref`, hash
  `outref || output || address`.
- `mempool` and `processed_mempool` fingerprints: ordered by `tx_id`, hash
  `tx_id || tx`.
- `blocks` fingerprint per header: ordered by explicit ordinal once added, hash
  `header_hash || ordinal || tx_id`.
- `pending_block_finalizations` fingerprint: header, status, submitted tx hash,
  block end time, ordered member ids.

Fingerprints are not a substitute for row-level checks during mutation, but they
give recovery and startup a fast way to detect drift before workers run.

## Flow Integration Points

### Mempool Acceptance Batch

Replace the tx queue processor's direct calls to `TxRejectionsDB.insertMany` and
`MempoolDB.insertMultiple` with a `LocalLedgerMutations.acceptMempoolBatch`
boundary.

Implementation shape:

1. Build a deterministic validation result plan from `decodeRejected`,
   `phaseA.rejected`, `phaseB.rejected`, `phaseB.accepted`, and
   `phaseB.statePatch`.
2. Insert a `mempool_acceptance_batch` journal row keyed by validation batch id
   and accepted/rejected tx ids.
3. In one serializable SQL transaction:
   - insert rejections;
   - insert accepted tx rows;
   - insert produced UTxOs;
   - insert tx deltas;
   - delete spent UTxOs;
   - mark consumed deposit event ids;
   - insert address history;
   - record SQL postcondition marker;
   - mark job `sql_applied`.
4. Since this job has no MPT mutation in the acceptance path, mark `mpt_applied`
   with empty MPT plan hash and verify SQL postconditions.
5. Only after job `complete`, update the in-memory validation cache.

The current `MempoolDB.insertMultiple` body at `src/database/mempool.ts:56` to
`src/database/mempool.ts:91` should become a transaction-aware domain helper
called by the journal executor, not an independent atomicity boundary.

The direct local transfer path in `src/commands/submit-l2-transfer.ts` must call
the same `mempool_acceptance_batch` executor for both rejection persistence and
accepted transaction persistence. Do not keep a CLI-only shortcut around
`MempoolDB.insertMultiple` or `TxRejectionsDB.insertMany`.

### Deposit Projection Batch

Convert standalone deposit projection into a `deposit_projection_batch` job, or
remove standalone projection from normal runtime and project deposits only as
part of `block_root_preparation`.

If standalone projection remains, implementation shape:

1. Read due awaiting deposits and already projected deposits under a stable
   observation timestamp.
2. Build a deterministic plan containing:
   - deposit event ids and their expected current statuses;
   - exact `mempool_ledger` rows to insert or verify;
   - expected consumed/finalized absence for those event ids;
   - expected pre/post `mempool_ledger` and deposit-status fingerprints.
3. Insert the journal row before mutating `deposits_utxos` or
   `mempool_ledger`.
4. In one serializable SQL transaction:
   - lock planned deposit rows;
   - verify the planned statuses and ledger payload bytes;
   - insert missing `mempool_ledger` rows with strict byte-identical conflict
     checks;
   - update deposit statuses only for the planned event ids;
   - record row counts and SQL postcondition marker;
   - mark job `sql_applied`.
5. Mark `mpt_applied` with an empty MPT plan unless this projection also updates
   a materialized mempool-ledger MPT generation.
6. Update in-memory mempool-ledger version refs only after the job is
   `complete`.

Startup must not catch and suppress deposit projection failures. A projection
failure during startup either becomes a recoverable unfinished job or a
quarantine reason that keeps workers and HTTP mutation routes stopped.

### Genesis Bootstrap

Replace startup `Genesis.program` local state seeding and `makeMpts` automatic
genesis MPT seeding with an explicit `genesis_bootstrap` job.

Implementation shape:

1. Permit `genesis_bootstrap` only when startup proves this is a fresh local
   deployment: no non-terminal jobs, empty `mempool`, `processed_mempool`,
   `immutable`, `blocks`, `confirmed_ledger`, `mempool_ledger`, empty or absent
   MPT root manifests, and compatible empty L1 protocol state for the configured
   network.
2. Build a deterministic plan from configured genesis UTxOs, network id, contract
   deployment identity, expected empty roots, and expected post-root manifests.
3. Insert genesis SQL rows and root manifests under the general job executor.
4. Do not fork genesis as a daemon and do not swallow database conflicts as
   "already exists". Byte-identical replay may complete the job; any conflicting
   row quarantines startup.
5. Submit genesis deposits only after the local bootstrap job is complete and
   only through the normal durable submission path. Deposit submission failure
   must not roll back or obscure local bootstrap state.
6. Mainnet or production configurations with no configured genesis UTxOs should
   create no genesis job and should require existing root manifests or an
   explicit operator bootstrap command.

### Block Root Preparation

Move the mutation semantics currently embedded in `processMpts` behind a
`block_root_preparation` job.

Implementation shape:

1. Read candidate mempool rows and pending processed rows.
2. Resolve tx deltas and deposit window without mutating SQL or MPT.
3. Build a deterministic plan containing:
   - mempool tx ids and CBOR hashes;
   - malformed tx ids and rejection details;
   - deposit event ids to include;
   - processed-mempool transfer set and explicit member order when prior block
     confirmation is still pending;
   - mempool MPT operations;
   - ledger MPT operations;
   - expected pre-roots and expected post-roots;
   - block end-time and header roots.
4. Insert the job before any domain mutation.
5. Apply SQL side effects in one transaction:
   - project/reconcile due deposits;
   - mark deposits projected only if rows match the plan;
   - drop malformed mempool txs and insert their rejections;
   - transfer planned tx payloads from `mempool` to `processed_mempool` and
     delete only the planned `mempool` rows when deferring until confirmation;
   - record all planned members and postcondition hashes.
6. Apply MPT side effects after SQL commits:
   - mutate staged candidate roots only if current active roots match expected
     pre-roots;
   - verify both mempool and ledger post-roots;
   - record roots in `local_mpt_roots` with `root_role = 'staged'`.
7. Return roots to commit transaction building only after the job is verified.
   The active root pointer must not change in this job.

This removes the current mismatch where `processMpts` mutates SQL, mempool MPT,
and ledger MPT directly (`src/workers/utils/mpt.ts:398` to
`src/workers/utils/mpt.ts:417`) while only the ledger trie is inside
`withTrieTransaction` (`src/workers/commit-block-header.ts:1058` to
`src/workers/commit-block-header.ts:1060`).

### Block Local Finalization

Replace `finalizeCommittedBlockLocally` with a `block_local_finalization` job
executor.

Implementation shape:

1. Use `pending_block_finalizations` as the block lifecycle journal, but link it
   to a general `local_ledger_mutation_jobs` row by `job_id`.
2. Build the finalization plan from the pending block row, planned tx ids,
   included deposit event ids, submitted tx hash, header hash, immutable tx
   payloads, staged root ids from the matching `block_root_preparation` job, and
   current active MPT root manifest.
3. SQL transaction:
   - validate pending block status and submitted tx hash;
   - insert immutable rows with strict byte-identical conflict checks;
   - insert `blocks` rows with explicit `ordinal`;
   - clear planned tx ids from `mempool` and `mempool_tx_deltas`;
   - clear `processed_mempool` only for planned tx ids, not whole-table truncate;
   - mark deposits finalized/projected for this header;
   - update pending finalization to the next lifecycle state;
   - mark SQL postconditions.
4. MPT phase:
   - verify the staged candidate roots still match the header roots and the
     expected backing-store ids;
   - clear only planned mempool tx keys or promote an empty mempool generation;
   - verify mempool root equals expected post-root;
   - promote the verified ledger/mempool roots to `active`, update
     `local_mpt_active_roots`, and retire superseded active generations in the
     same SQL transaction that records MPT postconditions.
5. Verification:
   - every planned tx id exists in `immutable`;
   - every planned tx id has a `blocks` row for the header and ordinal;
   - no planned tx id remains in `mempool`, `processed_mempool`, or
     `mempool_tx_deltas`;
   - mempool MPT root matches the manifest;
   - active root pointers match the finalization job's expected post-roots;
   - pending finalization status matches the required next state.

Avoid `ProcessedMempoolDB.clear` and `mempoolTrie.delete()` as normal production
steps. Whole-table truncation and filesystem deletion are recovery-hostile.

### Merge Local Finalization

Replace `finalizeLocalMergeProgram` with a `merge_local_finalization` job
executor. This must be sequenced after the separate P0 fix that refuses local
finalization unless merge L1 confirmation is proven.

Implementation shape:

1. Create a merge lifecycle journal row before submit or immediately after
   submit, depending on the final merge confirmation design.
2. After L1 evidence is confirmed, build a deterministic local finalization plan:
   - header hash;
   - block tx ids and ordinals;
   - spent confirmed outrefs and expected old outputs;
   - produced confirmed outrefs and expected new outputs;
   - expected `blocks` rows to remove;
   - expected confirmed ledger fingerprint before and after.
3. SQL transaction:
   - verify L1 evidence marker;
   - lock the merge job and relevant block rows;
   - verify all spent rows exist and match expected bytes;
   - verify produced rows are absent or byte-identical;
   - delete spent rows;
   - insert produced rows with strict conflict checks;
   - clear the block rows;
   - mark SQL postconditions.
4. MPT phase:
   - if confirmed-state MPT materialization is added, apply planned confirmed
     root changes and record the root;
   - if no confirmed MPT exists, record an empty MPT plan hash and verify the
     confirmed ledger SQL fingerprint.
5. Mark complete only after `confirmed_ledger`, `blocks`, and root/fingerprint
   postconditions all verify.

## Recovery State Machine

Recovery must run under a node-wide advisory lock so two node instances cannot
replay the same local mutation at the same time.

For each unfinished job ordered by `created_at`:

```text
planned:
  verify plan hash and preconditions
  apply SQL transaction
  continue to sql_applied

sql_applied:
  verify SQL postconditions
  if mpt plan is empty, continue to mpt_applied
  inspect current MPT roots
  if current roots equal post-roots, mark mpt_applied
  if current roots equal pre-roots, apply MPT plan and verify post-roots
  otherwise quarantine

mpt_applying:
  verify SQL postconditions
  inspect current MPT roots
  if current roots equal post-roots, mark mpt_applied
  if current roots equal pre-roots, reapply MPT plan and verify post-roots
  otherwise quarantine

mpt_applied:
  verify SQL postconditions and MPT post-roots
  verify root-role expectations:
    staged roots for preparation jobs
    active pointers for finalization/bootstrap/rebuild jobs
  continue to verified

verified:
  mark complete

quarantined:
  keep readiness false
  require explicit operator recovery
```

Do not roll back SQL domain mutations after `sql_applied`. Once the SQL
transaction commits, recovery must roll the job forward or quarantine. Rollback
by inverse mutation is a separate audited recovery command, not automatic
startup behavior.

Abandoned jobs are terminal only when their plan proves no authoritative local
state was promoted. For `block_root_preparation`, abandoned staged roots may be
marked `abandoned` only after recovery verifies active root pointers still
reference the pre-job generation and no pending block finalization depends on
the staged roots.

## Startup Replay And Quarantine Behavior

Startup sequence must be:

1. Initialize SQL client.
2. Assert schema migrations are at the expected version.
3. Acquire the node startup/recovery advisory lock.
4. Open MPT stores without genesis reseeding.
5. Load active root manifests and unfinished `local_ledger_mutation_jobs`.
6. If configuration requires fresh-node bootstrap and no bootstrap job exists,
   create the explicit `genesis_bootstrap` job under the same recovery lock
   after proving the fresh-deployment preconditions.
7. Perform pre-recovery root checks that allow each unfinished job's planned
   pre-roots or post-roots. A missing manifest is allowed only when an unfinished
   `genesis_bootstrap` or explicit `mpt_rebuild` job proves it is expected.
8. Replay recoverable jobs in order and stop on the first quarantine.
9. If any job quarantines, set durable node state to degraded/quarantined and
   fail readiness.
10. Verify SQL root manifests strictly against actual MPT roots after replay.
11. Hydrate in-memory refs from durable SQL only after recovery succeeds.
12. Start submission, validation, commit, confirmation, deposit, and merge
    workers only after recovery succeeds.

Quarantine conditions:

- current MPT root is neither planned pre-root nor planned post-root;
- SQL fingerprint differs from both planned precondition and planned
  postcondition;
- existing row conflicts with planned bytes;
- pending block lifecycle status contradicts job status;
- required L1 evidence is missing for a local finalization that depends on it;
- LevelDB path is missing or empty while SQL indicates non-empty state;
- startup genesis/bootstrap is requested but SQL, MPT, root manifests, or L1
  protocol state are not provably fresh;
- a standalone deposit projection failure was observed without a recoverable
  journal job;
- plan hash does not match stored plan bytes;
- schema version of the job is unknown to the running binary.

Quarantine behavior:

- write `local_ledger_mutation_quarantines` with job id, reason code,
  observed fingerprints, observed roots, and operator guidance;
- set readiness false with a specific reason such as
  `local_mutation_quarantined:<job_id>:<reason>`;
- refuse `/submit`, validation batches, block commitment, local block
  finalization, and merge finalization;
- allow read-only status and explicit recovery inspection commands;
- do not delete, reseed, truncate, or rebuild automatically.

## Schema Changes

Add migrations. Do not add silent `CREATE TABLE IF NOT EXISTS` behavior to normal
startup as the migration mechanism.

### `local_ledger_mutation_jobs`

```sql
CREATE TABLE local_ledger_mutation_jobs (
  job_id UUID PRIMARY KEY,
  kind TEXT NOT NULL,
  status TEXT NOT NULL,
  schema_version INTEGER NOT NULL,
  idempotency_key TEXT NOT NULL UNIQUE,
  correlation_id TEXT NOT NULL,
  plan_cbor BYTEA NOT NULL,
  plan_hash BYTEA NOT NULL,
  sql_plan_hash BYTEA NOT NULL,
  mpt_plan_hash BYTEA NOT NULL,
  expected_sql_pre_hash BYTEA,
  expected_sql_post_hash BYTEA,
  expected_mpt_pre_roots JSONB NOT NULL DEFAULT '{}'::jsonb,
  expected_mpt_post_roots JSONB NOT NULL DEFAULT '{}'::jsonb,
  observed_sql_hash BYTEA,
  observed_mpt_roots JSONB NOT NULL DEFAULT '{}'::jsonb,
  attempt_count INTEGER NOT NULL DEFAULT 0,
  last_error_code TEXT,
  last_error_detail TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  sql_applied_at TIMESTAMPTZ,
  mpt_applied_at TIMESTAMPTZ,
  verified_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  CHECK (kind IN (
    'mempool_acceptance_batch',
    'deposit_projection_batch',
    'genesis_bootstrap',
    'block_root_preparation',
    'block_local_finalization',
    'merge_local_finalization',
    'mpt_rebuild'
  )),
  CHECK (status IN (
    'planned',
    'sql_applied',
    'mpt_applying',
    'mpt_applied',
    'verified',
    'complete',
    'quarantined',
    'abandoned'
  ))
);
```

Indexes:

- `(status, created_at)` for startup recovery.
- `(kind, status, created_at)` for dashboards.
- partial unique index allowing only one non-terminal job for block/merge
  lifecycle if overlapping mutation cannot be proven safe.

### `local_ledger_mutation_events`

Append-only audit stream for each job:

```sql
CREATE TABLE local_ledger_mutation_events (
  event_id BIGSERIAL PRIMARY KEY,
  job_id UUID NOT NULL REFERENCES local_ledger_mutation_jobs(job_id),
  event_type TEXT NOT NULL,
  event_detail JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

### `local_ledger_mutation_sql_markers`

Used by recovery to prove a SQL transaction committed:

```sql
CREATE TABLE local_ledger_mutation_sql_markers (
  job_id UUID PRIMARY KEY REFERENCES local_ledger_mutation_jobs(job_id),
  sql_post_hash BYTEA NOT NULL,
  row_counts JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

### `local_mpt_roots` And `local_mpt_active_roots`

Use the root manifest schema described above. The active pointer should be
updated only from the MPT executor after post-root verification and, for
block-derived roots, only from block local finalization after required L1
evidence is verified. Migrations must allow repeated root hashes across
generations and must enforce that active pointers cannot reference `staged`,
`retired`, or `abandoned` manifest rows.

### `local_ledger_mutation_quarantines`

```sql
CREATE TABLE local_ledger_mutation_quarantines (
  quarantine_id BIGSERIAL PRIMARY KEY,
  job_id UUID NOT NULL REFERENCES local_ledger_mutation_jobs(job_id),
  reason_code TEXT NOT NULL,
  reason_detail TEXT NOT NULL,
  observed_sql_hash BYTEA,
  observed_mpt_roots JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  resolved_at TIMESTAMPTZ
);
```

### Existing Table Adjustments

- Add `job_id` nullable references to mutation-owned rows where useful for
  traceability: `mempool`, `processed_mempool`, `mempool_ledger`,
  `mempool_tx_deltas`, `tx_rejections`, `immutable`, `blocks`,
  `pending_block_finalizations`, and deposit status transitions. If a generic
  table helper is shared by multiple logical tables, add the column through
  explicit migrations and keep helper APIs transaction-aware rather than
  relying on implicit defaults.
- Add `ordinal` to `blocks` so block tx order is durable and replayable. Current
  `blocks` rows have `height`, `header_hash`, and unique `tx_id`
  (`src/database/blocks.ts:68` to `src/database/blocks.ts:87`) but no explicit
  per-header ordinal.
- Add strict conflict helpers for tx and ledger inserts. Existing generic tx
  bulk insert uses `ON CONFLICT DO NOTHING`
  (`src/database/utils/tx.ts:179` to `src/database/utils/tx.ts:192`) and ledger
  bulk insert uses `ON CONFLICT DO NOTHING`
  (`src/database/utils/ledger.ts:425` to `src/database/utils/ledger.ts:437`).
  Production replay needs byte-identical conflict verification.
- Replace whole-table `TRUNCATE ... CASCADE` in mutation paths with planned
  keyed deletes. `clearTable` currently truncates (`src/database/utils/common.ts:38`
  to `src/database/utils/common.ts:54`) and should remain limited to explicit
  offline/reset commands, not normal finalization.

## Observability And Readiness

### Metrics

Add metrics:

- `local_ledger_mutation_jobs_total{kind,status}`
- `local_ledger_mutation_recovery_attempts_total{kind,result}`
- `local_ledger_mutation_quarantines_total{kind,reason_code}`
- `local_ledger_mutation_oldest_unfinished_age_ms`
- `local_ledger_mutation_replay_duration_ms{kind}`
- `local_mpt_root_mismatch_total{trie_name}`
- `local_mpt_root_verification_duration_ms{trie_name}`
- `local_sql_fingerprint_verification_duration_ms{kind}`

### Logs And Audit Events

Every mutation log line should include:

- `job_id`
- `kind`
- `status`
- `correlation_id`
- `header_hash` when available
- `tx_count` and `deposit_count` when available
- pre-root and post-root hashes for MPT jobs
- SQL fingerprint hashes

Durable audit events are required for:

- job creation;
- SQL apply start and success;
- MPT apply start and success;
- verification success;
- replay start and result;
- quarantine;
- operator recovery commands.

### Readiness

Readiness must include durable mutation state, not only in-memory
`LOCAL_FINALIZATION_PENDING`.

Add readiness reasons:

- `local_mutation_unfinished:<count>`
- `local_mutation_recovering:<job_id>`
- `local_mutation_quarantined:<job_id>:<reason_code>`
- `mpt_root_manifest_missing:<trie_name>`
- `mpt_root_mismatch:<trie_name>`
- `mpt_root_staged_without_owner:<trie_name>:<generation>`
- `genesis_bootstrap_required`
- `deposit_projection_recovery_required:<job_id>`
- `pending_finalization_unresolved:<header_hash>`

The node is not ready to admit traffic when any non-terminal job exists unless
startup recovery is actively running under the recovery lock and workers have not
started. After workers start, any non-terminal or quarantined job means
readiness false.

## Tests And Fault Injection

Add deterministic fault-injection hooks around every journal stage. Hooks should
be test-only and unavailable in production configuration unless explicitly
compiled or configured for test mode.

Required unit tests:

- plan canonicalization is deterministic and hashes change when any planned row
  or MPT op changes;
- strict SQL conflict helpers accept byte-identical replay and reject conflicting
  bytes;
- MPT executor treats pre-root and post-root as recoverable and any other root
  as quarantine;
- recovery state machine transitions are monotonic;
- readiness reports specific reasons for unfinished and quarantined jobs;
- root manifest helpers allow repeated root hashes across generations while
  preventing active pointers to staged/retired/abandoned rows.

Required integration tests:

- mempool acceptance crash before SQL transaction: restart replays from
  `planned`;
- mempool acceptance crash after SQL commit before `complete`: restart verifies
  SQL and completes exactly once;
- mempool acceptance conflicting existing row: quarantine, no cache update;
- local CLI transfer acceptance and rejection use the same
  `mempool_acceptance_batch` journal path as HTTP/queue submission;
- deposit projection crash after `mempool_ledger` insert before deposit status
  marker: restart verifies or quarantines without suppressing startup failure;
- genesis bootstrap on a fresh configured test deployment creates matching SQL
  rows and root manifests exactly once;
- genesis bootstrap with any pre-existing conflicting SQL row, MPT root, root
  manifest, pending job, or non-empty L1 deployment quarantines before mutation;
- block root preparation crash after SQL deposit projection before MPT: restart
  applies MPT and verifies roots;
- block root preparation crash after one trie root advances but before manifest
  update: restart verifies current roots and either completes or quarantines;
- block root preparation produces only staged roots and does not advance active
  root pointers before local finalization;
- deferred mempool-to-processed-mempool transfer crash after insert before delete
  rolls forward from the `block_root_preparation` job without duplicating or
  losing tx payloads;
- block local finalization crash after immutable insert before block rows:
  restart rolls forward from the journal;
- block local finalization crash after mempool MPT clear before SQL complete
  marker: restart identifies post-root and verifies SQL postconditions;
- block local finalization promotes exactly the staged roots linked to the
  confirmed header and rejects unrelated staged roots;
- merge local finalization crash after confirmed ledger deletes before inserts:
  SQL transaction rollback leaves status recoverable from `planned`;
- merge local finalization crash after SQL commit before `complete`: restart
  verifies confirmed ledger fingerprint and clears the block exactly once;
- startup with empty ledger MPT and non-empty SQL: quarantine, no genesis
  reseeding;
- startup with missing root manifest: readiness false and no workers started;
- two node instances start: advisory lock prevents concurrent recovery;
- provider outage during merge confirmation: no local merge finalization job
  reaches SQL apply without L1 evidence.

Required property tests:

- replaying any generated idempotent MPT batch zero, one, or two times from
  allowed roots either reaches the same post-root or quarantines;
- generated SQL acceptance batches preserve UTxO conservation under replay;
- generated block finalization jobs leave no tx id in both `mempool` and
  `immutable` after completion.

## Rollout Steps

1. Add versioned migrations for the new journal, event, quarantine, and MPT root
   manifest tables.
2. Add strict SQL helper variants for byte-identical conflict checks and keyed
   deletes.
3. Add the local mutation job repository and recovery state machine behind tests.
4. Add MPT root manifest initialization with fail-closed checks, staged/active
   root roles, and active-pointer enforcement. Remove automatic genesis
   reseeding from normal startup.
5. Convert mempool acceptance to `mempool_acceptance_batch` jobs.
6. Convert local CLI transfer submission to the same
   `mempool_acceptance_batch` executor.
7. Convert or remove standalone deposit projection. If retained, run it through
   `deposit_projection_batch` jobs and stop suppressing startup projection
   failures.
8. Replace startup genesis seeding with explicit `genesis_bootstrap` jobs or an
   operator bootstrap command; do not fork local ledger seeding in the
   background.
9. Convert `processMpts` into a pure plan builder plus journaled SQL/MPT
   executor for `block_root_preparation`.
10. Move deferred mempool-to-processed-mempool transfer into the
    `block_root_preparation` job.
11. Convert `finalizeCommittedBlockLocally` into `block_local_finalization`,
    including active-root promotion from staged roots.
12. Convert merge local finalization into `merge_local_finalization` after the
    merge confirmation P0 fix is in place.
13. Extend startup to run recovery before worker startup.
14. Extend readiness, metrics, and audit events.
15. Add fault-injection integration tests and run them in CI.
16. Provide explicit operator commands for inspecting jobs, quarantines, and MPT
    root manifests.
17. Document the required one-time migration path for existing local state:
    either wipe and bootstrap in an explicitly local/dev environment, or run an
    audited production migration that fingerprints SQL and builds root manifests.

## Risks And Open Questions

- MPT library persistence semantics need verification. The implementation must
  confirm whether trie `batch`, `checkpoint`, `commit`, root persistence, and
  LevelDB close are crash-safe enough for the pre-root/post-root replay model.
- `processMpts` currently mutates active tries to compute candidate roots before
  submission. The safest design may require staged trie namespaces or copy-on-
  write root handles so failed submissions do not advance active roots.
- The exact confirmed-state MPT model is unclear from the current merge path.
  If confirmed ledger needs an MPT root, add it explicitly. If not, record an
  empty MPT plan and rely on SQL fingerprints for merge finalization.
- Deposit status transitions are coupled to mempool acceptance, block
  preparation, and block finalization. The final schema should define one
  deposit lifecycle state machine so replay cannot mark a deposit consumed and
  projected inconsistently.
- Standalone deposit projection may be unnecessary once block preparation owns
  deposit inclusion. If it remains for latency or operator ergonomics, it must
  be journaled; otherwise remove the background fiber and startup reconciliation
  mutation path from production mode.
- Genesis behavior must be separated from normal startup. A fresh-node bootstrap
  command can exist, but production startup must not silently seed SQL or MPT
  state based on an empty directory.
- Existing local databases may not have enough historical data to derive all
  required fingerprints and root manifests. Production rollout should prefer
  explicit fail-closed migration over compatibility shims.
- Advisory locking needs to cover worker threads and multiple process instances.
  The lock should be in PostgreSQL, not an in-process ref.
- Whole-table reset code remains dangerous for production. It should be isolated
  behind explicit offline/admin controls in a separate readiness item, but this
  plan should avoid depending on reset for recovery.

## Concrete Checklist

- [ ] Add migrations for `local_ledger_mutation_jobs`,
      `local_ledger_mutation_events`, `local_ledger_mutation_sql_markers`,
      `local_mpt_roots`, `local_mpt_active_roots`, and
      `local_ledger_mutation_quarantines`.
- [ ] Add explicit `blocks.ordinal` migration and deterministic retrieval by
      `(header_hash, ordinal)`.
- [ ] Add strict byte-identical conflict helpers for tx rows, ledger rows, block
      rows, and deposit-origin rows.
- [ ] Add SQL fingerprint helpers for mempool, mempool ledger, processed
      mempool, confirmed ledger, blocks, and pending finalization rows.
- [ ] Add MPT root manifest read/write helpers with staged/active/retired roles
      and repeated-root-hash support across generations.
- [ ] Add active-root pointer enforcement so staged, retired, or abandoned roots
      cannot become active except through the verified promotion helper.
- [ ] Add MPT executor that checks pre-root/post-root and quarantines unknown
      roots.
- [ ] Add local mutation job repository with monotonic status transitions.
- [ ] Add recovery runner guarded by PostgreSQL advisory lock.
- [ ] Run recovery before startup hydrates globals or starts workers.
- [ ] Replace mempool acceptance persistence with a journaled
      `mempool_acceptance_batch` executor.
- [ ] Route local CLI transfer persistence through the same
      `mempool_acceptance_batch` executor.
- [ ] Refactor `MempoolDB.insertMultiple` into a transaction-aware domain helper.
- [ ] Convert standalone deposit projection into `deposit_projection_batch` or
      remove it from normal runtime.
- [ ] Replace background startup genesis seeding with explicit
      `genesis_bootstrap` or operator bootstrap flow.
- [ ] Replace direct SQL/MPT mutations in `processMpts` with
      `block_root_preparation` planning and execution.
- [ ] Include deferred mempool-to-processed-mempool transfer in the
      `block_root_preparation` journal.
- [ ] Remove normal-path `mempoolTrie.delete()` and replace it with deterministic
      planned clearing or generation promotion.
- [ ] Replace `finalizeCommittedBlockLocally` with a journaled
      `block_local_finalization` executor that promotes only verified staged
      roots for the confirmed header.
- [ ] Replace merge confirmed ledger/block mutations with a journaled
      `merge_local_finalization` executor.
- [ ] Ensure merge local finalization requires proven L1 confirmation before SQL
      apply.
- [ ] Remove automatic genesis reseeding from normal startup; fresh bootstrap
      must be an explicit journaled job.
- [ ] Extend readiness with unfinished, recovering, quarantined, and root
      mismatch reasons.
- [ ] Add metrics and durable audit events for every job stage.
- [ ] Add fault-injection tests for every crash point listed above.
- [ ] Add operator commands to inspect, replay, quarantine, and explicitly
      rebuild MPT state.
- [ ] Document production migration and rollback procedures.
