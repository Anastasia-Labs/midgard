# Midgard Node Production-Readiness Report

This report explains the production-readiness task list for `demo/midgard-node`
in plain language. The goal is not just to name fragile areas, but to explain
what each item means, why it matters for a production L2, and what a strong fix
should accomplish.

The priority labels follow the repository directive:

- `P0`: production blocker. These can affect correctness, safety, liveness, or
  recovery in ways that make the node unsafe to operate as a production L2.
- `P1`: correctness and safety hardening. These may not always break the happy
  path, but they leave important edge cases, adversarial cases, or operator
  failure modes under-specified.
- `P2`: operational maturity. These make the node easier to run, audit, secure,
  benchmark, and recover.

## Glossary

Some terms appear throughout this report:

- **Ack / acknowledgement**: the response the node sends back to a caller saying
  it accepted responsibility for something. In `/submit`, the current ack is the
  JSON response with `status: "queued"`.
- **Durable**: stored in a place that survives process crashes and restarts,
  such as Postgres or a carefully managed on-disk log.
- **Idempotent**: safe to repeat. If the same transaction is submitted twice, the
  second request should not create duplicate work or change the final result.
- **Duplicate suppression**: detecting that a transaction was already submitted,
  accepted, rejected, committed, or queued, then returning the existing status
  instead of validating it again.
- **Replayable after crash**: after the process dies and restarts, the node can
  look at durable state and continue from the last safe point.
- **Fail closed**: stop serving or refuse admission when correctness cannot be
  proven. The opposite is "log and keep going."
- **Canonical state**: the current authoritative state of a transaction or
  ledger object, not an old intermediate observation.
- **Journal**: a durable record of an in-progress state transition. If the node
  crashes halfway through, the journal tells startup code exactly what to finish,
  roll forward, or quarantine.

## P0: Production Blockers

### 1. Make Submission Durable Before Ack

**Finding**

`/submit` currently puts the transaction into an in-memory Effect queue and then
returns `status: "queued"`. The queue is created here:

[`src/commands/listen.ts:65`](./src/commands/listen.ts#L65)

```ts
const txQueue = yield* Queue.dropping<QueuedTxPayload>(
  Math.max(1, nodeConfig.MAX_SUBMIT_QUEUE_SIZE),
);
```

The ack is returned after the payload is offered to that queue:

[`src/commands/listen-router.ts:1037`](./src/commands/listen-router.ts#L1037)

```ts
const queued = yield* txQueue.offer(payload);
...
return yield* HttpServerResponse.json({
  txId: normalized.txIdHex,
  status: "queued",
});
```

**What this means**

The node tells the client "I queued your transaction" before the transaction is
stored durably. That is an ack. But the queue lives in process memory. If the
process crashes after this response and before the validation worker drains the
queue, the transaction is gone.

`Queue.dropping` also means that when the queue is full, new offers can be
dropped rather than being part of a durable backpressure protocol. The handler
does check the return value, which is good, but the core issue remains: accepted
work is not recoverable.

**Why this matters**

For users and operators, `status: "queued"` means the node accepted
responsibility for the transaction. If a crash can erase that transaction, then
clients can observe false acceptance:

1. Client submits transaction.
2. Node returns `{"status":"queued"}`.
3. Node process crashes.
4. Node restarts with no record of that transaction.
5. Client asks for status and gets `not_found`.

That is not a production L2 admission contract. A production node should either
durably accept the transaction or clearly reject it.

**What a production fix should do**

Create a durable admission table or log before returning success. Conceptually:

```sql
BEGIN;

INSERT INTO tx_admissions (tx_id, tx_cbor, status, first_seen_at, last_seen_at)
VALUES ($1, $2, 'queued', NOW(), NOW())
ON CONFLICT (tx_id) DO UPDATE
SET last_seen_at = NOW()
RETURNING status;

COMMIT;
```

Then `/submit` can return a response based on durable state:

```ts
const admission = yield* TxAdmissionsDB.admit(normalized);

return HttpServerResponse.json({
  txId: normalized.txIdHex,
  status: admission.status,
});
```

The validation worker should read from the durable queue, not only a process
queue. On restart, it should replay all rows still in a queued or validating
state.

**Definitions in this context**

- **Idempotent admission**: submitting the same `tx_id` twice returns the same
  durable status instead of inserting two jobs.
- **Duplicate suppression**: if `tx_id` already exists in admission, mempool,
  immutable, or canonical rejection state, the node should return that status
  before doing expensive validation again.
- **Replayable after crash**: the worker can restart and resume from
  `tx_admissions` without the client resubmitting.

### 2. Make Ledger Mutations Atomic And Recoverable

**Finding**

The node updates several pieces of durable state as separate operations. For
example, validation accepts or rejects transactions, then persists those results
in separate calls:

[`src/fibers/tx-queue-processor.ts:521`](./src/fibers/tx-queue-processor.ts#L521)

```ts
if (allRejected.length > 0) {
  yield* TxRejectionsDB.insertMany(...);
}

if (phaseB.accepted.length > 0) {
  yield* MempoolDB.insertMultiple(...);
}
```

Block finalization also mutates multiple stores concurrently:

[`src/workers/utils/commit-submission.ts:120`](./src/workers/utils/commit-submission.ts#L120)

```ts
yield* Effect.all(
  [
    Effect.forEach(filteredBatches, ...),
    ProcessedMempoolDB.clear,
    mempoolTrie.delete(),
  ],
  { concurrency: "unbounded" },
);
```

Merge finalization updates confirmed ledger rows and block rows after on-chain
submission:

[`src/transactions/state-queue/merge-to-confirmed-state.ts:1413`](./src/transactions/state-queue/merge-to-confirmed-state.ts#L1413)

```ts
const finalizeLocalMergeProgram = Effect.gen(function* () {
  yield* ConfirmedLedgerDB.clearUTxOs(...);
  yield* ConfirmedLedgerDB.insertMultiple(...);
  yield* BlocksDB.clearBlock(headerHash);
});
```

**What this means**

The node has state split across multiple stores:

- Postgres tables like `mempool`, `immutable`, `blocks`, `confirmed_ledger`.
- LevelDB-backed MPT stores for ledger and mempool roots.
- In-memory runtime refs used by fibers.

If a state transition touches several of these and one write fails midway, the
node can end up with a mixed state. For example:

- immutable tx rows inserted, but block links missing.
- mempool SQL rows cleared, but the mempool trie still contains old entries.
- confirmed ledger spent UTxOs deleted, but produced UTxOs not inserted.
- block cleared from `BlocksDB`, but confirmed ledger update failed.

**Why this matters**

An L2 node is only as reliable as its state transition boundaries. If a block is
half-finalized locally, the next block or merge may use an incorrect ledger root.
That is a correctness issue, not just an operational nuisance.

Postgres transactions can make SQL writes atomic, but SQL and LevelDB cannot be
made atomic by a normal SQL transaction alone. That is where a durable journal is
needed.

**What a production fix should do**

Use explicit state-transition records. A safe shape is:

```sql
INSERT INTO local_finalization_jobs (
  job_id,
  kind,
  header_hash,
  status,
  planned_sql_changes_hash,
  planned_mpt_changes_hash,
  created_at
) VALUES (..., 'block_commit', ..., 'started', ..., ..., NOW());
```

Then progress through explicit stages:

```text
started
sql_applied
mpt_applied
verified
complete
```

On startup:

```ts
const unfinished = yield* LocalFinalizationJobsDB.retrieveUnfinished();

for (const job of unfinished) {
  yield* recoverOrQuarantine(job);
}
```

The key production property is this: after a crash, startup can determine
whether to finish the operation, roll it forward, or fail closed with a clear
operator error. It should not guess.

### 3. Do Not Finalize Local Merge State After L1 Confirmation Failure

**Finding**

The merge path catches `TxConfirmError`, logs it, and then continues:

[`src/transactions/state-queue/merge-to-confirmed-state.ts:1402`](./src/transactions/state-queue/merge-to-confirmed-state.ts#L1402)

```ts
const onConfirmFailure = (err: TxConfirmError) =>
  Effect.logError(`Confirm tx error: ${err}`);

yield* handleSignSubmit(lucid, txBuilder).pipe(
  Effect.catchTag("TxSubmitError", onSubmitFailure),
  Effect.catchTag("TxConfirmError", onConfirmFailure),
);

yield* Effect.logInfo("Merge transaction submitted, updating the db...");
```

**What this means**

`handleSignSubmit` can fail because the node could not confirm that the L1
transaction landed. For merge, that confirmation matters because the local DB is
about to be changed to reflect the merge.

Today, a confirmation failure becomes a log line. The function then continues
into local finalization.

**Why this matters**

The local confirmed ledger must not claim a merge is canonical unless L1
confirms it. Otherwise:

1. Merge tx is submitted.
2. Confirmation fails or times out.
3. Node updates local confirmed ledger anyway.
4. L1 may later reject, expire, or not contain that transaction.
5. Local confirmed state diverges from canonical chain state.

For a production L2, that is a severe state-integrity risk.

**What a production fix should do**

Do not continue to local finalization on `TxConfirmError`. Instead:

```ts
const onConfirmFailure = (err: TxConfirmError) =>
  Effect.fail(
    new MergeConfirmationError({
      message: "Merge tx was not confirmed; local merge finalization blocked",
      cause: err,
      txHash: txBuilder.toHash(),
    }),
  );
```

If there is a chance the tx actually landed but confirmation failed because of a
provider outage, record a durable pending merge recovery job:

```text
merge_submitted -> confirmation_unknown -> operator/recovery verifies L1 -> local_finalize
```

The important rule: local merge finalization should happen only after confirmed
L1 evidence, or through an explicit recovery path that verifies that evidence.

### 4. Fail Closed On Startup Reconciliation And Integrity Failures

**Finding**

Startup catches deposit reconciliation failures, logs them, and continues:

[`src/commands/listen.ts:73`](./src/commands/listen.ts#L73)

```ts
yield* fetchAndInsertDepositUTxOs.pipe(
  Effect.tapError((e) =>
    Effect.logWarning(`Startup deposit catch-up failed: ${JSON.stringify(e)}`),
  ),
  Effect.catchAll(() => Effect.void),
);
```

The same pattern appears for deposit projection:

[`src/commands/listen.ts:81`](./src/commands/listen.ts#L81)

```ts
yield* projectDepositsToMempoolLedger.pipe(
  Effect.tapError((e) =>
    Effect.logWarning(
      `Startup deposit projection reconciliation failed: ${JSON.stringify(e)}`,
    ),
  ),
  Effect.catchAll(() => Effect.void),
);
```

**What this means**

The node can start serving even if it failed to catch up deposits or failed to
project them into the mempool ledger. That means the node may admit or validate
transactions against an incomplete local view.

**Why this matters**

Startup is the moment where the node should prove its local state is safe. If it
cannot prove that deposits, pending finalizations, SQL tables, MPT roots, and
on-chain state line up, it should not accept new traffic.

Continuing after a failed reconciliation creates failures that are hard to
diagnose:

- a deposit exists on L1 but not in local state;
- a transaction that spends a deposit is rejected incorrectly;
- a deposit is projected twice or not at all;
- a future root is computed from incomplete state.

**What a production fix should do**

Make startup checks explicit and mandatory:

```ts
yield* InitDB.program;
yield* ensureProtocolInitializedOnStartup;
yield* verifySchemaVersion;
yield* verifySqlAndMptRoots;
yield* hydratePendingBlockFinalizationOnStartup;
yield* reconcileDepositsOrFail;
yield* verifyReadinessDependencies;
```

If a check fails, the process should fail startup or enter an explicit degraded
mode that refuses admission:

```text
startup_failed: deposit_projection_incomplete
```

The operator should see a concrete reason and a recovery path, not a warning
buried in logs.

### 5. Replace Ad Hoc Schema Creation With Versioned Migrations

**Finding**

Database initialization creates current tables directly:

[`src/database/init.ts:21`](./src/database/init.ts#L21)

```ts
yield* AddressHistory.createTable;
yield* BlocksDB.createTable;
yield* Ledger.createTable(ConfirmedLedgerDB.tableName);
yield* Ledger.createTable(LatestLedgerDB.tableName);
...
```

Most table creation uses `CREATE TABLE IF NOT EXISTS`.

**What this means**

The code ensures tables exist, but it does not record which schema version the
database is on. It also does not provide an auditable chain of migrations from
old schema to new schema.

This works for demos because a fresh database can be created from the latest
code. It is not enough for production because production databases live across
upgrades.

**Why this matters**

Without versioned migrations:

- operators cannot tell which schema version is deployed;
- a partially upgraded database may look "initialized";
- code can start against an incompatible schema;
- rollback procedures are unclear;
- schema changes are not easy to audit.

For an L2, schema drift can become state drift.

**What a production fix should do**

Introduce a migration table:

```sql
CREATE TABLE schema_migrations (
  version TEXT PRIMARY KEY,
  checksum TEXT NOT NULL,
  applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

Then store each schema change as an explicit migration:

```text
001_initial_schema.sql
002_add_pending_block_submitted_at.sql
003_add_admission_queue.sql
```

Startup should fail if the database is missing migrations or has unknown ones:

```ts
yield* MigrationRunner.assertAtExpectedVersion(EXPECTED_SCHEMA_VERSION);
```

`CREATE TABLE IF NOT EXISTS` can still be useful inside a migration, but normal
startup should not silently reshape production schema.

### 6. Remove Unsafe Recovery Shortcuts

**Finding**

The MPT initialization path reseeds genesis UTxOs when the ledger trie root is
empty:

[`src/workers/utils/mpt.ts:100`](./src/workers/utils/mpt.ts#L100)

```ts
const ledgerRootIsEmpty = yield* ledgerTrie.rootIsEmpty();
if (ledgerRootIsEmpty) {
  yield* Effect.logInfo(
    "No previous ledger trie root found - inserting genesis utxos",
  );
  ...
}
```

**What this means**

An empty ledger trie is interpreted as "new node, bootstrap from genesis." That
is valid only when all other durable state also says this is a brand-new node.

If Postgres has existing ledger/block state but the LevelDB trie directory is
lost or empty, reseeding genesis creates a local trie root that does not match
the SQL state.

**Why this matters**

This is a classic recovery ambiguity:

- Fresh node: empty trie is expected.
- Corrupted node: empty trie is a serious problem.

Production code must distinguish those cases. If it does not, a data-loss event
can look like normal bootstrap.

**What a production fix should do**

Gate genesis seeding behind an explicit condition:

```ts
if (ledgerTrieEmpty && durableSqlStateEmpty && explicitBootstrapMode) {
  yield* seedGenesisTrie();
} else if (ledgerTrieEmpty) {
  yield* Effect.fail(new IntegrityError("ledger trie missing for non-empty node"));
}
```

If rebuilding from SQL is supported, make that an explicit recovery operation:

```text
midgard-node recover-ledger-mpt --from confirmed_ledger --verify-root <expected>
```

The default production path should fail closed, not silently reseed.

### 7. Fix Readiness So It Means Safe To Admit Traffic

**Finding**

Readiness currently checks database health, worker heartbeat age, queue depth,
and local finalization pending:

[`src/commands/readiness.ts:15`](./src/commands/readiness.ts#L15)

```ts
export type ReadinessInput = {
  readonly maxQueueDepth: number;
  readonly queueDepth: number;
  readonly workerHeartbeats: WorkerHeartbeats;
  readonly localFinalizationPending: boolean;
  readonly dbHealthy: boolean;
};
```

The queue processor also drains the Effect queue into an in-memory
`pendingPayloads` array:

[`src/fibers/tx-queue-processor.ts:387`](./src/fibers/tx-queue-processor.ts#L387)

```ts
const drainedChunk = yield* Queue.takeAll(txQueue);
const drainedPayloads = Chunk.toReadonlyArray(drainedChunk);
pendingPayloads.push(...drainedPayloads);
```

**What this means**

The externally visible queue depth can look small after the processor drains the
queue, even though many payloads are still pending validation in memory. Also,
readiness does not include several states that affect whether it is safe to
accept traffic:

- unresolved L1 block submission;
- stale deposit ingestion;
- provider/indexer outage;
- reference-script availability;
- validation infrastructure failure;
- startup integrity failure;
- durable admission backlog.

**Why this matters**

Load balancers and operators often use readiness as "can I send this node
traffic?" If readiness says yes while the node is saturated or unable to make
progress, traffic keeps flowing into a bad state.

That turns an internal problem into a larger outage.

**What a production fix should do**

Readiness should answer this question:

```text
Is this node safe to admit new user traffic right now?
```

Inputs should include:

```ts
type ProductionReadinessInput = {
  dbHealthy: boolean;
  durableAdmissionBacklog: number;
  validationBacklog: number;
  unresolvedSubmittedBlockAgeMs: number | null;
  depositIngestionLagMs: number | null;
  providerHealthy: boolean;
  indexerHealthy: boolean;
  mptIntegrityHealthy: boolean;
  localRecoveryState: "none" | "recovering" | "failed";
};
```

Readiness should fail with machine-readable reasons such as:

```json
{
  "ready": false,
  "reasons": [
    "unresolved_submitted_block:720000",
    "deposit_ingestion_stale",
    "durable_admission_backlog_exceeded"
  ]
}
```

### 8. Make Retention State-Aware

**Finding**

The retention sweeper prunes deposits, address history, and rejections by age:

[`src/fibers/retention-sweeper.ts:26`](./src/fibers/retention-sweeper.ts#L26)

```ts
const cutoff = computeRetentionCutoff(new Date(), nodeConfig.RETENTION_DAYS);
const [prunedTxRejections, prunedAddressHistory, prunedDeposits] =
  yield* Effect.all([
    TxRejectionsDB.pruneOlderThan(cutoff),
    AddressHistoryDB.pruneOlderThan(cutoff),
    DepositsDB.pruneOlderThan(cutoff),
  ]);
```

**What this means**

Age alone decides whether deposit rows can be deleted. It does not first prove
that a deposit is terminal, fully projected, committed, merged, archived, or no
longer needed for replay/audit.

**Why this matters**

Deposits are part of the bridge between L1 and L2. Deleting live or replay-needed
deposit evidence can break:

- deposit inclusion auditability;
- replay after crash;
- fraud/integrity investigation;
- reconciliation between L1 and local L2 state.

For production, retention must never delete evidence that is still needed to
prove correctness.

**What a production fix should do**

Add explicit retention classes and terminal-state checks:

```text
awaiting_l1_confirmation -> never prune
projected_to_mempool -> never prune
included_in_committed_block -> retain until merge/audit boundary
archived_terminal -> prune only after archive checksum is durable
```

The prune query should look more like:

```sql
DELETE FROM deposits_utxos
WHERE status = 'archived_terminal'
  AND archived_at < $cutoff
  AND audit_export_id IS NOT NULL;
```

The goal is not "keep everything forever." The goal is "delete only what is safe
to delete, and leave an audit trail."

### 9. Remove Or Hard-Disable Destructive Reset From Production

**Finding**

The reset code currently has a disabled on-chain UTxO reset, but the local DB/MPT
wipe code still exists:

[`src/reset.ts:218`](./src/reset.ts#L218)

```ts
export const resetUTxOs = Effect.fail(
  new SDK.LucidError({
    message:
      "Reset endpoint is disabled: deinit path is not implemented for deployed contracts",
    cause: "reset-disabled",
  }),
);
```

[`src/reset.ts:233`](./src/reset.ts#L233)

```ts
export const resetDatabases = Effect.all(
  [
    MempoolDB.clear,
    MempoolLedgerDB.clear,
    ...
    deleteMempoolMpt,
    deleteLedgerMpt,
  ],
  { discard: true },
);
```

The route is exposed as `GET /reset`:

[`src/commands/listen-router.ts:655`](./src/commands/listen-router.ts#L655)

```ts
/**
 * `GET /reset`: triggers the reset flow that reclaims protocol-controlled
 * UTxOs.
 */
```

**What this means**

There is still a runtime path whose purpose is to delete local node state. Even
if the current on-chain part fails first, keeping this as part of the production
HTTP surface is unsafe.

**Why this matters**

A production L2 node should not have an online endpoint that can wipe local
ledger state. Recovery actions should be explicit, offline, audited, and gated
by operator procedures.

Reset endpoints are especially risky because they combine:

- high blast radius;
- easy accidental invocation;
- hard-to-verify aftermath;
- possible divergence from L1 state.

**What a production fix should do**

Remove `/reset` from production routing. If local reset is needed for tests or
dev, isolate it behind a non-production build or explicit command:

```text
midgard-node unsafe-dev-reset --i-understand-this-deletes-local-state
```

For production recovery, create narrowly scoped tools:

```text
midgard-node verify-state
midgard-node rebuild-mempool-mpt
midgard-node recover-finalization-job <job-id>
```

Each tool should emit durable audit records.

## P1: Correctness And Safety Hardening

### 10. Canonicalize Tx Status And Rejection Storage

**Finding**

The rejection table allows multiple rows per transaction:

[`src/database/txRejections.ts:33`](./src/database/txRejections.ts#L33)

```ts
CREATE TABLE IF NOT EXISTS tx_rejections (
  tx_id BYTEA NOT NULL,
  reject_code TEXT NOT NULL,
  reject_detail TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

Status resolution gives any rejection priority over committed, accepted, or
pending state:

[`src/commands/tx-status.ts:49`](./src/commands/tx-status.ts#L49)

```ts
if (input.rejection !== null) {
  return {
    txId: input.txIdHex,
    status: "rejected",
    ...
  };
}
```

**What this means**

If a transaction has any rejection record, `/tx-status` can report `rejected`
even if another table later says it is in mempool, processed mempool, or
immutable.

**Why this matters**

Users and operators need transaction status to answer "what is true now?" not
"did anything bad ever happen?" Historical rejection events are useful, but they
should not override canonical committed state.

Otherwise clients can make wrong decisions:

- resubmit a committed transaction;
- assume funds are stuck;
- miss a real finalization;
- alert on stale data.

**What a production fix should do**

Separate event history from canonical lifecycle state:

```text
tx_events: append-only audit trail
tx_status: one row per tx_id with current canonical state
```

Status priority should generally be:

```text
committed > pending_commit > accepted > rejected > queued > not_found
```

If a tx was rejected and later accepted, that transition should either be
impossible by invariant or explicitly recorded and reflected correctly.

### 11. Preserve Deterministic Block Transaction Order

**Finding**

`blocks` stores a `header_hash` and `tx_id`, but no position:

[`src/database/blocks.ts:73`](./src/database/blocks.ts#L73)

```ts
CREATE TABLE IF NOT EXISTS blocks (
  height SERIAL PRIMARY KEY,
  header_hash BYTEA NOT NULL,
  tx_id BYTEA NOT NULL UNIQUE,
  timestamptz TIMESTAMPTZ NOT NULL DEFAULT(NOW())
);
```

Retrieval does not order by an explicit position:

[`src/database/blocks.ts:132`](./src/database/blocks.ts#L132)

```ts
SELECT tx_id FROM blocks
WHERE header_hash = ${headerHash}
```

**What this means**

The code records which transactions belong to a block, but not their authored
order inside that block.

SQL result order is not guaranteed without `ORDER BY`. Even if it usually looks
stable in development, relying on implicit order is not deterministic.

**Why this matters**

Block order is part of replay determinism. If the node later reconstructs a
block from the database and gets transactions in a different order, it can
compute different intermediate effects or produce different diagnostics.

For a production L2, replay should be byte-for-byte and order-for-order
deterministic wherever block contents are persisted.

**What a production fix should do**

Store a position:

```sql
CREATE TABLE blocks (
  header_hash BYTEA NOT NULL,
  tx_index INTEGER NOT NULL,
  tx_id BYTEA NOT NULL,
  PRIMARY KEY (header_hash, tx_index),
  UNIQUE (tx_id)
);
```

Retrieve with:

```sql
SELECT tx_id
FROM blocks
WHERE header_hash = $1
ORDER BY tx_index ASC;
```

Tests should verify exact order, not just set equality.

### 12. Reject Conflicting Duplicate Durable Data Explicitly

**Finding**

The generic transaction insert helper overwrites bytes on `tx_id` conflict:

[`src/database/utils/tx.ts:151`](./src/database/utils/tx.ts#L151)

```ts
/**
 * Upserts one transaction row so the latest serialized body wins for a given
 * transaction id.
 */
...
ON CONFLICT (tx_id) DO UPDATE SET tx = ${txPair.tx}
```

Bulk insert leaves existing rows unchanged:

[`src/database/utils/tx.ts:176`](./src/database/utils/tx.ts#L176)

```ts
/**
 * Bulk-inserts transactions while leaving already-present rows unchanged.
 */
```

**What this means**

The code treats conflicts as normal, but does not always prove the conflicting
payload is identical. In one path, the latest body wins. In another path, the
existing body wins.

**Why this matters**

`tx_id` should identify exactly one canonical transaction body. If code ever
sees the same `tx_id` with different bytes, that is an integrity event. It may
be a bug, database corruption, malicious input, or an impossible cryptographic
collision. In production, "should be impossible" is not a reason to silently
continue.

**What a production fix should do**

Use "same bytes is idempotent, different bytes is an error":

```sql
INSERT INTO mempool (tx_id, tx)
VALUES ($1, $2)
ON CONFLICT (tx_id) DO UPDATE
SET tx = mempool.tx
WHERE mempool.tx = EXCLUDED.tx;
```

If no row is affected because bytes differ, raise an integrity error:

```ts
if (conflict && !bytesEqual(existing.tx, incoming.tx)) {
  yield* Effect.fail(new DatabaseIntegrityError("tx_id payload mismatch"));
}
```

This gives the node a clear invariant: duplicate identical data is safe;
duplicate conflicting data is never hidden.

### 13. Finish The Native Plutus Validation Boundary

**Finding**

The repository already documents this as future work:

[`src/validation/FUTURE_TASK_NATIVE_SCRIPT_CONTEXT_LOCAL_UPLC.md:1`](./src/validation/FUTURE_TASK_NATIVE_SCRIPT_CONTEXT_LOCAL_UPLC.md#L1)

```md
# Future Task: Native Midgard Script-Context Local UPLC Evaluation
```

The document says the current bridge reconstructs Cardano tx bytes and uses a
whole-transaction phase-two API:

[`src/validation/FUTURE_TASK_NATIVE_SCRIPT_CONTEXT_LOCAL_UPLC.md:20`](./src/validation/FUTURE_TASK_NATIVE_SCRIPT_CONTEXT_LOCAL_UPLC.md#L20)

```md
- reconstructs Cardano tx bytes from `MidgardNativeTxFull`
- passes those bytes into a whole-transaction evaluator callback
```

**What this means**

The current implementation uses local UPLC evaluation, which is good. The gap is
the evaluation boundary: the production ideal is to derive script contexts
directly from Midgard-native tx bytes and effective ledger state, not to make
reconstructed Cardano transaction bytes the source of truth.

**Why this matters**

Script validation is a consensus-critical boundary. The more implicit the
context construction is, the harder it is to audit:

- which script purposes were evaluated;
- which redeemer each script saw;
- which reference inputs were included;
- whether ordering matched ledger semantics;
- whether native Midgard encoding was the true source of truth.

For production, the node should make those facts explicit.

**What a production fix should do**

Implement the model described in the task doc:

```text
native Midgard tx + effective UTxO state
  -> canonical TxInfo
  -> enumerate script purposes
  -> build ScriptContext per purpose
  -> local UPLC evaluate each script
```

The report from the audit did not find `.complete({ localUPLCEval: false })`.
That should remain a hard rule.

### 14. Harden HTTP And Control Plane Semantics

**Finding**

Several mutating admin actions are exposed as `GET`:

[`src/commands/listen-router.ts:530`](./src/commands/listen-router.ts#L530)

```ts
/**
 * `GET /init`: initializes protocol state
 */
```

[`src/commands/listen-router.ts:588`](./src/commands/listen-router.ts#L588)

```ts
/**
 * `GET /commit`: triggers manual block commitment.
 */
```

[`src/commands/listen-router.ts:611`](./src/commands/listen-router.ts#L611)

```ts
/**
 * `GET /merge`: triggers manual merge
 */
```

`/submit` accepts tx CBOR from query parameters before reading the body:

[`src/commands/listen-router.ts:972`](./src/commands/listen-router.ts#L972)

```ts
const queryTxHex = extractSubmitTxHexFromQueryParams(params);
...
const txString = queryTxHex ?? bodyTxHex;
```

Request JSON is parsed before a transport-level size limit:

[`src/commands/listen-router.ts:976`](./src/commands/listen-router.ts#L976)

```ts
const parsedBody = yield* Effect.either(request.json);
```

**What this means**

HTTP method semantics and request parsing are too permissive for a production
control plane:

- `GET` should be safe and idempotent; it should not mutate protocol state.
- Query strings are commonly logged by proxies, browsers, dashboards, and access
  logs.
- Parsing JSON before enforcing body size means the node may spend memory and CPU
  on a request it should reject at the boundary.

**Why this matters**

Control-plane mistakes can cause real state transitions. A crawler, health
checker, cache prefetcher, or copied URL should never be able to trigger commit,
merge, init, or reset behavior.

For public endpoints, malformed or oversized requests should be cheap to reject.
Otherwise attackers can force unnecessary CPU and memory work.

**What a production fix should do**

- Move mutating actions to `POST` or a stricter admin RPC surface.
- Require admin auth for all control-plane mutations.
- Remove query-string transaction submission.
- Enforce byte limits before JSON parsing.
- Add rate limits or quotas for write-heavy public endpoints.

Example policy:

```text
GET  /readyz          safe read
GET  /tx-status/:id   safe read
POST /submit          public write with size limit and rate policy
POST /admin/commit    authenticated control-plane action
POST /admin/merge     authenticated control-plane action
```

### 15. Turn Stuck L1 Submission Into Explicit Degraded State

**Finding**

Pending block age currently comes from `updated_at`:

[`src/fibers/block-confirmation.ts:56`](./src/fibers/block-confirmation.ts#L56)

```ts
updatedAtMs:
  record[PendingBlockFinalizationsDB.Columns.UPDATED_AT].getTime(),
```

Some no-confirmation outcomes just break without changing readiness or latching
a degraded state:

[`src/fibers/block-confirmation.ts:332`](./src/fibers/block-confirmation.ts#L332)

```ts
case "NoTxForConfirmationOutput": {
  break;
}
case "FailedConfirmationOutput": {
  break;
}
```

**What this means**

The node can have an unresolved submitted L1 transaction, but still look healthy
from readiness. Also, using mutable `updated_at` can hide the true age of a
submission if status changes update the timestamp.

**Why this matters**

L1 submission is a critical progress point. If a submitted block is unresolved,
the node may be unable to safely commit more blocks or may need operator
intervention. A production node should not look ready while stuck.

**What a production fix should do**

Add immutable timestamps:

```sql
submitted_at TIMESTAMPTZ NOT NULL,
first_unresolved_at TIMESTAMPTZ,
last_checked_at TIMESTAMPTZ
```

Expose a latched degraded condition:

```json
{
  "ready": false,
  "reasons": ["pending_l1_submission_stale"],
  "pendingSubmission": {
    "txHash": "...",
    "ageMs": 900000
  }
}
```

The node should require explicit recovery or verified L1 confirmation before it
returns to ready.

### 16. Move Protocol Bootstrap Side Effects Out Of Hot Paths

**Finding**

The scheduler witness helper can mint and submit a scheduler witness token while
normal block-commitment work is running:

[`src/workers/utils/scheduler-refresh.ts:544`](./src/workers/utils/scheduler-refresh.ts#L544)

```ts
const ensureRealSchedulerWitnessUtxo = (...) =>
  Effect.gen(function* () {
    ...
    if (existingWitness !== undefined) {
      return existingWitness;
    }

    yield* Effect.logInfo(
      "Scheduler witness token ... missing; creating one ...",
    );
    ...
    const bootstrapTxHash = yield* handleSignSubmitNoConfirmation(...);
  });
```

**What this means**

Normal block production can trigger protocol bootstrapping if a required witness
is missing.

**Why this matters**

Hot paths should be predictable. Block commitment should commit blocks, not
discover missing protocol setup and mutate L1 to repair it. Hidden bootstrap
side effects make performance, failure modes, and operator expectations harder
to reason about.

**What a production fix should do**

Move this to startup invariant checks or explicit admin setup:

```text
midgard-node verify-protocol-invariants
midgard-node bootstrap-scheduler-witness
```

Startup should fail closed if the scheduler witness is missing:

```text
startup_failed: missing_scheduler_witness
```

Normal block commitment should assume required protocol objects already exist.

### 17. Enforce Production Key Separation

**Finding**

The reference-script seed phrase defaults to the operator seed:

[`src/services/config.ts:96`](./src/services/config.ts#L96)

```ts
const referenceScriptSeedPhrase = yield* Config.string(
  "L1_REFERENCE_SCRIPT_SEED_PHRASE",
).pipe(Config.withDefault(operatorSeedPhrase));
```

**What this means**

If `L1_REFERENCE_SCRIPT_SEED_PHRASE` is not configured, the node reuses the
operator seed for reference-script operations.

**Why this matters**

Production key roles should be separated. Reusing one seed for multiple roles
increases blast radius:

- compromise of one role compromises the other;
- accounting is harder;
- rotation is harder;
- operator policy is less explicit.

Security-sensitive defaults should force the operator to make an intentional
choice.

**What a production fix should do**

Require explicit configuration:

```ts
const referenceScriptSeedPhrase = yield* Config.string(
  "L1_REFERENCE_SCRIPT_SEED_PHRASE",
);
```

If same-wallet use is ever allowed for local development, it should be behind a
clearly named non-production flag, not the default.

## P2: Operational Maturity

### 18. Structured Logs, Request Correlation, And Durable Audit Records

**Finding**

Some logs are ad hoc and can include raw payload material. For example:

[`src/commands/listen-router.ts:157`](./src/commands/listen-router.ts#L157)

```ts
const txHashBytes = Buffer.from(fromHex(txHashParam));
yield* Effect.logInfo("txHashBytes", txHashBytes);
const foundCbor: Buffer = yield* MempoolDB.retrieveTxCborByHash(txHashBytes);
```

**What this means**

The node does not yet have a consistent correlation model from request receipt
through queueing, validation, commit, rejection, and finalization. Logs are
useful, but they are not enough as an audit trail for production.

**Why this matters**

When something goes wrong, operators need to answer:

- Which request introduced this transaction?
- Was it accepted durably?
- Which validation batch processed it?
- Which block included it?
- Was finalization attempted?
- Did a recovery job touch it?

Without correlation IDs and durable audit events, incident response becomes log
forensics.

**What a production fix should do**

Add stable IDs:

```text
request_id -> admission_id -> validation_batch_id -> block_header_hash
```

Emit structured logs:

```json
{
  "level": "info",
  "event": "tx_admitted",
  "request_id": "...",
  "tx_id": "...",
  "admission_status": "queued"
}
```

For destructive, recovery, retention, and migration actions, write durable audit
records, not only logs.

### 19. Secure Deployment Defaults

**Finding**

The sample env contains default database credentials and known seeds:

[`./.env.example:62`](./.env.example#L62)

```env
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
```

[`./.env.example:69`](./.env.example#L69)

```env
TESTNET_GENESIS_WALLET_SEED_PHRASE_A="..."
USER_SEED_PHRASE="..."
```

Config also defaults Postgres credentials:

[`src/services/config.ts:214`](./src/services/config.ts#L214)

```ts
const postgresPassword = yield* Config.string("POSTGRES_PASSWORD").pipe(
  Config.withDefault("postgres"),
);
```

The monitoring stack exposes risky defaults:

[`docker-compose.yaml:83`](./docker-compose.yaml#L83)

```yaml
prometheus:
  user: root
```

[`docker-compose.yaml:147`](./docker-compose.yaml#L147)

```yaml
- GF_AUTH_ANONYMOUS_ENABLED=true
- GF_AUTH_ANONYMOUS_ORG_ROLE=Admin
```

**What this means**

The default operational scaffolding is still demo-oriented.

**Why this matters**

Production incidents often start with defaults:

- known credentials copied into shared environments;
- anonymous admin dashboards;
- over-privileged containers;
- metrics/logging endpoints exposed too broadly;
- mutable image tags that make rollback hard.

For an L2, deployment reproducibility and least privilege are part of safety.

**What a production fix should do**

- Remove seeded secrets from default examples or mark them as local-only fixtures.
- Require explicit DB credentials in production.
- Disable anonymous Grafana admin.
- Avoid running containers as root where possible.
- Isolate Prometheus, Loki, Tempo, and Grafana to trusted networks.
- Pin images and document upgrade procedure.

### 20. Add Production-Grade Fault-Injection Tests

**Finding**

The highest-risk gaps involve partial failure:

- submit ack followed by crash;
- SQL write succeeds then MPT write fails;
- MPT write succeeds then SQL write fails;
- merge submitted but local finalization fails;
- startup sees incomplete recovery state.

These are not normal unit-test happy paths.

**What this means**

The node needs tests that intentionally break things between state-transition
steps and then restart or retry. This is fault-injection testing.

**Why this matters**

Production failures rarely happen at clean boundaries. Power loss, process
crashes, provider outages, disk errors, and database restarts can happen between
any two awaits.

If tests only cover the happy path, recovery logic may look correct but fail
under real operational pressure.

**What a production fix should do**

Add tests for:

```text
submit -> durable ack -> crash -> restart -> tx still queued
validate batch -> fail after mempool insert -> retry -> exact-once result
commit block -> fail after immutable insert -> restart -> recover deterministically
merge submit -> confirmation unknown -> no local finalization
rejected -> accepted/committed -> canonical status is correct
block tx order -> persist/retrieve -> exact order preserved
migration mismatch -> startup fails closed
provider outage -> readiness false
```

The important assertion is not just "eventually succeeds." It is "no duplicate,
lost, or contradictory durable state appears."

### 21. Promote Benchmarks Into Performance Gates

**Finding**

The throughput benchmark tooling has improved, but it is still primarily a
manual stress tool. The package scripts are in:

[`package.json`](./package.json)

The current stress script is:

[`scripts/throughput-valid-stress.mjs`](./scripts/throughput-valid-stress.mjs)

**What this means**

A benchmark is useful for local investigation, but a production performance gate
needs reproducibility:

- pinned environment;
- fixed node configuration;
- fixed transaction shape;
- warmup and measurement windows;
- baseline history;
- failure thresholds.

**Why this matters**

Throughput regressions are easy to introduce accidentally. Without a gate, the
team may discover regressions only after a larger integration or deployment.

For an L2, throughput matters, but it must be measured without weakening
correctness. Benchmark shortcuts must not become production defaults.

**What a production fix should do**

Create a benchmark profile:

```text
benchmark/preprod-local.env
benchmark/docker-compose.yaml
benchmark/baselines/main.json
benchmark/results/YYYY-MM-DD.json
```

Gate changes with thresholds:

```json
{
  "maxAcceptedTpsRegressionPct": 10,
  "maxP95LatencyRegressionPct": 15,
  "maxErrorRate": 0.001
}
```

Keep separate modes for:

- maximum admission TPS;
- maximum validation TPS;
- end-to-end accepted TPS;
- finalization-limited throughput.

Those are different bottlenecks and should not be collapsed into one number.

### 22. Write Real Operator Runbooks

**Finding**

There is a preprod deposit/send guide:

[`PREPROD_DEPOSIT_AND_SEND_TX.md`](./PREPROD_DEPOSIT_AND_SEND_TX.md)

But production operation needs more than happy-path transaction submission.

**What this means**

Runbooks are step-by-step operational procedures for normal and abnormal events.
They are part of production readiness because they define how humans safely
operate the system.

**Why this matters**

When a node is degraded, operators should not invent recovery steps under
pressure. Improvised recovery is how local state gets deleted, reset, replayed
incorrectly, or made inconsistent with L1.

**What a production fix should do**

Add runbooks for:

- initial bootstrap;
- protocol invariant verification;
- schema migration;
- upgrade and rollback;
- backup and restore;
- key rotation;
- reference-script deployment and verification;
- unresolved L1 submission;
- local finalization recovery;
- MPT rebuild;
- degraded readiness reasons;
- incident evidence collection.

Each runbook should include:

```text
When to use this
Prerequisites
Commands
Expected output
Abort conditions
Verification steps
Audit records produced
Rollback or escalation path
```

## Recommended Implementation Order

The highest-impact order is:

1. Durable, idempotent submission admission.
2. Versioned migrations, because many other fixes need schema changes.
3. Startup fail-closed integrity checks.
4. Readiness and degraded-state model.
5. Atomic/journaled local finalization for mempool, commit, and merge paths.
6. Merge confirmation failure fix.
7. Unsafe recovery/reset removal.
8. Canonical status and duplicate conflict handling.
9. Retention and auditability improvements.
10. Operational hardening, benchmarks, and runbooks.

This order keeps correctness first. It also avoids building new features on top
of state transitions that are not yet durable or recoverable.
