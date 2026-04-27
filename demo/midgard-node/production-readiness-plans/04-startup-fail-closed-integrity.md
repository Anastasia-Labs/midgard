# P0 Blocker 4: Fail Closed On Startup Reconciliation And Integrity Failures

## Problem Statement

Startup is the last safe point before the node accepts L2 traffic and starts
mutating local protocol state. Today, several startup checks are best-effort:
deposit catch-up and deposit projection failures are logged and suppressed, and
the HTTP server plus background fibers still start. A production L2 cannot admit
transactions, build block roots, or perform local finalization when the node has
not proven that local SQL state, LevelDB-backed MPT state, L1 state, deposit
state, and pending recovery journals agree.

The production behavior must be fail closed by default:

- If startup cannot prove state integrity, the node must not serve `/submit` or
  run mutating background fibers.
- If an integrity check fails, the operator must receive a precise error code,
  evidence, and a recovery path.
- Startup may perform deterministic, idempotent reconciliation only when the
  source of truth is available and the reconciliation result is verified before
  serving.
- Startup must never silently rewrite data, reseed state, delete MPT stores, or
  continue from ambiguous state.

This plan is intentionally limited to implementation planning. It does not
implement code.

## Current Behavior

`runNode` creates an in-memory submit queue before startup checks:
[`src/commands/listen.ts:63`](../src/commands/listen.ts#L63).
That queue is not externally reachable until the HTTP router is bound, but the
production startup gate should still be evaluated before constructing admission
plumbing so a failed node has no usable submission surface.

Database initialization and the current startup checks run before the HTTP
server:
[`src/commands/listen.ts:69`](../src/commands/listen.ts#L69).
This initialization is schema-mutating: `InitDB.program` executes
`CREATE TABLE IF NOT EXISTS`, index creation, and index drop/recreate statements
during normal `listen` startup:
[`src/database/init.ts:21`](../src/database/init.ts#L21).
That behavior is incompatible with fail-closed startup because it can mask an
unversioned, partially migrated, or drifted database before integrity checks
inspect it.

Protocol initialization and topology checks are already mandatory because
`ensureProtocolInitializedOnStartup` logs and dies on failure:
[`src/commands/listen-startup.ts:91`](../src/commands/listen-startup.ts#L91)
and
[`src/commands/listen-startup.ts:152`](../src/commands/listen-startup.ts#L152).

The local block boundary is seeded from the current state-queue tip and, when
the tip header has a pending-finalization journal, from that journal's block end
time:
[`src/commands/listen-startup.ts:165`](../src/commands/listen-startup.ts#L165).
This is mandatory via `Effect.orDie`:
[`src/commands/listen-startup.ts:202`](../src/commands/listen-startup.ts#L202).

Pending finalization hydration loads at most one active journal into process
refs:
[`src/commands/listen-startup.ts:211`](../src/commands/listen-startup.ts#L211).
It sets `LOCAL_FINALIZATION_PENDING` for `pending_submission`,
`submitted_local_finalization_pending`, and `observed_waiting_stability`:
[`src/commands/listen-startup.ts:235`](../src/commands/listen-startup.ts#L235).
It is also mandatory via `Effect.orDie`:
[`src/commands/listen-startup.ts:252`](../src/commands/listen-startup.ts#L252).

Startup deposit catch-up is not mandatory. It logs a warning and suppresses all
errors:
[`src/commands/listen.ts:73`](../src/commands/listen.ts#L73).

Startup deposit projection reconciliation is also not mandatory. It logs a
warning and suppresses all errors:
[`src/commands/listen.ts:81`](../src/commands/listen.ts#L81).

The steady-state deposit fetch fiber also suppresses failures:
[`src/fibers/fetch-and-insert-deposit-utxos.ts:257`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L257).
The steady-state deposit projector does the same:
[`src/fibers/project-deposits-to-mempool-ledger.ts:121`](../src/fibers/project-deposits-to-mempool-ledger.ts#L121).
That is acceptable for a periodic worker only if readiness and startup integrity
surface the degraded state; it is not acceptable during startup.

Deposit fetch reconciles the full visible deposit UTxO set from L1:
[`src/fibers/fetch-and-insert-deposit-utxos.ts:151`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L151)
and converts each L1 deposit into the local `deposits_utxos` row shape:
[`src/fibers/fetch-and-insert-deposit-utxos.ts:94`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L94).
The insert path is strict about duplicate event IDs with conflicting payloads:
[`src/database/deposits.ts:97`](../src/database/deposits.ts#L97).

Deposit projection has useful integrity checks already. Projected deposits must
match their existing `mempool_ledger` row by tx id, outref, output, address, and
source event id:
[`src/fibers/project-deposits-to-mempool-ledger.ts:7`](../src/fibers/project-deposits-to-mempool-ledger.ts#L7).
Mismatched projected payloads fail:
[`src/fibers/project-deposits-to-mempool-ledger.ts:62`](../src/fibers/project-deposits-to-mempool-ledger.ts#L62).
Awaiting deposits are projected inside a SQL transaction:
[`src/fibers/project-deposits-to-mempool-ledger.ts:78`](../src/fibers/project-deposits-to-mempool-ledger.ts#L78).

`mempool_ledger` enforces one row per outref and one row per deposit
`source_event_id`:
[`src/database/mempoolLedger.ts:54`](../src/database/mempoolLedger.ts#L54).
Its strict deposit reconcile path fails when an existing source-event row has a
different payload:
[`src/database/mempoolLedger.ts:141`](../src/database/mempoolLedger.ts#L141).

The MPT helper opens persistent ledger and mempool tries from configured paths:
[`src/workers/utils/mpt.ts:86`](../src/workers/utils/mpt.ts#L86).
If the ledger trie root is empty, it currently inserts configured genesis UTxOs:
[`src/workers/utils/mpt.ts:100`](../src/workers/utils/mpt.ts#L100).
For production startup this is dangerous unless proven to be the first
deployment state, because an empty or missing trie can mean data loss.

Block construction mutates MPT roots and writes the new transaction and UTxO
roots:
[`src/workers/utils/mpt.ts:411`](../src/workers/utils/mpt.ts#L411).
It already refuses to build a block past the deposit visibility barrier:
[`src/workers/utils/mpt.ts:350`](../src/workers/utils/mpt.ts#L350).

Pending finalization journals are durable and constrained to one active record:
[`src/database/pendingBlockFinalizations.ts:108`](../src/database/pendingBlockFinalizations.ts#L108).
The active journal is used by the confirmation fiber:
[`src/fibers/block-confirmation.ts:195`](../src/fibers/block-confirmation.ts#L195).
Confirmed pending blocks update deposit header assignment and pending status:
[`src/fibers/block-confirmation.ts:87`](../src/fibers/block-confirmation.ts#L87).

Local commit finalization mutates immutable transactions, block links,
processed/mempool rows, and the mempool trie as separate effects:
[`src/workers/utils/commit-submission.ts:38`](../src/workers/utils/commit-submission.ts#L38).
Successful submission then marks included deposits and the pending journal:
[`src/workers/utils/commit-submission.ts:163`](../src/workers/utils/commit-submission.ts#L163).
Recovery completion marks the pending journal finalized:
[`src/workers/utils/commit-submission.ts:203`](../src/workers/utils/commit-submission.ts#L203).

Readiness currently checks database connectivity, worker heartbeats, queue
depth, and `LOCAL_FINALIZATION_PENDING`:
[`src/commands/readiness.ts:37`](../src/commands/readiness.ts#L37).
The `/readyz` route wires those inputs:
[`src/commands/listen-router.ts:440`](../src/commands/listen-router.ts#L440).
It has no startup-integrity status input.

## Target Startup Invariants

Startup must complete a single ordered gate before HTTP serving and before any
mutating background fiber starts:

```text
open DB connection without application DDL
verify schema/migration state
verify protocol deployment and reference scripts
verify L1 state-queue topology and current canonical root/tail
verify/hydrate pending finalization journal
reconcile visible stable deposits from L1 into SQL
reconcile due/projected deposits into mempool_ledger
verify SQL cross-table invariants
verify SQL roots against persistent MPT roots
verify local roots against L1 state-queue root/tail and pending journal
publish startup_integrity_status=ok
construct submit queue and admission router
start HTTP server and worker fibers
```

The required invariants are:

1. The configured database schema is exactly the expected production schema for
   this binary before any application table is read or written. Missing
   columns, indexes, constraints, migration version, or checksum are startup
   failures.
2. The configured protocol deployment is complete and the state-queue topology
   is healthy.
3. Runtime reference scripts required by the node are present and match the
   configured contract bundle.
4. The L1 provider is reachable enough to fetch the current state queue and the
   visible stable deposit set. Provider uncertainty is not a warning at startup;
   it blocks serving.
5. The pending-finalization journal is internally consistent and externally
   consistent with L1:
   - zero or one active pending journal;
   - active member rows exist and are ordered deterministically;
   - active journal header hash, submitted tx hash, block end time, and status
     are coherent;
   - if L1 already contains the journal header, startup marks the node as
     needing explicit local-finalization recovery rather than accepting traffic;
   - if L1 proves the journal is stale or absent past the configured stability
     window, startup fails closed and requires explicit recovery or abandon.
6. The latest local block boundary equals the canonical L1 state-queue tail end
   time except while an active pending-finalization journal requires a more
   conservative boundary.
7. Every stable visible L1 deposit exists in `deposits_utxos` with byte-identical
   derived payload, or startup inserts it idempotently and verifies the insert.
8. No persisted deposit event ID maps to conflicting info, inclusion time,
   Cardano tx hash, L2 tx id, output, or L2 address.
9. Every `projected` deposit has exactly one matching `mempool_ledger` row with
   the same source event id and payload unless it is assigned to a finalized
   block and no longer expected in mempool pre-state.
10. Every `awaiting` deposit due by the startup deposit visibility barrier is
    projected before serving, and projection is verified.
11. No deposit assigned to a header is assigned to multiple headers or to a
    header not represented by a valid pending/finalized block path.
12. Volatile transaction tables are mutually coherent:
    - a tx id cannot be in `mempool`, `processed_mempool`, `immutable`, and
      `tx_rejections` in contradictory states;
    - each `blocks` tx link points to exactly one immutable tx;
    - processed payloads waiting for confirmation are represented in the MPT
      and by the pending context that needs them.
13. SQL ledger tables and persistent MPT roots agree with their explicit
    authorities:
    - `confirmed_ledger` root matches the confirmed-state `utxoRoot` in the L1
      state-queue root node;
    - persistent `ledger` MPT root matches the L1 state-queue tail header's
      `utxosRoot`, or the confirmed-state `utxoRoot` when the queue is empty,
      with pending-journal-specific recovery targets handled explicitly;
    - `latest_ledger` is either removed from startup trust or verified only as a
      derived cache with an exact source root;
    - `mempool_ledger` is verified as the SQL admission pre-state and is not
      treated as an L1 block root;
    - persistent `mempool` trie root matches `processed_mempool` when that trie
      is preserving a deferred transaction root, otherwise it must be empty or
      rebuildable from a verified journal.
14. Empty MPT stores are accepted only for a proven fresh deployment. If L1,
    SQL, or pending journals indicate prior state, an empty trie is a data-loss
    signal and startup fails closed.
15. No check relies on legacy IDs, fallback schemas, compatibility modes, or
    operator toggles that preserve old behavior in `demo/midgard-node`.
16. A failed startup leaves a durable or exported failure report, but performs
    no application DDL, no MPT reseed, no destructive reset, and no admission
    queue exposure.

## Integrity Checks To Add

### Startup Gate Module

Add a startup integrity module under `src/commands/listen-startup.ts` or a new
nearby module such as `src/commands/startup-integrity.ts`. It should expose one
effect used by `runNode`:

```ts
yield* verifyStartupIntegrityOrFail;
```

The implementation should return a structured success record containing:

- schema version/fingerprint;
- L1 state-queue root/tail hash, header hash, end time, and slot/time evidence;
- deposit reconciliation barrier;
- SQL root summaries;
- MPT root summaries;
- pending-finalization journal summary;
- reconciliation mutations applied during startup.

It should fail with a typed `StartupIntegrityError` that includes:

- stable `code`;
- `severity: "fatal" | "degraded"`;
- `component`;
- human message;
- machine-readable evidence;
- recommended recovery command(s);
- whether admission, read APIs, and background fibers are allowed.

### SQL And Schema Checks

The startup gate must verify schema before trusting table contents. For
production `listen`, replace `InitDB.program` with a compatibility assertion
from P0 blocker 5. The assertion opens a DB connection, verifies the migration
ledger, expected version, migration checksums, and application-schema
fingerprint, and then returns without running application DDL.

Only explicit migration commands may create or alter application schema. The
metadata table bootstrap exception from P0 blocker 5 is allowed only inside the
migration runner and schema checker, never as a hidden table-creation path for
application tables.

Until P0 blocker 5 lands, this plan may include a stopgap
`assertCurrentSchemaFingerprint` used by `listen`, but it must be read-only. It
must fail if the database is empty, unversioned, missing tables, or structurally
different. It must not call existing `createTable` helpers because several of
those helpers run `CREATE TABLE IF NOT EXISTS` and index drop/recreate DDL.

The read-only assertion must introspect the exact expected tables, columns,
constraints, unique indexes, and foreign keys used by this plan:

- `deposits_utxos` status check and awaiting/header invariant:
  [`src/database/deposits.ts:55`](../src/database/deposits.ts#L55).
- `mempool_ledger` primary key, source-event foreign key, and source-event
  uniqueness:
  [`src/database/mempoolLedger.ts:54`](../src/database/mempoolLedger.ts#L54).
- pending-finalization tables, statuses, member foreign keys, and single-active
  unique index:
  [`src/database/pendingBlockFinalizations.ts:108`](../src/database/pendingBlockFinalizations.ts#L108).
- ledger-style table primary key on `outref`:
  [`src/database/utils/ledger.ts:46`](../src/database/utils/ledger.ts#L46).

Failures:

- `startup_failed:schema_version_missing`
- `startup_failed:schema_fingerprint_mismatch`
- `startup_failed:constraint_missing`

Recovery:

- Run the explicit migration command once P0 blocker 5 exists.
- Do not auto-create, auto-alter, or silently reshape production schema during
  `listen` startup.
- For an empty fresh database, run the migration runner first. Do not let
  `listen` bootstrap tables as a convenience.

### L1 Protocol Checks

Reuse `ensureProtocolInitializedOnStartup`, but include its result in the
startup integrity report instead of treating it as an isolated preflight. Verify:

- state-queue topology is initialized, exactly one root and tail, and healthy;
- configured validators match the fetched authenticated UTxOs;
- required runtime reference scripts are present and match expected script
  hashes;
- current state-queue root and tail datums can be decoded, and the tail header
  hash/end time can be recomputed deterministically when the queue is non-empty.

Failures:

- `startup_failed:l1_state_queue_unhealthy`
- `startup_failed:l1_state_queue_tip_decode_failed`
- `startup_failed:reference_scripts_missing`
- `startup_failed:contract_bundle_mismatch`

Recovery:

- Fix deployment configuration or run explicit initialization/reference-script
  publication commands. Do not start the node against partial or ambiguous L1
  state.

### Deposit Ingestion Checks

Startup must make `fetchAndInsertDepositUTxOs` mandatory. Remove the startup
`Effect.catchAll(() => Effect.void)` wrappers in `listen.ts` and call a strict
startup-specific reconciliation function.

Do not reuse the steady-state function without changes: the current helper does
not persist `deposit_ingestion_cursor`, does not re-read and byte-compare every
fetched event, and does not expose the stable L1 barrier needed to make startup
projection reproducible.

The strict function should:

1. Resolve a stable deposit visibility barrier from L1 before the fetch. The
   barrier must include provider identity, stable tip hash, stable slot, stable
   time, and scan upper-bound time. It must be derived from configured
   finality/stability policy, not from unbounded wall-clock `new Date()`.
2. Fetch the visible stable deposit UTxO set from L1 using the configured
   deposit validator and policy, bounded by the barrier:
   [`src/fibers/fetch-and-insert-deposit-utxos.ts:151`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L151).
3. Capture the L1 evidence used for the fetch: provider, stable tip hash, stable
   slot, stable time, scan upper bound, and fetched event count. The existing
   `deposit_ingestion_cursor` table already has fields for this evidence:
   [`src/database/depositIngestionCursor.ts:41`](../src/database/depositIngestionCursor.ts#L41).
4. Convert every fetched UTxO to the exact SQL row shape:
   [`src/fibers/fetch-and-insert-deposit-utxos.ts:94`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L94).
5. Insert missing entries idempotently using the strict conflict behavior in
   `DepositsDB.insertEntries`:
   [`src/database/deposits.ts:97`](../src/database/deposits.ts#L97).
6. Re-read all fetched event IDs and byte-compare persisted values to derived
   values.
7. Advance the ingestion cursor only after the re-read succeeds. Cursor advance
   must be monotonic by stable slot and scan upper-bound time. A cursor rollback,
   provider fork behind the recorded stable slot, or scan upper-bound regression
   fails startup unless an explicit reorg recovery command has produced an
   audited rewind plan.
8. Return the verified barrier to the startup gate so deposit projection uses
   exactly the same bound.

Failures:

- L1 fetch/provider failure:
  `startup_failed:deposit_l1_fetch_unavailable`
- deposit auth NFT invariant failure:
  `startup_failed:deposit_auth_nft_invalid`
- conflicting persisted payload:
  `startup_failed:deposit_payload_conflict`
- cursor regression or impossible cursor jump:
  `startup_failed:deposit_cursor_inconsistent`

Recovery:

- Fix the L1 provider or contract configuration and restart.
- For payload conflicts, stop and investigate. The recovery command may produce
  a read-only diff, but it must not rewrite the row without an explicit audited
  operator decision.

### Deposit Projection Checks

Startup must make `projectDepositsToMempoolLedger` mandatory and then verify its
result. The current reconciliation already catches missing projected rows and
payload mismatches:
[`src/fibers/project-deposits-to-mempool-ledger.ts:27`](../src/fibers/project-deposits-to-mempool-ledger.ts#L27).

Do not reuse the current unbounded `projectAwaitingDeposits` behavior as-is. It
selects due deposits with `new Date()`:
[`src/fibers/project-deposits-to-mempool-ledger.ts:82`](../src/fibers/project-deposits-to-mempool-ledger.ts#L82).
Startup projection must accept the verified deposit barrier from ingestion and
use that fixed time for all reads, writes, and verification.

The strict startup projection should:

1. Reconcile already `projected` deposits to `mempool_ledger`.
2. Project all `awaiting` deposits due by the startup deposit visibility
   barrier, not by an unbounded `new Date()` that cannot be reproduced later.
3. Re-read:
   - all projected deposits;
   - all due awaiting deposits;
   - all `mempool_ledger` rows with non-null `source_event_id`.
4. Verify every due projected deposit has exactly one matching
   `mempool_ledger` row.
5. Verify there are no orphan `mempool_ledger.source_event_id` rows.
6. Verify no deposit-origin outref collides with a different non-deposit
   `mempool_ledger` row.
7. Increment `MEMPOOL_LEDGER_VERSION` only after the entire startup projection
   verifies:
   [`src/fibers/project-deposits-to-mempool-ledger.ts:115`](../src/fibers/project-deposits-to-mempool-ledger.ts#L115).

Failures:

- `startup_failed:deposit_projection_incomplete`
- `startup_failed:deposit_projection_payload_mismatch`
- `startup_failed:deposit_projection_orphan_source_event`
- `startup_failed:deposit_projection_outref_collision`

Recovery:

- Run a dry-run projection diff command.
- Run an explicit `project-deposits-once --strict --verify` recovery command
  only when it reports deterministic idempotent inserts/updates.
- Restart the node after the command verifies success.

### Pending Finalization Checks

Hydration must become a verification plus hydration step. It should not only
copy SQL values into refs.

Checks:

- Retrieve all active pending-finalization rows, not just the first row, and
  fail if more than one exists even if the index should prevent it.
- Validate member tables:
  - ordinals are contiguous from zero;
  - no duplicate deposit event IDs or tx IDs in a journal;
  - every deposit member exists in `deposits_utxos`;
  - every tx member exists in the correct local source table for the journal
    stage.
- Validate status-specific required fields:
  - `pending_submission` has no submitted tx hash;
  - `submitted_local_finalization_pending`, `submitted_unconfirmed`, and
    `observed_waiting_stability` have or can recover a submitted tx hash;
  - `observed_waiting_stability` has `observed_confirmed_at_ms`.
- Fetch L1 state-queue nodes and compare the active journal header hash with
  on-chain evidence.
- If the active journal header is confirmed on L1 but local finalization is not
  complete, set `LOCAL_FINALIZATION_PENDING=true`, set
  `AVAILABLE_LOCAL_FINALIZATION_BLOCK`, and return a startup result that allows
  only explicit recovery execution. Default `listen` must still refuse
  admission and mutating fibers until local finalization completes.
- If L1 does not contain the active journal header and the submission is still
  within the confirmation window, set unconfirmed refs and keep readiness false.
- If L1 proves the pending journal stale, fail startup with an explicit stale
  pending-journal error. Do not abandon automatically during startup.

Failures:

- `startup_failed:pending_journal_duplicate_active`
- `startup_failed:pending_journal_member_missing`
- `startup_failed:pending_journal_status_invalid`
- `startup_failed:pending_journal_l1_mismatch`
- `startup_failed:pending_journal_stale_requires_operator`

Recovery:

- `node dist/index.js pending-finalization inspect --json`
- `node dist/index.js pending-finalization recover-local --header-hash <hex>`
- `node dist/index.js pending-finalization abandon --header-hash <hex> --evidence <file>`

The abandon command must require L1 evidence that the header is not canonical and
must write an audit record before changing state.

### Startup Ordering And Runtime Wiring

`runNode` must be rewired so the fail-closed decision is structurally before any
serving or worker construction:

1. Build the DB/L1/config service layers.
2. Run schema compatibility assertion without application DDL.
3. Run `verifyStartupIntegrityOrFail`.
4. Set startup integrity refs to `ok` only after every check and allowed
   reconciliation verifies.
5. Construct the submit queue and HTTP router.
6. Start the HTTP server and background fibers.

If `verifyStartupIntegrityOrFail` fails in default mode:

- do not construct the submit queue;
- do not bind the HTTP server;
- do not launch genesis, commitment, confirmation, merge, deposit, projection,
  retention, monitoring, or tx-queue fibers;
- emit the structured failure report and exit non-zero.

If diagnostic read-only mode is explicitly enabled, construct a separate
read-only router. Do not pass a live submit queue into normal mutating routes
and then rely only on route-level conditionals.

### SQL Cross-Table Checks

Add read-only SQL checks for contradictory transaction and ledger state:

- `blocks` rows must reference existing immutable tx rows. The existing
  `audit-blocks-immutable` command starts this surface:
  [`src/index.ts:640`](../src/index.ts#L640).
- `immutable` tx IDs must not appear in `mempool` or `processed_mempool`.
- `tx_rejections` must not contradict immutable inclusion for the same tx id.
- `processed_mempool` rows must be allowed only when there is a deferred
  commitment context that can explain the persistent mempool trie root.
- `mempool_tx_deltas` rows must reference a tx in `mempool`,
  `processed_mempool`, or `immutable` according to the lifecycle stage.
- `deposits_utxos.projected_header_hash` must refer to either the active
  pending-finalization journal, a finalized/known block header, or be null.
- `consumed` deposits must be assigned to a valid header; `awaiting` deposits
  must have `projected_header_hash IS NULL`.

Failures:

- `startup_failed:block_immutable_link_broken`
- `startup_failed:tx_state_contradiction`
- `startup_failed:processed_payload_without_context`
- `startup_failed:mempool_delta_orphan`
- `startup_failed:deposit_header_assignment_invalid`

Recovery:

- Run read-only audit commands first.
- Repairs must be explicit, narrow, evidence-backed, and logged. Do not run
  automatic repairs during normal startup.

### MPT And Root Checks

The startup gate must open the persistent MPT stores in read/verify mode before
any worker mutates them. The `makeMpts` behavior that auto-inserts genesis when
the ledger trie is empty must be split:

- Fresh deployment path: allowed only when L1 state queue is empty/genesis,
  SQL ledger tables are empty, pending journals are empty, and explicit
  initialization policy allows bootstrapping.
- Existing deployment path: empty or missing ledger trie is a fatal integrity
  error.

Root authority is defined as follows:

- The L1 state-queue root node's confirmed-state datum is authoritative for
  `confirmed_ledger`. Rebuild a SQL root from `confirmed_ledger` and compare it
  with that datum's `utxoRoot`. If the confirmed-state datum is unavailable or
  undecodable, fail startup.
- The L1 state-queue tail is authoritative for the persistent `ledger` MPT
  pre-state used for the next block. If the queue has at least one committed
  block, compare the persistent ledger MPT root with the tail header's
  `utxosRoot`. If the queue is empty, compare it with the confirmed-state
  `utxoRoot`.
- During an active pending-finalization journal, compare against the
  status-specific recovery target: the journal header for locally prepared or
  submitted state, and the confirmed on-chain header when L1 already contains
  it. Ambiguous status-to-root mapping is a startup failure, not a warning.
- `latest_ledger` is not startup authority. Either remove it from the trusted
  path in the implementation or define and verify it as a derived cache with a
  precise source root. A mismatch in a derived cache must not cause startup to
  trust the cache over L1, SQL, or the persistent MPT.
- The persistent `mempool` MPT is authoritative only for deferred processed
  transaction bytes that are still required to match a pending or unconfirmed
  block's transaction root. Otherwise it must be empty or absent after verified
  local finalization.
- `mempool_ledger` is the SQL admission pre-state. Rebuild its root for
  observability and compare it with any declared admission-cache root, but do
  not confuse it with the block transaction root stored in the mempool MPT.

Root verification should use canonical ordering and deterministic encoding:

1. Rebuild a temporary root from SQL entries using raw outref bytes as keys and
   raw output bytes as values. Sort keys lexicographically before building to
   make the root computation auditable even if the MPT implementation is order
   independent.
2. Compare the computed confirmed/admission roots with the relevant SQL tables
   and compare persistent roots with `ledgerTrie.getRootHex()` and
   `mempoolTrie.getRootHex()`:
   [`src/workers/utils/mpt.ts:770`](../src/workers/utils/mpt.ts#L770).
3. Compare the ledger root with the root represented by the current L1
   state-queue tail or by the active pending journal's confirmed recovery block.
4. Compare the mempool transaction root with `processed_mempool` when the
   mempool trie is intentionally preserving deferred processed txs. Otherwise
   require the mempool trie to be empty after verified local finalization.

Failures:

- `startup_failed:ledger_mpt_missing`
- `startup_failed:ledger_mpt_root_mismatch`
- `startup_failed:mempool_mpt_root_mismatch`
- `startup_failed:l1_root_mismatch`
- `startup_failed:unexpected_genesis_reseed_required`
- `startup_failed:root_authority_ambiguous`

Recovery:

- `node dist/index.js mpt verify --json`
- `node dist/index.js mpt rebuild --from-sql --dry-run --write-audit <path>`
- `node dist/index.js mpt rebuild --from-sql --apply --audit <path>`

The apply command must:

- require a prior dry-run audit file;
- back up the old LevelDB directory;
- rebuild into a new directory;
- verify roots against SQL and L1;
- atomically swap directories;
- write an immutable recovery manifest with old root, new root, L1 evidence,
  schema version, operator identity, and command arguments.

## Fail-Closed And Degraded Semantics

Default behavior is hard fail:

- The process exits non-zero before binding the HTTP port.
- No submit queue is exposed.
- No block commitment, confirmation, merge, deposit fetch, deposit projection,
  retention, monitoring, or tx processor fibers start.
- Logs contain a single top-level startup failure summary plus structured
  details for every failed check.

Read-only degraded mode may be added only as an explicit diagnostic/recovery
mode, not as default behavior and not as a compatibility shim. It must be named
clearly, for example:

```text
MIDGARD_STARTUP_FAILURE_MODE=diagnostic_readonly
```

In diagnostic read-only mode:

- `/healthz` may return 200 because the process is alive.
- `/readyz` must return 503 with `startup_integrity_failed:<code>`.
- `/submit`, `/deposit/build` if it depends on local state, admin mutation
  routes, and all worker fibers must be disabled.
- Read-only inspection endpoints may return state plus the startup failure
  report.
- The process must expose that it is not participating as an L2 node.
- Recovery commands may be invoked out of band; the degraded HTTP process must
  not perform recovery mutations itself unless a separate authenticated admin
  command path is explicitly designed, audited, and disabled by default.

Do not add a mode that accepts submissions while startup integrity is failed.
Do not add fallback/legacy behavior for old IDs or old schema shapes.

## Operator-Visible Errors

Startup errors should be stable and easy to route in logs, metrics, and runbooks.
Use one top-level format:

```json
{
  "event": "startup_integrity_failed",
  "code": "startup_failed:deposit_projection_payload_mismatch",
  "component": "deposit_projection",
  "fatal": true,
  "admission_allowed": false,
  "background_fibers_allowed": false,
  "evidence": {
    "event_id": "<hex>",
    "expected_outref": "<hex>",
    "actual_outref": "<hex>"
  },
  "recovery": [
    "node dist/index.js deposit-reconcile --dry-run --event-id <hex>",
    "node dist/index.js project-deposits-once --strict --verify"
  ]
}
```

Each error must include enough evidence for an operator to confirm the failure
without rerunning the node in a debugger:

- table names and primary keys;
- event IDs, tx hashes, header hashes, and outrefs in hex;
- expected and actual roots;
- L1 state-queue root/tail hashes and slot/time;
- migration/schema version;
- provider used for L1 evidence;
- whether the failure is replayable/idempotent or requires manual
  investigation.

## Recovery Commands

Add or harden these commands as part of implementation:

- `startup-integrity check --json`: run the exact startup gate without starting
  HTTP or fibers.
- `deposit-reconcile --dry-run --json`: fetch stable L1 deposits, derive local
  rows, and report missing/conflicting/projected state.
- `deposit-reconcile --apply --verify`: apply only deterministic missing-row
  reconciliation and verify by re-read.
- `project-deposits-once --strict --verify`: make the existing command from
  [`src/index.ts:622`](../src/index.ts#L622) fail on any incomplete projection
  or payload mismatch.
- `pending-finalization inspect --json`: show active journal, member rows, L1
  evidence, and recommended action.
- `pending-finalization recover-local --header-hash <hex>`: complete local
  finalization only against confirmed L1 evidence.
- `pending-finalization abandon --header-hash <hex> --evidence <file>`: abandon
  only after explicit stale evidence.
- `mpt verify --json`: compare SQL-derived temporary roots, persistent MPT
  roots, and L1 roots.
- `mpt rebuild --from-sql --dry-run --write-audit <path>` and `--apply`: rebuild
  MPT state only through an auditable backup/swap flow.
- `audit-blocks-immutable --json`: expose the existing audit command in a form
  startup and runbooks can reference.

Recovery commands must be deterministic, idempotent where possible, and explicit
about which stores they modify. Commands that modify state must support dry-run
and write an audit manifest.

## Observability And Readiness

Add process-level startup integrity state to `Globals` or a dedicated service:

- `STARTUP_INTEGRITY_STATUS`: `starting | ok | failed | degraded`
- `STARTUP_INTEGRITY_FAILURE_CODE`
- `STARTUP_INTEGRITY_REPORT_JSON`
- `STARTUP_INTEGRITY_COMPLETED_AT_MS`

Extend `evaluateReadiness`:
[`src/commands/readiness.ts:37`](../src/commands/readiness.ts#L37)
with startup integrity input. Readiness must be false unless startup integrity
is `ok`.

Extend `/readyz`:
[`src/commands/listen-router.ts:440`](../src/commands/listen-router.ts#L440)
to return:

- startup status;
- startup failure code and component;
- local finalization pending state;
- root summaries;
- deposit cursor/barrier summary;
- current queue depth and heartbeats.

Metrics:

- `midgard_startup_integrity_status{status,code}`
- `midgard_startup_integrity_check_duration_ms{check}`
- `midgard_startup_integrity_failures_total{code,component}`
- `midgard_deposit_reconciliation_visible_count`
- `midgard_deposit_reconciliation_inserted_count`
- `midgard_deposit_projection_reconciled_count`
- `midgard_sql_mpt_root_match{root="confirmed|ledger|mempool_tx|admission"}`
- `midgard_pending_finalization_active{status}`
- `midgard_startup_l1_tail_slot`

Tracing:

- one root span `startup.integrity`;
- child spans per check;
- include hashes and counts as attributes, not large payloads.

Logging:

- log a concise success summary once;
- log a structured fatal failure once;
- avoid continuing warning spam after a fatal startup decision.

## Tests And Fault Injection

Unit tests:

- `evaluateReadiness` returns not ready when startup status is not `ok`.
- `StartupIntegrityError` serializes stable code, component, evidence, and
  recovery commands.
- Root computation sorts keys canonically and is deterministic.
- Deposit projection verifier rejects mismatched payloads, orphan source events,
  duplicate source events, and outref collisions.
- Pending-journal verifier rejects invalid status/field combinations and
  non-contiguous member ordinals.

Database integration tests with temporary Postgres:

- Missing `deposits_utxos` row for fetched L1 deposit is inserted and verified.
- Conflicting persisted deposit payload fails startup.
- `projected` deposit missing from `mempool_ledger` is repaired by strict
  projection and verified.
- Existing `mempool_ledger` row with same `source_event_id` but different
  outref/output fails startup.
- `awaiting` deposit due before barrier remains awaiting after projection
  attempt and fails startup.
- Orphan `mempool_ledger.source_event_id` fails startup.
- Deposit assigned to unknown header fails startup.
- Tx present in both `immutable` and `mempool` fails startup.
- `blocks` row referencing missing immutable tx fails startup.
- Active pending journal with missing member rows fails startup.
- Duplicate active pending journals fail startup, even if created by bypassing
  the production constraint in test setup.

MPT fault-injection tests with temporary LevelDB directories:

- Missing ledger MPT on non-fresh deployment fails startup.
- Empty ledger trie with non-empty SQL or non-empty L1 state fails startup.
- Confirmed SQL root differs from the L1 root-node confirmed-state root and
  fails startup.
- Persistent ledger root differs from the L1 tail root and fails startup.
- Mempool trie root is non-empty without processed/deferred context and fails
  startup.
- Rebuild dry-run produces old/new root evidence without mutating existing MPT.

L1/provider fault-injection tests:

- Deposit fetch provider failure fails startup before HTTP bind.
- State-queue fetch provider failure fails startup before HTTP bind.
- L1 state-queue topology unhealthy fails startup.
- Active pending journal found confirmed on L1 starts only in recovery/degraded
  semantics and refuses readiness until local finalization completes.
- Active pending journal proven stale requires explicit abandon recovery.

End-to-end startup tests:

- `runNode` with failing deposit reconciliation does not bind the HTTP port.
- `runNode` with failing projection reconciliation does not bind the HTTP port.
- In diagnostic read-only mode, `/healthz` returns 200, `/readyz` returns 503,
  `/submit` returns 503, and no mutating fibers run.
- Successful startup publishes `startup_integrity_status=ok` and then starts
  fibers.

Regression tests:

- No startup path contains `Effect.catchAll(() => Effect.void)` around mandatory
  reconciliation.
- No production path uses `.complete({ localUPLCEval: false })`.
- No demo/midgard-node compatibility or legacy-ID fallback is introduced.

## Rollout Steps

1. Add typed startup integrity errors and report serialization without changing
   behavior. Cover serialization and readiness unit tests.
2. Replace `listen`'s direct `InitDB.program` dependency with a read-only schema
   compatibility assertion. Keep table creation available only through the
   explicit migration path from P0 blocker 5.
3. Move submit-queue construction and normal router construction after a
   successful startup integrity gate.
4. Add `startup-integrity check --json` that runs read-only checks and returns
   non-zero on failure. Use it in CI and local runbooks.
5. Make startup deposit catch-up strict. Remove the suppressing catch in
   `listen.ts` and prove provider failure prevents HTTP bind.
6. Make startup deposit projection strict. Remove the suppressing catch in
   `listen.ts` and verify due/projected deposits before serving.
7. Add SQL cross-table integrity checks and fail startup on contradictions.
8. Split `makeMpts` into explicit fresh-deployment initialization and existing
   deployment verification. Prevent automatic genesis reseed on existing state.
9. Add SQL-derived temporary root verification for confirmed/admission ledgers
   and persistent ledger/mempool MPTs.
10. Add L1 root comparison and pending-finalization verification.
11. Extend readiness and metrics with startup integrity status.
12. Add recovery commands with dry-run, audit manifests, and apply flows.
13. Enable fail-closed startup as the default in all environments.
14. Document operator runbooks and expected error codes.

Each step should include tests before enabling the next enforcement point.
Temporary diagnostic controls must be explicitly named, non-default, and removed
or kept isolated from production serving behavior.

## Risks And Open Questions

- Exact stability constants: choose the configured L1 stability depth/time and
  stale pending-journal threshold in code and record them in the startup report.
  The plan requires stable-bound reconciliation and explicit stale recovery, but
  the numeric defaults must be set with protocol operations input.
- Reorg recovery command design: startup must fail on cursor rollback or
  provider view behind the recorded stable cursor. A later recovery command may
  implement audited rewind, but normal startup must not auto-rewind.
- Recovery authority: define who may run mutation recovery commands and how
  audit manifests are retained.
- Migration dependency: schema verification should ultimately depend on P0
  blocker 5. Until then, table/constraint introspection is a stopgap and should
  not become a permanent substitute for explicit migrations.
- Performance: SQL-derived root rebuild may be expensive on large state. It is
  still acceptable at startup for a production correctness gate. Optimize only
  after correctness is established.

## Concrete Checklist

- [ ] Introduce `StartupIntegrityError` with stable codes, evidence, and recovery
      commands.
- [ ] Add `verifyStartupIntegrityOrFail` and call it before HTTP/fiber startup.
- [ ] Replace `listen` startup `InitDB.program` application DDL with a read-only
      schema compatibility assertion.
- [ ] Move submit-queue and normal router construction after successful startup
      integrity.
- [ ] Remove startup suppression around `fetchAndInsertDepositUTxOs`.
- [ ] Remove startup suppression around `projectDepositsToMempoolLedger`.
- [ ] Add strict startup deposit reconciliation with cursor evidence and
      byte-for-byte re-read verification.
- [ ] Add strict startup deposit projection verification using the same stable
      L1 barrier returned by deposit reconciliation.
- [ ] Add pending-finalization journal verifier and L1 comparison.
- [ ] Add SQL cross-table invariant checks for tx states, block links, deposits,
      deltas, and pending members.
- [ ] Split fresh-deployment MPT initialization from existing-deployment MPT
      verification.
- [ ] Add SQL-derived temporary root computation for confirmed/admission state
      and persistent MPT roots.
- [ ] Compare local roots against explicit L1 root/tail and pending-journal
      evidence.
- [ ] Extend readiness inputs and `/readyz` response with startup integrity
      status.
- [ ] Add metrics and tracing for startup checks.
- [ ] Add `startup-integrity check --json`.
- [ ] Harden `project-deposits-once` with `--strict --verify`.
- [ ] Add deposit, pending-finalization, and MPT recovery commands with dry-run
      and audit manifests.
- [ ] Add unit, integration, MPT fault-injection, L1-provider fault-injection,
      and end-to-end startup tests.
- [ ] Document runbooks for every `startup_failed:*` code.
- [ ] Verify no compatibility shim, silent rewrite, or weakened validation is
      introduced.
