# midgard-l1-follower

The shared L1 chain follower's store: the canonical L1 facts every role
(operator node, DA committee, watcher) reads, and the one rollback mechanism
they all use, "rewind the facts, then recompute".

This package owns:

- the `FactStore`, with a Postgres adapter (node, committee) and a SQLite
  adapter (`node:sqlite`, watcher only);
- the follower schema as namespaced migrations, each table headed
  `-- class: <A|B|C|D-t|D-x>; retention: <rule>`;
- the sequential block writer: qualification, facts, the role's S3
  derivations and the cursor advance in one transaction per block;
- a raw CBOR block decoder that keeps the exact byte slices of each body,
  witness set, datum and redeemer (no dependency on the N2C transport);
- `rewind(target)`: one transaction that truncates every registered temporal
  (D-t) table, un-spends and deletes facts above the target (seed rows by
  their seed point), bumps the generation, logs the rollback and notifies
  `l1_generation`;
- the temporal registry, which generates the rewind and prune SQL;
- invariants INV1–INV6, checked at start and (scoped) inside every rewind;
- views `(generation, point)` and their validity check;
- the read API, and budgeted retention pruning;
- the writer lease (one writing process per store) and
  `midgard-l1-follower reset --to-origin`;
- the heads module: the one definition of depth, the levels local, landed,
  safe, final and merged, and `slotNow`.

It also owns the bridge from the node transport's chain-sync events to the
store (`applyChainSyncEvent`, `intersectionPoints`, `startWhenFree`), the
role projection seam (`FollowerProjection`, `projectionStoreOptions`) and the
fork simulator (`./testing`). The live chain-sync client itself
(`@al-ft/l1-node-transport`), the decode pool and the role wiring live
elsewhere. The library entry points never open a connection to a Cardano
node: the origin gate and `find-origin` (below) read the chain through a
caller-supplied `L1NodeTransport`. Only the `find-origin` CLI opens one.

## Usage

```ts
import { decodeBlock, openPostgresFactStore } from "@al-ft/midgard-l1-follower";

const store = openPostgresFactStore({
  connection: { connectionString: process.env.DATABASE_URL! },
  securityParameter: 2160,
  trackedSet, // addresses, payment credentials and policies (hex)
  temporalTables, // the role's D-t tables
  migrations: [roleMigrations], // creates them, with class headers
  derivations: [roleDerivation], // writes them, in the block's transaction
});

const started = await store.start(); // lease, migrate, INV1–INV6, load live outrefs
if (started.kind === "store_locked") retryWithBackoff(); // transient
if (started.kind === "intervention") markUnready(started.reason);
await store.initialize({ point: origin, height: originHeight });

// Roll forward / roll backward from chain-sync:
await store.applyBlock(decodeBlock(rawBlockBytes));
await store.rewind(intersectionPoint);
```

Every write returns a typed result and never throws for a chain or store
condition. `intervention` (R1 `rollback_beyond_k`, R2
`intersection_outside_history`, R5 `store_integrity`) means the role reports
unready and keeps running; `error` means the transaction rolled back with
nothing changed and the caller retries with backoff. R5 is sticky until a
restart passes `start()` again. `store_locked` is transient: see the writer
lease below.

## Writer lease

One process writes a store. `start()` takes the store's writer lease first
and holds it until `close()`:

- Postgres: a session advisory lock (one key per database and schema) on a
  dedicated connection outside the pool;
- SQLite: core's `SqliteProcessMutex` on the sidecar file
  `<database>.writer-lease` (an in-memory database needs none).

While another process holds it, `start()` returns `{ kind: "store_locked",
detail }`, never throws and never exits. The caller retries with backoff;
`followChain` (below) reports it as `l1_follower_waiting` with cause
`store_locked`, a transient reason. This is the committee's
active/passive pair (C2): the passive member's follower waits, and takes over
within one retry once the active process dies, because its lock dies with its
session or process.

Each holder bumps a fencing epoch in `l1_follower_writer` at start, and every
write reads it under a share lock. A holder that lost its lease (its lease
connection dropped, or a newer holder bumped the epoch) gets `store_locked`
from its next write and changes nothing; it must call `start()` again, which
re-takes the lease or waits. Reads never need the lease.

## Origin

The origin O (`l1Origin`, `<slot>.<block hash>`) is the point immediately
before the block holding the deployment's `prepareHubOracleNonce` tx. An
operator may override it per role: `L1_ORIGIN` for the node and the
committee, `$.l1.origin` in the watcher config. The override never enters a
profile or manifest. `parseL1Origin`, `formatL1Origin` and
`checkL1OriginBeforeHubOracleNonceBlock` live in
`@al-ft/midgard-core/l1-origin`. `find-origin` applies that invariant to its
own result; `deployment:check` will enforce it once the manifest carries
`l1Origin` (with the redeploy).

```ts
startFromOrigin({ store, transport, origin, credit }): Promise<OriginStart>
protocolInitStatus(store, { origin, hubOracleOneShot }, tip): Promise<ProtocolInitStatus>
```

`startFromOrigin` opens chain-sync at `[O]` on a fresh store, takes O's height
from the first block after it (its block number minus one; that block must
extend O) and initializes the store there. It returns `initialized` or
`already_initialized` with `{ cursor, stream, first }` (the runner applies
`first`, acks it and follows the stream), `resume` when the store already has
a cursor at O, an intervention (R4, or `origin_mismatch` below) or a
`StoreError`. It closes the stream on every other path.

- R4 `origin_not_on_chain`: on a fresh start (no cursor, not resuming) the
  node cannot intersect O, or the first block after O does not extend it. A
  failed intersection with a stored cursor, or on a resume, stays R2
  (`intersectionFailure` classifies it).
- R3 `origin_after_protocol_init`: `protocolInitStatus` at the node tip finds
  no stored valid tx spending `hubOracleOneShot` (the protocol-init tx). It is
  `pending` before the cursor reaches the tip and `seen` once the spend is
  stored. It is not sticky. It needs the role's tracked set to qualify the
  init tx (the hub oracle policy), and relies on the init tx staying stored
  while an output it created is live; it can be wrong only after the hub
  oracle NFT is burned and the init tx pruned.
- `origin_mismatch`: the store was initialized at another origin than the
  configured one, for example after the operator corrected `l1Origin` to
  clear R3. The store is never reset silently; it stays as it is until the
  operator resets the follower store (`reset --to-origin`, below) or restores
  the old origin. A cursor
  that another writer initializes between the check and `initialize` gives
  the same intervention. (`FactStore.initialize` itself still reports the
  case as `{ kind: "origin_mismatch", cursor }`.)

All three are unready reasons: the role fails `/readyz`, stays live on `/healthz`
and keeps running.

`FactStore.txSpending(outRef)` returns the earliest stored valid tx listing
`outRef` as an input (`{ txHash, slot } | null`). It scans `l1_txs`; it is
meant for rare checks such as R3.

### `midgard-l1-follower find-origin`

```sh
midgard-l1-follower find-origin --tx <prepareHubOracleNonce tx id> \
  --network-magic <n> [--socket <node socket>] [--sidecar <binary>] \
  [--from <slot>.<block hash>]
```

Scans the node's chain from `--from` (default genesis) for the tx and prints
`{ l1Origin, origin, prepareHubOracleNonceBlock, txIndex, depth }` as JSON.
`--socket` defaults to `CARDANO_NODE_SOCKET_PATH`, `--sidecar` to
`MIDGARD_L1_NODE_TRANSPORT_BINARY`. Exit codes: 0 found, 1 failed, 2 usage, 3
not found (absent, `--from` not on the chain, or the tx in the chain's first
block). The same scan is `findOrigin({ transport, txHash, from? })`.

### `midgard-l1-follower reset --to-origin`

```sh
midgard-l1-follower reset --to-origin --postgres <connection string>
midgard-l1-follower reset --to-origin --sqlite <database file>
```

The recovery for R1, R2, R5 and `origin_mismatch` (§7.5): the next start
initializes at the configured `l1Origin` and replays from it. It needs only
the connection; it reads which tables to clear from the catalog. In one
transaction it:

- deletes every row of every catalog table of class A, D-t or D-x: the
  facts, the seeds, the cursor, the rollback log, and every such role table
  migrated through the follower;
- never deletes a class B row (own signed material), a class C row, the
  migration ledger, the catalog or the writer row. Class C rows (`l1_scripts`,
  foreign payloads) are immutable and content-addressed, so they are correct
  for any chain and are kept while referenced (§7): the next `prune()`
  removes the `l1_scripts` rows no retained output references, and a role's
  own retention removes its class C rows;
- raises the next generation above every generation the store used, and
  notifies `l1_generation`, so no view taken before the reset validates by
  generation after it;
- bumps the fencing epoch.

It refuses while a follower holds the writer lease (stop the follower
first), and refuses, deleting nothing, when a table outside the catalog has a
foreign key into a table it would clear. It is idempotent: a second reset
deletes nothing and leaves the next generation as it was. D-x rows are
deleted; the external stores they version (MPF roots, Level nodes) are not.
Exit codes: 0 reset, 1 failed, 2 usage, 4 refused (writer lease held). The
JSON on stdout is `{ reset, nextGeneration, tables }`. A Postgres password may
come from `PGPASSWORD` rather than the connection string. The same operation
is `resetToOrigin(backend)`, which returns
`{ kind: "reset", tables, nextGeneration } | StoreLocked`.

## API

Value types (`Point`, `BlockSummary`, `TxSummary`, `OutputSummary`, `OutRef`,
`StoredOutput`, `StoredTx`, `StoredBlock`, `Cursor`, `View`, `TrackedSet`,
`Intervention`) are in `src/types.ts`. Byte fields are `Buffer`s with the
exact on-chain bytes; slots, heights and indexes are numbers; quantities are
`bigint`s.

### Opening a store

```ts
openPostgresFactStore(options: FactStoreOptions & { connection: PostgresConnection }): FactStore
openSqliteFactStore(options: FactStoreOptions & { path: string }): FactStore
createFactStore(backend: SqlBackend, options: FactStoreOptions): FactStore

type PostgresConnection =
  | { pool: pg.Pool } // caller-owned; close() leaves it open; caller listens for 'error'
  | {
      connectionString: string;
      maxConnections?: number;
      onConnectionError?: (error: Error) => void; // a connection Postgres dropped
    };

type FactStoreOptions = {
  securityParameter: number; // k in blocks
  trackedSet: TrackedSet;
  temporalTables?: readonly TemporalTableSpec[];
  migrations?: readonly MigrationSet[];
  derivations?: readonly DerivationHook[];
  retentionPins?: RetentionPins; // columns that keep txs/blocks from pruning
};
```

### `FactStore`

Writers are serialised on one lane. `start()` must succeed before any write,
and every write below can also return `StoreLocked` (the lease was lost).

| Member                                                | Result                                                                                                                                                                                    |
| ----------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `start()`                                             | `{ kind: "ready", cursor, liveOutRefs, migrated } \| Intervention \| StoreLocked`                                                                                                         |
| `initialize({ point, height })`                       | `initialized \| already_initialized \| origin_mismatch` (with `cursor`), or `StoreError`                                                                                                  |
| `applyBlock(block)`                                   | `BlockApplied { cursor, qualified, created, spent } \| ApplyRejection { reason: "not_initialized" \| "not_on_cursor" } \| Intervention \| StoreError`                                     |
| `rewind(target: Point)`                               | `Rewound { generation, from, to, depth, cursor, unspent, deleted } \| RewindNoop \| Intervention \| StoreError`                                                                           |
| `insertSeedOutputs(at: Point, outputs: SeedOutput[])` | `SeedResult { cursor, inserted, skipped } \| SeedCursorMoved \| StoreError \| StoreLocked \| null` (null: not initialized; `cursor_moved`: `at` is no longer the cursor, nothing written) |
| `prune(budget = 5000)`                                | `PruneResult { deleted, done, prunedThroughSlot } \| StoreError`                                                                                                                          |
| `checkInvariants()`                                   | `InvariantReport { ok, violations }` (full INV1–INV6)                                                                                                                                     |
| `cursor()`                                            | `Cursor \| null`                                                                                                                                                                          |
| `currentView()` / `viewValid(view)`                   | `View \| null` / `boolean`                                                                                                                                                                |
| `onGeneration(listener)`                              | unsubscribe function; called after each committed rewind                                                                                                                                  |
| `setTrackedSet(set)` / `trackedSet()`                 | replaces / returns the static tracked set                                                                                                                                                 |
| `isTrackedLive(outRef)` / `liveOutRefCount()`         | the in-memory live tracked-outref set                                                                                                                                                     |
| `transaction(mode, run)`                              | a raw `SqlTx` on the store's backend (`"read"` snapshot or `"write"`)                                                                                                                     |
| `close()`                                             | releases the writer lease and the backend after queued writes                                                                                                                             |

Reads (each a consistent snapshot):

| Member                                                              | Result                                                                          |
| ------------------------------------------------------------------- | ------------------------------------------------------------------------------- |
| `liveUtxos(filter: UtxoFilter, at?: Point)`                         | `{ kind: "ok", utxos: StoredOutput[] } \| PointRefusal`                         |
| `output(outRef)`                                                    | `StoredOutput \| null`                                                          |
| `spenderOf(outRef)`                                                 | `{ kind: "unspent" } \| { kind: "spent", txHash, slot } \| { kind: "unknown" }` |
| `txByHash(hash)`                                                    | `StoredTx \| null` (with exact body, witness and aux CBOR)                      |
| `isCanonical(blockHash)`                                            | `boolean`                                                                       |
| `pointStatus(point)`                                                | `{ kind: "canonical", height, depth } \| PointRefusal`                          |
| `blockByHash(hash)` / `blockAtHeight(h)` / `blockAtOrBeforeSlot(s)` | `StoredBlock \| null`                                                           |

`UtxoFilter` is `{ by: "address", address }`, `{ by: "payment_credential",
hash }`, `{ by: "unit", policyId, assetName? }` or `{ by: "outref", outRefs }`.
`PointRefusal.kind` is `point_not_canonical`, `point_beyond_retention` (below
`cursor.prunedThroughSlot`) or `not_initialized`. `pointStatus` reports
`depth` as the heads module counts it: 1 at the cursor.

A backend that owns its pool listens for every connection's `'error'` (an
idle pooled one, and one checked out for a transaction): Postgres dropping a
connection (a restart, a failover, an idle reaper, `pg_terminate_backend`)
never reaches the process as an uncaught exception. The pool discards the
client; a transaction on it rejects, which the follow loop backs off from.

### Views (§8.1)

A view is `(generation, point, height)`. `viewValid(view)` is true while no
rewind happened since it was read, or its point is still stored. To guard a
write in the role's own transaction, run `viewValidQuery(dialect, view)` (it
takes `FOR SHARE` on the cursor row, so no rewind commits between the check
and the write) or `viewValidIn(tx, dialect, view)`. Another process learns
of rewinds, and of resets, with `listenForGenerations(pool, (generation) =>
…, onConnectionLost?)`, which returns an async `stop()`. If Postgres drops
the listening connection, listening has stopped: `onConnectionLost(error)` is
told (never an uncaught `'error'`) and the caller listens again. Generations never repeat: `initialize`
starts at the writer row's next generation (0 on a new store), and a reset
raises it above every generation used before.

### Temporal tables and derivations

```ts
type TemporalTableSpec =
  | { name; shape: "versioned"; startColumn; endColumn; parents?; retention }
  | { name; shape: "append_only"; slotColumn; parents?; retention };
type RetentionRule =
  | { kind: "closed_k_deep" } | { kind: "created_k_deep" }
  | { kind: "owner"; description: string };
type DerivationHook = {
  name: string;
  writes: readonly string[]; // every name must be registered
  apply(ctx: { tx; dialect; block; qualified: QualifiedTx[]; previous: Cursor }): Promise<void>;
};
createTemporalRegistry(specs): TemporalRegistry // throws RegistryError
```

A derivation must be a pure function of the block, the facts and the role's
own class B/C content: no clock, randomness or network (enforced by the
determinism lint). A rewind deletes rows whose start or slot is above the
target and reopens versioned rows closed above it, children before parents.
Pruning deletes rows k deep per their `retention`. A role may also insert
`l1_event_keys` rows (`kind`, `key`, `origin_outref`, `first_canonical_slot`);
the rewind removes keys first seen above its target.

### Decoding and codecs

`decodeLedgerUtxos(answer)` decodes an LSQ UTxO answer (`utxo_by_address`,
`utxo_by_txin`) into `{ outRef, output }` pairs.

`decodeBlock(raw: Uint8Array): BlockSummary` (throws `BlockDecodeError`) takes
one bare Shelley-family block (Alonzo and later: five elements), as the N2C
transport delivers it after the era tag, and returns every transaction, valid
or phase-2-failed. `encodeOutRef` / `decodeOutRef` use the
34-byte form (tx hash, then a big-endian u16 index); `outRefKey` is the hex
form used for maps.

### Heads (§9)

```ts
depth(tipHeight, pointHeight): number          // tip − point + 1; the tip is depth 1
heightAtDepth(tipHeight, atDepth): number
levelAtDepth(atDepth, { confirmationDepth, securityParameter }): "landed" | "safe" | "final" | null
levelOf(atDepth | null, parameters, own): "local" | "landed" | "safe" | "final" | null
isSafe(atDepth, parameters) / isFinal(atDepth, parameters)
mergedStatus(mergeTxLevel): { merged: true, level } | { merged: false, level: "local" | null }
createSlotClock({ slotLengthMs, monotonicNowMs? }): { observeTipSlot, tipSlot, slotNow }
createHeads({ confirmationDepth, securityParameter, slotLengthMs, monotonicNowMs? }): Heads
```

The levels: `local` (own, not on the chain), `landed` (depth ≥ 1), `safe`
(depth ≥ cd; liveness only, never a reason to delete, release or retire),
`final` (depth > k) and `merged` (an L2 header whose merge tx has landed,
always reported with that tx's level). Final is strictly deeper than k: a
rollback of k blocks is legal (`rewind` refuses only deeper ones) and
removes depths 1..k.

`slotNow()` is max(tip slot, last tip slot + elapsed / slotLength), with
elapsed time from `performance.now()`. A wall clock that runs fast or jumps
does not move it, and a tip behind the estimate (a rollback, a stale source)
never moves it back. It is null until the first tip observation; a caller
then takes no L1 decision and retries. It is the only "now" for an L1
validity decision.

No module outside `src/heads.ts` compares a value against
`confirmationDepth` or `automaticRecoveryMaxDepth`; the ESLint rule
`midgard/depth-through-heads` enforces it (see
`docs/agents/lint-rules.md`).

### Lints (`@al-ft/midgard-l1-follower/lint`)

```ts
lintSchema(sets: readonly MigrationSet[], registry?: TemporalRegistry): SchemaLintProblem[]
declaredTables(sets: readonly MigrationSet[]): DeclaredTable[]
lintDeterminism(files: readonly { path: string; source: string }[], options?: { bannedModules?: string[] }): DeterminismProblem[]
lintDeterminismSource(path: string, source: string, options?): DeterminismProblem[]
```

`lintSchema` fails a `CREATE TABLE` without a class and retention header, an
unknown class, an empty rule, a D-t table that is not registered, a
registered table not declared D-t or D-x, and a class B table with a foreign
key (inline or by `ALTER TABLE`) into a table that is not class B, or that no
migration declares: a reset or a rewind must never be blocked by, or cascade
into, class B rows.

The migration runner records every table a migration declares, with its
class, in the catalog `l1_follower_tables`, and refuses a migration whose
table lacks its header. The bookkeeping tables (`l1_follower_migrations`,
`l1_follower_tables`, `l1_follower_writer`, in `FOLLOWER_BOOKKEEPING_DDL`)
are created before any migration and are not in the catalog. The lints load the TypeScript
compiler, so they are kept out of the runtime entry point.

### Wallet seed (§5.3 step 4)

```ts
seedWallets(store, ledger: WalletLedger, addresses: Buffer[], attempts = 3): Promise<WalletSeedResult>
createWalletSeeder({ store, ledger, wallets }): WalletSeeder
// WalletLedger = Pick<L1NodeTransport, "withLedgerState">
```

Own wallets can hold UTxOs the follower never stored: created before the
origin, or paid to a wallet after the origin while it was not tracked (also
by a stored tx whose output to it got no row). `seedWallets` acquires LSQ at
the store's cursor P, reads `utxo_by_address` for the wallets and writes
every outref the store holds no row for as a seed row (`created_slot NULL`,
`seed_slot = P`). The write is
refused unless the cursor is still P (a block or rewind in between could
hold a spend the seed would miss); the read is then repeated at the new
cursor. LSQ acquires only volatile points, so the seed succeeds once the
cursor is within k of the node's tip. It never throws: a `pending` result
names the transient reason (`not_initialized`, `cursor_not_acquirable`,
`cursor_moved`, `ledger_unavailable`, `ledger_answer_invalid`,
`store_error`, `store_locked`).

A seed row is a fact observed at P. A rewind to a target T deletes, in the
same transaction, every seed row with `seed_slot > T` (and un-spends the
rest like any row); rows seeded at or below T observed a state that is still
canonical and stay. INV6: no seed row lies above the cursor, and no stored
creator contradicts a seed row (created after the seed point, or not
creating that index).

`createWalletSeeder` owes the seed of each wallet and settles it with
`step()`. The role reports `/readyz` unready with `wallet_seed_pending`
(`WALLET_SEED_PENDING`) while `ready()` is false and calls `step()` after
follower steps. `addWallets(addresses)` adds the wallets to the store's
tracked set and owes their seed, and only theirs. After a committed rewind
below a wallet's seed point (which deleted its seed rows from that point)
the wallet is owed again and is read at a later cursor: the fresh answer
holds a pre-origin UTxO the rollback made live again and drops an output
whose creating tx the fork orphaned. Bootstrap and added wallets take the
same path, with no wait. `wallet_seed_pending` is transient: the role keeps
running and raises no intervention. Every wallet starts owed, so a restart
re-seeds; that writes nothing the store already holds.

The role wiring composes it with the origin start: start the store,
initialize at the origin and follow chain-sync; create the seeder once the
store is initialized and call `step()` after each settled follower step
until it is ready (the first steps return `cursor_not_acquirable` while
the follower replays from an origin more than k blocks deep).

### Following chain-sync

```ts
followChain(options: FollowChainOptions): Promise<FollowStatus>
applyChainSyncEvent(store, event: ChainSyncEvent): Promise<FollowStep>
stepSettled(step: FollowStep): boolean // applied, rewound or noop
intersectionPoints(store): Promise<BlockPoint[]>
storePoint(point: BlockPoint): Point; transportPoint(point: Point): BlockPoint
startWhenFree(store, { signal?, backoffMs?, log?, onLocked? }): Promise<StartResult | undefined>
classifyFailure(error): "transient" | "deterministic" | "unknown"
```

`followChain` is every role's follow loop. It starts the store (waiting out
the writer lease), starts from the configured origin or resumes from the
store's own intersection points, applies each event and acknowledges it
once the store settled it, checks `protocolInitStatus` at every tip, and
prunes. It never throws and never exits the process: transient failures
back off (capped exponential, default 500 ms to 30 s) and start again; an
intervention stops the loop and stays in its status until the operator acts
and the process restarts. It resolves with the final status once stopped by
an intervention or by `signal`.

```ts
type FollowChainOptions = {
  store: FactStore;
  transport: Pick<L1NodeTransport, "openChainSync">;
  origin: OriginConfig;
  signal: AbortSignal;
  credit?: number; // the chain-sync credit (default 64)
  backoffMs?: { initial: number; max: number };
  log?: (line: string) => void;
  onStatus?: (status: FollowStatus) => void | Promise<void>; // awaited; a throw is logged
  stuckAfter?: number; // default 5
  prune?: { budget?: number; everyEvents?: number }; // default 500 rows, every 100 events
};

type FollowStatus = {
  state: "starting" | "following" | "waiting" | "intervention" | "stopped";
  readiness: { reason: FollowReadinessReason; detail: string }[]; // empty: ready
  interventions: { reason: InterventionReason; detail: string }[];
  waiting: {
    cause: "store_locked" | "stream" | "store" | "apply";
    detail: string;
  } | null;
  stuck: { at: string; failures: number; detail: string } | null;
  protocolInit: "seen" | "pending" | "unknown";
  cursor: { slot: number; height: number; generation: number } | null;
  tip: { slot: number; height: number } | null; // the node tip of the last applied event
  atTip: boolean; // the cursor is that tip
  events: number; // events applied by this loop
  lastError: string | null; // cleared by the next applied event
  prune: {
    steps: number;
    prunedThroughSlot: number | null;
    lastError: string | null;
  };
};
```

`readiness` is what the role's `/readyz` reports; `/healthz` is the role's
own concern (the loop never makes the process unhealthy). Its reasons:

| Reason                              | When                                                                                                                                                                                                                                    |
| ----------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `rollback_beyond_k` (R1)            | a rewind deeper than k or below `prunedThroughSlot`, or a roll-backward to the genesis. Stops the loop.                                                                                                                                 |
| `intersection_outside_history` (R2) | the node has none of a resumed store's intersection points. Stops the loop.                                                                                                                                                             |
| `origin_after_protocol_init` (R3)   | at the tip without the protocol-init spend. The loop keeps following; cleared once the spend is stored.                                                                                                                                 |
| `origin_not_on_chain` (R4)          | a fresh store's origin is not on the node's chain. Stops the loop.                                                                                                                                                                      |
| `store_integrity` (R5)              | the store fails INV1–INV6 at start or after a rewind. Stops the loop.                                                                                                                                                                   |
| `origin_mismatch`                   | the store was initialized at another origin. Stops the loop.                                                                                                                                                                            |
| `l1_follower_apply_stuck`           | one event failed to apply `stuckAfter` times in a row, or once with a deterministic failure (an undecodable block, a constraint or data error). The loop keeps retrying; the next applied event clears it.                              |
| `l1_follower_waiting`               | backing off from a transient failure: the writer lease (`store_locked`), a stream failure or end (`stream`), a failed start or read (`store`), an apply failure below the stuck threshold (`apply`). Cleared by the next applied event. |
| `l1_follower_catching_up`           | the cursor is not at the node tip of the last applied event (and before the first one). A role whose decisions stay safe on a lagging view may ignore it.                                                                               |

`classifyFailure` sorts a failed write: `transient` (Postgres SQLSTATE
classes 08, 40, 53, 55, 57, network errno codes, pg's own connection errors,
SQLite BUSY and LOCKED) never counts toward `stuckAfter`; `deterministic`
(SQLSTATE 22, 23, 42, SQLite CONSTRAINT and MISMATCH) escalates at once;
`unknown` (anything else, and an `ApplyRejection`) counts toward
`stuckAfter`. A failure to start or resume escalates only when
deterministic.

Pruning runs inside the loop, under its error handling: after each applied
event at the tip, and every `everyEvents` applied events while catching up
(then after every event until a step reports `done`), one `prune(budget)`
step. The default budget, 500 rows per table per step, is the one B8 showed
keeps every table at its plateau with one step per block; each step is one
write transaction deleting at most 500 rows from each table, so it holds the
writer for a bounded time between two events. A failed step is logged and
reported in `prune.lastError`, and the next one retries.

`applyChainSyncEvent` decodes a roll-forward and applies it, or rewinds to a
roll-backward's point. A rollback to the genesis is R1 `rollback_beyond_k`
without touching the store; an undecodable block is `block_undecodable`.
`intersectionPoints` offers the 64 newest stored blocks, then blocks 128,
256, 512, ... below the cursor, then the store's origin (at most 256 points,
the transport's limit). `startWhenFree` starts the store and waits out
`store_locked` (another process holds the writer lease) with backoff instead
of exiting, telling `onLocked` each time; it returns the first other start
result, or `undefined` once `signal` aborts.

### Role projections

A role's `FollowerProjection` (main entry) bundles its tracked set, D-t
tables, migrations, derivations and retention pins, plus the fork
simulator's optional `traffic`, `protects` and `check`.
`projectionStoreOptions(projections, { securityParameter, trackedSet },
dialect)` merges them into the `FactStoreOptions` the role opens its store
with (`mergeTrackedSets` unions tracked sets). The role's production store
and its fork-simulator cases use the same projection.

## Fork simulator (`@al-ft/midgard-l1-follower/testing`)

Seeded fork scenarios, emitted as the transport's `ChainSyncEvent`s and fed
to the sequential writer. Each episode builds an old branch, rolls back
1..k blocks and builds a new one, in one of five shapes: `reland` (the
subject transaction lands again on the new branch), `never_reland` (it never
does, sometimes because a conflicting spend took its input), `changed_valid_to`
(it lands again with a different upper validity bound), `new_fork_only`
(a transaction only the new branch has) and `phase2_failed` (a failed
transaction whose collateral is spent, re-landed failed, replaced by a valid
one, or absent).

```ts
runForkScenario(scenario, { open, k, projections?, source?, prepare?, walletSeed? }): Promise<ForkRunOutcome>
forkScenarioArbitrary(k, maxEpisodes?, { prune? }): fc.Arbitrary<ForkScenario> // fast-check
forkCorpus(k, { prune? }): NamedScenario[] // every shape and variant at depths 1, k/2, k, and the prune cases
```

An episode with `prune` prunes the store to completion (in budget-2 steps)
after its old branch's last block and again after its rollback: the
rollback rewinds over pruned rows, and its cursor drop leaves rows pruned
before within k of the new cursor (the E7 case). The corpus adds every shape
pruned around a depth-k rollback and the chain of every shape pruned;
`{ prune: false }` leaves prune cases out.

After every event `runForkScenario` checks that the store equals a store
rebuilt from scratch from the canonical chain (every fact and temporal
table), that INV1–INV6 hold, that the tracked outputs and spenders at each
episode's checkpoints match the simulator's own ledger model, and every
plugged projection's `check`. Once the store has pruned
(`prunedThroughSlot` above the origin), "equals" becomes two checks against
the unpruned rebuild: every row the store holds is a row of the rebuild, and
every row the rebuild holds that plan §11 retention keeps at
`prunedThroughSlot` is in the store (`retainedQueries`, `diffPruned`;
live and recently spent outputs, rows a projection's `retentionPins` pin,
checkpoint blocks, D-t rows by their retention rule). Until then the check is
plain equality. `source` replaces the
in-memory event list with a real one (the transport test serves it through a
fake sidecar and the real frame client).

`walletSeed` (`{ wallets, preOrigin, startAfter, added? }`) runs the wallet
seeder against the simulated node's LSQ from event `startAfter` on, with the
wallets tracked and `preOrigin` UTxOs in the ledger at the origin; `added`
wallets join through `addWallets` after event `atEvent`. After every event
the store's seed rows must be exactly the rows seeded and not rewound (new
rows only at the cursor), the fresh replay tracks and seeds where the store
did, and once the seeder is ready the store's live rows at every wallet must
be exactly the ledger's UTxOs there (no phantom, none missing).

A role ticket adds cases by passing a `FollowerProjection`: its tracked set,
D-t tables, migrations, derivations and retention pins, optional `traffic`
(transactions the simulator mixes into blocks, so the role's own outputs
appear on both branches) and an optional `check` run after every event. Role
packages run their cases from a `test:fork-sim` script; CI runs every
package's `test:fork-sim` (see below).

## Fresh-replay gate

The follower's correctness gate is the fork simulator, not a comparison with
the code it replaces. After every operation of every scenario (each
roll-forward, rollback, prune and wallet seed), every fact table and every
projection's D-t table must equal a fresh forward-only replay of the
current canonical chain (once pruned: hold only its rows and every row
retention keeps), INV1–INV6 must hold and each projection's `check` must
pass. Role packages plug their projections into the same runner from
their own `test:fork-sim` script. The devnet journeys then run the follower
against real node CBOR, with no Kupo or Ogmios.

## Tests and benchmarks

```sh
pnpm test          # needs the test Postgres on 127.0.0.1:5433
pnpm test:fork-sim # fork simulator and its fresh-replay gate (also Postgres)
pnpm bench       # B3 (rewind at N = 10^6) and B8 (retention soak)
```

Set `MIDGARD_TEST_DATABASE_PREFIX` per worktree. `L1_FOLLOWER_PROPERTY_OPS`
sets the property-test length (default 10^4); `L1_FORK_SIM_RUNS` and
`L1_FORK_SIM_POSTGRES_RUNS` set the fast-check run counts (default 200 on
SQLite, 20 on Postgres). Bench reports are written to
`bench/output/` (or `L1_FOLLOWER_BENCH_OUTPUT`); `L1_FOLLOWER_B3_N`,
`L1_FOLLOWER_B8_K` and `L1_FOLLOWER_B8_FULL_K=1` scale them.

CI runs `test:fork-sim` in the `l1-fork-simulator` job. On a pull request it
runs only when the change touches one of its inputs, which
`scripts/ci/fork-simulator-inputs.mjs` derives from the workspace: every
package that defines `test:fork-sim`, their workspace dependencies, the
follower, the transport and the install pins.
