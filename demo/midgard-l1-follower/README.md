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
  (D-t) table, un-spends and deletes facts above the target, bumps the
  generation, logs the rollback and notifies `l1_generation`;
- the temporal registry, which generates the rewind and prune SQL;
- invariants INV1–INV6, checked at start and (scoped) inside every rewind;
- views `(generation, point)` and their validity check;
- the read API, and budgeted retention pruning;
- the writer lease (one writing process per store) and
  `midgard-l1-follower reset --to-origin`.

The live chain-sync client, the decode pool and the role wiring live
elsewhere. The store never opens a network connection; only the origin gate
and `find-origin` (below) read the chain, through a caller-supplied
`L1NodeTransport` from `@al-ft/l1-node-transport`.

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
detail }`, never throws and never exits. The caller retries with backoff, and
readiness reports `store_locked` as a transient reason. This is the committee's
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

- deletes every row of every catalog table of class A, C, D-t or D-x: the
  facts, the seeds, the cursor, the rollback log, `l1_scripts`, and every role
  table migrated through the follower;
- never deletes a class B row, the migration ledger, the catalog or the
  writer row;
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
  | { pool: pg.Pool } // caller-owned; close() leaves it open
  | { connectionString: string; maxConnections?: number };

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

| Member                                               | Result                                                                                                                                                |
| ---------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `start()`                                            | `{ kind: "ready", cursor, liveOutRefs, migrated } \| Intervention \| StoreLocked`                                                                     |
| `initialize({ point, height })`                      | `initialized \| already_initialized \| origin_mismatch` (with `cursor`), or `StoreError`                                                              |
| `applyBlock(block)`                                  | `BlockApplied { cursor, qualified, created, spent } \| ApplyRejection { reason: "not_initialized" \| "not_on_cursor" } \| Intervention \| StoreError` |
| `rewind(target: Point)`                              | `Rewound { generation, from, to, depth, cursor, unspent, deleted } \| RewindNoop \| Intervention \| StoreError`                                       |
| `insertSeedOutputs(seedSlot, outputs: SeedOutput[])` | `SeedResult { inserted, skipped } \| StoreError \| null` (null: not initialized)                                                                      |
| `prune(budget = 5000)`                               | `PruneResult { deleted, done, prunedThroughSlot } \| StoreError`                                                                                      |
| `checkInvariants()`                                  | `InvariantReport { ok, violations }` (full INV1–INV6)                                                                                                 |
| `cursor()`                                           | `Cursor \| null`                                                                                                                                      |
| `currentView()` / `viewValid(view)`                  | `View \| null` / `boolean`                                                                                                                            |
| `onGeneration(listener)`                             | unsubscribe function; called after each committed rewind                                                                                              |
| `setTrackedSet(set)` / `trackedSet()`                | replaces / returns the static tracked set                                                                                                             |
| `isTrackedLive(outRef)` / `liveOutRefCount()`        | the in-memory live tracked-outref set                                                                                                                 |
| `transaction(mode, run)`                             | a raw `SqlTx` on the store's backend (`"read"` snapshot or `"write"`)                                                                                 |
| `close()`                                            | releases the writer lease and the backend after queued writes                                                                                         |

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
`cursor.prunedThroughSlot`) or `not_initialized`.

### Views (§8.1)

A view is `(generation, point, height)`. `viewValid(view)` is true while no
rewind happened since it was read, or its point is still stored. To guard a
write in the role's own transaction, run `viewValidQuery(dialect, view)` (it
takes `FOR SHARE` on the cursor row, so no rewind commits between the check
and the write) or `viewValidIn(tx, dialect, view)`. Another process learns
of rewinds, and of resets, with `listenForGenerations(pool, (generation) =>
…)`, which returns an async `stop()`. Generations never repeat: `initialize`
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

`decodeBlock(raw: Uint8Array): BlockSummary` (throws `BlockDecodeError`) takes
one bare Shelley-family block (Alonzo and later: five elements), as the N2C
transport delivers it after the era tag, and returns every transaction, valid
or phase-2-failed. `encodeOutRef` / `decodeOutRef` use the
34-byte form (tx hash, then a big-endian u16 index); `outRefKey` is the hex
form used for maps.

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

## Tests and benchmarks

```sh
pnpm test        # needs the test Postgres on 127.0.0.1:5433
pnpm bench       # B3 (rewind at N = 10^6) and B8 (retention soak)
```

Set `MIDGARD_TEST_DATABASE_PREFIX` per worktree. `L1_FOLLOWER_PROPERTY_OPS`
sets the property-test length (default 10^4). Bench reports are written to
`bench/output/` (or `L1_FOLLOWER_BENCH_OUTPUT`); `L1_FOLLOWER_B3_N`,
`L1_FOLLOWER_B8_K` and `L1_FOLLOWER_B8_FULL_K=1` scale them.
