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
- the heads module: the one definition of depth, the levels local, landed,
  safe, final and merged, and `slotNow`.

The live chain-sync client, the decode pool and the role wiring live
elsewhere. This package never opens a network connection.

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

const started = await store.start(); // migrate, INV1–INV6, load live outrefs
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
restart passes `start()` again.

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

Writers are serialised on one lane. `start()` must succeed before any write.

| Member                                               | Result                                                                                                                                                |
| ---------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `start()`                                            | `{ kind: "ready", cursor, liveOutRefs, migrated } \| Intervention`                                                                                    |
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
| `close()`                                            | releases the backend after queued writes                                                                                                              |

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

### Views (§8.1)

A view is `(generation, point, height)`. `viewValid(view)` is true while no
rewind happened since it was read, or its point is still stored. To guard a
write in the role's own transaction, run `viewValidQuery(dialect, view)` (it
takes `FOR SHARE` on the cursor row, so no rewind commits between the check
and the write) or `viewValidIn(tx, dialect, view)`. Another process learns
of rewinds with `listenForGenerations(pool, (generation) => …)`, which
returns an async `stop()`.

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
unknown class, an empty rule, a D-t table that is not registered, and a
registered table not declared D-t or D-x. The lints load the TypeScript
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
