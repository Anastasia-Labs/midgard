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
- the read API, and budgeted retention pruning.

It also owns the bridge from the node transport's chain-sync events to the
store (`applyChainSyncEvent`, `intersectionPoints`), the fork simulator
(`./testing`) and the shadow-diff harness with its devnet soak runner
(`./shadow`). The live chain-sync client itself (`@al-ft/l1-node-transport`),
the decode pool and the role wiring live elsewhere. The library entry points
never open a network connection; only the soak CLI does.

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

| Member                                                | Result                                                                                                                                                                     |
| ----------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `start()`                                             | `{ kind: "ready", cursor, liveOutRefs, migrated } \| Intervention`                                                                                                         |
| `initialize({ point, height })`                       | `initialized \| already_initialized \| origin_mismatch` (with `cursor`), or `StoreError`                                                                                   |
| `applyBlock(block)`                                   | `BlockApplied { cursor, qualified, created, spent } \| ApplyRejection { reason: "not_initialized" \| "not_on_cursor" } \| Intervention \| StoreError`                      |
| `rewind(target: Point)`                               | `Rewound { generation, from, to, depth, cursor, unspent, deleted } \| RewindNoop \| Intervention \| StoreError`                                                            |
| `insertSeedOutputs(at: Point, outputs: SeedOutput[])` | `SeedResult { cursor, inserted, skipped } \| SeedCursorMoved \| StoreError \| null` (null: not initialized; `cursor_moved`: `at` is no longer the cursor, nothing written) |
| `prune(budget = 5000)`                                | `PruneResult { deleted, done, prunedThroughSlot } \| StoreError`                                                                                                           |
| `checkInvariants()`                                   | `InvariantReport { ok, violations }` (full INV1–INV6)                                                                                                                      |
| `cursor()`                                            | `Cursor \| null`                                                                                                                                                           |
| `currentView()` / `viewValid(view)`                   | `View \| null` / `boolean`                                                                                                                                                 |
| `onGeneration(listener)`                              | unsubscribe function; called after each committed rewind                                                                                                                   |
| `setTrackedSet(set)` / `trackedSet()`                 | replaces / returns the static tracked set                                                                                                                                  |
| `isTrackedLive(outRef)` / `liveOutRefCount()`         | the in-memory live tracked-outref set                                                                                                                                      |
| `transaction(mode, run)`                              | a raw `SqlTx` on the store's backend (`"read"` snapshot or `"write"`)                                                                                                      |
| `close()`                                             | releases the backend after queued writes                                                                                                                                   |

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

`decodeLedgerUtxos(answer)` decodes an LSQ UTxO answer (`utxo_by_address`,
`utxo_by_txin`) into `{ outRef, output }` pairs.

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
unknown class, an empty rule, a D-t table that is not registered, and a
registered table not declared D-t or D-x. The lints load the TypeScript
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
`store_error`).

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
applyChainSyncEvent(store, event: ChainSyncEvent): Promise<FollowStep>
stepSettled(step: FollowStep): boolean // applied, rewound or noop
intersectionPoints(store): Promise<BlockPoint[]>
storePoint(point: BlockPoint): Point; transportPoint(point: Point): BlockPoint
```

`applyChainSyncEvent` decodes a roll-forward and applies it, or rewinds to a
roll-backward's point. A rollback to the genesis is R1 `rollback_beyond_k`
without touching the store; an undecodable block is `block_undecodable`.
`intersectionPoints` offers the 64 newest stored blocks, then blocks 128,
256, 512, ... below the cursor, then the store's origin (at most 256 points,
the transport's limit).

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
runForkScenario(scenario, { open, k, projections?, comparators?, source?, walletSeed? }): Promise<ForkRunOutcome>
forkScenarioArbitrary(k): fc.Arbitrary<ForkScenario> // fast-check
forkCorpus(k): NamedScenario[] // every shape and variant at depths 1, k/2, k
```

After every event `runForkScenario` checks that the store equals a store
rebuilt from scratch from the canonical chain (every fact and temporal
table), that INV1–INV6 hold, that the tracked outputs and spenders at each
episode's checkpoints match the simulator's own ledger model, every plugged
projection's `check`, and every shadow comparator. `source` replaces the
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

## Shadow diff and devnet soak (`@al-ft/midgard-l1-follower/shadow`)

A `ShadowComparator` reads one thing two ways at the store's cursor: the
role's new projection (`projected`) and the current code's view
(`current`), optionally fed every event first (`observe`). `compareAll`
normalises both sides to JSON and reports `equal`, `differs` (with the
paths), `skipped` (a side is `unavailable`) or `error` (a side threw); it
never throws. Comparators that read old code are development tooling: they
live in their own files and are deleted with the old code at each role's
cutover.

A role plugs in through a module whose default export is a
`ShadowPlugin`: `{ role, projections?, comparators(env) }`, where `env`
carries the transport, the store, the soak directory and the plugin's
options from `soak.json`.

The soak runner follows a devnet into a SQLite store and journals one record
per event to `<dir>/journal.jsonl` (fsynced): the event, the cursor and
every comparator's result. It resumes from the store's cursor after a
restart (comparing once at the cursor when the journal lags the store),
retries store and transport errors with backoff, and stops only at an
intervention (R1, R2, R5, an undecodable block), a refusal, `--max-events`
or a signal.

```sh
node dist/shadow/soak-cli.js run --dir <dir> [--max-events <n>]
node dist/shadow/soak-cli.js report --dir <dir>
```

`<dir>/soak.json`:

```json
{
  "socketPath": "node.socket",
  "networkMagic": 42,
  "binaryPath": "../l1-node-transport/dist/native/midgard-l1-node-transport",
  "securityParameter": 2160,
  "trackedSet": {
    "addresses": ["<hex>"],
    "paymentCredentials": [],
    "policies": []
  },
  "ledgerAddresses": ["<hex>"],
  "plugins": [{ "module": "./committee-shadow.mjs", "options": {} }]
}
```

Relative paths resolve against `<dir>`. A fresh soak starts at the node's
tip. `report` prints the block count, each comparator's outcomes, the roles
with no comparator yet, the first non-empty diff and the last stop; `run`
writes the same as `summary.json` when it stops. Exit codes: 0 stopped at the
limit or on a signal, 3 intervention, 4 refused, 1 crashed.

Until the role comparators exist, the soak runs the built-in ledger
comparator (role `follower`, when `ledgerAddresses` is non-empty): the
store's live outputs at those addresses against the node's own UTxO set at
the same block, minus what already existed at the store's origin. It is not
old-code tooling and stays.

## Tests and benchmarks

```sh
pnpm test          # needs the test Postgres on 127.0.0.1:5433
pnpm test:fork-sim # fork simulator, shadow diff and soak runner (also Postgres)
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
