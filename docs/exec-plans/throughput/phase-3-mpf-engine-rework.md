# ExecPlan: Throughput Phase 3 — MPF Engine Rework (C1, C2, C5)

**Status:** In progress; Architecture G production owner/RPC, journal,
worker/runtime, crash-recovery, and release wiring are implemented behind an
explicit opt-in. Historical retained-session selection and full-index prototype
gates pass. Formal production retained-growth, canonical fresh-process 50k x20,
clean live E2E, soak, independent review, and a separate default/cap decision
remain open.
**Effort:** weeks 4–10, highest effort & value in the throughput program
**Owner:** TBD
**Depends on:** Phase 0 instrumentation (MPF phase timers — the `logCommitMpfPhaseTiming` phases at `demo/midgard-node/src/workers/utils/mpf.ts:1342-1357` must already be flowing into the benchmark dashboard); parent plan `THROUGHPUT-2500-TPS-PLAN.md` §Phase 3 (bottlenecks C1, C2, C5)
**Exit criterion:** 50k-tx block build <10 s p95; no commit-time degradation as the confirmed UTxO set grows from 100k to 1M entries; root equivalence between old and new engines proven per-block in CI.

All file paths below are relative to `demo/midgard-node` unless prefixed otherwise. All line numbers were verified against the working tree on 2026-07-09.

---

## 1. Pre-implementation baseline (verified 2026-07-09)

This section records the legacy bottlenecks and decisions that selected the
work. It is intentionally historical: the production integration described in
Section 9 now changes several call sites, ownership boundaries, and config
surfaces below. Use current source and the Architecture G integration plan for
the implemented state.

### 1a. Trie inventory — what actually gets built per commit

The parent plan names four tries ("ledger, transactions, trace, event-to-step"). The code has **two persistent tries and at least seven scratch trie builds** per commit:

| #   | Root                                                        | Backing                                                                                                                                                  | Built where                                                                                           |
| --- | ----------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| 1   | `utxosRoot` (ledger MPF)                                    | **Persistent LevelDB** at `NodeConfig.LEDGER_MPF_DB_PATH` (`src/services/config.ts:87,429,575`)                                                          | `makeMpfs` (`src/workers/utils/mpf.ts:181-231`), mutated per-event in `buildTransitionTraceResult`    |
| 2   | raw transactions MPF                                        | **Persistent LevelDB** at `TRANSACTIONS_MPF_DB_PATH` (`config.ts:88,433,576`); reset to empty every block (`src/workers/commit-block-header.ts:257-263`) | `processMpfs` → `transactionsMpf.applyBatch` (`mpf.ts:1872-1884`)                                     |
| 3   | `txRoot` (transactions source root)                         | Scratch in-memory                                                                                                                                        | `buildTransactionsSourceRoot` (`mpf.ts:699-702`) → `countedRootFromEncodedEntries` (`mpf.ts:676-697`) |
| 4   | `transitionTraceRoot`                                       | Scratch in-memory                                                                                                                                        | `mpf.ts:1047-1053`                                                                                    |
| 5   | `eventToStepRoot`                                           | Scratch in-memory                                                                                                                                        | `mpf.ts:1054-1060`                                                                                    |
| 6   | UTxO-payload root (`payload_root_check`)                    | Scratch in-memory over the **entire ledger + delta**                                                                                                     | `computeUtxoPayloadRoot` (`mpf.ts:651-657`, called at `mpf.ts:1897`)                                  |
| 7–9 | `depositsRoot`, `withdrawalsRoot`, `forcedTransactionsRoot` | Scratch in-memory                                                                                                                                        | `src/workers/commit-block-header/event-roots.ts:13-66`                                                |

Every scratch root goes through `keyValuePhasRootWithCount` (`src/workers/utils/mpf/phas.ts:60-87`), which sorts+dedups entries and then builds a trie via `MidgardMpf.createScratch` + `applyBatch` — i.e. **N sequential `insert` calls**, each re-hashing its full path (see §1b).

**Correction (vs. parent plan C1):** the scratch tries are already memory-backed (`MidgardMpf.createScratch` → Map store, `mpf.ts:2313-2321`). "Disk-backed per-op updates" applies only to tries #1 and #2. The scratch tries' cost is pure CPU (sequential-insert hashing), which is a different fix (bulk build via `Trie.fromList`, §2.2).

### 1b. Verified `@aiken-lang/merkle-patricia-forestry` API surface (load-bearing)

Package `@aiken-lang/merkle-patricia-forestry@1.2.0` (deps: `blake2b ^2.1.4`, `level ^8.0.1`). Public exports: `Trie`, `Store`, `Proof` (`node_modules/@aiken-lang/merkle-patricia-forestry/dist/index.d.ts`).

**The library has a first-class in-memory store and an overridable Store abstraction:**

```ts
declare class Store {
  constructor(filename: any); // filename === undefined ⇒ in-memory Map
  ready(): Promise<any>;
  batch(callback: any): Promise<any>; // buffers put/del during callback, one db.batch() at end
  get(key: any, deserialise: any): Promise<any>;
  put(key: any, value: any): Promise<void>;
  del(key: any): Promise<void>;
  size(): Promise<any>;
}
```

- `new Store()` with no filename backs onto a plain `Map` (`dist/index.js:141-150`, `inMemoryMap()` at `:191-211`); with a filename it opens `new Level(filename, { valueEncoding: "json" })`.
- `new Trie(store?)` defaults to an in-memory store (`dist/index.js:312`); `Trie` only type-checks the store via `assertInstanceOf(Store, …)` (`:313`, an `instanceof` check, `:61-70`) — **subclasses that override all six public methods are fully supported**, and Midgard already relies on this (`MidgardMpfRootViewStore extends Store`, `src/workers/utils/mpf.ts:2023`).
- **Hashing is eager and per-operation.** `trie.hash` is a plain property, always current after each `insert`/`delete` resolves. There is no lazy/deferred root mode. `Branch.insert` walks the path calling `fetchChildren()` per level (a `store.get` for **every present child** of each branch on the path, `dist/index.js:862,1077-1095`), then re-saves every node on the path bottom-up: each `Branch.save` recomputes `computeHash(prefix, merkleRoot(children))` where `merkleRoot` over 16 children costs **15 blake2b digests** plus 1 for the node hash (`dist/index.js:92-113, 1048-1050`).
- **Each single `insert`/`delete` is internally wrapped in `store.batch(...)`** (`dist/index.js:857, 931`): puts/dels buffer during the op and land as one `db.batch()` write per op.
- `Trie.save(previousHash)` issues `store.del(previousHash)` for superseded node versions and writes a `__root__` marker when `isRoot` (`dist/index.js:351-362`). Nodes are content-addressed (key = node hash).
- **`Trie.fromList(pairs, store?)` builds bottom-up, hashing each node exactly once** — O(N) digests total instead of O(N·depth·16) for N sequential inserts (`dist/index.js:377-410`). This is the primitive the scratch roots should use.
- `Trie.load(store)` reads the `__root__` marker and materializes only the root node (`dist/index.js:338-343`); children are fetched on demand.
- `Proof` generation/verification (`prove`, `verify`, `toCBOR`) operates against whatever store the trie has — no disk requirement.

**Consequence for the design:** we cannot defer root computation (the library hashes eagerly), and we do not need to. The per-op costs to attack are (a) store round-trips against LevelDB with JSON value encoding, and (b) the O(depth×16) digest count per op for sequential inserts where `fromList` would do O(1) per node. Root computation itself (`trie.hash`) is free.

### 1c. `MidgardMpf` / `MidgardMpfRootViewStore` — the existing wrapper

`src/workers/utils/mpf.ts`:

- `MidgardMpf.root()` returns `Effect.succeed(Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT))` (`mpf.ts:2413-2415`). **Correction (vs. parent plan C1):** the per-op `Effect.andThen(() => this.root())` chained onto `insert`/`delete` (`mpf.ts:2444, 2452`) is a 32-byte buffer copy, _not_ a root recomputation. The parent plan's statement that each op "recomputes the root" is wrong in mechanism; the real per-op cost is inside the library (§1b) plus the store layer below. Phase 0 instrumentation distinguishes the two by construction: run the same op batch against a memory-backed and a LevelDB-backed `MidgardMpf` — the delta is store cost, the memory-backed residual is hashing cost (see §5).
- `applyBatch(ops)` (`mpf.ts:2455-2485`; parent plan cites 2455-2478, actual span is 2455-2485) loops ops sequentially — one library-level `insert`/`delete` (and thus one LevelDB `batch()` write plus path-length×16 `get`s) per op — then persists the root marker once per call (`persistRootMarker`, `mpf.ts:2558-2572`).
- `MidgardMpfRootViewStore` (`mpf.ts:2023-2177`) is a dual-backend store: `level?: Level` or `memory?: Map`. `get` goes **straight to LevelDB** (no read cache) with JSON decode (`:2093-2097`, `JSON_LEVEL_ENCODING_OPTS` at `:51`), then overlays the current op's pending batch via `applyPendingBatch` — a **linear scan** of the pending op array (`mpf.ts:102-112`). This is fine while batches are per-insert-sized, and a quadratic trap if anyone naively wraps a whole block in one library-level batch (§2.3 rejects that).
- `del` **drops all node deletions** — only the `__root__` marker key is ever deleted (`:2129-2142`). The store is append-only for nodes; that is what makes `resetToRoot` (`:2526-2538`, a `Trie.load` against an older root) safe, and why the LevelDB directories grow monotonically.
- `MidgardMpf.create(name, path?)` opens LevelDB and reads the persisted root marker (`:2287-2311`); `createScratch` is Map-backed (`:2313-2321`).

### 1d. The commit path, per block

One worker thread per commit attempt (`new Worker(resolveWorkerEntry(import.meta.url, "commit-block-header.js"), …)`, `src/fibers/block-commitment.ts:568-588`; worker bootstrap at `src/workers/commit-block-header.ts:1011-1035`). Inside the worker:

1. `makeMpfs` re-opens both LevelDB tries fresh (`commit-block-header.ts:986`; `mpf.ts:181-231`), closed at the end (`:987-999`), wrapped in `withMpfRootTransactions` root-snapshot/rollback (`mpf.ts:1986-2013`).
2. `resolveCommitBaseLedgerEntries` (`commit-block-header.ts:137-224`) — **unconditionally** fetches the whole `confirmed_ledger` (`ConfirmedLedgerDB.retrieve` at `:145` → `retrieveAllEntries`'s unbounded `SELECT * FROM …`, `src/database/utils/ledger.ts:120-133`, re-exported at `src/database/confirmedLedger.ts:12`), and when the base is `confirmed_ledger` (the common steady-state case) **builds a full scratch trie over the entire ledger** to recompute its root (`computeLedgerMpfRootFromLedgerEntries` at `:214-215` → `keyValuePhasRoot` → N sequential in-memory inserts). This is a _second_ full-ledger trie build per commit that the parent plan does not call out. **Correction:** C2 must cover it, not just `payload_root_check`.
3. `alignCommitMpfsToBase` (`:226-266`) — if the persistent ledger MPF root ≠ base root, re-hydrates the **entire** ledger trie into LevelDB (`hydrateLedgerMpfFromLedgerEntries`, `mpf.ts:249-257`: reset + N ops through `applyBatch`).
4. `processMpfs` (`mpf.ts:1359-1968`), timed phases via `logCommitMpfPhaseTiming` (`:1342-1357`): `tx_delta_resolution` (`:1435-1443`), `transition_trace_build` (`:1858-1866`), `transaction_mpf_apply` (`:1885-1891`), `payload_root_check` (`:1898-1904`).
   - Base-root cross-check: with this caller, `initialLedgerEntries`/`selectedBaseUtxoRoot` are always supplied (`commit-block-header.ts:810-811`), so the `computeUtxoPayloadRoot` fallback at `mpf.ts:1713-1717` is skipped — but the check at `:1718-1727` still requires the base entries array in memory.
   - `buildTransitionTraceResult` (`mpf.ts:970-1075`): **one source event per included withdrawal, forced tx, L2 tx, and deposit** (`:1834-1839`; L2 events at `:1781-1815` carry `spent`-delete + `produced`-insert ledger ops). Events are applied strictly sequentially; each records `pre_utxos_root`/`post_utxos_root` into the `TransitionStep` (`:1011-1018`) after `applyTraceLedgerOpsToMpf` (`:769-825`), which additionally issues one `ledgerMpf.get` per op for presence validation (`:784-789`) before `applyBatch(ops)` (`:817`). **Correction (design constraint the parent plan glosses over):** the per-event `post_utxos_root` is protocol data committed into the trace root — "apply all ops, compute root once at batch end" is semantically impossible for the ledger trie across events. What _is_ removable is all per-op disk I/O and the per-op JSON codec work; the eager per-event hashing is required and stays.
   - `payload_root_check`: `materializeUtxoPayloadEntries(initialLedgerEntries, transitionLedgerOps)` re-materializes the **full post-state UTxO set** (`:1867-1870`, impl `:624-649`), then `computeUtxoPayloadRoot` builds a full scratch trie over it (`:1896-1904`) and asserts equality with the incrementally maintained ledger root (`:1905-1914`). **Correction:** this is a _consistency cross-check_ (Postgres-derived state vs. LevelDB trie), not an independent header root — the header's `utxosRoot` is the ledger MPF root. Removing it from the hot path loses a safety net, not a header input; that is exactly why §3 item (iii) replaces it with an audit cadence + CI invariant rather than deleting it.
5. Submission (`src/workers/commit-block-header/submission.ts`): event roots built as scratch tries (`event-roots.ts:13-66`), header roots handed to `buildUnsignedCommitTx` (`submission.ts:453-463`) — these are the roots that land in the L1 header datum and are fault-provable on-chain; the pending-finalization journal stores expected roots **and the full O(N) UTxO snapshot** (`preparePendingSubmission` `submission.ts:492-527`; members decoded back by `materializeConfirmedLedgerSnapshot`, `src/transactions/state-queue/confirmed-ledger-snapshot.ts:61-84`, which runs _yet another_ full scratch root build at `:71-82`). On finalization the whole `confirmed_ledger` table is cleared and re-inserted in batches of 100 (`replaceConfirmedLedgerWithEntries`, `:85-103`). **Correction:** C2's "commit cost independent of history size" exit criterion is violated by these O(N)-per-block journal/table rewrites even after the trie fixes; they are in scope here (item iii-b).

### 1e. Planner cost model (C5)

`src/workers/utils/commit-block-planner.ts`:

- `DEFAULT_COMMIT_BATCH_BUDGET_LIMITS` (`:70-83`): `maxL2TxCount: 10_000` (`:71`), `maxLedgerOpCount: 40_000` (`:73`), `maxTransitionStepCount: 40_000` (`:74`), `maxEstimatedCommitBuildMs: 30_000` (`:77`), `estimatedCommitBuildMsPerTx: 1` (`:82`). **Correction:** the ledger-ops/transition-steps caps are 40k today (parent plan's "raise to 400k" implies 10× from these, it never states the current values).
- `planCommitBatchBudgets` is called with the hard-coded defaults — no config override at the call site (`commit-block-header.ts:681-683`).
- The greedy packing loop is accidentally quadratic: each candidate rebuilds `[...selected, candidate]` and re-reduces byte totals over the whole prefix (`:423-432` with `estimateCommitBatchPlan`'s reduce at `:364-367`). Harmless at 10k; at 100k candidates it is ~10¹⁰ array-touches. Fixed in item (v).

### 1f. Cost model of the status quo (why C1 dominates)

Per ledger op against LevelDB today: ~depth levels × `fetchChildren` = up to 16 JSON `get`s per level (depth ≈ log₁₆|UTxO| ≈ 5–6 at 1M–16M entries, so up to ~80–100 gets), + one `level.batch` write of ~depth JSON-encoded node puts (~2 KB serialized branch each: 16×64-hex children strings + prefix, `dist/index.js:1096-1107`), + ~depth×16 blake2b digests. Even at optimistic 20–50 µs per cached LevelDB JSON get, an op costs 2–10 ms wall-clock — 10k txs ≈ 20–40k ledger ops ≈ **40–400 s**, matching the observed Stage-C ceiling. The digest cost alone (WASM blake2b via the `blake2b` package, ~0.2–1 µs/digest for ≤512 B inputs) is ~50–100 µs/op ≈ 7–15 s per 150k ops — relevant but second-order once disk I/O is gone (and it is the number the growth-regression test in §6 watches).

---

## 2. Architecture decisions

Sections 2.1–2.8 record the staged library-backed candidates and the evidence
that eliminated them. Section 2.9 is the selected Architecture G direction;
its production boundary is maintained in the companion integration plan.

### 2.1 Historical candidate: keep `@aiken-lang/merkle-patricia-forestry`

**Initial decision (superseded by Section 2.9):** all root computation stays on
the audited upstream library; change _stores_ and _call patterns_ only.

Rationale: the roots feed the L1 header datum and the on-chain fault-proof validators (`buildUnsignedCommitTx`, `submission.ts:453-463`); the Aiken on-chain implementation and this JS library are co-developed and cross-tested upstream. Every byte of hashing semantics (nibble paths, sparse-merkle-16 neighbor layout, leaf/branch prefix encoding — mirrored independently in `src/workers/utils/mpf/phas.ts:198-256` for proof verification) is consensus-critical.

Rejected alternatives:

- **Custom Rust/WASM MPF engine (historical rejection, superseded by measured
  evidence).** It was initially rejected because it re-implements
  consensus-critical hashing semantics with fault-provable failure modes and
  therefore required the complete differential harness first. Architectures
  B–F then failed the binding retained-growth gate; the harness-backed
  retained-session Architecture G design cleared the selection gate and is now
  the implemented production candidate. Its exact ownership, RPC, promotion,
  and recovery boundary is specified in
  `phase-3-architecture-g-production-integration.md`.
- **Different JS trie library / hand-rolled.** No other implementation matches Aiken MPF semantics; disqualified outright.

### 2.2 C1a — kill per-op store round-trips and use `fromList` for scratch roots; per-event ledger hashing stays

**Decision:** (a) `MidgardMpf.applyBatch` and the whole block build run against a memory overlay (§2.3), so each library-level op's `store.batch` lands in a Map instead of a `level.batch`; (b) drop the vestigial `Effect.andThen(() => this.root())` on `insert`/`delete` (`mpf.ts:2444,2452`) — cheap, but it is dead weight and its removal makes the "no per-op root" claim true in code; (c) `applyBatch` stops persisting the root marker to LevelDB per call — the marker write moves to the block-level flush (§2.3); (d) **all seven scratch roots switch from sequential `applyBatch` inserts to `Trie.fromList`** on a plain in-memory `Store`, cutting digest counts from O(N·depth·16) to O(N) — this is the single biggest CPU win available without touching the library (verified `fromList` hashes each node once, `dist/index.js:377-410`).

Where the parent plan says "compute root once at batch end": for the transactions trie and scratch roots this is exactly what happens (`fromList` produces the root once). For the ledger trie the per-_event_ root is protocol-mandated (§1d Correction), so "once per batch" applies at the store-I/O level (one flush per block), not the hashing level. Phase 0 instrumentation splits the two costs by running identical op streams against memory-backed vs. level-backed stores (§5).

Rejected alternative: wrapping the entire block in one library-level `store.batch`. The library asserts non-reentrant batches (`dist/index.js:155`) — each `insert` opens its own — and `applyPendingBatch`'s linear scan (`mpf.ts:102-112`) would go quadratic in ops. The overlay store achieves the same write-coalescing without fighting the library.

### 2.3 C1b — Map-backed overlay store over LevelDB, block-scoped, with early node spill and root-marker-gated durability

**Decision:** extend `MidgardMpfRootViewStore` with an overlay mode: a `Map<string, MpfStoredValue>` write buffer layered over the `Level` backend. Semantics:

- `get`: overlay hit → return (no JSON decode — values are stored decoded); miss → `level.get` read-through, then _cached in a bounded read cache_ (top-of-trie nodes are re-read every op via `fetchChildren`; caching them is most of the read win).
- `put`: always into the overlay. Never a direct `level.put` during block build.
- `del`: **honored for overlay-resident keys only** (drop the entry), ignored for base keys — this prunes superseded intra-block node versions (the library `del`s the previous hash on every re-save, `dist/index.js:351-354`) while preserving today's append-only invariant for anything already durable, so `resetToRoot` to any pre-block root still works (`mpf.ts:2526-2538`).
- **Early spill:** when the overlay exceeds `MPF_OVERLAY_SPILL_BYTES`, spill node entries to LevelDB in the background via `level.batch`. This is _safe before commit_ because nodes are content-addressed and the store is append-only — orphaned nodes are garbage, not corruption; state is defined solely by the `__root__` marker. This bounds RAM without giving up atomicity.
- **Durability point:** the `__root__` marker (`ROOT_KEY`, `mpf.ts:50`) is written to LevelDB _only_ in a final flush — `flush(root)` = one `level.batch([...remaining node puts, put __root__])`. Atomicity of the marker-inclusive batch is what makes the flush atomic in the only sense that matters.

**Crash-consistency contract (anchored to what "committed" means today):** the node's durable block state advances when `PendingBlockFinalizationsDB.preparePendingSubmission`/`markSubmitted` journal the block (`submission.ts:492-527, 1012-1035`) and, at finalization, when `confirmed_ledger` is replaced (`confirmed-ledger-snapshot.ts:85-113`). The worker outcome classes that preserve MPF roots are enumerated in `shouldPreserveCommitMpfRoots` (`commit-block-header.ts:268-284`). The flush therefore runs **after `preparePendingSubmission` succeeds and strictly before the worker returns a root-preserving output**; on any failure output the overlay is discarded (cheaper than today's `resetToRoot`). On restart: `MidgardMpf.create` reads the persisted marker (`readPersistedRoot`, `mpf.ts:2575-2584`); if a crash occurred pre-flush, the marker still names the previous block's root and the existing recovery path (`alignCommitMpfsToBase` re-hydration, `commit-block-header.ts:226-266`) is unchanged — except after item (iii) it hydrates from the journal delta instead of a full rebuild in the common case. Crash _mid-flush_ leaves either the old marker (batch not applied — LevelDB batches are atomic) or the new marker with all its nodes (batch applied); no intermediate is observable.

Rejected alternatives:

- **Full in-memory copy of the ledger trie** (load everything at startup, LevelDB as backup): O(UTxO set) RAM permanently (≈2–3 KB/node × ~1.5 nodes/UTxO ⇒ ~4 GB at 1M UTxOs), long warmup, and a second source of truth to reconcile. The overlay keeps resident memory O(block delta), not O(state).
- **Library's own in-memory `Store` per block, seeded lazily:** the library store has no read-through fallback; a miss is a hard error. The overlay must live in our subclass, which already exists and is already the proven extension point (`mpf.ts:2023`).
- **Flush before journaling:** would advance trie state past the durable journal on crash, forcing root-mismatch re-hydration on every crash recovery. Flush-after-journal makes the marker always ≤ journal state, and re-applying a block delta forward is cheap; rebuilding backwards is not.

### 2.4 C2 — the incremental payload root _is_ the persistent ledger trie; the rebuild becomes an audit

**Decision:** stop treating the full-ledger scratch rebuild as a per-commit gate. Concretely:

1. The header's `utxosRoot` continues to come from the persistently maintained ledger MPF (it already does — `utxoRoot = ledgerMpf.rootHex()`, `mpf.ts:1895`). Nothing new needs to be "maintained incrementally"; what changes is that we _trust_ the incremental trie between audits instead of re-deriving the same root from Postgres every block.
2. `payload_root_check` (`mpf.ts:1896-1914`) and the base-root scratch build (`commit-block-header.ts:214-215`) leave the hot path, gated by `MPF_PAYLOAD_ROOT_CHECK=every_block|periodic|off`. The current default remains `every_block`; a change to `periodic` requires the separate post-gate review. The base check is replaced by comparing the persistent ledger-MPF root directly against the state-queue tip header's `utxosRoot` (available at `commit-block-header.ts:180-183`) — an O(1) equality that catches the same divergence class the O(N) rebuild caught.
3. The unconditional `ConfirmedLedgerDB.retrieve` at `commit-block-header.ts:145` becomes lazy: fetched only on the mismatch/recovery path or when an audit is due. `processMpfs` gets `initialLedgerEntries?: undefined` support — the only remaining consumers are the payload check (now optional) and `materializeUtxoPayloadEntries` for the journal (replaced next).
4. **(iii-b)** The pending-finalization journal stores the **block delta** (spent outrefs + produced entries + expected roots) instead of the full UTxO member list (`submission.ts:492-527`), `materializeConfirmedLedgerSnapshot` applies the delta to the previous snapshot instead of rebuilding a root over all members (`confirmed-ledger-snapshot.ts:61-84`), and `replaceConfirmedLedgerWithEntries`'s full table clear+reinsert (`:85-103`) becomes delta DELETE + UPSERT. Without this, per-block work stays O(state) and the exit criterion fails regardless of trie work.
5. **Bootstrap/migration:** on first startup with the new engine, if the LevelDB root marker is absent or does not equal the confirmed tip's `utxosRoot`, run one final full rebuild (`hydrateLedgerMpfFromLedgerEntries` from `confirmed_ledger` — the machinery at `mpf.ts:249-257` / `synchronizeCommitMpfStoresFromConfirmedLedger` `mpf.ts:305-316` already exists) and stamp a `migration_version` key in the LevelDB store. Existing deployments therefore migrate with one O(N) pass, exactly once.
6. **Divergence detection:** (a) background audit fiber recomputes the full payload root off the commit path every `MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS` (default 500) or `…_INTERVAL_MS` (default 6 h), whichever first, using a Postgres snapshot read + `Trie.fromList`; mismatch ⇒ `mpf_payload_audit_divergence` metric + ERROR log + **halt further commits** (a wrong `utxosRoot` on L1 is fault-provable — stopping is strictly better than committing); (b) offline CLI `mpf-audit` (new `src/commands/mpf-audit.ts` following the existing commands layout) for operator-initiated verification and for CI corpora; (c) the CI differential harness (§6) asserts equivalence on every merge.

Rejected alternatives: keeping the every-block check but making it incremental-only (defeats the point — the check's value is precisely that it derives the root from an independent data path; a cheap dependent check detects nothing); maintaining a _second_ persistent payload trie keyed differently (there is no second keying — payload entries are the same `(outref, output)` pairs the ledger trie holds; verified identical key/value construction `mpf.ts:233-239` vs `:624-657`).

### 2.5 C1c — parallel root computation in worker threads

**Decision:** after `transition_trace_build` completes (it is inherently sequential — each event's ops depend on the prior event's trie state), the remaining independent root builds — transactions-source (`fromList` over ≤100k pairs), transition-trace, event-to-step, deposits/withdrawals/forced, plus the optional payload audit — fan out across a small `worker_threads` pool. Workers follow the existing conventions: entry modules under `src/workers/` (bundled by `tsup src/workers/*`, `package.json:13`), resolved via `resolveWorkerEntry` (`src/fibers/resolve-worker-entry.ts:5-26`), `workerData` in / `postMessage` out like `commit-block-header.ts:1011-1035`. Inputs are `(domain, sorted key/value Buffer pairs)`; output is the root hex — trivially serializable via transferable ArrayBuffers.

**Honest gain estimate:** after items (i)–(iii), each scratch root is a `fromList` over ≤~100k entries ≈ 0.3–1 s; running ~5 of them in parallel saves roughly `(k−1)/k` of their sum, bounded by the largest (the transactions-source root at 100k txs). Expected saving ≈ 1–3 s at 100k-tx blocks, ~0.5–1 s at 50k. This is the _lowest-value_ item in the plan and is sequenced last among the build-path changes; it exists because Phase 4's pipelining squeezes total budget further. If Phase 0 numbers post-(iii) show the trace build itself dominating, this item may be descoped without affecting the exit criterion.

### 2.6 C5 — measured EWMA cost model, persisted in Postgres

**Decision:** replace `estimatedCommitBuildMsPerTx: 1` with an EWMA of measured per-tx build cost. Source signal: total MPF build duration (`mpf_processing` span already logged at `commit-block-header.ts:785-817`, plus the per-phase timers) divided by processed tx count, sampled per successful commit. `ewma ← α·sample + (1−α)·ewma`, α default 0.2. The planner packs against `maxEstimatedCommitBuildMs` using `ewma × safetyFactor` (default 1.5), floored at 0.05 ms/tx and capped at 50 ms/tx to bound pathological feedback.

**Persistence: a Postgres table** (`commit_build_calibration(id, ms_per_tx_ewma, sample_count, updated_at)`), read at worker start, written after each commit. Justification over alternatives: (a) env/config file — static, cannot be written by the node, wrong tool for a _measured_ quantity; (b) LevelDB — the MPF stores are owned by the commit worker and deleted/re-synced during recovery (`deleteMpfStore`, `mpf.ts:354-365`), which would silently reset calibration exactly when it matters most; (c) Postgres is already the durable operational store every worker can reach (`Database` service), survives MPF-store resets and container rebuilds, and is trivially inspectable by operators.

**Cap raise is gated, not blind:** `maxL2TxCount` 10k→100k, `maxLedgerOpCount` 40k→400k, `maxTransitionStepCount` 40k→400k ship as _configurable_ limits immediately (item v), but defaults are raised only when the Phase 0 benchmark shows p95 build <10 s at 50k txs with the new engine. The EWMA + 30 s budget then provides continuous backpressure if a deployment's hardware can't keep up — the planner self-limits batch size instead of timing out.

Rejected alternative: one-off static recalibration from Phase 0 measurements — wrong across hardware profiles, UTxO-set sizes, and future engine changes; it re-creates the exact failure mode C5 describes with a different constant.

### 2.7 Safety net — differential root-equivalence harness is mandatory, first

**Decision:** no stage of this plan merges until the harness (§6) can replay a recorded corpus through the legacy and reworked engines and assert byte-identical roots per block — including every per-event `pre/post_utxos_root` in the transition trace, since those are committed under `transitionTraceRoot`. The reference and rollback surfaces stay callable throughout: `MPF_ENGINE` accepts `legacy`, `overlay`, `event_flat`, and the separate `architecture_g` candidate; `MPF_SCRATCH_BUILD` accepts `insert` and `fromlist`. Current defaults remain `legacy` and `insert`; gates do not flip them implicitly. Rationale: a root divergence that reaches L1 is a fault-provable, slashing-grade failure (§7) — this is the one place in the throughput program where "measure twice" is literal.

### 2.8 C1d — block-scoped raw path cache + live current-root arena (growth-gate follow-up)

**Decision:** replace the experimental per-chunk serialized checkpoint with two bounded, block-scoped tiers while preserving the same library hashes and strict per-event roots:

1. Before applying events, run one breadth-first `Level.getMany` traversal for the block's complete touched-path set. Retain the returned raw `MpfStoredValue` nodes in a dedicated path cache keyed by content hash, bounded by the unique touched-node DAG rather than the ledger size. Collapse the attached trie after the traversal; each later chunk reattaches from this cache without another Level read.
2. Keep the library's block-created mutable node objects live in a current-root arena across chunks. The patched library issues a module-private, one-use mutation proof immediately before its own synchronous save callback; only that proof lets the store retain an object by identity without clone/authenticate/map work for every intermediate content hash. A chunk checkpoint traverses from the current root, authenticates only reachable dirty objects, clears unreachable mutation intermediates, and does not materialize or collapse the live arena. Direct/external retained-node writes still require an immutable detached snapshot and authenticate before entering the arena.
3. Before a fork or final promotion, traverse the hydrated current root, clone-detach and authenticate the reachable live closure, and clear the mutable tier. At promotion, mark from the current root across those immutable snapshots, the raw touched-path cache, and every unpromoted ancestor/fork store; import the complete reachable unpersisted closure, discard superseded/orphan arena entries, serialize each reachable block-created node exactly once, and atomically batch those nodes with the root marker. Before promotion, the durable-write counter must remain zero. Exceeding either fixed node/byte cap fails closed and poisons/discards the overlay; it never silently spills during a speculative build. Initial implementation caps are 1,000,000 nodes / 1 GiB for both the raw path cache and the combined live/snapshot arena.

Memory is O(unique touched path DAG + current reachable block delta × bounded radix depth + mutation intermediates since the last checkpoint), not O(confirmed ledger size). Hashing and serialized LevelDB format stay unchanged, so rollback remains `MPF_PATH_HYDRATION_MODE=whole_block` plus the existing legacy engine switches. The exact 2026-07-10 paired diagnostics in §9 predict a useful absolute improvement but do **not** prove the ±10% slope gate: after subtracting repeated chunk prefetch and intermediate materialization from the observed pair, a conservative extrapolation is still about +23%. This design is therefore the next single measured architecture, not a performance claim or a reason to flip defaults. If its residual is still dominated by per-event path mutation, the next decision point is an event-atomic multi-path mutation primitive that recomputes shared ancestors once while retaining each event's post-root; that higher-risk fork change requires the full differential and proof matrix before use.

### 2.9 Contingency decision — persistent authenticated native owner (Architecture G)

The raw-path/dirty-V2 Architecture E and one-shot native/WASM Architecture F both preserved exact roots but failed the ±10% growth gate (§9). Their paired counters isolate the reason: every ephemeral build authenticates a depth-growing touched proof and then authenticates/transfers a depth-growing dirty closure in addition to the canonical mutations themselves. A smaller transfer encoding or same-root retry cache cannot remove that work; even deleting all fetch cost from F leaves a +26.8557% projection.

**Decision:** the next gate-capable architecture is a long-lived native MPF owner. It holds a compact, authenticated, marker-keyed representation of the complete durable Forestry closure and its 15-node branch Merkle caches; creates append-only copy-on-write speculative generations; ingests only the fixed ordered event-op stream; emits every mandatory per-event root plus the candidate root; and retains the generation behind an opaque handle through Phase 4's submit/promote tail. It does not change leaf/branch encoding, BLAKE2b-256, path compression, per-event root semantics, the Level node format, or the atomic nodes-plus-root-marker promotion. Legacy/event-flat behavior remains the default and rollback path.

This conditionally supersedes §2.1's rejected full-memory alternative only because three facts materially change the tradeoff: the owner is a compact native representation rather than a JS object graph, the resident index is a non-authoritative cache tied to the durable marker and rebuilt on any marker/schema/digest mismatch, and Architecture F provides exact evidence that repeated proof/artifact work—not canonical mutation alone—causes the failing slope. The Level marker remains the sole state authority.

Crash recovery cannot depend on an in-memory handle. Before signing/submission, the pending journal must durably bind the base/candidate roots, every ordered event root, and a compact canonical ordered event-op replay log. A service crash after submission but before promotion restarts from the authoritative marker, rebuilds or validates the resident index, replays the log, requires byte-identical roots, and only then atomically promotes. Stale markers, expired handles, cap breaches, corrupt caches/logs, or replay divergence fail closed. The sidecar/index may lag promotion and is discarded rather than repaired silently.

The minimal retained-session prototype clears the narrow build projection (§9), but setup is not waived. Before production wiring, acceptance must load and authenticate the complete marker-reachable index, report startup wall/RSS and steady-state bounds, and prove journal replay/restart across each submission/promotion crash boundary. If those operational gates fail, Architecture G remains a prototype and defaults do not flip.

---

## 3. Implementation items

Ordered; each stage lands independently behind its flag and is individually revertible.

### (i) Store-level batching + dead `root()` removal — `src/workers/utils/mpf.ts`

1. Delete the `Effect.andThen(() => this.root())` from `insert`/`delete` (`:2444, :2452`); change their return type to `Effect<void, MpfError>`; `applyBatch` computes `rootAfter` once at `:2475` (already does).
2. Add overlay mode to `MidgardMpfRootViewStore`: fields `overlay?: Map<string, MpfStoredValue>`, `readCache?: LruMap`, `spillThresholdBytes`; rework `get` (`:2089-2100`) to check overlay → readCache → level; `put` (`:2102-2127`) to write overlay; `del` (`:2129-2142`) to delete overlay-resident keys. Keep `applyPendingBatch` for the per-op library batch window (unchanged semantics).
3. New methods on `MidgardMpf`: `beginBlockOverlay()`, `flushBlockOverlay(root: Buffer)` (single `level.batch` including the `__root__` put — replaces `persistRootMarker`'s separate `level.put`, `:2558-2572`), `discardBlockOverlay()`, `spillIfNeeded()` (background, batches node puts only, never the marker).
4. `applyBatch` in overlay mode: no per-call marker persistence; marker persistence happens only via `flushBlockOverlay`.
5. Unit tests extend `tests/mpf.test.ts` (currently 547 lines, covers batch persist/rollback DBs): overlay get/put/del layering, spill-then-crash equivalence, marker atomicity.

Exit: measured `transaction_mpf_apply` and `transition_trace_build` on the Phase 0 bench drop by the store-I/O share (expected ≥10×); roots identical via harness.

### (ii) Block-scoped overlay wiring in the commit worker — `src/workers/commit-block-header.ts`, `src/workers/commit-block-header/submission.ts`

1. `runCommitBlockHeaderWorkerProgram` (`commit-block-header.ts:976-1009`): after `makeMpfs`, `beginBlockOverlay()` on both tries; replace `withMpfRootTransactions` rollback semantics with `discardBlockOverlay()` for non-preserving outputs (`shouldPreserveCommitMpfRoots`, `:268-284`) — `resetToRoot` remains only for the legacy engine path.
2. Flush placement: in `submitTxBackedCommit`/`submitDepositOnlyCommit` immediately after `preparePendingSubmission` succeeds (`submission.ts:492`, `:876`), before returning the root-preserving output; on `deferProcessedCommitPayloadUntilConfirmation` (`commit-block-header.ts:895-900`), flush too (the processed payload is durable in `ProcessedMempoolDB` — verified comment `:879-885`).
3. `alignCommitMpfsToBase` (`:226-266`) unchanged in logic; hydration now runs through the overlay (one flush at the end) — turning recovery hydration from N `level.batch`es into one.

Exit: crash-recovery tests (§6) green; commit path never writes LevelDB between `beginBlockOverlay` and flush (asserted by a store write-counter in tests).

### (iii) Incremental payload root, lazy base fetch, migration — `src/workers/utils/mpf.ts`, `src/workers/commit-block-header.ts`, `src/workers/utils/mpf/phas.ts`

1. `keyValuePhasRootWithCount` (`phas.ts:60-87`): replace `createScratch`+`applyBatch` with `Trie.fromList(entries, new Store())` (entries are already sorted/deduped at `:37-48`; `fromList` doesn't require sorting but determinism is nice). Same for `keyValuePhasProof`/`keyValuePhasNonMembershipProof` scratch builds (`:111-118, :162-169`) — gated by `MPF_SCRATCH_BUILD`.
2. `resolveCommitBaseLedgerEntries` (`commit-block-header.ts:137-224`): compare `ledgerMpf` persisted root against the state-queue tip header `utxosRoot` (`:180-183`) first; only on mismatch fetch `confirmed_ledger`/journal snapshot and re-hydrate. Remove the unconditional `:145` fetch and the `:214-215` scratch root build from the match path.
3. `processMpfs`: gate `payload_root_check` (`mpf.ts:1896-1914`) and the base check's entry materialization on `MPF_PAYLOAD_ROOT_CHECK`; when off/periodic-not-due, `utxoPayloadEntries` is not materialized (see 4).
4. **(iii-b)** Journal delta: extend `PendingBlockFinalizationsDB` schema with delta columns (spent outrefs, produced `(outref, output)` pairs — exactly `transitionLedgerOps`, `mpf.ts:1840-1842`); `buildPendingJournalMetadata`/`preparePendingSubmission` (`submission.ts:492-527`) store the delta; `materializeConfirmedLedgerSnapshot` (`confirmed-ledger-snapshot.ts:61-84`) gains a delta path (previous snapshot + delta → entries, root asserted against `expectedRoots.utxosRoot` — no scratch rebuild); `replaceConfirmedLedgerWithEntries` (`:85-103`) gains `applyConfirmedLedgerDelta` (DELETE spent + UPSERT produced, one transaction). Full-snapshot columns remain readable for one release for rollback.
5. Migration & audits per §2.4: startup one-shot rebuild + `migration_version` stamp; background audit fiber (new `src/fibers/mpf-payload-audit.ts`, registered alongside existing fibers); `src/commands/mpf-audit.ts` CLI (reuses `synchronizeCommitMpfStoresFromConfirmedLedger`-style logic read-only).

Exit: commit build time flat as `confirmed_ledger` grows 100k→1M (growth regression test, §6); audit fiber proves equivalence on schedule in soak.

### (iv) Parallel scratch roots — new `src/workers/mpf-root-builder.ts`

Worker entry taking `{ domain, entries: [key,value][] (transferables) }`, returning `{ rootHex, count }`; pool of `MPF_ROOT_WORKERS` (default `min(4, cores-2)`) spawned via `resolveWorkerEntry`; `countedRootFromEncodedEntries` (`mpf.ts:676-697`) and `event-roots.ts` dispatch through the pool when entry count > `MPF_PARALLEL_ROOT_MIN_ENTRIES` (default 5k), inline otherwise. Add `mpf-root-builder` to the tsup workers glob (already covered by `src/workers/*`).

Exit: wall-clock `commit MPF phase` sum shrinks by ≈ sum-minus-max of scratch-root times at 50k-tx bench; no regression at small blocks.

### (v) Planner recalibration + cap lift — `src/workers/utils/commit-block-planner.ts`, `src/services/config.ts`, new DB table

1. Make `planCommitBatchBudgets` incremental (running accumulators; kill the O(n²) loop `:413-451`).
2. `CommitBatchBudgetLimits` from `NodeConfig` (new env vars, §4) instead of the hard-coded literal at the `commit-block-header.ts:681` call site.
3. `commit_build_calibration` table + `CalibrationDB` module (`src/database/`); EWMA update after each successful commit using the `mpf_processing` duration (`commit-block-header.ts:814-817`) and processed count; planner reads `msPerTx = clamp(ewma × safety, 0.05, 50)`.
4. Raise default caps to 100k/400k/400k **in a separate commit** merged only after the §6 gate shows 50k <10 s p95.

Exit: planner never selects a batch whose _measured-model_ estimate exceeds the 30 s budget; timeout-instead-of-backpressure incidents go to zero in soak.

### (vi) Differential harness (built first, see §2.7 and §6) — `tests/mpf-differential.test.ts`, `src/commands/mpf-replay.ts`

### (vii) Contingency (not committed): library fork with incremental 16-ary merkle update (4 digests instead of 15 per branch save) if post-(iii) profiling shows hashing blocking <10 s at 50k. Only behind the harness.

---

## 4. Config surface and current defaults

All new; existing `LEDGER_MPF_DB_PATH` / `TRANSACTIONS_MPF_DB_PATH` (`config.ts:87-88,429-433,575-576`) unchanged.

| Env var                                                                                      | Default                    | Purpose                                                                                                                                                                                                             |
| -------------------------------------------------------------------------------------------- | -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `MPF_ENGINE`                                                                                 | `legacy`                   | Selects `legacy`, `overlay`, `event_flat`, or the separate `architecture_g` production candidate. Architecture G requires an exact pinned native binary SHA-256 and remains opt-in until every closure gate passes. |
| `MPF_SCRATCH_BUILD`                                                                          | `insert`                   | Scratch-root build primitive (`fromlist` remains an explicit alternative); no default flip is authorized yet.                                                                                                       |
| `MPF_PATH_HYDRATION_MODE`                                                                    | `whole_block`              | `whole_block` (strict default), `chunked` (serialized-checkpoint rollback), or `chunked_arena` (raw-cache/live-arena candidate); remains `whole_block` until the growth gate passes                                 |
| `MPF_HYDRATION_CHUNK_OPS` / `MPF_RETAIN_HYDRATED_DEPTH`                                      | `512` / `2`                | Experimental chunk arena bounds; ignored by `whole_block` mode                                                                                                                                                      |
| `MPF_OVERLAY_SPILL_BYTES`                                                                    | `536870912` (512 MiB)      | Overlay spill threshold                                                                                                                                                                                             |
| `MPF_PAYLOAD_ROOT_CHECK`                                                                     | `every_block`              | Hot-path cross-check gating; retain the strict default until a separate post-gate review.                                                                                                                           |
| `MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS` / `_MS`                                                  | `500` / `21600000`         | Background audit cadence                                                                                                                                                                                            |
| `MPF_PARALLEL_ROOTS`                                                                         | `false`                    | Stage iv toggle; remains opt-in pending the explicit default decision.                                                                                                                                              |
| `MPF_ROOT_WORKERS` / `MPF_PARALLEL_ROOT_MIN_ENTRIES`                                         | `min(4, cores−2)` / `5000` | Pool sizing                                                                                                                                                                                                         |
| `COMMIT_MAX_L2_TX_COUNT` / `COMMIT_MAX_LEDGER_OP_COUNT` / `COMMIT_MAX_TRANSITION_STEP_COUNT` | `10000`/`40000`/`40000`    | Configured planner caps. A 10x increase requires the formal performance, live, soak, and separate review gates.                                                                                                     |
| `COMMIT_BUILD_COST_MODEL`                                                                    | `static`                   | C5 toggle; `ewma` is implemented but not the current default.                                                                                                                                                       |
| `COMMIT_BUILD_EWMA_ALPHA` / `_SAFETY_FACTOR`                                                 | `0.2` / `1.5`              | EWMA tuning                                                                                                                                                                                                         |

## 5. Observability

- **Existing (Phase 0 must already scrape):** `Commit MPF phase <p> completed duration_ms=…` for `tx_delta_resolution`, `transition_trace_build`, `transaction_mpf_apply`, `payload_root_check` (`mpf.ts:1342-1357` and call sites); base-hydration log (`commit-block-header.ts:779-784`); `pipeline_trace phase=mpf_processing_finished` (`:815-817`).
- **New store-level counters** on `MidgardMpfRootViewStore`, logged per phase: `level_gets`, `level_get_ms`, `overlay_hits`, `readcache_hits`, `level_batch_writes`, `bytes_flushed`, `json_codec_ms`. These are what let Phase 0 attribute per-op cost to disk vs. hashing: hashing = phase duration − store time; additionally the bench runs the recorded op stream against a pure-memory `MidgardMpf` as the hashing-only baseline.
- **Overlay gauges:** `mpf_overlay_entries`, `mpf_overlay_bytes`, `mpf_overlay_spills`, `mpf_flush_ms`, `mpf_flush_bytes`.
- **Trie sizes:** ledger trie `size` property and `diagnostics().entries` (`mpf.ts:2551-2556`) per block; LevelDB directory bytes (append-only growth — feeds the compaction task below).
- **EWMA state:** `commit_build_ms_per_tx_ewma`, `sample_count`, plus per-commit `measured_ms_per_tx` — dashboard alongside planner `stop_reason` distribution (logged at `commit-block-header.ts:689-692`).
- **Audit results:** `mpf_payload_audit_last_block`, `mpf_payload_audit_duration_ms`, `mpf_payload_audit_divergence` (0/1 — page immediately on 1; commits halt).

## 6. Test & verification plan

**Differential root-equivalence harness (mandatory, first).**

- _Corpus:_ recorded from the Phase 0 soak benchmark — per block, an NDJSON record of `(sourceEvents with ledgerOps, transactionOps, initialLedgerEntries-ref, config)` captured by a tap in `processMpfs` behind `MPF_RECORD_CORPUS=path`; plus a synthetic generator (seeded) producing adversarial op patterns: delete-then-reinsert same key, single-child branch collapse (the `delete` neighbor-merge path, `dist/index.js:925-960`), keys sharing long common prefixes, empty events (invalid-withdrawal events have `ledgerOps: []`, `mpf.ts:1737-1745`). Corpora checked into a fixtures bucket; a small one vendored in-repo for CI.
- _Replay:_ `src/commands/mpf-replay.ts` and the Architecture G production
  probe run the same ordered corpus through the retained legacy reference and
  candidate engines. They assert byte-equality of `utxoRoot`, raw transaction
  root, `txRoot`, `transitionTraceRoot`, `eventToStepRoot`,
  deposits/withdrawals/forced roots, and every
  `TransitionStep.pre/post_utxos_root`. The legacy path remains callable, so the
  harness never needs an old checkout.
- _CI:_ runs on the vendored corpus per merge; nightly on the full recorded corpus. Also cross-checks `computeUtxoPayloadRoot`-vs-ledger-root on the final state of each corpus block (the invariant that leaves the hot path).
- _Proof compatibility:_ for a sample of keys per corpus block, `prove` on new-engine tries and `verify` via the independent verifier in `phas.ts:413-442` against the committed root.

**Crash-recovery tests.** The implemented production matrix covers protocol
corruption, worker/lease release, real native-child SIGKILL and stale owner
epochs, failure before and after the atomic Level promotion batch, and exact
post-submit journal replay. Restart must expose the authoritative old or
candidate marker only, reject mismatched replay/binary/root data, converge to
the submitted candidate when recovery is required, and produce
harness-identical roots. The final combined suite and one live restart/recovery
exercise remain mandatory.

**Growth regression (C2 guard):** the formal Architecture G gate builds one
identical 10k-op stream against fresh immutable 100k / 300k / 1M Level
fixtures, with three fresh processes per fixture. It requires the maximum and
minimum fixture medians to differ by no more than 10%, deterministic complete
root tuples, unchanged fixture marker/logical digest/record count, and a zero
confirmed-ledger full-scan counter. This is the implemented guard available
today; if closure requires literal SQL statement interception in addition to
the counter, add and record that probe rather than claiming it indirectly.

**Perf gate for cap raise:** run 20 fresh Architecture G processes over a
dependency-closed first 50k prefix of one explicitly named, manifest-verified
Phase 1 corpus slice. Require nearest-rank p95 build below 10 seconds,
deterministic complete root tuples, and unchanged corpus/slice/fixture
identities before any cap or default review.

**Planner tests:** extend `tests/commit-block-planner.test.ts` for the incremental packer (property: identical selection to the old quadratic packer on random inputs) and EWMA clamp/persistence behavior.

## 7. Risks & rollback

- **Root divergence (worst case).** A wrong root in a committed L1 header is fault-provable on-chain: the operator's block can be disproven, the operator slashed, and the chain halted/rolled back per protocol — this is the maximum-severity failure available to this codebase, worse than any crash. Mitigations: harness-first sequencing (§2.7), per-stage flags, `every_block` payload check retained through rollout, audit-halt on divergence (§2.4.6), proof cross-verification via the independent `phas.ts` verifier. Rollback: flip `MPF_ENGINE=legacy` / `MPF_SCRATCH_BUILD=insert` — no data migration needed, the LevelDB format is unchanged (same content-addressed nodes + `__root__` marker).
- **Memory footprint of the overlay at 100k-tx blocks.** Estimate: ~400k ledger ops × ~6 new node versions/op, minus intra-block supersession pruning (§2.3 `del` handling) and shared upper-path dedup ⇒ ~0.8–1.5M live overlay entries; each a decoded branch object ≈ 2.1 KB of strings (16×64-hex children + prefix) + V8 overhead ≈ ~3 KB budgeted ⇒ **~2.5–4.5 GB unspilled worst case**. The 512 MiB spill threshold caps residency; spill cost is sequential append-only `level.batch` writes off the critical path. Watch `mpf_overlay_bytes`/`mpf_overlay_spills`; if spill churn dominates, raise threshold on bigger hosts (config).
- **LevelDB flush latency.** Final flush ≤ overlay remainder (≤ spill threshold) in one batch; LevelDB sustains >100 MB/s sequential batch writes, so ≤512 MiB ⇒ single-digit seconds worst case, and typically ≪1 s because spill has drained most of it. Flush sits after journaling, off the pre-submission critical path but inside the worker turnaround — tracked by `mpf_flush_ms` and included in the EWMA (so the planner sees it).
- **Hashing may still block <10 s at 50k txs** if the WASM `blake2b` path is not active or trace depth is worse than modeled (§1f: ~7–15 s per 150k ops at JS-fallback speeds). Mitigation: Phase 0 measures digest throughput explicitly; contingency (vii) library fork; scope guard — the exit criterion is measured at 50k, and the planner EWMA keeps production safe even if the target slips.
- **Migration failure modes.** (a) One-shot rebuild crashes mid-way: marker not yet stamped ⇒ rebuild re-runs idempotently (hydration is reset+rebuild, `mpf.ts:249-257`). (b) Journal delta schema rollout: old rows lack delta columns ⇒ materializer falls back to full-member path (kept for one release). (c) Rollback after delta-only journals exist: legacy materializer can't read delta rows ⇒ rollback procedure documents replaying via `synchronizeCommitMpfStoresFromConfirmedLedger` (`mpf.ts:305-316`) before downgrade.
- **Unbounded LevelDB growth** (pre-existing, worsened slightly by spilled orphans): nodes are never deleted (`mpf.ts:2129-2142`). Out of scope for the <10 s target; flagged for an operational compaction task (rewrite store from live root, offline) — tracked in §5 via directory-size metric.
- **Per-stage rollback paths:** every stage is a flag flip (§4) with no destructive format change; (iii-b) is the only schema change and carries the dual-read fallback above.

## 8. Interface contracts

- **To Phase 4 (pipelined commits):** the build-time budget this plan must deliver is p95 <10 s at 50k txs, leaving ≥10 s headroom inside the 20–40 s L1 cadence for Phase 4's speculative build of block N+1. The legacy/overlay/event-flat paths retain Phase 4's `LedgerOverlayHandle` and park/resume contracts. Architecture G implements the same logical `fork`/ordered delta/root/`promote`/`discard` lifecycle through a main-owned `NativeMpfOwnerService`; workers receive only a `NativeMpfOwnerClient` over `MessagePort` and never own the ledger Level handle or a transferable node closure. Both paths preserve the binding post-submit rule: the retained/promoted root must equal the submitted `utxosRoot`, and no candidate marker may become authoritative before the replay journal and submit boundary permit promotion.
- **Transfer/lock boundary used by Phase 4:** legacy/overlay/event-flat continue to use `parkBlockOverlay()`, `ParkedMpfOverlayV1`, `resumeParkedOverlay`, and `promoteParkedOverlay`. Architecture G does not use that transferable closure path: the long-lived main owner holds the sole Level lock, leases opaque generation handles to worker ports, persists exact replay data before submit, validates and atomically promotes the generated closure, and recovers from the authoritative old-or-candidate marker. A rename or semantic change to either branch must update both plans and their combined lifecycle tests.
- **To/from Phase 1 (retrieve pagination, C4):** the planner caps (`COMMIT_MAX_L2_TX_COUNT` etc.) become the page-size contract for the fixed `MempoolDB.retrieve` — Phase 1's keyset pagination should fetch at most the configured cap per commit tick, replacing today's full `MempoolDB.retrieve` at `commit-block-header.ts:535`. The oldest-first ordering fix (Phase 1 item 5) is assumed by the EWMA's per-commit sampling (starved-tx bursts would otherwise skew per-tx cost).
- **From Phase 0 (must already measure before this plan starts):** the four `logCommitMpfPhaseTiming` phases and `mpf_processing` span as dashboard series; blake2b digest throughput (WASM vs JS fallback detection); a recorded op-stream corpus (the `MPF_RECORD_CORPUS` tap is small enough to land as a Phase 0 patch); baseline p95 build times at 1k/5k/10k-tx blocks against 100k and 1M-entry ledgers. The EWMA seed value is Phase 0's measured mean, shipped as the migration row's initial value.
- **To Phase 6 (soak):** the growth-regression test (§6) and audit-divergence metric are the two signals Phase 6's 24 h soak must hold flat/zero.

## 9. Execution evidence (2026-07-10)

The first bullets below are historical selection and implementation evidence.
They remain useful provenance but do not substitute for the final-tree commands,
formal production benchmarks, live E2E, or soak listed at the end of this
section.

- The named seeded adversarial differential is now binding across `legacy`, `overlay`, and the production Architecture G native owner, each with insert- and from-list-built starting fixtures. `pnpm run test:mpf:differential` builds and SHA-identifies the release owner binary, then ran 6 engine/build combinations and 12 independent membership/non-membership proof checks. The corpus includes an empty event, delete/reinsert, a deterministic six-nibble hashed-prefix collapse/resplit sequence, all complete production roots, and every transition pre/post root. Each Architecture G run uses a fresh Level fixture and child, fails on any root/proof/child mismatch, discards the fork, verifies the durable marker and zero active generations, closes the child/Level owner, and removes the fixture. The current owner binary SHA-256 for this run was `eb0e236a39db756831bc771551b244aecf7fcf98dfd20b2b85c183ba489bb7e2`.
- An earlier focused MPF/differential run was green (87/87 across the then-current MPF, WASM-digest, differential, speculative lifecycle, and safety-guard suites). It covered canonical direct CBOR against Lucid for all transition variants and integer boundaries, strict invalid-mutation behavior, poison/single-discard recovery, fork isolation with hydrated nodes, promotion ownership, conservative large-value spill accounting, exact DA aggregate sizing, chunk-size equivalence (`1`, `2`, `3`, and `100` ops), upper-arena corruption poisoning, zero-write checkpoint behavior, exact per-event roots in transient current-root mode, transient reachable-node corruption poisoning, immutable parent/fork closure through child promotion and reopen, flat-multiproof compilation against the final trie root, transferable park/resume with immediate Level-path reopening, digest tamper rejection with an unchanged marker, stale-base rejection, scratch/no-path resume, exclusion of unreachable serialized mutation intermediates from parked artifacts, real-Level parent/fork park/reopen ownership, and exact ownership transfer to a promoted fork. This count predates later Architecture G and shared Phase 1/4 edits and must not be reported as the final closure count.
- A previous purpose-built 50k dependent-transition probe passed the performance exit gate over 20 fresh processes: p50 `8542.069 ms`, nearest-rank p95 `9272.354 ms`, mean `8725.832 ms`, max `12062.847 ms`. All 20 runs produced identical UTxO, transition-trace, and event-to-step roots. That evidence predates removal of a process-global pair cache and is therefore stale; it must be rerun before any cap/default flip.
- The scratch-root worker proof used two simultaneously active workers with transferable arenas. Worker startup is prewarmed outside the timed commit path; worker failures, timeouts, respawn, and metrics remain explicit.
- The production-shaped collapsed-Level growth gate was run once with fixed Architecture B settings: 10k transitions, `MPF_PATH_HYDRATION_MODE=chunked`, `MPF_HYDRATION_CHUNK_OPS=512`, retained depth `2`, parallel scratch roots, branch diagnostics off, and identical build/reuse methodology. The 100k fixture completed in `2687.356565 ms`; the 1M fixture completed in `3281.023668 ms`; growth was **+22.0911%**, failing the required ±10% bound. Exact expected UTxO/transition/event roots matched at both sizes, Level batch writes were `0`, confirmed-ledger full scans were `0`, and maximum native batches were `486`/`507` keys.
- Paired attribution: prefetch grew `413.226→621.075 ms` (+`207.849`), Level read time `241.355→364.935 ms` (+`123.580`), checkpoint `366.385→483.604 ms` (+`117.219`), materialization `215.338→310.404 ms` (+`95.066`), authentication `252.084→270.007 ms`, and collapse `20.394→30.164 ms`. Checkpoints serialized `59,236→75,285` nodes (`40,826,449→59,625,910` bytes). Peak decoded nodes stayed bounded (`1,631→2,015`), retained upper nodes were `273` at both sizes, and peak RSS was `1,291,560→1,786,520 KiB`. Fixture sizes were `30,834,617` and `304,766,752` bytes. The host guard never approached its 4 GiB floor and both fixtures were removed immediately after measurement.
- The failed Architecture B pair selected §2.8. Its raw whole-block path cache plus proof-gated transient current-root arena is implemented behind experimental `chunked_arena` behavior. The first fixed pair completed in `2808.399/3945.292 ms` (+`40.482%`). Deferring full authentication to immutable boundaries and generation-scoping repeated callback dedup improved the next fixed pair to `2180.348/2897.750 ms`, but growth remained **+32.9031%**, still outside the required ±10%. Exact roots matched; full scans, Level writes, and checkpoint serialization remained zero. Prefetch grew approximately `524→956 ms`, Level read time `132.7→277.9 ms`, and the reachable closure `32,924→48,910` nodes, selecting the plan's event-atomic/flat-multiproof decision point.
- The selected follow-up now records branch mutations as an event-local dirty graph and finalizes reachable dirty nodes bottom-up once per event, including branches with multiple dirty children. A flat authenticated compiler packs metadata, node hashes, prefixes, branch child hashes/local IDs, and leaf keys/values into typed arenas after authenticating the hydrated graph. Fork, flush, and park remain immutable boundaries: they clone-detach and fully authenticate every reachable transient node before insertion into any hash-keyed store; corruption synchronously poisons/discards and cannot advance the marker. The build probe now times this no-write capture boundary and reports build-plus-capture, so the gate cannot hide deferred authentication cost.
- A fresh fixed 100k diagnostic ran the event-atomic build, authenticated flat compilation, and actual parked-artifact capture in one measured path. The 10k-transition build took `2547.732427 ms`; flat compile took `192.298615 ms`; park/capture took `726.763832 ms`; build-plus-boundary was `3466.794874 ms` with `1,423,832 KiB` maximum RSS. Expected UTxO/transition/event roots matched; full scans, Level writes, checkpoint serialization, and materialization were zero. The run recorded `10,000` event-atomic finalizations (`128,678` dirty nodes, maximum `17` per event), compiled `32,831` flat nodes (`13,918,629` bytes), and parked `32,248` authenticated nodes (`13,607,522` bytes), leaving zero transient live/dirty nodes. The last fixed 1M prefetch cost alone raises this total to a lower bound of `3870.788730 ms` (**+11.6532%**) even if every other component stays flat; scaling only the authenticated boundary by the observed closure ratio `48,910/32,924` projects `4317.032573 ms` (**+24.5252%**). The 100k diagnostic therefore did not justify the 1M fixture/run under the unchanged ±10% rule.
- Architecture E replaced eager frontier enumeration with store-owned, content-authenticated raw touched proofs and made V2 transfer dirty-only. The exact fixed pair remained root- and lifecycle-correct but failed: build-plus-boundary `3185.844737→4306.078796 ms` (**+35.1629%**), raw proof nodes/Level gets `32,925→48,911`, and V2 dirty nodes `32,033→48,603`. Both expected candidate/rehydrated roots matched, Level writes/full-ledger scans/live/dirty nodes were zero, and fixture markers/counts remained unchanged. This proved a compact V3 transfer plus same-root cache cannot meet the gate: even unrealistically deleting all raw and park cost leaves +20.4592%.
- Architecture F moved raw-proof authentication, all ordered event mutations/per-event roots, and compact sparse dirty closure construction into one canonical Rust/WASM call graph. Native tests, hard-coded and live Forestry differentials, long-prefix collapse/resplit, sibling-cache regression, and raw/artifact corruption gates pass; pinned WASM SHA-256 is `caab18ddf65a3121a2ea7f5768088e47b4b449a05a159ec297cb3776809bafe4`. The one-shot exact pair still failed: conservative projected totals `2068.090173→3875.178805 ms` (**+87.3796%**); even removing all fetch cost leaves +26.8557%. Production integration therefore stopped.
- Architecture G's retained-session roots-only prototype removes per-block proof authentication and dirty-closure transfer while retaining canonical mutation. It emits a digest-bound fixed `1,460,092`-byte event stream and `320,108`-byte root stream. Every one of 10,000 event roots matched the one-shot/Forestry reference at both sizes; stale handles, two-fork replay/discard, generation caps, failed-mutation rollback, and event corruption failed closed; fixture markers were unchanged. Timed hot work was `158.494412→186.913568 ms`; after the shared conservative `849.795243 ms` scratch-root allowance, projected build was `1008.289655→1036.708811 ms` (**+2.8186%**), clearing ±10%.
- The native64 full-index operational prototype then authenticated the complete real Level closures read-only. At 100k it loaded `137,420` nodes/`137,419` edges (`77,574,912` compact bytes); at 1M, `1,345,734` nodes/`1,345,733` edges (`635,318,656` bytes). The 1M marker-matched restart authenticated in `2434.492264 ms`; prepare/corrupt/stale rebuilds were `2983.720636`/`2951.556935`/`2648.188080 ms`. Separate-process replay produced the exact candidate root/digest; corrupt event logs were rejected; corrupt/stale sidecars rebuilt; simulated fork/discard/promote rejected stale handles; logical fixture hashes before/after were identical; final Level reopen proved lock release; temporary artifacts were removed. The 1M owner used `763,808 KiB` after startup, `881,396 KiB` steady, and `1,886,504 KiB` peak, below the hard 2 GiB cap by `210,648 KiB`. Native64 binary SHA-256 is `5d258a1583f9173a73cb18d58e58499d81bef28ca9687c9cca80bef696eb6535`.
- Production Architecture G now has a main-owned sole-Level-lock service, bounded digest-framed native RPC, pinned binary identity, worker `MessagePort` clients, strict caps/timeouts/restart policy, fork/apply/discard, atomic generated-closure promotion, replay-journal migration `0024`, post-submit recovery, runtime config/layer wiring, and a pinned Rust Docker build that copies only the owner binary and SHA manifest into the Node runtime. Focused tests cover malformed framing, the now-binding named adversarial native/legacy root and proof comparison, real child SIGKILL/stale epochs, both promotion crash boundaries, and post-submit recovery. The broader final-tree crash/recovery command and remaining formal artifacts still need to be recorded after the shared tree settles.
- The formal production gate tooling now requires immutable durable fixtures, exact CPU affinity, fresh processes, deterministic complete root tuples, corpus/fixture identity checks, and a zero full-scan counter. The canonical 50k selector validates the Phase 1 manifest and complete named slice, rejects dependency discontinuity/cross-slice chain reuse, and records its dependency-closed first-50k boundary proof. The formal retained-growth and 50k runs have not yet executed on released resources.
- Clean opt-in deployment, real deposit/L2/DA/header merge/finalization, child restart/post-submit recovery, and soak evidence remain open. The fail-closed 86,400-second live runner and offline verifier now exist (`docs/benchmark-scenarios/phase-3-architecture-g-soak.md`) and enforce zero audit divergence, zero unplanned full-scan/timeout deltas, continuous readiness, owner/generated-closure caps, non-growing DA/merge queues, no restart, and process RSS growth below 10% per day. They also pin achieved offered/accepted/saturation floors, bind actual measured elapsed, sample from before workload spawn through process exit after drain, and require exact submit-attempt schema/cardinality/ordered identity. Clean-E2E stdout/stderr is redacted before retention and any detected sensitive line fails its step. Required background audits are bound one-for-one to monotone `last_audit_at` advances rather than mislabeled as hot-path scans. No 24-hour pass is claimed until that exact production gate runs.
- No production parameter sweep or default flip has been authorized. Current defaults remain `MPF_ENGINE=legacy`, `MPF_SCRATCH_BUILD=insert`, `MPF_PAYLOAD_ROOT_CHECK=every_block`, `MPF_PARALLEL_ROOTS=false`, `MPF_PATH_HYDRATION_MODE=whole_block`, `COMMIT_BUILD_COST_MODEL=static`, and caps `10000/40000/40000`. Any change requires the differential/crash, retained-growth, canonical fresh 50k x20, release-image, clean E2E, soak, and independent-review evidence in a separate reviewed commit.
