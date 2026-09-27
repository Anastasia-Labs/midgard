# P0-a: can the ledger MPF store retain every recent block's root? (read-only investigation)

Scope: §4.D of docs/exec-plans/node-l1-rollback-redesign.md (lines 146-153: "fast: retain roots
for every block inside the rollback horizon ... rewind = switch the root pointer. Requires
root-view-store GC to keep retained roots, not only the current one (P0-a)").
Paths below are relative to demo/midgard-node/ unless absolute.

## Headline

The premise of P0-a is inverted. There is **no durable GC at all** in either MPF store. The
LedgerDB node store is append-only and content-addressed, so the closure of **every root ever
promoted is already retained on disk, forever**. Retention is free today (and unbounded). The
existing rewind path, `restoreCanonicalRoot`, already depends on it and is used by the
state-queue correction rewind. What is missing is (a) a *bounded* GC keyed to a retained-root
set, and (b) an O(1) in-memory root switch in the native owner. Today a rewind costs a
full-closure reload, not a pointer switch.

## 1. What GC / mark-sweep runs today, and what it keeps

**Durable (Level): none. Nothing ever deletes a node key.**
- `src/mpf/root-view-store.ts:584-617` `del()`: node deletes during a mutation go only into the
  in-memory deferred sets, and in live-arena mode they return immediately (`:596`,
  `if (this.liveArenaEnabled) return;`). Outside a mutation, `:606`
  `if (storageKey !== ROOT_KEY || !this.persistRootMarker) { return; }` means a node-key delete
  is a **no-op**. Only the root marker is ever deleted.
- `:1302-1339` `flushOverlay` builds `ops` only as `type: "put"` (overlay entries plus ROOT_KEY).
  `:1419-1464` `spillIfNeeded` writes put-only batches to Level in the middle of a block, so
  nodes of a later-*discarded* overlay can also be left in Level permanently.
- Native owner `src/services/mpf-native-owner/service.ts:1414-1421`: the promotion batch is
  `records.map(put)` plus `__root__`, with no deletes. `restoreRetainedRoot` (`:1642-1648`) puts
  only the recovery record and `__root__`.
- `grep` for compact/garbage/sweep/orphan/prune across `src/mpf` and `src/services/mpf-native-owner`
  finds nothing durable. `git log -S`/`--grep` finds no GC that existed and was later removed.

**In-memory mark/sweep: per block, current root only, and it never reaches Level.**
- `root-view-store.ts:1823-1858` `pruneLiveArenaToRoot(root)` marks from the block's final root
  over `blockDeferredNodePuts ∪ overlay ∪ blockPathCache` and drops **only unreachable entries of
  `blockDeferredNodePuts`**. These are intermediate nodes created by earlier events *within the
  same block* and then superseded. It is called from `flushOverlay` (`:1321-1323`) and
  `parkCurrentOverlay` (`:965`).
- These are the comments quoted in the brief:
  - `:537-540`: `"Live MPF arena requires an immutable detached node snapshot"`. `putRetainedNode`
    requires a `cloneDetached()` copy, so a content key shared by several parents is never
    mutated in place.
  - `:544-545`: "A content key may already have been authenticated for a different object.
    Verify every new detached snapshot before it can replace that key".
  - `:566-567` (in `deleteRetainedNode`): "Content hashes can be shared by multiple parents.
    Retain immutable snapshots until the final current-root mark/sweep proves them orphaned."
    That "final current-root mark/sweep" is `pruneLiveArenaToRoot`, the in-memory, per-block
    sweep described above.
- Native in-memory index: `native/mpf-event-flat-wasm/src/owner.rs:233-276` `commit` **appends**
  every generated node not already present (`index.append`, `:260-263`) and moves `marker`
  (`:272`). It never removes anything. After a promotion the resident `FullIndex` therefore
  holds the closure of *every root promoted since the child process loaded*. It shrinks back to
  one closure only on reload (startup, crash restart, or `restoreCanonicalRoot`).

## 2. Content addressing, the retained-root notion, and restoreCanonicalRoot

- **Content-addressed: yes.** Level key = node hash hex. `walkReachableRecords`
  (`service.ts:527-560`) does `db.getMany(hashes)` and follows `children` hashes. Promotion
  filters out records the index already has (`owner.rs:256-259`, `!index.ids.contains_key`). Every
  root naturally shares unchanged subtrees. Per root, MPF stores Leaf{prefix,key,value} and
  Branch{prefix,children[16],size} (`service.ts:49-61`).
- **Retained-root set: only implicit.** Nothing names a set of roots. The only "retained" notions
  are these:
  - `__canonical_recovery__:<id>` plan records (`service.ts:1580-1585`).
  - The doc-comment "Restore only a retained, hash-verified closure" (`:1529-1533`).
  - The refusal at `:1605-1619`, "target root … is not retained in full; refusing to restore".
    It holds only because nothing deletes.
- **`restoreCanonicalRoot` (`service.ts:1534-1565` → `restoreRetainedRoot` `:1567-1660`) switches
  roots without replaying transactions, but it is not a pointer switch.** It does the following:
  1. Compare-and-swap (CAS) that `__root__` equals `expectedRoot`, and require zero active
     generations (`:1587-1602`).
  2. Call `buildOrReadFullIndex(targetRoot)` **with the sidecar deliberately disabled**
     (`sidecarPath: undefined`, `:1610-1614`). This walks the target's full closure out of Level
     twice (count pass, then encode pass, `:588-605`).
  3. **Close the native child** and **start a new one** that loads and re-authenticates the whole
     index (`FullIndex::from_payload` plus `authenticate_complete_closure`) (`:1625-1630`).
  4. Write the recovery record and `__root__` in one synchronous batch (`:1642-1648`), then bump
     the epoch, which invalidates all handles.

  Cost = O(whole live trie), not O(1). Caller:
  `state-queue-correction-rewind.ts:715` (targetRoot = first removed block's `BASE_UTXOS_ROOT`)
  → `executeHistoryDependentRecovery` → `history-dependent-recovery.ts:34`.
- **Why a pointer switch is impossible in the native owner today:**
  - `owner.rs:120-126` `fork` requires `base_root == marker` ("fork base root is stale").
  - There is no RPC that moves `marker`, although the resident index already contains the old
    closures.
  - `maxActiveGenerations: 2` (`protocol.ts:18`, `owner.rs:121`).

## 3. Per-block retained size

**Measured, from repo evidence.** Source: local, untracked logs
`demo/midgard-node/logs/phase-3-verify-owner-survivor-cpu08-20260713T180500Z/summary.json` (+ `.md`);
`phase-3-growth-diagnostic-20260713T011500Z` agrees.

| Initial UTxOs | Level records | Level dir bytes | B/record on disk | Native resident bytes | B/node resident | owner startupMs (3 runs) |
|---:|---:|---:|---:|---:|---:|---|
| 100k | 137,421 | 30.8 MB | ~224 | 77.6 MB | ~565 | 462 / 423 / 498 |
| 300k | 401,993 | 90.6 MB | ~225 | 204 MB | ~508 | 1189 / 1230 / 1188 |
| 1M | 1,345,735 | 302.9 MB | ~225 | 635 MB | ~472 | 5046 / 4139 / 3975 |

- Records ≈ 1.35 × UTxOs, so branches ≈ 0.35 N.
- The bench uses 2 ledger ops per tx (`ledgerOpCount` 20,000 for 10,000 tx).
- Resident size is dominated by `branch_merkle` (15×32 B per branch, `owner.rs:51`).
- **No per-block dirty-node or generated-record count is measured anywhere I found.**
  - The probe discards its generation, so `generatedNodes` is 0.
  - `artifacts/` is gitignored and has no nonzero `generatedNodes`.
  - No `commit_build_calibration` or `docs/perf*` numbers exist for this.

**Estimate.** New records per block ≈ inserted leaves + distinct branches on touched paths
≈ Σ_level B_l(1−e^{−k/B_l}), with k = ledger ops and B_l = 16^l. At N = 1M the trie has
about 6 levels (level 5 is partial, ~27% of paths). Each new node supersedes about one old
node, so the retained cost of keeping a block's root ≈ its dirty count.

| Block (tx, ~2 ops/tx) | new records/block | disk/block (~225 B) | 100 blocks disk | 100 blocks resident (~500 B) |
|---|---:|---:|---:|---:|
| 10 tx | ~90 | ~20 KB | ~2 MB | ~4.5 MB |
| 100 tx | ~700 | ~160 KB | ~16 MB | ~35 MB |
| 1,000 tx | ~5.4k | ~1.2 MB | ~120 MB | ~270 MB |
| 10,000 tx (bench block) | ~37k | ~8 MB | ~830 MB | ~1.85 GB (exceeds the 2M-node / 2 GiB cap together with a 1M-UTxO base) |

Real transfers with 1 input and 2 outputs are ~3 ops/tx, so scale these by ~1.4.

## 4. What changes to retain a set of roots instead of one

**Durable retention: no change needed. It already retains everything.** The work that P0-a
actually needs is *bounding*: a Level mark/sweep keyed to a retained-root set R.
- R = current `__root__`
  ∪ `BASE/EXPECTED_UTXOS_ROOT` of every non-confirmed own or landed block within the horizon
  ∪ the confirmed-ledger root
  ∪ `expectedRoot`/`targetRoot` of any `__canonical_recovery__:*` plan
  ∪ base/candidate of any `pending_block_finalizations` native-replay row
  (`database/pendingBlockFinalizations.ts:94-95,2103`).
- The mark is roughly the union of closures, about |live trie| plus retained dirt: seconds at
  1M, similar to the startup walk. The sweep iterates all Level keys.
- It must run under the owner's operation serialization, with **no active generation and no
  in-flight promotion**. `validatePromotionClosure` (`service.ts:1840-1880`) accepts children
  "already in Level", so a sweep racing a promotion could leave a dangling child.
- For the legacy/overlay engines, spilled nodes of discarded overlays are also garbage that
  only a sweep reclaims.

**Fast pointer switch (native), minimal diff:**
- Add a `SwitchRoot(target)` RPC. Accept it iff `target ∈ index.ids` and there are 0
  generations. Then set `marker` and `index.root`. The TS side CASes `__root__` exactly as
  `restoreRetainedRoot` does, but without restarting the child.
- This is sound for roots promoted in *this child's lifetime*. Commit appends each root's full
  reachable closure (`generated_reachable_nodes`) with resolved child ids, so an index-resident
  root is closure-complete.
- After any restart the index holds only the marker closure. The fallback is the existing
  full-reload `restoreCanonicalRoot`.

**Invariants that break if you instead try to *load* a multi-root index:**
- `owner.rs:677-683` "full index has unreachable records". Every loaded record must be reachable
  from the single marker root.
- `owner.rs:724-727` "canonical closure is not a tree". A shared child has >1 parent across
  versions.
- `owner.rs:447-448`: duplicate record rejected. That one is fine for the union.

So the native load contract is single-root by design. Keep multi-root retention in Level and in
the in-process append-only index, not in the load payload.

**The root-view-store invariant `:537-540` does not break.** Immutable detached snapshots are
what makes sharing safe.

**Memory cap interaction:**
- The resident index already grows by ~dirty/block with no compaction until reload. Caps are
  `maxResidentNodes 2,000,000` and 2 GiB (`protocol.ts:11-12`, `owner.rs:16-18`).
- A cap breach on `PreparePromotion` returns an Error frame (`owner.rs:1580-1588`). The TS side
  only rejects the request (`service.ts:1025-1027`). No compacting restart is triggered, so a
  long run with large blocks near 1M UTxOs can hit a liveness wall. This is pre-existing and
  inferred from code; I did not observe it.
- Retaining roots in RAM makes the wall more likely. A deliberate compaction (reload at the
  current marker) is needed when retained nodes exceed a budget.

## 5. Is there a slow path that rebuilds from confirmed_ledger + replay?

- **Legacy/overlay engines only.**
  - `src/mpf/ledger-hydration.ts:161-174` `hydrateLedgerMpfFromLedgerEntries` does
    `resetToEmpty` + `applyBatch` of all entries.
  - `:176-238` `synchronizeCommitMpfStoresFromLedgerEntries` / `...FromConfirmedLedger` is called
    after a confirmed merge (`transactions/state-queue/merge-to-confirmed-state.ts:1311-1317`).
  - `resetToEmpty` is only a logical marker reset (`store.ts:590-624`). No nodes are deleted.
- **architecture_g refuses it explicitly.**
  - `ledger-hydration.ts:82-92`: "…refusing to open its Level path in a commit worker".
  - `:185-197`: "…refusing to reopen its LevelDB path for persistent-store synchronization".
- **Native slow tier, the closest existing pieces:**
  1. `restoreCanonicalRoot(confirmedRoot)`. It works because Level keeps everything.
  2. Replay each block's persisted native event log through fork → apply → promote. `recover()`
     does exactly this for one block (`service.ts:1436-1475`), and logs live in
     `pending_block_finalizations.mpf_replay_event_log`.

  I did not verify that those rows are retained for every block back to the confirmed root.
  They are deleted on finalization and supersession (`pendingBlockFinalizations.ts:2022-2027,
  2419, 2587-2591`).
- **Hydration timing: no measurement in the repo.** The only related numbers are the native
  full-closure load times in §3 (0.4-0.5 s @100k, ~1.2 s @300k, ~4-5 s @1M UTxOs). I could not
  tell whether each run read the sidecar or walked Level. `restoreCanonicalRoot` always walks
  Level, so the first-run (probably cold) figures are the relevant ones.

## Could not determine

- Real per-block dirty/generated record counts. §3 is an analytic estimate.
- L2 block cadence, i.e. how many own blocks fit in 30 L1 blocks.
- Whether the startupMs runs used the sidecar.
- Whether per-block native replay logs survive back to the confirmed root.
- Level on-disk write amplification and compaction overhead for append-only growth.
- Behaviour at the native resident cap in a live run.
