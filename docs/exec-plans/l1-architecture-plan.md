# Midgard L1 architecture plan: deferred follower work

Status: Active. The core of the L1 chain-follower program (#784) is
implemented: one ChainSync follower per process as the only authority for
canonical L1 history, and rollback as "rewind the facts, then recompute". This
plan holds the work the owner deferred from it on 2026-10-07, tracked in #811.

Last reviewed: 2026-11-23 (each deferred ticket against the source at
`5c3963c5b`).

- Current reference: [`demo/midgard-l1-follower/README.md`](../../demo/midgard-l1-follower/README.md)
  (fact store, origin, heads module and its levels).

Non-goals: on-chain changes; outputs created before the deployment origin.

Terms: **k** is 2,160 blocks, the manifest's `automaticRecoveryMaxDepth`.
**cd** is the manifest `confirmationDepth`. **N** is the state size: tracked
live outputs, L2 ledger entries and live events.

## Tickets

Every ticket that changes offchain contract behaviour includes emulator tests
in both polarities. "Fork simulator" is the follower's property and fork
simulator in CI.

| Ticket | Scope                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Depends on | Acceptance                                                                                                                                                                                                 | Pick up when                                                      |
| ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| **F6** | Pipelined ingestion: decode pool, reorder buffer, batched ordered writer and tracked-outref cache (design below). The follower writes one block at a time.                                                                                                                                                                                                                                                                                                                                         | —          | B1 and B2 targets met. Under random worker delays, writes stay in sequence order. After a rewind, the tracked-outref cache equals a fresh load.                                                            | Catch-up from origin is too slow for a restart or a new operator. |
| **F9** | Devnet L1 fork drill (#735). The devnet runs one cardano-node (`demo/midgard-node-tools/devnet/phase4-process/compose.yaml`), so it cannot fork. Add a second block producer, a partition toggle between the two, and drills `l1-fork-shallow` (depth ≤ cd), `l1-fork-deep` (cd < depth < k) and `l1-fork-correction` (the fork removes an admitted correction). Replace `stop-kupo` and `stop-ogmios` in `demo/midgard-node-tools/src/devnet-stack/chaos.drill-catalogue.ts` with `kill-sidecar`. | —          | Each drill produces a fork of the requested depth, checked against both nodes' tips. Each role's `/readyz` returns to ready with no restart and no CLI. Projections equal a fresh replay after the rejoin. | Before the public testnet.                                        |
| **M1** | The native MPF owner's `SwitchRoot` RPC (design below). Today the MPF follows a rewind through `restoreRetainedRoot` (`services/mpf-native-owner/service.production-native-mpf-owner-service.ts`), which restarts the MPF child process, not the node, once per rewind that crosses an applied block.                                                                                                                                                                                              | —          | Switching to a resident root at depth 3 takes ≤ 10 ms with no restart (B4). It is refused while generations are active.                                                                                    | The MPF child restart shows up in liveness or latency.            |
| **M2** | The multi-root load contract, `mpf_root_pins` and `mpf_roots`.                                                                                                                                                                                                                                                                                                                                                                                                                                     | M1         | Load accepts records reachable from any pinned root and rejects orphans.                                                                                                                                   | With M1.                                                          |
| **M3** | `ImportClosure`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | M2         | A switch at depth 2,160 meets the B4 target with no restart. A missing record raises `mpf_closure_missing`.                                                                                                | With M1.                                                          |
| **M4** | Budgeted `Sweep`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | M2         | After a sweep, every pinned root reopens and every unpinned root is gone. Meets the B4 target.                                                                                                             | With M1.                                                          |
| **M5** | Compaction on an MPF cap breach (#813). A root over the full-index caps holds the node as `native_mpf_promotion_index_cap_exceeded` or `native_mpf_restore_index_cap_exceeded`. Above 75 % of the resident cap, or on a cap error, run a planned child restart that loads only the current root's closure.                                                                                                                                                                                         | —          | A fixture that exceeds the cap: the next promotion succeeds after compaction, and the root is unchanged. Compaction takes ≤ 10 s at 1M resident records (B4).                                              | #813.                                                             |
| **N7** | The mempool-ledger cache rebases by delta after a rewind, a commit error or a rejection, instead of its full reload (`services/mempool-ledger-cache.make-mempool-ledger-cache-service.ts`).                                                                                                                                                                                                                                                                                                        | —          | No full reload on rollback, commit error or rejection.                                                                                                                                                     | B2 or a soak misses its target.                                   |
| **N8** | The commit base is the native root at the parent queue position, with no hydration and no per-journal materialisation. The `MPF_PAYLOAD_ROOT_CHECK` cross-check, kept on by default as a net against a native trie defect, computes one from-scratch root per commit over the post-state.                                                                                                                                                                                                          | M2         | Commit build at N = 10^6 meets the B2 target, or the numbers go to the owner.                                                                                                                              | B2 or a soak misses its target.                                   |
| **N9** | Hot-path SQL: bounded queries, counters, and a DA reconciler that runs on deltas.                                                                                                                                                                                                                                                                                                                                                                                                                  | —          | Each query has a plan test or an `EXPLAIN` fixture with no sequential scan over a growing table.                                                                                                           | B2 or a soak misses its target.                                   |
| **I4** | Committee submissions under the collateral rule (2026-10-04): an unretired attempt never gates later actions; collateral is released when the attempt is confirmed and re-signed if a rollback removes that confirmation; attempts that share a funding input are mutually exclusive. The watcher keeps no record of its sends and derives their progress from the ledger and the mempool, so it is not part of I4.                                                                                | —          | Committee collateral is released at confirmation and re-signed after a rollback. An unretired attempt does not block the next one.                                                                         | The next committee submission bug.                                |
| **U1** | API head levels. The API reports `{point, depth, level}` for each head, and tx status reports the level, so `merged` is never claimed bare. Today tx status reports a merge `finalized` once its merge tx is `safe` (`commands/tx-status-merge-evidence.ts`).                                                                                                                                                                                                                                      | —          | A status below `final` reverts after a fork in the fork simulator.                                                                                                                                         | Before the public testnet.                                        |
| **U2** | Readiness sweep in every role: each intervention case fails `/readyz` with its reason and the process stays up. R8 `wallet_below_floor` (own wallet funds below the fee floor for pending work) has no reason in the source; the node pages low role wallets through `MidgardWalletBalanceLow` and has no refill loop.                                                                                                                                                                             | —          | For each reason, a test that the process stays up and is unready with that reason. A supervisor test shows no restart loop. B7.                                                                            | Before the public testnet.                                        |
| **U4** | Remove Kupo and Ogmios from the devnet compose, the drill catalogue, the operator tools' `--l1 kupmios` source, the fault-proofs CLI, config and docs. The roles do not use them.                                                                                                                                                                                                                                                                                                                  | —          | No Kupo or Ogmios process runs in the devnet. Every journey passes.                                                                                                                                        | Now.                                                              |

## Design: pipelined ingestion (F6)

```
sidecar ──frames(seq)──► decode pool (W workers) ──summaries(seq)──► reorder buffer ──► ordered writer ──► DB
   ▲                                                                                        │
   └──────────────────────────── ack(seq committed) ◄───────────────────────────────────────┘
```

1. **Credit window.** The follower grants the sidecar C frames: 50 while the
   cursor is more than k blocks behind the node tip, 1 at the tip. Each
   acknowledgement names the highest sequence number committed, so a slow
   store slows the node, never the other way round.
2. **Decode pool.** `worker_threads`, W = min(4, cores − 1). Each worker turns
   one raw block into a stateless block summary: exact tx byte slices and body
   hash; inputs, reference inputs, collaterals, outputs, mint, withdrawals,
   redeemers and validity bounds; and the qualification rules that need only
   the static tracked set (addresses, credentials, policies, and the wallets
   the writer passes in).
3. **Reorder buffer.** Summaries are released strictly in sequence order. A
   `roll_backward` frame is a barrier: the writer finishes everything before
   it, and later summaries are decoded only once it has been applied.
4. **Ordered writer.** The only writer of the fact tables. It resolves the
   rules that need the live tracked-outref set, inserts rows with multi-row
   `INSERT … SELECT unnest(…)` (or `COPY` during catch-up), runs the role's
   derivations in the same transaction, and advances the cursor. During
   catch-up one transaction covers up to 100 blocks or 250 ms of work; at the
   tip, one block. `synchronous_commit` stays on.
5. **Tracked-outref cache.** Loaded on start from the unspent tracked outputs
   and patched after a rewind from the rewind's own result, never fully
   reloaded.

Per-block cost is O(txs in the block) to decode and O(qualifying rows) to
write; neither depends on N.

## Design: native MPF versioning (M1–M4)

**Tables (node Postgres).** `mpf_roots (queue_position, header_hash, root,
from_slot, to_slot)` holds the canonical ledger root at each landed queue
position, plus own local roots (`from_slot NULL`, linked to their intent).
`mpf_root_pins (root, reason, holder, until_point NULL)` with reasons
`canonical_within_k`, `live_intent`, `proof` (watcher objectives, merge
proofs in flight) and `operator`.

| RPC                   | Precondition                                | Effect                                                                                                  | Cost                                   |
| --------------------- | ------------------------------------------- | ------------------------------------------------------------------------------------------------------- | -------------------------------------- |
| `SwitchRoot(root)`    | No active generations; the root is resident | CAS `__root__` to `root`, clear the generation leases, drain readers, bump the epoch. No child restart. | O(1)                                   |
| `ImportClosure(root)` | The root is in Level                        | Append to the resident index every record reachable from the root that it lacks                         | O(missing records)                     |
| `Sweep(budget)`       | A quiescent window                          | Mark from every pinned root, then sweep unmarked Level records, resumably within the budget             | O(store), off the block path           |
| `Compact()`           | Planned                                     | Restart the child, loading only the closure of the pinned roots                                         | O(resident window), off the block path |

**Load contract.** The resident-index load accepts a record if it is
reachable from any root in the pin set the owner supplies, and still rejects
orphans.

**Rollback.** The rewind truncates `mpf_roots`; the owner reads the desired
root and calls `SwitchRoot`, after `ImportClosure` if it is not resident. A
closure missing from Level raises `mpf_closure_missing`. No path within k
restarts the child or rebuilds the index.

**Why the resident window cannot be k.** Estimated dirty nodes per block are
about 90, 700 and 5,400 at 10, 100 and 1,000 L2 txs per block (3 touched
UTxOs per tx, trie depth about 5 at N ≈ 10^6). Over k blocks that is about
194k, 1.5M and 11.7M nodes against the 2M resident cap. So the cap bounds the
resident window and triggers compaction above 75 %, Level keeps every pinned
root's closure for k (about 340 MB of disk at 100 txs per block), and deeper
rollbacks use `ImportClosure`. B4 replaces these estimates.

**Retention.** A root is pinned `canonical_within_k` while its `mpf_roots` row
is current or closed less than k blocks ago; `live_intent` pins drop when the
intent becomes terminal. `Sweep` runs when the store has grown by 20 % since
the last sweep, or daily.

## Benchmarks

Bench files run under `demo/midgard-node/vitest.bench.config.ts` and write a
JSON report. If a target is missed, the ticket reports the numbers to the
owner rather than relaxing the target.

| #   | Measures                                                                                                                   | Workload                                                                                                                                                                 | Target                                                                            | Ticket                                  |
| --- | -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------- | --------------------------------------- |
| B1  | Catch-up throughput                                                                                                        | Sparse blocks (≤ 1 qualifying tx) and dense blocks (50 qualifying txs), credit window 50                                                                                 | ≥ 1,000 blocks/s sparse; ≥ 100 blocks/s dense                                     | F6                                      |
| B2  | Per-block apply at tip, every projection included, DA payload encoding excluded (`tests/benchmarks/l1-events-b2.bench.ts`) | 10 qualifying txs per block at N = 10^4, 10^5 and 10^6                                                                                                                   | p99 ≤ 50 ms; p99(10^6) / p99(10^4) ≤ 1.2                                          | Trigger for N7–N9; target for F6 and N8 |
| B4  | MPF versioning                                                                                                             | `SwitchRoot` to a resident root; `ImportClosure` at depth 2,160; `Sweep` and compaction at 1M resident records; dirty nodes per block at 10, 100 and 1,000 txs per block | Switch ≤ 10 ms; import ≤ 30 s; sweep ≤ 60 s in budgeted slices; compaction ≤ 10 s | M1–M5                                   |
| B7  | Restart to ready                                                                                                           | Cursor at tip, N = 10^6, including the MPF load of the current root                                                                                                      | ≤ 30 s                                                                            | U2                                      |

## Questions after measurement

- N8: the per-commit from-scratch root cross-check, if B2 cannot be met at
  N = 10^6 with it on every block.
- B4: the resident window, if the measured dirty-node rate is far from the
  estimate above.
