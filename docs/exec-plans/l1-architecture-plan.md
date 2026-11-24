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
| **N7** | The mempool-ledger cache rebases by delta after a rewind, a commit error or a rejection, instead of its full reload (`services/mempool-ledger-cache.make-mempool-ledger-cache-service.ts`).                                                                                                                                                                                                                                                                                                        | —          | No full reload on rollback, commit error or rejection.                                                                                                                                                     | B2 or a soak misses its target.                                   |
| **N8** | The commit base is the native root at the parent queue position, with no hydration and no per-journal materialisation. The `MPF_PAYLOAD_ROOT_CHECK` cross-check, kept on by default as a net against a native trie defect, computes one from-scratch root per commit over the post-state.                                                                                                                                                                                                          | —          | Commit build at N = 10^6 meets the B2 target, or the numbers go to the owner.                                                                                                                              | B2 or a soak misses its target.                                   |
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

## Native MPF roots (formerly M1–M5)

Tickets M1–M5 and benchmark B4 planned root switching, closure import, sweeps
and compaction for an owner that kept the whole trie resident. They are
closed without that design:

- The ledger store keeps every retained root's nodes by reference count
  (`demo/midgard-fault-proofs/src/ledger-store`), so there is no pin table and
  no sweep.
- The owner keeps a bounded cache of nodes and reads the rest from the store
  by hash. Moving the root back to a retained root is one `SetRoot` message
  with no child restart, and there is no resident cap to compact under
  (#813). See
  [Architecture G is the only MPF engine](../midgard/decisions/architecture-g-sole-mpf-engine.md).

## Benchmarks

Bench files run under `demo/midgard-node/vitest.bench.config.ts` and write a
JSON report. If a target is missed, the ticket reports the numbers to the
owner rather than relaxing the target.

| #   | Measures                                                                                                                   | Workload                                                                                 | Target                                        | Ticket                                  |
| --- | -------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- | --------------------------------------------- | --------------------------------------- |
| B1  | Catch-up throughput                                                                                                        | Sparse blocks (≤ 1 qualifying tx) and dense blocks (50 qualifying txs), credit window 50 | ≥ 1,000 blocks/s sparse; ≥ 100 blocks/s dense | F6                                      |
| B2  | Per-block apply at tip, every projection included, DA payload encoding excluded (`tests/benchmarks/l1-events-b2.bench.ts`) | 10 qualifying txs per block at N = 10^4, 10^5 and 10^6                                   | p99 ≤ 50 ms; p99(10^6) / p99(10^4) ≤ 1.2      | Trigger for N7–N9; target for F6 and N8 |
| B7  | Restart to ready                                                                                                           | Cursor at tip, N = 10^6                                                                  | ≤ 30 s                                        | U2                                      |

## Questions after measurement

- N8: the per-commit from-scratch root cross-check, if B2 cannot be met at
  N = 10^6 with it on every block.
