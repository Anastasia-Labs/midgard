# Architecture G is the only MPF engine

Status: Accepted (owner ruling, 2026-09-26).

Supersedes: [Benchmark-gated runtime options](benchmark-gated-runtime-options.md)
(2026-09-07).

## Decision

The node has one ledger MPF engine: Architecture G, the native owner process
(`architecture-g-owner`) that computes a block's ledger roots and hands commit
workers a fork of its durable root. The `legacy`, `overlay` and `event_flat` engines are
deleted, together with the `MPF_ENGINE` setting and the probes and tests that
existed only for them. No setting selects an engine and no fallback path
remains.

Startup fails closed unless the node is pinned to one owner binary:

- `MPF_NATIVE_OWNER_BINARY_PATH` names the binary (the release image ships it
  at `/app/native/architecture-g-owner`);
- `MPF_NATIVE_OWNER_BINARY_SHA256` is its lowercase 64-hex SHA-256 (the image
  ships the value next to the binary as `architecture-g-owner.sha256`).

Speculative commit building and its `SPECULATIVE_COMMIT_BUILD` setting were
deleted on 2026-10-03 (#752). A commit is built only once its base block has
landed.

## The owner's node cache (owner ruling, 2026-11-24)

The owner process no longer keeps the ledger trie in memory. It keeps a cache
of trie nodes keyed by content hash, bounded at 1 GiB
(`NATIVE_MPF_OWNER_NODE_CACHE_BYTES`, a constant), and asks the node for the
nodes a block's paths reach that it lacks. The node reads them from its
LevelDB ledger store in batches. Start, a child restart and moving the root
back to a retained root each send the child only the root.

This replaces the resident index, the two walks of every stored node at
start, the sidecar file and `MPF_NATIVE_OWNER_SIDECAR_PATH`, the 2,000,000
node ceiling, and the readiness reasons
`native_mpf_promotion_index_cap_exceeded` and
`native_mpf_restore_index_cap_exceeded`. Roots, the node encoding and the
LevelDB format are unchanged. The message layouts are in
[the crate README](../../../demo/midgard-node/native/mpf-event-flat-wasm/README.md).

Measured once on each ledger, before and after, with
`demo/midgard-node/tests/benchmarks/native-owner-node-cache.bench.ts` (blocks
of half spends and half new outputs, 64-byte values):

| 1,000,000 UTxOs                               | Resident trie       | Node cache                |
| --------------------------------------------- | ------------------- | ------------------------- |
| Start                                         | 22–26 s             | 11–27 ms                  |
| Roots of 10,000 operations, paths cached      | 0.13–0.18 s         | 0.16 s                    |
| Roots of 10,000 operations, first after start | as cached           | 0.53–0.71 s               |
| Roots of 40,000 operations, paths cached      | 0.50–0.58 s         | 0.57–0.59 s               |
| Roots of 40,000 operations, first after start | as cached           | 1.7–2.1 s                 |
| Roots with a cache a tenth of the trie        | not possible        | 0.54–0.65 s and 1.8–1.9 s |
| Promote 10,000 operations                     | 3.6–4.3 s           | 1.7–2.4 s                 |
| Promote 40,000 operations                     | 10.3–11.3 s         | 4.7–5.6 s                 |
| Child memory after start                      | 772 MiB             | 2 MiB                     |
| Child memory with every path cached           | 772 MiB and growing | 639 MiB                   |

At 200,000 UTxOs start went from 4.3–4.7 s to 13–26 ms, and a block's roots
cost the same as before once its paths are cached. The whole trie at
1,000,000 UTxOs is 1.4 million nodes and 662 MiB of cache, so at that size
every node stays cached; above the budget the node keeps running and reads
the colder nodes from the store.

The lower promote time is not the cache: the promotion check stopped reading
each new branch's stored children one at a time, which the store's `adopt`
already checks in batches before it writes.

## Why

- Expired-intent release and local-finalization owner recovery exist only on
  Architecture G; both rewind or promote the native owner's root. The other
  engines recovered corrections through a separate forward-reinclusion path
  that no live run exercised.
- The tracked default had diverged from every live run. The 2026-09-07 record
  kept `MPF_ENGINE=legacy` as the default, but every live deployment, devnet
  run and acceptance run since then has used `architecture_g`. The default
  described a configuration nobody operated or tested end to end.

## Open mainnet gates

This ruling does not say that Architecture G passed its acceptance gates. The
owner did not rule on them, and none has a retained passing report on the
current revision. These gates stay open and block mainnet:

- the 24-hour live soak on matching identities;
- the 50k and retained-growth root and candidate gates;
- release-image verification of the shipped owner binary;
- the final-build differential and crash/recovery coverage and a clean live
  lifecycle.

Deleting the other engines does not discharge any of these. A failed gate stays
failed; changing defaults, inflating limits or shortening runs cannot
discharge it.

## Carried forward from the superseded record

No commit is submitted as a child of an unconfirmed L1 commit. Unconfirmed
chaining needs a separate design for rollback, journals, provider acceptance,
and on-chain state linkage. Multi-block merge likewise requires an explicit
validator and recovery assessment.

DA capacity is a consensus and memory-admission boundary, not an environment
escape hatch. The retired 50k distribution fixture could not be regenerated
within the canonical payload bound, so its former timing results and commands
were removed. Any replacement must use the current payload format and actual
publication measurements; a fixed transaction count alone proves neither fit
nor latency. A future format change follows
[prelaunch replacement rules](prelaunch-format-replacement.md) or the relevant
shipped-version upgrade policy.

## Consequences

- Every node, test harness and benchmark runs the native owner. A fresh stack
  must provide the owner binary's path and SHA-256 before the node starts.
- Acceptance must bind the actual runtime, corpus, applied deployment,
  topology, and producer/consumer evidence.
- The TypeScript MPF store stays, in its `direct` and `overlay` modes, for the
  commit worker's scratch transactions trie, offline replay and the reference
  oracle. It is not a ledger engine.
