# Native MPF owner

Status: Active

Last reviewed: 2026-11-24 (owner node cache and RPC schema 2; one-shot WASM
path removed).

This Rust crate supplies the native `architecture-g-owner` binary used by the
node's MPF service. Runtime integration lives in
`../../src/services/mpf-native-owner/`; the commit path uses it through
`../../src/mpf/transition-trace.ts`.

From this directory:

```sh
make check
```

The Makefile uses locked Cargo dependencies. From `demo/midgard-node`, build
the runtime binary with `pnpm run native:mpf-owner:build`, and replay the
adversarial corpus against it with `pnpm run test:mpf:differential`. The
configured owner path is defined in `src/services/config.mpf.ts`;
package/release verification must bind the binary and ABI to the deployed node.

The binary runs only as the node's child, `architecture-g-owner --rpc`, and
speaks the RPC below on stdin/stdout.

## Owner node cache

The child holds the durable root (the marker), a bounded cache of trie nodes
keyed by content hash, and at most two open generations. It never holds the
whole trie and reads nothing at start; the parent sends the root with
`SetRoot`.

An apply walks the stream's keys from the marker through the cache, selecting
the nodes on the touched paths plus, where deletes could leave a branch with
one child, all of that branch's children (subtree sizes skip this when two
children must survive). If a node is not cached the child answers `NeedNodes`
with every absent hash it can name this round and changes nothing; the parent
reads them from LevelDB, sends them in `ProvideNodes`, and sends the same
`ApplyEvents` again. A child whose delete decides a branch's fate is fetched
before that branch's siblings, so no sibling is fetched speculatively. Each
round reaches at least one level deeper, so a path of n nodes takes at most n
rounds plus one for the siblings of a collapse at its bottom. When nothing is
absent the events are applied to an arena built from the selected nodes,
exactly as before.

Every provided record is authenticated against its hash on entry; one bad
record refuses the whole frame and caches nothing from it. A branch's subtree
size is trusted from the store: a wrong size can only make the arena stop at
an unavailable frontier, never produce a wrong root.

The budget (bytes, from `Hello`) bounds an estimate of the cache's heap use;
the formula is beside `entry_bytes` in `src/owner/node_cache.rs`. Eviction
uses two generations: a hit in the old one moves the node to the young one,
and a trim over budget drops the old generation and makes the young one old.
Trims run only after an apply that built its arena, after
`PromotionCommitted` and after `SetRoot`, never between the rounds of one
apply; after each, the cache is within the budget, and with a budget below
one block's working set that may mean empty. `PromotionCommitted` puts the
block's new reachable nodes in the cache. Memory is the budget plus the open
generations' arenas; per-block limits on events, ops, arena nodes and
generations stay.

## Owner RPC (schema 2)

All integers are little-endian. A frame is `length:u32` (bytes after this
field), then `MGRP`, `schema:u16 = 2`, `kind:u16`, `request_id:u64`,
`owner_epoch[16]`, `payload_length:u32`, the payload, and a 32-byte
BLAKE2b-256 digest of `MIDGARD-MPF-OWNER-RPC-V2` followed by every frame byte
from the magic to the end of the payload. Frames are at most 64 MiB, and
`ProvideNodes` and `PromotionChunk` payloads at most 16 MiB. Any request may
be answered with
`Error = 23`, whose payload is a UTF-8 message.

| Request                   | Reply                                              |
| ------------------------- | -------------------------------------------------- |
| `Hello = 1`               | `HelloAck = 2`                                     |
| `SetRoot = 3`             | `Ready = 6`                                        |
| `Fork = 7`                | `Forked = 8`                                       |
| `ApplyEvents = 9`         | `NeedNodes = 24` or `Applied = 10`                 |
| `ProvideNodes = 25`       | `NodesProvided = 26`                               |
| `Discard = 11`            | `Discarded = 12`                                   |
| `PreparePromotion = 13`   | `PromotionChunk = 14`..., then `PromotionEnd = 15` |
| `PromotionCommitted = 16` | `PromotionCommitted = 16`                          |
| `Diagnostics = 17`        | `DiagnosticsResult = 18`                           |
| `Ping = 19`               | `Pong = 20` (payload echoed)                       |
| `Shutdown = 21`           | `ShutdownAck = 22`                                 |

Payloads (offset: field):

- `Hello` (40 bytes): 0 `binary_sha256[32]`, 32 `cache_budget_bytes:u64`.
- `HelloAck` (86 bytes): 0 `schema:u16 = 2`, 2 `binary_sha256[32]`,
  34 `cache_budget_bytes:u64` (echoed), 42 `max_events:u32`,
  46 `max_ops:u32`, 50 `max_generations:u32 = 2`, 54 BLAKE2b-256 of
  `MIDGARD-MPF-OWNER-BLAKE2B-SELFTEST-V1` (32 bytes).
- `SetRoot` (32 bytes): `root[32]`; the empty root is allowed. Refused while
  any generation is open. The parent sends it once after `Hello` and again
  whenever it moves the durable root back. Until the first one, `Fork`,
  `ApplyEvents`, the promotion messages and `Diagnostics` answer
  "Architecture G owner is not ready".
- `Ready` (32 bytes): `root[32]`.
- `Fork` (32 bytes): `base_root[32]`. `Forked` (48): `id[16]`, `base_root[32]`.
- `ApplyEvents`: `id[16]`, then the `MEGO` event stream.
- `NeedNodes` (20 + 32n bytes): 0 `id[16]`, 16 `count:u32 = n`, 20 `n`
  hashes, sorted and unique, none of them cached. The generation is
  unchanged; resend the same `ApplyEvents` after providing them.
- `Applied` (100 + 32n bytes): 0 `id[16]`, 16 `candidate_root[32]`,
  48 BLAKE2b-256 of `MIDGARD-MPF-ARCH-G-EVENT-LOG-V1` and the event stream,
  80 `count:u32 = n`, 84 `n` event roots, then `proof_duration_ns:u64` (walk
  and arena build of the final round) and `mutation_duration_ns:u64`.
- `ProvideNodes` (1 byte to 16 MiB): one or more concatenated node records in
  the record format below. Already cached nodes are accepted.
- `NodesProvided` (4 bytes): `count:u32`, the records in the frame.
- `Discard` (16 bytes): `id[16]`; `Discarded` echoes it.
- `PreparePromotion` (16 bytes): `id[16]`. `PromotionChunk`: concatenated
  records of the generation's new reachable nodes, sorted by hash.
  `PromotionEnd` (116 bytes): `id[16]`, `base_root[32]`,
  `candidate_root[32]`, `record_count:u32`, BLAKE2b-256 of
  `MIDGARD-MPF-OWNER-PROMOTION-V1`, base, candidate and every record.
- `PromotionCommitted` (16 bytes): `id[16]`; the reply is `candidate_root[32]`.
  The marker moves to the candidate, the new nodes enter the cache, and every
  open generation is dropped.
- `Diagnostics`: empty. `DiagnosticsResult` (88 bytes): 0 `marker[32]`,
  32 `cached_nodes:u64`, 40 `cached_bytes:u64`, 48 `generations:u64`,
  56 `generated_nodes:u64`, 64 `generated_bytes:u64`, 72 `VmRSS_kib:u64`,
  80 `VmHWM_kib:u64`.

## Node records and events

All integers are little-endian. Hashes are canonical 32-byte BLAKE2b-256
values. Prefixes contain one nibble per byte so malformed digits can be rejected
without an ambiguous packed-tail representation.

Each node record is `kind:u8, hash[32], prefix_len:u8, prefix[prefix_len]`. A
leaf (kind 1) continues with `key_len:u16, value_len:u32, key, value`. A branch
(kind 2) continues with `size:u64, child_bitmap:u16`, then one 32-byte hash for
every set bit in ascending child order. In the event stream each event is
`op_count:u32` followed by its ops. An op is
`kind:u8, key_len:u16, value_len:u32, key, value`; insert is kind 1 (including
an empty value) and delete is kind 2 with a zero value length.

## Bounds

The event stream's own caps may only tighten the absolute limits: 100,000
events, 400,000 ops and 512 MiB per stream. An apply's arena holds at most
1,000,000 nodes. Every length uses checked parsing. Inserting a present key or
deleting an absent one fails the whole `ApplyEvents`, and the generation stays
unapplied.
