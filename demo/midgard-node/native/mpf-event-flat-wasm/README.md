# Native MPF owner and WASM differential oracle

Status: Active

Last reviewed: 2026-09-07 (integration and build commands).

This Rust crate supplies the native `architecture-g-owner` binary used by the
node's MPF service and the WASM differential oracle. Runtime integration lives
in `../../src/services/mpf-native-owner/`; the commit path uses it through
`../../src/mpf/transition-trace.ts`. This is no longer an unintegrated prototype.

From this directory:

```sh
make check
make build
make differential
```

The Makefile uses locked Cargo dependencies. The WASM output is generated in
`../../.architecture-f-wasm`. From `demo/midgard-node`, build the runtime binary
with `pnpm run native:mpf-owner:build`. The configured owner path is defined in
`src/services/config.ts`; package/release verification must bind the binary and
ABI to the deployed node. Prototype benchmark scripts remain measurement tools,
not startup or recovery procedures.

The following describes the one-shot differential ABI; the native service's
RPC, journal, and recovery implementation is maintained alongside the runtime.

## Binary ABI v1

All integers are little-endian. Hashes are canonical 32-byte BLAKE2b-256
values. Prefixes contain one nibble per byte so malformed digits can be rejected
without an ambiguous packed-tail representation.

Input header (72 bytes):

| Offset | Field                  |
| -----: | ---------------------- |
|      0 | `MEF6` magic           |
|      4 | `u16 version = 1`      |
|      6 | `u16 flags = 0`        |
|      8 | `u32 max_records`      |
|     12 | `u32 max_events`       |
|     16 | `u32 max_ops`          |
|     20 | `u32 max_input_bytes`  |
|     24 | `u32 max_output_bytes` |
|     28 | `u32 record_count`     |
|     32 | `u32 event_count`      |
|     36 | `u32 op_count`         |
|     40 | `base_root[32]`        |

Each authenticated raw record is `kind:u8, hash[32], prefix_len:u8,
prefix[prefix_len]`. A leaf continues with `key_len:u16, value_len:u32, key,
value`. A branch continues with `size:u64, child_bitmap:u16`, then one 32-byte
hash for every set bit in ascending child order. Each event is `op_count:u32`.
An op is `kind:u8, key_len:u16, value_len:u32, key, value`; insert is kind 1
(including an empty value) and delete is kind 2 with a zero value length.

Output header (120 bytes): `MEFO`, version/flags, event count, dirty-record
count, delta offset/length, base root, candidate root, and a BLAKE2b digest
binding the domain string, base/candidate roots, every ordered event root, and
the aggregate event/record/byte counts and compact sparse dirty closure. Event roots immediately follow the header;
the reconstructable dirty records use the same binary record format.

## Bounds and failure boundary

Caller caps may only tighten the absolute limits: 1,000,000 raw records,
100,000 events, 400,000 ops, 512 MiB input/output, and 1,000,000 arena nodes.
Every length uses checked parsing. Raw records are content-authenticated before
mutation; their available subgraph must be rooted at the pinned base root.
Strict insert/delete semantics abort the single call on the first invalid op.
No partial output or durable write exists. The JS owner must compare the durable
marker to `base_root` before Level reads and again before accepting output; a
marker change discards the entire result. The receiver authenticates and stages
the returned closure before one atomic nodes-plus-marker promotion.

The one-shot differential ABI emits a compact sparse full-record delta, not the
smaller predecessor-patch representation. That is deliberate: the growth gate
must be demonstrated before adding a more complex transfer encoding.

## Memory model

The call owns one immutable raw proof plus append-only generated nodes. Opaque
untouched children remain 32-byte commitments. Caller memory accounting must include the input, output, arena nodes, and leaf
key/value allocations. The byte caps do not by themselves bound total process
RSS. Callers must reject before their configured memory cap and free the whole
WASM instance or result on error. There
is no cache across base-root markers and no fail-open JS hashing fallback.
