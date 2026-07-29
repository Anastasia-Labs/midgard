# Architecture F MPF prototype

This crate is the fail-closed prototype for Throughput Phase 3's native/WASM
contingency. It does not change a production default and is not linked into the
commit worker.

The intended build wiring is:

```sh
wasm-pack build --target nodejs --release \
  --out-dir ../../.architecture-f-wasm \
  native/mpf-event-flat-wasm
```

The checked-in `Makefile` pins the lockfile for `make check`, `make build`, and
`make differential`; the expanded commands above make the generated boundary
and output directory explicit for audit review.

The generated directory is ignored. A production integration may consume it
only through a pinned, reproducible build step that checks the Rust/Cargo lock,
WASM SHA-256, ABI version, and differential corpus before bundling.

After building, the independent adversarial differential and the two exact
prototype probes are:

```sh
node scripts/mpf-event-flat-wasm-prototype.mjs
node scripts/mpf-event-flat-wasm-prototype.mjs \
  --level=/tmp/midgard-mpf-growth-event-flat-e-100000-level \
  --utxos=100000 --txs=10000
node scripts/mpf-event-flat-wasm-prototype.mjs \
  --level=/tmp/midgard-mpf-growth-event-flat-e-1000000-level \
  --utxos=1000000 --txs=10000
```

The first command checks the hard-coded Forestry corpus roots for empty events,
delete/reinsert, collapse, split, and long hashed paths, and corrupts one raw
record to prove the call fails closed. The Level probes assert the fixture
marker and expected candidate root, re-read the unchanged marker, and report
raw-fetch, binary-encode, WASM, artifact, RSS, and conservative projection
timings. Their shared `scratchRootUpperMs` is the larger sum of both parallel
workers' CPU time plus serialization from the preceding exact pair, so it
overstates the unchanged scratch-root wall rather than hiding it.

## Architecture G retained-session gate

Architecture G supersedes the plan's earlier rejection of a full-memory MPF
alternative only as a measured decision point. The tradeoff is materially
different now: a compact native owner (not a JS object graph), a marker-keyed
authenticated cache whose contents are never authoritative, explicit rebuild
on marker/schema/digest mismatch, and Architecture F evidence that repeated raw
authentication plus artifact reconstruction—not canonical mutation alone—is
the dominant size-dependent work. Canonical Forestry hashing and Level's root
marker remain unchanged.

The minimal retained-session prototype deliberately does not load the full
ledger or integrate a service. It authenticates the exact touched closure into
a retained session during reported setup, forks an isolated generation, then
times only fixed binary event encoding plus sequential canonical mutation and
the mandatory root stream. It byte-compares every event root to the one-shot
engine and final known Forestry root, and re-reads the unchanged fixture marker:

```sh
node scripts/mpf-event-flat-wasm-prototype.mjs \
  --session \
  --level=/tmp/midgard-mpf-growth-event-flat-e-100000-level \
  --utxos=100000 --txs=10000
node scripts/mpf-event-flat-wasm-prototype.mjs \
  --session \
  --level=/tmp/midgard-mpf-growth-event-flat-e-1000000-level \
  --utxos=1000000 --txs=10000
```

Setup/authentication is outside this narrow hot-path projection but is not
outside operational acceptance. Before any production wiring, a later gate
must load and authenticate the complete marker-reachable index, measure startup
wall/RSS and bounded steady-state memory, prove restart/journal replay after
crashes at every submission/promotion boundary, and reject the design if those
recovery/liveness bounds are not operationally acceptable.

Retained-session event streams (`MEGO` v1) bind counts, caps, base root, and
ordered bytes under BLAKE2b-256. Root streams (`MEGR` v1) bind base/candidate and
all event roots. A session permits at most two active generations; stale/base
mismatched handles, malformed caps, corruption, and failed mutations are
fail-closed, with append-only rollback to the pre-call root/node watermark.

## Native64 full-index operational prototype

The `architecture-g-owner` binary is the next bounded gate, not production
commit wiring. A Node exporter opens each named Level fixture read-only, records
the durable marker, streams every content-addressed record into the canonical
binary ABI, re-reads the marker, and closes the database. The native64 owner
then content-authenticates every record, proves the complete closure is a
single marker-rooted tree with each leaf at its key-derived path, and compacts
it into sparse pools for prefixes, edges, branch Merkle caches, keys, and
values. It refuses more than 2,000,000 records, 512 MiB encoded input, or 2 GiB
estimated/observed resident memory.

```sh
cargo build --release --locked \
  --manifest-path native/mpf-event-flat-wasm/Cargo.toml \
  --bin architecture-g-owner
node scripts/mpf-architecture-g-owner-prototype.mjs --utxos=100000
node scripts/mpf-architecture-g-owner-prototype.mjs --utxos=1000000
```

The harness performs separate-process prepare/recover replay using the same
digest-bound event log, validates marker-matched sidecar restart, forces corrupt
and stale sidecar rebuilds, rejects a corrupt replay log, simulates isolated
fork/discard/promote and stale-generation rejection without writing either
fixture, and finally reopens/closes Level to prove clean lock release. The
sidecar is a cache: it contains the marker-bound canonical export under a file
digest, is written by fsync-plus-rename, and is discarded/rebuilt on any
mismatch. Level remains authoritative.

A production deployment would bundle two artifacts from the same locked Rust
source: the existing WASM differential oracle and a platform-specific native64
owner binary. The node would launch one supervised owner per ledger store,
transfer exclusive Level ownership to it, use a versioned local binary RPC
instead of benchmark files, persist the replay log in the pending-finalization
journal, pin binary/ABI/sidecar schema hashes in release metadata, and expose
startup/rebuild/RSS/lease metrics. None of those deployment changes are part of
this prototype.

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

The prototype currently emits a compact sparse full-record delta, not the
smaller predecessor-patch representation. That is deliberate: the growth gate
must be demonstrated before adding a more complex transfer encoding.

## Memory model

The call owns one immutable raw proof plus append-only generated nodes. Opaque
untouched children remain 32-byte commitments. Production wiring must budget
`input + output + ~1.8 KiB * arena_nodes + leaf key/value bytes`, reject before
the configured cap, and free the whole WASM instance or result on error. There
is no cache across base-root markers and no fail-open JS hashing fallback.
