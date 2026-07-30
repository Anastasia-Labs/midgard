# Architecture G production integration boundary

**Status:** production owner, RPC, journal, worker, runtime, and release wiring
are implemented behind the explicit `MPF_ENGINE=architecture_g` opt-in. The
formal retained-growth, canonical fresh-process 50k x20, clean live E2E, soak,
and separate default/cap review gates remain open; no default has changed.

**Historical selection evidence:** the retained-session 100k→1M projection
grew `+2.8186%`; the native64 full-index prototype authenticated the real 1M
fixture in
`2434.492264 ms` on marker-matched restart, uses `881,396 KiB` steady and
`1,886,504 KiB` peak in its intentionally duplicated two-generation recovery
simulation, and leaves the fixture marker/logical hash unchanged.

This document fixed the production boundary before shared Phase 4 and
commit-path integration. Sections 1–4 remain the binding design; Section 5 now
records the implemented inventory, and Section 7 separates current evidence
from the gates that still block closure.

## 1. Ownership and source of truth

The service is long-lived and owned by the main Node process.

- Node owns the only Level handle and lock for the ledger MPF, reads the
  durable marker, validates closure records returned for promotion, and performs
  the single atomic generated-nodes-plus-`__root__` batch.
- The pinned native64 child never opens Level. It owns the compact authenticated
  index, cached 15-node branch Merkle trees, active append-only generations,
  and canonical Forestry-compatible mutation implementation.
- Level's marker and content-addressed records remain authoritative. A native
  sidecar/index is a cache bound to the Level path, marker, ABI/schema, binary
  SHA-256, and complete closure digest. Any mismatch discards and rebuilds it.
- `legacy`, `overlay`, and `event_flat` remain available. Architecture G is a
  separate opt-in engine and cannot become the default until differential,
  crash, soak, and fresh 50k×20 p95 gates pass.

The main service holds the Level lock for its lifetime. Commit worker threads
receive a transferred `MessagePort` client; they do not open the ledger Level
path. The main process routes worker requests to the supervised native child
over its private stdio pipes.

## 2. Length-prefixed child RPC

Every child frame is:

| Bytes | Field                                              |
| ----: | -------------------------------------------------- |
|     4 | little-endian frame length, excluding this prefix  |
|     4 | `MGRP` magic                                       |
|     2 | schema version (`1`)                               |
|     2 | message kind                                       |
|     8 | request id                                         |
|    16 | owner epoch (new random value on each child start) |
|     4 | payload length                                     |
|     N | payload                                            |
|    32 | BLAKE2b-256 domain-separated frame digest          |

The digest binds `MIDGARD-MPF-OWNER-RPC-V1`, every header byte after the length,
and the payload. Unknown schemas/kinds, duplicate/out-of-order request ids,
wrong epochs, length/digest mismatches, trailing bytes, unsolicited responses,
and cap violations terminate the child and fail every in-flight request. There
is no JSON or fail-open hash fallback.

Initial caps/timeouts:

- frame `64 MiB`; load/promotion chunk `16 MiB`;
- resident index `2,000,000` nodes / `2 GiB` estimated and observed RSS;
- generated closure `1,000,000` nodes / `1 GiB`;
- `100,000` events / `400,000` ops; two active generations;
- handshake `5 s`, load/rebuild `120 s`, hot apply `30 s`, promotion stream
  `120 s`, graceful shutdown `10 s`.

Message kinds:

1. `Hello` / `HelloAck`: schema, binary SHA, compiled caps, BLAKE2b self-test.
2. `LoadBegin`, ordered `LoadChunk`, `LoadEnd` / `Ready`: canonical compact
   records rooted at the current marker, aggregate digest, node/edge/byte/RSS
   diagnostics.
3. `Fork` / `Forked`: expected base root to opaque 16-byte generation handle.
   Handles are scoped to the owner epoch and never reused.
4. `ApplyEvents` / `Applied`: digest-bound ordered event log to every ordered
   post-event root, candidate root, counters, and the same opaque handle. This is
   the only hot-build payload/result.
5. `Discard` / `Discarded`: terminal handle release.
6. `PreparePromotion`, `PromotionChunk`, `PromotionEnd`: one reachable generated
   closure, sorted by content hash, bound to base/candidate/handle and aggregate
   counts/digest.
7. `PromotionCommitted`: sent by Node only after the Level batch succeeds. The
   child then advances the resident durable generation and invalidates siblings.
8. `Diagnostics`, `Ping`, `Shutdown` and their responses.

The worker-facing `MessagePort` protocol uses the same request ids, epochs,
caps, and typed payloads but does not expose load/promotion chunks to ordinary
build callers.

## 3. Service and overlay interfaces

New public types (names are binding unless review changes this document too):

```ts
type NativeMpfGenerationHandle = {
  readonly ownerEpoch: Uint8Array; // exactly 16 bytes
  readonly generationId: Uint8Array; // exactly 16 bytes
  readonly baseRoot: string;
};

type NativeMpfApplyResult = {
  readonly handle: NativeMpfGenerationHandle;
  readonly candidateRoot: string;
  readonly eventRoots: readonly string[];
  readonly eventLogDigest: string;
};

interface NativeMpfOwnerClient {
  fork(baseRoot: string): Promise<NativeMpfGenerationHandle>;
  applyEvents(
    handle: NativeMpfGenerationHandle,
    eventLog: Uint8Array,
  ): Promise<NativeMpfApplyResult>;
  discard(handle: NativeMpfGenerationHandle): Promise<void>;
}

interface NativeMpfOwnerService extends NativeMpfOwnerClient {
  createWorkerPort(): MessagePort;
  promote(handle: NativeMpfGenerationHandle): Promise<void>;
  recover(replay: PersistedNativeMpfReplay): Promise<void>;
  diagnostics(): Promise<NativeMpfOwnerDiagnostics>;
  close(): Promise<void>;
}
```

The Architecture G `LedgerOverlayHandle` adapter delegates `fork`, ordered
block-delta application, `rootHex`, `promote`, and `discard` to this client. It
does not expose a `MidgardMpf`, Level path, mutable arena, or transferable node
closure to a worker.

## 4. Promotion, crash, and recovery ordering

The existing journal-before-submit and post-submit-promotion rules remain:

1. Build in one generation and verify the returned root count equals the exact
   source-event count; construct every transition step from the returned roots.
2. Before signing/submission, persist the exact event-log bytes, its digest,
   base/candidate roots, packed ordered event roots, owner schema, and binary
   SHA in the pending-finalization journal.
3. Sign and submit.
4. Ask the child for the one generated closure. Node independently validates
   framing/digest, content hashes, sorted uniqueness, generated closure
   reachability, candidate root, and current Level marker.
5. Node performs one atomic Level batch containing all generated records and the
   candidate marker, then sends `PromotionCommitted`.

Failure model:

- Worker crash: the main service survives; its leased handle is discarded on
  timeout unless the durable journal owns it.
- Child crash before submit: restart/reload and rebuild; no marker changed.
- Child crash after submit but before promotion: restart from the Level marker,
  replay the journal log in one comparison generation, compare every root, then
  stream/promote. Production recovery never retains the old and comparison
  generations simultaneously; the prototype's 1.8 GiB peak is therefore an
  upper bound, not the intended steady path.
- Node crash during Level batch: Level atomically exposes the old or candidate
  marker. Restart treats the marker as authority. Candidate means promotion is
  already complete; base means replay and retry.
- Marker/replay/root/binary mismatch: halt the commit path and require explicit
  operator recovery. Never rewrite or accept a different root silently.

## 5. Implemented file inventory

Architecture G implementation files:

- `demo/midgard-node/src/services/mpf-native-owner/protocol.ts`: schemas,
  message kinds, caps, handles, frame types.
- `.../codec.ts`: bounded frame/event/root/closure binary codecs and digest
  domains.
- `.../service.ts`: main-owned Level/child supervisor and atomic promotion.
- `.../client.ts`: worker `MessagePort` client with request timeouts/epoch
  validation.
- `.../layer.ts` and `.../index.ts`: Effect service/layer and exports.
- `demo/midgard-node/native/mpf-event-flat-wasm/src/rpc.rs`: native framed RPC
  loop over stdin/stdout.
- `demo/midgard-node/tests/mpf-native-owner-protocol.test.ts`,
  `mpf-native-owner-service.test.ts`, and
  `mpf-native-promotion-recovery.test.ts`.
- `demo/midgard-node/src/database/migrations/sql/0024_architecture_g_replay_log.sql`,
  registered by `src/database/migrations/index.ts`.

Integrated production surfaces:

- `src/services/config.ts`: opt-in engine, binary path/SHA, caps, timeouts,
  sidecar path and restart policy.
- main application layer/entrypoint: start one owner service and close it after
  worker/fiber shutdown.
- `src/fibers/block-commitment.ts`: transfer a main-router `MessagePort` to the
  commit worker only for Architecture G.
- `src/workers/commit-block-header.ts`: construct the native overlay adapter;
  never open the ledger Level path in G mode.
- `src/workers/utils/mpf.ts`: engine union/factory only; keep existing engines
  unchanged.
- `src/workers/commit-block-header/submission.ts`: persist replay before submit
  and invoke main-owned post-submit promotion.
- pending-finalization DB module and recovery fiber: read/write/replay the new
  fields.
- `package.json`: locked native build/check commands and release copy step.
- `Dockerfile`: pinned Rust builder stage; copy only the stripped native owner
  and its SHA manifest into the Node runtime image. Runtime contains no Cargo or
  compiler.
- Compose/runtime manifests: binary/sidecar paths, memory limit with headroom
  above the hard owner cap, health/readiness signal, and persistent sidecar
  volume. No environment flips `MPF_ENGINE` by default.

Journal migration columns:

- `mpf_owner_schema smallint`, `mpf_owner_binary_sha256 bytea`;
- `mpf_replay_base_root bytea`, `mpf_replay_candidate_root bytea`;
- `mpf_replay_event_log bytea`, `mpf_replay_event_log_digest bytea`;
- `mpf_replay_event_roots bytea` (32-byte concatenation plus separately checked
  event count).

Rows from older engines leave the fields null. Architecture G refuses to submit
unless every field is present and internally consistent. No compatibility
fallback converts or fabricates a replay log.

## 6. Integration state and remaining sequence

| Surface                                 | Integration state                                          | Remaining rule                                                                                     |
| --------------------------------------- | ---------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `mpf.ts`, commit worker, submission     | Integrated with Phase 4 handle/recovery work               | Re-run the combined focused suites after all shared edits settle.                                  |
| pending-finalization schema/DB/recovery | Migration `0024` and strict dual-null read path integrated | Verify migration, post-submit replay, and both atomic-promotion crash boundaries together.         |
| `block-commitment.ts` worker lifecycle  | Main-owned port and lease lifecycle integrated             | Verify real worker failure, stale epoch rejection, and owner survival.                             |
| config/package/Docker/Compose           | Integrated behind explicit opt-in                          | Verify pinned SHA, runtime image contents, cgroup headroom, and readiness in the release image.    |
| native crate and owner service          | Integrated                                                 | Re-run locked Rust, protocol, service, differential, and malformed-frame suites on the final tree. |

Implementation order 1–6 is complete in the working tree. Remaining closure
order:

1. Build the final locked native binary and Node distribution; record the
   binary, source, Git, and diff identities.
2. Re-run the full differential, protocol/service, malformed-frame,
   crash/restart, atomic-promotion, journal-replay, and shared Phase 4 focused
   suites.
3. Run the formal retained-growth root gate and then its cross-bound production
   commit-candidate gate over fresh immutable 100k, 300k, and 1M fixtures with
   three fresh processes per fixture and one identical canonical 10k-op stream.
   Require maximum/minimum fixture medians within 10%, zero confirmed-ledger
   full scans, zero submission attempts, no journal-row change, deterministic
   complete root tuples, exact root-gate/candidate-gate equality, and unchanged
   fixture marker/logical digest/record count.
4. Run the formal 50k root gate and then its cross-bound production
   commit-candidate gate over 20 fresh processes on a dependency-closed first
   50k prefix of one explicitly named, manifest-verified Phase 1 corpus slice.
   Require nearest-rank p95 below 10 seconds at both gates, zero submission
   attempts, no journal-row change, deterministic complete root tuples, exact
   cross-gate equality, and unchanged corpus/slice/fixture identities.
5. Build and inspect the release image: only the stripped pinned owner and SHA
   manifest enter the Node runtime; no Cargo/compiler is present; cgroup
   headroom and readiness checks pass.
6. Run a clean opt-in Architecture G deployment through deposit, L2 submit, DA,
   header merge/finalization, readiness, DB/balance checks, child restart, and
   post-submit recovery. Complete the required soak with zero audit divergence,
   stable memory/caps, no hot-path full-ledger scans, and a non-growing DA/merge
   queue slope.
7. Obtain an independent final review of implementation and evidence.
8. Only then consider an explicit default or cap/model/root-check change in a
   separate reviewed commit. Until then the strict legacy defaults remain.

## 7. Execution evidence and open gates

| Requirement                         | Current evidence                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Closure state                                                                                                                                                               |
| ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Production owner/RPC                | `protocol.ts`, `codec.ts`, `client.ts`, `service.ts`, `layer.ts`, and the Rust `rpc.rs`/`owner.rs` implementation are wired through the main-owned owner service. Protocol, caps, epoch, pinned-SHA, and fail-closed paths have focused tests.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Implemented; final-tree rerun pending.                                                                                                                                      |
| Runtime/worker ownership            | Architecture G starts one main-owned service, transfers worker ports, prevents the commit worker from opening the ledger path, and retains `legacy`, `overlay`, and `event_flat`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Implemented; clean live validation pending.                                                                                                                                 |
| Journal/promotion/recovery          | Migration `0024`, all-or-none replay fields, pre-submit replay persistence, atomic Level promotion, owner restart, and post-submit replay are wired. Focused tests cover old-or-candidate marker outcomes and stale/mismatched recovery rejection.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Implemented; final-tree crash suite and live restart pending.                                                                                                               |
| Release/runtime boundary            | The Dockerfile uses a pinned Rust builder and copies the owner binary plus SHA manifest into the Node runtime. Compose retains the 8 GiB node limit and `/readyz` healthcheck; `.env.example` leaves Architecture G commented out.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Implemented; final image build/inspection pending.                                                                                                                          |
| Historical architecture selection   | The retained-session prototype cleared projected growth at `+2.8186%`; the native64 prototype authenticated the complete 1M fixture and remained below its 2 GiB owner cap.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Proven for selection only; not production closure evidence.                                                                                                                 |
| Production differential/crash gates | The named `test:mpf:differential` command now builds and identifies the release owner and binds the seeded adversarial corpus to `legacy`, `overlay`, and Architecture G across insert/from-list fixtures: 6 runs, 12 independent proof checks, complete roots, and every transition pre/post root. Separate focused tests cover protocol corruption, real child SIGKILL/stale epoch, atomic promotion crash boundaries, and journal recovery.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Named adversarial differential closed; broader final-tree crash/recovery command and artifacts pending.                                                                     |
| Formal retained-growth gates        | The root gate and full production commit-candidate gate require fresh processes, immutable fixtures, one canonical workload identity, complete deterministic roots, zero full-scan/submission/journal mutation, exact cross-gate roots, and <=10% median spread across 100k/300k/1M.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Pending resource release and durable fixtures.                                                                                                                              |
| Canonical fresh 50k x20 gates       | Both gates require exact formal cardinality. Corpus preparation verifies the Phase 1 manifest/index/generation evidence and full named slice, selects a dependency-closed first 50k prefix, resolves exact funding outputs, and records its boundary proof. The candidate gate must equal the root gate and independently clear nearest-rank p95 below 10 seconds.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Pending resource release. The retained 2026-07-12 corpus requires the explicit identity rebind below.                                                                       |
| Clean E2E and soak                  | Opt-in runtime path, readiness diagnostics, cgroup checks, and recovery hooks exist. The fail-closed 86,400-second runner/report schema and offline artifact verifier are documented in `docs/benchmark-scenarios/phase-3-architecture-g-soak.md`; test-only timing cannot produce production evidence. Full corpus SHA/cardinality/uniqueness and the final source identity are captured in an immutable artifact before lifecycle timing; the measured workload consumes that source-bound artifact without repeating the multi-hour corpus scan. Bounded per-chain consumed-prefix digests are recomputed during the verifier's one offline corpus hash pass, including a same-size/restored-mtime adversarial case. Formal load generation requires a SHA-bound, non-root, finite-memory cgroup-v2 scope with a cgroup-effective cpuset disjoint from the node; all four `/proc` UID values are retained and effective UID zero fails. The node PID is not operator input: the runner rejects caller Docker routing/configuration, binds the trusted absolute Docker client bytes and local Unix socket/daemon identity, and executes the client with a sanitized environment. Two exact inspections around double-read process identity captures bind the Phase 1 container host PID, immutable image, Architecture G runtime, restart/health state, healthcheck, and readiness/metrics published ports. After the initial readiness/metrics/database probe, a second immutable artifact repeats the trusted Docker, daemon, container, coordinator, and node identity captures before lifecycle timing. The verifier checks both artifact identities and current client/socket integrity. The successful pre-lifecycle readiness observation becomes sample zero without a second network call; the lifecycle timestamp remains null until corpus/source validation, measured isolation, readiness, and reinspection all pass. Output, timing, endpoint, identity, preflight, isolation, readiness, and reinspection setup failures retain failed report and verification artifacts with exact phase labels. The gate pins achieved offered/accepted/saturation floors, actual measured elapsed, pre-spawn through post-drain sampling, and submit-attempt schema/cardinality/identity. Clean-E2E driver stdout/stderr is secret-scanned and redacted before retention, with any redaction failing the step. | Implementation closed; fresh deployment, full real flow, restart exercise, 2 TB local NVMe, sufficient verified Preprod funding, and the actual 24-hour run remain pending. |
| Default/cap decision                | Current defaults remain `legacy`, `insert`, `every_block`, root parallelism off, static planner, and `10000/40000/40000` caps.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Intentionally pending all preceding gates and separate review.                                                                                                              |

### Formal gate operator commands

Run from `demo/midgard-node`. `28-31` is the explicit four-CPU affinity set;
the coordinator must reserve it before execution. The corpus, manifest, index,
verification evidence, and 4,096 wallet records below are the retained, verified Phase 1 artifacts superseding the absent 2026-07-09 path. This is an explicit identity rebind, not a symlink or an implicit path substitution. `--prepare-corpus-only`
emits a non-gate preparation schema (`formalGateEvidence: false`); it cannot
produce a pass verdict.

### Explicit Phase 1 corpus identity rebind (2026-07-13)

The plan-named `logs/phase-1-full-corpus-20260709T002743Z` corpus root, manifest, index, generation result, and wallet directory are absent. The retained `logs/throughput-resume-20260712T154400Z/phase1-benchmark/corpus-live-4096` set is the accepted replacement only because its manifest and generation result declare the exact formal shape (4,096 chains, depth 748, 3,063,808 rows, 5,000 TPS for 600 seconds) and its binding records the live wallet, funding, and deployment identities.

Before any gate, record these immutable identities and recreate a binding that names the generation result, not the standalone verifier:

```bash
set -euo pipefail
export PHASE1_RETAINED_ROOT="$PWD/logs/throughput-resume-20260712T154400Z/phase1-benchmark"
export PHASE1_BINDING="$PHASE1_RETAINED_ROOT/phase1-formal-binding.json"
export RUN="$PWD/logs/phase-3-architecture-g-formal-20260713-rebind"
export PHASE1_REBOUND_BINDING="$RUN/phase1-formal-binding-rebound.json"
export GENERATION_RESULT="$PHASE1_RETAINED_ROOT/corpus-generate-live-4096.log"
export STRESS_CORPUS_PATH="$PHASE1_RETAINED_ROOT/corpus-live-4096/corpus.ndjson"
export STRESS_CORPUS_INDEX_PATH="$STRESS_CORPUS_PATH.index.ndjson"
export STRESS_CORPUS_MANIFEST_PATH="$STRESS_CORPUS_PATH.manifest.json"
export STRESS_CORPUS_SLICE_ID=default
export STRESS_CORPUS_SHAPE=chain
export STRESS_CORPUS_READAHEAD_ROWS=50
export CORPUS_SHA256=61c53f60e2993bbd09df61510437d2f944a87c00aef135025404e5a4c7ef0e59
export INDEX_SHA256=244747e844fd6320ef8af362d471c35b474e9cafef262082345fb508df52f629
export MANIFEST_SHA256=a3cef4073d241671436a6812a5fc69f7baaae0ba4ee259e55e879942509f5f84
export GENERATION_RESULT_SHA256=87e5fef138bcd17b10524e792db2bf4d8274012a93e737c2fbe2a23fcbc9973b
export WALLET_SET_SHA256=51232e6bb68e7a745f9a31ffff02b93c22408a314d4644bf3a3af3f1bd72775e
export FUNDING_SET_SHA256=f1773409a46edba272b44c156fed16492f5b51712397de77e33a97a70f1c5f71
export DEPLOYMENT_MANIFEST_ID=bbdfca85031de1c9adea89239f51180c1ba804e926f9c66c2b11294b666f7ea5
export NODE_IMAGE_ID="$(jq -r .nodeImageId "$PHASE1_BINDING")"
export NODE_CONTAINER_ID="$(jq -r .nodeContainerId "$PHASE1_BINDING")"
test "$(sha256sum "$PHASE1_RETAINED_ROOT/corpus-live-4096/corpus.ndjson" | cut -d" " -f1)" = "$CORPUS_SHA256"
test "$(sha256sum "$PHASE1_RETAINED_ROOT/corpus-live-4096/corpus.ndjson.index.ndjson" | cut -d" " -f1)" = "$INDEX_SHA256"
test "$(sha256sum "$PHASE1_RETAINED_ROOT/corpus-live-4096/corpus.ndjson.manifest.json" | cut -d" " -f1)" = "$MANIFEST_SHA256"
test "$(sha256sum "$GENERATION_RESULT" | cut -d" " -f1)" = "$GENERATION_RESULT_SHA256"
test "$(jq -r .walletSetIdentity.walletSetSha256 "$STRESS_CORPUS_MANIFEST_PATH")" = "$WALLET_SET_SHA256"
test "$(jq -r .walletSetIdentity.fundingSetSha256 "$STRESS_CORPUS_MANIFEST_PATH")" = "$FUNDING_SET_SHA256"
mkdir -p "$RUN"
node scripts/create-phase1-formal-binding.mjs \
  --out "$PHASE1_REBOUND_BINDING" \
  --generation-result "$GENERATION_RESULT" \
  --deployment-manifest-id "$DEPLOYMENT_MANIFEST_ID" \
  --node-image-id "$NODE_IMAGE_ID" \
  --node-container-id "$NODE_CONTAINER_ID"
export PHASE1_REBOUND_BINDING_SHA256="$(sha256sum "$PHASE1_REBOUND_BINDING" | cut -d" " -f1)"
test "$(node -p 'process.version')" = "v22.22.2"
export ARCH_G_RUNTIME_VERSION="$(node -p 'process.version')"
export ARCH_G_RUNTIME_EXECUTABLE="$(node -p 'process.execPath')"
export ARCH_G_RUNTIME_EXECUTABLE_SHA256="$(sha256sum "$ARCH_G_RUNTIME_EXECUTABLE" | cut -d" " -f1)"
```

The generation log is JSON with schema `midgard-stress-corpus-generation-v1` despite its `.log` suffix and is the formal `--corpus-verification` input. `corpus.ndjson.verify.json` and `standalone-verify-result.json` use `midgard-stress-corpus-verification-v1` and must be rejected by the Architecture G formal gate. Silent path substitution, symlinks, regeneration, reduced inputs, or reuse of the old binding is forbidden; retain the new binding SHA and the complete bound deployment, node, wallet, funding, corpus, generation, harness, and runtime identities in the run evidence.

```bash
set -euo pipefail
export CPUSET=28-31
export RUN="$PWD/logs/phase-3-architecture-g-formal-20260713-rebind"
export CORPUS="$PHASE1_RETAINED_ROOT/corpus-live-4096/corpus.ndjson"
export MANIFEST="$CORPUS.manifest.json"
export INDEX="$CORPUS.index.ndjson"
export VERIFICATION="$PHASE1_RETAINED_ROOT/corpus-generate-live-4096.log"
export WALLETS="$PHASE1_RETAINED_ROOT/wallets"
export BINARY="$PWD/native/mpf-event-flat-wasm/target/release/architecture-g-owner"
export PROBE="$PWD/dist/mpf-engine-probe.js"
mkdir -p "$RUN/50k/root" "$RUN/growth/root"

pnpm run native:mpf-owner:build
pnpm run build

node scripts/mpf-architecture-g-gate.mjs --mode=50k \
  --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
  --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
  --runtime-version="$ARCH_G_RUNTIME_VERSION" \
  --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
  --prepare-corpus-only=true --fixture-root="$RUN/50k/fixtures" \
  --cpuset="$CPUSET" --corpus="$CORPUS" --corpus-manifest="$MANIFEST" \
  --corpus-index="$INDEX" --corpus-verification="$VERIFICATION" \
  --corpus-slice-id=default --wallets-dir="$WALLETS" \
  --out="$RUN/50k/root/summary.json" > "$RUN/50k/corpus-preparation.json"

node scripts/mpf-architecture-g-gate.mjs --mode=growth \
  --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
  --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
  --runtime-version="$ARCH_G_RUNTIME_VERSION" \
  --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
  --prepare-corpus-only=true --fixture-root="$RUN/growth/fixtures" \
  --cpuset="$CPUSET" --corpus="$CORPUS" --corpus-manifest="$MANIFEST" \
  --corpus-index="$INDEX" --corpus-verification="$VERIFICATION" \
  --corpus-slice-id=default --wallets-dir="$WALLETS" \
  --out="$RUN/growth/root/summary.json" > "$RUN/growth/corpus-preparation.json"
```

Create each retained fixture from the exact funding roots selected above plus
deterministic filler to the declared cardinality. The creator records the
payload aggregate later stamped into the production candidate build.

The fresh 1M fixture requires an 8 GiB V8 old-space limit and 10–12 GiB of
available host memory so that the V8 heap, native/LevelDB allocations, and OS
overhead all have headroom. Do not start it until that host headroom is
reserved. The retained 4 GiB attempt failed with a JavaScript heap OOM in
`logs/phase-3-fresh-closure-20260712T180000Z/fixture-create-1000000.raw.log`
(SHA-256 `f1a919e970c2fe6715a0544bdfdba058d834c6922f22bba32e26d793329cba2b`),
while the 8 GiB retry completed the 1,000,000-entry fixture in
`logs/phase-3-fresh-closure-20260712T180000Z/fixture-create-1000000-8g.raw.log`
(SHA-256 `898274574546bf1266fbbfcee1418e876087c015532a03bd8c9fc0abdde78a42`).

```bash
create_fixture() {
  mode="$1"; txs="$2"; size="$3"
  dir="$RUN/$mode"
  heap_mb=4096
  if [ "$size" -eq 1000000 ]; then
    heap_mb=8192
  fi
  mkdir -p "$dir/fixtures"
  slice="$dir/root/canonical-corpus-slice.ndjson"
  funding="$dir/root/canonical-corpus-funding.json"
  MPF_ENGINE_PROBE_CREATE_LEVEL_FIXTURE=true \
  MPF_ENGINE_PROBE_LEVEL_DB="$dir/fixtures/utxos-$size-level" \
  MPF_ENGINE_PROBE_INITIAL_UTXOS="$size" \
  MPF_ENGINE_PROBE_TXS="$txs" \
  MPF_ENGINE_PROBE_CORPUS_SLICE_PATH="$slice" \
  MPF_ENGINE_PROBE_CORPUS_SLICE_SHA256="$(sha256sum "$slice" | cut -d' ' -f1)" \
  MPF_ENGINE_PROBE_CORPUS_FUNDING_PATH="$funding" \
  MPF_ENGINE_PROBE_CORPUS_FUNDING_SHA256="$(sha256sum "$funding" | cut -d' ' -f1)" \
  NODE_OPTIONS=--max-old-space-size="$heap_mb" \
    taskset -c "$CPUSET" node --expose-gc "$PROBE" \
      > "$dir/fixture-create-$size.raw.log"
  tail -n 1 "$dir/fixture-create-$size.raw.log" \
    > "$dir/fixture-create-$size.json"
  jq -e --argjson size "$size" \
    '.fixtureCreated == true and .initialUtxoCount == $size and
     .canonicalFunding != null and .utxoPayloadAggregate.entryCount == $size' \
    "$dir/fixture-create-$size.json" >/dev/null
}

create_fixture 50k 50000 1000000
create_fixture growth 10000 100000
create_fixture growth 10000 300000
create_fixture growth 10000 1000000
```

Run the complete-root gates. Formal cardinality is fixed in code: 50k is
20 fresh processes x 50,000 transactions; growth is three fresh processes per
100k/300k/1M fixture x one identical 10,000-transaction prefix. Supplying
`--runs` or `--transactions` cannot reduce a formal run.

```bash
pnpm run bench:mpf:architecture-g:50k -- \
  --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
  --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
  --runtime-version="$ARCH_G_RUNTIME_VERSION" \
  --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
  --fixture-root="$RUN/50k/fixtures" --cpuset="$CPUSET" \
  --corpus="$CORPUS" --corpus-manifest="$MANIFEST" --corpus-index="$INDEX" \
  --corpus-verification="$VERIFICATION" --corpus-slice-id=default \
  --wallets-dir="$WALLETS" --out="$RUN/50k/root/summary.json"

pnpm run bench:mpf:architecture-g:growth -- \
  --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
  --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
  --runtime-version="$ARCH_G_RUNTIME_VERSION" \
  --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
  --fixture-root="$RUN/growth/fixtures" --cpuset="$CPUSET" \
  --corpus="$CORPUS" --corpus-manifest="$MANIFEST" --corpus-index="$INDEX" \
  --corpus-verification="$VERIFICATION" --corpus-slice-id=default \
  --wallets-dir="$WALLETS" --out="$RUN/growth/root/summary.json"
```

Prepare one production candidate input per fixture. Each input is SHA-bound to
the final binary, original corpus, selected slice, funding map, durable fixture
marker, recorded aggregate, and the node-selected slot mapping. Capture the
slot mapping once from the same network configuration as the candidate run.
For `Custom`, capture queries the same configured Ogmios endpoint used by the
node and binds both its normalized endpoint identity and canonical Shelley
genesis response.

```bash
SLOT_CONFIG_ARTIFACT="$RUN/node-slot-config.json"
if [ "$NETWORK" = "Custom" ]; then
  pnpm run capture:slot-config -- \
    --network="$NETWORK" \
    --ogmios-url="$L1_OGMIOS_KEY" \
    --out="$SLOT_CONFIG_ARTIFACT"
else
  pnpm run capture:slot-config -- \
    --network="$NETWORK" \
    --out="$SLOT_CONFIG_ARTIFACT"
fi
SLOT_CONFIG_ARTIFACT_SHA256="$(sha256sum "$SLOT_CONFIG_ARTIFACT" | cut -d' ' -f1)"

prepare_candidate() {
  mode="$1"; txs="$2"; size="$3"
  dir="$RUN/$mode"
  input_dir="$dir/candidate-input-$size"
  fixture_json="$dir/fixture-create-$size.json"
  pnpm run prepare:mpf-commit-candidate-input -- \
    --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
    --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
    --runtime-version="$ARCH_G_RUNTIME_VERSION" \
    --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
    --level="$dir/fixtures/utxos-$size-level" --binary="$BINARY" \
    --fixture-creation-summary="$fixture_json" \
    --corpus="$CORPUS" --corpus-slice="$dir/root/canonical-corpus-slice.ndjson" \
    --funding-map="$dir/root/canonical-corpus-funding.json" \
    --transactions="$txs" \
    --aggregate-entry-count="$(jq -r '.utxoPayloadAggregate.entryCount' "$fixture_json")" \
    --aggregate-tuple-bytes="$(jq -r '.utxoPayloadAggregate.encodedTupleBytes' "$fixture_json")" \
    --network="$NETWORK" \
    --slot-config-artifact="$SLOT_CONFIG_ARTIFACT" \
    --slot-config-artifact-sha256="$SLOT_CONFIG_ARTIFACT_SHA256" \
    --out="$input_dir"
}

prepare_candidate 50k 50000 1000000
prepare_candidate growth 10000 100000
prepare_candidate growth 10000 300000
prepare_candidate growth 10000 1000000
```

The captured document is byte-bound by SHA-256. Mainnet, Preview, and Preprod
must equal Lucid `0.6.0`'s immutable network table. `Custom` is derived from and
binds the canonical genesis response and normalized identity of the configured
Ogmios endpoint. The production probe compares the document network with
`NodeConfig.NETWORK`; for `Custom` it re-queries that exact endpoint and
requires the same canonical genesis digest before the provider-free candidate
build begins. Candidate construction embeds the verified mapping as plain data
and does not acquire an L1 provider.

Use dedicated benchmark databases. These names satisfy the seed command's
fail-closed allow-list and never point at the live `midgard` database.

```bash
export POSTGRES_USER="${POSTGRES_USER:-postgres}"
export POSTGRES_PASSWORD="${POSTGRES_PASSWORD:-postgres}"
export POSTGRES_HOST=127.0.0.1
export POSTGRES_PORT=5433
export DB50=midgard_phase3_arch_g_50k_20260710
export DBGROWTH=midgard_phase3_arch_g_growth_20260710
docker compose exec -T postgres createdb -U "$POSTGRES_USER" "$DB50"
docker compose exec -T postgres createdb -U "$POSTGRES_USER" "$DBGROWTH"

POSTGRES_DB="$DB50" \
MPF_COMMIT_CANDIDATE_SEED_INPUT="$RUN/50k/candidate-input-1000000/seed-input.json" \
  pnpm run seed:mpf-commit-candidate
POSTGRES_DB="$DBGROWTH" \
MPF_COMMIT_CANDIDATE_SEED_INPUT="$RUN/growth/candidate-input-100000/seed-input.json" \
  pnpm run seed:mpf-commit-candidate
```

Finally run the production `commit-block-header` build path. The benchmark
configuration is asserted inside the probe. The candidate-ready hook returns a
typed invalidation before signing/submission; the gate requires zero submission
attempts and an unchanged pending-finalization journal, while the clean live E2E
below remains the binding proof for signing, submission, merge, and recovery.

```bash
export MPF_ENGINE=architecture_g
export MPF_NATIVE_OWNER_BINARY_PATH="$BINARY"
export MPF_NATIVE_OWNER_BINARY_SHA256="$(sha256sum "$BINARY" | cut -d' ' -f1)"
export MPF_SCRATCH_BUILD=fromlist
export MPF_PAYLOAD_ROOT_CHECK=off
export MPF_PARALLEL_ROOTS=true
export MPF_ROOT_WORKERS=2
export MPF_PARALLEL_ROOT_MIN_ENTRIES=1
export COMMIT_BUILD_COST_MODEL=ewma
export MEMPOOL_RETRIEVE_PAGE_SIZE=50000
export COMMIT_MAX_L2_TX_COUNT=50000
export COMMIT_MAX_LEDGER_OP_COUNT=150000
export COMMIT_MAX_TRANSITION_STEP_COUNT=50000

POSTGRES_DB="$DB50" pnpm run bench:mpf:architecture-g:commit-candidate:50k -- \
  --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
  --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
  --runtime-version="$ARCH_G_RUNTIME_VERSION" \
  --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
  --cpuset="$CPUSET" --root-gate-summary="$RUN/50k/root/summary.json" \
  --candidate-input-1000000="$RUN/50k/candidate-input-1000000/candidate-input.json" \
  --out="$RUN/50k/candidate-summary.json"

POSTGRES_DB="$DBGROWTH" pnpm run bench:mpf:architecture-g:commit-candidate:growth -- \
  --phase1-formal-binding="$PHASE1_REBOUND_BINDING" \
  --phase1-formal-binding-sha256="$PHASE1_REBOUND_BINDING_SHA256" \
  --runtime-version="$ARCH_G_RUNTIME_VERSION" \
  --runtime-executable-sha256="$ARCH_G_RUNTIME_EXECUTABLE_SHA256" \
  --cpuset="$CPUSET" --root-gate-summary="$RUN/growth/root/summary.json" \
  --candidate-input-100000="$RUN/growth/candidate-input-100000/candidate-input.json" \
  --candidate-input-300000="$RUN/growth/candidate-input-300000/candidate-input.json" \
  --candidate-input-1000000="$RUN/growth/candidate-input-1000000/candidate-input.json" \
  --out="$RUN/growth/candidate-summary.json"
```

### Formal-mode bypass review (2026-07-10)

- Formal cardinality is code-fixed at `20 x 50,000` and
  `3 x 10,000 x {100k,300k,1M}`. Reduced arguments fail before fixture or
  corpus work; only the separately labeled smoke schema permits reduction.
- Every root preparation, root gate, candidate-input build, and candidate gate
  requires the canonical absolute Phase 1 binding path plus its independently
  retained SHA-256. The code rehashes the binding, corpus, index, manifest,
  generation result, and current harness before use; a stale harness or altered
  artifact fails closed. Root and candidate evidence retain and compare the
  same deployment, node, wallet, funding, corpus, generation, and harness
  identities.
- The operator pins Node `v22.22.2` and the exact Node executable SHA-256.
  Root summaries, candidate inputs, and candidate summaries retain the runtime
  version, canonical executable path, and executable digest; missing pins or
  any root/candidate runtime drift fails before a benchmark child is accepted.
- Formal root mode cannot use the probe's synthetic workload fallback. It
  requires all six corpus inputs, verifies the corpus/index SHA and generation
  evidence, validates the complete named slice, resolves exact funding outputs,
  and decodes every selected signed transaction into its exact spend and all
  produced outputs.
- Fixture preparation is not evidence and emits no verdict. Formal gates reopen
  immutable, marker-matched fixtures in fresh CPU-pinned processes, require the
  SHA-bound fixture-creation artifact to match the actual Level path, marker,
  cardinality, and payload aggregate, and require unchanged marker, logical
  digest, and record count after every run group.
- The binding candidate gate calls the exported production
  `runCommitBlockHeaderWorkerProgram` through the production service layers. It
  does not substitute a benchmark root builder. Its only hook is the real
  candidate-ready instruction boundary, where typed invalidation prevents the
  benchmark from signing or submitting; an injected fail-closed Lucid factory
  observes any provider/signing-boundary attempt, and the gate asserts zero,
  unchanged journal rows, exact transaction count, zero full scans, production
  config, deterministic roots including the raw transaction MPF root, and
  equality with the complete-root gate.
- The pre-timing aggregate stamp models the production cache using the exact
  fixture aggregate recorded at creation; it is outside the measured path and
  SHA-bound to the fixture-creation artifact, Level path, durable marker, and
  fixture cardinality in the candidate input. Corpus, selected slice, funding map, binary,
  fixture root, built probe, source tree, Git head/diff, and scoped worktree
  status identities are all retained in evidence.
- `MPF_PAYLOAD_ROOT_CHECK=off`, `fromlist`, parallel roots, and EWMA are real
  explicit production configuration, not test-only branches. Strict default
  behavior and the sign/submit/finalize/recovery tail remain separate binding
  requirements of the clean live E2E and soak; neither performance-gate schema
  claims to satisfy them.

Final evidence must record exact commands, pass counts, Git/source/diff/binary
SHA-256 values, fixture/corpus/slice identities, full growth and 50k timing
distributions, image inspection, live transaction/header identifiers, readiness
and DB/balance assertions, restart/recovery observations, soak interval and queue
slope, reviewer findings, and the explicit default decision.
