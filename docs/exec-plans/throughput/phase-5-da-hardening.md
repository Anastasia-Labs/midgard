# ExecPlan: Throughput Phase 5 — DA Hardening (D2)

**Status:** V1 implementation, fail-closed exact-50k distribution gate, and strict historical 5,000,000-row/100-fixture provenance complete; bounded three-process closeout rerun passed; formal Docker timing distribution, fresh-live claim, 100k/one-hour, and ≤2 s p99 exit criteria remain blocked
**Effort:** weeks 10–14, parallel track
**Owner:** TBD
**Depends on:** none hard (runs parallel to Phases 2–4); interacts with Phase 4's commit cadence (the DA publish critical-path budget shrinks as cadence tightens). Parent plan: `THROUGHPUT-2500-TPS-PLAN.md` §Phase 5, bottleneck **D2**. Bottleneck **D3** (serialized merges) is explicitly OUT of scope here — assessment-only elsewhere; it appears in §8 only as an interface note.
**Exit criterion:** **Blocked under V1.** The valid operational 100k payload measures 84,506,373 decompressed bytes (over the pinned 64 MiB limit) and 2.9787x compression (just below 3x). The exact pre-submit gate correctly refuses it before journal/L1 mutation, so the required 100k one-hour soak cannot run without violating the safety contract. The valid 50k V1 payload has passed end-to-end publication through three independent committee processes, including a current 1.903 s threshold observation, but the retained single samples span 1.903–2.493 s and do not establish the ≤2 s p99 interface target. See the implementation record in §9 and the protocol proposal.

---

## 1. Current state (verified)

### 1a. The publish path end-to-end

The producer publishes a DA payload once per committed block, inline in local block finalization: `workers/utils/commit-submission.ts:256-283` builds the payload insert (`buildDaPayloadInsert`), persists it (`DaPayloadsDB.upsertAvailable`, `:260`), then **awaits** `publishDaPayloadInsertFromEnv(persistedDaPayload)` (`:273`) inside the same finalization program. A publish failure fails the whole `local_block_finalization` mutation job. This is not hypothetical: the Phase-1 benchmark log records a node stuck in a restart loop with job `local_block_finalization:cf8922…` at `status=failed, attempts=2108`, last error `DatabaseError: Failed to publish DA payload over libp2p`, blocking `/readyz` (`docs/exec-plans/phase-1-corpus-generator.md`, 2026-07-09 entries). DA publish failure is today a _liveness_ failure of the whole node, which §3 item 6 fixes.

`publishDaPayloadInsertFromEnv` (`demo/midgard-node/src/da/libp2p-producer.ts:686-724`) creates a **fresh dial-only libp2p transport per publication** and closes it in `finally` (`:705-716`) — every block pays full TCP + noise handshakes to every committee peer.

`publishDaPayloadInsert` (`libp2p-producer.ts:576-684`):

1. Rejects payloads over `manifest.maxPayloadBytes` (`:590-596`).
2. Encodes **one** `DaPayloadSubmitRequestV1` CBOR with `mode: "inline"`, `chunkManifest: null`, and the full `payloadBytes` embedded (`:608-616`). No compression anywhere.
3. Dials **all committee peers concurrently** via `Promise.all` (`:617-629`). `submitPayloadToPeer` (`:1235-1284`) never rejects — every peer attempt resolves to a status result (`transport_error` on failure), each bounded by `requestTimeoutMs` (15 s, `AbortSignal.timeout` on dial at `:950` plus `withTimeout` on the response read at `:1715-1742`).
4. Counts `acceptedPeers` = results with status in `{accepted, duplicate}` (`ACCEPTED_RESPONSE_STATUSES`, `:40`; filter at `:630-632`) and **throws `DaPayloadPublicationError` if `acceptedPeers < manifest.threshold`** (`:642-647`).
5. Only after threshold is met, signs and gossips a small `DaPayloadAnnouncementV1` (`:648-675`; capped at `maxGossipMessageBytes` = 64 KiB, `:670-674`).

**Correction:** the parent plan's D2 phrasing ("pushed inline to every committee peer") is accurate on bytes but understates what already exists: publication is **already fully concurrent across peers** (unbounded `Promise.all`) and **already has a t-of-n acceptance threshold** (`manifest.threshold`, parsed from `da_committee.threshold` at `:261-268`, with `committeePeers.length >= threshold` enforced at `:273-277`). What is missing is _early return_: `Promise.all` waits for **every** peer attempt to settle — one slow or dead peer pins publish latency at the full 15 s timeout even when threshold was reached in the first second. The real D2 changes are compression, early threshold return, backpressure-aware streaming writes, and eliminating per-peer full-payload copies — not "add concurrency" or "add a threshold".

**Correction (chunking):** "no chunking" is true of the _submit_ path but not of the protocol. The wire format has a `chunked` submit mode and a `payload-chunk` request/response protocol (`midgard-core/src/da-transport.ts:76-81, 62-71, 148-153, 203-217`), and the **retrieval** (pull) path uses chunking end-to-end: both the producer's retained-payload server (`libp2p-producer.ts:436-499`) and the committee node (`da-committee-node/src/da/libp2p/payload-protocols.ts:219-289`) answer `found_chunked` with a manifest for payloads above `maxInlineResponseBytes` (1 MiB). But the producer hardcodes inline submit (`:613-615`), and the committee's `handlePayloadSubmit` answers a chunked submit with status `deferred` without ever fetching the chunks (`payload-protocols.ts:101-110`) — chunked _push_ is scaffolding, not a working path. This plan does not build it out (§2.4).

Per-peer byte handling: `transport.request` (`libp2p-producer.ts:946-955`) calls `encodeFrame(payload, …)` **per peer**, which allocates a fresh `4 + len` Buffer and copies the whole request into it (`:1703-1713`), then hands the entire buffer to `stream.send(...)` in one call, **ignoring the boolean backpressure return** (compare `writeSingleFrame` `:1744-1754`, which does honor `onDrain`). At a 30 MiB payload and an 8-peer committee that is ≥8 full 30 MiB copies materialized simultaneously, plus the CBOR-encode copy. Responses are read by buffering the whole stream then concatenating (`readSingleFrame`, `:1715-1742`) — fine for submit responses (tiny), relevant only as a pattern note.

libp2p stack (verified `demo/midgard-node/package.json`): `libp2p 3.3.4`, `@libp2p/tcp 11.0.22`, `@chainsafe/libp2p-noise 17.0.0`, `@chainsafe/libp2p-yamux 8.0.1`, `@libp2p/gossipsub 16.0.3`; node construction at `libp2p-producer.ts:907-928` (TCP transport, noise encryption, yamux muxing). **No transport-level compression exists anywhere in this stack** — noise encrypts, yamux frames, neither compresses — so application-layer compression of the payload (before it enters the encrypted stream) is both safe and the only place compression can happen.

### 1b. Wire format as it exists

Framing: 4-byte big-endian length prefix + body, hard-capped at `maxPayloadBytes` on both ends (`encodeFrame` `libp2p-producer.ts:1703-1713`; committee-side incremental decoder `da-committee-node/src/da/libp2p/DaStreamCodec.ts:42-99`, which enforces the cap on the _declared_ length before buffering, `:64-68`).

Body: `DaPayloadSubmitRequestV1`, a **fixed 7-element canonical CBOR array** — `[deployment_fingerprint, header_hash, payload_hash, payload_schema_version, mode, payload_bytes, chunk_manifest]` (`da-transport.ts:169-177`, encoder/decoder `:761-812` with strict canonical round-trip assertion via `decodeTupleCbor` `:675-689`). Two version-relevant facts:

- The request already carries **`payloadSchemaVersion`** as a first-class field. Today it is always 2.
- `payload_bytes` is itself a versioned structure: `SDK.DaPayloadV2` (Plutus-Data CBOR, `demo/midgard-sdk/src/da-payload.ts:40-49`), whose **first field is `version` = `DA_PAYLOAD_V2_VERSION = 2n`** (`:6`).

Limits are protocol constants: `DA_TRANSPORT_LIMITS_V1` (`da-transport.ts:27-35`) — `maxPayloadBytes: 67_108_864` (64 MiB), `maxInlineResponseBytes`/`maxChunkBytes`: 1 MiB, `requestTimeoutMs: 15_000`. The producer manifest parser **pins** these: any `da_transport.limits.*` value that differs from the constant throws (`exactLimit`, `libp2p-producer.ts:1778-1796`). So the 64 MiB cap is not deploy-time tunable; changing it is a protocol-version event (protocol IDs embed `DA_TRANSPORT_PROTOCOL_VERSION`, `da-transport.ts:576-582`).

Payload contents (`workers/commit-block-header/da-payload.ts:342-452`): the payload is **not** just tx CBOR — `block_body` carries `utxos`, `withdrawals`, `forced_transactions`, `transactions`, `deposits`, `transition_trace`, `event_to_step` as sorted `(hex-key, hex-value)` entry arrays plus header and counts (`:358-419`), Plutus-Data-encoded via `SDK.encodeDaPayloadV2` (`:422`). Per-tx values are full canonical tx CBOR (member `PAYLOAD_CBOR` columns). This matters for sizing (§2.5): the parent plan's 15–30 MiB estimate (100k tx × ~300 B) covers the `transactions` array only; trace/event/utxo entries ride on top. It also matters for compression: the structure is highly repetitive (repeated CBOR framing per entry, recurring addresses/policy IDs, hex-string symmetry) — favorable zstd input.

### 1c. What consumers do with payloads they cannot parse (verified — governs rollout)

Committee-side `handlePayloadSubmit` (`da-committee-node/src/da/libp2p/payload-protocols.ts:82-165`) for an inline submit runs `checkInlineSubmitPayload` (`:355-391`): size cap (`:364`), `sha256(payload_bytes) == payload_hash` (`:367-369`), then `decodeDaPayloadV2Strict` (`:372`) and `payload.version == payloadSchemaVersion` (`:378`). Every failure returns a **structured `rejected` response with a reason code** (`payload_too_large`, `payload_hash_mismatch`, `malformed_payload`, `payload_decode_failed`, `payload_schema_version_mismatch`) — not a stream error. `decodeDaPayloadV2Strict` (`da-committee-node/src/da/payload.ts:66-93`) fails closed on undecodable bytes (`malformed_da`), non-canonical re-encode, and `wrong_version` (`:87-91`).

Consequence: **an old committee node receiving a new-format payload rejects it deterministically with `payload_decode_failed`/`malformed_payload`, and the producer counts that peer as not-accepted** (`ACCEPTED_RESPONSE_STATUSES` excludes `rejected`). A premature producer flip therefore degrades to a below-threshold `DaPayloadPublicationError` — loud, attributable per-peer, and non-corrupting. Only a malformed _request envelope_ (wrong tuple arity) escalates to a thrown `DaLibp2pPayloadProtocolError`/stream error (`payload-protocols.ts:470-476`), which is why §2.1 versions inside `payload_bytes` rather than changing the request tuple.

### 1d. Attestation and merge quorum semantics

Two thresholds exist and must not be conflated:

- **Transport-accept threshold** — `da_committee.threshold` from the deployment manifest (`libp2p-producer.ts:261-268`), used only by the producer to decide a publication succeeded and may be announced (`:642-647`).
- **On-chain attestation threshold** — `da_threshold` in the on-chain `DaParamsDatum`. The L1 attestation flow (`demo/midgard-node/src/transactions/da-attestation.ts`) inits a DA-attestation UTxO, accumulates committee signatures (`incompleteAddDaAttestationSignaturesTxProgram`, `:464-488`), and applies to the state queue only when `attestation_count >= da_threshold` (`daAttestationReachedThreshold`, `:152-154`; threshold-signed candidate selection `:490-495`). The attestation signature preimage is domain + `headerHash` **only** (`encodeDaAttestationV1Preimage`, `da-transport.ts:562-566`) — payload bytes/hash never enter the signed message, so payload re-encoding is invisible to attestation validity. (Demo-mode note: the current node signs as `OPERATOR_DA_SIGNER_INDEX = 0` only, `da-attestation.ts:30, 363-376`.)

The merge path consumes attestation as a **binary on-chain fact**: `classifyOldestQueuedBlockReadiness` skips with `skipped_oldest_block_unattested` unless the state-queue node's `da_attestation` equals the DA-attestation policy id (`merge-readiness.ts:394-405`; `requiredDaAttestation: contracts.daAttestation.policyId` at `merge-to-confirmed-state.ts:339-347`), plus the maturity buffer (`MERGE_MATURITY_DELAY_BUFFER_MS = 20_000`, `merge-readiness.ts:20`; min queue length 8, `:15`). Merge never inspects peer-ACK counts — the t-of-n enforcement it relies on lives in the on-chain validator. This is what makes §2.3's threshold-ACK early return safe.

### 1e. Node.js / zstd capability

- `demo/midgard-node/Dockerfile:3`: `FROM node:22` (floating major tag; current `node:22` images are ≥ 22.15).
- `demo/package.json:28-29`: `"engines": { "node": ">=18" }` — the only engines constraint in the workspace; `demo/midgard-node/package.json` has none.
- Native zstd (`zstdCompress`/`zstdCompressSync`/`zstdDecompress` + `maxOutputLength` option) landed in `node:zlib` in v23.8.0 and was backported to **v22.15.0**. The deployed runtime (`node:22` image) therefore has it; the loose `>=18` engines field does not guarantee it for bare-metal runs.

**Decision consequence (detailed in §2.2):** use native `node:zlib` zstd, raise `engines` to `>=22.15.0` in `demo/midgard-node/package.json`, and add a startup capability assertion. No new native dependency.

---

## 2. Architecture decisions

### 2.1 Versioned payload envelope, versioned inside `payload_bytes`

**Decision:** Introduce **`DaPayloadEnvelopeV3`**, a canonical-CBOR envelope that wraps the _unchanged_ `DaPayloadV2` bytes, signalled by `payloadSchemaVersion = 3` in the existing submit request field and by `VERSION = 3` in `DaPayloadsDB`. The request tuple (`DaPayloadSubmitRequestV1`, 7 elements) does **not** change shape.

Byte layout (encoded with the existing `midgard-core` canonical CBOR codec, same style as every `da-transport.ts` message):

```
DaPayloadEnvelopeV3 = [
  version           : uint   = 3,
  content_encoding  : uint     (0 = identity, 1 = zstd),
  inner_bytes       : uint     (exact decoded length of the inner DaPayloadV2 CBOR),
  inner_sha256      : bytes32  (sha256 of the inner DaPayloadV2 CBOR),
  body              : bytes    (zstd frame when content_encoding=1, raw inner bytes when 0)
]
```

- `payload_hash` (submit request field, announcement, `DaPayloadsDB.PAYLOAD_SHA256`, conflict detection) = **sha256 of the envelope bytes as stored/transmitted**. This keeps every existing byte-identity check working unmodified: producer self-check (`verifyPayloadHash`, `libp2p-producer.ts:1569-1580`), retained-row check (`:1348-1372`), committee inline check (`payload-protocols.ts:367-369`), chunk manifests (chunks are slices of the envelope). Determinism across zstd versions is a non-issue because **only the producer ever compresses**; everyone else verifies the bytes they received. The _content_ identity (`inner_sha256`) is carried inside and re-verified after decompression.
- The inner payload stays `DaPayloadV2` with `version = 2` untouched — root computation, fault-proof semantics, and `decodeDaPayloadV2Strict` operate on the decompressed inner bytes exactly as today. Consumers parsing an envelope check `payloadSchemaVersion === 3` at the request layer and `inner.version === 2` after unwrap (adjusting the equality check at `payload-protocols.ts:378`, which today compares inner version to the request field directly).

**Rationale / rejected alternatives:**

- _Changing the `DaPayloadSubmitRequestV1` tuple (add encoding fields):_ rejected — old decoders enforce exact arity (`fixedArray(value, 7, …)`, `da-transport.ts:779`) and would fail with a **thrown stream error** rather than a structured `rejected` response (§1c), turning a rollout mistake into opaque `transport_error`s instead of attributable reason codes.
- _Bumping `DA_TRANSPORT_PROTOCOL_VERSION`:_ rejected — protocol IDs and gossip topics embed the version (`da-transport.ts:568-582`), so a bump forks every protocol and topic simultaneously and requires a committee flag-day; old and new peers cannot even exchange rejections. The envelope gives per-payload coexistence on the existing protocol.
- _Compressing at the frame layer (transparent to `payload_hash`):_ rejected — `payload_hash` would then bind to bytes nobody can recompute without agreeing on the compressor, and the retained-payload/chunk retrieval path (which serves stored bytes and hashes slices of them) would need parallel logic. Envelope-in-payload keeps one artifact, one hash.
- _Storing uncompressed in `DaPayloadsDB` and compressing per-publish:_ rejected — stored `PAYLOAD_SHA256` would diverge from the wire `payload_hash`, breaking `verifyPayloadHash` and making announcements/conflict evidence refer to bytes not in the DB. Instead the envelope **is** the stored artifact (compress-at-rest; also shrinks the `da_payloads` table ~3–6×). All in-node readers of `DaPayloadsDB.PAYLOAD_CBOR` go through one new unwrap helper (§3 item 2).

**Rollout (decoder-first, verified-safe by §1c):**

1. Ship envelope _decoding_ everywhere first — committee node (`payload-protocols.ts`, watcher verification), producer retained-payload server, node-internal readers. Decoders accept `VERSION 2` (raw `DaPayloadV2`) and `VERSION 3` (envelope, either encoding) from day one.
2. Producer gains a feature flag (`MIDGARD_DA_PAYLOAD_ENVELOPE=off|identity|zstd`, default `off`). `identity` exercises the envelope path without compression (canary).
3. After the whole committee runs decoder-capable builds (observable: preflight already reports per-peer reachability, `libp2p-producer.ts:733-875`; add an envelope-capability probe via a tiny `identity` test submit or a version tag in the runtime manifest), flip the flag to `zstd`. A premature flip fails loudly as below-threshold publication with per-peer `payload_decode_failed` reason codes — no silent corruption path exists (§1c).

### 2.2 zstd via native `node:zlib`, level 3

**Decision:** `content_encoding = 1` is zstd, produced by `node:zlib` (`zstdCompress`, async, off the event loop in the libuv pool) at **level 3** (configurable). Raise `demo/midgard-node/package.json` `engines` to `">=22.15.0"` and assert `typeof zlib.zstdCompressSync === "function"` at startup when the flag enables zstd (§3 item 7).

**Level arithmetic (estimates, to be validated in §6):** zstd-3 compresses ~150–300 MB/s per core on server-class x86; a 30 MiB worst-case payload costs ~0.1–0.25 s once per block against a ~20 s cadence (<1.5% of budget). Decompression is ~500+ MB/s → ~60 ms per committee member. Level 9+ drops to ~50 MB/s (~0.6 s+) for typically ≤10–15% additional ratio on this kind of input — not worth it on the commit path. Level 1 saves ~0.1 s but costs ~10–20% ratio, i.e. megabytes per block per peer of egress. Level 3 is the standard knee of the curve; expose it as config for tuning against measured corpus payloads.

**Rejected alternatives:**

- _gzip (`node:zlib` deflate):_ ~3–5× slower to compress at comparable ratios (~30–60 MB/s at level 6 ⇒ 0.5–1 s per block) and decompresses far slower than zstd; no `maxOutputLength`-equivalent advantage since zlib also supports it — speed is the differentiator at 15–30 MiB.
- _brotli:_ excellent ratios at quality ≥ 9 but compression throughput is an order of magnitude too slow for a per-block hot path; low-quality brotli loses to zstd-3 on both axes for binary CBOR.
- _`@mongodb-js/zstd` / `zstd-napi`:_ prebuilt native deps add supply-chain surface, platform build matrix, and a second copy of libzstd — justified only if the deployed Node were <22.15, which it is not (Dockerfile `node:22`, §1e). Fallback plan if a deployment surfaces on older Node: gate on the startup capability check and fail with a message naming `@mongodb-js/zstd` as the sanctioned escape hatch; do not add it preemptively.

### 2.3 The 64 MiB cap binds both compressed and decompressed bytes; explicit decompression guard

**Decision:** `maxPayloadBytes` (64 MiB, pinned — §1b) is enforced on **(a)** the envelope (transmitted/stored) bytes, exactly where it is enforced today (`libp2p-producer.ts:590-596`, frame codecs, `payload-protocols.ts:364`), **and (b)** the declared and actual decompressed size. Consumer decode procedure, in order, before any decompression work:

1. Envelope bytes ≤ `maxPayloadBytes` (existing checks, unchanged).
2. Decode envelope CBOR; require `version = 3`, known `content_encoding`, `inner_bytes ∈ (0, maxPayloadBytes]`. Reject (`reasonCode: "declared_inner_too_large"` / `"unknown_content_encoding"`) otherwise — **before touching the body**.
3. Decompress with `zlib.zstdDecompress(body, { maxOutputLength: inner_bytes })` — the kernel of the zip-bomb guard: output is hard-capped at the _declared_ size, which was itself capped at 64 MiB; an over-expanding frame errors out instead of allocating. (Node's one-shot zlib methods honor `maxOutputLength`; this is the reason to require ≥22.15 rather than piping through a hand-rolled streaming loop, though the streaming variant with an output counter is the documented fallback if profiling ever demands it.)
4. Require `decompressed.length === inner_bytes` and `sha256(decompressed) === inner_sha256` (`reasonCode: "inner_length_mismatch"` / `"inner_hash_mismatch"`).
5. Hand the inner bytes to the existing `decodeDaPayloadV2Strict` pipeline unchanged.

Worst-case adversarial cost to a committee member per malicious submit: one ≤64 MiB allocation and one bounded decompress — the same order as today's worst-case legitimate inline submit, and rejected submits already exist as a handled path. **Rejected alternative** — enforcing the cap on compressed size only: a 64 MiB zstd frame can expand to gigabytes; committee members are exactly the nodes an adversarial operator would want to OOM before they can attest. Enforcing on decompressed only (letting compressed exceed) is moot: compressed > uncompressed payloads are pathological and the frame codec caps transmitted bytes anyway.

### 2.4 Publish: shared immutable frame, bounded concurrency, backpressured chunked writes; no chunked-submit protocol work

**Decision:** Keep single-frame inline submit as the wire mechanism (compressed 100k-tx payloads are ~5–10 MiB — comfortably inline), and fix the three real defects in the send path:

1. **Encode once, share across peers.** Hoist request-CBOR _and_ frame encoding out of `transport.request` (today `encodeFrame` copies per peer, `libp2p-producer.ts:952, 1703-1713`); pass one frozen `Buffer` to all peer writes. Memory high-water drops from `O(peers × payload)` to `O(payload)`.
2. **Backpressure-aware chunked writes.** Write the shared frame in `maxChunkBytes` (1 MiB) slices, honoring `stream.send()`'s boolean and awaiting `onDrain` when false — the discipline `writeSingleFrame` (`:1744-1754`) and the committee's `writeDaStreamFrame` (`DaStreamCodec.ts:101-113`) already follow and `transport.request` (`:952`) skips. `Buffer.subarray` slices are zero-copy. The receiver needs no change: the committee's `decodeDaStreamFrames` is already an incremental accumulator indifferent to chunk boundaries (`DaStreamCodec.ts:42-99`).
3. **Bounded peer concurrency.** Replace unbounded `Promise.all` fan-out with a concurrency limiter (default `min(committeeSize, 8)`, config §4) so a large committee doesn't multiply simultaneous 5–10 MiB in-flight streams beyond NIC/muxer comfort. For today's small committees the default equals current behavior.

**Rejected alternatives:** _(a) implement chunked-mode submit end-to-end_ (producer sends manifest, committee pulls chunks): the committee's `deferred` stub (§1a) means building a whole pull-orchestration state machine on the committee side; post-compression payload sizes don't justify it, and the pull path (`payload-by-header` + `payload-chunk`) already covers the "payload too big to push inline" contingency. Revisit only if measured envelopes approach `maxPayloadBytes`. _(b) per-peer re-framing kept as-is with compression only:_ leaves the O(peers × payload) copy amplification and the backpressure bug in place — cheap to fix while touching the function.

Additionally, stop paying per-block dial setup: hold the dial-only transport open across publications (lazily created, reused, re-dialed on failure) instead of create/close per block (`:705-716`). libp2p 3.x maintains the connection pool; `dialProtocol` on an existing connection is a yamux stream open, not a handshake.

### 2.5 Threshold-ACK return with background stragglers and a reconciler

**Decision:** `publishDaPayloadInsert` resolves as soon as `acceptedPeers >= manifest.threshold`, not when all peers settle. Remaining in-flight attempts continue detached ("stragglers"); their results feed metrics and the report asynchronously. Peers that ultimately fail are retried by a background **publication reconciler** (§3 item 6) until full-committee replication or retention expiry.

**Why this is safe (verified, §1d):** the merge gate consumes only the applied on-chain attestation (`merge-readiness.ts:397`), whose own threshold (`da_threshold`) is enforced by the validator when signatures are applied (`da-attestation.ts:152-154, 490-495`). Producer-side peer-ACK counting gates nothing except the announcement. Waiting for peers beyond the transport threshold adds latency with zero safety: acceptance by peer t+1 has no on-chain effect, and any committee member that missed the push can pull the payload via the existing retrieval protocols (`payload-source.ts` pull path, §1a) before signing. One coupling must be made explicit rather than assumed: **a committee member only signs after verifying the payload, so the transport threshold must be ≥ the on-chain `da_threshold`** — otherwise the producer could declare success while too few members hold the payload to ever reach on-chain quorum through push alone (pull would eventually rescue it, but the cadence budget shouldn't depend on that). §3 item 7 adds a startup assertion `manifest.threshold >= daParams.da_threshold`. If a deployment sets on-chain quorum _higher_ than the manifest threshold, the assertion fails closed at startup — the spec is "transport threshold matches or exceeds on-chain quorum", not a blind t.

Timeout/retry policy: first attempt per peer keeps the pinned 15 s `requestTimeoutMs`. Stragglers get no extension — they run out their existing attempt. Reconciler: scan every 30 s for payloads within retention whose per-peer acceptance set is incomplete; re-submit to missing peers with exponential backoff per (payload, peer) — 30 s base, ×2, cap 5 min — stopping on `accepted`/`duplicate`, on `conflict` (surfaced as an alert; conflict means divergent payload bytes for a header, which is evidence-grade), or at retention expiry. Reconciler state derives from `DaPayloadsDB` plus a small per-peer acceptance table (§3 item 6) so it survives restarts — precisely the crash-consistency the current inline-await approach lacks (§1a incident).

---

## 3. Implementation items (ordered)

1. **`midgard-core`: envelope codec.** New `demo/midgard-core/src/da-payload-envelope.ts`: `encodeDaPayloadEnvelopeV3`/`decodeDaPayloadEnvelopeV3` (canonical CBOR, round-trip-asserted like every codec in `da-transport.ts`), `DA_PAYLOAD_ENVELOPE_V3_VERSION = 3`, `DaPayloadContentEncoding = { identity: 0, zstd: 1 }`, and `unwrapDaPayload(bytes, { maxPayloadBytes }): { schemaVersion: 2|3, innerBytes: Buffer }` implementing the §2.3 guard sequence (sniff: raw `DaPayloadV2` if leading structure matches v2, else envelope; explicit, not heuristic — dispatch on the caller-supplied schema version where available). zstd calls live behind a tiny `da-compression.ts` (async `compress(bytes, level)` / `decompress(bytes, maxOutputLength)`) so the capability check and any future backend swap have one seam. Export from `midgard-core` package exports.

2. **Consumers decode-first (deployable independently, no behavior change for v2):**

   - Committee: `payload-protocols.ts` `checkInlineSubmitPayload` (`:355-391`) branches on `request.payloadSchemaVersion`: `2` → existing path; `3` → §2.3 guard, then existing strict-decode pipeline on inner bytes; version equality check becomes "envelope ⇒ inner.version === 2". `resolveStoredPayload` (`:409-441`) and the watcher's verification path unwrap before root recomputation; stored records keep envelope bytes + schema version.
   - Producer/retained server & in-node readers: audit the whole-payload readers of `DaPayloadsDB.PAYLOAD_CBOR` (verified list: `database/daPayloads.ts`, `workers/commit-block-header/da-payload-backfill.ts`, `workers/utils/commit-submission.ts`, `da/libp2p-producer.ts`, `e2e/da-gates.ts` area) and route content access through `unwrapDaPayload`; byte-identity paths (hash checks, chunk manifests, retained-server responses) stay on raw stored bytes by design (§2.1).

3. **Producer envelope + flag.** `buildDaPayloadInsert` (`workers/commit-block-header/da-payload.ts:342-452`): after `SDK.encodeDaPayloadV2(payload)` (`:422`), when `MIDGARD_DA_PAYLOAD_ENVELOPE != off`, wrap per §2.1 (async zstd at configured level), set `VERSION` column to 3 and `PAYLOAD_SHA256` to the envelope hash (`:426-431`). `publishDaPayloadInsert` passes `insert[VERSION]` through as `payloadSchemaVersion` — it already does (`libp2p-producer.ts:612`); no wire-code change for versioning.

4. **Send-path mechanics** (`libp2p-producer.ts`): hoist `encodeFrame` out of `transport.request` (encode once at `:608-616` alongside the request CBOR); replace the single `stream.send(frame)` (`:952`) with a 1 MiB-sliced, `onDrain`-honoring write loop; add the bounded-concurrency mapper over `manifest.committeePeers` (`:617-629`); make the dial-only transport in `publishDaPayloadInsertFromEnv` (`:705-716`) a memoized long-lived instance with failure-triggered rebuild.

5. **Threshold-ACK early return** (`libp2p-producer.ts:617-647`): race peer results, resolve when accepted count reaches `manifest.threshold`; move announcement signing/publish (`:648-675`) to fire at that moment; detached stragglers update a mutable report and metrics on settle. The `DaPayloadPublicationError` below-threshold path now fires only when _all_ attempts have settled below threshold.

6. **Publication reconciler + decoupling from finalization.** New peer-delivery and gossip-announcement outboxes are seeded after the payload commit but before `local_block_finalization` is marked complete. The worker performs no committee network I/O and returns the finalized header hash. Both the legacy and speculative parent submit paths release the outer L1-control-plane permit before attempting the best-effort threshold publish; failure is logged and the durable reconciler owns retry. This directly retires the `attempts=2108` readiness-blocking failure mode (§1a) without moving a 15 s dead-peer wait into the serialized L1 control plane. A merge still cannot outrun a failed publication — the on-chain attestation gate (§1d) holds it — so no safety is lost, only false liveness coupling.

7. **Startup checks.** When envelope/zstd enabled: assert `zlib.zstdCompressSync` exists (Node ≥22.15); compress/decompress round-trip self-test; assert `manifest.threshold >= da_threshold` from the DA params datum (§2.5) — read via the existing `fetchDaParamsUtxo` machinery (`da-attestation.ts:96-135`); bump `demo/midgard-node/package.json` engines to `>=22.15.0`.

8. **Rollout execution:** deploy items 1–2 to committee + producers everywhere (inert); canary `identity` on one deployment; flip `zstd` after committee capability confirmed; keep `off` as instant rollback for the producer side indefinitely (decoders stay v2+v3 forever — v2 remains the format of all historical retained payloads).

---

## 4. Config surface

| Var                                               | Default             | Meaning                                                                                                                                                                  |
| ------------------------------------------------- | ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `MIDGARD_DA_PAYLOAD_ENVELOPE`                     | `off`               | `off` = emit raw `DaPayloadV2` (today); `identity` = envelope, no compression (canary); `zstd` = envelope + zstd. Producer-side only; decoders always accept everything. |
| `MIDGARD_DA_ZSTD_LEVEL`                           | `3`                 | zstd level, 1–19 accepted, §2.2 arithmetic justifies 3.                                                                                                                  |
| `MIDGARD_DA_PUBLISH_CONCURRENCY`                  | `min(committee, 8)` | Bounded per-peer publish fan-out (§2.4).                                                                                                                                 |
| `MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS`        | `30000`             | Reconciler scan interval.                                                                                                                                                |
| `MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS` / `_MAX_MS` | `30000` / `300000`  | Per-(payload,peer) exponential backoff (§2.5).                                                                                                                           |

Not configurable, deliberately: `maxPayloadBytes`, `maxChunkBytes`, `requestTimeoutMs` — pinned protocol constants (`exactLimit`, `libp2p-producer.ts:1778-1796`); the ACK threshold — it comes from the deployment manifest (`da_committee.threshold`) and is cross-checked against on-chain `da_threshold` at startup, not free-floating env config.

## 5. Observability

All emitted from the producer publish path and reconciler (naming follows the existing `*_duration` timer style, cf. `tx-queue-processor.ts:131-139`):

- `da_payload_bytes_uncompressed` / `da_payload_bytes_envelope` (gauge per block) and derived `da_payload_compression_ratio` — the ≥3× exit criterion reads from this.
- `da_payload_compress_duration_ms` — guards the §2.2 CPU estimate with real numbers.
- `da_publish_peer_duration_ms{peer_id,status}` — per-peer submit latency and outcome (incl. straggler completions).
- `da_publish_threshold_duration_ms` vs `da_publish_all_peer_duration_ms` — the direct before/after measure of §2.5; the gap is the latency the early return reclaims.
- `da_publish_straggler_total{status}` — stragglers by eventual outcome; sustained `transport_error` growth is the "dead committee peer" alarm.
- `da_publish_reconciler_attempts_total` / `da_publish_reconciler_backlog` — reconciler pressure; nonzero steady-state backlog within retention is an operator page.
- `da_publish_rejected_total{reason_code}` — per-reason rejects; `payload_decode_failed` spiking during rollout = a committee member missing the decoder (the §2.1 misflip signature).

## 6. Test & verification plan

1. **Envelope codec unit tests** (`midgard-core`): round-trip, canonical-encoding assertion, every §2.3 rejection (`declared_inner_too_large`, `unknown_content_encoding`, `inner_length_mismatch`, `inner_hash_mismatch`), identity and zstd encodings.
2. **Zip-bomb guard test:** a crafted zstd frame expanding past its declared `inner_bytes` must error from `maxOutputLength` without full allocation; a frame declaring `inner_bytes > maxPayloadBytes` must reject before any decompress call (assert via spy that the zstd binding is never invoked).
3. **Mixed-version committee test** (extends the docker-compose DA fixtures, `docker-compose.da-libp2p.yaml`): producer flipped to `zstd` against a committee of one old-build and n−1 new-build members. Assert: old member returns structured `rejected/payload_decode_failed` (not a stream error); publish still succeeds iff new members ≥ threshold; metrics show the reject reason. Inverse: decoder-upgraded committee with producer `off` — pure v2 traffic unaffected.
4. **Compression-ratio assertion on real corpus data:** build a `DaPayloadV2` from Phase-1 corpus transactions (`stress-corpus-generate` output; `OpenLoopCorpusRow.canonicalCborHex/ByteLength` fields per `docs/exec-plans/phase-1-corpus-generator.md`) at 10k/50k/100k tx scales, including proportional utxo/trace/event entries; assert envelope ≤ uncompressed/3 (exit criterion) and record actual ratio. **Note:** the parent plan's ~300 B/tx figure and 3–6× ratio are both estimates (parent plan §3 target math); this test replaces them with measurements — if 3× is not met on real data, the exit criterion is renegotiated with numbers, not assumed.
5. **Headroom validation:** same synthetic-max harness asserts the **full** 100k-tx payload — transactions _plus_ `utxos`, `transition_trace` (up to 400k steps per parent-plan Phase 3 targets), `event_to_step` — fits `maxPayloadBytes` uncompressed (the protocol cap binds decompressed size, §2.3). **This is a genuine open risk, not a formality** (§7): the 15–30 MiB estimate excludes trace entries. If the measured worst case exceeds 64 MiB, that is a Phase-5 exit blocker escalated as a `DA_TRANSPORT_PROTOCOL_VERSION` bump proposal (limits are pinned, §1b) — decided on measurement, in this phase, not discovered in production.
6. **Threshold-ACK behavior tests:** fake transport with one peer delayed 14 s — publish must return at threshold in ms, straggler recorded on settle; all-below-threshold still raises `DaPayloadPublicationError`; announcement fires exactly once at threshold.
7. **Reconciler tests:** kill a committee member during soak; assert publication row goes incomplete, reconciler retries on schedule, converges after member restart, and `local_block_finalization` never fails or blocks `/readyz` (regression test for the §1a incident).
8. **100k-tx soak** (with Phases 3–4 in place, or the payload-build harness standalone otherwise): sustained block cadence with full-size payloads for ≥1 h; assert `da_publish_threshold_duration_ms` p99 within the cadence budget (§8) and zero reconciler backlog growth. **Unmet under V1:** the exact pre-submit gate rejects the measured 100k payload before mutation, and no one-hour Phase 5 soak harness/result exists in this checkout.

## 7. Risks & rollback

- **Mixed-committee incompatibility.** Failure mode is verified-loud (§1c): below-threshold publication with per-peer reason codes, never silent divergence. Flipping `MIDGARD_DA_PAYLOAD_ENVELOPE=off` stops future V3 production only; it is not sufficient rollback while an unattested or incompletely replicated V3 payload remains in the durable outbox. Operators must keep a V3-capable quorum available until those rows converge (or explicitly abandon the associated header through the protocol recovery path). Decoders remain V2+V3 permanently.
- **Uncompressed 100k-tx payload may exceed the pinned 64 MiB cap** once trace/event entries are counted (§6 item 5). Compression does not help — the cap binds decompressed size by design (§2.3). Contingency is a protocol-version bump for `DA_TRANSPORT_LIMITS_V1`; the decision gate is the §6.5 measurement, scheduled early in the phase.
- **CPU on the commit path.** zstd-3 at ~150–300 MB/s/core (estimate) ⇒ ~0.1–0.25 s per 30 MiB block; async `zlib` keeps it off the event loop but it still occupies a libuv thread and a core. If §6.8 measurement shows contention with Phase-2/3 worker pools, escape hatches in order: lower level to 1, dedicate a `worker_threads` compressor, or compress during the L1-confirmation wait (Phase-4 overlap makes that window free). Committee-side decompress (~60 ms) is noise.
- **Threshold-ACK hides sick peers.** Publish "success" no longer implies full replication; a chronically failing member is now visible only via metrics (`da_publish_straggler_total`, reconciler backlog) instead of blocking the producer. That is the intended trade, but it demands the §5 alerts actually be wired to paging before the flip — otherwise replication quietly degrades to exactly threshold.
- **Native-dep supply chain: none added.** The zstd path is Node core (§2.2). The risk transfers to the runtime floor: bare-metal deployments on Node <22.15 fail the startup assertion; the documented (not shipped) fallback is `@mongodb-js/zstd`, accepted only with pinned version + provenance checks if that deployment class materializes.
- **Long-lived producer transport** (§2.4) changes failure surface from per-block dial errors to connection-pool staleness; mitigated by failure-triggered rebuild and the fact that preflight (`runDaLibp2pPreflightFromEnv`) already exercises dial health independently.

## 8. Interface contracts

- **D3 / merge path (out of scope, shared semantics):** the merge gate stays exactly `node.da_attestation == daAttestation.policyId` + maturity (`merge-readiness.ts:394-422`, `merge-to-confirmed-state.ts:339-347`). This plan changes _when the producer's publish call returns_, never what merge requires: on-chain quorum remains `attestation_count >= da_threshold` enforced by the validator (`da-attestation.ts:152-154`). The one cross-plan invariant introduced here — **transport threshold ≥ on-chain `da_threshold`**, asserted at startup (§3 item 7) — must be preserved by any D3 work that touches committee or threshold configuration. Any future D3 merge-batching consumes attestation state identically; nothing here needs re-review for it.
- **Phase 4 cadence budget:** Phase 4 targets effective cadence ≈ L1 block time (~20 s) with block N+1 build overlapping block N confirmation. This plan's contribution to the critical path is: compress (~0.1–0.25 s, estimate §2.2) + threshold-ACK publish (wire time for ~5–10 MiB to the fastest t peers; sub-second LAN, low seconds WAN) — budgeted at **≤2 s p99 total**, asserted in §6.8. The old worst case (15 s single-straggler timeout pinning `Promise.all`) is exactly what threshold-ACK removes; without Phase 5, Phase 4's tightened cadence would have inherited it. The retained exact-50k samples span 1.903–2.493 s and all clear the ~20 s cadence, but they are individual observations rather than a p99 distribution; the ≤2 s criterion remains unmet.
- **Wire/storage contract for all other consumers:** `payload_hash` and `DaPayloadsDB.PAYLOAD_SHA256` = sha256 of stored/transmitted bytes (envelope when v3), `payloadSchemaVersion`/`VERSION` ∈ {2, 3}, inner content always `DaPayloadV2` version 2, content access exclusively via `unwrapDaPayload`. Chunk manifests and retained-payload retrieval operate on stored bytes and are format-agnostic by construction.

## 9. Implementation and verification record (2026-07-10)

### Implemented

- Added canonical V3 identity/zstd envelopes with stored-byte and inner-byte hashes, declared/actual decompressed limits, `maxOutputLength`, structured rejection codes, and dual V2/V3 decoding.
- Raised the native-zstd runtime floor to Node 22.15.0 for the committee package and to Node 22.16.0 for the workspace and Midgard node, whose validation workers require `Worker.getHeapStatistics()`. Pinned `.nvmrc`, CI/nightly, Docker, and formal benchmark evidence to Node 22.22.2 so the immutable zstd fixture and worker-runtime behavior are reproducible.
- Replaced whole-payload Lucid hex encode/decode with byte-oriented, byte-identical `DaPayloadV2` codecs. Differential coverage includes 100 varied payloads and Plutus byte boundaries 0/1/23/24/63/64/65/127/128/129. The old 10k encoder took 3.138 s; the byte encoder took 0.223 s on the final canonical operational harness. The uncheckpointed legacy 10k/50k/100k run was censored at 30m14s with 3.93 GiB peak RSS and no output.
- Added exact encoded-size APIs and the Phase 3 durable post-state UTxO aggregate. Both commit paths enforce the pinned inner cap before aggregate stamp, journal preparation, signing, or submit. The deep-state regression proves rejection discards the overlay and leaves no journal/L1 mutation.
- Moved full-state delta materialization and DA encoding before the local-finalization SQL transaction; only the finished payload upsert is atomic with finalization. Build and SQL durations are separate metrics. Conflict rollback and idempotent retry are covered.
- Added threshold-early-return publication, shared zero-copy/backpressured frame chunks, bounded fan-out, stable threshold/all-peer snapshots, safe straggler callbacks, long-lived transport reuse/rebuild, per-peer results, durable publication outbox/reconciler, restart seeding, leases, exponential retry, and monotone conflict evidence.
- Decoupled publication failure from local-finalization liveness while preserving the on-chain attestation merge gate. Startup checks enforce transport threshold at least the on-chain DA threshold.
- Added a no-HTTP/URL DA transport scanner over 19 committee, producer, and core transport target files. The committee and producer keep the libp2p-only transport boundary.
- Added [the protocol-version proposal](./phase-5-da-transport-version-proposal.md). V1 remains pinned and envelope production remains `off` by default pending policy approval.

### Corpus measurements

All rows use real Phase 1 canonical transaction CBOR and exact SDK encoding. Operational rows contain canonical, committee-valid transition/event members.

| Scenario           |  Txs |         Inner |     Envelope |   Ratio |   Encode |     zstd | Result                      |
| ------------------ | ---: | ------------: | -----------: | ------: | -------: | -------: | --------------------------- |
| Operational        |  10k |   8,429,577 B |  1,968,034 B | 4.2832x |   223 ms |   206 ms | V1 pass                     |
| Operational        |  50k |  42,149,577 B | 13,707,581 B | 3.0749x |   653 ms | 1,000 ms | V1 pass                     |
| Operational        | 100k |  84,506,373 B | 28,370,604 B | 2.9787x | 1,832 ms | 2,413 ms | **Cap and ratio blocked**   |
| Max-envelope model |  50k |  67,200,421 B | 15,413,415 B | 4.3599x | 1,021 ms |   612 ms | **Cap blocked by 91,557 B** |
| Max-envelope model | 100k | 134,400,429 B | 31,820,813 B | 4.2237x | 1,986 ms | 1,120 ms | **Cap blocked**             |

**Current operational artifact remeasurement (2026-07-13).** The checked Phase-1
corpus snapshot at
`logs/throughput-resume-20260712T154400Z/phase1-benchmark/corpus-live-4096/corpus.ndjson`
was rerun with the exact byte encoder, `scales=50000`, and
`traceStepsPerTx=1`. The measurement consumes the first 50,000 selected chain
rows, each with 379-byte canonical transaction CBOR (18,950,000 transaction
bytes). The result is deterministic and is the artifact bound by the
separate-process gate:

- inner bytes: **41,949,577 B**, SHA-256
  `0cad493355048c36b85c9d9998863c47b5fe8c012b4de1ae88dd91f7587603d0`;
- zstd V3 envelope: **13,681,302 B**, SHA-256
  `d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6`;
- corpus prefix SHA-256 (the first 50,000 rows):
  `4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e`;
- header hash: `8ffd0001ced7f02bc858def1b3bd6f254a90e1ae908529985e7d7d99`;
- compression ratio: **3.0661977201x**; both inner and envelope fit the pinned
  64 MiB V1 limit.

The historical 13,707,581-byte row above used a prior corpus whose rows were
4 bytes larger (the exact 200,000-byte inner delta). That prior artifact is not
present in this checkout and is not substituted silently; the gate requires a
complete operational report, corpus prefix binding, canonical model/counts, and
minimum operational sizes (rejecting the ~400 KB structural fixture).

### Strict offline historical corpus and fixture suite (2026-07-14)

The Phase 5-only historical extension and its exact fixture suite are retained
under
`demo/midgard-node/logs/phase5-historical-corpus-20260714`. The extension
preserves the exact 3,063,808-row Phase 1 byte prefix and adds 1,936,192
terminal-derived continuation rows, for **5,000,000 rows across 4,096 chains**.
Its index contains **8,192 runs**: one retained base run and one continuation
run for every chain. The strict verifier independently checked the retained
terminal identities, fanout wallet/address/payment-key ownership, first-funding
roots, native transaction IDs, signatures, exact one-input/two-output transfer
semantics, fee formula, value conservation, chain links, and NDJSON index byte
boundaries.

| Artifact                                   | SHA-256                                                            |
| ------------------------------------------ | ------------------------------------------------------------------ |
| `historical-corpus.ndjson`                 | `d8282fed16e1fbf2f9d6a7a1ca4e302d1e8d6537b4f7a7bd0b05bdbfe1292f25` |
| `historical-corpus.ndjson.index.ndjson`    | `1808ac8a8e004bfee0969ff3d5c119cf6cb56e2b82d1c3c7b14ab40921f8884f` |
| `historical-corpus.ndjson.manifest.json`   | `88548d399232e1f314e359142a4981f7a88504248d4996d1c8497e5dc1a50c13` |
| `historical-corpus.ndjson.verify.json`     | `df874b0eb750d8561635abfc108040ae4e8f41a93b3866941925898a9d89a875` |
| `historical-corpus-binding.json`           | `721eb3a5dc1243f1a75db5bcd04fb033672664a3f4496c112cab6cafa45706a1` |
| `historical-corpus-generation-result.json` | `fa8dbf0410e97b6ee9b2776fbb55827df80d267d053984d99fc9770242e99950` |
| fixture-suite `manifest.json`              | `4c073f9fa64042b670fe6ace483d1254e13d999f77eb311129b3f84035081e96` |

The suite contains **100 disjoint windows of 50,000 transactions**. It has 100
unique header hashes, envelopes, inner payloads, key-plus-CBOR transaction-set
hashes, and key-independent canonical-CBOR content hashes. Entry zero remains
the checked operational anchor. This closes the offline cardinality and corpus
provenance prerequisite for the formal distribution runner.

The final Node `v22.22.2` consumer-side
`loadPhase5DaDistributionEvidence` gate also passed against the published
suite. It repeated the full historical provenance verification, rescanned all
5,000,000 source rows and 100 disjoint windows, unwrapped all 100 envelopes,
and reproduced the corpus, binding, source-manifest, generation-result,
fixture-manifest, and anchor identities above.

This evidence is deliberately marked
`claimScope: historical-offline-corpus-extension`, `freshLiveClaim: false`,
`phase1FormalBindingCompatible: false`, and
`phase2ValidationCorpusCompatible: false`. It is valid only for the Phase 5 DA
distribution fixture. It is not a fresh-live Phase 1 or Phase 2 benchmark, and
it contains no publication timings. The Docker distribution run has not run,
so the ≤2 s p99 criterion remains **NO-GO**. The 100k payload still exceeds the
pinned 64 MiB decompressed limit and its one-hour soak remains blocked.

### Live path evidence

- Separate-process 3-peer libp2p, valid 50k operational envelope, threshold 2, default concurrency: threshold 2,247.40 ms; all peers 2,273.17 ms; 3/3 accepted. Committee handler wall times were 2,068.78/2,069.84/2,095.14 ms and peak RSS was 841,252,864/855,519,232/859,463,680 B. This clears the ~20 s block cadence budget but is 247 ms above §8's aspirational 2 s p99 interface target; it is not described as a 2 s pass.
- Real 3-peer degraded/recovery path: threshold returns while a third peer is delayed to the pinned timeout; the straggler is recorded; restart converges to `duplicate` and readiness recovers.
- The same real test emulates one old committee member returning structured `rejected/payload_decode_failed`: V3 succeeds exactly when the two upgraded peers meet threshold. The inverse raw-V2 publication is accepted by all peers.
- Real Postgres: payload committed without outbox rows is reseeded after restart; competing claims are excluded; released work resumes; late failure cannot downgrade success; conflict evidence is sticky.
- Depth-three pending delta chains materialize the complete post-state and round-trip through both raw V2 and zstd V3 payloads.

### Commands and results

- `pnpm --dir demo/midgard-core exec vitest run tests/da-payload-envelope.test.ts` — 8/8; core typecheck/build pass.
- `pnpm --dir demo/midgard-sdk test` — 52/52; SDK typecheck/build pass.
- `pnpm --dir demo/da-committee-node run guard:no-http-da-transport && pnpm --dir demo/da-committee-node test` — guard pass; 148 passed, 1 skipped; committee typecheck/build pass.
- `NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/da-payload.test.ts tests/da-payload-libp2p-producer.test.ts tests/da-hardening.test.ts` — 25/25.
- `NODE_OPTIONS=--max-old-space-size=4096 NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/da-multi-process-50k-integration.test.ts` — 1/1, separate-process numbers above.
- Focused Postgres publication/rollback/delta command — 3 passed, 52 skipped. Phase 3 aggregate suite — 29/29; focused deep-state Postgres — 4/4.
- `pnpm --dir demo/midgard-node exec tsc --noEmit` — pass.
- `pnpm --dir demo/midgard-node run test:da-phase5-e2e --reporter=basic` — **25/25, no skips**, including the joined reconciler E2E; repeated twice with the same result. The exact operational 50k gate was also rerun independently (3/3 accepted).

The command results above are historical implementation evidence. The retained
`logs/throughput-resume-20260712T154400Z/phase5-da-e2e-fresh.log` predates the
strict operational binding: it records 23 tests and a 400,774-byte structural
fixture, so it is not accepted as current closeout evidence. A fresh 25-test
rerun against the checked-in 13,681,302-byte exact fixture and an isolated,
migrated Postgres database remains required before commit.

### Blocked exit gates

- The V1 100k one-hour soak is intentionally not run: the exact pre-submit guard rejects both measured 100k scenarios before mutation. Claiming a soak pass would require bypassing a protocol safety invariant.
- The 100k operational ratio is 2.9787x, below the stated 3x requirement, and its inner bytes exceed V1. The protocol proposal records cap/version choices and the durable delta/checkpoint alternative. No cap, timeout, default, or chunked-submit behavior was silently changed.

Exit-criterion mapping:

| Criterion                                                                       | Verdict                                  | Evidence                                                                                                                                                                                                                                                                                                |
| ------------------------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Canonical envelope, bounded decode, zip-bomb rejection                          | Pass                                     | Core envelope/sizing/deadline tests; declared oversize rejects before decompression and `maxOutputLength` bounds actual output.                                                                                                                                                                         |
| Decoder-first mixed-version rollout                                             | Pass at V1 deployable bound              | Real three-peer mixed-version run plus capability-quorum startup tests; old decoder rejection is structured and threshold-counted.                                                                                                                                                                      |
| Threshold early return, bounded fan-out, straggler recovery                     | Pass at 50k V1                           | Separate-process 50k run, degraded-peer/restart run, producer unit tests, and durable peer/announcement outbox tests.                                                                                                                                                                                   |
| Publication failure does not block local mutation readiness or L1 control plane | Pass                                     | Outboxes seed before mutation completion; worker returns before network I/O; legacy and speculative parent paths publish only after releasing the L1 permit. Delayed/dead-peer tests prove the permit is reacquirable, the mutation is complete, backlog remains durable, and publication is attempted. |
| Transport threshold is at least on-chain threshold                              | Pass                                     | Fail-closed startup assertion and capability tests.                                                                                                                                                                                                                                                     |
| No HTTP/URL DA transport boundary                                               | Pass                                     | Static guard passes across 19 committee, producer, and core target files.                                                                                                                                                                                                                               |
| 100k inner payload fits pinned V1 64 MiB                                        | **Blocked**                              | Operational 100k is 84,506,373 B; max-envelope 100k is 134,400,429 B.                                                                                                                                                                                                                                   |
| 100k compression ratio at least 3x                                              | **Blocked**                              | Operational 100k measures 2.9787x.                                                                                                                                                                                                                                                                      |
| Full 100k one-hour soak                                                         | **Blocked by preceding V1 safety gates** | Exact pre-submit sizing rejects before journal/sign/submit; running the soak would require bypassing the protocol invariant.                                                                                                                                                                            |
| Threshold publication ≤2 s p99                                                  | **Unmet**                                | Recorded exact-50k threshold samples span 1.903–2.493 s and are individual observations, not a p99 distribution; one sub-2-second run cannot establish p99.                                                                                                                                             |

### Historical implementation review (2026-07-10)

- The unchanged 64 MiB V1 frame cap now covers the complete inline submit request, including schema/envelope/CBOR overhead. Exact inner ceilings are 67,108,757 bytes (`off`), 67,108,710 (`identity`), and 66,847,587 (`zstd` worst-case bound).
- Non-off startup performs a deployment-scoped capability request and requires a transport-threshold quorum advertising schema V3, the selected content encoding, protocol V1, and exact manifest limits. Non-off mode without a publication manifest fails startup; `off` remains permitted without one.
- Producer and committee request paths use one absolute deadline across dial, write/backpressure, close, and response drain, aborting a stalled stream at expiry.
- Every committee payload-submit handler map shares one process-wide FIFO admission limiter before frame read/decompression. The live 50k three-process run reported `admissionPeakActive=1` for all processes.
- Peer delivery and gossip announcement are distinct durable outboxes. Claims use owner+token fencing, bounded parallel work, and token/expiry-conditioned completion. Zero-recipient gossip remains retryable; a later failure cannot downgrade published delivery.
- The durable outboxes are seeded before the local mutation job completes. The commit worker never waits on committee networking; it returns the finalized header hash, and both legacy and speculative parent paths trigger best-effort publication only after their outer L1-control-plane permit has been released.
- Conflicts are logged and metered immediately, stored monotonically, counted durably, and exposed as `da_publication_conflict:<count>` readiness failures.
- With the workflow and fixture closeout patch committed, clean CI will no longer silently skip or weaken the multi-process gate: the exact operational envelope and its measurement report are intentional checked-in fixtures. Missing or mismatched artifacts fail closed; there is no structural fallback.

Recorded review verification:

- `midgard-core`: 10 focused sizing/transport/deadline tests pass; typecheck/build pass.
- `da-committee-node`: 20 focused protocol/runtime tests pass; typecheck passes. The handler-map test proves two independently created maps share the same pre-read limiter.
- `midgard-node`: 22 capability/startup/producer tests pass; typecheck passes.
- Parent publication ordering: 3 focused tests pass, including delayed/failing legacy and speculative triggers with a reacquirable L1 permit and healthy readiness while durable backlog remains pending. Adjacent speculative safety/planner/session/MPF suites pass 39 tests.
- Recorded lightweight closure rerun: core 19/19; committee 34/34 plus typecheck and the 19-file no-HTTP guard; SDK 5/5; fault-proof decoder-first guard 1/1; node Phase-5 focused 37/37; full node typecheck, scoped ESLint/Prettier, and `git diff --check` pass. The absolute-deadline regression proves that a non-cooperative dial resolving after timeout is immediately aborted and never enters exchange work.
- The independent rereview was **GO for the implemented V1 scope, with no remaining P1/P2 in that scope**. It confirmed fail-closed/default-off behavior and the explicit 100k policy blocker; it did not waive the unmet exit criteria above.
- Real Postgres migration/outbox suite: 2 focused tests pass, covering crash-window seeding, competing claims, stale-token fencing, zero-recipient retry, monotone success, and durable conflicts.
- Real separate-process 50k operational path: 3/3 accepted, threshold 2,453.95 ms, all peers 2,529.43 ms, and admission peak 1 on all processes. This clears the ~20 s cadence but not the ≤2 s p99 target. The former generated structural fallback is no longer part of the gate.

### Closeout audit (2026-07-14)

- The exact 50k fixture contract is `tests/fixtures/da-operational-50k/envelope-50000.cbor` plus `measurement.json`; both must be committed with the strict test and provenance documentation.
- Main CI and nightly must provision and explicitly migrate an ephemeral Postgres database before the joined reconciler E2E. The test clears DA payload tables and must never target a live node database.
- A current clean-checkout run must retain the 25-test result, exact fixture hashes, three-process resource measurements, threshold/all-peer timings, and reconciler restart convergence.
- The 100k fit/ratio gate, one-hour soak, and ≤2 s p99 target remain blockers. They are not waived by the safe V1 implementation or the valid 50k single-run evidence.

### Exact-50k distribution-gate hardening (2026-07-14)

- `gate:phase5:da-50k-distribution` now owns the formal 100-publication
  measurement. It runs only on Node `v22.22.2` in the exact immutable
  redeployed image, starts one producer transport and three independent
  committee processes once, and records raw threshold/all-peer timings plus
  per-handler RSS and admission evidence. Process startup is outside every
  sample. Each interval starts before pinned zstd-3 envelope creation and its
  threshold interval stops exactly at the second accepted result; post-quorum
  gossip is not misreported as threshold-ACK time.
- The report verifier recomputes nearest-rank p50/p95/p99/max from all 100 raw
  samples and rejects padding, repeated header/envelope/inner/transaction-set
  identities, `duplicate` responses, declarative percentile edits, forced
  passes, V1-limit drift, runtime/image drift, missing resource samples, or
  admission concurrency above one. A measured p99 above 2,000 ms is preserved
  as `passed: false` and makes the formal command fail after retaining the
  report. The formal wrapper and offline verifier also re-hash the fixture
  suite, its source binding/manifest/generation evidence, full corpus and
  windows, and every decoded envelope, then cross-bind every report sample to
  the corresponding re-derived fixture identity. Every retained corpus and
  envelope transaction is decoded and re-encoded by the production Midgard
  native codec, then its compact native body is hashed with
  `computeMidgardNativeTxId`; the recomputed transaction ID must equal the
  declared key. File-backed negative tests cover raw corpus/window, envelope,
  inner payload, Phase 1, and report-sample tampering, including a false
  transaction ID whose aggregate hashes were recomputed by the attacker.
- The dedicated `Dockerfile.phase5-da-gate` is the reproducible operator
  surface. The deployed production image intentionally lacks test/dev tooling;
  the benchmark image pins Node `22.22.2`, installs the locked workspace,
  builds the `midgard-node` workspace dependency closure from the clean source
  context, and self-inspects through a read-only Docker CLI/socket mount. The
  report binds the immutable image ID, container/hostname self-identity,
  runtime, CPU, and memory configuration.
- The suite contract is stricter than a nominal repeat count: it requires 100
  disjoint 50k windows from a verified 5,000,000-row corpus and proves each
  decoded envelope transaction set against its source window. Both
  key-plus-CBOR and key-independent CBOR content hashes must be unique, so key
  relabeling cannot manufacture semantic independence. A direct Phase 1 source
  must match its formal binding; the retained 5M source instead uses a strict
  historical binding that anchors the exact Phase 1 base artifacts and proves
  the byte-identical prefix plus terminal-derived continuations. Entry zero is
  the checked 13,681,302-byte operational artifact. The exact contract and
  operator command are in
  `docs/benchmark-scenarios/phase-5-da-50k-distribution.md`.
- The former fixture-provenance blocker is closed by the strict offline
  historical extension under
  `demo/midgard-node/logs/phase5-historical-corpus-20260714`: 5,000,000 rows,
  4,096 chains, 8,192 index runs, and 100 disjoint exact-50k envelopes with 100
  unique transaction-set and content identities. Its corpus, binding,
  generation-result, and suite-manifest hashes are recorded above. Replaying
  the checked envelope, changing only its request/header identity, or counting
  `duplicate` results remains forbidden.
- Closing corpus provenance does not close the formal benchmark. The suite is
  explicitly historical-only (`freshLiveClaim: false`) and no Docker
  three-process timing distribution has run. There is therefore no 100-sample
  threshold-ACK report and no ≤2 s p99 pass. The 100k/64 MiB and one-hour-soak
  blockers are unchanged.
- After correcting the distribution verifier to use the production Midgard
  native transaction codec and compact-body transaction ID, the exact Node
  `v22.22.2` verifier suite passed 24 logical tests. A fresh bounded 50k
  three-process smoke then passed with 3/3 accepted, threshold ACK in
  2,492.46 ms, all peers in 2,542.21 ms, and admission peak one in every
  committee process. This is correctness and cadence evidence, not a formal
  p99 pass.
