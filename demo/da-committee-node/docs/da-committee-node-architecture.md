# Midgard DA Committee Node Architecture

This document defines the first public Midgard data-availability mechanism for deployments that cannot use Cardano Leios blobs.
The mechanism is a threshold committee of DA nodes that independently store, verify, sign, broadcast, and publicly serve Midgard block payloads.

The current on-chain `da_attestation.ak` design provides the attestation control plane.
It records that a threshold of configured committee keys attested to a Midgard block header.
This document defines the missing data plane: where the full block data is published, how committee nodes decide whether to sign, how signatures are broadcast, and how watchers retrieve the bytes without trusting operator-local APIs.

## Current Contract Boundary

This document is intentionally stronger than the contracts in one dimension: it defines the public data-plane obligations that make a DA signature meaningful.
The current Midgard contracts only verify the attestation control plane.

As implemented in this repository:

- The DA params governor owns a `MIDGARD_DA_PARAMS` NFT whose datum contains `committee`, `committee_signers_hash`, `da_threshold`, governance `owners`, and `update_threshold`.
- `committee` is the sorted unique packed byte string of 32-byte Ed25519 verification keys.
- `committee_signers_hash` is `blake2b_256(committee)`.
- `da_threshold` must be greater than zero and no larger than the committee length.
- Per block, the DA attestation policy mints a `DAAT || header_hash` token into an attestation UTxO.
- The attestation datum carries `header_hash`, `da_threshold`, `committee_signers_hash`, a 256-bit MSB-first signer bitmap, and `attestation_count`.
- `AddSignatures` accepts a packed byte string of `1-byte signer index || 64-byte Ed25519 signature` chunks. Indexes must be strictly ascending, and each signature is verified against the key at that index in the current DA params committee.
- `ApplyToStateQueue` requires `attestation_count >= da_threshold`, burns the `DAAT || header_hash` token, and rewrites the state-queue node from empty `da_attestation` to the DA attestation policy id.
- After attachment, the live state-queue node does not carry the signer bitmap, threshold, committee hash, or peer metadata. It only carries the DA attestation policy id marker; detailed signer evidence is in the historical attestation transactions and the attestation UTxO while it exists.
- The contracts do not verify payload bytes, libp2p retrieval readiness, retention windows, deployment manifests, peer broadcasts, or the 14-day availability promise. Those are requirements of the `threshold-mirror-v1` committee profile defined here.

The current node implementation also has a demo/default path: protocol initialization derives a one-key committee from the operator payment key with threshold `1`, and `attest-state-queue-once` creates, signs with signer index `0`, and applies attestations itself.
The public committee architecture below is the generalized profile that should replace or wrap that operator-local path for production deployments.

## Current Demo Node Compatibility

As of the current `demo` implementation, the demo node is not yet a DA committee
node.
It provides useful building blocks, but production committee support still requires new node, SDK, and deployment-surface work.

Reusable pieces:

- State-queue header construction, local header hashing, and root commitment logic already exist in the demo node and SDK.
- Pending block finalization journals already persist base block metadata and included transaction, deposit, and withdrawal member payloads before submission.
- The one-key `attest-state-queue-once` path already exercises the DA attestation UTxO lifecycle against the current contracts.

Required changes before this architecture is implemented by Midgard nodes:

- Protocol initialization must accept configured DA params and signed deployment-manifest DA metadata instead of always deriving a threshold-1 committee from the operator key.
- The Midgard producer payload and committee DA payload must become shared
  canonical SDK/core codecs. Current pending-finalization tables are
  implementation journals, not protocol payload formats.
- Block commitment must publish or hand off the canonical payload to the configured DA committee over the DA libp2p network and track publication/attestation state by `header_hash`.
- DA signing must be gated on durable payload storage, libp2p retrieval readiness, L1 header matching, and retention eligibility.
- `AddSignatures` transaction building must accept arbitrary sorted external witnesses and compute bitmap/count updates from the existing datum instead of hard-coding signer index `0` and count `1`.
- The node or a companion DA service must run the manifest-bound libp2p protocols defined below.
- Watcher and merge policies must require payload retrieval and header reconstruction when operating in DA-verifying mode, not only the on-chain DA-attestation marker.
- DA payload retention must be managed independently of the demo node's ordinary database retention sweeper.

## Security Claim

For a deployment using `threshold-mirror-v1` DA:

- A block is DA-acceptable only if a threshold of configured DA committee nodes have independently verified, stored, and made the canonical block payload retrievable over the DA libp2p network for at least 14 days.
- A DA committee signature over `header_hash` means: "I have the public payload needed to reconstruct this exact state-queue header and I will keep it available for 14 days."
- Watchers retrieve the payload from DA committee peers over libp2p, reconstruct the state-queue header from that payload, compare it to Cardano L1, and then run normal Midgard block verification.
- Operator databases, MPF stores, and admin endpoints are not security-critical DA sources.
- The DA committee is trusted for availability until a stronger L1-secured DA layer exists.

This is weaker than Leios because availability is backed by a configured committee, not Cardano L1 consensus.
It is still a concrete public mechanism: committee members cannot honestly sign until they can reconstruct the on-chain state commitment from locally stored payload bytes that are retrievable over libp2p.

## Attestation Semantics

The current `da_attestation.ak` message is:

```text
MidgardDAAttestationV1 || header_hash
```

By signing this message, a committee member attests that the block payload and
any derived proof artifacts required to reconstruct, verify, and challenge the
state commitment are publicly available and will remain available for 14 days.
This is the only required DA attestation in the V1 committee profile.
On chain, the signature preimage is exactly the UTF-8 bytes of `MidgardDAAttestationV1` concatenated with the 28-byte `header_hash`.

`MidgardDAAttestationV1` is the attestation profile and signing domain, not a separately served payload format.
Semantically, it means: "for this `header_hash`, the signer has the raw Midgard
block data and any required derived trace/proof artifacts needed to
independently reconstruct the committed `transactions_root`, `deposits_root`,
`withdrawals_root`, `utxos_root`, and related proof-critical commitments from
Cardano L1 state plus data retrievable from the DA libp2p network."
The Midgard producer payload is the source data bundle used to make the block
roots independently checkable by watchers. It must not be required to include
`transition_proof_data`. When transition trace roots are enabled, DA committee
nodes may derive trace/proof artifacts from that producer payload and propagate
an enriched committee DA payload containing `transition_proof_data` to other
committee peers. Receivers must recompute or verify the derived data before
signing. The chain still verifies only a signature over
`MidgardDAAttestationV1 || header_hash`.

The public identifier for DA retrieval is the state-queue `header_hash`.
Committee nodes and watchers validate payloads by deterministically reconstructing the full state-queue header from the payload and comparing it to the state-queue header observed on Cardano L1.

Local implementations may use private storage checksums for disk integrity.
Those checksums are not protocol commitments and are not part of watcher consensus.

Committee peer discovery is deployment-manifest-bound, not operator-supplied.
Given a signed deployment manifest and an attested state-queue header, a watcher can dial configured committee peers and retrieve the payload by `header_hash`.

## Components

```mermaid
flowchart LR
  OP["Operator node"] --> P2P["DA libp2p swarm"]
  P2P --> VAL["Payload validator"]
  VAL --> BS["Canonical block store"]
  VAL --> L1["L1 header resolver"]
  BS --> P2P
  L1 --> SIGN["Attestation signer"]
  SIGN --> BC["Signature broadcaster"]
  BC --> AGG["Attestation coordinator"]
  BC --> PEER["Peer DA nodes"]
  AGG --> SQ["Cardano state queue"]
  P2P --> W["Watcher/challenger"]
  SQ --> W
```

### DA Libp2p Transport

Accepts and replicates canonical block payloads from operators or peer committee nodes.
The DA protocol data plane is libp2p-only; HTTP endpoints are not part of the DA
transport, including as fallback, debug, gateway, or local development transport.

Transport responsibilities:

- Accept canonical Midgard producer payload bytes from operators.
- Accept canonical committee DA payload bytes from peer committee nodes, including
  derived `transition_proof_data` when trace roots are enabled.
- Authenticate peers by deployment-manifest peer id and configured signing key.
- Reject oversized payloads before expensive validation.
- Store an immutable staging record before any signature is produced.
- Return deterministic status for duplicate submissions.
- Reject conflicting bytes for a `header_hash` that this node has already signed.
- Apply peer scoring, rate limits, and backpressure before expensive validation.

### Payload Validator

Validates that the payload is complete and reconstructs the committed block header.

Validation responsibilities:

- Decode canonical CBOR with no trailing bytes, duplicate keys, or unknown required fields.
- Verify `deployment_fingerprint`, `network_id`, `protocol_version`, and `schema_version`.
- Recompute `transactions_root`, `deposits_root`, `withdrawals_root`, `utxos_root`, and all proof-critical member counts from the payload.
- Derive transition trace entries, event-to-step entries, and proof-bundle indexes when the L1 header commits to trace roots.
- Reconstruct the exact Midgard `Header` value from the payload and reconstructed roots.
- Compute `header_hash = blake2b_224(serialise_data(reconstructed_header))`.
- Resolve the matching state-queue node from Cardano L1 before signing.
- Verify the reconstructed header equals the state-queue header datum observed on L1.
- Verify the reconstructed `header_hash` equals the state-queue linked-list key and block asset suffix.
- Verify that all required preimages and committee-derived proof-bundle inputs needed for launch-scope proof families are present.
- Verify the local retention policy can keep the payload for 14 days from attestation.

The validator may store a payload before the L1 header exists, but it must not sign until the L1 header is observed and matched.

### Canonical Block Store

Stores immutable payload bytes and metadata by `header_hash`.

Required object keys:

```text
payload:{deployment_fingerprint}:{header_hash}
metadata:{deployment_fingerprint}:{header_hash}
proof_bundle:{deployment_fingerprint}:{header_hash}
trace_step:{deployment_fingerprint}:{header_hash}:{step_index}
event_to_step:{deployment_fingerprint}:{header_hash}:{event_key}
attestations:{deployment_fingerprint}:{header_hash}
```

The content keys are the DA retrieval identifiers used by libp2p request-response protocols.
Payload bytes are immutable after the node signs the header.

Store requirements:

- Atomic write before publish.
- Immutable storage keyed by deployment fingerprint and `header_hash`.
- Conflict detection for different payload bytes under one `header_hash`.
- Durable local disk persistence.
- Chunked reads for large payloads over libp2p streams.
- Retention sweeper that refuses deletion before the 14-day promise plus configured safety margin.

### L1 Header Resolver

Follows Cardano L1 enough to confirm that the target header exists in the Midgard state queue.

The resolver should use the same deployment manifest fields as watchers:

- Network id.
- Hub oracle.
- State queue policy and address.
- DA attestation policy.
- Protocol version.
- Finality and rollback policy.

The committee node can validate a staged payload before the header is on L1, but it signs only after the header is present at the configured confidence threshold.

### Attestation Signer

Owns one DA committee signing key and signs only after durable storage, libp2p retrieval readiness, and header reconstruction validation succeed.

Signer inputs:

- Canonical payload bytes.
- Reconstructed header.
- L1-observed state-queue header.
- Local libp2p retrieval readiness.
- 14-day retention eligibility.
- Deployment manifest.

Signer output:

```text
OnChainDaSignatureWitness =
  signer_index_u8 || ed25519_sign("MidgardDAAttestationV1" || header_hash)

AddSignatures.signatures =
  OnChainDaSignatureWitness*
```

Each witness is 65 bytes.
Witnesses in one `AddSignatures` redeemer must be sorted by strictly increasing signer index.
The signer index selects the 32-byte verification key slot in the packed on-chain DA params committee.

The signer may provide unsigned metadata such as payload byte length, schema version, reconstructed root summary, local retention expiry, and local storage status through `metadata-by-header`.
That metadata helps watchers and operators inspect retrieval, but the DA promise is the threshold on-chain signature over `header_hash`.

The signing key should live behind a local signer process, HSM, KMS, or strict filesystem permissions.
No service should log private keys, signing payload preimages that include secrets, or raw operator credentials.

### Signature Broadcaster

Broadcasts `OnChainDaSignatureWitness` after signing.

Broadcast targets:

- Attestation coordinator peer.
- Peer DA committee nodes.
- Optional operator peer.
- Local `attestations-by-header` request-response service.

Broadcast responsibilities:

- Retry with backoff until the signature is accepted or the block is no longer relevant.
- Deduplicate signatures by signer index and `header_hash`.
- Never broadcast a signature before the local payload is durably stored and retrievable over libp2p.
- Continue making payload retrievable even if no coordinator accepts the signature.

### Attestation Coordinator

Collects committee signatures and submits the on-chain DA attestation transactions.
This role can be run by the operator, a DA committee member, or a separate relayer.

Coordinator responsibilities:

- Create the initial DA attestation UTxO for an unattested state-queue header.
- Collect `AddSignatures` witnesses from committee nodes.
- Submit one or more `AddSignatures` transactions until the datum's `attestation_count` reaches the threshold.
- Submit `ApplyToStateQueue` to burn the `DAAT || header_hash` token and mark the state-queue node with the DA attestation policy id.
- Gossip the final attestation transaction references through the DA libp2p network.

The coordinator is not trusted for data availability.
It only transports signatures and submits L1 transactions.
The attestation policy is stateless and does not prove global uniqueness of `DAAT || header_hash`, so coordinators and watchers must resolve the valid attestation UTxO by datum, asset, and lifecycle evidence rather than assuming token supply uniqueness alone.

### Libp2p Protocol API

DA payload, metadata, and attestation exchange use stable libp2p protocol ids.
Every watcher should be able to start from a deployment manifest and Cardano L1
state, discover committee peers, and fetch payloads without operator assistance.

Required GossipSub topics:

```text
/midgard/{deployment_fingerprint}/da/payload-announcements/1
/midgard/{deployment_fingerprint}/da/attestations/1
/midgard/{deployment_fingerprint}/da/conflicts/1
```

Required request-response protocols:

```text
/midgard/{deployment_fingerprint}/da/payload-submit/1
/midgard/{deployment_fingerprint}/da/payload-by-header/1
/midgard/{deployment_fingerprint}/da/payload-chunk/1
/midgard/{deployment_fingerprint}/da/metadata-by-header/1
/midgard/{deployment_fingerprint}/da/proof-bundle-by-header/1
/midgard/{deployment_fingerprint}/da/attestations-by-header/1
```

`payload-submit` is the producer-to-committee write path. It can carry inline
payload bytes or a chunk manifest for large payloads. `payload-by-header` returns
canonical CBOR bytes, or a chunk manifest when the payload exceeds the
single-response limit. `payload-chunk` returns byte ranges or content-addressed
chunks from that manifest. Metadata, proof bundle, and attestation payloads use
canonical CBOR, not JSON.

## Deployment Manifest Fields

Watchers and committee nodes need the same DA discovery data in the signed deployment manifest.

Example:

```json
{
  "da": {
    "mode": "threshold-mirror-v1",
    "schemaVersion": 1,
    "committeeSignersHash": "...",
    "threshold": 5,
    "retentionSlots": 1209600,
    "payloadEncoding": "canonical-cbor",
    "members": [
      {
        "index": 0,
        "vkey": "...",
        "peerId": "12D3KooW...",
        "multiaddrs": [
          "/dns4/da-0.example.org/tcp/30333/noise/yamux/p2p/12D3KooW..."
        ]
      },
      {
        "index": 1,
        "vkey": "...",
        "peerId": "12D3KooW...",
        "multiaddrs": [
          "/dns4/da-1.example.org/tcp/30333/noise/yamux/p2p/12D3KooW..."
        ]
      }
    ]
  }
}
```

The sorted `members[*].vkey` bytes must hash to the on-chain `committee_signers_hash`.
More precisely, the on-chain committee is the concatenation of sorted unique 32-byte verification keys, and `committeeSignersHash` must equal `blake2b_256` of that packed byte string.
Peer identity and multiaddr metadata are manifest-bound so watchers do not depend on operator-supplied transport locations.
The manifest is not currently authenticated by the DA contracts; deployments must bind it to their release/deployment process.

## Payload Schema

The Midgard producer payload should be a canonical CBOR object with enough source
data to reconstruct the full state-queue header. It should not include
`transition_proof_data`; that data is derived and propagated by DA committee
nodes.

The committee-propagated DA payload may wrap the producer payload plus derived
`transition_proof_data`. Successful retrieval and validation of the producer
payload and any required committee-derived proof data justifies a committee
member's `MidgardDAAttestationV1 || header_hash` signature.
At minimum:

- `network_id`.
- `deployment_fingerprint`.
- `schema_version`.
- `protocol_version`.
- `prev_utxos_root`.
- `start_time`.
- `end_time`.
- `prev_header_hash`.
- `operator_vkey`.
- Ordered transaction envelopes and compact transaction members.
- Full transaction field-list preimages needed by fraud proofs.
- Deposit event members and preimages.
- Withdrawal event members and preimages.
- Included transaction members and preimages.
- Root member counts and byte lengths.
- Enough source data for a committee node to derive transition trace data once
  state-transition proofs are in scope.
- Enough source data for a committee node to derive proof-bundle metadata needed
  by every launch-scope proof family.

The payload does not need to carry a claimed `header_hash`.
The DA node derives `transactions_root`, `deposits_root`, `withdrawals_root`, `utxos_root`, builds the header, computes `header_hash`, and compares the result to Cardano L1 before signing.

## Publish And Attest Workflow

```mermaid
sequenceDiagram
  participant O as Operator
  participant D as DA Node
  participant C as Coordinator
  participant L as Cardano L1
  participant W as Watcher

  O->>D: Open libp2p stream with producer payload
  D->>D: Decode producer payload
  D->>D: Reconstruct roots and header
  D->>D: Derive trace/proof data if required
  D->>D: Store immutable payload
  O->>L: Append state-queue header
  D->>L: Observe state-queue header
  D->>D: Compare reconstructed header to L1 header
  D->>C: Gossip OnChainDaSignatureWitness
  D->>D: Gossip signature to peer DA nodes
  C->>L: AddSignatures until threshold
  C->>L: ApplyToStateQueue
  W->>L: Observe attested state-queue header
  W->>D: libp2p request payload by header hash
  W->>W: Reconstruct header and verify block
```

The operator may submit to multiple DA nodes directly over libp2p.
DA nodes may also request missing payloads from peer committee nodes after seeing a signature for a header, but a node must never sign data it has not stored, reconstructed, matched to L1, and made retrievable over libp2p.

## Watcher Retrieval Policy

For every queued block, a watcher should:

1. Observe the state-queue header on Cardano L1.
2. Confirm the state-queue node's `da_attestation` field equals the expected DA attestation policy id.
3. Load the DA committee peer ids and multiaddrs from the signed deployment manifest.
4. Fetch `metadata` from committee peers with `metadata-by-header`.
5. Recover threshold signature evidence from the DA attestation lifecycle transactions where available, especially if checking before attachment or auditing a deployment after the attestation UTxO has been burned.
6. Fetch payload bytes by `header_hash` from committee peers with `payload-by-header` and `payload-chunk`.
7. Reconstruct the state-queue header from the payload and compare it to the L1 header.
8. Cache the payload and all proof-critical preimages locally.

If the state queue is DA-attested but the watcher cannot retrieve a valid payload from the configured committee peers before the warning deadline, the block decision is `pending_da` and should escalate.
Near maturity this is an emergency, because missing DA can prevent fraud proof construction.

## Committee Node State Machine

```text
received
  -> decoded
  -> reconstructed_header
  -> stored
  -> waiting_for_l1_header
  -> l1_header_matched
  -> libp2p_retrievable
  -> signed
  -> gossiped
  -> expired
```

Failure states:

```text
rejected_malformed
rejected_wrong_deployment
rejected_header_mismatch
rejected_root_mismatch
rejected_missing_proof_data
rejected_retention_unavailable
conflicting_payload
storage_unavailable
signer_unavailable
broadcast_failed
```

Every state transition should be durable and auditable.
The node must recover after restart without signing a payload whose storage and libp2p retrieval status are unknown.

## Failure Handling

Malformed payload:

- Reject with a stable error code.
- Do not store as canonical.
- Do not sign.

Wrong root or header mismatch:

- Store optional forensic record.
- Do not sign.
- Expose conflict status for operators and monitors.

Duplicate payload:

- Return the existing metadata and signature if already signed.
- Do not rewrite immutable bytes.

Conflicting payload for same header:

- Mark `conflicting_payload`.
- Continue making any already signed canonical payload retrievable over libp2p.
- Do not sign a second payload for the same header.
- Alert operators and watchers.

Storage outage:

- Readiness fails.
- Signing disabled.
- Existing payloads remain retrievable over libp2p if possible.

Signer outage:

- Payload validation and storage may continue.
- Signing disabled.
- Readiness reports degraded signer state.

Broadcast outage:

- Payload retrieval continues over libp2p.
- Signature remains available from local storage and `attestations-by-header`.
- Retry broadcast to coordinator and peers with backoff.

Retention expiry:

- Delete payload bytes only after the 14-day promise plus configured safety margin.
- Keep metadata, signature witnesses, and attestation transaction references longer than payload bytes.

## Observability

Required metrics:

- `da_payloads_received_total`.
- `da_payloads_signed_total`.
- `da_payloads_rejected_total{reason}`.
- `da_payload_bytes_stored`.
- `da_header_reconstruction_seconds`.
- `da_l1_header_match_seconds`.
- `da_signature_broadcast_total{target,status}`.
- `da_libp2p_payload_requests_total{status}`.
- `da_libp2p_payload_bytes_total`.
- `da_libp2p_peers_connected`.
- `da_libp2p_gossip_messages_total{topic,status}`.
- `da_retention_expiring_payloads`.
- `da_storage_free_bytes`.
- `da_signer_available`.
- `da_ready`.

Required alerts:

- Valid payload cannot be stored.
- Payload is stored but cannot be retrieved over libp2p.
- L1 header observed but payload missing.
- Payload waits too long for L1 header.
- Reconstructed header does not match L1 header.
- Conflict for a header hash.
- Signer key unavailable.
- Signature broadcast fails repeatedly.
- Retention deadline is close while block is still in challenge window.
- Libp2p payload request error rate exceeds threshold.

## Implementation Phases

### Phase 1: Libp2p Public-Testnet Mirror

- Implement the canonical Midgard producer payload codec.
- Implement DA libp2p transport, header reconstruction, L1 header matching, header-keyed storage, and libp2p retrieval protocols.
- Add deployment manifest DA peer id and multiaddr fields.
- Produce indexed `OnChainDaSignatureWitness` values only after local payload storage, libp2p retrieval readiness, and L1 header matching are ready.
- Keep using current `da_attestation.ak` on-chain signatures.
- Watchers require on-chain DA attestation plus successful payload retrieval and header reconstruction.

### Phase 2: Signature Broadcast And Attestation Coordinator

- Add signature broadcaster in each DA committee node.
- Generalize the current operator-local `attest-state-queue-once` flow into a coordinator service for the DA attestation UTxO lifecycle.
- Collect indexed signatures from committee nodes.
- Submit `Init`, `AddSignatures`, and `ApplyToStateQueue`.
- Expose final attestation sets for watchers.

### Phase 3: Committee Accountability

- Add committee operator runbooks and uptime requirements.
- Add signed incident records for unavailable data.
- If the protocol later supports committee slashing, bind signatures to slashable DA commitments.

## Open Protocol Decisions

- Exact Midgard producer payload CBOR shape.
- Exact committee DA payload CBOR shape, including optional
  `transition_proof_data`.
- Exact deterministic header reconstruction rules, including transition trace commitments once enabled.
- Exact slot/deadline expression for the 14-day retention promise.
- How committee nodes discover coordinator peers.
- Whether committee members are bonded and slashable for false availability claims.
- Whether DA nodes should require authenticated operator payload streams or accept public payload streams with rate limits.
