import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import { DA_PAYLOAD_INNER_SCHEMA_VERSION } from "./da-payload-envelope.js";

export const DA_TRANSPORT_PROTOCOL_VERSION = 1 as const;

export const DA_DEPLOYMENT_FINGERPRINT_LENGTH = 32;

export const DA_HEADER_HASH_LENGTH = 28;

export const DA_HASH_LENGTH = 32;

export const DA_GOSSIP_SIGNATURE_LENGTH = 64;

export const DA_ON_CHAIN_WITNESS_LENGTH = 65;

export const DA_RUNTIME_MANIFEST_SCHEMA_VERSION =
  "midgard-da-libp2p-runtime-manifest-v1";

export const DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE =
  "contract_deployment_manifest_id";

export const DA_PUBLIC_RETAINED_DA_PROFILE = "public-retained-da-v1" as const;

export const DA_PUBLIC_RETAINED_DA_ACCESS_POLICY =
  "any_noise_authenticated_peer" as const;

/**
 * This is deliberately a positive list.  The public profile is a one-way
 * retained-data service; it must never grow into the committee control plane.
 */
export const DA_PUBLIC_RETAINED_DA_PROTOCOLS = [
  "capabilities",
  "payload-by-header",
  "payload-chunk",
  "metadata-by-header",
  "proof-bundle-by-header",
  "trace-step-by-index",
  "event-to-step-by-event",
] as const;

export const DA_TRANSPORT_LIMITS = {
  maxPayloadBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
  maxInlineResponseBytes: 1_048_576,
  maxChunkBytes: 1_048_576,
  maxGossipMessageBytes: 65_536,
  maxStreamsPerPeer: 16,
  requestTimeoutMs: 15_000,
  minimumRetentionDays: 15,
} as const;

export const DA_ON_CHAIN_ATTESTATION_DOMAIN = "MidgardDAAttestationV1";

export type DaTransportTimingOptions = {
  readonly monotonicNow?: () => number;
  readonly onStageTiming?: (
    stage: "submit_request_decode",
    durationMs: number,
  ) => void;
};

export const DaTransportSigningDomain = {
  payloadAnnouncement: "MidgardDALibp2pPayloadAnnouncementV1",
  payloadSubmit: "MidgardDALibp2pPayloadSubmitV1",
  conflictEvidence: "MidgardDALibp2pConflictEvidenceV1",
} as const;

export type DaTransportSigningDomain =
  (typeof DaTransportSigningDomain)[keyof typeof DaTransportSigningDomain];

export const DaGossipTopic = {
  payloadAnnouncements: "payload-announcements",
  attestations: "attestations",
  conflicts: "conflicts",
} as const;

export type DaGossipTopic = (typeof DaGossipTopic)[keyof typeof DaGossipTopic];

export type DaLibp2pRuntimeManifest = {
  readonly schemaVersion: typeof DA_RUNTIME_MANIFEST_SCHEMA_VERSION;
  readonly network: string;
  readonly deployment: {
    readonly fingerprint: string;
    readonly contract_deployment_manifest_id: string;
    readonly contract_deployment_info_sha256: string;
    readonly identity_source: typeof DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE;
  };
  readonly runtime_topology:
    | {
        readonly target: "producer";
        readonly profile: string;
        readonly producer_peer_id: string;
      }
    | {
        readonly target: "committee";
        readonly profile: string;
        readonly producer_peer_id: string;
        readonly local_signer_index: number;
      };
  readonly da_transport: {
    readonly kind: "libp2p";
    readonly no_http_da_transport: true;
    readonly listen_multiaddrs: readonly string[];
    readonly announce_multiaddrs: readonly string[];
    readonly bootstrap_multiaddrs: readonly string[];
    readonly gossip: {
      readonly strict_sign: true;
      readonly emit_self: false;
      readonly allowed_topics_only: true;
      readonly max_gossip_message_bytes: number;
    };
    readonly limits: {
      readonly max_payload_bytes: number;
      readonly max_inline_response_bytes: number;
      readonly max_chunk_bytes: number;
      readonly max_streams_per_peer: number;
      readonly request_timeout_ms: number;
    };
    readonly retention_days: number;
  };
  readonly public_retained_da: {
    readonly profile: typeof DA_PUBLIC_RETAINED_DA_PROFILE;
    readonly access_policy: typeof DA_PUBLIC_RETAINED_DA_ACCESS_POLICY;
    /** A dedicated, non-committee Noise identity. */
    readonly peer_id: string;
    readonly listen_multiaddrs: readonly string[];
    readonly announce_multiaddrs: readonly string[];
    readonly protocols: readonly (typeof DA_PUBLIC_RETAINED_DA_PROTOCOLS)[number][];
    readonly limits: {
      readonly max_streams_per_peer: number;
      readonly max_inflight_requests: number;
      readonly max_inflight_requests_per_peer: number;
      readonly max_inflight_proof_requests: number;
      readonly request_timeout_ms: number;
    };
  };
  readonly da_committee: {
    readonly threshold: number;
    readonly members: readonly {
      readonly signer_index: number;
      readonly da_vkey: string;
      readonly peer_id: string;
      readonly multiaddrs: readonly string[];
      readonly roles: readonly string[];
    }[];
  };
};

export const DaRequestResponseProtocol = {
  capabilities: "capabilities",
  payloadSubmit: "payload-submit",
  payloadByHeader: "payload-by-header",
  payloadChunk: "payload-chunk",
  metadataByHeader: "metadata-by-header",
  proofBundleByHeader: "proof-bundle-by-header",
  traceStepByIndex: "trace-step-by-index",
  eventToStepByEvent: "event-to-step-by-event",
  attestationsByHeader: "attestations-by-header",
} as const;

export type DaRequestResponseProtocol =
  (typeof DaRequestResponseProtocol)[keyof typeof DaRequestResponseProtocol];

export const DaPayloadSubmitMode = {
  inline: 0,
  chunked: 1,
} as const;

export type DaPayloadSubmitMode = keyof typeof DaPayloadSubmitMode;

export const DaPayloadSubmitStatus = {
  accepted: 0,
  duplicate: 1,
  conflict: 2,
  rejected: 3,
  deferred: 4,
} as const;

export type DaPayloadSubmitStatus = keyof typeof DaPayloadSubmitStatus;

export const DaPayloadByHeaderStatus = {
  found_inline: 0,
  found_chunked: 1,
  not_found: 2,
  conflict: 3,
  rejected: 4,
} as const;

export type DaPayloadByHeaderStatus = keyof typeof DaPayloadByHeaderStatus;

export const DaGenericFoundStatus = {
  found: 0,
  not_found: 1,
  rejected: 2,
} as const;

export type DaGenericFoundStatus = keyof typeof DaGenericFoundStatus;

export const DaProofBundleStatus = {
  found_inline: 0,
  found_chunked: 1,
  not_found: 2,
  rejected: 3,
} as const;

export type DaProofBundleStatus = keyof typeof DaProofBundleStatus;

export const DaMetadataStatus = {
  found: 0,
  not_found: 1,
  conflict: 2,
  rejected: 3,
} as const;

export type DaMetadataStatus = keyof typeof DaMetadataStatus;

export const DaLocalPayloadStatus = {
  staged: 0,
  verified: 1,
  signed: 2,
  conflict: 3,
} as const;

export type DaLocalPayloadStatus = keyof typeof DaLocalPayloadStatus;

export const DaConflictEvidenceKind = {
  conflicting_payload_bytes: 0,
  invalid_roots: 1,
  signature_without_retrieval: 2,
  malformed_message: 3,
  equivocation: 4,
} as const;

export type DaConflictEvidenceKind = keyof typeof DaConflictEvidenceKind;

export type DaPayloadChunkManifest = {
  readonly payloadHash: Buffer;
  readonly totalBytes: number;
  readonly chunkSize: number;
  readonly chunkHashes: readonly Buffer[];
};

export type DaPayloadAnnouncement = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly payloadSchemaVersion: typeof DA_PAYLOAD_INNER_SCHEMA_VERSION;
  readonly payloadBytes: number;
  readonly chunkSize: number;
  readonly chunkCount: number;
  readonly rootSummaryHash: Buffer;
  readonly announcedByPeerId: string;
  readonly announcedAtSlot: number;
  readonly signature: Buffer;
};

export type DaPayloadSubmitRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly payloadSchemaVersion: typeof DA_PAYLOAD_INNER_SCHEMA_VERSION;
  readonly mode: DaPayloadSubmitMode;
  readonly payloadBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifest | null;
};

export type DaPayloadSubmitResponse = {
  readonly status: DaPayloadSubmitStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly reasonCode: string | null;
  readonly retryAfterMs: number | null;
};

export type DaCapabilitiesRequest = {
  readonly deploymentFingerprint: Buffer;
};

export type DaCapabilitiesResponse = {
  readonly deploymentFingerprint: Buffer;
  readonly transportProtocolVersion: typeof DA_TRANSPORT_PROTOCOL_VERSION;
  readonly payloadSchemaVersions: readonly [
    typeof DA_PAYLOAD_INNER_SCHEMA_VERSION,
  ];
  readonly envelopeContentEncodings: readonly number[];
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly requestTimeoutMs: number;
};

export type DaPayloadByHeaderRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly acceptedPayloadHashes: readonly Buffer[] | null;
  readonly maxInlineBytes: number;
};

export type DaPayloadByHeaderResponse = {
  readonly status: DaPayloadByHeaderStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer | null;
  readonly payloadBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifest | null;
  readonly reasonCode: string | null;
};
