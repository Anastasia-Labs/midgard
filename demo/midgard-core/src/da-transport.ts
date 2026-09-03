import { sha256 } from "@noble/hashes/sha2.js";

import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "./codec/cbor.js";
import {
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
} from "./codec/errors.js";
import { ensureHash32 } from "./codec/hash.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile-v1.js";
import {
  DA_PAYLOAD_INNER_SCHEMA_VERSION,
  DaPayloadContentEncoding,
} from "./da-payload-envelope.js";

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
        readonly target: "watcher";
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

export type DaPayloadChunkRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly chunkIndex: number;
};

export type DaPayloadChunkResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly chunkIndex: number;
  readonly chunkBytes: Buffer | null;
  readonly chunkHash: Buffer | null;
};

export type DaMetadataByHeaderResponse = {
  readonly status: DaMetadataStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer | null;
  readonly payloadSchemaVersion: typeof DA_PAYLOAD_INNER_SCHEMA_VERSION | null;
  readonly payloadBytes: number | null;
  readonly rootSummaryHash: Buffer | null;
  readonly proofBundleHash: Buffer | null;
  readonly transitionTraceRoot: Buffer | null;
  readonly eventToStepRoot: Buffer | null;
  readonly retainedUntilSlot: number | null;
  readonly localStatus: DaLocalPayloadStatus | null;
};

export type DaProofBundleByHeaderRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly maxInlineBytes: number;
};

export type DaProofBundleByHeaderResponse = {
  readonly status: DaProofBundleStatus;
  readonly headerHash: Buffer;
  readonly proofBundleHash: Buffer | null;
  readonly proofBundleBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifest | null;
  readonly reasonCode: string | null;
};

export type DaTraceStepByIndexRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly stepIndex: number;
};

export type DaTraceStepByIndexResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly stepIndex: number;
  readonly transitionStepBytes: Buffer | null;
  readonly membershipProofBytes: Buffer | null;
};

export type DaEventToStepByEventRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly eventKey: Buffer;
};

export type DaEventToStepByEventResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly eventKey: Buffer;
  readonly eventToStepEntryBytes: Buffer | null;
  readonly membershipOrNonmembershipProofBytes: Buffer | null;
};

export type DaAttestationGossip = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly availabilityCommitmentCbor: Buffer;
  readonly availabilityCommitmentDigest: Buffer;
  readonly signerIndex: number;
  readonly daVkey: Buffer;
  readonly onChainWitness: Buffer;
  readonly retentionUntilSlot: number;
  readonly announcedByPeerId: string;
};

export type DaAttestationsByHeaderRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly acceptedSignerIndexes: readonly number[] | null;
  readonly maxAttestations: number | null;
};

export type DaAttestationsByHeaderResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly attestations: readonly DaAttestationGossip[];
  readonly reasonCode: string | null;
};

export type DaConflictEvidence = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly evidenceKind: DaConflictEvidenceKind;
  readonly evidenceHash: Buffer;
  readonly compactEvidence: Buffer | null;
};

export type DaConflictingSignatureHeaderEvidence = {
  readonly signerIndex: number;
  readonly daVkey: Buffer;
  readonly lowerHeaderHash: Buffer;
  readonly lowerCommitmentCbor: Buffer;
  readonly lowerHeaderWitness: Buffer;
  readonly upperHeaderHash: Buffer;
  readonly upperCommitmentCbor: Buffer;
  readonly upperHeaderWitness: Buffer;
};

type NumericEnumTable = Readonly<Record<string, number>>;
type NumericEnumLabel<T extends NumericEnumTable> = Extract<keyof T, string>;

const fail = (
  code: (typeof MidgardTxCodecErrorCodes)[keyof typeof MidgardTxCodecErrorCodes],
  message: string,
  detail?: string,
): never => {
  throw new MidgardTxCodecError(code, message, detail);
};

const enumCode = <T extends NumericEnumTable>(
  table: T,
  label: NumericEnumLabel<T>,
  fieldName: string,
): number => {
  const code = table[label];
  if (code === undefined) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} has unsupported enum label`,
      String(label),
    );
  }
  return code;
};

const enumLabel = <T extends NumericEnumTable>(
  table: T,
  code: number,
  fieldName: string,
): NumericEnumLabel<T> => {
  for (const [label, candidateCode] of Object.entries(table) as [
    NumericEnumLabel<T>,
    number,
  ][]) {
    if (candidateCode === code) {
      return label;
    }
  }
  return fail(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `${fieldName} has unsupported enum code`,
    String(code),
  );
};

const ensureUint = (value: unknown, fieldName: string): number => {
  const int = asBigInt(value, fieldName);
  if (int < 0n || int > BigInt(Number.MAX_SAFE_INTEGER)) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a safe unsigned integer`,
      int.toString(),
    );
  }
  return Number(int);
};

const ensureDaPayloadSchema = (
  value: unknown,
  fieldName: string,
): typeof DA_PAYLOAD_INNER_SCHEMA_VERSION => {
  const version = ensureUint(value, fieldName);
  if (version !== DA_PAYLOAD_INNER_SCHEMA_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${DA_PAYLOAD_INNER_SCHEMA_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return DA_PAYLOAD_INNER_SCHEMA_VERSION;
};

const ensureDaTransport = (
  value: unknown,
  fieldName: string,
): typeof DA_TRANSPORT_PROTOCOL_VERSION => {
  const version = ensureUint(value, fieldName);
  if (version !== DA_TRANSPORT_PROTOCOL_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${DA_TRANSPORT_PROTOCOL_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return DA_TRANSPORT_PROTOCOL_VERSION;
};

const ensureUint8 = (value: unknown, fieldName: string): number => {
  const int = ensureUint(value, fieldName);
  if (int > 255) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a uint8`,
      String(int),
    );
  }
  return int;
};

const cborUint = (value: unknown, fieldName: string): bigint =>
  BigInt(ensureUint(value, fieldName));

const cborOptionalUint = (value: unknown, fieldName: string): bigint | null =>
  value == null ? null : cborUint(value, fieldName);

const stringValue = (value: unknown, fieldName: string): string => {
  if (typeof value !== "string") {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a string`,
    );
  }
  return value as string;
};

const exactStringEnumValue = <T extends Readonly<Record<string, string>>>(
  table: T,
  value: unknown,
  fieldName: string,
): T[keyof T] => {
  const exact = stringValue(value, fieldName);
  if (!Object.values(table).includes(exact)) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} is not supported by DA transport V1`,
      exact,
    );
  }
  return exact as T[keyof T];
};

const optionalStringValue = (
  value: unknown,
  fieldName: string,
): string | null => (value == null ? null : stringValue(value, fieldName));

const fixedArray = (
  value: unknown,
  expectedLength: number,
  fieldName: string,
): unknown[] => {
  const arr = asArray(value, fieldName);
  if (arr.length !== expectedLength) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must have exactly ${expectedLength} elements`,
      `length=${arr.length}`,
    );
  }
  return arr;
};

const ensureByteLength = (
  value: Uint8Array,
  expectedLength: number,
  fieldName: string,
): Buffer => {
  const bytes = Buffer.from(value);
  if (bytes.length !== expectedLength) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be ${expectedLength} bytes`,
      `length=${bytes.length}`,
    );
  }
  return bytes;
};

const bytesValue = (value: unknown, fieldName: string): Buffer =>
  asBufferView(asBytes(value, fieldName));

const asBufferView = (value: Uint8Array): Buffer =>
  Buffer.isBuffer(value)
    ? value
    : Buffer.from(value.buffer, value.byteOffset, value.byteLength);

const optionalBytesValue = (
  value: unknown,
  fieldName: string,
): Buffer | null => (value == null ? null : bytesValue(value, fieldName));

const ensureNonEmptyBytes = (value: Uint8Array, fieldName: string): Buffer => {
  const bytes = Buffer.from(value);
  if (bytes.length === 0) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be non-empty`,
    );
  }
  return bytes;
};

export const ensureDaDeploymentFingerprint = (
  value: Uint8Array,
  fieldName = "deployment_fingerprint",
): Buffer =>
  ensureByteLength(value, DA_DEPLOYMENT_FINGERPRINT_LENGTH, fieldName);

export const ensureDaHeaderHash = (
  value: Uint8Array,
  fieldName = "header_hash",
): Buffer => ensureByteLength(value, DA_HEADER_HASH_LENGTH, fieldName);

export const ensureDaHash32 = (value: Uint8Array, fieldName = "hash"): Buffer =>
  ensureHash32(value, fieldName);

export const ensureDaPayloadHash = (
  value: Uint8Array,
  fieldName = "payload_hash",
): Buffer => ensureDaHash32(value, fieldName);

export const normalizeDaDeploymentFingerprintHex = (
  value: string | Uint8Array,
): string => {
  if (typeof value !== "string") {
    return ensureDaDeploymentFingerprint(value).toString("hex");
  }
  const normalized = value.toLowerCase();
  if (!/^[0-9a-f]{64}$/.test(normalized)) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "deployment_fingerprint must be 32-byte hex",
      value,
    );
  }
  return normalized;
};

export const daDeploymentFingerprintFromHex = (value: string): Buffer =>
  Buffer.from(normalizeDaDeploymentFingerprintHex(value), "hex");

const recordValue = (
  value: unknown,
  fieldName: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a JSON object`,
    );
  }
  return value as Record<string, unknown>;
};

const nonEmptyStringValue = (value: unknown, fieldName: string): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a non-empty string`,
    );
  }
  return value as string;
};

const stringArrayValue = (
  value: unknown,
  fieldName: string,
  allowEmpty = false,
): readonly string[] => {
  if (!Array.isArray(value)) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a string array`,
    );
  }
  if (!allowEmpty && value.length === 0) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a non-empty string array`,
    );
  }
  return value.map((entry, index) =>
    nonEmptyStringValue(entry, `${fieldName}[${index.toString()}]`),
  );
};

const safeIntegerValue = (
  value: unknown,
  fieldName: string,
  minimum: number,
  maximum = Number.MAX_SAFE_INTEGER,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum ||
    value > maximum
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a safe integer from ${minimum.toString()} through ${maximum.toString()}`,
    );
  }
  return value as number;
};

const exactRecordKeys = (
  value: Record<string, unknown>,
  keys: readonly string[],
  fieldName: string,
): void => {
  const expected = new Set(keys);
  for (const key of Object.keys(value)) {
    if (!expected.has(key)) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName}.${key} is unexpected`,
      );
    }
  }
  for (const key of keys) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName}.${key} is required`,
      );
    }
  }
};

const parseDaLibp2pRuntimeManifestDeployment = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["deployment"] => {
  const deployment = recordValue(value, fieldName);
  exactRecordKeys(
    deployment,
    [
      "fingerprint",
      "contract_deployment_manifest_id",
      "contract_deployment_info_sha256",
      "identity_source",
    ],
    fieldName,
  );
  if (
    deployment.identity_source !== DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.identity_source must be ${DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE}`,
      String(deployment.identity_source),
    );
  }
  const fingerprint = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(deployment.fingerprint, `${fieldName}.fingerprint`),
  );
  const contractDeploymentManifestId = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(
      deployment.contract_deployment_manifest_id,
      `${fieldName}.contract_deployment_manifest_id`,
    ),
  );
  if (fingerprint !== contractDeploymentManifestId) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.fingerprint must equal deployment.contract_deployment_manifest_id`,
      `fingerprint=${fingerprint},contract_deployment_manifest_id=${contractDeploymentManifestId}`,
    );
  }
  return {
    fingerprint,
    contract_deployment_manifest_id: contractDeploymentManifestId,
    contract_deployment_info_sha256: normalizeDaDeploymentFingerprintHex(
      nonEmptyStringValue(
        deployment.contract_deployment_info_sha256,
        `${fieldName}.contract_deployment_info_sha256`,
      ),
    ),
    identity_source: DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE,
  };
};

const parseDaLibp2pRuntimeTopology = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["runtime_topology"] => {
  const topology = recordValue(value, fieldName);
  if (topology.target === "producer") {
    exactRecordKeys(
      topology,
      ["target", "profile", "producer_peer_id"],
      fieldName,
    );
    return {
      target: "producer",
      profile: nonEmptyStringValue(topology.profile, `${fieldName}.profile`),
      producer_peer_id: nonEmptyStringValue(
        topology.producer_peer_id,
        `${fieldName}.producer_peer_id`,
      ),
    };
  }
  if (topology.target === "watcher") {
    exactRecordKeys(
      topology,
      ["target", "profile", "producer_peer_id", "local_signer_index"],
      fieldName,
    );
    return {
      target: "watcher",
      profile: nonEmptyStringValue(topology.profile, `${fieldName}.profile`),
      producer_peer_id: nonEmptyStringValue(
        topology.producer_peer_id,
        `${fieldName}.producer_peer_id`,
      ),
      local_signer_index: safeIntegerValue(
        topology.local_signer_index,
        `${fieldName}.local_signer_index`,
        0,
        255,
      ),
    };
  }
  return fail(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    `${fieldName}.target must be producer or watcher`,
    String(topology.target),
  );
};

const parseDaLibp2pRuntimeTransport = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["da_transport"] => {
  const transport = recordValue(value, fieldName);
  exactRecordKeys(
    transport,
    [
      "kind",
      "no_http_da_transport",
      "listen_multiaddrs",
      "announce_multiaddrs",
      "bootstrap_multiaddrs",
      "gossip",
      "limits",
      "retention_days",
    ],
    fieldName,
  );
  if (transport.kind !== "libp2p") {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.kind must be libp2p`,
      String(transport.kind),
    );
  }
  if (transport.no_http_da_transport !== true) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.no_http_da_transport must be true`,
      String(transport.no_http_da_transport),
    );
  }
  const gossipFieldName = `${fieldName}.gossip`;
  const gossip = recordValue(transport.gossip, gossipFieldName);
  exactRecordKeys(
    gossip,
    [
      "strict_sign",
      "emit_self",
      "allowed_topics_only",
      "max_gossip_message_bytes",
    ],
    gossipFieldName,
  );
  if (
    gossip.strict_sign !== true ||
    gossip.emit_self !== false ||
    gossip.allowed_topics_only !== true
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${gossipFieldName} must use strict_sign=true, emit_self=false, and allowed_topics_only=true`,
    );
  }
  if (
    gossip.max_gossip_message_bytes !==
    DA_TRANSPORT_LIMITS.maxGossipMessageBytes
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${gossipFieldName}.max_gossip_message_bytes must be ${DA_TRANSPORT_LIMITS.maxGossipMessageBytes.toString()}`,
      String(gossip.max_gossip_message_bytes),
    );
  }
  const limitsFieldName = `${fieldName}.limits`;
  const limits = recordValue(transport.limits, limitsFieldName);
  exactRecordKeys(
    limits,
    [
      "max_payload_bytes",
      "max_inline_response_bytes",
      "max_chunk_bytes",
      "max_streams_per_peer",
      "request_timeout_ms",
    ],
    limitsFieldName,
  );
  const expectedLimits = {
    max_payload_bytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    max_inline_response_bytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
    max_chunk_bytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
    max_streams_per_peer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
    request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedLimits)) {
    if (limits[key] !== expected) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${limitsFieldName}.${key} must be ${expected.toString()}`,
        String(limits[key]),
      );
    }
  }
  const retentionDays = safeIntegerValue(
    transport.retention_days,
    `${fieldName}.retention_days`,
    DA_TRANSPORT_LIMITS.minimumRetentionDays,
  );
  return {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: stringArrayValue(
      transport.listen_multiaddrs,
      `${fieldName}.listen_multiaddrs`,
    ),
    announce_multiaddrs: stringArrayValue(
      transport.announce_multiaddrs,
      `${fieldName}.announce_multiaddrs`,
    ),
    bootstrap_multiaddrs: stringArrayValue(
      transport.bootstrap_multiaddrs,
      `${fieldName}.bootstrap_multiaddrs`,
      true,
    ),
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
    },
    limits: expectedLimits,
    retention_days: retentionDays,
  };
};

const parseDaLibp2pRuntimeCommittee = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["da_committee"] => {
  const committee = recordValue(value, fieldName);
  exactRecordKeys(committee, ["threshold", "members"], fieldName);
  const threshold = safeIntegerValue(
    committee.threshold,
    `${fieldName}.threshold`,
    1,
  );
  const rawMembers = committee.members;
  if (!Array.isArray(rawMembers)) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.members must be an array`,
    );
  }
  if (rawMembers.length === 0) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.members must be a non-empty array`,
    );
  }
  const members = rawMembers.map((value, index) => {
    const memberFieldName = `${fieldName}.members[${index.toString()}]`;
    const member = recordValue(value, memberFieldName);
    exactRecordKeys(
      member,
      ["signer_index", "da_vkey", "peer_id", "multiaddrs", "roles"],
      memberFieldName,
    );
    return {
      signer_index: safeIntegerValue(
        member.signer_index,
        `${memberFieldName}.signer_index`,
        0,
        255,
      ),
      da_vkey: nonEmptyStringValue(
        member.da_vkey,
        `${memberFieldName}.da_vkey`,
      ),
      peer_id: nonEmptyStringValue(
        member.peer_id,
        `${memberFieldName}.peer_id`,
      ),
      multiaddrs: stringArrayValue(
        member.multiaddrs,
        `${memberFieldName}.multiaddrs`,
      ),
      roles: stringArrayValue(member.roles, `${memberFieldName}.roles`),
    };
  });
  if (threshold > members.length) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.threshold must be no greater than member count`,
    );
  }
  return { threshold, members };
};

const parsePublicRetainedDaProfile = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["public_retained_da"] => {
  const profile = recordValue(value, fieldName);
  exactRecordKeys(
    profile,
    [
      "profile",
      "access_policy",
      "peer_id",
      "listen_multiaddrs",
      "announce_multiaddrs",
      "protocols",
      "limits",
    ],
    fieldName,
  );
  if (profile.profile !== DA_PUBLIC_RETAINED_DA_PROFILE) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.profile must be ${DA_PUBLIC_RETAINED_DA_PROFILE}`,
      String(profile.profile),
    );
  }
  if (profile.access_policy !== DA_PUBLIC_RETAINED_DA_ACCESS_POLICY) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.access_policy must be ${DA_PUBLIC_RETAINED_DA_ACCESS_POLICY}`,
      String(profile.access_policy),
    );
  }
  const protocols = stringArrayValue(
    profile.protocols,
    `${fieldName}.protocols`,
  );
  if (
    protocols.length !== DA_PUBLIC_RETAINED_DA_PROTOCOLS.length ||
    protocols.some(
      (protocol, index) => protocol !== DA_PUBLIC_RETAINED_DA_PROTOCOLS[index],
    )
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.protocols must equal the public retained DA protocol allowlist`,
    );
  }
  const limitsFieldName = `${fieldName}.limits`;
  const limits = recordValue(profile.limits, limitsFieldName);
  exactRecordKeys(
    limits,
    [
      "max_streams_per_peer",
      "max_inflight_requests",
      "max_inflight_requests_per_peer",
      "max_inflight_proof_requests",
      "request_timeout_ms",
    ],
    limitsFieldName,
  );
  const maxStreamsPerPeer = safeIntegerValue(
    limits.max_streams_per_peer,
    `${limitsFieldName}.max_streams_per_peer`,
    1,
    DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
  );
  const maxInflightRequests = safeIntegerValue(
    limits.max_inflight_requests,
    `${limitsFieldName}.max_inflight_requests`,
    1,
    256,
  );
  const maxInflightRequestsPerPeer = safeIntegerValue(
    limits.max_inflight_requests_per_peer,
    `${limitsFieldName}.max_inflight_requests_per_peer`,
    1,
    maxInflightRequests,
  );
  const maxInflightProofRequests = safeIntegerValue(
    limits.max_inflight_proof_requests,
    `${limitsFieldName}.max_inflight_proof_requests`,
    1,
    maxInflightRequests,
  );
  const requestTimeoutMs = safeIntegerValue(
    limits.request_timeout_ms,
    `${limitsFieldName}.request_timeout_ms`,
    100,
    DA_TRANSPORT_LIMITS.requestTimeoutMs,
  );
  const peerId = nonEmptyStringValue(profile.peer_id, `${fieldName}.peer_id`);
  const announceMultiaddrs = stringArrayValue(
    profile.announce_multiaddrs,
    `${fieldName}.announce_multiaddrs`,
  );
  if (
    !announceMultiaddrs.every((address) => address.endsWith(`/p2p/${peerId}`))
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.announce_multiaddrs must bind public_retained_da.peer_id`,
    );
  }
  return {
    profile: DA_PUBLIC_RETAINED_DA_PROFILE,
    access_policy: DA_PUBLIC_RETAINED_DA_ACCESS_POLICY,
    peer_id: peerId,
    listen_multiaddrs: stringArrayValue(
      profile.listen_multiaddrs,
      `${fieldName}.listen_multiaddrs`,
    ),
    announce_multiaddrs: announceMultiaddrs,
    protocols: [...DA_PUBLIC_RETAINED_DA_PROTOCOLS],
    limits: {
      max_streams_per_peer: maxStreamsPerPeer,
      max_inflight_requests: maxInflightRequests,
      max_inflight_requests_per_peer: maxInflightRequestsPerPeer,
      max_inflight_proof_requests: maxInflightProofRequests,
      request_timeout_ms: requestTimeoutMs,
    },
  };
};

export const parseDaLibp2pRuntimeManifest = (
  value: unknown,
): DaLibp2pRuntimeManifest => {
  const fieldName = "DA libp2p runtime manifest";
  const manifest = recordValue(value, fieldName);
  exactRecordKeys(
    manifest,
    [
      "schemaVersion",
      "network",
      "deployment",
      "runtime_topology",
      "da_transport",
      "public_retained_da",
      "da_committee",
    ],
    fieldName,
  );
  if (manifest.schemaVersion !== DA_RUNTIME_MANIFEST_SCHEMA_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.schemaVersion must be ${DA_RUNTIME_MANIFEST_SCHEMA_VERSION}`,
      String(manifest.schemaVersion),
    );
  }
  const publicRetainedDa = parsePublicRetainedDaProfile(
    manifest.public_retained_da,
    `${fieldName}.public_retained_da`,
  );
  const daCommittee = parseDaLibp2pRuntimeCommittee(
    manifest.da_committee,
    `${fieldName}.da_committee`,
  );
  const topology = parseDaLibp2pRuntimeTopology(
    manifest.runtime_topology,
    `${fieldName}.runtime_topology`,
  );
  if (
    publicRetainedDa.peer_id === topology.producer_peer_id ||
    daCommittee.members.some(
      (member) => member.peer_id === publicRetainedDa.peer_id,
    )
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.public_retained_da.peer_id must not be a producer or committee peer identity`,
    );
  }
  return {
    schemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
    network: nonEmptyStringValue(manifest.network, `${fieldName}.network`),
    deployment: parseDaLibp2pRuntimeManifestDeployment(
      manifest.deployment,
      `${fieldName}.deployment`,
    ),
    runtime_topology: topology,
    da_transport: parseDaLibp2pRuntimeTransport(
      manifest.da_transport,
      `${fieldName}.da_transport`,
    ),
    public_retained_da: publicRetainedDa,
    da_committee: daCommittee,
  };
};

export const computeDaSha256Hash = (value: Uint8Array): Buffer =>
  Buffer.from(sha256(value));

export const encodeDaAttestationPreimage = (headerHash: Uint8Array): Buffer =>
  Buffer.concat([
    Buffer.from(DA_ON_CHAIN_ATTESTATION_DOMAIN, "utf8"),
    ensureDaHeaderHash(headerHash),
  ]);

export const daGossipTopic = (
  deploymentFingerprint: string | Uint8Array,
  topic: DaGossipTopic,
): string =>
  `/midgard/${normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  )}/da/${exactStringEnumValue(
    DaGossipTopic,
    topic,
    "gossip_topic",
  )}/${DA_TRANSPORT_PROTOCOL_VERSION}`;

export const daRequestResponseProtocolId = (
  deploymentFingerprint: string | Uint8Array,
  protocol: DaRequestResponseProtocol,
): string =>
  `/midgard/${normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  )}/da/${exactStringEnumValue(
    DaRequestResponseProtocol,
    protocol,
    "request_response_protocol",
  )}/${DA_TRANSPORT_PROTOCOL_VERSION}`;

const hashValue = (value: unknown, fieldName: string): Buffer =>
  ensureDaHash32(asBytes(value, fieldName), fieldName);

const payloadHashValue = (value: unknown, fieldName: string): Buffer =>
  ensureDaPayloadHash(asBytes(value, fieldName), fieldName);

const optionalPayloadHashValue = (
  value: unknown,
  fieldName: string,
): Buffer | null => (value == null ? null : payloadHashValue(value, fieldName));

const headerHashValue = (value: unknown, fieldName: string): Buffer =>
  ensureDaHeaderHash(asBytes(value, fieldName), fieldName);

const deploymentFingerprintValue = (
  value: unknown,
  fieldName: string,
): Buffer =>
  ensureDaDeploymentFingerprint(asBytes(value, fieldName), fieldName);

const cborEnum = <T extends NumericEnumTable>(
  table: T,
  label: NumericEnumLabel<T>,
  fieldName: string,
): bigint => BigInt(enumCode(table, label, fieldName));

const decodedEnum = <T extends NumericEnumTable>(
  table: T,
  value: unknown,
  fieldName: string,
): NumericEnumLabel<T> =>
  enumLabel(table, ensureUint(value, fieldName), fieldName);

const cborOptionalEnum = <T extends NumericEnumTable>(
  table: T,
  label: NumericEnumLabel<T> | null,
  fieldName: string,
): bigint | null => (label == null ? null : cborEnum(table, label, fieldName));

const decodedOptionalEnum = <T extends NumericEnumTable>(
  table: T,
  value: unknown,
  fieldName: string,
): NumericEnumLabel<T> | null =>
  value == null ? null : decodedEnum(table, value, fieldName);

const hashArrayValue = (value: unknown, fieldName: string): Buffer[] => {
  const items = asArray(value, fieldName);
  return items.map((item, index) => hashValue(item, `${fieldName}[${index}]`));
};

const optionalHashArrayValue = (
  value: unknown,
  fieldName: string,
): Buffer[] | null => (value == null ? null : hashArrayValue(value, fieldName));

const sortedUintArrayValue = (value: unknown, fieldName: string): number[] => {
  const result = asArray(value, fieldName).map((item, index) =>
    ensureUint(item, `${fieldName}[${index}]`),
  );
  for (let index = 1; index < result.length; index += 1) {
    if (result[index - 1]! >= result[index]!) {
      fail(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `${fieldName} must be strictly increasing`,
      );
    }
  }
  return result;
};

const cborSortedUintArray = (
  value: readonly number[],
  fieldName: string,
): bigint[] => {
  const normalized = value.map((item, index) =>
    ensureUint(item, `${fieldName}[${index}]`),
  );
  for (let index = 1; index < normalized.length; index += 1) {
    if (normalized[index - 1]! >= normalized[index]!) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName} must be strictly increasing`,
      );
    }
  }
  return normalized.map(BigInt);
};

const signerIndexArrayValue = (value: unknown, fieldName: string): number[] => {
  const result = asArray(value, fieldName).map((item, index) =>
    ensureUint8(item, `${fieldName}[${index}]`),
  );
  for (let index = 1; index < result.length; index += 1) {
    if (result[index - 1]! >= result[index]!) {
      fail(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `${fieldName} must be strictly increasing`,
      );
    }
  }
  return result;
};

const cborSignerIndexArray = (
  value: readonly number[],
  fieldName: string,
): bigint[] => signerIndexArrayValue(value, fieldName).map(BigInt);

const exactDaPayloadSchemaVersions = (
  value: unknown,
  fieldName: string,
): readonly [typeof DA_PAYLOAD_INNER_SCHEMA_VERSION] => {
  const versions = sortedUintArrayValue(value, fieldName);
  if (
    versions.length !== 1 ||
    versions[0] !== DA_PAYLOAD_INNER_SCHEMA_VERSION
  ) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain exactly DA payload schema V1`,
    );
  }
  return [DA_PAYLOAD_INNER_SCHEMA_VERSION];
};

const exactDaEnvelopeContentEncodings = (
  value: unknown,
  fieldName: string,
): readonly number[] => {
  const encodings = sortedUintArrayValue(value, fieldName);
  const supported = new Set<number>(Object.values(DaPayloadContentEncoding));
  if (
    encodings.length === 0 ||
    encodings.some((encoding) => !supported.has(encoding))
  ) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain only DA envelope V1 content encodings`,
    );
  }
  return encodings;
};

const payloadAnnouncementSignatureValue = (
  value: unknown,
  fieldName: string,
): Buffer => {
  const signature = bytesValue(value, fieldName);
  if (
    signature.length !== 0 &&
    signature.length !== DA_GOSSIP_SIGNATURE_LENGTH
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be 64 bytes or empty only for the signing preimage`,
    );
  }
  return signature;
};

const encodePayloadChunkManifestValue = (
  manifest: DaPayloadChunkManifest,
): unknown[] => [
  ensureDaPayloadHash(manifest.payloadHash, "chunk_manifest.payload_hash"),
  cborUint(manifest.totalBytes, "chunk_manifest.total_bytes"),
  cborUint(manifest.chunkSize, "chunk_manifest.chunk_size"),
  manifest.chunkHashes.map((hash, index) =>
    ensureDaHash32(hash, `chunk_manifest.chunk_hashes[${index}]`),
  ),
];

const decodePayloadChunkManifestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadChunkManifest => {
  const v = fixedArray(value, 4, fieldName);
  return {
    payloadHash: payloadHashValue(v[0], `${fieldName}.payload_hash`),
    totalBytes: ensureUint(v[1], `${fieldName}.total_bytes`),
    chunkSize: ensureUint(v[2], `${fieldName}.chunk_size`),
    chunkHashes: hashArrayValue(v[3], `${fieldName}.chunk_hashes`),
  };
};

const optionalPayloadChunkManifestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadChunkManifest | null =>
  value == null ? null : decodePayloadChunkManifestValue(value, fieldName);

const cborOptionalPayloadChunkManifest = (
  value: DaPayloadChunkManifest | null,
): unknown[] | null =>
  value == null ? null : encodePayloadChunkManifestValue(value);

const decodeTupleCbor = <T>(
  bytes: Uint8Array,
  fieldName: string,
  decodeValue: (value: unknown, fieldName: string) => T,
): T => {
  // decodeSingleCbor performs a complete canonical framing/trailing-byte pass
  // before schema decoding. Re-encoding schema-validated transport messages is
  // redundant and copies inline payload bodies at full DA scale.
  return decodeValue(decodeSingleCbor(bytes), fieldName);
};

export const encodeDaPayloadChunkManifestCbor = (
  manifest: DaPayloadChunkManifest,
): Buffer => encodeCbor(encodePayloadChunkManifestValue(manifest));

export const decodeDaPayloadChunkManifestCbor = (
  bytes: Uint8Array,
): DaPayloadChunkManifest =>
  decodeTupleCbor(
    bytes,
    "PayloadChunkManifestV1",
    decodePayloadChunkManifestValue,
  );

const encodePayloadAnnouncementValue = (
  message: DaPayloadAnnouncement,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  BigInt(
    ensureDaPayloadSchema(
      message.payloadSchemaVersion,
      "payload_schema_version",
    ),
  ),
  cborUint(message.payloadBytes, "payload_bytes"),
  cborUint(message.chunkSize, "chunk_size"),
  cborUint(message.chunkCount, "chunk_count"),
  ensureDaHash32(message.rootSummaryHash, "root_summary_hash"),
  nonEmptyStringValue(message.announcedByPeerId, "announced_by_peer_id"),
  cborUint(message.announcedAtSlot, "announced_at_slot"),
  payloadAnnouncementSignatureValue(message.signature, "signature"),
];

const decodePayloadAnnouncementValue = (
  value: unknown,
  fieldName: string,
): DaPayloadAnnouncement => {
  const v = fixedArray(value, 11, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadSchemaVersion: ensureDaPayloadSchema(
      v[3],
      `${fieldName}.payload_schema_version`,
    ),
    payloadBytes: ensureUint(v[4], `${fieldName}.payload_bytes`),
    chunkSize: ensureUint(v[5], `${fieldName}.chunk_size`),
    chunkCount: ensureUint(v[6], `${fieldName}.chunk_count`),
    rootSummaryHash: hashValue(v[7], `${fieldName}.root_summary_hash`),
    announcedByPeerId: nonEmptyStringValue(
      v[8],
      `${fieldName}.announced_by_peer_id`,
    ),
    announcedAtSlot: ensureUint(v[9], `${fieldName}.announced_at_slot`),
    signature: ensureByteLength(
      asBytes(v[10], `${fieldName}.signature`),
      DA_GOSSIP_SIGNATURE_LENGTH,
      `${fieldName}.signature`,
    ),
  };
};

export const encodeDaPayloadAnnouncementCbor = (
  message: DaPayloadAnnouncement,
): Buffer => encodeCbor(encodePayloadAnnouncementValue(message));

export const decodeDaPayloadAnnouncementCbor = (
  bytes: Uint8Array,
): DaPayloadAnnouncement =>
  decodeTupleCbor(
    bytes,
    "DaPayloadAnnouncementV1",
    decodePayloadAnnouncementValue,
  );

const encodePayloadSubmitRequestValue = (
  message: DaPayloadSubmitRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  BigInt(
    ensureDaPayloadSchema(
      message.payloadSchemaVersion,
      "payload_schema_version",
    ),
  ),
  cborEnum(DaPayloadSubmitMode, message.mode, "mode"),
  message.payloadBytes == null
    ? null
    : bytesValue(message.payloadBytes, "payload_bytes"),
  cborOptionalPayloadChunkManifest(message.chunkManifest),
];

const decodePayloadSubmitRequestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadSubmitRequest => {
  const v = fixedArray(value, 7, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadSchemaVersion: ensureDaPayloadSchema(
      v[3],
      `${fieldName}.payload_schema_version`,
    ),
    mode: decodedEnum(DaPayloadSubmitMode, v[4], `${fieldName}.mode`),
    payloadBytes: optionalBytesValue(v[5], `${fieldName}.payload_bytes`),
    chunkManifest: optionalPayloadChunkManifestValue(
      v[6],
      `${fieldName}.chunk_manifest`,
    ),
  };
};

export const encodeDaPayloadSubmitRequestCbor = (
  message: DaPayloadSubmitRequest,
): Buffer => encodeCbor(encodePayloadSubmitRequestValue(message));

export const decodeDaPayloadSubmitRequestCbor = (
  bytes: Uint8Array,
  timing: DaTransportTimingOptions = {},
): DaPayloadSubmitRequest => {
  const startedAt = readTransportTimingNow(timing);
  try {
    return decodeTupleCbor(
      bytes,
      "PayloadSubmitRequestV1",
      decodePayloadSubmitRequestValue,
    );
  } finally {
    const completedAt = readTransportTimingNow(timing);
    try {
      if (startedAt !== undefined && completedAt !== undefined) {
        timing.onStageTiming?.(
          "submit_request_decode",
          completedAt - startedAt,
        );
      }
    } catch {
      // Observability must not change transport acceptance semantics.
    }
  }
};

const readTransportTimingNow = (
  timing: DaTransportTimingOptions,
): number | undefined => {
  try {
    return (timing.monotonicNow ?? (() => performance.now()))();
  } catch {
    return undefined;
  }
};

const encodePayloadSubmitResponseValue = (
  message: DaPayloadSubmitResponse,
): unknown[] => [
  cborEnum(DaPayloadSubmitStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  message.reasonCode == null
    ? null
    : stringValue(message.reasonCode, "reason_code"),
  cborOptionalUint(message.retryAfterMs, "retry_after_ms"),
];

const decodePayloadSubmitResponseValue = (
  value: unknown,
  fieldName: string,
): DaPayloadSubmitResponse => {
  const v = fixedArray(value, 5, fieldName);
  return {
    status: decodedEnum(DaPayloadSubmitStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    reasonCode: optionalStringValue(v[3], `${fieldName}.reason_code`),
    retryAfterMs:
      v[4] == null ? null : ensureUint(v[4], `${fieldName}.retry_after_ms`),
  };
};

export const encodeDaPayloadSubmitResponseCbor = (
  message: DaPayloadSubmitResponse,
): Buffer => encodeCbor(encodePayloadSubmitResponseValue(message));

export const decodeDaPayloadSubmitResponseCbor = (
  bytes: Uint8Array,
): DaPayloadSubmitResponse =>
  decodeTupleCbor(
    bytes,
    "PayloadSubmitResponseV1",
    decodePayloadSubmitResponseValue,
  );

const encodePayloadByHeaderRequestValue = (
  message: DaPayloadByHeaderRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  message.acceptedPayloadHashes == null
    ? null
    : message.acceptedPayloadHashes.map((hash, index) =>
        ensureDaPayloadHash(hash, `accepted_payload_hashes[${index}]`),
      ),
  cborUint(message.maxInlineBytes, "max_inline_bytes"),
];

const decodePayloadByHeaderRequestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadByHeaderRequest => {
  const v = fixedArray(value, 4, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    acceptedPayloadHashes: optionalHashArrayValue(
      v[2],
      `${fieldName}.accepted_payload_hashes`,
    ),
    maxInlineBytes: ensureUint(v[3], `${fieldName}.max_inline_bytes`),
  };
};

export const encodeDaPayloadByHeaderRequestCbor = (
  message: DaPayloadByHeaderRequest,
): Buffer => encodeCbor(encodePayloadByHeaderRequestValue(message));

export const decodeDaPayloadByHeaderRequestCbor = (
  bytes: Uint8Array,
): DaPayloadByHeaderRequest =>
  decodeTupleCbor(
    bytes,
    "PayloadByHeaderRequestV1",
    decodePayloadByHeaderRequestValue,
  );

const encodePayloadByHeaderResponseValue = (
  message: DaPayloadByHeaderResponse,
): unknown[] => [
  cborEnum(DaPayloadByHeaderStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  message.payloadHash == null
    ? null
    : ensureDaPayloadHash(message.payloadHash, "payload_hash"),
  message.payloadBytes == null
    ? null
    : bytesValue(message.payloadBytes, "payload_bytes"),
  cborOptionalPayloadChunkManifest(message.chunkManifest),
  message.reasonCode == null
    ? null
    : stringValue(message.reasonCode, "reason_code"),
];

const decodePayloadByHeaderResponseValue = (
  value: unknown,
  fieldName: string,
): DaPayloadByHeaderResponse => {
  const v = fixedArray(value, 6, fieldName);
  return {
    status: decodedEnum(DaPayloadByHeaderStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: optionalPayloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadBytes: optionalBytesValue(v[3], `${fieldName}.payload_bytes`),
    chunkManifest: optionalPayloadChunkManifestValue(
      v[4],
      `${fieldName}.chunk_manifest`,
    ),
    reasonCode: optionalStringValue(v[5], `${fieldName}.reason_code`),
  };
};

export const encodeDaPayloadByHeaderResponseCbor = (
  message: DaPayloadByHeaderResponse,
): Buffer => encodeCbor(encodePayloadByHeaderResponseValue(message));

export const decodeDaPayloadByHeaderResponseCbor = (
  bytes: Uint8Array,
): DaPayloadByHeaderResponse =>
  decodeTupleCbor(
    bytes,
    "PayloadByHeaderResponseV1",
    decodePayloadByHeaderResponseValue,
  );

const encodePayloadChunkRequestValue = (
  message: DaPayloadChunkRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  cborUint(message.chunkIndex, "chunk_index"),
];

const decodePayloadChunkRequestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadChunkRequest => {
  const v = fixedArray(value, 4, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    chunkIndex: ensureUint(v[3], `${fieldName}.chunk_index`),
  };
};

export const encodeDaPayloadChunkRequestCbor = (
  message: DaPayloadChunkRequest,
): Buffer => encodeCbor(encodePayloadChunkRequestValue(message));

export const decodeDaPayloadChunkRequestCbor = (
  bytes: Uint8Array,
): DaPayloadChunkRequest =>
  decodeTupleCbor(
    bytes,
    "PayloadChunkRequestV1",
    decodePayloadChunkRequestValue,
  );

const encodePayloadChunkResponseValue = (
  message: DaPayloadChunkResponse,
): unknown[] => [
  cborEnum(DaGenericFoundStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  cborUint(message.chunkIndex, "chunk_index"),
  message.chunkBytes == null
    ? null
    : bytesValue(message.chunkBytes, "chunk_bytes"),
  message.chunkHash == null
    ? null
    : ensureDaHash32(message.chunkHash, "chunk_hash"),
];

const decodePayloadChunkResponseValue = (
  value: unknown,
  fieldName: string,
): DaPayloadChunkResponse => {
  const v = fixedArray(value, 6, fieldName);
  return {
    status: decodedEnum(DaGenericFoundStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    chunkIndex: ensureUint(v[3], `${fieldName}.chunk_index`),
    chunkBytes: optionalBytesValue(v[4], `${fieldName}.chunk_bytes`),
    chunkHash: v[5] == null ? null : hashValue(v[5], `${fieldName}.chunk_hash`),
  };
};

export const encodeDaPayloadChunkResponseCbor = (
  message: DaPayloadChunkResponse,
): Buffer => encodeCbor(encodePayloadChunkResponseValue(message));

export const decodeDaPayloadChunkResponseCbor = (
  bytes: Uint8Array,
): DaPayloadChunkResponse =>
  decodeTupleCbor(
    bytes,
    "PayloadChunkResponseV1",
    decodePayloadChunkResponseValue,
  );

const encodeMetadataByHeaderResponseValue = (
  message: DaMetadataByHeaderResponse,
): unknown[] => [
  cborEnum(DaMetadataStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  message.payloadHash == null
    ? null
    : ensureDaPayloadHash(message.payloadHash, "payload_hash"),
  message.payloadSchemaVersion == null
    ? null
    : BigInt(
        ensureDaPayloadSchema(
          message.payloadSchemaVersion,
          "payload_schema_version",
        ),
      ),
  cborOptionalUint(message.payloadBytes, "payload_bytes"),
  message.rootSummaryHash == null
    ? null
    : ensureDaHash32(message.rootSummaryHash, "root_summary_hash"),
  message.proofBundleHash == null
    ? null
    : ensureDaHash32(message.proofBundleHash, "proof_bundle_hash"),
  message.transitionTraceRoot == null
    ? null
    : bytesValue(message.transitionTraceRoot, "transition_trace_root"),
  message.eventToStepRoot == null
    ? null
    : bytesValue(message.eventToStepRoot, "event_to_step_root"),
  cborOptionalUint(message.retainedUntilSlot, "retained_until_slot"),
  cborOptionalEnum(DaLocalPayloadStatus, message.localStatus, "local_status"),
];

const decodeMetadataByHeaderResponseValue = (
  value: unknown,
  fieldName: string,
): DaMetadataByHeaderResponse => {
  const v = fixedArray(value, 11, fieldName);
  return {
    status: decodedEnum(DaMetadataStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: optionalPayloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadSchemaVersion:
      v[3] == null
        ? null
        : ensureDaPayloadSchema(v[3], `${fieldName}.payload_schema_version`),
    payloadBytes:
      v[4] == null ? null : ensureUint(v[4], `${fieldName}.payload_bytes`),
    rootSummaryHash:
      v[5] == null ? null : hashValue(v[5], `${fieldName}.root_summary_hash`),
    proofBundleHash:
      v[6] == null ? null : hashValue(v[6], `${fieldName}.proof_bundle_hash`),
    transitionTraceRoot: optionalBytesValue(
      v[7],
      `${fieldName}.transition_trace_root`,
    ),
    eventToStepRoot: optionalBytesValue(
      v[8],
      `${fieldName}.event_to_step_root`,
    ),
    retainedUntilSlot:
      v[9] == null
        ? null
        : ensureUint(v[9], `${fieldName}.retained_until_slot`),
    localStatus: decodedOptionalEnum(
      DaLocalPayloadStatus,
      v[10],
      `${fieldName}.local_status`,
    ),
  };
};

export const encodeDaMetadataByHeaderResponseCbor = (
  message: DaMetadataByHeaderResponse,
): Buffer => encodeCbor(encodeMetadataByHeaderResponseValue(message));

export const decodeDaMetadataByHeaderResponseCbor = (
  bytes: Uint8Array,
): DaMetadataByHeaderResponse =>
  decodeTupleCbor(
    bytes,
    "MetadataByHeaderResponseV1",
    decodeMetadataByHeaderResponseValue,
  );

const encodeProofBundleByHeaderRequestValue = (
  message: DaProofBundleByHeaderRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  cborUint(message.maxInlineBytes, "max_inline_bytes"),
];

const decodeProofBundleByHeaderRequestValue = (
  value: unknown,
  fieldName: string,
): DaProofBundleByHeaderRequest => {
  const v = fixedArray(value, 3, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    maxInlineBytes: ensureUint(v[2], `${fieldName}.max_inline_bytes`),
  };
};

export const encodeDaProofBundleByHeaderRequestCbor = (
  message: DaProofBundleByHeaderRequest,
): Buffer => encodeCbor(encodeProofBundleByHeaderRequestValue(message));

export const decodeDaProofBundleByHeaderRequestCbor = (
  bytes: Uint8Array,
): DaProofBundleByHeaderRequest =>
  decodeTupleCbor(
    bytes,
    "ProofBundleByHeaderRequestV1",
    decodeProofBundleByHeaderRequestValue,
  );

const encodeProofBundleByHeaderResponseValue = (
  message: DaProofBundleByHeaderResponse,
): unknown[] => [
  cborEnum(DaProofBundleStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  message.proofBundleHash == null
    ? null
    : ensureDaHash32(message.proofBundleHash, "proof_bundle_hash"),
  message.proofBundleBytes == null
    ? null
    : bytesValue(message.proofBundleBytes, "proof_bundle_bytes"),
  cborOptionalPayloadChunkManifest(message.chunkManifest),
  message.reasonCode == null
    ? null
    : stringValue(message.reasonCode, "reason_code"),
];

const decodeProofBundleByHeaderResponseValue = (
  value: unknown,
  fieldName: string,
): DaProofBundleByHeaderResponse => {
  const v = fixedArray(value, 6, fieldName);
  return {
    status: decodedEnum(DaProofBundleStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    proofBundleHash:
      v[2] == null ? null : hashValue(v[2], `${fieldName}.proof_bundle_hash`),
    proofBundleBytes: optionalBytesValue(
      v[3],
      `${fieldName}.proof_bundle_bytes`,
    ),
    chunkManifest: optionalPayloadChunkManifestValue(
      v[4],
      `${fieldName}.chunk_manifest`,
    ),
    reasonCode: optionalStringValue(v[5], `${fieldName}.reason_code`),
  };
};

export const encodeDaProofBundleByHeaderResponseCbor = (
  message: DaProofBundleByHeaderResponse,
): Buffer => encodeCbor(encodeProofBundleByHeaderResponseValue(message));

export const decodeDaProofBundleByHeaderResponseCbor = (
  bytes: Uint8Array,
): DaProofBundleByHeaderResponse =>
  decodeTupleCbor(
    bytes,
    "ProofBundleByHeaderResponseV1",
    decodeProofBundleByHeaderResponseValue,
  );

const encodeTraceStepByIndexRequestValue = (
  message: DaTraceStepByIndexRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  cborUint(message.stepIndex, "step_index"),
];

const decodeTraceStepByIndexRequestValue = (
  value: unknown,
  fieldName: string,
): DaTraceStepByIndexRequest => {
  const v = fixedArray(value, 3, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    stepIndex: ensureUint(v[2], `${fieldName}.step_index`),
  };
};

export const encodeDaTraceStepByIndexRequestCbor = (
  message: DaTraceStepByIndexRequest,
): Buffer => encodeCbor(encodeTraceStepByIndexRequestValue(message));

export const decodeDaTraceStepByIndexRequestCbor = (
  bytes: Uint8Array,
): DaTraceStepByIndexRequest =>
  decodeTupleCbor(
    bytes,
    "TraceStepByIndexRequestV1",
    decodeTraceStepByIndexRequestValue,
  );

const encodeTraceStepByIndexResponseValue = (
  message: DaTraceStepByIndexResponse,
): unknown[] => [
  cborEnum(DaGenericFoundStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  cborUint(message.stepIndex, "step_index"),
  message.transitionStepBytes == null
    ? null
    : bytesValue(message.transitionStepBytes, "transition_step_bytes"),
  message.membershipProofBytes == null
    ? null
    : bytesValue(message.membershipProofBytes, "membership_proof_bytes"),
];

const decodeTraceStepByIndexResponseValue = (
  value: unknown,
  fieldName: string,
): DaTraceStepByIndexResponse => {
  const v = fixedArray(value, 5, fieldName);
  return {
    status: decodedEnum(DaGenericFoundStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    stepIndex: ensureUint(v[2], `${fieldName}.step_index`),
    transitionStepBytes: optionalBytesValue(
      v[3],
      `${fieldName}.transition_step_bytes`,
    ),
    membershipProofBytes: optionalBytesValue(
      v[4],
      `${fieldName}.membership_proof_bytes`,
    ),
  };
};

export const encodeDaTraceStepByIndexResponseCbor = (
  message: DaTraceStepByIndexResponse,
): Buffer => encodeCbor(encodeTraceStepByIndexResponseValue(message));

export const decodeDaTraceStepByIndexResponseCbor = (
  bytes: Uint8Array,
): DaTraceStepByIndexResponse =>
  decodeTupleCbor(
    bytes,
    "TraceStepByIndexResponseV1",
    decodeTraceStepByIndexResponseValue,
  );

const encodeEventToStepByEventRequestValue = (
  message: DaEventToStepByEventRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  bytesValue(message.eventKey, "event_key"),
];

const decodeEventToStepByEventRequestValue = (
  value: unknown,
  fieldName: string,
): DaEventToStepByEventRequest => {
  const v = fixedArray(value, 3, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    eventKey: bytesValue(v[2], `${fieldName}.event_key`),
  };
};

export const encodeDaEventToStepByEventRequestCbor = (
  message: DaEventToStepByEventRequest,
): Buffer => encodeCbor(encodeEventToStepByEventRequestValue(message));

export const decodeDaEventToStepByEventRequestCbor = (
  bytes: Uint8Array,
): DaEventToStepByEventRequest =>
  decodeTupleCbor(
    bytes,
    "EventToStepByEventRequestV1",
    decodeEventToStepByEventRequestValue,
  );

const encodeEventToStepByEventResponseValue = (
  message: DaEventToStepByEventResponse,
): unknown[] => [
  cborEnum(DaGenericFoundStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  bytesValue(message.eventKey, "event_key"),
  message.eventToStepEntryBytes == null
    ? null
    : bytesValue(message.eventToStepEntryBytes, "event_to_step_entry_bytes"),
  message.membershipOrNonmembershipProofBytes == null
    ? null
    : bytesValue(
        message.membershipOrNonmembershipProofBytes,
        "membership_or_nonmembership_proof_bytes",
      ),
];

const decodeEventToStepByEventResponseValue = (
  value: unknown,
  fieldName: string,
): DaEventToStepByEventResponse => {
  const v = fixedArray(value, 5, fieldName);
  return {
    status: decodedEnum(DaGenericFoundStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    eventKey: bytesValue(v[2], `${fieldName}.event_key`),
    eventToStepEntryBytes: optionalBytesValue(
      v[3],
      `${fieldName}.event_to_step_entry_bytes`,
    ),
    membershipOrNonmembershipProofBytes: optionalBytesValue(
      v[4],
      `${fieldName}.membership_or_nonmembership_proof_bytes`,
    ),
  };
};

export const encodeDaEventToStepByEventResponseCbor = (
  message: DaEventToStepByEventResponse,
): Buffer => encodeCbor(encodeEventToStepByEventResponseValue(message));

export const decodeDaEventToStepByEventResponseCbor = (
  bytes: Uint8Array,
): DaEventToStepByEventResponse =>
  decodeTupleCbor(
    bytes,
    "EventToStepByEventResponseV1",
    decodeEventToStepByEventResponseValue,
  );

const encodeAttestationGossipValue = (
  message: DaAttestationGossip,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  ensureNonEmptyBytes(
    message.availabilityCommitmentCbor,
    "availability_commitment_cbor",
  ),
  ensureDaHash32(
    message.availabilityCommitmentDigest,
    "availability_commitment_digest",
  ),
  cborUint(ensureUint8(message.signerIndex, "signer_index"), "signer_index"),
  ensureDaHash32(message.daVkey, "da_vkey"),
  ensureByteLength(
    message.onChainWitness,
    DA_ON_CHAIN_WITNESS_LENGTH,
    "on_chain_witness",
  ),
  cborUint(message.retentionUntilSlot, "retention_until_slot"),
  nonEmptyStringValue(message.announcedByPeerId, "announced_by_peer_id"),
];

const decodeAttestationGossipValue = (
  value: unknown,
  fieldName: string,
): DaAttestationGossip => {
  const v = fixedArray(value, 10, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    availabilityCommitmentCbor: ensureNonEmptyBytes(
      asBytes(v[3], `${fieldName}.availability_commitment_cbor`),
      `${fieldName}.availability_commitment_cbor`,
    ),
    availabilityCommitmentDigest: ensureDaHash32(
      asBytes(v[4], `${fieldName}.availability_commitment_digest`),
      `${fieldName}.availability_commitment_digest`,
    ),
    signerIndex: ensureUint8(v[5], `${fieldName}.signer_index`),
    daVkey: hashValue(v[6], `${fieldName}.da_vkey`),
    onChainWitness: ensureByteLength(
      asBytes(v[7], `${fieldName}.on_chain_witness`),
      DA_ON_CHAIN_WITNESS_LENGTH,
      `${fieldName}.on_chain_witness`,
    ),
    retentionUntilSlot: ensureUint(v[8], `${fieldName}.retention_until_slot`),
    announcedByPeerId: nonEmptyStringValue(
      v[9],
      `${fieldName}.announced_by_peer_id`,
    ),
  };
};

export const encodeDaAttestationGossipCbor = (
  message: DaAttestationGossip,
): Buffer => encodeCbor(encodeAttestationGossipValue(message));

export const decodeDaAttestationGossipCbor = (
  bytes: Uint8Array,
): DaAttestationGossip =>
  decodeTupleCbor(bytes, "DaAttestationGossipV1", decodeAttestationGossipValue);

const encodeAttestationsByHeaderRequestValue = (
  message: DaAttestationsByHeaderRequest,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  message.acceptedSignerIndexes == null
    ? null
    : cborSignerIndexArray(
        message.acceptedSignerIndexes,
        "accepted_signer_indexes",
      ),
  cborOptionalUint(message.maxAttestations, "max_attestations"),
];

const decodeAttestationsByHeaderRequestValue = (
  value: unknown,
  fieldName: string,
): DaAttestationsByHeaderRequest => {
  const v = fixedArray(value, 4, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    acceptedSignerIndexes:
      v[2] == null
        ? null
        : signerIndexArrayValue(v[2], `${fieldName}.accepted_signer_indexes`),
    maxAttestations:
      v[3] == null ? null : ensureUint(v[3], `${fieldName}.max_attestations`),
  };
};

export const encodeDaAttestationsByHeaderRequestCbor = (
  message: DaAttestationsByHeaderRequest,
): Buffer => encodeCbor(encodeAttestationsByHeaderRequestValue(message));

export const decodeDaAttestationsByHeaderRequestCbor = (
  bytes: Uint8Array,
): DaAttestationsByHeaderRequest =>
  decodeTupleCbor(
    bytes,
    "AttestationsByHeaderRequestV1",
    decodeAttestationsByHeaderRequestValue,
  );

const encodeAttestationsByHeaderResponseValue = (
  message: DaAttestationsByHeaderResponse,
): unknown[] => [
  cborEnum(DaGenericFoundStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  message.attestations.map(encodeAttestationGossipValue),
  message.reasonCode == null
    ? null
    : stringValue(message.reasonCode, "reason_code"),
];

const decodeAttestationsByHeaderResponseValue = (
  value: unknown,
  fieldName: string,
): DaAttestationsByHeaderResponse => {
  const v = fixedArray(value, 4, fieldName);
  return {
    status: decodedEnum(DaGenericFoundStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    attestations: asArray(v[2], `${fieldName}.attestations`).map(
      (attestation, index) =>
        decodeAttestationGossipValue(
          attestation,
          `${fieldName}.attestations[${index}]`,
        ),
    ),
    reasonCode: optionalStringValue(v[3], `${fieldName}.reason_code`),
  };
};

export const encodeDaAttestationsByHeaderResponseCbor = (
  message: DaAttestationsByHeaderResponse,
): Buffer => encodeCbor(encodeAttestationsByHeaderResponseValue(message));

export const decodeDaAttestationsByHeaderResponseCbor = (
  bytes: Uint8Array,
): DaAttestationsByHeaderResponse =>
  decodeTupleCbor(
    bytes,
    "AttestationsByHeaderResponseV1",
    decodeAttestationsByHeaderResponseValue,
  );

const validateConflictingSignatureHeaderEvidence = (
  evidence: DaConflictingSignatureHeaderEvidence,
): DaConflictingSignatureHeaderEvidence => {
  const signerIndex = ensureUint8(evidence.signerIndex, "signer_index");
  const daVkey = ensureDaHash32(evidence.daVkey, "da_vkey");
  const lowerHeaderHash = ensureDaHeaderHash(evidence.lowerHeaderHash);
  const upperHeaderHash = ensureDaHeaderHash(evidence.upperHeaderHash);
  const lowerCommitmentCbor = ensureNonEmptyBytes(
    evidence.lowerCommitmentCbor,
    "lower_commitment_cbor",
  );
  const upperCommitmentCbor = ensureNonEmptyBytes(
    evidence.upperCommitmentCbor,
    "upper_commitment_cbor",
  );
  const lowerHeaderWitness = ensureByteLength(
    evidence.lowerHeaderWitness,
    DA_ON_CHAIN_WITNESS_LENGTH,
    "lower_header_witness",
  );
  const upperHeaderWitness = ensureByteLength(
    evidence.upperHeaderWitness,
    DA_ON_CHAIN_WITNESS_LENGTH,
    "upper_header_witness",
  );
  const lowerIdentity = Buffer.concat([
    lowerHeaderHash,
    computeDaSha256Hash(lowerCommitmentCbor),
  ]);
  const upperIdentity = Buffer.concat([
    upperHeaderHash,
    computeDaSha256Hash(upperCommitmentCbor),
  ]);
  if (Buffer.compare(lowerIdentity, upperIdentity) >= 0) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "conflicting signature/commitment evidence identities must be strictly ordered",
    );
  }
  if (
    lowerHeaderWitness[0] !== signerIndex ||
    upperHeaderWitness[0] !== signerIndex
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "conflicting signature/header witnesses must embed signer_index",
    );
  }
  return {
    signerIndex,
    daVkey,
    lowerHeaderHash,
    lowerCommitmentCbor,
    lowerHeaderWitness,
    upperHeaderHash,
    upperCommitmentCbor,
    upperHeaderWitness,
  };
};

const encodeConflictingSignatureHeaderEvidenceValue = (
  evidence: DaConflictingSignatureHeaderEvidence,
): unknown[] => {
  const canonical = validateConflictingSignatureHeaderEvidence(evidence);
  return [
    cborUint(canonical.signerIndex, "signer_index"),
    canonical.daVkey,
    canonical.lowerHeaderHash,
    canonical.lowerCommitmentCbor,
    canonical.lowerHeaderWitness,
    canonical.upperHeaderHash,
    canonical.upperCommitmentCbor,
    canonical.upperHeaderWitness,
  ];
};

const decodeConflictingSignatureHeaderEvidenceValue = (
  value: unknown,
  fieldName: string,
): DaConflictingSignatureHeaderEvidence => {
  const tuple = fixedArray(value, 8, fieldName);
  return validateConflictingSignatureHeaderEvidence({
    signerIndex: ensureUint8(tuple[0], `${fieldName}.signer_index`),
    daVkey: hashValue(tuple[1], `${fieldName}.da_vkey`),
    lowerHeaderHash: headerHashValue(
      tuple[2],
      `${fieldName}.lower_header_hash`,
    ),
    lowerCommitmentCbor: asBytes(
      tuple[3],
      `${fieldName}.lower_commitment_cbor`,
    ),
    lowerHeaderWitness: ensureByteLength(
      asBytes(tuple[4], `${fieldName}.lower_header_witness`),
      DA_ON_CHAIN_WITNESS_LENGTH,
      `${fieldName}.lower_header_witness`,
    ),
    upperHeaderHash: headerHashValue(
      tuple[5],
      `${fieldName}.upper_header_hash`,
    ),
    upperCommitmentCbor: asBytes(
      tuple[6],
      `${fieldName}.upper_commitment_cbor`,
    ),
    upperHeaderWitness: ensureByteLength(
      asBytes(tuple[7], `${fieldName}.upper_header_witness`),
      DA_ON_CHAIN_WITNESS_LENGTH,
      `${fieldName}.upper_header_witness`,
    ),
  });
};

export const encodeDaConflictingSignatureHeaderEvidenceCbor = (
  evidence: DaConflictingSignatureHeaderEvidence,
): Buffer =>
  encodeCbor(encodeConflictingSignatureHeaderEvidenceValue(evidence));

export const decodeDaConflictingSignatureHeaderEvidenceCbor = (
  bytes: Uint8Array,
): DaConflictingSignatureHeaderEvidence =>
  decodeTupleCbor(
    bytes,
    "DaConflictingSignatureHeaderEvidenceV1",
    decodeConflictingSignatureHeaderEvidenceValue,
  );

const encodeConflictEvidenceValue = (
  message: DaConflictEvidence,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  cborEnum(DaConflictEvidenceKind, message.evidenceKind, "evidence_kind"),
  ensureDaHash32(message.evidenceHash, "evidence_hash"),
  message.compactEvidence == null
    ? null
    : bytesValue(message.compactEvidence, "compact_evidence"),
];

const decodeConflictEvidenceValue = (
  value: unknown,
  fieldName: string,
): DaConflictEvidence => {
  const v = fixedArray(value, 5, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    evidenceKind: decodedEnum(
      DaConflictEvidenceKind,
      v[2],
      `${fieldName}.evidence_kind`,
    ),
    evidenceHash: hashValue(v[3], `${fieldName}.evidence_hash`),
    compactEvidence: optionalBytesValue(v[4], `${fieldName}.compact_evidence`),
  };
};

export const encodeDaConflictEvidenceCbor = (
  message: DaConflictEvidence,
): Buffer => encodeCbor(encodeConflictEvidenceValue(message));

export const decodeDaConflictEvidenceCbor = (
  bytes: Uint8Array,
): DaConflictEvidence =>
  decodeTupleCbor(bytes, "ConflictEvidenceV1", decodeConflictEvidenceValue);

const encodeCapabilitiesRequestValue = (
  message: DaCapabilitiesRequest,
): unknown[] => [ensureDaDeploymentFingerprint(message.deploymentFingerprint)];

const decodeCapabilitiesRequestValue = (
  value: unknown,
  fieldName: string,
): DaCapabilitiesRequest => {
  const fields = fixedArray(value, 1, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      fields[0],
      `${fieldName}.deployment_fingerprint`,
    ),
  };
};

export const encodeDaCapabilitiesRequestCbor = (
  message: DaCapabilitiesRequest,
): Buffer => encodeCbor(encodeCapabilitiesRequestValue(message));

export const decodeDaCapabilitiesRequestCbor = (
  bytes: Uint8Array,
): DaCapabilitiesRequest =>
  decodeTupleCbor(
    bytes,
    "DaCapabilitiesRequestV1",
    decodeCapabilitiesRequestValue,
  );

const encodeCapabilitiesResponseValue = (
  message: DaCapabilitiesResponse,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  BigInt(
    ensureDaTransport(
      message.transportProtocolVersion,
      "transport_protocol_version",
    ),
  ),
  cborSortedUintArray(
    exactDaPayloadSchemaVersions(
      message.payloadSchemaVersions,
      "payload_schema_versions",
    ),
    "payload_schema_versions",
  ),
  cborSortedUintArray(
    exactDaEnvelopeContentEncodings(
      message.envelopeContentEncodings,
      "envelope_content_encodings",
    ),
    "envelope_content_encodings",
  ),
  cborUint(message.maxPayloadBytes, "max_payload_bytes"),
  cborUint(message.maxInlineResponseBytes, "max_inline_response_bytes"),
  cborUint(message.maxChunkBytes, "max_chunk_bytes"),
  cborUint(message.maxStreamsPerPeer, "max_streams_per_peer"),
  cborUint(message.requestTimeoutMs, "request_timeout_ms"),
];

const decodeCapabilitiesResponseValue = (
  value: unknown,
  fieldName: string,
): DaCapabilitiesResponse => {
  const fields = fixedArray(value, 9, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      fields[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    transportProtocolVersion: ensureDaTransport(
      fields[1],
      `${fieldName}.transport_protocol_version`,
    ),
    payloadSchemaVersions: exactDaPayloadSchemaVersions(
      fields[2],
      `${fieldName}.payload_schema_versions`,
    ),
    envelopeContentEncodings: exactDaEnvelopeContentEncodings(
      fields[3],
      `${fieldName}.envelope_content_encodings`,
    ),
    maxPayloadBytes: ensureUint(fields[4], `${fieldName}.max_payload_bytes`),
    maxInlineResponseBytes: ensureUint(
      fields[5],
      `${fieldName}.max_inline_response_bytes`,
    ),
    maxChunkBytes: ensureUint(fields[6], `${fieldName}.max_chunk_bytes`),
    maxStreamsPerPeer: ensureUint(
      fields[7],
      `${fieldName}.max_streams_per_peer`,
    ),
    requestTimeoutMs: ensureUint(fields[8], `${fieldName}.request_timeout_ms`),
  };
};

export const encodeDaCapabilitiesResponseCbor = (
  message: DaCapabilitiesResponse,
): Buffer => encodeCbor(encodeCapabilitiesResponseValue(message));

export const decodeDaCapabilitiesResponseCbor = (
  bytes: Uint8Array,
): DaCapabilitiesResponse =>
  decodeTupleCbor(
    bytes,
    "DaCapabilitiesResponseV1",
    decodeCapabilitiesResponseValue,
  );
