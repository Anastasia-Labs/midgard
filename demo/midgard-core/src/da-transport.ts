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
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "./consensus-profile-v1.js";
import { DA_PAYLOAD_INNER_V1_SCHEMA_VERSION } from "./da-payload-envelope.js";

export const DA_TRANSPORT_V1_PROTOCOL_VERSION = 1 as const;
export const DA_DEPLOYMENT_FINGERPRINT_LENGTH = 32;
export const DA_HEADER_HASH_LENGTH = 28;
export const DA_HASH_LENGTH = 32;
export const DA_ON_CHAIN_WITNESS_LENGTH = 65;
export const DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION =
  "midgard-da-libp2p-runtime-manifest-v1";
export const DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE =
  "contract_deployment_manifest_id";

export const DA_TRANSPORT_LIMITS_V1 = {
  maxPayloadBytes: MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes,
  maxInlineResponseBytes: 1_048_576,
  maxChunkBytes: 1_048_576,
  maxGossipMessageBytes: 65_536,
  maxStreamsPerPeer: 16,
  requestTimeoutMs: 15_000,
  minimumRetentionDays: 15,
} as const;

export const DA_ON_CHAIN_ATTESTATION_V1_DOMAIN = "MidgardDAAttestationV1";

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

export type DaLibp2pRuntimeManifestDeploymentIdentity = {
  readonly fingerprint: string;
  readonly contractDeploymentManifestId: string;
  readonly contractDeploymentInfoSha256: string;
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

export type DaPayloadChunkManifestV1 = {
  readonly payloadHash: Buffer;
  readonly totalBytes: number;
  readonly chunkSize: number;
  readonly chunkHashes: readonly Buffer[];
};

export type DaPayloadAnnouncementV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly payloadSchemaVersion: typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION;
  readonly payloadBytes: number;
  readonly chunkSize: number;
  readonly chunkCount: number;
  readonly rootSummaryHash: Buffer;
  readonly announcedByPeerId: string;
  readonly announcedAtSlot: number;
  readonly signature: Buffer;
};

export type DaPayloadSubmitRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly payloadSchemaVersion: typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION;
  readonly mode: DaPayloadSubmitMode;
  readonly payloadBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifestV1 | null;
};

export type DaPayloadSubmitResponseV1 = {
  readonly status: DaPayloadSubmitStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly reasonCode: string | null;
  readonly retryAfterMs: number | null;
};

export type DaCapabilitiesRequestV1 = {
  readonly deploymentFingerprint: Buffer;
};

export type DaCapabilitiesResponseV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly transportProtocolVersion: typeof DA_TRANSPORT_V1_PROTOCOL_VERSION;
  readonly payloadSchemaVersions: readonly [
    typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION,
  ];
  readonly envelopeContentEncodings: readonly number[];
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly requestTimeoutMs: number;
};

export type DaPayloadByHeaderRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly acceptedPayloadHashes: readonly Buffer[] | null;
  readonly maxInlineBytes: number;
};

export type DaPayloadByHeaderResponseV1 = {
  readonly status: DaPayloadByHeaderStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer | null;
  readonly payloadBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifestV1 | null;
  readonly reasonCode: string | null;
};

export type DaPayloadChunkRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly chunkIndex: number;
};

export type DaPayloadChunkResponseV1 = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly chunkIndex: number;
  readonly chunkBytes: Buffer | null;
  readonly chunkHash: Buffer | null;
};

export type DaMetadataByHeaderResponseV1 = {
  readonly status: DaMetadataStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer | null;
  readonly payloadSchemaVersion:
    | typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION
    | null;
  readonly payloadBytes: number | null;
  readonly rootSummaryHash: Buffer | null;
  readonly proofBundleHash: Buffer | null;
  readonly transitionTraceRoot: Buffer | null;
  readonly eventToStepRoot: Buffer | null;
  readonly retainedUntilSlot: number | null;
  readonly localStatus: DaLocalPayloadStatus | null;
};

export type DaProofBundleByHeaderRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly maxInlineBytes: number;
};

export type DaProofBundleByHeaderResponseV1 = {
  readonly status: DaProofBundleStatus;
  readonly headerHash: Buffer;
  readonly proofBundleHash: Buffer | null;
  readonly proofBundleBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifestV1 | null;
  readonly reasonCode: string | null;
};

export type DaTraceStepByIndexRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly stepIndex: number;
};

export type DaTraceStepByIndexResponseV1 = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly stepIndex: number;
  readonly transitionStepBytes: Buffer | null;
  readonly membershipProofBytes: Buffer | null;
};

export type DaEventToStepByEventRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly eventKey: Buffer;
};

export type DaEventToStepByEventResponseV1 = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly eventKey: Buffer;
  readonly eventToStepEntryBytes: Buffer | null;
  readonly membershipOrNonmembershipProofBytes: Buffer | null;
};

export type DaAttestationGossipV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly signerIndex: number;
  readonly daVkey: Buffer;
  readonly onChainWitness: Buffer;
  readonly retentionUntilSlot: number;
  readonly announcedByPeerId: string;
};

export type DaAttestationsByHeaderRequestV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly acceptedSignerIndexes: readonly number[] | null;
  readonly maxAttestations: number | null;
};

export type DaAttestationsByHeaderResponseV1 = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly attestations: readonly DaAttestationGossipV1[];
  readonly reasonCode: string | null;
};

export type DaConflictEvidenceV1 = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly evidenceKind: DaConflictEvidenceKind;
  readonly evidenceHash: Buffer;
  readonly compactEvidence: Buffer | null;
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

const ensureDaPayloadSchemaV1 = (
  value: unknown,
  fieldName: string,
): typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION => {
  const version = ensureUint(value, fieldName);
  if (version !== DA_PAYLOAD_INNER_V1_SCHEMA_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${DA_PAYLOAD_INNER_V1_SCHEMA_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return DA_PAYLOAD_INNER_V1_SCHEMA_VERSION;
};

const ensureDaTransportV1 = (
  value: unknown,
  fieldName: string,
): typeof DA_TRANSPORT_V1_PROTOCOL_VERSION => {
  const version = ensureUint(value, fieldName);
  if (version !== DA_TRANSPORT_V1_PROTOCOL_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${DA_TRANSPORT_V1_PROTOCOL_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return DA_TRANSPORT_V1_PROTOCOL_VERSION;
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

export const parseDaLibp2pRuntimeManifestDeploymentIdentity = (
  manifest: Record<string, unknown>,
  fieldName = "DA libp2p runtime manifest",
): DaLibp2pRuntimeManifestDeploymentIdentity => {
  if (manifest.schemaVersion !== DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.schemaVersion must be ${DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION}`,
      String(manifest.schemaVersion),
    );
  }
  const deployment = recordValue(
    manifest.deployment,
    `${fieldName}.deployment`,
  );
  exactRecordKeys(
    deployment,
    [
      "fingerprint",
      "contract_deployment_manifest_id",
      "contract_deployment_info_sha256",
      "identity_source",
    ],
    `${fieldName}.deployment`,
  );
  if (
    deployment.identity_source !== DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.deployment.identity_source must be ${DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE}`,
      String(deployment.identity_source),
    );
  }
  const fingerprint = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(
      deployment.fingerprint,
      `${fieldName}.deployment.fingerprint`,
    ),
  );
  const contractDeploymentManifestId = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(
      deployment.contract_deployment_manifest_id,
      `${fieldName}.deployment.contract_deployment_manifest_id`,
    ),
  );
  if (fingerprint !== contractDeploymentManifestId) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.deployment.fingerprint must equal deployment.contract_deployment_manifest_id`,
      `fingerprint=${fingerprint},contract_deployment_manifest_id=${contractDeploymentManifestId}`,
    );
  }
  const contractDeploymentInfoSha256 = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(
      deployment.contract_deployment_info_sha256,
      `${fieldName}.deployment.contract_deployment_info_sha256`,
    ),
  );
  return {
    fingerprint,
    contractDeploymentManifestId,
    contractDeploymentInfoSha256,
  };
};

export const computeDaSha256Hash = (value: Uint8Array): Buffer =>
  Buffer.from(sha256(value));

export const encodeDaAttestationV1Preimage = (headerHash: Uint8Array): Buffer =>
  Buffer.concat([
    Buffer.from(DA_ON_CHAIN_ATTESTATION_V1_DOMAIN, "utf8"),
    ensureDaHeaderHash(headerHash),
  ]);

export const daGossipTopic = (
  deploymentFingerprint: string | Uint8Array,
  topic: DaGossipTopic,
): string =>
  `/midgard/${normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  )}/da/${topic}/${DA_TRANSPORT_V1_PROTOCOL_VERSION}`;

export const daRequestResponseProtocolId = (
  deploymentFingerprint: string | Uint8Array,
  protocol: DaRequestResponseProtocol,
): string =>
  `/midgard/${normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  )}/da/${protocol}/${DA_TRANSPORT_V1_PROTOCOL_VERSION}`;

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

const exactDaPayloadSchemaVersionsV1 = (
  value: unknown,
  fieldName: string,
): readonly [typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION] => {
  const versions = sortedUintArrayValue(value, fieldName);
  if (
    versions.length !== 1 ||
    versions[0] !== DA_PAYLOAD_INNER_V1_SCHEMA_VERSION
  ) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain exactly DA payload schema V1`,
    );
  }
  return [DA_PAYLOAD_INNER_V1_SCHEMA_VERSION];
};

const encodePayloadChunkManifestValue = (
  manifest: DaPayloadChunkManifestV1,
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
): DaPayloadChunkManifestV1 => {
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
): DaPayloadChunkManifestV1 | null =>
  value == null ? null : decodePayloadChunkManifestValue(value, fieldName);

const cborOptionalPayloadChunkManifest = (
  value: DaPayloadChunkManifestV1 | null,
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
  manifest: DaPayloadChunkManifestV1,
): Buffer => encodeCbor(encodePayloadChunkManifestValue(manifest));

export const decodeDaPayloadChunkManifestCbor = (
  bytes: Uint8Array,
): DaPayloadChunkManifestV1 =>
  decodeTupleCbor(
    bytes,
    "PayloadChunkManifestV1",
    decodePayloadChunkManifestValue,
  );

const encodePayloadAnnouncementValue = (
  message: DaPayloadAnnouncementV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  BigInt(
    ensureDaPayloadSchemaV1(
      message.payloadSchemaVersion,
      "payload_schema_version",
    ),
  ),
  cborUint(message.payloadBytes, "payload_bytes"),
  cborUint(message.chunkSize, "chunk_size"),
  cborUint(message.chunkCount, "chunk_count"),
  ensureDaHash32(message.rootSummaryHash, "root_summary_hash"),
  stringValue(message.announcedByPeerId, "announced_by_peer_id"),
  cborUint(message.announcedAtSlot, "announced_at_slot"),
  bytesValue(message.signature, "signature"),
];

const decodePayloadAnnouncementValue = (
  value: unknown,
  fieldName: string,
): DaPayloadAnnouncementV1 => {
  const v = fixedArray(value, 11, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadSchemaVersion: ensureDaPayloadSchemaV1(
      v[3],
      `${fieldName}.payload_schema_version`,
    ),
    payloadBytes: ensureUint(v[4], `${fieldName}.payload_bytes`),
    chunkSize: ensureUint(v[5], `${fieldName}.chunk_size`),
    chunkCount: ensureUint(v[6], `${fieldName}.chunk_count`),
    rootSummaryHash: hashValue(v[7], `${fieldName}.root_summary_hash`),
    announcedByPeerId: stringValue(v[8], `${fieldName}.announced_by_peer_id`),
    announcedAtSlot: ensureUint(v[9], `${fieldName}.announced_at_slot`),
    signature: bytesValue(v[10], `${fieldName}.signature`),
  };
};

export const encodeDaPayloadAnnouncementV1Cbor = (
  message: DaPayloadAnnouncementV1,
): Buffer => encodeCbor(encodePayloadAnnouncementValue(message));

export const decodeDaPayloadAnnouncementV1Cbor = (
  bytes: Uint8Array,
): DaPayloadAnnouncementV1 =>
  decodeTupleCbor(
    bytes,
    "DaPayloadAnnouncementV1",
    decodePayloadAnnouncementValue,
  );

const encodePayloadSubmitRequestValue = (
  message: DaPayloadSubmitRequestV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  BigInt(
    ensureDaPayloadSchemaV1(
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
): DaPayloadSubmitRequestV1 => {
  const v = fixedArray(value, 7, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadSchemaVersion: ensureDaPayloadSchemaV1(
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

export const encodeDaPayloadSubmitRequestV1Cbor = (
  message: DaPayloadSubmitRequestV1,
): Buffer => encodeCbor(encodePayloadSubmitRequestValue(message));

export const decodeDaPayloadSubmitRequestV1Cbor = (
  bytes: Uint8Array,
  timing: DaTransportTimingOptions = {},
): DaPayloadSubmitRequestV1 => {
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
  message: DaPayloadSubmitResponseV1,
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
): DaPayloadSubmitResponseV1 => {
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

export const encodeDaPayloadSubmitResponseV1Cbor = (
  message: DaPayloadSubmitResponseV1,
): Buffer => encodeCbor(encodePayloadSubmitResponseValue(message));

export const decodeDaPayloadSubmitResponseV1Cbor = (
  bytes: Uint8Array,
): DaPayloadSubmitResponseV1 =>
  decodeTupleCbor(
    bytes,
    "PayloadSubmitResponseV1",
    decodePayloadSubmitResponseValue,
  );

const encodePayloadByHeaderRequestValue = (
  message: DaPayloadByHeaderRequestV1,
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
): DaPayloadByHeaderRequestV1 => {
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

export const encodeDaPayloadByHeaderRequestV1Cbor = (
  message: DaPayloadByHeaderRequestV1,
): Buffer => encodeCbor(encodePayloadByHeaderRequestValue(message));

export const decodeDaPayloadByHeaderRequestV1Cbor = (
  bytes: Uint8Array,
): DaPayloadByHeaderRequestV1 =>
  decodeTupleCbor(
    bytes,
    "PayloadByHeaderRequestV1",
    decodePayloadByHeaderRequestValue,
  );

const encodePayloadByHeaderResponseValue = (
  message: DaPayloadByHeaderResponseV1,
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
): DaPayloadByHeaderResponseV1 => {
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

export const encodeDaPayloadByHeaderResponseV1Cbor = (
  message: DaPayloadByHeaderResponseV1,
): Buffer => encodeCbor(encodePayloadByHeaderResponseValue(message));

export const decodeDaPayloadByHeaderResponseV1Cbor = (
  bytes: Uint8Array,
): DaPayloadByHeaderResponseV1 =>
  decodeTupleCbor(
    bytes,
    "PayloadByHeaderResponseV1",
    decodePayloadByHeaderResponseValue,
  );

const encodePayloadChunkRequestValue = (
  message: DaPayloadChunkRequestV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  cborUint(message.chunkIndex, "chunk_index"),
];

const decodePayloadChunkRequestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadChunkRequestV1 => {
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

export const encodeDaPayloadChunkRequestV1Cbor = (
  message: DaPayloadChunkRequestV1,
): Buffer => encodeCbor(encodePayloadChunkRequestValue(message));

export const decodeDaPayloadChunkRequestV1Cbor = (
  bytes: Uint8Array,
): DaPayloadChunkRequestV1 =>
  decodeTupleCbor(
    bytes,
    "PayloadChunkRequestV1",
    decodePayloadChunkRequestValue,
  );

const encodePayloadChunkResponseValue = (
  message: DaPayloadChunkResponseV1,
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
): DaPayloadChunkResponseV1 => {
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

export const encodeDaPayloadChunkResponseV1Cbor = (
  message: DaPayloadChunkResponseV1,
): Buffer => encodeCbor(encodePayloadChunkResponseValue(message));

export const decodeDaPayloadChunkResponseV1Cbor = (
  bytes: Uint8Array,
): DaPayloadChunkResponseV1 =>
  decodeTupleCbor(
    bytes,
    "PayloadChunkResponseV1",
    decodePayloadChunkResponseValue,
  );

const encodeMetadataByHeaderResponseValue = (
  message: DaMetadataByHeaderResponseV1,
): unknown[] => [
  cborEnum(DaMetadataStatus, message.status, "status"),
  ensureDaHeaderHash(message.headerHash),
  message.payloadHash == null
    ? null
    : ensureDaPayloadHash(message.payloadHash, "payload_hash"),
  message.payloadSchemaVersion == null
    ? null
    : BigInt(
        ensureDaPayloadSchemaV1(
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
): DaMetadataByHeaderResponseV1 => {
  const v = fixedArray(value, 11, fieldName);
  return {
    status: decodedEnum(DaMetadataStatus, v[0], `${fieldName}.status`),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: optionalPayloadHashValue(v[2], `${fieldName}.payload_hash`),
    payloadSchemaVersion:
      v[3] == null
        ? null
        : ensureDaPayloadSchemaV1(
            v[3],
            `${fieldName}.payload_schema_version`,
          ),
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

export const encodeDaMetadataByHeaderResponseV1Cbor = (
  message: DaMetadataByHeaderResponseV1,
): Buffer => encodeCbor(encodeMetadataByHeaderResponseValue(message));

export const decodeDaMetadataByHeaderResponseV1Cbor = (
  bytes: Uint8Array,
): DaMetadataByHeaderResponseV1 =>
  decodeTupleCbor(
    bytes,
    "MetadataByHeaderResponseV1",
    decodeMetadataByHeaderResponseValue,
  );

const encodeProofBundleByHeaderRequestValue = (
  message: DaProofBundleByHeaderRequestV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  cborUint(message.maxInlineBytes, "max_inline_bytes"),
];

const decodeProofBundleByHeaderRequestValue = (
  value: unknown,
  fieldName: string,
): DaProofBundleByHeaderRequestV1 => {
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

export const encodeDaProofBundleByHeaderRequestV1Cbor = (
  message: DaProofBundleByHeaderRequestV1,
): Buffer => encodeCbor(encodeProofBundleByHeaderRequestValue(message));

export const decodeDaProofBundleByHeaderRequestV1Cbor = (
  bytes: Uint8Array,
): DaProofBundleByHeaderRequestV1 =>
  decodeTupleCbor(
    bytes,
    "ProofBundleByHeaderRequestV1",
    decodeProofBundleByHeaderRequestValue,
  );

const encodeProofBundleByHeaderResponseValue = (
  message: DaProofBundleByHeaderResponseV1,
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
): DaProofBundleByHeaderResponseV1 => {
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

export const encodeDaProofBundleByHeaderResponseV1Cbor = (
  message: DaProofBundleByHeaderResponseV1,
): Buffer => encodeCbor(encodeProofBundleByHeaderResponseValue(message));

export const decodeDaProofBundleByHeaderResponseV1Cbor = (
  bytes: Uint8Array,
): DaProofBundleByHeaderResponseV1 =>
  decodeTupleCbor(
    bytes,
    "ProofBundleByHeaderResponseV1",
    decodeProofBundleByHeaderResponseValue,
  );

const encodeTraceStepByIndexRequestValue = (
  message: DaTraceStepByIndexRequestV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  cborUint(message.stepIndex, "step_index"),
];

const decodeTraceStepByIndexRequestValue = (
  value: unknown,
  fieldName: string,
): DaTraceStepByIndexRequestV1 => {
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

export const encodeDaTraceStepByIndexRequestV1Cbor = (
  message: DaTraceStepByIndexRequestV1,
): Buffer => encodeCbor(encodeTraceStepByIndexRequestValue(message));

export const decodeDaTraceStepByIndexRequestV1Cbor = (
  bytes: Uint8Array,
): DaTraceStepByIndexRequestV1 =>
  decodeTupleCbor(
    bytes,
    "TraceStepByIndexRequestV1",
    decodeTraceStepByIndexRequestValue,
  );

const encodeTraceStepByIndexResponseValue = (
  message: DaTraceStepByIndexResponseV1,
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
): DaTraceStepByIndexResponseV1 => {
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

export const encodeDaTraceStepByIndexResponseV1Cbor = (
  message: DaTraceStepByIndexResponseV1,
): Buffer => encodeCbor(encodeTraceStepByIndexResponseValue(message));

export const decodeDaTraceStepByIndexResponseV1Cbor = (
  bytes: Uint8Array,
): DaTraceStepByIndexResponseV1 =>
  decodeTupleCbor(
    bytes,
    "TraceStepByIndexResponseV1",
    decodeTraceStepByIndexResponseValue,
  );

const encodeEventToStepByEventRequestValue = (
  message: DaEventToStepByEventRequestV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  bytesValue(message.eventKey, "event_key"),
];

const decodeEventToStepByEventRequestValue = (
  value: unknown,
  fieldName: string,
): DaEventToStepByEventRequestV1 => {
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

export const encodeDaEventToStepByEventRequestV1Cbor = (
  message: DaEventToStepByEventRequestV1,
): Buffer => encodeCbor(encodeEventToStepByEventRequestValue(message));

export const decodeDaEventToStepByEventRequestV1Cbor = (
  bytes: Uint8Array,
): DaEventToStepByEventRequestV1 =>
  decodeTupleCbor(
    bytes,
    "EventToStepByEventRequestV1",
    decodeEventToStepByEventRequestValue,
  );

const encodeEventToStepByEventResponseValue = (
  message: DaEventToStepByEventResponseV1,
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
): DaEventToStepByEventResponseV1 => {
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

export const encodeDaEventToStepByEventResponseV1Cbor = (
  message: DaEventToStepByEventResponseV1,
): Buffer => encodeCbor(encodeEventToStepByEventResponseValue(message));

export const decodeDaEventToStepByEventResponseV1Cbor = (
  bytes: Uint8Array,
): DaEventToStepByEventResponseV1 =>
  decodeTupleCbor(
    bytes,
    "EventToStepByEventResponseV1",
    decodeEventToStepByEventResponseValue,
  );

const encodeAttestationGossipValue = (
  message: DaAttestationGossipV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  ensureDaPayloadHash(message.payloadHash),
  cborUint(ensureUint8(message.signerIndex, "signer_index"), "signer_index"),
  ensureDaHash32(message.daVkey, "da_vkey"),
  ensureByteLength(
    message.onChainWitness,
    DA_ON_CHAIN_WITNESS_LENGTH,
    "on_chain_witness",
  ),
  cborUint(message.retentionUntilSlot, "retention_until_slot"),
  stringValue(message.announcedByPeerId, "announced_by_peer_id"),
];

const decodeAttestationGossipValue = (
  value: unknown,
  fieldName: string,
): DaAttestationGossipV1 => {
  const v = fixedArray(value, 8, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      v[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    headerHash: headerHashValue(v[1], `${fieldName}.header_hash`),
    payloadHash: payloadHashValue(v[2], `${fieldName}.payload_hash`),
    signerIndex: ensureUint8(v[3], `${fieldName}.signer_index`),
    daVkey: hashValue(v[4], `${fieldName}.da_vkey`),
    onChainWitness: ensureByteLength(
      asBytes(v[5], `${fieldName}.on_chain_witness`),
      DA_ON_CHAIN_WITNESS_LENGTH,
      `${fieldName}.on_chain_witness`,
    ),
    retentionUntilSlot: ensureUint(v[6], `${fieldName}.retention_until_slot`),
    announcedByPeerId: stringValue(v[7], `${fieldName}.announced_by_peer_id`),
  };
};

export const encodeDaAttestationGossipV1Cbor = (
  message: DaAttestationGossipV1,
): Buffer => encodeCbor(encodeAttestationGossipValue(message));

export const decodeDaAttestationGossipV1Cbor = (
  bytes: Uint8Array,
): DaAttestationGossipV1 =>
  decodeTupleCbor(bytes, "DaAttestationGossipV1", decodeAttestationGossipValue);

const encodeAttestationsByHeaderRequestValue = (
  message: DaAttestationsByHeaderRequestV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  ensureDaHeaderHash(message.headerHash),
  message.acceptedSignerIndexes == null
    ? null
    : message.acceptedSignerIndexes.map((signerIndex, index) =>
        cborUint(
          ensureUint8(signerIndex, `accepted_signer_indexes[${index}]`),
          `accepted_signer_indexes[${index}]`,
        ),
      ),
  cborOptionalUint(message.maxAttestations, "max_attestations"),
];

const decodeAttestationsByHeaderRequestValue = (
  value: unknown,
  fieldName: string,
): DaAttestationsByHeaderRequestV1 => {
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
        : asArray(v[2], `${fieldName}.accepted_signer_indexes`).map(
            (signerIndex, index) =>
              ensureUint8(
                signerIndex,
                `${fieldName}.accepted_signer_indexes[${index}]`,
              ),
          ),
    maxAttestations:
      v[3] == null ? null : ensureUint(v[3], `${fieldName}.max_attestations`),
  };
};

export const encodeDaAttestationsByHeaderRequestV1Cbor = (
  message: DaAttestationsByHeaderRequestV1,
): Buffer => encodeCbor(encodeAttestationsByHeaderRequestValue(message));

export const decodeDaAttestationsByHeaderRequestV1Cbor = (
  bytes: Uint8Array,
): DaAttestationsByHeaderRequestV1 =>
  decodeTupleCbor(
    bytes,
    "AttestationsByHeaderRequestV1",
    decodeAttestationsByHeaderRequestValue,
  );

const encodeAttestationsByHeaderResponseValue = (
  message: DaAttestationsByHeaderResponseV1,
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
): DaAttestationsByHeaderResponseV1 => {
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

export const encodeDaAttestationsByHeaderResponseV1Cbor = (
  message: DaAttestationsByHeaderResponseV1,
): Buffer => encodeCbor(encodeAttestationsByHeaderResponseValue(message));

export const decodeDaAttestationsByHeaderResponseV1Cbor = (
  bytes: Uint8Array,
): DaAttestationsByHeaderResponseV1 =>
  decodeTupleCbor(
    bytes,
    "AttestationsByHeaderResponseV1",
    decodeAttestationsByHeaderResponseValue,
  );

const encodeConflictEvidenceValue = (
  message: DaConflictEvidenceV1,
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
): DaConflictEvidenceV1 => {
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

export const encodeDaConflictEvidenceV1Cbor = (
  message: DaConflictEvidenceV1,
): Buffer => encodeCbor(encodeConflictEvidenceValue(message));

export const decodeDaConflictEvidenceV1Cbor = (
  bytes: Uint8Array,
): DaConflictEvidenceV1 =>
  decodeTupleCbor(bytes, "ConflictEvidenceV1", decodeConflictEvidenceValue);

const encodeCapabilitiesRequestValue = (
  message: DaCapabilitiesRequestV1,
): unknown[] => [ensureDaDeploymentFingerprint(message.deploymentFingerprint)];

const decodeCapabilitiesRequestValue = (
  value: unknown,
  fieldName: string,
): DaCapabilitiesRequestV1 => {
  const fields = fixedArray(value, 1, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      fields[0],
      `${fieldName}.deployment_fingerprint`,
    ),
  };
};

export const encodeDaCapabilitiesRequestV1Cbor = (
  message: DaCapabilitiesRequestV1,
): Buffer => encodeCbor(encodeCapabilitiesRequestValue(message));

export const decodeDaCapabilitiesRequestV1Cbor = (
  bytes: Uint8Array,
): DaCapabilitiesRequestV1 =>
  decodeTupleCbor(
    bytes,
    "DaCapabilitiesRequestV1",
    decodeCapabilitiesRequestValue,
  );

const encodeCapabilitiesResponseValue = (
  message: DaCapabilitiesResponseV1,
): unknown[] => [
  ensureDaDeploymentFingerprint(message.deploymentFingerprint),
  BigInt(
    ensureDaTransportV1(
      message.transportProtocolVersion,
      "transport_protocol_version",
    ),
  ),
  cborSortedUintArray(
    exactDaPayloadSchemaVersionsV1(
      message.payloadSchemaVersions,
      "payload_schema_versions",
    ),
    "payload_schema_versions",
  ),
  cborSortedUintArray(
    message.envelopeContentEncodings,
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
): DaCapabilitiesResponseV1 => {
  const fields = fixedArray(value, 9, fieldName);
  return {
    deploymentFingerprint: deploymentFingerprintValue(
      fields[0],
      `${fieldName}.deployment_fingerprint`,
    ),
    transportProtocolVersion: ensureDaTransportV1(
      fields[1],
      `${fieldName}.transport_protocol_version`,
    ),
    payloadSchemaVersions: exactDaPayloadSchemaVersionsV1(
      fields[2],
      `${fieldName}.payload_schema_versions`,
    ),
    envelopeContentEncodings: sortedUintArrayValue(
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

export const encodeDaCapabilitiesResponseV1Cbor = (
  message: DaCapabilitiesResponseV1,
): Buffer => encodeCbor(encodeCapabilitiesResponseValue(message));

export const decodeDaCapabilitiesResponseV1Cbor = (
  bytes: Uint8Array,
): DaCapabilitiesResponseV1 =>
  decodeTupleCbor(
    bytes,
    "DaCapabilitiesResponseV1",
    decodeCapabilitiesResponseValue,
  );
