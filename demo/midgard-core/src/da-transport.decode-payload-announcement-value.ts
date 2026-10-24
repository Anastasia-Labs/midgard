import {
  asArray,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "./codec/cbor.js";
import { MidgardTxCodecErrorCodes } from "./codec/errors.js";
import {
  DA_PAYLOAD_INNER_SCHEMA_VERSION,
  DaPayloadContentEncoding,
} from "./da-payload-envelope.js";
import {
  DA_GOSSIP_SIGNATURE_LENGTH,
  type DaPayloadAnnouncement,
  type DaPayloadChunkManifest,
  DaPayloadSubmitMode,
  type DaPayloadSubmitRequest,
  type DaTransportTimingOptions,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  bytesValue,
  cborUint,
  ensureByteLength,
  ensureDaPayloadSchema,
  ensureUint,
  ensureUint8,
  fail,
  fixedArray,
} from "./da-transport.enum-label.js";
import {
  ensureDaDeploymentFingerprint,
  ensureDaHash32,
  ensureDaHeaderHash,
  ensureDaPayloadHash,
  nonEmptyStringValue,
  optionalBytesValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import {
  cborEnum,
  decodedEnum,
  deploymentFingerprintValue,
  hashArrayValue,
  hashValue,
  headerHashValue,
  payloadHashValue,
  sortedUintArrayValue,
} from "./da-transport.parse-public-retained-da-profile.js";

export const cborSortedUintArray = (
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

export const signerIndexArrayValue = (
  value: unknown,
  fieldName: string,
): number[] => {
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

export const cborSignerIndexArray = (
  value: readonly number[],
  fieldName: string,
): bigint[] => signerIndexArrayValue(value, fieldName).map(BigInt);

export const exactDaPayloadSchemaVersions = (
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

export const exactDaEnvelopeContentEncodings = (
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

export const optionalPayloadChunkManifestValue = (
  value: unknown,
  fieldName: string,
): DaPayloadChunkManifest | null =>
  value == null ? null : decodePayloadChunkManifestValue(value, fieldName);

export const cborOptionalPayloadChunkManifest = (
  value: DaPayloadChunkManifest | null,
): unknown[] | null =>
  value == null ? null : encodePayloadChunkManifestValue(value);

export const decodeTupleCbor = <T>(
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
