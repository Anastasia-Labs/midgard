import { encodeCbor } from "./codec/cbor.js";
import {
  DaGenericFoundStatus,
  DaLocalPayloadStatus,
  DaMetadataStatus,
  type DaPayloadByHeaderRequest,
  type DaPayloadByHeaderResponse,
  DaPayloadByHeaderStatus,
  type DaPayloadSubmitResponse,
  DaPayloadSubmitStatus,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  cborOptionalPayloadChunkManifest,
  decodeTupleCbor,
  optionalPayloadChunkManifestValue,
} from "./da-transport.decode-payload-announcement-value.js";
import {
  bytesValue,
  cborOptionalUint,
  cborUint,
  type DaMetadataByHeaderResponse,
  type DaPayloadChunkRequest,
  type DaPayloadChunkResponse,
  ensureDaPayloadSchema,
  ensureUint,
  fixedArray,
  optionalStringValue,
  stringValue,
} from "./da-transport.enum-label.js";
import {
  ensureDaDeploymentFingerprint,
  ensureDaHash32,
  ensureDaHeaderHash,
  ensureDaPayloadHash,
  optionalBytesValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import {
  cborEnum,
  cborOptionalEnum,
  decodedEnum,
  decodedOptionalEnum,
  deploymentFingerprintValue,
  hashValue,
  headerHashValue,
  optionalHashArrayValue,
  optionalPayloadHashValue,
  payloadHashValue,
} from "./da-transport.parse-public-retained-da-profile.js";

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
