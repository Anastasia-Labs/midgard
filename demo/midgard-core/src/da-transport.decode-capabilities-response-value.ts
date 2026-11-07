import { encodeCbor } from "./codec/cbor.js";
import {
  type DaCapabilitiesRequest,
  type DaCapabilitiesResponse,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  cborSortedUintArray,
  decodeTupleCbor,
  exactDaEnvelopeContentEncodings,
  exactDaPayloadSchemaVersions,
} from "./da-transport.decode-payload-announcement-value.js";
import {
  cborUint,
  type DaConflictEvidence,
  ensureDaTransport,
  ensureUint,
  fixedArray,
} from "./da-transport.enum-label.js";
import { ensureDaDeploymentFingerprint } from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import { deploymentFingerprintValue } from "./da-transport.parse-public-retained-da-profile.js";
import { decodeConflictEvidenceValue } from "./da-transport.validate-conflicting-signature-header-evidence.js";

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
