import { encodeCbor } from "./codec/cbor.js";
import {
  DA_ON_CHAIN_WITNESS_LENGTH,
  DaGenericFoundStatus,
  DaProofBundleStatus,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  cborOptionalPayloadChunkManifest,
  decodeTupleCbor,
  optionalPayloadChunkManifestValue,
} from "./da-transport.decode-payload-announcement-value.js";
import {
  bytesValue,
  cborUint,
  type DaAttestationGossip,
  type DaEventToStepByEventRequest,
  type DaEventToStepByEventResponse,
  type DaProofBundleByHeaderRequest,
  type DaProofBundleByHeaderResponse,
  type DaTraceStepByIndexRequest,
  type DaTraceStepByIndexResponse,
  ensureByteLength,
  ensureUint,
  ensureUint8,
  fixedArray,
  optionalStringValue,
  stringValue,
} from "./da-transport.enum-label.js";
import {
  ensureDaDeploymentFingerprint,
  ensureDaHash32,
  ensureDaHeaderHash,
  ensureDaPayloadHash,
  ensureNonEmptyBytes,
  nonEmptyStringValue,
  optionalBytesValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import {
  cborEnum,
  decodedEnum,
  deploymentFingerprintValue,
  hashValue,
  headerHashValue,
} from "./da-transport.parse-public-retained-da-profile.js";

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

export const encodeAttestationGossipValue = (
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
