import { asArray, asBytes, encodeCbor } from "./codec/cbor.js";
import { MidgardTxCodecErrorCodes } from "./codec/errors.js";
import {
  DA_ON_CHAIN_WITNESS_LENGTH,
  DaConflictEvidenceKind,
  DaGenericFoundStatus,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  cborSignerIndexArray,
  decodeTupleCbor,
  signerIndexArrayValue,
} from "./da-transport.decode-payload-announcement-value.js";
import { encodeAttestationGossipValue } from "./da-transport.encode-attestation-gossip-value.js";
import {
  bytesValue,
  cborOptionalUint,
  cborUint,
  type DaAttestationGossip,
  type DaAttestationsByHeaderRequest,
  type DaAttestationsByHeaderResponse,
  type DaConflictEvidence,
  type DaConflictingSignatureHeaderEvidence,
  ensureByteLength,
  ensureUint,
  ensureUint8,
  fail,
  fixedArray,
  optionalStringValue,
  stringValue,
} from "./da-transport.enum-label.js";
import {
  ensureDaDeploymentFingerprint,
  ensureDaHash32,
  ensureDaHeaderHash,
  ensureNonEmptyBytes,
  nonEmptyStringValue,
  optionalBytesValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";
import {
  cborEnum,
  computeDaSha256Hash,
  decodedEnum,
  deploymentFingerprintValue,
  hashValue,
  headerHashValue,
  payloadHashValue,
} from "./da-transport.parse-public-retained-da-profile.js";

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

export const decodeConflictEvidenceValue = (
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
