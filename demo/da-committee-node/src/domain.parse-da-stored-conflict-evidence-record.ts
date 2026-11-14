import {
  computeDaSha256Hash,
  decodeDaConflictingSignatureHeaderEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import { type DaStoredConflictEvidenceRecord } from "./domain.da-payload-record.js";
import {
  requireLowerHex,
  requireNonEmptyString,
  requireUint8,
} from "./domain.parse-da-signature-record.js";
import {
  conflictEvidenceRecordKeys,
  requireExactObject,
  requireString,
} from "./domain.parse-da-stored-payload-record.js";

export const parseDaStoredConflictEvidenceRecord = (
  value: unknown,
): DaStoredConflictEvidenceRecord => {
  const record = requireExactObject(
    value,
    conflictEvidenceRecordKeys,
    [],
    "DA stored conflict evidence record V1",
  );
  if (record.conflictSchemaVersion !== 1) {
    throw new Error(
      "DA stored conflict evidence record V1.conflictSchemaVersion must be exactly 1",
    );
  }
  if (record.evidenceKind !== "equivocation") {
    throw new Error(
      "DA stored conflict evidence record V1.evidenceKind must be equivocation",
    );
  }
  const deploymentFingerprint = requireLowerHex(
    record.deploymentFingerprint,
    32,
    "DA stored conflict evidence record V1.deploymentFingerprint",
  );
  const headerHash = requireLowerHex(
    record.headerHash,
    28,
    "DA stored conflict evidence record V1.headerHash",
  );
  const conflictingHeaderHash = requireLowerHex(
    record.conflictingHeaderHash,
    28,
    "DA stored conflict evidence record V1.conflictingHeaderHash",
  );
  const commitmentDigest = requireLowerHex(
    record.commitmentDigest,
    32,
    "DA stored conflict evidence record V1.commitmentDigest",
  );
  const conflictingCommitmentDigest = requireLowerHex(
    record.conflictingCommitmentDigest,
    32,
    "DA stored conflict evidence record V1.conflictingCommitmentDigest",
  );
  const signerIndex = requireUint8(
    record.signerIndex,
    "DA stored conflict evidence record V1.signerIndex",
  );
  const evidenceHash = requireLowerHex(
    record.evidenceHash,
    32,
    "DA stored conflict evidence record V1.evidenceHash",
  );
  const compactEvidenceCborHex = requireLowerHex(
    record.compactEvidenceCborHex,
    undefined,
    "DA stored conflict evidence record V1.compactEvidenceCborHex",
  );
  if (compactEvidenceCborHex.length === 0) {
    throw new Error(
      "DA stored conflict evidence record V1.compactEvidenceCborHex must not be empty",
    );
  }
  const compactEvidence = Buffer.from(compactEvidenceCborHex, "hex");
  const decoded =
    decodeDaConflictingSignatureHeaderEvidenceCbor(compactEvidence);
  if (
    !encodeDaConflictingSignatureHeaderEvidenceCbor(decoded).equals(
      compactEvidence,
    )
  ) {
    throw new Error(
      "DA stored conflict evidence record V1.compactEvidenceCborHex must be canonical CBOR",
    );
  }
  const lowerCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    decoded.lowerCommitmentCbor.toString("hex"),
  );
  const upperCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    decoded.upperCommitmentCbor.toString("hex"),
  );
  const decodedCommitmentDigest = computeDaSha256Hash(
    decoded.lowerCommitmentCbor,
  ).toString("hex");
  const decodedConflictingCommitmentDigest = computeDaSha256Hash(
    decoded.upperCommitmentCbor,
  ).toString("hex");
  if (
    decoded.lowerHeaderHash.toString("hex") !== headerHash ||
    lowerCommitment.header_hash !== headerHash ||
    decodedCommitmentDigest !== commitmentDigest ||
    decoded.upperHeaderHash.toString("hex") !== conflictingHeaderHash ||
    upperCommitment.header_hash !== conflictingHeaderHash ||
    decodedConflictingCommitmentDigest !== conflictingCommitmentDigest ||
    decoded.signerIndex !== signerIndex
  ) {
    throw new Error(
      "DA stored conflict evidence record V1 derived conflict identity does not match compact evidence",
    );
  }
  if (
    `${headerHash}${commitmentDigest}`.localeCompare(
      `${conflictingHeaderHash}${conflictingCommitmentDigest}`,
    ) >= 0
  ) {
    throw new Error(
      "DA stored conflict evidence record V1 composite identities must be strictly ordered",
    );
  }
  if (computeDaSha256Hash(compactEvidence).toString("hex") !== evidenceHash) {
    throw new Error(
      "DA stored conflict evidence record V1.evidenceHash does not match compact evidence",
    );
  }
  return {
    conflictSchemaVersion: 1,
    deploymentFingerprint,
    headerHash,
    commitmentDigest,
    conflictingHeaderHash,
    conflictingCommitmentDigest,
    signerIndex,
    evidenceKind: "equivocation",
    evidenceHash,
    compactEvidenceCborHex,
    reporterPeerId: requireNonEmptyString(
      record.reporterPeerId,
      "DA stored conflict evidence record V1.reporterPeerId",
    ),
    receivedAt: requireCanonicalIsoTimestamp(
      record.receivedAt,
      "DA stored conflict evidence record V1.receivedAt",
    ),
  };
};

const requireCanonicalIsoTimestamp = (
  value: unknown,
  label: string,
): string => {
  const result = requireString(value, label);
  let canonical: string;
  try {
    canonical = new Date(result).toISOString();
  } catch {
    throw new Error(`${label} must be a canonical ISO timestamp`);
  }
  if (canonical !== result) {
    throw new Error(`${label} must be a canonical ISO timestamp`);
  }
  return result;
};
