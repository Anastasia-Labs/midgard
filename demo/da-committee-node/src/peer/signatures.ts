import {
  computeDaSha256Hash,
  encodeDaConflictEvidenceV1Cbor,
  encodeDaConflictingSignatureHeaderEvidenceV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import { parseSignatureWitness } from "../coordinator/witnesses.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaStoredConflictEvidenceRecordV1,
} from "../domain.js";
import { parseDaSignatureRecordV1 } from "../domain.js";
import {
  type DaCommitteeValidation,
  verifyDaSignatureWitness,
} from "../signer.js";

export type SignatureRecordValidationArgs = {
  readonly body: Partial<DaSignatureRecord>;
  readonly headerHash: string;
  readonly deploymentFingerprint: string;
  readonly localSignerIndex?: number;
  readonly signerValidation?: DaCommitteeValidation;
  readonly verifiedPayload?: DaPayloadRecord;
  readonly expectedAvailabilityCommitmentCbor?: string;
  readonly expectedAvailabilityCommitmentDigest?: string;
};

export type DaAvailabilityCommitmentAuthorityV1 = Readonly<{
  deploymentIdentity: string;
  bondOwnerCredential: string;
  responseGeometry: Readonly<{
    chunkByteLength: number;
    trancheByteLength: number;
    maxTrancheCount: number;
  }>;
}>;

export const classifyDaLocalSigningCommitmentV1 = (args: {
  readonly records: readonly DaSignatureRecord[];
  readonly signerIndex: number;
  readonly expectedCommitmentDigest: string;
}): Readonly<{
  existingExact?: DaSignatureRecord;
  conflictingVariants: readonly DaSignatureRecord[];
  maySign: boolean;
}> => {
  const signerVariants = args.records.filter(
    (record) => record.signerIndex === args.signerIndex,
  );
  const existingExact = signerVariants.find(
    (record) =>
      record.availabilityCommitmentDigest === args.expectedCommitmentDigest,
  );
  const conflictingVariants = signerVariants.filter(
    (record) =>
      record.availabilityCommitmentDigest !== args.expectedCommitmentDigest,
  );
  return {
    ...(existingExact === undefined ? {} : { existingExact }),
    conflictingVariants,
    maySign: conflictingVariants.length === 0,
  };
};

export const deriveExpectedDaAvailabilityCommitmentV1 = (args: {
  readonly authority: DaAvailabilityCommitmentAuthorityV1;
  readonly headerHash: string;
  readonly payloadCborHex: string;
}): Readonly<{
  commitment: SDK.DaAvailabilityCommitmentV1;
  commitmentCbor: string;
  commitmentDigest: string;
}> => {
  const commitment = SDK.buildDaAvailabilityCommitmentV1({
    deploymentIdentity: args.authority.deploymentIdentity,
    headerHash: args.headerHash,
    payload: Buffer.from(args.payloadCborHex, "hex"),
    bondOwner: args.authority.bondOwnerCredential,
    responseGeometry: SDK.availabilityResponseGeometryV1(
      args.authority.responseGeometry,
    ),
  });
  const commitmentCbor = SDK.encodeDaAvailabilityCommitmentV1(commitment);
  return {
    commitment,
    commitmentCbor,
    commitmentDigest: computeDaSha256Hash(
      Buffer.from(commitmentCbor, "hex"),
    ).toString("hex"),
  };
};

export const buildDaSignatureConflictEvidenceV1 = (args: {
  readonly first: DaSignatureRecord;
  readonly second: DaSignatureRecord;
  readonly daVkey: string;
  readonly reporterPeerId: string;
  readonly receivedAt: string;
}):
  | Readonly<{
      record: DaStoredConflictEvidenceRecordV1;
      gossipCbor: Buffer;
    }>
  | undefined => {
  const first = parseDaSignatureRecordV1(args.first);
  const second = parseDaSignatureRecordV1(args.second);
  if (
    first.deploymentFingerprint !== second.deploymentFingerprint ||
    first.signerIndex !== second.signerIndex
  ) {
    throw new Error(
      "DA signature conflict evidence requires one deployment and signer",
    );
  }
  const firstIdentity = `${first.headerHash}${first.availabilityCommitmentDigest}`;
  const secondIdentity = `${second.headerHash}${second.availabilityCommitmentDigest}`;
  if (firstIdentity === secondIdentity) {
    return undefined;
  }
  const [lower, upper] =
    firstIdentity.localeCompare(secondIdentity) < 0
      ? [first, second]
      : [second, first];
  const compactEvidence = encodeDaConflictingSignatureHeaderEvidenceV1Cbor({
    signerIndex: lower.signerIndex,
    daVkey: Buffer.from(args.daVkey, "hex"),
    lowerHeaderHash: Buffer.from(lower.headerHash, "hex"),
    lowerCommitmentCbor: Buffer.from(lower.availabilityCommitmentCbor, "hex"),
    lowerHeaderWitness: Buffer.from(lower.signatureWitness, "hex"),
    upperHeaderHash: Buffer.from(upper.headerHash, "hex"),
    upperCommitmentCbor: Buffer.from(upper.availabilityCommitmentCbor, "hex"),
    upperHeaderWitness: Buffer.from(upper.signatureWitness, "hex"),
  });
  const evidenceHash = computeDaSha256Hash(compactEvidence);
  const record: DaStoredConflictEvidenceRecordV1 = {
    conflictSchemaVersion: 1,
    deploymentFingerprint: lower.deploymentFingerprint,
    headerHash: lower.headerHash,
    commitmentDigest: lower.availabilityCommitmentDigest,
    conflictingHeaderHash: upper.headerHash,
    conflictingCommitmentDigest: upper.availabilityCommitmentDigest,
    signerIndex: lower.signerIndex,
    evidenceKind: "equivocation",
    evidenceHash: evidenceHash.toString("hex"),
    compactEvidenceCborHex: compactEvidence.toString("hex"),
    reporterPeerId: args.reporterPeerId,
    receivedAt: args.receivedAt,
  };
  return {
    record,
    gossipCbor: encodeDaConflictEvidenceV1Cbor({
      deploymentFingerprint: Buffer.from(lower.deploymentFingerprint, "hex"),
      headerHash: Buffer.from(lower.headerHash, "hex"),
      evidenceKind: "equivocation",
      evidenceHash,
      compactEvidence,
    }),
  };
};

export const validateDaSignatureRecord = ({
  body,
  headerHash,
  deploymentFingerprint,
  localSignerIndex,
  signerValidation,
  verifiedPayload,
  expectedAvailabilityCommitmentCbor,
  expectedAvailabilityCommitmentDigest,
}: SignatureRecordValidationArgs): string | undefined => {
  let record: ReturnType<typeof parseDaSignatureRecordV1>;
  try {
    record = parseDaSignatureRecordV1(body);
  } catch {
    return "invalid signature record";
  }
  if (
    record.headerHash !== headerHash ||
    record.deploymentFingerprint !== deploymentFingerprint
  ) {
    return "invalid signature record";
  }
  const signerIndex = record.signerIndex;
  const validation = record.validation;
  if (validation.headerHash !== headerHash || validation.rootsMatch !== true) {
    return "signature validation summary does not match header";
  }
  let parsedWitness: ReturnType<typeof parseSignatureWitness>;
  try {
    parsedWitness = parseSignatureWitness(record.signatureWitness);
  } catch {
    return "invalid signature witness";
  }
  if (parsedWitness.signerIndex !== signerIndex) {
    return "signature witness signer index does not match record";
  }
  if (verifiedPayload !== undefined) {
    const payloadError = validateSignatureMatchesVerifiedPayload(
      record,
      verifiedPayload,
    );
    if (payloadError !== undefined) {
      return payloadError;
    }
  }
  if (
    (expectedAvailabilityCommitmentCbor === undefined) !==
    (expectedAvailabilityCommitmentDigest === undefined)
  ) {
    return "expected availability commitment authority is incomplete";
  }
  if (
    expectedAvailabilityCommitmentCbor !== undefined &&
    (record.availabilityCommitmentCbor !== expectedAvailabilityCommitmentCbor ||
      record.availabilityCommitmentDigest !==
        expectedAvailabilityCommitmentDigest)
  ) {
    return "signature availability commitment does not match authenticated release parameters";
  }
  if (signerValidation === undefined) {
    return signerIndex === localSignerIndex
      ? undefined
      : "peer signatures require committee validation";
  }
  if (record.committeeSignersHash !== signerValidation.committeeSignersHash) {
    return "signature committee hash does not match this deployment";
  }
  if (signerIndex >= signerValidation.committeeKeys.length) {
    return "signature signer index is outside the DA committee";
  }
  const publicKeyHex = signerValidation.committeeKeys[signerIndex]!;
  const availabilityCommitment = SDK.parseDaAvailabilityCommitmentV1Cbor(
    record.availabilityCommitmentCbor,
  );
  return verifyDaSignatureWitness({
    publicKeyHex,
    availabilityCommitment,
    witnessHex: record.signatureWitness,
  })
    ? undefined
    : "signature witness verification failed";
};

export const validateSignatureMatchesVerifiedPayload = (
  signature: Pick<
    DaSignatureRecord,
    "payloadHash" | "availabilityCommitmentCbor"
  >,
  payload: DaPayloadRecord,
): string | undefined => {
  if (payload.validationStatus !== "verified") {
    return "local payload is not verified";
  }
  if (payload.payloadSha256 !== signature.payloadHash) {
    return "signature payload hash does not match local verified payload";
  }
  const commitment = SDK.parseDaAvailabilityCommitmentV1Cbor(
    signature.availabilityCommitmentCbor,
  );
  if (
    !SDK.verifyDaAvailabilityPayloadCommitmentV1({
      commitment,
      payload: Buffer.from(payload.payloadCborHex, "hex"),
    })
  ) {
    return "signature availability commitment does not match local verified payload";
  }
  return undefined;
};
