import { parseSignatureWitness } from "../coordinator/witnesses.js";
import type { DaPayloadRecord, DaSignatureRecord } from "../domain.js";
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
};

export const validateDaSignatureRecord = ({
  body,
  headerHash,
  deploymentFingerprint,
  localSignerIndex,
  signerValidation,
  verifiedPayload,
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
  return verifyDaSignatureWitness({
    publicKeyHex,
    headerHash,
    witnessHex: record.signatureWitness,
  })
    ? undefined
    : "signature witness verification failed";
};

export const validateSignatureMatchesVerifiedPayload = (
  signature: Pick<DaSignatureRecord, "payloadHash">,
  payload: DaPayloadRecord,
): string | undefined => {
  if (payload.validationStatus !== "verified") {
    return "local payload is not verified";
  }
  if (payload.payloadSha256 !== signature.payloadHash) {
    return "signature payload hash does not match local verified payload";
  }
  return undefined;
};
