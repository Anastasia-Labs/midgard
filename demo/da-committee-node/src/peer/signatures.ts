import type { DaPayloadRecord, DaSignatureRecord } from "../domain.js";
import {
  verifyDaSignatureWitness,
  type DaCommitteeValidation,
} from "../signer.js";
import { parseSignatureWitness } from "../coordinator/witnesses.js";

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
  if (
    body.headerHash !== headerHash ||
    body.deploymentFingerprint !== deploymentFingerprint ||
    typeof body.signerIndex !== "number" ||
    !Number.isSafeInteger(body.signerIndex) ||
    body.signerIndex < 0 ||
    body.signerIndex > 255 ||
    typeof body.signatureWitness !== "string" ||
    typeof body.payloadHash !== "string" ||
    typeof body.committeeSignersHash !== "string" ||
    typeof body.signedAt !== "string" ||
    typeof body.validation !== "object" ||
    body.validation === null
  ) {
    return "invalid signature record";
  }
  const signerIndex = body.signerIndex;
  const validation = body.validation;
  if (validation.headerHash !== headerHash || validation.rootsMatch !== true) {
    return "signature validation summary does not match header";
  }
  let parsedWitness: ReturnType<typeof parseSignatureWitness>;
  try {
    parsedWitness = parseSignatureWitness(body.signatureWitness);
  } catch {
    return "invalid signature witness";
  }
  if (parsedWitness.signerIndex !== signerIndex) {
    return "signature witness signer index does not match record";
  }
  if (verifiedPayload !== undefined) {
    const payloadError = validateSignatureMatchesVerifiedPayload(
      body as DaSignatureRecord,
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
  if (body.committeeSignersHash !== signerValidation.committeeSignersHash) {
    return "signature committee hash does not match this deployment";
  }
  if (signerIndex >= signerValidation.committeeKeys.length) {
    return "signature signer index is outside the DA committee";
  }
  const publicKeyHex = signerValidation.committeeKeys[signerIndex]!;
  return verifyDaSignatureWitness({
    publicKeyHex,
    headerHash,
    witnessHex: body.signatureWitness,
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
