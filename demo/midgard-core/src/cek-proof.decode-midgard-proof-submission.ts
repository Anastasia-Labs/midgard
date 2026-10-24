import {
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";

export const MIDGARD_PROOF_SUBMISSION_ENVELOPE_VERSION = 1n;
export type MidgardProofSubmission = { readonly transactionCbor: Buffer };

/**
 * Submission envelope containing the canonical transaction. Executable Flat
 * bytes travel inside its witness and reference-script items.
 */
export const encodeMidgardProofSubmission = (
  submission: MidgardProofSubmission,
): Buffer => {
  const transactionCbor = Buffer.from(submission.transactionCbor);
  if (transactionCbor.length === 0) {
    throw new Error("V1 submission transaction must not be empty");
  }
  return encodeCbor([
    MIDGARD_PROOF_SUBMISSION_ENVELOPE_VERSION,
    transactionCbor,
  ]);
};

export const decodeMidgardProofSubmission = (
  bytes: Uint8Array,
): MidgardProofSubmission => {
  const source = Buffer.from(bytes);
  const header = readCborArrayHeader(source, 0, "proof_submission");
  if (header.length !== 2) {
    throw new Error("V1 submission must contain exactly two fields");
  }
  const version = readCborUnsigned(
    source,
    header.nextOffset,
    "proof_submission.version",
  );
  if (version.value !== MIDGARD_PROOF_SUBMISSION_ENVELOPE_VERSION) {
    throw new Error(
      `unsupported V1 submission version ${version.value.toString()}`,
    );
  }
  const transactionCbor = readCborBytes(
    source,
    version.nextOffset,
    "proof_submission.transaction",
  );
  if (transactionCbor.value.length === 0) {
    throw new Error("V1 submission transaction must not be empty");
  }
  if (transactionCbor.nextOffset !== source.length) {
    throw new Error("V1 submission has trailing bytes");
  }
  const decoded = Object.freeze({
    transactionCbor: transactionCbor.value,
  });
  if (!encodeMidgardProofSubmission(decoded).equals(source)) {
    throw new Error("V1 submission CBOR is not canonical");
  }
  return decoded;
};
