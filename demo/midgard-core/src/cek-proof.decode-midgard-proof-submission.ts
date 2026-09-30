import {
  encodeMidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialEntry,
} from "./cek-proof.decode-midgard-cek-program-envelope.js";
import {
  decodeMidgardCekProgramMaterialEntryList,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialEntryList,
  MIDGARD_PROOF_SUBMISSION_ENVELOPE_VERSION,
  type MidgardProofSubmission,
} from "./cek-proof.verify-program-material-bundle.js";
import {
  compareBytes,
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";

/**
 * Merges transaction-local sidecars into the block-wide content-addressed DA
 * set. Repeated roots are deduplicated only when their exact typed entry bytes
 * agree; any conflicting preimage fails closed.
 */
export const mergeMidgardCekProgramMaterialSidecars = (
  sidecars: Iterable<Uint8Array>,
): readonly MidgardCekProgramMaterialEntry[] => {
  const byRoot = new Map<
    string,
    {
      readonly encoded: Buffer;
      readonly entry: MidgardCekProgramMaterialEntry;
    }
  >();
  for (const sidecar of sidecars) {
    for (const entry of decodeMidgardCekProgramMaterialSidecar(sidecar)) {
      const rootHex = Buffer.from(entry.root).toString("hex");
      const encoded = encodeMidgardCekProgramMaterialEntry(entry);
      const existing = byRoot.get(rootHex);
      if (existing !== undefined && !existing.encoded.equals(encoded)) {
        throw new Error(`conflicting V1 program material for root ${rootHex}`);
      }
      byRoot.set(rootHex, { encoded, entry });
    }
  }
  return Object.freeze(
    [...byRoot.values()]
      .sort((left, right) => compareBytes(left.entry.root, right.entry.root))
      .map(({ entry }) => entry),
  );
};

/**
 * Exact proof-profile submission envelope. Material is sorted by its
 * content-addressed root, and each versioned value is independently usable as
 * the matching DA entry.
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
    encodeMidgardCekProgramMaterialEntryList(
      submission.programMaterial,
      "V1 submission",
    ),
  ]);
};

export const decodeMidgardProofSubmission = (
  bytes: Uint8Array,
): MidgardProofSubmission => {
  const source = Buffer.from(bytes);
  const header = readCborArrayHeader(source, 0, "proof_submission");
  if (header.length !== 3) {
    throw new Error("V1 submission must contain exactly three fields");
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
  const material = decodeMidgardCekProgramMaterialEntryList(
    source,
    transactionCbor.nextOffset,
    "proof_submission.program_material",
  );
  if (material.nextOffset !== source.length) {
    throw new Error("V1 submission has trailing bytes");
  }
  const decoded = Object.freeze({
    transactionCbor: transactionCbor.value,
    programMaterial: material.entries,
  });
  if (!encodeMidgardProofSubmission(decoded).equals(source)) {
    throw new Error("V1 submission CBOR is not canonical");
  }
  return decoded;
};
