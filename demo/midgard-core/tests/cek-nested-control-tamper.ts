import {
  MIDGARD_BLAKE2B_256_ROUNDS,
  type MidgardBlake2b256TraceControl,
  MidgardBlake2b256TraceStages,
  type MidgardCekSourceBlobControl,
  MidgardCekSourceBlobStages,
} from "../src/index.js";

/**
 * Tampering helpers for the nested-control entry-validation tests: each
 * defect sits only in a nested child (down to the innermost BLAKE2b trace)
 * while the parent's own relational checks still pass.
 */

// Invalid in every BLAKE2b stage: Ready/Terminal need round 0, Round needs
// round < 12 and Finish needs round 12. The parent's own relational checks
// (total length, stage) still pass.
export const tamperHash = (
  control: MidgardBlake2b256TraceControl,
): MidgardBlake2b256TraceControl => ({
  ...control,
  round: MIDGARD_BLAKE2B_256_ROUNDS + 1,
});

export const tamperPadding = (
  control: MidgardBlake2b256TraceControl,
): MidgardBlake2b256TraceControl => {
  const activeBlock = Buffer.from(control.activeBlock);
  activeBlock[activeBlock.length - 1] = 1;
  return { ...control, activeBlock };
};

export const tamperBlobHash = (
  control: MidgardCekSourceBlobControl,
  tamper: (
    hash: MidgardBlake2b256TraceControl,
  ) => MidgardBlake2b256TraceControl = tamperHash,
): MidgardCekSourceBlobControl => ({
  ...control,
  activeHash: tamper(control.activeHash!),
});

export const tamperBlobFrontier = (
  control: MidgardCekSourceBlobControl,
): MidgardCekSourceBlobControl => ({
  ...control,
  frontier: {
    ...control.frontier,
    byteLength: control.frontier.byteLength + 1n,
  },
});

// A canonical Cardano positive bignum with a magnitude of `length` bytes:
// magnitudes above 64 bytes are carried as 64-byte indefinite chunks.
export const chunkedBignum = (length: number): Buffer => {
  const magnitude = Buffer.alloc(length, 0x6a);
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < length; offset += 64) {
    const chunk = magnitude.subarray(offset, offset + 64);
    chunks.push(
      chunk.length < 24
        ? Buffer.from([0x40 + chunk.length])
        : Buffer.from([0x58, chunk.length]),
      chunk,
    );
  }
  return Buffer.concat([
    Buffer.from("c25f", "hex"),
    ...chunks,
    Buffer.from("ff", "hex"),
  ]);
};

// The step that seals a child whose blob is already terminal: it does not
// touch the blob, so only the entry check can see a defect in it.
export const sealingBlob = (
  control: MidgardCekSourceBlobControl | null,
): boolean =>
  control !== null && control.stage === MidgardCekSourceBlobStages.Terminal;

export const BLAKE_REFUSAL = "Invalid V1 BLAKE2b-256 trace control";
export const BLOB_REFUSAL = "Invalid V1 CEK source blob control";
export const INTEGER_REFUSAL = "Invalid V1 CEK Data integer control";
export const BYTES_REFUSAL = "Invalid V1 CEK Data bytes control";
export const TRAVERSE_REFUSAL = "Invalid V1 CEK Data traversal control";

export const readyBlob = (
  control: MidgardCekSourceBlobControl | null,
): boolean =>
  control !== null &&
  control.stage === MidgardCekSourceBlobStages.Active &&
  control.activeHash!.stage === MidgardBlake2b256TraceStages.Ready &&
  control.activeHash!.cursor > 0;

export const partialRoundBlob = (control: MidgardCekSourceBlobControl | null) =>
  control !== null &&
  control.stage === MidgardCekSourceBlobStages.Active &&
  control.activeHash!.stage === MidgardBlake2b256TraceStages.Round &&
  control.activeHash!.activeBlockLength < 128;
