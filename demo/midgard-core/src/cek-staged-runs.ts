/**
 * Staged operands: the TypeScript twin of `cek_staged_blob_v1`.
 *
 * An operand longer than an arm can carry inline is split into 4,095-byte
 * chunks (every chunk but the last full, the empty operand one empty chunk)
 * and staged as runs of at most three chunks, each run the inline datum
 * `Constr 0 [List [B chunk, ...]]` of its own output. A step names the staging
 * outputs by output reference, in ledger reference-input order, and each
 * operand by the indices of its runs. The operand's root is the flat chunk
 * tree root that `commitMidgardCekBlob` gives its bytes.
 */
import {
  commitMidgardCekBlob,
  hashMidgardCekBlobChunk,
} from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import { type Bytes } from "./cek-proof.encode-midgard-cek-term-node.js";
import { type Hash32 } from "./codec/hash.js";

export const MIDGARD_CEK_STAGED_CHUNK_BYTES = 4_095 as const;
export const MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS = 3 as const;

/** An output reference: transaction id and output index. */
export type MidgardCekStagedOutRef = {
  readonly transactionId: Bytes;
  readonly outputIndex: number;
};

/**
 * A reference input as the walk sees it: its output reference and, when its
 * inline datum is a staging run, the run's chunks. `chunks` is `null` for an
 * output whose datum is not a staging run.
 */
export type MidgardCekStagedReferenceInput = {
  readonly outRef: MidgardCekStagedOutRef;
  readonly chunks: readonly Bytes[] | null;
};

export type MidgardCekStagedOperand = {
  readonly root: Hash32;
  readonly length: number;
  readonly chunks: readonly Buffer[];
};

/** The operand's chunks: 4,095 bytes each but the last; one empty chunk for no bytes. */
export const splitMidgardCekStagedChunks = (bytes: Bytes): Buffer[] => {
  const source = Buffer.from(bytes);
  if (source.length === 0) return [Buffer.alloc(0)];
  const chunks: Buffer[] = [];
  for (
    let offset = 0;
    offset < source.length;
    offset += MIDGARD_CEK_STAGED_CHUNK_BYTES
  ) {
    chunks.push(
      source.subarray(
        offset,
        Math.min(offset + MIDGARD_CEK_STAGED_CHUNK_BYTES, source.length),
      ),
    );
  }
  return chunks;
};

/** The chunks grouped into runs of at most three, in order. */
export const groupMidgardCekStagedRuns = (
  chunks: readonly Bytes[],
): Buffer[][] => {
  const runs: Buffer[][] = [];
  for (
    let index = 0;
    index < chunks.length;
    index += MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS
  ) {
    runs.push(
      chunks
        .slice(index, index + MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS)
        .map((chunk) => Buffer.from(chunk)),
    );
  }
  return runs;
};

/** Ledger order of output references: transaction id bytes, then index. */
export const compareMidgardCekStagedOutRefs = (
  left: MidgardCekStagedOutRef,
  right: MidgardCekStagedOutRef,
): number => {
  const byId = Buffer.compare(
    Buffer.from(left.transactionId),
    Buffer.from(right.transactionId),
  );
  if (byId !== 0) return byId;
  return Math.sign(left.outputIndex - right.outputIndex);
};

/**
 * The staging outputs in ledger order with their runs, as a step names them.
 * Refuses a repeated output reference.
 */
export const orderMidgardCekStagedRuns = (
  staged: readonly {
    readonly outRef: MidgardCekStagedOutRef;
    readonly chunks: readonly Bytes[];
  }[],
): {
  readonly refs: MidgardCekStagedOutRef[];
  readonly runs: Buffer[][];
} => {
  const ordered = [...staged].sort((left, right) =>
    compareMidgardCekStagedOutRefs(left.outRef, right.outRef),
  );
  for (let index = 1; index < ordered.length; index += 1) {
    if (
      compareMidgardCekStagedOutRefs(
        ordered[index - 1]!.outRef,
        ordered[index]!.outRef,
      ) === 0
    ) {
      throw new Error("staging output reference is repeated");
    }
  }
  return {
    refs: ordered.map((entry) => entry.outRef),
    runs: ordered.map((entry) => entry.chunks.map((c) => Buffer.from(c))),
  };
};

/**
 * Reads the runs named by `refs` from the reference inputs, both in ledger
 * order: each ref matches the first later reference input with that output
 * reference, skipping the others. Refuses a ref with no match and a matched
 * output that does not carry a run of one to three chunks.
 */
export const collectMidgardCekStagedRuns = (
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
  refs: readonly MidgardCekStagedOutRef[],
): Buffer[][] => {
  const runs: Buffer[][] = [];
  let cursor = 0;
  for (const ref of refs) {
    while (
      cursor < referenceInputs.length &&
      compareMidgardCekStagedOutRefs(referenceInputs[cursor]!.outRef, ref) !== 0
    ) {
      cursor += 1;
    }
    const input = referenceInputs[cursor];
    if (input === undefined) {
      throw new Error("staging output is not among the reference inputs");
    }
    cursor += 1;
    const chunks = input.chunks;
    if (
      chunks === null ||
      chunks.length < 1 ||
      chunks.length > MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS
    ) {
      throw new Error("staging output does not carry a run of 1 to 3 chunks");
    }
    runs.push(chunks.map((chunk) => Buffer.from(chunk)));
  }
  return runs;
};

/**
 * The operand made of the runs at `indices`, in that order: every chunk but
 * the last exactly 4,095 bytes, the last non-empty unless it is the only one.
 */
export const bindMidgardCekStagedOperand = (
  runs: readonly (readonly Bytes[])[],
  indices: readonly number[],
): MidgardCekStagedOperand => {
  const chunks: Buffer[] = [];
  for (const index of indices) {
    const run = runs[index];
    if (run === undefined) throw new Error("run index is outside the runs");
    for (const chunk of run) chunks.push(Buffer.from(chunk));
  }
  if (chunks.length === 0) throw new Error("operand has no chunks");
  const last = chunks.length - 1;
  chunks.forEach((chunk, index) => {
    if (index < last) {
      if (chunk.length !== MIDGARD_CEK_STAGED_CHUNK_BYTES) {
        throw new Error("a chunk before the last is not 4,095 bytes");
      }
    } else if (
      chunk.length > MIDGARD_CEK_STAGED_CHUNK_BYTES ||
      (last > 0 && chunk.length === 0)
    ) {
      throw new Error("the last chunk is empty or over 4,095 bytes");
    }
  });
  const length = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
  return Object.freeze({
    root: midgardCekFlatRootOfPieces(chunks),
    length,
    chunks,
  });
};

/** The flat chunk tree root of the concatenation of `pieces`. */
export const midgardCekFlatRootOfPieces = (pieces: readonly Bytes[]): Hash32 =>
  commitMidgardCekBlob(Buffer.concat(pieces.map((p) => Buffer.from(p)))).root;

/** The flat chunk tree root of `bytes`. */
export const midgardCekFlatRoot = (bytes: Bytes): Hash32 =>
  commitMidgardCekBlob(bytes).root;

/** The root of one chunk of at most 4,095 bytes. */
export const midgardCekStagedChunkRoot = (chunk: Bytes): Hash32 =>
  hashMidgardCekBlobChunk(chunk);
