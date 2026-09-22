import {
  hashMidgardCekBlobBranch,
  hashMidgardCekBlobChunk,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
} from "./cek-proof.js";
import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import { MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT } from "./validation-merkle.js";

export const MIDGARD_CEK_BLOB_FRONTIER_VERSION = 1 as const;

export type MidgardCekBlobFrontierPeak = {
  readonly height: number;
  readonly root: Hash32;
  readonly byteLength: bigint;
};

/**
 * Peaks are ordered from the rightmost/smallest subtree to the
 * leftmost/largest subtree. This makes append deterministic and permits the
 * rightmost peak alone to contain the final partial CEK blob leaf.
 */
export type MidgardCekBlobFrontier = {
  readonly count: number;
  readonly byteLength: bigint;
  readonly peaks: readonly MidgardCekBlobFrontierPeak[];
};

const exactUint32 = (value: number, field: string): number => {
  if (
    !Number.isSafeInteger(value) ||
    value < 0 ||
    value > MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT
  ) {
    throw new Error(`Invalid V1 CEK blob frontier ${field}`);
  }
  return value;
};

const exactUint64 = (value: bigint, field: string): bigint => {
  if (value < 0n || value > 0xffff_ffff_ffff_ffffn) {
    throw new Error(`Invalid V1 CEK blob frontier ${field}`);
  }
  return value;
};

const powerOfTwo = (height: number): bigint => {
  const exact = exactUint32(height, "peak height");
  if (exact > 31) {
    throw new Error("Invalid V1 CEK blob frontier peak height");
  }
  return 1n << BigInt(exact);
};

export const validateMidgardCekBlobFrontier = (
  frontier: MidgardCekBlobFrontier,
): void => {
  const count = exactUint32(frontier.count, "count");
  const byteLength = exactUint64(frontier.byteLength, "byte length");
  const expectedMinimum =
    count === 0 ? 0n : BigInt(count - 1) * BigInt(MIDGARD_CEK_BLOB_CHUNK_BYTES);
  const expectedMaximum = BigInt(count) * BigInt(MIDGARD_CEK_BLOB_CHUNK_BYTES);
  if (
    byteLength < expectedMinimum ||
    byteLength > expectedMaximum ||
    (count === 0 && frontier.peaks.length !== 0)
  ) {
    throw new Error("Invalid V1 CEK blob frontier length");
  }

  let remaining = BigInt(count);
  let priorHeight = -1;
  let peakByteLength = 0n;
  for (let index = 0; index < frontier.peaks.length; index += 1) {
    const peak = frontier.peaks[index]!;
    const height = exactUint32(peak.height, "peak height");
    const leaves = powerOfTwo(height);
    const maximum = leaves * BigInt(MIDGARD_CEK_BLOB_CHUNK_BYTES);
    const minimum =
      index === 0
        ? (leaves - 1n) * BigInt(MIDGARD_CEK_BLOB_CHUNK_BYTES)
        : maximum;
    if (
      height <= priorHeight ||
      (remaining & leaves) === 0n ||
      peak.byteLength < minimum ||
      peak.byteLength > maximum
    ) {
      throw new Error("Invalid V1 CEK blob frontier peak");
    }
    ensureHash32(peak.root, "cek_blob_frontier_v1.peak.root");
    exactUint64(peak.byteLength, "peak byte length");
    priorHeight = height;
    remaining -= leaves;
    peakByteLength += peak.byteLength;
  }
  if (remaining !== 0n || peakByteLength !== byteLength) {
    throw new Error("Invalid V1 CEK blob frontier occupancy");
  }
};

export const emptyMidgardCekBlobFrontier = (): MidgardCekBlobFrontier =>
  Object.freeze({
    count: 0,
    byteLength: 0n,
    peaks: Object.freeze([]),
  });

export const appendMidgardCekBlobFrontierChunkRoot = (
  frontier: MidgardCekBlobFrontier,
  chunk: {
    readonly root: Uint8Array;
    readonly byteLength: number;
  },
): MidgardCekBlobFrontier => {
  validateMidgardCekBlobFrontier(frontier);
  if (
    frontier.count >= MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT ||
    (frontier.count > 0 &&
      frontier.byteLength !==
        BigInt(frontier.count) * BigInt(MIDGARD_CEK_BLOB_CHUNK_BYTES))
  ) {
    throw new Error("V1 CEK blob frontier cannot append after a final leaf");
  }
  if (
    !Number.isSafeInteger(chunk.byteLength) ||
    chunk.byteLength < 0 ||
    chunk.byteLength > MIDGARD_CEK_BLOB_CHUNK_BYTES
  ) {
    throw new Error("V1 CEK blob frontier chunk exceeds 4,095 bytes");
  }

  const peaks = [...frontier.peaks];
  let peak: MidgardCekBlobFrontierPeak = {
    height: 0,
    root: ensureHash32(chunk.root, "cek_blob_frontier_v1.appended_chunk.root"),
    byteLength: BigInt(chunk.byteLength),
  };
  while (peaks[0]?.height === peak.height) {
    const left = peaks.shift()!;
    const byteLength = left.byteLength + peak.byteLength;
    peak = {
      height: peak.height + 1,
      root: hashMidgardCekBlobBranch({
        left: left.root,
        right: peak.root,
        byteLength,
      }),
      byteLength,
    };
  }
  const next = Object.freeze({
    count: frontier.count + 1,
    byteLength: frontier.byteLength + BigInt(chunk.byteLength),
    peaks: Object.freeze([peak, ...peaks]),
  });
  validateMidgardCekBlobFrontier(next);
  return next;
};

export const appendMidgardCekBlobFrontierChunk = (
  frontier: MidgardCekBlobFrontier,
  chunk: Uint8Array,
): MidgardCekBlobFrontier => {
  const bytes = Buffer.from(chunk);
  return appendMidgardCekBlobFrontierChunkRoot(frontier, {
    root: hashMidgardCekBlobChunk(bytes),
    byteLength: bytes.length,
  });
};

export const finalizeMidgardCekBlobFrontier = (
  frontier: MidgardCekBlobFrontier,
): Hash32 | null => {
  validateMidgardCekBlobFrontier(frontier);
  if (frontier.count === 0) return null;
  let aggregate = frontier.peaks[0]!;
  for (let index = 1; index < frontier.peaks.length; index += 1) {
    const left = frontier.peaks[index]!;
    const byteLength = left.byteLength + aggregate.byteLength;
    aggregate = {
      height: left.height + 1,
      root: hashMidgardCekBlobBranch({
        left: left.root,
        right: aggregate.root,
        byteLength,
      }),
      byteLength,
    };
  }
  return aggregate.root;
};

export const encodeMidgardCekBlobFrontier = (
  frontier: MidgardCekBlobFrontier,
): Buffer => {
  validateMidgardCekBlobFrontier(frontier);
  return encodeCbor([
    BigInt(MIDGARD_CEK_BLOB_FRONTIER_VERSION),
    BigInt(frontier.count),
    frontier.byteLength,
    frontier.peaks.map(({ height, root, byteLength }) => [
      BigInt(height),
      root,
      byteLength,
    ]),
  ]);
};
