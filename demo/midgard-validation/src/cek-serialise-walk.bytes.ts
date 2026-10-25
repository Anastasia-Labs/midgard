/**
 * The byte-level pieces of the serialise reader (`cek-serialise-walk.ts`):
 * minimal CBOR heads, the summaries of the nodes it closes, and the chunked
 * leaf bytes it streams under their own running root.
 */
import {
  appendMidgardCekBlobFrontierChunk,
  emptyMidgardCekBlobFrontier,
  hashMidgardCekDataNode,
  type MidgardCekBlobFrontier,
  midgardCekBytesMemoryWords,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  midgardCekIntegerMemoryWords,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import type { MidgardCekSerialiseFrame } from "./cek-serialise-walk.types.js";

export const MIDGARD_CEK_SERIALISE_CHUNK_BYTES = 4095;

type Frame = MidgardCekSerialiseFrame;
export type LeafState = Extract<Frame, { kind: "leaf" }>;

// ## CBOR heads

export const byteAt = (bytes: Uint8Array, at: number, end: number): number =>
  at < end ? bytes[at]! : -1;

export const bigEndian = (bytes: Uint8Array): bigint =>
  bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);

/** A minimal head: [major, argument (-1 when indefinite), next]. */
export const head = (
  bytes: Uint8Array,
  at: number,
  end: number,
): readonly [number, bigint, number] | null => {
  const first = byteAt(bytes, at, end);
  if (first < 0) return null;
  const major = first >> 5;
  const info = first & 31;
  if (info < 24) return [major, BigInt(info), at + 1];
  if (info <= 27) {
    const size = [1, 2, 4, 8][info - 24]!;
    if (at + 1 + size > end) return null;
    const value = bigEndian(bytes.subarray(at + 1, at + 1 + size));
    const minimal = Math.ceil(value.toString(16).length / 2);
    return value >= 24n && 2 * minimal > size
      ? [major, value, at + 1 + size]
      : null;
  }
  return info === 31 ? [major, -1n, at + 1] : null;
};

/** A definite byte string of at most 64 bytes: its content and next offset. */
export const shortBytes = (
  bytes: Uint8Array,
  at: number,
  end: number,
): readonly [Buffer, number] | null => {
  const read = head(bytes, at, end);
  if (read === null || read[0] !== 2) return null;
  const [, length, next] = read;
  if (length < 0n || length > 64n || next + Number(length) > end) return null;
  return [
    Buffer.from(bytes.subarray(next, next + Number(length))),
    next + Number(length),
  ];
};

// ## Node summaries

export const integerLeaf = (
  root: Uint8Array,
  cborLength: bigint,
  memory: bigint,
): MidgardCekDataSummary => ({
  root: hashMidgardCekDataNode({
    kind: "integer",
    cborRoot: root,
    cborLength,
    memory,
  }),
  cborLength,
  memory,
});

export const bytesLeaf = (
  root: Uint8Array,
  length: bigint,
  cborLength: bigint,
): MidgardCekDataSummary => {
  const memory = 4n + midgardCekBytesMemoryWords(length);
  return {
    root: hashMidgardCekDataNode({
      kind: "bytes",
      bytesRoot: root,
      bytesLength: length,
      cborLength,
      memory,
    }),
    cborLength,
    memory,
  };
};

export const integerMemory = (major: number, argument: bigint): bigint =>
  4n + midgardCekIntegerMemoryWords(argument + BigInt(major));

export const magnitudeMemory = (tag: number, length: bigint, allFf: boolean) =>
  4n +
  (length + 7n) / 8n +
  (tag === 3 && allFf && length % 8n === 0n ? 1n : 0n);

export const isAllOnes = (piece: Uint8Array): boolean =>
  piece.every((byte) => byte === 0xff);

export const finish = (
  open: Frame,
  folded: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary => {
  switch (open.kind) {
    case "list":
      return summarizeMidgardCekListData(folded);
    case "constr":
      return summarizeMidgardCekSmallConstrData(open.constructor, folded);
    case "largeConstr":
      return summarizeMidgardCekLargeConstrData({
        constructorCborRoot: open.indexRoot,
        constructorCborLength: open.indexCborLength,
        constructorMemory: open.indexMemory,
        fields: folded,
      });
    case "map":
      return summarizeMidgardCekMapData(folded);
    case "root":
    case "leaf":
    case "folding":
      throw new Error(`the serialise reader cannot finish a ${open.kind}`);
  }
};

// ## Streaming leaves

export type Reach = {
  readonly after: number;
  readonly got: number;
  readonly done: boolean;
};

export const reach = (
  bytes: Uint8Array,
  from: number,
  end: number,
  tag: number,
  held: number,
): Reach | null => {
  let at = from;
  let got = 0;
  for (;;) {
    const first = byteAt(bytes, at, end);
    if (first === 255) return { after: at + 1, got, done: true };
    if (first < 0) return { after: at, got, done: false };
    const size =
      first === 0x58
        ? byteAt(bytes, at + 1, end)
        : first >= 0x41 && first <= 0x57
          ? first - 0x40
          : -2;
    const content = first === 0x58 ? at + 2 : at + 1;
    if (size === -1) return { after: at, got, done: false };
    // A chunk under 24 bytes behind a 0x58 head is read like a minimal one.
    // It cannot end the loop: the leaf's cborLength counts the bytes read, so
    // the summary the exit compares with the argument's is not the canonical
    // encoding's.
    if (size < 1 || size > 64) return null;
    if (size === 64) {
      if (content + 64 > end) return { after: at, got, done: false };
      const after = content + 64;
      got += 64;
      const filled = tag === 0 ? held + got : held + after - from;
      if (filled >= MIDGARD_CEK_SERIALISE_CHUNK_BYTES)
        return { after, got, done: false };
      at = after;
      continue;
    }
    if (content + size >= end) return { after: at, got, done: false };
    if (bytes[content + size] === 255)
      return { after: content + size + 1, got: got + size, done: true };
    return null;
  }
};

/** The content of the chunks in [at, to), in order. */
export const contents = (
  bytes: Uint8Array,
  from: number,
  to: number,
): Buffer[] => {
  const pieces: Buffer[] = [];
  let at = from;
  while (at < to) {
    const first = bytes[at]!;
    if (first === 255) break;
    const size = first === 0x58 ? bytes[at + 1]! : first - 0x40;
    const content = first === 0x58 ? at + 2 : at + 1;
    pieces.push(Buffer.from(bytes.subarray(content, content + size)));
    at = content + size;
  }
  return pieces;
};

export const flush = (
  start: MidgardCekBlobFrontier,
  bytes: Buffer,
  last: boolean,
): {
  readonly leaf: MidgardCekBlobFrontier;
  readonly pending: Buffer;
  readonly chunks: number;
} => {
  let leaf = start;
  let pending = bytes;
  let chunks = 0;
  while (pending.length >= MIDGARD_CEK_SERIALISE_CHUNK_BYTES) {
    leaf = appendMidgardCekBlobFrontierChunk(
      leaf,
      pending.subarray(0, MIDGARD_CEK_SERIALISE_CHUNK_BYTES),
    );
    pending = pending.subarray(MIDGARD_CEK_SERIALISE_CHUNK_BYTES);
    chunks += 1;
  }
  if (last && pending.length > 0) {
    leaf = appendMidgardCekBlobFrontierChunk(leaf, pending);
    pending = Buffer.alloc(0);
    chunks += 1;
  }
  return { leaf, pending: Buffer.from(pending), chunks };
};

export const openLeaf = (
  parent: Frame,
  tag: 0 | 2 | 3,
  pending: Buffer,
  cborLength: bigint,
  allFf: boolean,
): LeafState => ({
  kind: "leaf",
  parent,
  tag,
  leaf: emptyMidgardCekBlobFrontier(),
  pending,
  length: 0n,
  cborLength,
  allFf,
});
