/**
 * The bit reader, streaming sinks and Data summaries of the script-decode
 * step machine, as in `onchain/aiken/lib/midgard/script-decode-bits-v1.ak`.
 */
import {
  appendMidgardCekBlobFrontierChunk,
  emptyMidgardCekBlobFrontier,
  finalizeMidgardCekBlobFrontier,
  hashMidgardCekBlobChunk,
  hashMidgardCekDataNode,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  midgardCekBytesMemoryWords,
  midgardCekDataBytesCborLength,
  midgardCekIntegerMemoryWords,
} from "@al-ft/midgard-core";

import { encodePlutusInteger } from "./script-decode.step-data.js";
import type { BlobSink, MagSink, Summary } from "./script-decode.step-types.js";

/** The `width` bits (at most 8) of `window` starting at bit `position`. */
export const readBits = (
  window: Uint8Array,
  position: number,
  width: number,
): number => {
  const index = position >>> 3;
  const word = (window[index]! << 8) | window[index + 1]!;
  return (word >>> (16 - (position & 7) - width)) & ((1 << width) - 1);
};

/** The number of significant bits of a byte (0 for 0). */
export const bitLength8 = (byte: number): number =>
  byte === 0 ? 0 : 32 - Math.clz32(byte);

/**
 * The position of the first set bit at or after `position` within the first
 * `flatLength` bytes of the window, if any.
 */
export const nextSetBit = (
  window: Uint8Array,
  position: number,
  flatLength: number,
): number | undefined => {
  const index = position >>> 3;
  if (index >= flatLength) return undefined;
  const masked = window[index]! & ((1 << (8 - (position & 7))) - 1);
  if (masked > 0) return index * 8 + 8 - bitLength8(masked);
  for (let found = index + 1; found < flatLength; found += 1) {
    const byte = window[found]!;
    if (byte > 0) return found * 8 + 8 - bitLength8(byte);
  }
  return undefined;
};

// ## Blob sink

export const emptySink = (): BlobSink => ({
  frontier: emptyMidgardCekBlobFrontier(),
  pending: Buffer.alloc(0),
});

export const sinkLength = (sink: BlobSink): number =>
  Number(sink.frontier.byteLength) + sink.pending.length;

export const sinkPush = (sink: BlobSink, bytes: Uint8Array): BlobSink => {
  let frontier = sink.frontier;
  let pending = Buffer.concat([sink.pending, Buffer.from(bytes)]);
  while (pending.length >= MIDGARD_CEK_BLOB_CHUNK_BYTES) {
    frontier = appendMidgardCekBlobFrontierChunk(
      frontier,
      pending.subarray(0, MIDGARD_CEK_BLOB_CHUNK_BYTES),
    );
    pending = pending.subarray(MIDGARD_CEK_BLOB_CHUNK_BYTES);
  }
  return { frontier, pending: Buffer.from(pending) };
};

/** The blob root of everything pushed. */
export const sinkRoot = (sink: BlobSink): Buffer => {
  const frontier =
    sink.pending.length > 0 || sink.frontier.count === 0
      ? appendMidgardCekBlobFrontierChunk(sink.frontier, sink.pending)
      : sink.frontier;
  const root = finalizeMidgardCekBlobFrontier(frontier);
  if (root === null) throw new Error("script decode: empty blob frontier");
  return Buffer.from(root);
};

// ## Summaries

export const bytesSummary = (length: number, root: Uint8Array): Summary => {
  const bytesLength = BigInt(length);
  const cborLength = midgardCekDataBytesCborLength(bytesLength);
  const memory = 4n + midgardCekBytesMemoryWords(bytesLength);
  return {
    root: hashMidgardCekDataNode({
      kind: "bytes",
      bytesRoot: root,
      bytesLength,
      cborLength,
      memory,
    }),
    cborLength,
    memory,
  };
};

export const shortBytesSummary = (bytes: Uint8Array): Summary =>
  bytesSummary(bytes.length, hashMidgardCekBlobChunk(bytes));

const integerNodeSummary = (
  cborRoot: Uint8Array,
  cborLength: bigint,
  memory: bigint,
): Summary => ({
  root: hashMidgardCekDataNode({
    kind: "integer",
    cborRoot,
    cborLength,
    memory,
  }),
  cborLength,
  memory,
});

/** The summary of an integer whose canonical CBOR fits one chunk. */
export const integerSummary = (value: bigint): Summary => {
  const encoded = encodePlutusInteger(value);
  return integerNodeSummary(
    hashMidgardCekBlobChunk(encoded),
    BigInt(encoded.length),
    4n + midgardCekIntegerMemoryWords(value),
  );
};

// ## Magnitude sink

export const magNew = (negative: boolean): MagSink => ({
  k: "short",
  negative,
  magnitude: Buffer.alloc(0),
});

const stripZeros = (bytes: Buffer): Buffer => {
  let start = 0;
  while (start < bytes.length && bytes[start] === 0) start += 1;
  return bytes.subarray(start);
};

const allOnes = (bytes: Uint8Array): boolean => bytes.every((b) => b === 255);

const MAGNITUDE_CHUNK = 64;

const longPush = (
  mag: Extract<MagSink, { k: "long" }>,
  bytes: Buffer,
): MagSink => {
  let sink = mag.sink;
  let part = Buffer.concat([mag.part, bytes]);
  while (part.length >= MAGNITUDE_CHUNK) {
    sink = sinkPush(
      sink,
      Buffer.concat([
        Buffer.from([0x58, 0x40]),
        part.subarray(0, MAGNITUDE_CHUNK),
      ]),
    );
    part = part.subarray(MAGNITUDE_CHUNK);
  }
  return {
    k: "long",
    negative: mag.negative,
    sink,
    part: Buffer.from(part),
    total: mag.total + bytes.length,
    first: mag.first,
    ones: mag.ones && allOnes(bytes),
  };
};

export const magPush = (mag: MagSink, bytes: Uint8Array): MagSink => {
  const chunk = Buffer.from(bytes);
  if (mag.k === "long") return longPush(mag, chunk);
  const joined =
    mag.magnitude.length === 0
      ? Buffer.from(stripZeros(chunk))
      : Buffer.concat([mag.magnitude, chunk]);
  if (joined.length <= MAGNITUDE_CHUNK) {
    return { k: "short", negative: mag.negative, magnitude: joined };
  }
  const tag = Buffer.from(mag.negative ? [0xc3, 0x5f] : [0xc2, 0x5f]);
  return longPush(
    {
      k: "long",
      negative: mag.negative,
      sink: sinkPush(emptySink(), tag),
      part: joined.subarray(0, 1),
      total: 1,
      first: joined[0]!,
      ones: true,
    },
    joined.subarray(1),
  );
};

/** The summary of the integer and its memory size in words. */
export const magFinish = (mag: MagSink): readonly [Summary, bigint] => {
  if (mag.k === "short") {
    const m =
      mag.magnitude.length === 0
        ? 0n
        : BigInt(`0x${mag.magnitude.toString("hex")}`);
    const value = mag.negative ? -1n - m : m;
    return [integerSummary(value), midgardCekIntegerMemoryWords(value)];
  }
  const remainder = mag.part.length;
  const tail =
    remainder === 0
      ? Buffer.from([0xff])
      : remainder < 24
        ? Buffer.concat([
            Buffer.from([64 + remainder]),
            mag.part,
            Buffer.from([0xff]),
          ])
        : Buffer.concat([
            Buffer.from([88, remainder]),
            mag.part,
            Buffer.from([0xff]),
          ]);
  const closed = sinkPush(mag.sink, tail);
  const cborLength = BigInt(sinkLength(closed));
  const firstBits = bitLength8(mag.first);
  // |-1 - m| = m + 1 gains a bit when m is all one bits.
  const carry =
    mag.negative && mag.ones && mag.first === 2 ** firstBits - 1 ? 1 : 0;
  const words = BigInt(
    Math.floor((8 * (mag.total - 1) + firstBits + carry - 1) / 64) + 1,
  );
  return [integerNodeSummary(sinkRoot(closed), cborLength, 4n + words), words];
};

// ## UTF-8

/**
 * UTF-8 validation state: 0 between characters, -1 after an invalid byte,
 * otherwise `need * 65536 + low * 256 + high`.
 */
const utf8Byte = (state: number, byte: number): number => {
  if (state === 0) {
    if (byte < 128) return 0;
    if (byte < 194) return -1;
    if (byte < 224) return 65536 + 128 * 256 + 191;
    if (byte === 224) return 2 * 65536 + 160 * 256 + 191;
    if (byte === 237) return 2 * 65536 + 128 * 256 + 159;
    if (byte < 240) return 2 * 65536 + 128 * 256 + 191;
    if (byte === 240) return 3 * 65536 + 144 * 256 + 191;
    if (byte < 244) return 3 * 65536 + 128 * 256 + 191;
    if (byte === 244) return 3 * 65536 + 128 * 256 + 143;
    return -1;
  }
  const need = Math.floor(state / 65536);
  if (byte < Math.floor(state / 256) % 256 || byte > state % 256) return -1;
  if (need === 1) return 0;
  return (need - 1) * 65536 + 128 * 256 + 191;
};

export const utf8Push = (state: number, bytes: Uint8Array): number => {
  let next = state;
  for (const byte of bytes) {
    if (next < 0) return next;
    next = utf8Byte(next, byte);
  }
  return next;
};
