/**
 * Canonical binary codec primitives for Midgard native transactions.
 *
 * Mirrors the staging-branch `midgard-ts` codec (which mirrors Fuel's
 * `fuel-types::canonical`). All fields are 8-byte aligned. Composite types
 * encode a "static" section (length + presence + fixed-size data) followed
 * by a "dynamic" section (variable bytes + nested dynamic sections).
 *
 * Source of truth for the layout: staging:demo/midgard-ts/src/codec.ts
 */

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export class BinaryWriter {
  private readonly chunks: Uint8Array[] = [];

  write(bytes: Uint8Array): void {
    if (bytes.length > 0) {
      this.chunks.push(bytes instanceof Uint8Array ? bytes : new Uint8Array(bytes));
    }
  }

  pushByte(b: number): void {
    this.chunks.push(new Uint8Array([b & 0xff]));
  }

  toBytes(): Buffer {
    let size = 0;
    for (const c of this.chunks) size += c.length;
    const out = Buffer.alloc(size);
    let off = 0;
    for (const c of this.chunks) {
      out.set(c, off);
      off += c.length;
    }
    return out;
  }
}

export class BinaryReader {
  private pos = 0;
  constructor(private readonly buf: Uint8Array) {}

  read(n: number): Buffer {
    if (this.pos + n > this.buf.length) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        "Binary reader: buffer too short",
        `need=${n} have=${this.buf.length - this.pos}`,
      );
    }
    const slice = Buffer.from(this.buf.subarray(this.pos, this.pos + n));
    this.pos += n;
    return slice;
  }

  skip(n: number): void {
    if (this.pos + n > this.buf.length) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        "Binary reader: cannot skip past end",
        `skip=${n} have=${this.buf.length - this.pos}`,
      );
    }
    this.pos += n;
  }

  remaining(): number {
    return this.buf.length - this.pos;
  }

  offset(): number {
    return this.pos;
  }

  expectEnd(fieldName: string): void {
    if (this.pos !== this.buf.length) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        `Trailing bytes after ${fieldName}`,
        `offset=${this.pos} length=${this.buf.length}`,
      );
    }
  }
}

// ---------------------------------------------------------------------------
// Integer encoding (Phase 2 size reduction): LEB128 varint for unsigned,
// ZigZag + LEB128 for signed. The function names below still read like
// fixed-width types because callers think of them as "the u64/i64/u16
// field" — only the wire format is variable-length now.
//
// Unsigned LEB128:
//   - 7 bits payload per byte, MSB is the continuation flag.
//   - 0 → 1 byte. Values < 128 → 1 byte. < 2^14 → 2 bytes. ... < 2^63 → 9.
//
// Signed: ZigZag-encode (n << 1) ^ (n >> 63), then LEB128 the result.
// ---------------------------------------------------------------------------

const U64_MAX = (1n << 64n) - 1n;
const I64_MIN = -(1n << 63n);
const I64_MAX = (1n << 63n) - 1n;
const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);
// Max bytes for a u64 LEB128 = ceil(64 / 7) = 10. Cap reads at 10 so a
// malformed input can't loop forever.
const MAX_VARUINT_BYTES = 10;

const writeVarUintBigint = (w: BinaryWriter, n: bigint): void => {
  // Fast path: values fitting in a u32 (the vast majority — lengths,
  // indexes, fees in ada-only txs, small counts) skip BigInt arithmetic.
  // Allocate the output buffer at its exact size to avoid both
  // intermediate arrays and aliasing through any shared scratch.
  if (n <= 0xffffffffn) {
    let value = Number(n);
    // Compute byte count first so we can allocate exactly.
    let probe = value;
    let len = 1;
    while (probe >= 0x80) {
      len += 1;
      probe >>>= 7;
    }
    const out = new Uint8Array(len);
    for (let i = 0; i < len - 1; i += 1) {
      out[i] = (value & 0x7f) | 0x80;
      value >>>= 7;
    }
    out[len - 1] = value;
    w.write(out);
    return;
  }
  // Slow path: full u64 range. Few real fields hit this (only big lovelace
  // / asset quantities), so the cost is acceptable.
  let value = n;
  let probe = value;
  let len = 1;
  while (probe >= 0x80n) {
    len += 1;
    probe >>= 7n;
  }
  const out = new Uint8Array(len);
  for (let i = 0; i < len - 1; i += 1) {
    out[i] = Number((value & 0x7fn) | 0x80n);
    value >>= 7n;
  }
  out[len - 1] = Number(value);
  w.write(out);
};

const readVarUintBigint = (r: BinaryReader): bigint => {
  let result = 0n;
  let shift = 0n;
  for (let i = 0; i < MAX_VARUINT_BYTES; i += 1) {
    const byte = r.read(1)[0];
    result |= BigInt(byte & 0x7f) << shift;
    if ((byte & 0x80) === 0) {
      if (result > U64_MAX) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.InvalidFieldType,
          "varuint > u64",
          result.toString(10),
        );
      }
      return result;
    }
    shift += 7n;
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    "varuint did not terminate within 10 bytes",
    "",
  );
};

export const writeBigU64 = (w: BinaryWriter, n: bigint): void => {
  if (n < 0n || n > U64_MAX) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u64 out of range",
      n.toString(10),
    );
  }
  writeVarUintBigint(w, n);
};

export const readBigU64 = (r: BinaryReader): bigint => readVarUintBigint(r);

export const writeBigI64 = (w: BinaryWriter, n: bigint): void => {
  if (n < I64_MIN || n > I64_MAX) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "i64 out of range",
      n.toString(10),
    );
  }
  // ZigZag: (n << 1) ^ (n >> 63). For bigint, `n >> 63` is arithmetic.
  const zigzag = (n << 1n) ^ (n >> 63n);
  writeVarUintBigint(w, zigzag);
};

export const readBigI64 = (r: BinaryReader): bigint => {
  const zigzag = readVarUintBigint(r);
  // Inverse ZigZag: (n >>> 1) ^ -(n & 1)
  return (zigzag >> 1n) ^ -(zigzag & 1n);
};

export const writeU64 = (w: BinaryWriter, n: number): void => {
  if (!Number.isInteger(n) || n < 0) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u64 (number) must be a non-negative integer",
      String(n),
    );
  }
  writeVarUintBigint(w, BigInt(n));
};

export const readU64 = (r: BinaryReader): number => {
  const big = readVarUintBigint(r);
  if (big > MAX_SAFE) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u64 exceeds JS safe integer range",
      big.toString(10),
    );
  }
  return Number(big);
};

// ---------------------------------------------------------------------------
// u16 — varint (Phase 2). Most indexes are < 128 → 1 byte.
// ---------------------------------------------------------------------------

export const writeU16 = (w: BinaryWriter, n: number): void => {
  if (!Number.isInteger(n) || n < 0 || n > 0xffff) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u16 out of range",
      String(n),
    );
  }
  writeVarUintBigint(w, BigInt(n));
};

export const readU16 = (r: BinaryReader): number => {
  const big = readVarUintBigint(r);
  if (big > 0xffffn) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u16 out of range",
      big.toString(10),
    );
  }
  return Number(big);
};

// ---------------------------------------------------------------------------
// u8 / bool — single byte.
// ---------------------------------------------------------------------------

export const writeU8 = (w: BinaryWriter, n: number): void => {
  w.pushByte(n);
};

export const readU8 = (r: BinaryReader): number => r.read(1)[0];

export const writeBool = (w: BinaryWriter, b: boolean): void =>
  writeU8(w, b ? 1 : 0);

export const readBool = (r: BinaryReader): boolean => readU8(r) !== 0;

// ---------------------------------------------------------------------------
// Fixed-size byte arrays (e.g. Hash28/32, VKey, Signature). No padding.
// ---------------------------------------------------------------------------

export const writeFixedBytes = (w: BinaryWriter, bytes: Uint8Array): void => {
  w.write(bytes);
};

export const readFixedBytes = (r: BinaryReader, len: number): Buffer =>
  r.read(len);

// ---------------------------------------------------------------------------
// Variable-length byte blobs (Address, AssetName, opaque Plutus payloads).
// Static = u64 length. Dynamic = raw bytes (no padding).
// ---------------------------------------------------------------------------

export const writeVarBytesStatic = (w: BinaryWriter, bytes: Uint8Array): void =>
  writeU64(w, bytes.length);

export const writeVarBytesDynamic = (w: BinaryWriter, bytes: Uint8Array): void => {
  w.write(bytes);
};

export const readVarBytesLen = (r: BinaryReader): number => readU64(r);

export const readVarBytesDynamic = (r: BinaryReader, len: number): Buffer =>
  r.read(len);

/** Encode a single variable-length byte blob (length + bytes + pad). */
export const writeVarBytes = (w: BinaryWriter, bytes: Uint8Array): void => {
  writeVarBytesStatic(w, bytes);
  writeVarBytesDynamic(w, bytes);
};

export const readVarBytes = (r: BinaryReader): Buffer => {
  const len = readVarBytesLen(r);
  return readVarBytesDynamic(r, len);
};

// ---------------------------------------------------------------------------
// Encode/decode helpers: build a static+dynamic encoder from two writer halves.
// ---------------------------------------------------------------------------

export const concatStaticDynamic = (
  staticBytes: Buffer,
  dynamicBytes: Buffer,
): Buffer => {
  if (dynamicBytes.length === 0) return staticBytes;
  if (staticBytes.length === 0) return dynamicBytes;
  return Buffer.concat([staticBytes, dynamicBytes]);
};
