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

export const ALIGN = 8;

export const alignmentBytes = (len: number): number => {
  const mod = len % ALIGN;
  return mod === 0 ? 0 : ALIGN - mod;
};

export const alignedSize = (len: number): number => len + alignmentBytes(len);

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

  writeZeros(n: number): void {
    if (n > 0) this.chunks.push(new Uint8Array(n));
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
// u64 (bigint) — full 8 byte big-endian.
// ---------------------------------------------------------------------------

const U64_MAX = (1n << 64n) - 1n;

export const writeBigU64 = (w: BinaryWriter, n: bigint): void => {
  if (n < 0n || n > U64_MAX) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u64 out of range",
      n.toString(10),
    );
  }
  const buf = Buffer.alloc(8);
  buf.writeBigUInt64BE(n, 0);
  w.write(buf);
};

export const readBigU64 = (r: BinaryReader): bigint =>
  r.read(8).readBigUInt64BE(0);

// ---------------------------------------------------------------------------
// i64 (bigint) — two's complement big-endian.
// ---------------------------------------------------------------------------

const I64_MIN = -(1n << 63n);
const I64_MAX = (1n << 63n) - 1n;

export const writeBigI64 = (w: BinaryWriter, n: bigint): void => {
  if (n < I64_MIN || n > I64_MAX) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "i64 out of range",
      n.toString(10),
    );
  }
  const buf = Buffer.alloc(8);
  buf.writeBigInt64BE(n, 0);
  w.write(buf);
};

export const readBigI64 = (r: BinaryReader): bigint =>
  r.read(8).readBigInt64BE(0);

// ---------------------------------------------------------------------------
// u64 as number — convenience for indices/lengths (assumed < 2^53).
// ---------------------------------------------------------------------------

const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);

export const writeU64 = (w: BinaryWriter, n: number): void => {
  if (!Number.isInteger(n) || n < 0) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u64 (number) must be a non-negative integer",
      String(n),
    );
  }
  writeBigU64(w, BigInt(n));
};

export const readU64 = (r: BinaryReader): number => {
  const big = readBigU64(r);
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
// u16 — 6 zero bytes + 2 data bytes = 8 total.
// ---------------------------------------------------------------------------

export const writeU16 = (w: BinaryWriter, n: number): void => {
  if (!Number.isInteger(n) || n < 0 || n > 0xffff) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "u16 out of range",
      String(n),
    );
  }
  w.writeZeros(6);
  const buf = Buffer.alloc(2);
  buf.writeUInt16BE(n, 0);
  w.write(buf);
};

export const readU16 = (r: BinaryReader): number => {
  r.skip(6);
  return r.read(2).readUInt16BE(0);
};

// ---------------------------------------------------------------------------
// u8 / bool — padded to 8 bytes.
// ---------------------------------------------------------------------------

export const writeU8 = (w: BinaryWriter, n: number): void => {
  w.writeZeros(7);
  w.pushByte(n);
};

export const readU8 = (r: BinaryReader): number => {
  r.skip(7);
  return r.read(1)[0];
};

export const writeBool = (w: BinaryWriter, b: boolean): void =>
  writeU8(w, b ? 1 : 0);

export const readBool = (r: BinaryReader): boolean => readU8(r) !== 0;

// ---------------------------------------------------------------------------
// Fixed-size byte arrays (e.g. Hash28/32, VKey, Signature).
// ---------------------------------------------------------------------------

export const writeFixedBytes = (w: BinaryWriter, bytes: Uint8Array): void => {
  w.write(bytes);
  w.writeZeros(alignmentBytes(bytes.length));
};

export const readFixedBytes = (r: BinaryReader, len: number): Buffer => {
  const bytes = r.read(len);
  r.skip(alignmentBytes(len));
  return bytes;
};

// ---------------------------------------------------------------------------
// Variable-length byte blobs (Address, AssetName, opaque Plutus payloads).
// Static = u64 length. Dynamic = bytes + alignment padding.
// ---------------------------------------------------------------------------

export const writeVarBytesStatic = (w: BinaryWriter, bytes: Uint8Array): void =>
  writeU64(w, bytes.length);

export const writeVarBytesDynamic = (w: BinaryWriter, bytes: Uint8Array): void => {
  w.write(bytes);
  w.writeZeros(alignmentBytes(bytes.length));
};

export const readVarBytesLen = (r: BinaryReader): number => readU64(r);

export const readVarBytesDynamic = (r: BinaryReader, len: number): Buffer => {
  const bytes = r.read(len);
  r.skip(alignmentBytes(len));
  return bytes;
};

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
