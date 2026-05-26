/**
 * Canonical fuel-vm-style binary codec primitives for the Midgard native tx
 * envelope and proof-critical preimages.
 *
 * Encoding rules:
 *   - Fixed-size ("static") fields are written first, then variable-size
 *     ("dynamic") fields.
 *   - All fields are 8-byte (64-bit) aligned; integers are big-endian.
 *   - A variable-length blob writes its length (u64) in the static section and
 *     its bytes (plus tail padding) in the dynamic section.
 */

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export const ALIGN = 8;

/** Number of zero bytes needed to pad `len` up to the next 8-byte boundary. */
export const alignmentBytes = (len: number): number => {
  const mod = len % ALIGN;
  return mod === 0 ? 0 : ALIGN - mod;
};

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    message,
    detail,
  );
};

// ---------------------------------------------------------------------------
// Writer / Reader
// ---------------------------------------------------------------------------

export class BinaryWriter {
  private readonly chunks: Buffer[] = [];

  write(bytes: Uint8Array): void {
    if (bytes.length > 0) this.chunks.push(Buffer.from(bytes));
  }

  writeZeros(n: number): void {
    if (n > 0) this.chunks.push(Buffer.alloc(n));
  }

  toBytes(): Buffer {
    return Buffer.concat(this.chunks);
  }
}

export class BinaryReader {
  private pos = 0;

  constructor(private readonly buf: Uint8Array) {}

  read(n: number): Buffer {
    if (this.pos + n > this.buf.length) {
      fail(
        "Unexpected end of binary input",
        `need=${n} have=${this.buf.length - this.pos}`,
      );
    }
    const slice = Buffer.from(this.buf.subarray(this.pos, this.pos + n));
    this.pos += n;
    return slice;
  }

  skip(n: number): void {
    if (this.pos + n > this.buf.length) {
      fail("Unexpected end of binary input", `skip=${n}`);
    }
    this.pos += n;
  }

  remaining(): number {
    return this.buf.length - this.pos;
  }
}

/** Throws if `r` still has unconsumed bytes (rejects trailing garbage). */
export const ensureNoTrailingBytes = (
  r: BinaryReader,
  fieldName: string,
): void => {
  if (r.remaining() !== 0) {
    fail(`${fieldName} has trailing bytes`, `remaining=${r.remaining()}`);
  }
};

// ---------------------------------------------------------------------------
// Integers
// ---------------------------------------------------------------------------

/** u64 from a JS number — used for collection / blob lengths. */
export const writeU64 = (w: BinaryWriter, n: number): void => {
  const buf = Buffer.alloc(8);
  buf.writeUInt32BE(Math.floor(n / 0x100000000), 0);
  buf.writeUInt32BE(n >>> 0, 4);
  w.write(buf);
};

export const readU64 = (r: BinaryReader): number => {
  const b = r.read(8);
  return b.readUInt32BE(0) * 0x100000000 + b.readUInt32BE(4);
};

/** Unsigned 64-bit big-endian integer as bigint (fee, network id, version). */
export const writeBigU64 = (w: BinaryWriter, n: bigint): void => {
  if (n < 0n || n > 0xffffffffffffffffn) {
    fail("u64 value out of range", n.toString());
  }
  const buf = Buffer.alloc(8);
  buf.writeBigUInt64BE(n, 0);
  w.write(buf);
};

export const readBigU64 = (r: BinaryReader): bigint =>
  r.read(8).readBigUInt64BE(0);

/** Signed 64-bit big-endian integer as bigint (validity interval bounds). */
export const writeBigI64 = (w: BinaryWriter, n: bigint): void => {
  if (n < -(2n ** 63n) || n >= 2n ** 63n) {
    fail("i64 value out of range", n.toString());
  }
  const buf = Buffer.alloc(8);
  buf.writeBigInt64BE(n, 0);
  w.write(buf);
};

export const readBigI64 = (r: BinaryReader): bigint =>
  r.read(8).readBigInt64BE(0);

/** u16 big-endian, padded to 8 bytes (6 zero bytes + 2 data bytes). */
export const writeU16 = (w: BinaryWriter, n: number): void => {
  if (!Number.isInteger(n) || n < 0 || n > 0xffff) {
    fail("u16 value out of range", String(n));
  }
  const buf = Buffer.alloc(8);
  buf.writeUInt16BE(n, 6);
  w.write(buf);
};

export const readU16 = (r: BinaryReader): number => r.read(8).readUInt16BE(6);

// ---------------------------------------------------------------------------
// Fixed-size byte arrays (raw bytes followed by tail alignment padding)
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
// Fixed 28-byte hash (Hash28: padded to 32) and 32-byte hash (no padding)
// ---------------------------------------------------------------------------

export const writeHash28 = (w: BinaryWriter, h: Uint8Array): void => {
  if (h.length !== 28) {
    fail("hash28 must be 28 bytes", `length=${h.length}`);
  }
  writeFixedBytes(w, h);
};

export const readHash28 = (r: BinaryReader): Buffer => readFixedBytes(r, 28);

export const writeHash32 = (w: BinaryWriter, h: Uint8Array): void => {
  if (h.length !== 32) {
    fail("hash32 must be 32 bytes", `length=${h.length}`);
  }
  w.write(h);
};

export const readHash32 = (r: BinaryReader): Buffer => r.read(32);

// ---------------------------------------------------------------------------
// Variable-length byte blobs
// ---------------------------------------------------------------------------

/** Static part of a variable blob: its length as u64. */
export const writeVarBytesStatic = (
  w: BinaryWriter,
  bytes: Uint8Array,
): void => {
  writeU64(w, bytes.length);
};

/** Dynamic part of a variable blob: the bytes plus tail alignment padding. */
export const writeVarBytesDynamic = (
  w: BinaryWriter,
  bytes: Uint8Array,
): void => {
  w.write(bytes);
  w.writeZeros(alignmentBytes(bytes.length));
};

export const readVarBytesLen = (r: BinaryReader): number => readU64(r);

export const readVarBytesDynamic = (r: BinaryReader, len: number): Buffer => {
  const bytes = r.read(len);
  r.skip(alignmentBytes(len));
  return bytes;
};
