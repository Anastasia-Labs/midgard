/**
 * Canonical binary codec for Midgard types.
 *
 * Encoding rules (mirrors midgard/src/codec.rs):
 *   - All fields are aligned to 8-byte (64-bit) boundaries.
 *   - Fixed-size ("static") fields are encoded first, then variable-size
 *     ("dynamic") fields.
 *   - Integers are big-endian, left-zero-padded to an 8-byte boundary.
 *   - Vec<T> encodes its length (u64) in the static section and its
 *     elements in the dynamic section.
 *   - Option<T> encodes a u64 presence flag (0 or 1) followed by the
 *     static part of T (if present); the dynamic part of T follows in
 *     the dynamic section.
 */

export const ALIGN = 8;

export function alignmentBytes(len: number): number {
  const mod = len % ALIGN;
  return mod === 0 ? 0 : ALIGN - mod;
}

export function alignedSize(len: number): number {
  return len + alignmentBytes(len);
}

// ---------------------------------------------------------------------------
// Writer
// ---------------------------------------------------------------------------

export class Writer {
  private readonly chunks: Uint8Array[] = [];

  write(bytes: Uint8Array): void {
    if (bytes.length > 0) this.chunks.push(new Uint8Array(bytes));
  }

  pushByte(b: number): void {
    this.chunks.push(new Uint8Array([b & 0xff]));
  }

  writeZeros(n: number): void {
    if (n > 0) this.chunks.push(new Uint8Array(n));
  }

  toBytes(): Uint8Array {
    let size = 0;
    for (const c of this.chunks) size += c.length;
    const out = new Uint8Array(size);
    let off = 0;
    for (const c of this.chunks) {
      out.set(c, off);
      off += c.length;
    }
    return out;
  }
}

// ---------------------------------------------------------------------------
// Reader
// ---------------------------------------------------------------------------

export class Reader {
  private pos = 0;
  constructor(private readonly buf: Uint8Array) {}

  read(n: number): Uint8Array {
    if (this.pos + n > this.buf.length)
      throw new Error(
        `BufferTooShort: need ${n}, have ${this.buf.length - this.pos}`,
      );
    const slice = this.buf.slice(this.pos, this.pos + n);
    this.pos += n;
    return slice;
  }

  skip(n: number): void {
    if (this.pos + n > this.buf.length)
      throw new Error(
        `BufferTooShort: skip ${n}, have ${this.buf.length - this.pos}`,
      );
    this.pos += n;
  }

  remaining(): number {
    return this.buf.length - this.pos;
  }
}

// ---------------------------------------------------------------------------
// u64 helpers — JS numbers are safe up to 2^53; sufficient for our tests
// ---------------------------------------------------------------------------

export function writeU64(w: Writer, n: number): void {
  const hi = Math.floor(n / 0x100000000);
  const lo = n >>> 0;
  const buf = new Uint8Array(8);
  buf[0] = (hi >>> 24) & 0xff;
  buf[1] = (hi >>> 16) & 0xff;
  buf[2] = (hi >>> 8) & 0xff;
  buf[3] = hi & 0xff;
  buf[4] = (lo >>> 24) & 0xff;
  buf[5] = (lo >>> 16) & 0xff;
  buf[6] = (lo >>> 8) & 0xff;
  buf[7] = lo & 0xff;
  w.write(buf);
}

export function readU64(r: Reader): number {
  const b = r.read(8);
  const hi = b[0] * 0x1000000 + ((b[1] << 16) | (b[2] << 8) | b[3]);
  const lo = b[4] * 0x1000000 + ((b[5] << 16) | (b[6] << 8) | b[7]);
  return hi * 0x100000000 + lo;
}

// ---------------------------------------------------------------------------
// u16 — 6 zero bytes + 2 data bytes = 8 total
// ---------------------------------------------------------------------------

export function writeU16(w: Writer, n: number): void {
  w.writeZeros(6);
  const buf = new Uint8Array(2);
  new DataView(buf.buffer).setUint16(0, n & 0xffff, false);
  w.write(buf);
}

export function readU16(r: Reader): number {
  r.skip(6);
  const b = r.read(2);
  return new DataView(b.buffer, b.byteOffset, 2).getUint16(0, false);
}

// ---------------------------------------------------------------------------
// u8 — 7 zero bytes + 1 data byte = 8 total (UNALIGNED_BYTES=true inside Vec<u8>)
// ---------------------------------------------------------------------------

export function writeU8(w: Writer, n: number): void {
  w.writeZeros(7);
  w.pushByte(n);
}

export function readU8(r: Reader): number {
  r.skip(7);
  return r.read(1)[0];
}

// ---------------------------------------------------------------------------
// bool — encoded as u8 (padded to 8 bytes)
// ---------------------------------------------------------------------------

export function writeBool(w: Writer, b: boolean): void {
  writeU8(w, b ? 1 : 0);
}

export function readBool(r: Reader): boolean {
  return readU8(r) !== 0;
}

// ---------------------------------------------------------------------------
// Fixed-size byte arrays  (Hash28/32, VKey, Signature)
// bytes written as-is, then zero-padded to next 8-byte boundary
// ---------------------------------------------------------------------------

export function writeFixedBytes(w: Writer, bytes: Uint8Array): void {
  w.write(bytes);
  w.writeZeros(alignmentBytes(bytes.length));
}

export function readFixedBytes(r: Reader, len: number): Uint8Array {
  const bytes = r.read(len);
  r.skip(alignmentBytes(len));
  return bytes;
}

// ---------------------------------------------------------------------------
// Variable-length byte blobs (Address, AssetName, Vec<u8> raw fields)
// Static section: u64 length.  Dynamic section: bytes + alignment padding.
// ---------------------------------------------------------------------------

export function writeVarBytesStatic(w: Writer, bytes: Uint8Array): void {
  writeU64(w, bytes.length);
}

export function writeVarBytesDynamic(w: Writer, bytes: Uint8Array): void {
  w.write(bytes);
  w.writeZeros(alignmentBytes(bytes.length));
}

export function readVarBytesLen(r: Reader): number {
  return readU64(r);
}

export function readVarBytesDynamic(r: Reader, len: number): Uint8Array {
  const bytes = r.read(len);
  r.skip(alignmentBytes(len));
  return bytes;
}
