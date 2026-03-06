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

// fuel-types/src/canonical.rs:186 — pub const ALIGN: usize = 8
export const ALIGN = 8;

// fuel-types/src/canonical.rs:190 — const fn alignment_bytes(len: usize) -> usize
export function alignmentBytes(len: number): number {
  const mod = len % ALIGN;
  return mod === 0 ? 0 : ALIGN - mod;
}

// fuel-types/src/canonical.rs:196 — pub const fn aligned_size(len: usize) -> usize
export function alignedSize(len: number): number {
  return len + alignmentBytes(len);
}

// ---------------------------------------------------------------------------
// Writer
// fuel-types/src/canonical.rs:59  — pub trait Output
// fuel-types/src/canonical.rs:491 — impl Output for Vec<u8>
// ---------------------------------------------------------------------------

export class Writer {
  private readonly chunks: Uint8Array[] = [];

  // fuel-types/src/canonical.rs:61 — Output::write
  write(bytes: Uint8Array): void {
    if (bytes.length > 0) this.chunks.push(new Uint8Array(bytes));
  }

  // fuel-types/src/canonical.rs:64 — Output::push_byte
  pushByte(b: number): void {
    this.chunks.push(new Uint8Array([b & 0xff]));
  }

  // No direct equivalent; Fuel does `for _ in 0..n { buffer.push_byte(0)?; }` inline
  // e.g. fuel-types/src/canonical.rs:223 (zero-padding in impl_for_primitives encode_static)
  writeZeros(n: number): void {
    if (n > 0) this.chunks.push(new Uint8Array(n));
  }

  // fuel-types/src/canonical.rs:112 — Serialize::to_bytes
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
// fuel-types/src/canonical.rs:120 — pub trait Input
// fuel-types/src/canonical.rs:515 — impl Input for &'_ [u8]
// ---------------------------------------------------------------------------

export class Reader {
  private pos = 0;
  constructor(private readonly buf: Uint8Array) {}

  // fuel-types/src/canonical.rs:128 — Input::read
  read(n: number): Uint8Array {
    if (this.pos + n > this.buf.length)
      throw new Error(
        `BufferTooShort: need ${n}, have ${this.buf.length - this.pos}`,
      );
    const slice = this.buf.slice(this.pos, this.pos + n);
    this.pos += n;
    return slice;
  }

  // fuel-types/src/canonical.rs:145 — Input::skip
  skip(n: number): void {
    if (this.pos + n > this.buf.length)
      throw new Error(
        `BufferTooShort: skip ${n}, have ${this.buf.length - this.pos}`,
      );
    this.pos += n;
  }

  // fuel-types/src/canonical.rs:122 — Input::remaining
  remaining(): number {
    return this.buf.length - this.pos;
  }
}

// ---------------------------------------------------------------------------
// u64 helpers — JS numbers are safe up to 2^53; sufficient for our tests
// fuel-types/src/canonical.rs:249 — impl_for_primitives!(u64, false)
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
// i64 — signed 64-bit big-endian integer. Used for Mint amounts (can be negative).
// fuel-types/src/canonical.rs:249 — impl_for_primitives!(i64, false)  (same size as u64)
// ---------------------------------------------------------------------------

export function writeI64(w: Writer, n: number): void {
  const buf = new ArrayBuffer(8);
  const dv = new DataView(buf);
  dv.setInt32(0, Math.floor(n / 0x100000000), false);
  dv.setUint32(4, n >>> 0, false);
  w.write(new Uint8Array(buf));
}

export function readI64(r: Reader): number {
  const b = r.read(8);
  const dv = new DataView(b.buffer, b.byteOffset, 8);
  return dv.getInt32(0, false) * 0x100000000 + dv.getUint32(4, false);
}

// ---------------------------------------------------------------------------
// u16 — 6 zero bytes + 2 data bytes = 8 total
// fuel-types/src/canonical.rs:246 — impl_for_primitives!(u16, false)
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
// fuel-types/src/canonical.rs:245 — impl_for_primitives!(u8, true)
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
// No direct equivalent in fuel-types canonical.rs; Fuel uses #[repr(u8)] enums.
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
// fuel-types/src/canonical.rs:374 — impl<const N, T> Serialize for [T; N]
//   UNALIGNED_BYTES path (lines 406-413): write raw bytes then pad to ALIGN
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
// fuel-types/src/canonical.rs:279 — impl<T: Serialize> Serialize for Vec<T>
//   encode_static (line 301): writes length as u64
//   encode_dynamic (line 309): writes bytes + tail alignment padding
//   decode_static (line 332): reads length / allocates capacity
//   decode_dynamic (line 352): reads bytes + skips tail padding
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
