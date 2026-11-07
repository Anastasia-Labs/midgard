/**
 * The byte reads of the forced-transaction admission screen, each the twin of
 * a read the tx-order mint makes. A read the mint would fail on throws
 * {@link Malformed}, which the screen reports as a refusal of the check that
 * made it.
 */

/** Field 2: an output map of two entries (address, value). */
export const TWO_ENTRY_OUTPUT_MAP_HEAD = 0xa2;

/** A read the mint would fail on. Caught and reported as a refusal. */
export class Malformed extends Error {}

export const byteAt = (bytes: Uint8Array, offset: number): number => {
  const value = bytes[offset];
  if (value === undefined) throw new Malformed(`no byte at ${offset}`);
  return value;
};

export const startsWithAt = (
  bytes: Uint8Array,
  offset: number,
  prefix: readonly number[],
): boolean =>
  offset + prefix.length <= bytes.length &&
  prefix.every((value, index) => bytes[offset + index] === value);

export const bigEndian = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): number => {
  let value = 0;
  for (let index = 0; index < width; index += 1) {
    value = value * 256 + byteAt(bytes, offset + index);
  }
  return value;
};

/** §5.1 array header: the offset of the first item and the item count. */
export const fieldHeader = (preimage: Uint8Array): [number, number] => {
  const tag = byteAt(preimage, 0);
  if (tag >= 0x80 && tag <= 0x97) return [1, tag - 0x80];
  if (tag === 0x98) return [2, byteAt(preimage, 1)];
  if (tag === 0x99) return [3, bigEndian(preimage, 1, 2)];
  throw new Malformed(`field header ${tag.toString(16)}`);
};

export const isEmptyField = (preimage: Uint8Array): boolean =>
  preimage.length === 1 && preimage[0] === 0x80;

/**
 * The width of the definite CBOR head at `offset`, which must have major type
 * `major`: the twin of the mint's `head_width`.
 */
export const headWidth = (
  bytes: Uint8Array,
  offset: number,
  major: number,
): number => {
  const additional = byteAt(bytes, offset) - major * 32;
  if (additional < 24) {
    if (additional < 0) throw new Malformed(`head below major ${major}`);
    return 1;
  }
  if (additional === 24) return 2;
  if (additional === 25) return 3;
  if (additional === 26) return 5;
  if (additional === 27) return 9;
  throw new Malformed(`head ${byteAt(bytes, offset)} at ${offset}`);
};

/** The argument of the CBOR head of width `width` at `offset`. */
export const headArgument = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): number => {
  if (width === 1) return byteAt(bytes, offset) % 32;
  if (width === 2) return byteAt(bytes, offset + 1);
  if (offset + width > bytes.length) {
    throw new Malformed(`head cut short at ${offset}`);
  }
  return bigEndian(bytes, offset + 1, width - 1);
};

/**
 * Offset one past the byte string whose head is at `offset`, the twin of the
 * mint's `skip_bytes`, short paths included: below `40` it reads another major
 * type by the same arithmetic.
 */
export const skipBytes = (bytes: Uint8Array, offset: number): number => {
  const tag = byteAt(bytes, offset);
  if (tag < 0x58) return offset + tag - 0x3f;
  if (tag === 0x58) return offset + 2 + byteAt(bytes, offset + 1);
  const width = headWidth(bytes, offset, 2);
  return offset + width + headArgument(bytes, offset, width);
};

/**
 * Visits each §5.1 item's payload `[start, end)` from the end of the header to
 * the end of the field, the twin of the mint's `variable_items_admitted`.
 */
export const eachVariableItem = (
  preimage: Uint8Array,
  visit: (start: number, end: number) => string | null,
): string | null => {
  const [header] = fieldHeader(preimage);
  for (let offset = header, index = 0; offset < preimage.length; index += 1) {
    const tag = byteAt(preimage, offset);
    const start = offset + (tag < 0x58 ? 1 : tag === 0x58 ? 2 : 3);
    const end = skipBytes(preimage, offset);
    const reason = visit(start, end);
    if (reason !== null) return `item ${index}: ${reason}`;
    offset = end;
  }
  return null;
};

/**
 * The big-endian argument of `width` bytes after the head at `offset`, the
 * mint's decoders' arithmetic; a read past the field fails as the mint's does.
 */
export const headValue = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): bigint => {
  let value = 0n;
  for (let index = 1; index <= width; index += 1) {
    value = value * 256n + BigInt(byteAt(bytes, offset + index));
  }
  return value;
};

/**
 * A canonical (minimal) head of `major` with at most four argument bytes, the
 * shared arithmetic of `decode_canonical_bytes_at` and
 * `decode_canonical_map_header_at`: the offset after the head and its argument.
 */
export const canonicalHeadAt = (
  bytes: Uint8Array,
  offset: number,
  major: number,
): [number, bigint] => {
  const additional = byteAt(bytes, offset) - major * 32;
  if (additional >= 0 && additional < 24) {
    return [offset + 1, BigInt(additional)];
  }
  const widths = new Map([
    [24, [1, 24n]],
    [25, [2, 0x100n]],
    [26, [4, 0x10000n]],
  ] as const);
  const shape = widths.get(additional as 24 | 25 | 26);
  if (shape === undefined) {
    throw new Malformed(`head ${byteAt(bytes, offset)} at ${offset}`);
  }
  const [width, least] = shape;
  const value = headValue(bytes, offset, width);
  if (value < least) throw new Malformed(`non-minimal head at ${offset}`);
  return [offset + 1 + width, value];
};

/**
 * A canonical byte string, the twin of `decode_canonical_bytes_at`: the offset
 * after it and its bytes, cut short at the end of the field as the mint's
 * slice is.
 */
export const canonicalBytesAt = (
  bytes: Uint8Array,
  offset: number,
): [number, Uint8Array] => {
  const [start, length] = canonicalHeadAt(bytes, offset, 2);
  const end = start + Number(length);
  return [end, bytes.subarray(start, end)];
};

/**
 * A canonical integer, the twin of `decode_canonical_int_at`: a minimal
 * unsigned or negative integer with at most eight argument bytes.
 */
export const canonicalIntAt = (
  bytes: Uint8Array,
  offset: number,
): [number, bigint] => {
  const tag = byteAt(bytes, offset);
  const major = tag < 0x20 ? 0 : 1;
  const additional = tag - major * 32;
  if (tag >= 0x40 || additional > 27) {
    throw new Malformed(`integer head ${tag.toString(16)} at ${offset}`);
  }
  let next: number;
  let argument: bigint;
  if (additional < 24) {
    [next, argument] = [offset + 1, BigInt(additional)];
  } else {
    const width = 2 ** (additional - 24);
    argument = headValue(bytes, offset, width);
    const least = additional === 24 ? 24n : 1n << BigInt(4 * width);
    if (argument < least) {
      throw new Malformed(`non-minimal integer at ${offset}`);
    }
    next = offset + 1 + width;
  }
  return [next, major === 0 ? argument : -1n - argument];
};
