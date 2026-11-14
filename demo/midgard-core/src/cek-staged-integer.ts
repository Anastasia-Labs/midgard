/**
 * Integers read from and written to their canonical Plutus Data CBOR, and
 * sign-magnitude arithmetic in 512-byte limbs: the TypeScript twin of
 * `cek_staged_integer_v1`.
 *
 * The canonical CBOR of `I n` is major type 0 (n >= 0) or 1 (n < 0) with the
 * minimal head while the CBOR argument (n, or -n - 1 when negative) fits 64
 * bits, and otherwise tag 2 or 3 over the argument's minimal big-endian bytes:
 * one definite string up to 64 bytes, an indefinite string of 64-byte
 * segments (only the last shorter, closed by a break) above.
 */
import { type Bytes } from "./cek-proof.encode-midgard-cek-term-node.js";
import { hashMidgardCekDataNode } from "./cek-semantic.decode-midgard-cek-data-node.js";
import { type Hash32 } from "./codec/hash.js";

export const MIDGARD_CEK_STAGED_LIMB_BYTES = 512 as const;
const LIMB_BASE = 1n << BigInt(8 * MIDGARD_CEK_STAGED_LIMB_BYTES);
const SEGMENT_BYTES = 64;
const UINT64_LIMIT = 1n << 64n;

/** An integer as its canonical CBOR spells it; the argument is minimal. */
export type MidgardCekCanonicalInteger = {
  readonly negative: boolean;
  readonly argument: Buffer;
};

/** Sign and magnitude limbs, least significant first, no zero top limb. */
export type MidgardCekSignedLimbs = {
  readonly negative: boolean;
  readonly magnitude: readonly bigint[];
};

const refuse = (message: string): never => {
  throw new Error(`non-canonical integer CBOR: ${message}`);
};

const bigEndian = (bytes: Uint8Array): bigint =>
  bytes.length === 0 ? 0n : BigInt(`0x${Buffer.from(bytes).toString("hex")}`);

const minimalBytes = (value: bigint): Buffer => {
  if (value < 0n) throw new RangeError("value is negative");
  if (value === 0n) return Buffer.alloc(0);
  const digits = value.toString(16);
  return Buffer.from(digits.length % 2 === 0 ? digits : `0${digits}`, "hex");
};

const paddedBytes = (value: bigint, size: number): Buffer => {
  const bytes = minimalBytes(value);
  if (bytes.length > size) throw new RangeError("value does not fit");
  return Buffer.concat([Buffer.alloc(size - bytes.length), bytes]);
};

const bignumArgument = (cbor: Buffer): Buffer => {
  const head = cbor[1];
  let argument: Buffer;
  if (head === undefined) return refuse("truncated bignum");
  if (head === 0x5f) {
    if (cbor[cbor.length - 1] !== 0xff) refuse("missing break");
    const breakAt = cbor.length - 1;
    const segments: Buffer[] = [];
    let offset = 2;
    for (;;) {
      const segmentHead = cbor[offset];
      if (segmentHead === undefined || offset >= breakAt) {
        return refuse("truncated segment");
      }
      let size: number;
      let start: number;
      if (segmentHead === 0x58) {
        size = cbor[offset + 1] ?? refuse("truncated segment head");
        if (size < 24) refuse("non-minimal segment head");
        start = offset + 2;
      } else if (segmentHead > 0x40 && segmentHead < 0x58) {
        size = segmentHead - 0x40;
        start = offset + 1;
      } else {
        return refuse("segment is not a definite byte string of 1 to 64");
      }
      if (size > SEGMENT_BYTES) refuse("segment over 64 bytes");
      const end = start + size;
      if (end > breakAt) refuse("segment overruns the break");
      segments.push(cbor.subarray(start, end));
      if (end === breakAt) break;
      if (size !== SEGMENT_BYTES) refuse("short segment before the last");
      offset = end;
    }
    argument = Buffer.concat(segments);
    if (argument.length <= SEGMENT_BYTES)
      refuse("indefinite form of 64 or fewer bytes");
  } else if (head === 0x58) {
    const size = cbor[2] ?? refuse("truncated head");
    if (size < 24 || size > SEGMENT_BYTES) refuse("definite size out of range");
    if (cbor.length !== 3 + size) refuse("length mismatch");
    argument = cbor.subarray(3);
  } else {
    if (head < 0x49 || head >= 0x58)
      refuse("bignum of 8 or fewer bytes, or bad head");
    if (cbor.length !== head - 0x3e) refuse("length mismatch");
    argument = cbor.subarray(2);
  }
  if (argument[0] === 0) refuse("leading zero byte");
  return Buffer.from(argument);
};

/** Reads an Integer's canonical CBOR, refusing every other spelling. */
export const parseMidgardCekCanonicalIntegerCbor = (
  bytes: Bytes,
): MidgardCekCanonicalInteger => {
  const cbor = Buffer.from(bytes);
  const initial = cbor[0];
  if (initial === undefined) return refuse("empty");
  if (initial < 0x40) {
    const negative = initial >= 0x20;
    const additional = initial & 0x1f;
    let value: bigint;
    if (additional < 24) {
      if (cbor.length !== 1) refuse("trailing bytes");
      value = BigInt(additional);
    } else {
      const extra = { 24: 1, 25: 2, 26: 4, 27: 8 }[additional];
      if (extra === undefined) return refuse("reserved head");
      if (cbor.length !== 1 + extra) refuse("length mismatch");
      value = bigEndian(cbor.subarray(1));
      const minimum = { 1: 24n, 2: 256n, 4: 65_536n, 8: 4_294_967_296n }[
        extra
      ]!;
      if (value < minimum) refuse("non-minimal head");
    }
    return Object.freeze({ negative, argument: minimalBytes(value) });
  }
  if (initial === 0xc2 || initial === 0xc3) {
    return Object.freeze({
      negative: initial === 0xc3,
      argument: bignumArgument(cbor),
    });
  }
  return refuse("not an integer");
};

/** The integer's value. */
export const midgardCekCanonicalIntegerValue = (
  integer: MidgardCekCanonicalInteger,
): bigint => {
  const argument = bigEndian(integer.argument);
  return integer.negative ? -argument - 1n : argument;
};

/** The canonical spelling of `value`. */
export const midgardCekCanonicalIntegerOf = (
  value: bigint,
): MidgardCekCanonicalInteger =>
  Object.freeze({
    negative: value < 0n,
    argument: minimalBytes(value < 0n ? -value - 1n : value),
  });

const definiteHead = (size: number): Buffer =>
  size < 24 ? Buffer.from([0x40 + size]) : Buffer.from([0x58, size]);

const majorHead = (major: number, value: bigint): Buffer => {
  const top = major << 5;
  if (value < 24n) return Buffer.from([top + Number(value)]);
  if (value < 256n) return Buffer.from([top + 24, Number(value)]);
  if (value < 65_536n)
    return Buffer.concat([Buffer.from([top + 25]), paddedBytes(value, 2)]);
  if (value < 4_294_967_296n) {
    return Buffer.concat([Buffer.from([top + 26]), paddedBytes(value, 4)]);
  }
  return Buffer.concat([Buffer.from([top + 27]), paddedBytes(value, 8)]);
};

/** The canonical CBOR of an Integer. */
export const encodeMidgardCekCanonicalIntegerCbor = (
  integer: MidgardCekCanonicalInteger,
): Buffer => {
  const argument = Buffer.from(integer.argument);
  if (argument.length > 0 && argument[0] === 0) {
    throw new RangeError("argument is not minimal");
  }
  const value = bigEndian(argument);
  if (value < UINT64_LIMIT) {
    return majorHead(integer.negative ? 1 : 0, value);
  }
  const tag = Buffer.from([integer.negative ? 0xc3 : 0xc2]);
  if (argument.length <= SEGMENT_BYTES) {
    return Buffer.concat([tag, definiteHead(argument.length), argument]);
  }
  const parts: Buffer[] = [tag, Buffer.from([0x5f])];
  for (let offset = 0; offset < argument.length; offset += SEGMENT_BYTES) {
    const segment = argument.subarray(
      offset,
      Math.min(offset + SEGMENT_BYTES, argument.length),
    );
    parts.push(definiteHead(segment.length), segment);
  }
  parts.push(Buffer.from([0xff]));
  return Buffer.concat(parts);
};

/** Cardano's memory words of the integer: of its magnitude, one for zero. */
export const midgardCekCanonicalIntegerMagnitudeWords = (
  integer: MidgardCekCanonicalInteger,
): bigint => {
  const argument = bigEndian(integer.argument);
  const magnitude = integer.negative ? argument + 1n : argument;
  if (magnitude === 0n) return 1n;
  return BigInt(Math.ceil(minimalBytes(magnitude).length / 8));
};

/** The Data summary of an Integer leaf from its CBOR root, length and words. */
export const midgardCekIntegerLeafFromCbor = ({
  cborRoot,
  cborLength,
  words,
}: {
  readonly cborRoot: Bytes;
  readonly cborLength: bigint;
  readonly words: bigint;
}): {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
} => {
  const memory = 4n + words;
  return Object.freeze({
    root: hashMidgardCekDataNode({
      kind: "integer",
      cborRoot,
      cborLength,
      memory,
    }),
    cborLength,
    memory,
  });
};

/** The limbs of a magnitude given as minimal big-endian bytes. */
export const toMidgardCekLimbs = (magnitude: Bytes): bigint[] => {
  const bytes = Buffer.from(magnitude);
  const limbs: bigint[] = [];
  for (let end = bytes.length; end > 0; end -= MIDGARD_CEK_STAGED_LIMB_BYTES) {
    limbs.push(
      bigEndian(
        bytes.subarray(Math.max(0, end - MIDGARD_CEK_STAGED_LIMB_BYTES), end),
      ),
    );
  }
  return limbs;
};

/** The minimal big-endian bytes of a magnitude in limbs. */
export const fromMidgardCekLimbs = (limbs: readonly bigint[]): Buffer => {
  let top = limbs.length;
  while (top > 0 && limbs[top - 1] === 0n) top -= 1;
  if (top === 0) return Buffer.alloc(0);
  const parts = [minimalBytes(limbs[top - 1]!)];
  for (let index = top - 2; index >= 0; index -= 1) {
    parts.push(paddedBytes(limbs[index]!, MIDGARD_CEK_STAGED_LIMB_BYTES));
  }
  return Buffer.concat(parts);
};

const normalized = (limbs: bigint[]): bigint[] => {
  while (limbs.length > 0 && limbs[limbs.length - 1] === 0n) limbs.pop();
  return limbs;
};

/** The sum of two magnitudes. */
export const addMidgardCekLimbs = (
  left: readonly bigint[],
  right: readonly bigint[],
): bigint[] => {
  const result: bigint[] = [];
  let carry = 0n;
  for (let index = 0; index < Math.max(left.length, right.length); index += 1) {
    const sum = (left[index] ?? 0n) + (right[index] ?? 0n) + carry;
    carry = sum >= LIMB_BASE ? 1n : 0n;
    result.push(sum - carry * LIMB_BASE);
  }
  if (carry > 0n) result.push(carry);
  return normalized(result);
};

/** The difference of two magnitudes; refuses `left < right`. */
export const subMidgardCekLimbs = (
  left: readonly bigint[],
  right: readonly bigint[],
): bigint[] => {
  if (right.length > left.length) throw new RangeError("left is below right");
  const result: bigint[] = [];
  let borrow = 0n;
  for (let index = 0; index < left.length; index += 1) {
    let difference = left[index]! - (right[index] ?? 0n) - borrow;
    borrow = difference < 0n ? 1n : 0n;
    difference += borrow * LIMB_BASE;
    result.push(difference);
  }
  if (borrow > 0n) throw new RangeError("left is below right");
  return normalized(result);
};

/** Compares two magnitudes: -1, 0 or 1. */
export const cmpMidgardCekMagnitude = (
  left: readonly bigint[],
  right: readonly bigint[],
): -1 | 0 | 1 => {
  if (left.length !== right.length) return left.length < right.length ? -1 : 1;
  for (let index = left.length - 1; index >= 0; index -= 1) {
    if (left[index]! !== right[index]!)
      return left[index]! < right[index]! ? -1 : 1;
  }
  return 0;
};

/** Sign and magnitude limbs of an integer read from its CBOR. */
export const midgardCekSignedLimbs = (
  integer: MidgardCekCanonicalInteger,
): MidgardCekSignedLimbs => {
  const limbs = toMidgardCekLimbs(integer.argument);
  return Object.freeze({
    negative: integer.negative,
    magnitude: integer.negative ? addMidgardCekLimbs(limbs, [1n]) : limbs,
  });
};

/** The sum of two integers. */
export const addMidgardCekSigned = (
  left: MidgardCekSignedLimbs,
  right: MidgardCekSignedLimbs,
): MidgardCekSignedLimbs => {
  if (left.negative === right.negative) {
    return Object.freeze({
      negative: left.negative,
      magnitude: addMidgardCekLimbs(left.magnitude, right.magnitude),
    });
  }
  const order = cmpMidgardCekMagnitude(left.magnitude, right.magnitude);
  if (order === 0) return Object.freeze({ negative: false, magnitude: [] });
  const [larger, smaller] = order > 0 ? [left, right] : [right, left];
  return Object.freeze({
    negative: larger.negative,
    magnitude: subMidgardCekLimbs(larger.magnitude, smaller.magnitude),
  });
};

/** The difference of two integers. */
export const subtractMidgardCekSigned = (
  left: MidgardCekSignedLimbs,
  right: MidgardCekSignedLimbs,
): MidgardCekSignedLimbs =>
  right.magnitude.length === 0
    ? left
    : addMidgardCekSigned(left, {
        negative: !right.negative,
        magnitude: right.magnitude,
      });

/** The canonical CBOR of an integer given by sign and magnitude limbs. */
export const midgardCekIntegerCborFromMagnitude = (
  integer: MidgardCekSignedLimbs,
): Buffer => {
  if (integer.negative) {
    if (integer.magnitude.length === 0) throw new RangeError("negative zero");
    return encodeMidgardCekCanonicalIntegerCbor({
      negative: true,
      argument: fromMidgardCekLimbs(
        subMidgardCekLimbs(integer.magnitude, [1n]),
      ),
    });
  }
  return encodeMidgardCekCanonicalIntegerCbor({
    negative: false,
    argument: fromMidgardCekLimbs(integer.magnitude),
  });
};
