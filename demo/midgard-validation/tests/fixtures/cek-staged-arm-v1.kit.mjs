/**
 * The building blocks of the `cek-staged-arm-v1` rows: operand bytes by
 * construction, integers by their CBOR argument, the native results the rows
 * claim, and the row shapes. See `cek-staged-arm-v1.vectors.mjs`.
 */

import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { blake2b } from "@noble/hashes/blake2.js";
import { ripemd160 } from "@noble/hashes/legacy.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { keccak_256, sha3_256 } from "@noble/hashes/sha3.js";

// ## Bytes by construction

/** `length` bytes (at most 65,536) where byte i is (i + shift) mod 256. */
export const P = (length, shift) => ({ pattern: [length, shift] });
/** `length` bytes of ASCII text. */
export const T = (length) => ({ text: length });
export const H = (hex) => ({ hex });
/** `count` copies of `byte`. */
export const R = (byte, count) => ({ repeat: [byte, count] });
export const C = (...parts) => ({ concat: parts });
/** The bytes with their last byte changed. */
export const F = (spec) => ({ flipLast: spec });
/** The canonical CBOR of an integer. */
export const I = (integer) => ({ integerCbor: integer });

const TEXT_BLOCK = Buffer.from("staged operand text ", "ascii");

const minimalHex = (value) =>
  value === 0n
    ? ""
    : value
        .toString(16)
        .padStart(Math.ceil(value.toString(16).length / 2) * 2, "0");

export const bytesOf = (spec) => {
  if (spec.pattern) {
    const [length, shift] = spec.pattern;
    return Buffer.from(Array.from({ length }, (_, i) => (i + shift) % 256));
  }
  if (spec.text !== undefined) {
    return Buffer.from(
      Array.from(
        { length: spec.text },
        (_, i) => TEXT_BLOCK[i % TEXT_BLOCK.length],
      ),
    );
  }
  if (spec.hex !== undefined) return Buffer.from(spec.hex, "hex");
  if (spec.repeat) return Buffer.alloc(spec.repeat[1], spec.repeat[0]);
  if (spec.concat) return Buffer.concat(spec.concat.map(bytesOf));
  if (spec.flipLast) {
    const bytes = Buffer.from(bytesOf(spec.flipLast));
    bytes[bytes.length - 1] = (bytes[bytes.length - 1] + 1) % 256;
    return bytes;
  }
  if (spec.integerCbor) return integerCbor(valueOf(spec.integerCbor));
  throw new Error(`unknown bytes spec ${JSON.stringify(spec)}`);
};

// ## Integers by their CBOR argument

/**
 * An integer by its CBOR argument (n, or -n - 1 when negative) as minimal
 * big-endian bytes: the form the Aiken module rebuilds without arithmetic,
 * which its test machine refuses on integers past the range.
 */
export const N = (negative, argument) => ({ negative, argument });

export const small = (value) =>
  N(value < 0n, H(minimalHex(value < 0n ? -value - 1n : value)));

export const valueOf = (integer) => {
  const bytes = bytesOf(integer.argument);
  const argument =
    bytes.length === 0 ? 0n : BigInt(`0x${bytes.toString("hex")}`);
  return integer.negative ? -argument - 1n : argument;
};

export const integerCbor = (value) =>
  Buffer.from(lucidDataToCborIterative(value));

/** A positive integer whose magnitude is `pattern(bytes, shift)`. */
export const mag = (bytes, shift) => N(false, P(bytes, shift));
/** 2^262142 - 1: 32,768 bytes, all 0xff below a 0x3f top byte. */
export const HALF = N(false, C(H("3f"), R(0xff, 32_767)));
/** 2^262142. */
export const HALF_UP = N(false, C(H("40"), R(0, 32_767)));
/** -2^262142 + 1. */
export const NEG_HALF = N(true, C(H("3f"), R(0xff, 32_766), H("fe")));
/** -2^262142. */
export const NEG_HALF_DOWN = N(true, C(H("3f"), R(0xff, 32_767)));
/** 2^262143 - 1 and -2^262143: the ends of the range. */
export const TOP = N(false, C(H("7f"), R(0xff, 32_767)));
export const BOTTOM = N(true, C(H("7f"), R(0xff, 32_767)));
/** One past each end. */
export const PAST_TOP = N(false, C(H("80"), R(0, 32_767)));
export const PAST_BOTTOM = N(true, C(H("80"), R(0, 32_767)));

// ## Values

const bytesValue = (spec, string = false) => ({
  type: string ? "string" : "bytes",
  bytes: bytesOf(spec),
});
const intValue = (integer) => ({ type: "integer", value: valueOf(integer) });
const boolValue = (value) => ({ type: "bool", value });

const hashes = {
  18: sha256,
  19: sha3_256,
  20: (bytes) => blake2b(bytes, { dkLen: 32 }),
  71: keccak_256,
  72: (bytes) => blake2b(bytes, { dkLen: 28 }),
  86: ripemd160,
};

/** divideInteger's quotient: rounded toward negative infinity. */
const floorQuotient = (a, b) =>
  a % b !== 0n && a < 0n !== b < 0n ? a / b - 1n : a / b;

/** Cardano's result of builtin `tag` over operand values, without bounds. */
const native = (tag, values) => {
  const [a, b, c] = values;
  if (tag === 10)
    return { type: "bytes", bytes: Buffer.concat([a.bytes, b.bytes]) };
  if (tag === 22)
    return { type: "string", bytes: Buffer.concat([a.bytes, b.bytes]) };
  if (tag === 11) {
    return {
      type: "bytes",
      bytes: Buffer.concat([Buffer.from([Number(a.value)]), b.bytes]),
    };
  }
  if (tag === 12) {
    const start = Number(a.value);
    return {
      type: "bytes",
      bytes: c.bytes.subarray(start, start + Number(b.value)),
    };
  }
  if (tag === 13) return { type: "integer", value: BigInt(a.bytes.length) };
  if (tag === 14)
    return { type: "integer", value: BigInt(a.bytes[Number(b.value)]) };
  if (tag === 15 || tag === 23) return boolValue(a.bytes.equals(b.bytes));
  if (tag === 16) return boolValue(Buffer.compare(a.bytes, b.bytes) < 0);
  if (tag === 17) return boolValue(Buffer.compare(a.bytes, b.bytes) <= 0);
  if (tag === 24) return { type: "bytes", bytes: a.bytes };
  if (tag === 25) return { type: "string", bytes: a.bytes };
  if (hashes[tag])
    return { type: "bytes", bytes: Buffer.from(hashes[tag](a.bytes)) };
  if (tag === 0) return { type: "integer", value: a.value + b.value };
  if (tag === 2) return { type: "integer", value: a.value * b.value };
  if (tag === 3) {
    return { type: "integer", value: floorQuotient(a.value, b.value) };
  }
  if (tag === 4) return { type: "integer", value: a.value / b.value };
  if (tag === 5) return { type: "integer", value: a.value % b.value };
  if (tag === 6) {
    return {
      type: "integer",
      value: a.value - b.value * floorQuotient(a.value, b.value),
    };
  }
  if (tag === 1) return { type: "integer", value: a.value - b.value };
  if (tag === 7) return boolValue(a.value === b.value);
  if (tag === 8) return boolValue(a.value < b.value);
  if (tag === 9) return boolValue(a.value <= b.value);
  throw new Error(`no native result for tag ${tag}`);
};

// ## Rows

/**
 * A row: builtin `tag` over operands given as [value spec, source] pairs,
 * with `staged` the bytes staged as operands 0, 1, ... and `claim` the result
 * claimed (by default Cardano's).
 */
export const row = (name, accepts, tag, operands, staged, claim) => {
  const values = operands.map(([value]) => value);
  return Object.freeze({
    name,
    accepts,
    tag,
    arguments: values,
    result: claim ? claim(native(tag, values)) : native(tag, values),
    staged,
    sources: operands.map(([, source]) => source),
  });
};

export const staged = (operand) => ({ staged: operand });
export const inline = (spec) => ({ inline: spec });
export const bytesLeaf = (spec) => ({ bytesLeaf: spec });
export const integerLeaf = (integer) => ({ integerLeaf: integer });
export const smallSource = (value) => ({ small: value });

export const b = (spec, string = false) => bytesValue(spec, string);
export const i = (integer) => intValue(integer);
export const smallArgument = (value) => [
  intValue(small(value)),
  smallSource(value),
];

/** A one-operand bytes builtin over a staged operand. */
export const unary = (name, accepts, tag, spec, claim, string = false) =>
  row(name, accepts, tag, [[b(spec, string), staged(0)]], [spec], claim);

/** A two-operand bytes builtin over two staged operands. */
export const binary = (
  name,
  accepts,
  tag,
  left,
  right,
  claim,
  string = false,
) =>
  row(
    name,
    accepts,
    tag,
    [
      [b(left, string), staged(0)],
      [b(right, string), staged(1)],
    ],
    [left, right],
    claim,
  );

/** An integer builtin over two staged operands. */
export const pair = (name, accepts, tag, left, right, claim) =>
  row(
    name,
    accepts,
    tag,
    [
      [i(left), staged(0)],
      [i(right), staged(1)],
    ],
    [I(left), I(right)],
    claim,
  );

/** An integer builtin over a staged left and an inline right operand. */
export const stagedAndInline = (name, accepts, tag, left, right, claim) =>
  row(
    name,
    accepts,
    tag,
    [
      [i(left), staged(0)],
      [i(right), inline(I(right))],
    ],
    [I(left)],
    claim,
  );

export const differ = (value) => ({ ...value, value: !value.value });
export const claimed = (value) => () => value;
export const PAST = (spec, tail = "00") => C(spec, H(tail));

/**
 * multiplyInteger or a division (tags 2 to 6) over a staged `left` and a
 * staged or inline `right`, carrying the integers its column loop compares
 * as further staged operands: the product for multiplyInteger, the quotient
 * for quotientInteger and divideInteger (the result), and the quotient and
 * the result for remainderInteger and modInteger. The claimed result is the
 * last of `carried`; an accepted row must carry Cardano's quotient and
 * result.
 */
export const looped = (
  name,
  accepts,
  tag,
  left,
  right,
  carried,
  inlineRight = false,
) => {
  const values = [i(left), i(right)];
  const result = i(carried.at(-1));
  if (accepts) {
    const quotient = native(tag === 5 ? 4 : tag === 6 ? 3 : tag, values);
    if (
      result.value !== native(tag, values).value ||
      (carried.length === 2 && valueOf(carried[0]) !== quotient.value)
    ) {
      throw new Error(`${name}: an accepted row carries another integer`);
    }
  }
  const stagedRight = inlineRight ? [] : [I(right)];
  const first = 1 + stagedRight.length;
  return Object.freeze({
    name,
    accepts,
    tag,
    arguments: values,
    result,
    staged: [I(left), ...stagedRight, ...carried.map(I)],
    sources: [
      staged(0),
      inlineRight ? inline(I(right)) : staged(1),
      ...carried.map((_, index) => staged(first + index)),
    ],
  });
};
