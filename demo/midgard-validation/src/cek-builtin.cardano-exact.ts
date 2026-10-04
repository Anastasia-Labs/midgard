/**
 * Builtins the node computes with repository-owned code that follows the
 * Cardano ledger's Plutus semantics exactly, values and failures alike.
 * Elliptic-curve arithmetic comes from the audited noble libraries; this file
 * only states which inputs Cardano refuses and how a verdict is reached.
 */
import { ed25519, ED25519_TORSION_SUBGROUP } from "@noble/curves/ed25519.js";
import { secp256k1 } from "@noble/curves/secp256k1.js";
import { sha512 } from "@noble/hashes/sha2.js";

export type CardanoExactValue =
  | { readonly kind: "integer"; readonly value: bigint }
  | { readonly kind: "bytes"; readonly value: Uint8Array }
  | { readonly kind: "bool"; readonly value: boolean };

/** Tags whose value and failure rule this module owns. */
export const MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS: ReadonlySet<number> =
  new Set([12, 16, 17, 21, 52, 77, 82, 83, 85]);

const INT64_MIN = -(1n << 63n);
const INT64_MAX = (1n << 63n) - 1n;

// Builtin arguments Cardano reads as a machine Int fail outside Int64.
const isInt64 = (value: bigint): boolean =>
  value >= INT64_MIN && value <= INT64_MAX;

const integer = (value: bigint): CardanoExactValue => ({
  kind: "integer",
  value,
});
const bytes = (value: Uint8Array): CardanoExactValue => ({
  kind: "bytes",
  value,
});
const bool = (value: boolean): CardanoExactValue => ({ kind: "bool", value });

const bigEndian = (source: Uint8Array): bigint =>
  source.length === 0 ? 0n : BigInt(`0x${Buffer.from(source).toString("hex")}`);

const littleEndian = (source: Uint8Array): bigint =>
  bigEndian(Uint8Array.from(source).reverse());

const bigEndianBytes = (value: bigint, length: number): Uint8Array =>
  length === 0
    ? new Uint8Array(0)
    : Uint8Array.from(
        Buffer.from(value.toString(16).padStart(length * 2, "0"), "hex"),
      );

const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

/** `sliceByteString start n xs = take n (drop start xs)`. */
const sliceByteString = (
  start: bigint,
  count: bigint,
  source: Uint8Array,
): CardanoExactValue | "failure" => {
  if (!isInt64(start) || !isInt64(count)) return "failure";
  const length = BigInt(source.length);
  const from = start < 0n ? 0n : start > length ? length : start;
  const wanted = count < 0n ? 0n : count;
  const to = from + wanted > length ? length : from + wanted;
  return bytes(source.slice(Number(from), Number(to)));
};

/** Lexicographic byte order: a proper prefix sorts first. */
const compareBytes = (left: Uint8Array, right: Uint8Array): number => {
  const shared = Math.min(left.length, right.length);
  for (let index = 0; index < shared; index++) {
    if (left[index] !== right[index]) return left[index]! - right[index]!;
  }
  return left.length - right.length;
};

const ED25519_P = ed25519.Point.Fp.ORDER;
const ED25519_L = ed25519.Point.Fn.ORDER;
const ED25519_Y_MASK = (1n << 255n) - 1n;
// The y-coordinates of the eight small-order points.
const ED25519_SMALL_ORDER_Y = new Set(
  ED25519_TORSION_SUBGROUP.map((encoded) =>
    ed25519.Point.fromHex(encoded).toAffine().y.toString(16),
  ),
);

const ed25519EncodedY = (encoded: Uint8Array): bigint =>
  littleEndian(encoded) & ED25519_Y_MASK;

// libsodium `ge25519_has_small_order`: the encoding, sign bit ignored, names a
// small-order point, counting the two encodings at or above p.
const ed25519HasSmallOrder = (encoded: Uint8Array): boolean =>
  ED25519_SMALL_ORDER_Y.has(
    (ed25519EncodedY(encoded) % ED25519_P).toString(16),
  );

/**
 * Cardano verifies with libsodium's `crypto_sign_ed25519_verify_detached`.
 * A key that is not 32 bytes or a signature that is not 64 bytes fails. The
 * result is False when S is not below the group order, when R or the key is
 * a small-order encoding, when the key encodes y at or above p or is not a
 * point, and otherwise unless [S]B - [h]A encodes to exactly R, where
 * h = SHA-512(R || A || M) mod L.
 */
const verifyEd25519Signature = (
  key: Uint8Array,
  message: Uint8Array,
  signature: Uint8Array,
): CardanoExactValue | "failure" => {
  if (key.length !== 32 || signature.length !== 64) return "failure";
  const encodedR = signature.subarray(0, 32);
  const s = littleEndian(signature.subarray(32));
  if (s >= ED25519_L || ed25519HasSmallOrder(encodedR)) return bool(false);
  if (ed25519EncodedY(key) >= ED25519_P || ed25519HasSmallOrder(key)) {
    return bool(false);
  }
  let point;
  try {
    point = ed25519.Point.fromBytes(key);
  } catch {
    return bool(false);
  }
  const h =
    littleEndian(sha512(Uint8Array.from([...encodedR, ...key, ...message]))) %
    ED25519_L;
  const check = ed25519.Point.BASE.multiplyUnsafe(s).subtract(
    point.multiplyUnsafe(h),
  );
  return bool(sameBytes(check.toBytes(), encodedR));
};

const SECP256K1_N = secp256k1.Point.Fn.ORDER;

/**
 * Cardano verifies with libsecp256k1 over a 32-byte message hash. The key must
 * parse as a 33-byte compressed point, the message must be 32 bytes and the
 * signature 64 bytes with r and s each below the group order; otherwise the
 * builtin fails. A zero r or s, or s above half the order, is False.
 */
const verifyEcdsaSecp256k1Signature = (
  key: Uint8Array,
  message: Uint8Array,
  signature: Uint8Array,
): CardanoExactValue | "failure" => {
  if (key.length !== 33 || message.length !== 32 || signature.length !== 64) {
    return "failure";
  }
  if (key[0] !== 2 && key[0] !== 3) return "failure";
  try {
    secp256k1.Point.fromBytes(key);
  } catch {
    return "failure";
  }
  const r = bigEndian(signature.subarray(0, 32));
  const s = bigEndian(signature.subarray(32));
  if (r >= SECP256K1_N || s >= SECP256K1_N) return "failure";
  if (r === 0n || s === 0n || s > SECP256K1_N >> 1n) return bool(false);
  return bool(
    secp256k1.verify(signature, message, key, {
      prehash: false,
      lowS: true,
      format: "compact",
    }),
  );
};

/**
 * With padding the result is as long as the longer argument and its tail is
 * the longer argument's tail; without it the result is as long as the
 * shorter one.
 */
const xorByteString = (
  padding: boolean,
  left: Uint8Array,
  right: Uint8Array,
): CardanoExactValue => {
  const [shorter, longer] =
    left.length <= right.length ? [left, right] : [right, left];
  const result = Uint8Array.from(
    padding ? longer : longer.subarray(0, shorter.length),
  );
  for (let index = 0; index < shorter.length; index++) {
    result[index] = shorter[index]! ^ longer[index]!;
  }
  return bytes(result);
};

/**
 * The byte string is one big-endian bit string; a positive amount moves bits
 * towards the most significant end, a negative amount towards the least, and
 * vacated bits are zero. The amount is a machine Int.
 */
const shiftByteString = (
  source: Uint8Array,
  amount: bigint,
): CardanoExactValue | "failure" => {
  if (!isInt64(amount)) return "failure";
  const width = BigInt(source.length) * 8n;
  const magnitude = amount < 0n ? -amount : amount;
  if (magnitude >= width) return bytes(new Uint8Array(source.length));
  const value = bigEndian(source);
  const shifted =
    amount >= 0n
      ? (value << amount) & ((1n << width) - 1n)
      : value >> magnitude;
  return bytes(bigEndianBytes(shifted, source.length));
};

/**
 * Rotation by `amount` towards the most significant end, modulo the bit
 * width; a negative amount rotates the other way. The amount is a machine
 * Int.
 */
const rotateByteString = (
  source: Uint8Array,
  amount: bigint,
): CardanoExactValue | "failure" => {
  if (!isInt64(amount)) return "failure";
  const width = BigInt(source.length) * 8n;
  if (width === 0n) return bytes(new Uint8Array(0));
  const left = ((amount % width) + width) % width;
  const value = bigEndian(source);
  const rotated =
    ((value << left) | (value >> (width - left))) & ((1n << width) - 1n);
  return bytes(bigEndianBytes(rotated, source.length));
};

/**
 * The index of the lowest set bit, counting from the least significant bit of
 * the last byte, or -1 when no bit is set.
 */
const findFirstSetBit = (source: Uint8Array): CardanoExactValue => {
  for (let index = source.length - 1; index >= 0; index--) {
    const byte = source[index]!;
    if (byte !== 0) {
      let bit = 0;
      while (((byte >> bit) & 1) === 0) bit++;
      return integer(BigInt((source.length - 1 - index) * 8 + bit));
    }
  }
  return integer(-1n);
};

const expectKinds = <const K extends readonly CardanoExactValue["kind"][]>(
  arguments_: readonly CardanoExactValue[],
  kinds: K,
): { [I in keyof K]: Extract<CardanoExactValue, { kind: K[I] }>["value"] } => {
  if (
    arguments_.length !== kinds.length ||
    arguments_.some((argument, index) => argument.kind !== kinds[index])
  ) {
    throw new Error("V1 builtin arguments do not match the builtin signature");
  }
  return arguments_.map((argument) => argument.value) as never;
};

/**
 * Evaluates one of `MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS` over saturated,
 * well-typed arguments. "failure" is the builtin failing on Cardano.
 */
export const evaluateCardanoExactBuiltin = (
  tag: number,
  arguments_: readonly CardanoExactValue[],
): CardanoExactValue | "failure" => {
  switch (tag) {
    case 12:
      return sliceByteString(
        ...expectKinds(arguments_, ["integer", "integer", "bytes"] as const),
      );
    case 16: {
      const [left, right] = expectKinds(arguments_, [
        "bytes",
        "bytes",
      ] as const);
      return bool(compareBytes(left, right) < 0);
    }
    case 17: {
      const [left, right] = expectKinds(arguments_, [
        "bytes",
        "bytes",
      ] as const);
      return bool(compareBytes(left, right) <= 0);
    }
    case 21:
      return verifyEd25519Signature(
        ...expectKinds(arguments_, ["bytes", "bytes", "bytes"] as const),
      );
    case 52:
      return verifyEcdsaSecp256k1Signature(
        ...expectKinds(arguments_, ["bytes", "bytes", "bytes"] as const),
      );
    case 77:
      return xorByteString(
        ...expectKinds(arguments_, ["bool", "bytes", "bytes"] as const),
      );
    case 82:
      return shiftByteString(
        ...expectKinds(arguments_, ["bytes", "integer"] as const),
      );
    case 83:
      return rotateByteString(
        ...expectKinds(arguments_, ["bytes", "integer"] as const),
      );
    case 85:
      return findFirstSetBit(...expectKinds(arguments_, ["bytes"] as const));
    default:
      throw new Error(`builtin ${String(tag)} is not evaluated here`);
  }
};
