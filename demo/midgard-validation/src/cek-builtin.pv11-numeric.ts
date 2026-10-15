import { pippenger } from "@noble/curves/abstract/curve.js";
import { bls12_381, bls12_381_Fr } from "@noble/curves/bls12-381.js";

import { midgardCekBlsScalarIsOutOfBounds } from "./cek-builtin.variant-e.js";

const EXP_LIMIT = 1n << 8191n;

const mod = (value: bigint, modulus: bigint): bigint =>
  ((value % modulus) + modulus) % modulus;

/** Plutus 1.71 ExpMod.expMod, including the modulus-one short circuit. */
export const cardanoExpModInteger = (
  base: bigint,
  exponent: bigint,
  modulus: bigint,
): bigint | "failure" => {
  if (modulus <= 0n || modulus >= EXP_LIMIT) return "failure";
  if (modulus === 1n) return 0n;
  if (
    base < -EXP_LIMIT ||
    base >= EXP_LIMIT ||
    exponent < -EXP_LIMIT ||
    exponent >= EXP_LIMIT
  )
    return "failure";
  let factor = mod(base, modulus);
  let power = exponent;
  if (power < 0n) {
    // Extended Euclid; a negative power exists only for an invertible base.
    let remainder = modulus;
    let nextRemainder = factor;
    let inverse = 0n;
    let nextInverse = 1n;
    while (nextRemainder !== 0n) {
      const quotient = remainder / nextRemainder;
      [remainder, nextRemainder] = [
        nextRemainder,
        remainder - quotient * nextRemainder,
      ];
      [inverse, nextInverse] = [nextInverse, inverse - quotient * nextInverse];
    }
    if (remainder !== 1n) return "failure";
    factor = mod(inverse, modulus);
    power = -power;
  }
  let result = 1n;
  while (power > 0n) {
    if ((power & 1n) !== 0n) result = (result * factor) % modulus;
    power >>= 1n;
    if (power !== 0n) factor = (factor * factor) % modulus;
  }
  return result;
};

/** All scalars are checked, including those past the end of the point list. */
export const cardanoBlsMultiScalarMul = (
  group: "blsG1" | "blsG2",
  scalars: readonly bigint[],
  points: readonly Uint8Array[],
): Uint8Array | "failure" => {
  if (scalars.some(midgardCekBlsScalarIsOutOfBounds)) return "failure";
  const count = Math.min(scalars.length, points.length);
  const reduced = scalars
    .slice(0, count)
    .map((scalar) => mod(scalar, bls12_381_Fr.ORDER));
  if (group === "blsG1") {
    const Point = bls12_381.G1.Point;
    const result =
      count === 0
        ? Point.ZERO
        : pippenger(
            Point,
            points.slice(0, count).map((bytes) => Point.fromBytes(bytes)),
            reduced,
          );
    // Cancellation can produce a projective infinity; encode the canonical zero.
    return (result.is0() ? Point.ZERO : result).toBytes();
  }
  const Point = bls12_381.G2.Point;
  const result =
    count === 0
      ? Point.ZERO
      : pippenger(
          Point,
          points.slice(0, count).map((bytes) => Point.fromBytes(bytes)),
          reduced,
        );
  return (result.is0() ? Point.ZERO : result).toBytes();
};
