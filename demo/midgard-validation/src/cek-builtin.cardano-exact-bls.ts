/**
 * The BLS12-381 Miller-loop builtins (tags 68, 69 and 70) as Cardano computes
 * them, with the pairing arithmetic from the audited noble library. A Miller
 * loop result is an Fp12 element and is never revealed; only finalVerify's
 * boolean leaves this module.
 */
import { bls12_381 } from "@noble/curves/bls12-381.js";

const Fp12 = bls12_381.fields.Fp12;

export type CardanoMlResult = ReturnType<typeof Fp12.mul>;

/**
 * `bls12_381_millerLoop`: the Miller loop of a G1 and a G2 element, given as
 * their compressed encodings. A loop over the point at infinity is the
 * identity of Fp12.
 */
export const cardanoBlsMillerLoop = (
  g1Compressed: Uint8Array,
  g2Compressed: Uint8Array,
): CardanoMlResult => {
  const g1 = bls12_381.G1.Point.fromBytes(g1Compressed);
  const g2 = bls12_381.G2.Point.fromBytes(g2Compressed);
  if (g1.is0() || g2.is0()) return Fp12.ONE;
  return bls12_381.pairing(g1, g2, false);
};

/** `bls12_381_mulMlResult`: the product in Fp12. */
export const cardanoBlsMulMlResult = (
  left: CardanoMlResult,
  right: CardanoMlResult,
): CardanoMlResult => Fp12.mul(left, right);

/**
 * `bls12_381_finalVerify`: True exactly when both arguments are equal after
 * the final exponentiation.
 */
export const cardanoBlsFinalVerify = (
  left: CardanoMlResult,
  right: CardanoMlResult,
): boolean =>
  Fp12.eql(Fp12.finalExponentiate(left), Fp12.finalExponentiate(right));
