import { midgardCekIntegerMemoryWords } from "@al-ft/midgard-core";

import {
  directArgumentsMatchKinds,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import { directInteger } from "./cek-builtin.selected-control-result.js";

/** The fewest words of a dividend the direct failure arm refuses. */
const MAX_DIRECT_DIVIDEND_WORDS = 4_096n;

/**
 * Whether builtin `tag` divides an integer of fewer than 4,096 words by a
 * revealed zero: divideInteger, quotientInteger, remainderInteger and
 * modInteger fail there whatever such a dividend, so the dividend may be
 * presented by its leaf alone. A dividend of 4,096 words or more is left to
 * the staged failure hop, which reads from its CBOR whether it is in range
 * and so charged; the direct failure arm refuses it.
 */
export const midgardCekDivisionByZero = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  if (tag < 3n || tag > 6n) return false;
  if (!directArgumentsMatchKinds(Number(tag), arguments_)) return false;
  const [dividend, divisor] = arguments_;
  const words =
    dividend!.kind === "semanticConstant"
      ? dividend!.witness.memory
      : midgardCekIntegerMemoryWords(directInteger(dividend!));
  return (
    divisor!.kind === "constant" &&
    directInteger(divisor!) === 0n &&
    words < MAX_DIRECT_DIVIDEND_WORDS
  );
};

const INT64_MIN = -(2n ** 63n);
const INT64_MAX = 2n ** 63n - 1n;

/**
 * consByteString, sliceByteString, shiftByteString and rotateByteString over
 * a compacted byte string: their failures (a byte outside [0, 255], a start,
 * length or amount outside Int64) are decided by the revealed integers alone,
 * the byte string by its type, as the direct failure arm decides them. A
 * success needs the bytes, which a compacted byte string does not reveal.
 */
export const compactedSourceFailure = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): "failure" => {
  const integers = (
    tag === 82n || tag === 83n ? arguments_.slice(1) : arguments_.slice(0, -1)
  ).map(directInteger);
  const fails =
    tag === 11n
      ? integers[0]! < 0n || integers[0]! > 255n
      : integers.some((value) => value < INT64_MIN || value > INT64_MAX);
  if (!fails) {
    throw new Error("V1 builtin over a compacted byte string must fail");
  }
  return "failure";
};

const INTEGER_TYPE = Buffer.from("9f00ff", "hex");

/**
 * A division builtin over a compacted dividend fails only on a zero divisor,
 * as the direct failure arm decides it from the dividend's type. A dividend
 * of 4,096 words or more is left to the staged failure hop, which reads from
 * its CBOR whether it is in range and so charged.
 */
export const compactedDividendFailure = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): "failure" => {
  const [dividend, divisor] = arguments_;
  if (
    dividend?.kind !== "semanticConstant" ||
    !Buffer.from(dividend.witness.typeCbor).equals(INTEGER_TYPE) ||
    dividend.witness.memory >= 4_096n ||
    directInteger(divisor!) !== 0n
  ) {
    throw new Error("V1 division over a compacted dividend must fail by zero");
  }
  return "failure";
};

/** scalarMul fails on a scalar outside [-2^4095, 2^4095 - 1]. */
const BLS_SCALAR_BOUND = 2n ** 4_095n;

/**
 * Whether G1 or G2 scalarMul (56, 63) fails on its revealed scalar: outside
 * [-2^4095, 2^4095 - 1] it fails whatever the point, so the point is read by
 * its type alone (it may be compacted, and is not uncompressed), as the direct
 * failure arm decides it.
 */
export const midgardCekScalarMulOutOfBound = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  if (tag !== 56n && tag !== 63n) return false;
  if (!directArgumentsMatchKinds(Number(tag), arguments_)) return false;
  const [scalar] = arguments_;
  if (scalar!.kind !== "constant") return false;
  const value = directInteger(scalar!);
  return value < -BLS_SCALAR_BOUND || value >= BLS_SCALAR_BOUND;
};
