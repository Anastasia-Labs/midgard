import { DataB, DataI } from "@harmoniclabs/plutus-data";

import {
  directArgumentsMatchKinds,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import { decodeMidgardCekConstantWitness } from "./cek-constant.js";

const BLS_SCALAR_LIMIT = 1n << 4095n;
export const midgardCekBlsScalarIsOutOfBounds = (scalar: bigint): boolean =>
  scalar < -BLS_SCALAR_LIMIT || scalar >= BLS_SCALAR_LIMIT;

/** Plutus V3 variant E denotation failures, charged after unlifting. */
export const midgardCekVariantEBuiltinFailure = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  if (tag !== 56n && tag !== 63n && tag !== 80n) return false;
  if (!directArgumentsMatchKinds(Number(tag), arguments_)) return false;
  const argument = arguments_[0];
  if (argument?.kind !== "constant") {
    throw new Error("variant E builtin bound requires a revealed constant");
  }
  const { type, payload } = decodeMidgardCekConstantWitness(argument.witness);
  if (tag === 80n) {
    if (type.kind !== "bytes" || !(payload instanceof DataB)) {
      throw new Error("writeBits requires bytes");
    }
    return payload.bytes.length > 4096;
  }
  if (type.kind !== "integer" || !(payload instanceof DataI)) {
    throw new Error("scalarMul requires an integer");
  }
  return midgardCekBlsScalarIsOutOfBounds(payload.int);
};
