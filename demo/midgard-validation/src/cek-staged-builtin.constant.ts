/**
 * The witness of a plain constant as a staged step carries it.
 */
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type {
  Bytes,
  MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import { referenceConstantToDirectWitness } from "./cek-builtin.evaluate-reference-builtin.js";
import { refuse } from "./cek-staged-builtin.verify.js";

const STRING_TYPE = Buffer.from("9f02ff", "hex");

/** A plain constant of the staged builtins' types. */
export type MidgardCekStagedConstant =
  | { readonly type: "integer"; readonly value: bigint }
  | { readonly type: "bytes"; readonly bytes: Bytes }
  | { readonly type: "string"; readonly bytes: Bytes }
  | { readonly type: "bool"; readonly value: boolean };

/**
 * The witness of a constant as a staged step carries it: inline while its
 * payload fits the direct route, compacted to its leaf summary past it.
 */
export const midgardCekStagedConstantWitness = (
  constant: MidgardCekStagedConstant,
): MidgardCekDirectValueWitness => {
  if (constant.type === "integer") {
    return referenceConstantToDirectWitness(CEKConst.int(constant.value), true);
  }
  if (constant.type === "bool") {
    return referenceConstantToDirectWitness(
      CEKConst.bool(constant.value),
      true,
    );
  }
  const bytes = referenceConstantToDirectWitness(
    CEKConst.byteString(Buffer.from(constant.bytes)),
    true,
  );
  if (constant.type === "bytes") return bytes;
  // A string's payload is the Data of its bytes, as a byte string's is.
  return bytes.kind === "constant"
    ? { kind: "constant", witness: { ...bytes.witness, typeCbor: STRING_TYPE } }
    : bytes.kind === "semanticConstant"
      ? {
          kind: "semanticConstant",
          witness: {
            ...bytes.witness,
            typeCbor: STRING_TYPE,
            // A string costs a word per four bytes.
            memory: BigInt(constant.bytes.length) / 4n,
          },
        }
      : refuse("not a constant");
};
