/**
 * Constants across the pinned reference evaluator's boundary: Midgard's UPLC
 * constants in, the evaluator's `CEKConst` values out, and back. The constant
 * type tags agree except array and Value, which the evaluator numbers the
 * other way round and Midgard's constants never hold.
 */
import { CEKConst } from "@harmoniclabs/plutus-machine";
import {
  type ConstType as ReferenceConstType,
  UPLCConst as ReferenceUPLCConst,
} from "@harmoniclabs/uplc";

import {
  type ConstType,
  ConstTyTag,
  type ConstValue,
  UPLCConst,
} from "./uplc-constant.js";

const representable = (type: readonly number[]): void => {
  if (type.some((tag) => tag === ConstTyTag.array || tag === ConstTyTag.value))
    throw new Error("V1 constant has an unknown type tag");
};

/** A Midgard constant as the reference evaluator's value. */
export const referenceValueOf = (constant: {
  readonly type: readonly number[];
  readonly value: ConstValue;
}): CEKConst => {
  representable(constant.type);
  return CEKConst.fromUplc(
    new ReferenceUPLCConst(
      [...constant.type] as ReferenceConstType,
      constant.value as never,
    ),
  );
};

/** A reference evaluator value as a Midgard constant. */
export const constantOfReferenceValue = (reference: CEKConst): UPLCConst => {
  representable(reference.type);
  return new UPLCConst(
    reference.type as readonly number[] as ConstType,
    reference.value as ConstValue,
  );
};
