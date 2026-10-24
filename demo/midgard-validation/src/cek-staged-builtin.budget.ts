/**
 * The charge the budget hop (`cek_core_builtin_budget`) posts for a staged
 * success (arm 31), whose arguments the roots hop committed compacted: the
 * TypeScript twin of `staged_builtin_budget_v1`.
 */
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.evaluate-reference-builtin.js";
import { midgardCekDirectBuiltinBudget } from "./cek-builtin.selected-control-result.js";
import {
  computeMidgardCekBuiltinBudget,
  type MidgardCekBuiltinBudget,
} from "./cek-cost.js";

/** The amount 0, which a shift's charge does not read. */
const UNREAD_AMOUNT = referenceConstantToDirectWitness(CEKConst.int(0n));

/** A value's memory as the roots hop commits it, compacted. */
const committedMemory = (value: MidgardCekDirectValueWitness): bigint => {
  const committed = semanticizeMidgardCekDirectConstant(value);
  if (committed.kind !== "semanticConstant") {
    throw new Error("a staged value is a constant");
  }
  return committed.witness.memory;
};

/**
 * The budget hop's charge for a staged step (`staged_builtin_budget_v1`).
 * The staged route commits the arguments compacted. shiftByteString and
 * rotateByteString cost their source alone, so the amount is not read.
 * integerToByteString costs its width's words, or for width 0 its input's
 * memory: in both cases its result's memory, so the width is not read.
 */
export const midgardCekStagedBuiltinBudget = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
): MidgardCekBuiltinBudget => {
  if (tag === 73n) {
    if (arguments_.length !== 3) {
      throw new Error("integerToByteString requires three arguments");
    }
    return computeMidgardCekBuiltinBudget(
      73,
      [arguments_[0]!, result, arguments_[2]!].map(committedMemory),
    );
  }
  if (tag !== 82n && tag !== 83n) {
    return midgardCekDirectBuiltinBudget(tag, arguments_);
  }
  if (arguments_.length !== 2) throw new Error("shift requires two arguments");
  return midgardCekDirectBuiltinBudget(tag, [arguments_[0]!, UNREAD_AMOUNT]);
};
