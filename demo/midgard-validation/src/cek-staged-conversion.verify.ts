/**
 * The staged semantic arm of semantics group 11 (integerToByteString,
 * byteStringToInteger, shiftByteString and rotateByteString): the
 * TypeScript twin of the group's result check in `cek_staged_arm_v1`.
 */
import {
  collectMidgardCekStagedRuns,
  type MidgardCekStagedReferenceInput,
} from "@al-ft/midgard-core";

import type { MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import { evaluateMidgardCekBuiltinOnValues } from "./cek-builtin.evaluate-direct-builtin.js";
import type { MidgardCekStagedBuiltinMaterial } from "./cek-staged-builtin.material.js";
import {
  type ArmInputs,
  bindBytes,
  bindInteger,
  committedBoolean,
  compact,
  compactValue,
  sameCompacted,
  smallInteger,
} from "./cek-staged-builtin.verify.js";
import { UPLCConst } from "./uplc-constant.js";

/**
 * Semantics group 11. byteStringToInteger over at most 65,536 bytes, and
 * shiftByteString or rotateByteString over any length (protocol version 11
 * bounds neither) by the amount the second source reveals, read an input
 * bound inline or from staging outputs by the first source.
 * integerToByteString reads the width the first source reveals and an input
 * bound by its canonical CBOR through the second. Endianness is the
 * committed Boolean.
 */
export const verifyConversionGroup = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  const compacted = arguments_.map(compact);
  const { sources } = material;
  const inputs: ArmInputs = {
    tag: Number(tag),
    arguments: [],
    result: compact(result),
    runs: collectMidgardCekStagedRuns(referenceInputs, material.runs.refs),
    operands: material.runs.operands,
  };
  let values: UPLCConst[];
  if (tag === 73n) {
    const [endianness, width, input] = compacted;
    if (compacted.length !== 3 || sources.length !== 2) return false;
    const { integer } = bindInteger(inputs, [input!, sources[1]!], false);
    if (integer === null) return false;
    values = [
      committedBoolean([endianness!]),
      UPLCConst.int(smallInteger([width!, sources[0]!])),
      UPLCConst.int(integer),
    ];
  } else {
    const conversion = tag === 74n;
    const [first, second] = compacted;
    if (compacted.length !== 2) return false;
    if (sources.length !== (conversion ? 1 : 2)) return false;
    const bound = bindBytes(
      inputs,
      [conversion ? second! : first!, sources[0]!],
      false,
      conversion,
    );
    if (bound.bytes === null) return false;
    // The bound bytes are a fresh copy, which the reference
    // byteStringToInteger may reverse in place.
    const bytes = UPLCConst.byteString(bound.bytes);
    values = conversion
      ? [committedBoolean([first!]), bytes]
      : [bytes, UPLCConst.int(smallInteger([second!, sources[1]!]))];
  }
  const evaluated = evaluateMidgardCekBuiltinOnValues(Number(tag), values);
  return (
    evaluated !== "failure" &&
    sameCompacted(inputs.result, compactValue(evaluated))
  );
};
