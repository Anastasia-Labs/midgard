import {
  hashMidgardCekValueNode,
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekFlatRoot,
  type MidgardCekMachineState,
  parseMidgardCekCanonicalIntegerCbor,
} from "@al-ft/midgard-core";
import { DataI } from "@harmoniclabs/plutus-data";
import { CEKConst } from "@harmoniclabs/plutus-machine";
import { Builtin } from "@harmoniclabs/uplc";

import type { MidgardCekDirectValueWitness } from "../../src/cek-builtin.argument-kinds.js";
import { referenceConstantToDirectWitness } from "../../src/cek-builtin.evaluate-reference-builtin.js";
import { hashMidgardCekDirectArguments } from "../../src/cek-builtin.selected-control-result.js";
import type { MidgardCekStagedOperand } from "../../src/cek-executor.staged-failure.js";
import { encodeMidgardCekPlutusData } from "../../src/plutus-data-iterative.encode.js";
import { call, int, run } from "./cek-staged-route-terms.js";

/** Operands and states for the staged failure step tests. */

/** The integer `value` as a staged step reads it from the store. */
export const integerOperand = (value: bigint): MidgardCekStagedOperand => {
  const cbor = encodeMidgardCekPlutusData(new DataI(value));
  return {
    type: "integer",
    bytes: cbor,
    value: CEKConst.int(value),
    leaf: {
      kind: "integerLeaf",
      cborRoot: midgardCekFlatRoot(cbor),
      cborLength: BigInt(cbor.length),
      words: midgardCekCanonicalIntegerMagnitudeWords(
        parseMidgardCekCanonicalIntegerCbor(cbor),
      ),
    },
  };
};

/** A byte string as a staged step reads it from the store. */
export const bytesOperand = (bytes: Uint8Array): MidgardCekStagedOperand => ({
  type: "bytes",
  bytes: Buffer.from(bytes),
  value: CEKConst.byteString(Buffer.from(bytes)),
  leaf: {
    kind: "bytesLeaf",
    root: midgardCekFlatRoot(Buffer.from(bytes)),
    length: BigInt(bytes.length),
  },
});

/** An operand as the machine presents it. */
export const presented = (
  operand: MidgardCekStagedOperand,
): MidgardCekDirectValueWitness =>
  referenceConstantToDirectWitness(operand.value, true);

const BASE: MidgardCekMachineState = (() => {
  const steps = run(call(Builtin.addInteger, int(1n), int(2n)), "proof")
    .execution!.steps;
  return steps.find((step) => step.pre.mode === "builtin")!.pre;
})();

/** A builtin-mode state from a real run, focused on builtin `tag` applied to
 * `arguments_`. */
export const builtinState = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekMachineState => {
  const { root, count } = hashMidgardCekDirectArguments(arguments_);
  return {
    ...BASE,
    focusRoot: hashMidgardCekValueNode({
      kind: "builtin",
      tag,
      forcesRemaining: 0n,
      argumentsCount: count,
      argumentsRoot: root,
    }),
  };
};
