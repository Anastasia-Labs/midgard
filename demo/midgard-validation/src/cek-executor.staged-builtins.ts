/**
 * The staged route of the scalar builtins (arm 31, semantics groups 7, 8 and
 * 9): a byte-string, string, bitwise, signature or linear integer builtin,
 * or byteStringToInteger, whose arguments or result do not fit the direct
 * carriage bound.
 *
 * The step is a pure function of the execution graph: each argument gets one
 * typed source (its bytes inline, a staged operand by index, its leaf
 * preimage, or a small integer's value), and the staged operands' bytes ride
 * with the witness. The material names no staging outputs; the one-step
 * driver stages the operands and fills them in
 * (`stageMidgardCekStagedBuiltinMaterial`) before the binder.
 *
 * The result is the value-level relation the direct route uses. A step that
 * fails on an argument past protocol version 11's bounds or on its operands'
 * content is given its failure witness (`midgardCekStagedFailure`). A step
 * no arm can certify is not emitted: it throws, so the run is indeterminate
 * rather than given a step no verifier accepts.
 */
import {
  MIDGARD_CEK_STAGED_CHUNK_BYTES,
  midgardCekCanonicalIntegerMagnitudeWords,
  parseMidgardCekCanonicalIntegerCbor,
} from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";
import { UPLCConst } from "@harmoniclabs/uplc";

import type {
  Bytes,
  MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import {
  evaluateMidgardCekBuiltinOnValues,
  midgardCekDirectBuiltinFits,
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.evaluate-reference-builtin.js";
import { encodeMidgardCekCanonicalConstant } from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import type { MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import {
  type MidgardCekStagedFailure,
  midgardCekStagedFailure,
  type MidgardCekStagedOperand,
} from "./cek-executor.staged-failure.js";
import type { MidgardCekCoreStepWitness } from "./cek-machine.js";
import { midgardCekSemanticGroup } from "./cek-semantic-group.js";
import type { MidgardCekStagedOperandSource } from "./cek-staged-builtin.material.js";
import {
  midgardCekPlaceholderStaging,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "./cek-staged-builtin.material.js";
import {
  compactValue,
  midgardCekStagedBuiltinProves,
  sameCompacted,
  verifyMidgardCekStagedBuiltin,
} from "./cek-staged-builtin.verify.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

type StagedWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinStaged" }
>;

/**
 * Whether builtin `tag` over these presented arguments takes the staged
 * route: a builtin the staged arms prove, with an argument already past the
 * direct carriage bound, or arguments or a result that do not fit it.
 */
export const isMidgardCekStagedStep = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean =>
  midgardCekStagedBuiltinProves(tag) &&
  (arguments_.some((argument) => argument.kind !== "constant") ||
    !midgardCekDirectBuiltinFits(tag, arguments_));

const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

const operandOf = (
  store: MidgardCekSemanticStore,
  root: Bytes,
  argument: MidgardCekDirectValueWitness,
): MidgardCekStagedOperand => {
  if (argument.kind !== "constant" && argument.kind !== "semanticConstant") {
    throw new Error("CEK staged builtin requires constant arguments");
  }
  const { type, dataRoot } = store.constant(root, argument);
  const node = store.node(dataRoot);
  if (type.kind === "integer" && node.kind === "integer") {
    const cbor = store.blob(node.cborRoot);
    return {
      type: "integer",
      bytes: cbor,
      value: CEKConst.int(store.integer(dataRoot)),
      leaf: {
        kind: "integerLeaf",
        cborRoot: node.cborRoot,
        cborLength: node.cborLength,
        words: midgardCekCanonicalIntegerMagnitudeWords(
          parseMidgardCekCanonicalIntegerCbor(cbor),
        ),
      },
    };
  }
  if (
    (type.kind === "bytes" || type.kind === "string") &&
    node.kind === "bytes"
  ) {
    const bytes = store.blob(node.bytesRoot);
    return {
      type: type.kind,
      bytes,
      value:
        type.kind === "bytes"
          ? CEKConst.byteString(bytes)
          : CEKConst.fromUplc(UPLCConst.str(utf8.decode(bytes))),
      leaf: {
        kind: "bytesLeaf",
        root: node.bytesRoot,
        length: node.bytesLength,
      },
    };
  }
  throw new Error("CEK staged builtin argument is not a scalar constant");
};

/** A committed Boolean argument: byteStringToInteger's endianness, or the
 * padding of andByteString, orByteString and xorByteString. */
const committedBoolean = (argument: MidgardCekDirectValueWitness): CEKConst => {
  const committed = semanticizeMidgardCekDirectConstant(argument);
  const constant = [true, false]
    .map((value) => CEKConst.bool(value))
    .find(
      (value) =>
        committed.kind === "semanticConstant" &&
        sameCompacted(committed.witness, compactValue(value)),
    );
  if (constant === undefined) {
    throw new Error("CEK staged builtin Boolean argument is not a Boolean");
  }
  return constant;
};

/** Whether the staged arm binds every argument of `tag` by its leaf alone. */
const leafOnly = (tag: number): boolean =>
  tag === 7 || tag === 13 || tag === 15 || tag === 23 || tag === 24;

/** Whether argument `index` of `tag` is a byte-string builtin's integer. */
const smallInteger = (tag: number, index: number): boolean =>
  (tag === 11 && index === 0) ||
  (tag === 12 && index < 2) ||
  (tag === 14 && index === 1) ||
  (tag === 79 && index === 1);

/**
 * The staged step of builtin `tag` over `arguments_` (presented, with value
 * roots `roots`) and its result, which the caller adds to the store, or the
 * witness of its failure.
 */
export const executeMidgardCekStagedBuiltin = (
  store: MidgardCekSemanticStore,
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
):
  | {
      readonly kind: "success";
      readonly witness: StagedWitness;
      readonly result: MidgardCekDirectValueWitness;
    }
  | ({ readonly kind: "failure" } & MidgardCekStagedFailure) => {
  const group = midgardCekSemanticGroup(tag);
  if (group !== 7 && group !== 8 && group !== 9) {
    throw new Error("CEK staged builtin has no staged semantics group");
  }
  // byteStringToInteger's endianness and the logical builtins' padding are
  // committed Booleans, not operands.
  const conversion = group === 9;
  const leading = conversion || (tag >= 75n && tag <= 77n) ? 1 : 0;
  const operands = arguments_
    .slice(leading)
    .map((argument, index) =>
      operandOf(store, roots[index + leading]!, argument),
    );
  // The reference byteStringToInteger reverses a little-endian input in
  // place, so each evaluation reads a copy of it.
  const values = () =>
    conversion
      ? [
          committedBoolean(arguments_[0]!),
          CEKConst.byteString(Buffer.from(operands[0]!.bytes)),
        ]
      : [
          ...arguments_.slice(0, leading).map(committedBoolean),
          ...operands.map((operand) => operand.value),
        ];
  const evaluate = () =>
    evaluateMidgardCekBuiltinOnValues(Number(tag), values());
  const failure = midgardCekStagedFailure(
    tag,
    arguments_,
    operands,
    () => evaluate() === "failure",
  );
  if (failure !== null) return { kind: "failure", ...failure };
  const evaluated = evaluate();
  if (evaluated === "failure") {
    throw new Error("CEK staged builtin failure has no staged proof");
  }
  const stagedOperands: Buffer[] = [];
  const sources = operands.map(
    (operand, index): MidgardCekStagedOperandSource => {
      if (leafOnly(Number(tag))) return operand.leaf;
      if (smallInteger(Number(tag), index)) {
        return { kind: "smallInteger", value: operand.value.value as bigint };
      }
      if (operand.bytes.length <= MIDGARD_CEK_STAGED_CHUNK_BYTES) {
        return { kind: "inline", bytes: operand.bytes };
      }
      const known = stagedOperands.findIndex((bytes) =>
        bytes.equals(operand.bytes),
      );
      if (known >= 0) return { kind: "staged", operand: known };
      stagedOperands.push(operand.bytes);
      return { kind: "staged", operand: stagedOperands.length - 1 };
    },
  );
  const canonical = encodeMidgardCekCanonicalConstant(
    new UPLCConst(evaluated.type, evaluated.value as never),
  );
  const result = referenceConstantToDirectWitness(evaluated, true);
  if (result.kind === "semanticConstant") {
    // Later steps read a large result through its semantic tree.
    store.installTree(
      commitMidgardCekDataTree(
        plutusDataFromCborIterative(canonical.payloadCbor),
      ),
    );
  }
  const witness: StagedWitness = {
    kind: "executeBuiltinStaged",
    tag,
    arguments: arguments_.map(semanticizeMidgardCekDirectConstant),
    result: semanticizeMidgardCekDirectConstant(result),
    material: { runs: { refs: [], operands: [] }, sources },
    stagedOperands,
  };
  // The staged arms check protocol version 11's bounds, which the shared
  // relation leaves to them: a step they refuse is not emitted.
  const staged = stageMidgardCekStagedBuiltinMaterial(
    witness.material,
    stagedOperands,
    midgardCekPlaceholderStaging(stagedOperands),
  );
  if (
    !verifyMidgardCekStagedBuiltin(
      tag,
      witness.arguments,
      witness.result,
      staged,
      midgardCekStagedReferenceInputs(staged, stagedOperands),
    )
  ) {
    throw new Error("CEK staged builtin step is past the staged arms' bounds");
  }
  return { kind: "success", witness, result };
};
