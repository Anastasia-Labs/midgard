/**
 * The staged route of the scalar builtins (arm 31, semantics groups 7, 8, 9
 * and 11): a byte-string, string, bitwise, signature or integer builtin,
 * or integerToByteString, byteStringToInteger, shiftByteString or
 * rotateByteString, whose arguments or result do not fit the direct carriage
 * bound.
 *
 * The step is a pure function of the execution graph: each argument gets one
 * typed source (its bytes inline, a staged operand by index, its leaf
 * preimage, or a small integer's value), and the staged operands' bytes ride
 * with the witness. multiplyInteger and the divisions also carry the CBOR
 * their column loop compares (the result's, the quotient's, or both), and a
 * step inlines at most two chunks of bytes in all, staging the rest. The material names no staging outputs; the one-step
 * driver stages the operands and fills them in
 * (`stageMidgardCekStagedBuiltinMaterial`) before the binder.
 *
 * The result is the value-level relation the direct route uses. A step that
 * fails on an argument past protocol version 11's bounds or on its operands'
 * content is given its failure witness (`midgardCekStagedFailure`). A step
 * no arm can certify is not emitted: it throws, so the run is indeterminate
 * rather than given a step no verifier accepts.
 *
 * serialiseData (semantics group 10) takes this route too, past the direct
 * bound or over an argument presented by its summary: the serialise loop
 * proves it over its staged result (`cek-executor.serialise-builtin.ts`).
 * So does writeBits (semantics group 12) over an input of at most 4,096
 * bytes and an index list presented by its summary or past 64 indices: the
 * writeBits loop proves it over its indices
 * (`cek-executor.write-bits-builtin.ts`).
 */
import {
  encodeMidgardCekCanonicalIntegerCbor,
  MIDGARD_CEK_STAGED_CHUNK_BYTES,
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekCanonicalIntegerOf,
  parseMidgardCekCanonicalIntegerCbor,
} from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type {
  Bytes,
  MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import { midgardCekDivisionByZero } from "./cek-builtin.compacted-failures.js";
import {
  evaluateMidgardCekBuiltinOnValues,
  midgardCekDirectBuiltinFits,
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.evaluate-reference-builtin.js";
import {
  constantOfReferenceValue,
  referenceValueOf,
} from "./cek-builtin.reference-value.js";
import { encodeMidgardCekCanonicalConstant } from "./cek-constant.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import type { MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import { executeMidgardCekSerialiseBuiltin } from "./cek-executor.serialise-builtin.js";
import {
  type MidgardCekStagedFailure,
  midgardCekStagedFailure,
  type MidgardCekStagedOperand,
} from "./cek-executor.staged-failure.js";
import {
  executeMidgardCekWriteBitsBuiltin,
  midgardCekWriteBitsTakesLoop,
} from "./cek-executor.write-bits-builtin.js";
import type { MidgardCekCoreStepWitness } from "./cek-machine.js";
import { midgardCekSemanticGroup } from "./cek-semantic-group.js";
import type { MidgardCekSerialiseWitness } from "./cek-serialise-builtin.js";
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
import type { MidgardCekWriteBitsWitness } from "./cek-write-bits-builtin.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { UPLCConst } from "./uplc-constant.js";

type StagedWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinStaged" }
>;

/** verifyEcdsaSecp256k1Signature, writeBits and replicateByte, whose staged
 * steps the staged failure hop proves only when they fail. */
const failureOnly = (tag: bigint): boolean =>
  tag === 52n || tag === 80n || tag === 81n;

/**
 * Whether builtin `tag` over these presented arguments takes the staged
 * route: a builtin the staged arms prove, with an argument already past the
 * direct carriage bound, or arguments or a result that do not fit it. A
 * division by zero of a dividend under 4,096 words takes the direct failure
 * route.
 */
export const isMidgardCekStagedStep = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean =>
  (midgardCekStagedBuiltinProves(tag) || tag === 51n || failureOnly(tag)) &&
  !midgardCekDivisionByZero(tag, arguments_) &&
  (arguments_.some((argument) => argument.kind !== "constant") ||
    !midgardCekDirectBuiltinFits(tag, arguments_) ||
    (tag === 80n && midgardCekWriteBitsTakesLoop(arguments_)));

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
          : referenceValueOf(UPLCConst.str(utf8.decode(bytes))),
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

/** Whether operand `index` of `tag` is a byte-string builtin's integer or
 * integerToByteString's width. */
const smallInteger = (tag: number, index: number): boolean =>
  (tag === 73 && index === 0) ||
  (tag === 11 && index === 0) ||
  (tag === 12 && index < 2) ||
  (tag === 14 && index === 1) ||
  ((tag === 79 || tag === 82 || tag === 83) && index === 1);

/**
 * The staged step of builtin `tag` over `arguments_` (presented, with value
 * roots `roots`) and its result, which the caller adds to the store unless
 * the step gives the result's value root, or the witness of its failure.
 */
export const executeMidgardCekStagedBuiltin = (
  store: MidgardCekSemanticStore,
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
):
  | {
      readonly kind: "success";
      readonly witness:
        | StagedWitness
        | MidgardCekSerialiseWitness
        | MidgardCekWriteBitsWitness;
      readonly result: MidgardCekDirectValueWitness;
      readonly root?: Bytes;
      /** The charge, when the direct budget cannot read the arguments. */
      readonly budget?: MidgardCekBuiltinBudget;
    }
  | ({ readonly kind: "failure" } & (
      | MidgardCekStagedFailure
      | {
          readonly witness: MidgardCekWriteBitsWitness;
          readonly budget: MidgardCekBuiltinBudget;
        }
    )) => {
  if (tag === 51n) {
    return {
      kind: "success",
      ...executeMidgardCekSerialiseBuiltin(store, arguments_, roots),
    };
  }
  const looped =
    tag === 80n
      ? executeMidgardCekWriteBitsBuiltin(store, arguments_, roots)
      : null;
  if (looped !== null) return looped;
  const group = midgardCekSemanticGroup(tag);
  if (
    group !== 7 &&
    group !== 8 &&
    group !== 9 &&
    group !== 11 &&
    !failureOnly(tag)
  ) {
    throw new Error("CEK staged builtin has no staged semantics group");
  }
  // byteStringToInteger's and integerToByteString's endianness and the
  // logical builtins' padding are committed Booleans, not operands.
  const conversion = tag === 74n;
  const leading =
    conversion || tag === 73n || (tag >= 75n && tag <= 77n) ? 1 : 0;
  // writeBits' indices and Boolean are not operands: its failure reads its
  // input alone.
  const operands = arguments_
    .slice(leading, tag === 80n ? 1 : undefined)
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
  if (failureOnly(tag)) {
    throw new Error("CEK staged builtin step has no staged proof");
  }
  const evaluated = evaluate();
  if (evaluated === "failure") {
    throw new Error("CEK staged builtin failure has no staged proof");
  }
  const stagedOperands: Buffer[] = [];
  let inlined = 0;
  const carried = (bytes: Buffer): MidgardCekStagedOperandSource => {
    if (
      bytes.length <= MIDGARD_CEK_STAGED_CHUNK_BYTES &&
      inlined + bytes.length <= 2 * MIDGARD_CEK_STAGED_CHUNK_BYTES
    ) {
      inlined += bytes.length;
      return { kind: "inline", bytes };
    }
    const known = stagedOperands.findIndex((staged) => staged.equals(bytes));
    if (known >= 0) return { kind: "staged", operand: known };
    stagedOperands.push(bytes);
    return { kind: "staged", operand: stagedOperands.length - 1 };
  };
  const sources = operands.map(
    (operand, index): MidgardCekStagedOperandSource => {
      if (leafOnly(Number(tag))) return operand.leaf;
      if (smallInteger(Number(tag), index)) {
        return { kind: "smallInteger", value: operand.value.value as bigint };
      }
      return carried(operand.bytes);
    },
  );
  if (group === 9) {
    const cbor = (value: bigint): Buffer =>
      encodeMidgardCekCanonicalIntegerCbor(midgardCekCanonicalIntegerOf(value));
    const [a, b] = operands.map((operand) => operand.value.value as bigint);
    const result = evaluated.value as bigint;
    if (tag === 5n || tag === 6n) {
      // The quotient of the division the remainder rounds as.
      const quotient = evaluateMidgardCekBuiltinOnValues(tag === 5n ? 4 : 3, [
        CEKConst.int(a!),
        CEKConst.int(b!),
      ]);
      if (quotient === "failure") {
        throw new Error("CEK staged builtin failure has no staged proof");
      }
      sources.push(carried(cbor(quotient.value as bigint)));
    }
    sources.push(carried(cbor(result)));
  }
  const canonical = encodeMidgardCekCanonicalConstant(
    constantOfReferenceValue(evaluated),
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
