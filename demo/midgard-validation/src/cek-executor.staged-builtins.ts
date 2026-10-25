/**
 * The staged route of the scalar builtins (arm 31, semantics groups 7, 8 and
 * 9): a byte-string, string or integer builtin whose arguments or result do
 * not fit the direct carriage bound.
 *
 * The step is a pure function of the execution graph: each argument gets one
 * typed source (its bytes inline, a staged operand by index, its leaf
 * preimage, or a small integer's value), and the staged operands' bytes ride
 * with the witness. multiplyInteger and the divisions also carry the CBOR
 * their column loop compares (the result's, the quotient's, or both), and a
 * step inlines at most two chunks of bytes in all, staging the rest. The
 * material names no staging outputs; the one-step driver stages the operands
 * and fills them in (`stageMidgardCekStagedBuiltinMaterial`) before the
 * binder.
 *
 * The result is the value-level relation the direct route uses. A step the
 * staged arms cannot certify (a builtin failure, or an argument or result
 * past protocol version 11's bounds) is not emitted: it throws, so the run is
 * indeterminate rather than given a step no verifier accepts.
 */
import {
  encodeMidgardCekCanonicalIntegerCbor,
  MIDGARD_CEK_STAGED_CHUNK_BYTES,
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekCanonicalIntegerOf,
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
  midgardCekDivisionByZero,
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.evaluate-reference-builtin.js";
import { encodeMidgardCekCanonicalConstant } from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import type { MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import type { MidgardCekCoreStepWitness } from "./cek-machine.js";
import type { MidgardCekStagedOperandSource } from "./cek-staged-builtin.material.js";
import {
  midgardCekPlaceholderStaging,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "./cek-staged-builtin.material.js";
import {
  midgardCekSemanticGroup,
  midgardCekStagedBuiltinProves,
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
 * direct carriage bound, or arguments or a result that do not fit it. A
 * division by zero takes the direct failure route whatever the dividend.
 */
export const isMidgardCekStagedStep = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean =>
  midgardCekStagedBuiltinProves(tag) &&
  !midgardCekDivisionByZero(tag, arguments_) &&
  (arguments_.some((argument) => argument.kind !== "constant") ||
    !midgardCekDirectBuiltinFits(tag, arguments_));

type Operand = {
  readonly type: "integer" | "bytes" | "string";
  /** A byte string's or string's bytes, or an integer's canonical CBOR. */
  readonly bytes: Buffer;
  readonly value: CEKConst;
  readonly leaf: MidgardCekStagedOperandSource;
};

const utf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

const operandOf = (
  store: MidgardCekSemanticStore,
  root: Bytes,
  argument: MidgardCekDirectValueWitness,
): Operand => {
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

/** Whether the staged arm binds every argument of `tag` by its leaf alone. */
const leafOnly = (tag: number): boolean =>
  tag === 7 || tag === 13 || tag === 15 || tag === 23 || tag === 24;

/** Whether argument `index` of `tag` is a byte-string builtin's integer. */
const smallInteger = (tag: number, index: number): boolean =>
  (tag === 11 && index === 0) ||
  (tag === 12 && index < 2) ||
  (tag === 14 && index === 1);

/**
 * The staged step of builtin `tag` over `arguments_` (presented, with value
 * roots `roots`) and its result, which the caller adds to the store.
 */
export const executeMidgardCekStagedBuiltin = (
  store: MidgardCekSemanticStore,
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
): {
  readonly witness: StagedWitness;
  readonly result: MidgardCekDirectValueWitness;
} => {
  const group = midgardCekSemanticGroup(tag);
  if (group !== 7 && group !== 8 && group !== 9) {
    throw new Error("CEK staged builtin has no staged semantics group");
  }
  const operands = arguments_.map((argument, index) =>
    operandOf(store, roots[index]!, argument),
  );
  const evaluated = evaluateMidgardCekBuiltinOnValues(
    Number(tag),
    operands.map((operand) => operand.value),
  );
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
  return { witness, result };
};
