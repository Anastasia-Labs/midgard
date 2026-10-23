/**
 * The staged semantic arms (arm 31, semantics groups 7, 8 and 9): the
 * TypeScript twin of `cek_staged_arm_v1` and, for multiplyInteger and the
 * divisions (group 9), of the result `cek_staged_product_v1` verifies.
 *
 * Every argument and the result are compared in the compacted form the roots
 * hop commits. Each argument is bound to its committed leaf through its
 * source, and the builtin's result is computed by the same value-level
 * relation the direct route uses. Arguments past protocol version 11's
 * bounds (65,536 bytes for a byte-string argument of a bounded builtin, and
 * [-2^262143, 2^262143 - 1] for an integer argument of an integer builtin
 * but equalsInteger) are refused. The bounds are on arguments only: a result
 * past the integer range is a success, as on L1.
 */
import {
  bindMidgardCekStagedOperand,
  collectMidgardCekStagedRuns,
  hashMidgardCekDataNode,
  midgardCekBytesMemoryWords,
  type MidgardCekCanonicalInteger,
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekCanonicalIntegerValue,
  midgardCekDataBytesCborLength,
  midgardCekFlatRoot,
  midgardCekIntegerLeafFromCbor,
  type MidgardCekStagedReferenceInput,
  parseMidgardCekCanonicalIntegerCbor,
} from "@al-ft/midgard-core";
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type {
  Bytes,
  MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import {
  evaluateMidgardCekBuiltinOnValues,
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekSemanticConstantWitness } from "./cek-constant.js";
import type {
  MidgardCekStagedBuiltinMaterial,
  MidgardCekStagedOperandSource,
} from "./cek-staged-builtin.material.js";
import {
  midgardCekStagedSourceCount,
  verifyMidgardCekStagedProduct,
} from "./cek-staged-product.verify.js";

/**
 * The semantics group of an arm 31 step for builtin `tag`, as the budget hop
 * routes it, or null for a tag no semantics group takes.
 */
export const midgardCekSemanticGroup = (tag: bigint): number | null => {
  const t = Number(tag);
  if (tag < 0n || tag > 255n) return null;
  if (t === 29 || t === 30) return 0;
  if (t === 31 || t === 32 || t === 35) return 1;
  if (t === 33 || t === 34) return 2;
  if (t === 36) return 3;
  if (t === 37 || t === 42) return 4;
  if (t >= 47 && t <= 50) return 6;
  if (t === 51) return 10;
  if ([39, 40, 41, 44, 45, 46].includes(t)) return 5;
  if (t >= 2 && t <= 6) return 9;
  if (t >= 0 && t <= 9) return 8;
  if ((t >= 10 && t <= 25 && t !== 21) || t === 71 || t === 72 || t === 86) {
    return 7;
  }
  return null;
};

/**
 * The builtins the staged arms prove: the integer builtins up to
 * lessThanEqualsInteger and the byte-string and string builtins of
 * semantics group 7.
 */
export const midgardCekStagedBuiltinProves = (tag: bigint): boolean =>
  midgardCekSemanticGroup(tag) === 7 || (tag >= 0n && tag <= 9n);

/** The canonical CBOR length of a byte string of 65,536 bytes. */
export const MIDGARD_CEK_STAGED_MAX_BOUNDED_BYTES_CBOR_LENGTH = 67_586n;
const MAX_BOUNDED_INTEGER_ARGUMENT_BYTES = 32_768;

const INTEGER_TYPE = Buffer.from("9f00ff", "hex");
const BYTES_TYPE = Buffer.from("9f01ff", "hex");
const STRING_TYPE = Buffer.from("9f02ff", "hex");

type Compacted = MidgardCekSemanticConstantWitness;

const refuse = (reason: string): never => {
  throw new Error(`staged step refused: ${reason}`);
};

const same = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const sameCompacted = (left: Compacted, right: Compacted): boolean =>
  same(left.typeCbor, right.typeCbor) &&
  same(left.payload.root, right.payload.root) &&
  left.payload.cborLength === right.payload.cborLength &&
  left.payload.memory === right.payload.memory &&
  left.memory === right.memory;

const compact = (value: MidgardCekDirectValueWitness): Compacted => {
  const compacted = semanticizeMidgardCekDirectConstant(value);
  if (compacted.kind !== "semanticConstant") return refuse("not a constant");
  return compacted.witness;
};

/** A constant value in the compacted form the roots hop commits. */
const compactValue = (value: CEKConst): Compacted =>
  compact(referenceConstantToDirectWitness(value, true));

const bytesMemory = (string: boolean, length: bigint): bigint =>
  string ? length / 4n : midgardCekBytesMemoryWords(length);

/** The Data summary of a byte-string leaf from its flat root and length. */
const bytesSummary = (length: bigint, root: Bytes): Compacted["payload"] => {
  if (length < 0n || root.length !== 32) return refuse("malformed leaf");
  const node = {
    kind: "bytes" as const,
    bytesRoot: root,
    bytesLength: length,
    cborLength: midgardCekDataBytesCborLength(length),
    memory: 4n + midgardCekBytesMemoryWords(length),
  };
  return {
    root: hashMidgardCekDataNode(node),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

const samePayload = (
  left: Compacted["payload"],
  right: Compacted["payload"],
): boolean =>
  same(left.root, right.root) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

type ArmInputs = {
  readonly tag: number;
  readonly arguments: readonly (readonly [
    Compacted,
    MidgardCekStagedOperandSource,
  ])[];
  readonly result: Compacted;
  readonly runs: Buffer[][];
  readonly operands: readonly (readonly number[])[];
};

const stagedOperand = (inputs: ArmInputs, operand: number) => {
  const indices = inputs.operands[operand];
  if (indices === undefined) return refuse("no such staged operand");
  return bindMidgardCekStagedOperand(inputs.runs, indices);
};

type BoundBytes = {
  readonly root: Bytes;
  readonly length: bigint;
  readonly bytes: Buffer | null;
};

const bindBytes = (
  inputs: ArmInputs,
  [value, source]: readonly [Compacted, MidgardCekStagedOperandSource],
  string: boolean,
  bounded: boolean,
): BoundBytes => {
  if (!same(value.typeCbor, string ? STRING_TYPE : BYTES_TYPE)) {
    return refuse("operand type");
  }
  if (
    bounded &&
    value.payload.cborLength > MIDGARD_CEK_STAGED_MAX_BOUNDED_BYTES_CBOR_LENGTH
  ) {
    return refuse("operand past the argument bound");
  }
  let bound: BoundBytes;
  if (source.kind === "inline") {
    const bytes = Buffer.from(source.bytes);
    bound = {
      root: midgardCekFlatRoot(bytes),
      length: BigInt(bytes.length),
      bytes,
    };
  } else if (source.kind === "staged") {
    const staged = stagedOperand(inputs, source.operand);
    bound = {
      root: staged.root,
      length: BigInt(staged.length),
      bytes: Buffer.concat(staged.chunks),
    };
  } else if (source.kind === "bytesLeaf") {
    bound = { root: source.root, length: source.length, bytes: null };
  } else {
    return refuse("operand source");
  }
  if (!samePayload(bytesSummary(bound.length, bound.root), value.payload)) {
    return refuse("operand leaf");
  }
  if (value.memory !== bytesMemory(string, bound.length)) {
    return refuse("operand memory");
  }
  return bound;
};

const INT64_MIN = -(2n ** 63n);
const INT64_MAX = 2n ** 63n - 1n;

/** The value of an integer argument of a byte-string builtin. */
const smallInteger = ([value, source]: readonly [
  Compacted,
  MidgardCekStagedOperandSource,
]): bigint => {
  if (source.kind !== "smallInteger") return refuse("integer source");
  if (source.value < INT64_MIN || source.value > INT64_MAX) {
    return refuse("integer past a machine integer");
  }
  if (!sameCompacted(compactValue(CEKConst.int(source.value)), value)) {
    return refuse("integer leaf");
  }
  return source.value;
};

/** The byte-string or string operands of `tag`, and its integer operands. */
const scalarOperands = <T>(
  tag: number,
  arguments_: readonly T[],
): readonly [readonly T[], readonly T[]] => {
  const [a, b, c] = arguments_;
  if (tag === 11 && arguments_.length === 2) return [[b!], [a!]];
  if (tag === 12 && arguments_.length === 3) return [[c!], [a!, b!]];
  if (tag === 14 && arguments_.length === 2) return [[a!], [b!]];
  if (tag === 11 || tag === 12 || tag === 14) return refuse("argument count");
  return [arguments_, []];
};

const verifyBytes = (inputs: ArmInputs): boolean => {
  const { tag } = inputs;
  const string = tag >= 22 && tag <= 24;
  const bounded = tag !== 13 && !string;
  const [byteArguments, integerArguments] = scalarOperands(
    tag,
    inputs.arguments,
  );
  const operands = byteArguments.map((argument) =>
    bindBytes(inputs, argument, string, bounded),
  );
  if (tag === 13) {
    if (operands.length !== 1) return false;
    return sameCompacted(
      inputs.result,
      compactValue(CEKConst.int(operands[0]!.length)),
    );
  }
  if (tag === 15 || tag === 23) {
    const [left, right] = operands;
    if (operands.length !== 2) return false;
    // Equal bytes have equal flat roots, and unequal bytes unequal ones.
    return sameCompacted(
      inputs.result,
      compactValue(CEKConst.bool(same(left!.root, right!.root))),
    );
  }
  if (tag === 22) {
    if (operands.length !== 2) return false;
    // appendString joins the bytes, as the arm does, without decoding them.
    const joined = Buffer.concat(
      operands.map(
        (operand) => operand.bytes ?? refuse("operand bytes not revealed"),
      ),
    );
    const length = BigInt(joined.length);
    return sameCompacted(inputs.result, {
      typeCbor: STRING_TYPE,
      payload: bytesSummary(length, midgardCekFlatRoot(joined)),
      memory: bytesMemory(true, length),
    });
  }
  if (tag === 24) {
    if (operands.length !== 1) return false;
    const [operand] = operands;
    // encodeUtf8 keeps the bytes, so the result has the operand's leaf.
    return sameCompacted(inputs.result, {
      typeCbor: BYTES_TYPE,
      payload: bytesSummary(operand!.length, operand!.root),
      memory: bytesMemory(false, operand!.length),
    });
  }
  const integers = integerArguments.map(smallInteger);
  const values = operands.map((operand) => {
    if (operand.bytes === null) return refuse("operand bytes not revealed");
    return CEKConst.byteString(operand.bytes);
  });
  const ordered =
    tag === 11
      ? [CEKConst.int(integers[0]!), values[0]!]
      : tag === 12
        ? [CEKConst.int(integers[0]!), CEKConst.int(integers[1]!), values[0]!]
        : tag === 14
          ? [values[0]!, CEKConst.int(integers[0]!)]
          : values;
  const result = evaluateMidgardCekBuiltinOnValues(tag, ordered);
  return (
    result !== "failure" && sameCompacted(inputs.result, compactValue(result))
  );
};

/** True when the integer is in [-2^262143, 2^262143 - 1]. */
const integerInRange = (integer: MidgardCekCanonicalInteger): boolean => {
  const size = integer.argument.length;
  return (
    size < MAX_BOUNDED_INTEGER_ARGUMENT_BYTES ||
    (size === MAX_BOUNDED_INTEGER_ARGUMENT_BYTES &&
      integer.argument[0]! <= 0x7f)
  );
};

/** The bytes an inline or staged source reveals. */
const sourceCbor = (
  inputs: ArmInputs,
  source: MidgardCekStagedOperandSource,
): Buffer => {
  if (source.kind === "inline") return Buffer.from(source.bytes);
  if (source.kind === "staged") {
    return Buffer.concat(stagedOperand(inputs, source.operand).chunks);
  }
  return refuse("operand source");
};

const bindInteger = (
  inputs: ArmInputs,
  [value, source]: readonly [Compacted, MidgardCekStagedOperandSource],
  bounded: boolean,
): { readonly root: Bytes; readonly integer: bigint | null } => {
  if (!same(value.typeCbor, INTEGER_TYPE)) return refuse("operand type");
  if (source.kind === "integerLeaf") {
    const leaf = midgardCekIntegerLeafFromCbor(source);
    if (!samePayload(leaf, value.payload) || value.memory !== source.words) {
      return refuse("operand leaf");
    }
    return { root: value.payload.root, integer: null };
  }
  const cbor = sourceCbor(inputs, source);
  const integer = parseMidgardCekCanonicalIntegerCbor(cbor);
  const words = midgardCekCanonicalIntegerMagnitudeWords(integer);
  const leaf = midgardCekIntegerLeafFromCbor({
    cborRoot: midgardCekFlatRoot(cbor),
    cborLength: BigInt(cbor.length),
    words,
  });
  if (!samePayload(leaf, value.payload) || value.memory !== words) {
    return refuse("operand leaf");
  }
  if (bounded && !integerInRange(integer)) {
    return refuse("operand past the integer range");
  }
  return {
    root: value.payload.root,
    integer: midgardCekCanonicalIntegerValue(integer),
  };
};

const verifyInteger = (inputs: ArmInputs): boolean => {
  const { tag } = inputs;
  // equalsInteger takes operands of any size.
  const bounded = tag !== 7;
  if (inputs.arguments.length !== 2) return false;
  const left = bindInteger(inputs, inputs.arguments[0]!, bounded);
  const right = bindInteger(inputs, inputs.arguments[1]!, bounded);
  if (tag === 7) {
    // Bound leaves are canonical, so equal values have equal leaves.
    return sameCompacted(
      inputs.result,
      compactValue(CEKConst.bool(same(left.root, right.root))),
    );
  }
  if (left.integer === null || right.integer === null) return false;
  if (tag !== 0 && tag !== 1 && tag !== 8 && tag !== 9) return false;
  const result = evaluateMidgardCekBuiltinOnValues(tag, [
    CEKConst.int(left.integer),
    CEKConst.int(right.integer),
  ]);
  if (result === "failure") return false;
  return sameCompacted(inputs.result, compactValue(result));
};

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
            memory: bytesMemory(true, BigInt(constant.bytes.length)),
          },
        }
      : refuse("not a constant");
};

/**
 * Whether the staged arm accepts builtin `tag` over `arguments_` (as the
 * binder presents them) returning `result`, with `material` read against the
 * transaction's `referenceInputs`.
 */
export const verifyMidgardCekStagedBuiltin = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekStagedBuiltinMaterial,
  referenceInputs: readonly MidgardCekStagedReferenceInput[],
): boolean => {
  try {
    const group = midgardCekSemanticGroup(tag);
    if (group !== 7 && group !== 8 && group !== 9) return false;
    const compacted = arguments_.map(compact);
    if (
      midgardCekStagedSourceCount(tag, compacted.length) !==
      material.sources.length
    ) {
      return false;
    }
    const inputs: ArmInputs = {
      tag: Number(tag),
      arguments: compacted.map(
        (value, index) => [value, material.sources[index]!] as const,
      ),
      result: compact(result),
      runs: collectMidgardCekStagedRuns(referenceInputs, material.runs.refs),
      operands: material.runs.operands,
    };
    if (group === 9) {
      return verifyMidgardCekStagedProduct(
        tag,
        inputs.arguments,
        inputs.result,
        material.sources.slice(compacted.length),
        {
          bind: (value, source, bounded) =>
            bindInteger(inputs, [value, source], bounded).integer,
          read: (source) =>
            midgardCekCanonicalIntegerValue(
              parseMidgardCekCanonicalIntegerCbor(sourceCbor(inputs, source)),
            ),
        },
      );
    }
    return group === 7 ? verifyBytes(inputs) : verifyInteger(inputs);
  } catch {
    return false;
  }
};
