import {
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_BLS_ML_RESULT_MEMORY,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
} from "@al-ft/midgard-core";

import {
  argumentKinds,
  type Bytes,
  directWitnessPayloadBytes,
  hashMidgardCekRuntimeArguments,
  matchesKind,
  type MidgardCekConstantTypeReader,
  type MidgardCekDirectValueWitness,
  type MidgardCekRuntimeValueWitness,
  mkConsIsWellTyped,
  sameBytes,
} from "./cek-builtin.argument-kinds.js";
import {
  isMidgardCekPv11BuiltinTag,
  midgardCekPv11BuiltinCostSizes,
} from "./cek-builtin.pv11.js";
import {
  decodeMidgardCekConstantWitness,
  hashMidgardCekConstantWitness,
  hashMidgardCekSemanticConstantWitness,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  midgardCekConstantMemorySize,
} from "./cek-constant.js";
import {
  computeMidgardCekBuiltinBudget,
  type MidgardCekBuiltinBudget,
} from "./cek-cost.js";
import { DataB, DataConstr, DataI, DataList } from "./uplc-data.js";

/**
 * Whether a saturated builtin's arguments fail its argument kinds, so that
 * applying it is a builtin type failure on L1, whatever their size.
 */
export const midgardCekBuiltinArgumentsFailKinds = (
  tag: bigint,
  arguments_: readonly MidgardCekRuntimeValueWitness[],
  typeOf?: MidgardCekConstantTypeReader,
): boolean => {
  try {
    if (
      tag < 0n ||
      tag > MIDGARD_CEK_MAX_BUILTIN_TAG ||
      tag > BigInt(Number.MAX_SAFE_INTEGER)
    ) {
      return false;
    }
    const numericTag = Number(tag);
    const kinds = argumentKinds(numericTag);
    if (arguments_.length !== kinds.length) return false;
    const wellTyped =
      numericTag === 32
        ? mkConsIsWellTyped(arguments_, typeOf)
        : arguments_.every((argument, index) =>
            matchesKind(argument, kinds[index], typeOf),
          );
    return !wellTyped;
  } catch {
    return false;
  }
};

/**
 * Whether the verifier proves a builtin type failure on these arguments:
 * they fail the builtin's argument kinds and their revealed payloads
 * together fit one direct payload.
 */
export const midgardCekBuiltinArgumentsAreIllTyped = (
  tag: bigint,
  arguments_: readonly MidgardCekRuntimeValueWitness[],
  typeOf?: MidgardCekConstantTypeReader,
): boolean =>
  directWitnessPayloadBytes(arguments_) <=
    BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES) &&
  midgardCekBuiltinArgumentsFailKinds(tag, arguments_, typeOf);

export const verifyMidgardCekBuiltinTypeFailure = (
  tag: bigint,
  builtinValueRoot: Bytes,
  arguments_: readonly MidgardCekRuntimeValueWitness[],
): boolean => {
  if (!midgardCekBuiltinArgumentsAreIllTyped(tag, arguments_)) return false;
  try {
    const { root, count } = hashMidgardCekRuntimeArguments(arguments_);
    return sameBytes(
      builtinValueRoot,
      hashMidgardCekValueNode({
        kind: "builtin",
        tag,
        forcesRemaining: 0n,
        argumentsCount: count,
        argumentsRoot: root,
      }),
    );
  } catch {
    return false;
  }
};

export type MidgardCekDirectBuiltinEvaluation =
  | {
      readonly kind: "success";
      readonly result: MidgardCekDirectValueWitness;
      readonly budget: MidgardCekBuiltinBudget;
    }
  | {
      readonly kind: "failure";
      readonly budget: MidgardCekBuiltinBudget;
    };

export const hashMidgardCekDirectValueWitness = (
  value: MidgardCekDirectValueWitness,
): Bytes => {
  switch (value.kind) {
    case "constant":
      return hashMidgardCekConstantWitness(value.witness);
    case "semanticConstant":
      return hashMidgardCekSemanticConstantWitness(value.witness);
    case "opaque":
      if (value.root.length !== 32) {
        throw new Error("V1 opaque CEK value root must be bytes32");
      }
      return value.root;
    case "blsMillerLoop":
      return hashMidgardCekValueNode({
        kind: "blsMillerLoop",
        expressionRoot: value.expressionRoot,
      });
  }
};

export const hashMidgardCekDirectArguments = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): { readonly root: Bytes; readonly count: bigint } => {
  let root: Bytes = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT;
  let count = 0n;
  for (const argument of arguments_) {
    count += 1n;
    root = hashMidgardCekSequenceNode({
      head: hashMidgardCekDirectValueWitness(argument),
      tail: root,
      length: count,
    });
  }
  return Object.freeze({ root, count });
};

const decodedDirectConstant = (value: MidgardCekDirectValueWitness) => {
  if (value.kind !== "constant") {
    throw new Error("V1 builtin requires a revealed constant");
  }
  return decodeMidgardCekConstantWitness(value.witness);
};

/**
 * The memory size of a revealed constant. The default decodes its witness;
 * a caller that already holds the constant's committed value node may pass
 * a reader of the size that node records.
 */
export type MidgardCekRevealedConstantMemory = (
  witness: Extract<
    MidgardCekDirectValueWitness,
    { readonly kind: "constant" }
  >["witness"],
) => bigint;

const decodedConstantMemory: MidgardCekRevealedConstantMemory = (witness) => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  return midgardCekConstantMemorySize(decoded.type, decoded.payload);
};

const directValueMemorySize = (
  value: MidgardCekDirectValueWitness,
  memoryOf: MidgardCekRevealedConstantMemory = decodedConstantMemory,
): bigint => {
  // An opaque value has no committed size. The only builtin positions that
  // admit one are the polymorphic branches of tags 26, 27, 28, 31 and 36,
  // which midgardCekDirectBuiltinCostSizes sizes without reading them.
  if (value.kind === "opaque") {
    throw new Error("V1 opaque CEK value has no memory size");
  }
  if (value.kind === "blsMillerLoop") {
    if (value.expressionRoot.length !== 32) {
      throw new Error("V1 BLS expression root must be bytes32");
    }
    return MIDGARD_CEK_BLS_ML_RESULT_MEMORY;
  }
  if (value.kind === "semanticConstant") {
    return value.witness.memory;
  }
  return memoryOf(value.witness);
};

const directBoolean = (value: MidgardCekDirectValueWitness): boolean => {
  const decoded = decodedDirectConstant(value);
  if (
    decoded.type.kind !== "boolean" ||
    !(decoded.payload instanceof DataConstr)
  ) {
    throw new Error("V1 builtin requires a boolean");
  }
  return decoded.payload.constr === 1n;
};

export const directByteLength = (
  value: MidgardCekDirectValueWitness,
): number => {
  const decoded = decodedDirectConstant(value);
  if (decoded.type.kind !== "bytes" || !(decoded.payload instanceof DataB)) {
    throw new Error("V1 builtin requires a byte string");
  }
  return decoded.payload.bytes.length;
};

export const directInteger = (value: MidgardCekDirectValueWitness): bigint => {
  const decoded = decodedDirectConstant(value);
  if (decoded.type.kind !== "integer" || !(decoded.payload instanceof DataI)) {
    throw new Error("V1 builtin requires an integer");
  }
  return decoded.payload.int;
};

const directListLength = (value: MidgardCekDirectValueWitness): number => {
  const decoded = decodedDirectConstant(value);
  if (decoded.type.kind !== "list" || !(decoded.payload instanceof DataList)) {
    throw new Error("V1 builtin requires a list");
  }
  return decoded.payload.list.length;
};

/**
 * Cardano costs an output width (integerToByteString) or a replication count
 * (replicateByte) as the words it occupies. Out-of-range widths fail before
 * costing; this stays total so the failure arm never throws.
 */
const widthCostSize = (width: bigint): bigint =>
  width <= 0n ? 0n : (width - 1n) / 8n + 1n;

const MAX_SIGNED_64 = 9_223_372_036_854_775_807n;

/**
 * Cardano costs a shift or rotation amount by its absolute value, saturated
 * to the largest signed 64-bit integer.
 */
const literalCostSize = (literal: bigint): bigint => {
  const magnitude = literal < 0n ? -literal : literal;
  return magnitude > MAX_SIGNED_64 ? MAX_SIGNED_64 : magnitude;
};

export const midgardCekDirectBuiltinCostSizes = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  memoryOf?: MidgardCekRevealedConstantMemory,
): readonly bigint[] => {
  if (isMidgardCekPv11BuiltinTag(tag)) {
    return Object.freeze(midgardCekPv11BuiltinCostSizes(tag, arguments_));
  }
  if (tag === 26n) {
    if (arguments_.length !== 3) {
      throw new Error("ifThenElse requires three arguments");
    }
    directBoolean(arguments_[0]!);
    return Object.freeze([1n, 1n, 1n]);
  }
  if (tag === 27n) {
    if (arguments_.length !== 2) {
      throw new Error("chooseUnit requires two arguments");
    }
    const unit = decodedDirectConstant(arguments_[0]!);
    if (unit.type.kind !== "unit") {
      throw new Error("chooseUnit requires unit");
    }
    return Object.freeze([1n, 1n]);
  }
  if (tag === 28n) {
    if (arguments_.length !== 2) {
      throw new Error("trace requires two arguments");
    }
    const message = decodedDirectConstant(arguments_[0]!);
    if (message.type.kind !== "string" || !(message.payload instanceof DataB)) {
      throw new Error("trace requires a string message");
    }
    return Object.freeze([BigInt(message.payload.bytes.length), 1n]);
  }
  if (tag === 31n) {
    if (arguments_.length !== 3) {
      throw new Error("chooseList requires three arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!, memoryOf),
      1n,
      1n,
    ]);
  }
  if (tag === 36n) {
    if (arguments_.length !== 6) {
      throw new Error("chooseData requires six arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!, memoryOf),
      1n,
      1n,
      1n,
      1n,
      1n,
    ]);
  }
  if (tag === 73n) {
    if (arguments_.length !== 3) {
      throw new Error("integerToByteString requires three arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!, memoryOf),
      widthCostSize(directInteger(arguments_[1]!)),
      directValueMemorySize(arguments_[2]!, memoryOf),
    ]);
  }
  if (tag === 80n) {
    if (arguments_.length !== 3) {
      throw new Error("writeBits requires three arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!, memoryOf),
      BigInt(directListLength(arguments_[1]!)),
      directValueMemorySize(arguments_[2]!, memoryOf),
    ]);
  }
  if (tag === 81n) {
    if (arguments_.length !== 2) {
      throw new Error("replicateByte requires two arguments");
    }
    return Object.freeze([
      widthCostSize(directInteger(arguments_[0]!)),
      directValueMemorySize(arguments_[1]!, memoryOf),
    ]);
  }
  if (tag === 82n || tag === 83n) {
    if (arguments_.length !== 2) {
      throw new Error("shift or rotate requires two arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!, memoryOf),
      literalCostSize(directInteger(arguments_[1]!)),
    ]);
  }
  return Object.freeze(
    arguments_.map((argument) => directValueMemorySize(argument, memoryOf)),
  );
};

export const midgardCekDirectBuiltinBudget = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  memoryOf?: MidgardCekRevealedConstantMemory,
): MidgardCekBuiltinBudget => {
  if (
    tag < 0n ||
    tag > MIDGARD_CEK_MAX_BUILTIN_TAG ||
    tag > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("V1 builtin tag is outside Plutus V3");
  }
  return computeMidgardCekBuiltinBudget(
    Number(tag),
    midgardCekDirectBuiltinCostSizes(tag, arguments_, memoryOf),
  );
};

export const selectedControlResult = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekDirectValueWitness | null => {
  if (tag === 26n) {
    if (arguments_.length !== 3) {
      throw new Error("ifThenElse requires three arguments");
    }
    return directBoolean(arguments_[0]!) ? arguments_[1]! : arguments_[2]!;
  }
  if (tag === 27n) {
    if (arguments_.length !== 2) {
      throw new Error("chooseUnit requires two arguments");
    }
    const unit = decodedDirectConstant(arguments_[0]!);
    if (unit.type.kind !== "unit") {
      throw new Error("chooseUnit requires unit");
    }
    return arguments_[1]!;
  }
  if (tag === 28n) {
    if (arguments_.length !== 2) {
      throw new Error("trace requires two arguments");
    }
    const message = decodedDirectConstant(arguments_[0]!);
    if (message.type.kind !== "string") {
      throw new Error("trace requires a string");
    }
    return arguments_[1]!;
  }
  if (tag === 31n) {
    if (arguments_.length !== 3) {
      throw new Error("chooseList requires three arguments");
    }
    const source = decodedDirectConstant(arguments_[0]!);
    if (source.type.kind !== "list" || !(source.payload instanceof DataList)) {
      throw new Error("chooseList requires a list");
    }
    return source.payload.list.length === 0 ? arguments_[1]! : arguments_[2]!;
  }
  if (tag === 36n) {
    if (arguments_.length !== 6) {
      throw new Error("chooseData requires six arguments");
    }
    const source = decodedDirectConstant(arguments_[0]!);
    if (source.type.kind !== "data") {
      throw new Error("chooseData requires Data");
    }
    const selected =
      source.payload instanceof DataConstr
        ? 1
        : source.payload.constructor.name === "DataMap"
          ? 2
          : source.payload instanceof DataList
            ? 3
            : source.payload.constructor.name === "DataI"
              ? 4
              : source.payload instanceof DataB
                ? 5
                : -1;
    if (selected < 1) {
      throw new Error("chooseData received an unknown Data variant");
    }
    return arguments_[selected]!;
  }
  return null;
};
