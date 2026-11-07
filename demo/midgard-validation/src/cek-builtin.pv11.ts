/**
 * The protocol version 11 builtins (tags 87 to 100) on direct constants:
 * the sizes Cardano costs each one on, and the list and array builtins
 * (88 to 91), which act on the committed payload as it stands. The other
 * PV 11 builtins compute through the pinned evaluator.
 */
import { type MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekConstantTypeCbor,
  encodeMidgardCekPlutusData,
  type MidgardCekConstantType,
  type MidgardCekConstantWitness,
  midgardCekIntegerMemorySize,
  midgardCekValueEntryCount,
} from "./cek-constant.js";
import {
  type Data,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "./uplc-data.js";

export const MIDGARD_CEK_FIRST_PV11_BUILTIN_TAG = 87n;
export const MIDGARD_CEK_LAST_PV11_BUILTIN_TAG = 100n;

export const isMidgardCekPv11BuiltinTag = (tag: bigint): boolean =>
  tag >= MIDGARD_CEK_FIRST_PV11_BUILTIN_TAG &&
  tag <= MIDGARD_CEK_LAST_PV11_BUILTIN_TAG;

const MAX_SIGNED_64 = 9_223_372_036_854_775_807n;

const decoded = (
  value: MidgardCekDirectValueWitness,
): { readonly type: MidgardCekConstantType; readonly payload: Data } => {
  if (value.kind !== "constant") {
    throw new Error("V1 PV 11 builtin requires a revealed constant");
  }
  return decodeMidgardCekConstantWitness(value.witness);
};

const integerOf = (value: MidgardCekDirectValueWitness): bigint => {
  const { type, payload } = decoded(value);
  if (type.kind !== "integer" || !(payload instanceof DataI)) {
    throw new Error("V1 PV 11 builtin requires an integer");
  }
  return payload.int;
};

const sequenceOf = (
  value: MidgardCekDirectValueWitness,
  kind: "list" | "array",
): { readonly element: MidgardCekConstantType; readonly items: Data[] } => {
  const { type, payload } = decoded(value);
  if (type.kind !== kind || !(payload instanceof DataList)) {
    throw new Error(`V1 PV 11 builtin requires a ${kind}`);
  }
  return { element: type.element, items: payload.list };
};

const valueOf = (value: MidgardCekDirectValueWitness): DataMap<Data, Data> => {
  const { type, payload } = decoded(value);
  if (type.kind !== "value" || !(payload instanceof DataMap)) {
    throw new Error("V1 PV 11 builtin requires a Value");
  }
  return payload as DataMap<Data, Data>;
};

/** Bits needed for `size`: 0 for 0, else floor(log2 size) + 1. */
const bitLength = (size: number): bigint =>
  size === 0 ? 0n : BigInt(size.toString(2).length);

/**
 * Cardano's `ValueMaxDepth`: the bit length of the number of currencies
 * plus that of the largest token map.
 */
export const midgardCekValueMaxDepth = (value: DataMap<Data, Data>): bigint => {
  let largest = 0;
  for (const { snd: tokens } of value.map) {
    if (!(tokens instanceof DataMap)) {
      throw new Error("V1 value token map is not DataMap");
    }
    largest = Math.max(largest, tokens.map.length);
  }
  return bitLength(value.map.length) + bitLength(largest);
};

/**
 * Cardano's Data node count: one for every constructor, map, list, integer
 * and byte string, counting both sides of each map entry.
 */
export const midgardCekDataNodeCount = (data: Data): bigint => {
  let count = 0n;
  const stack: Data[] = [data];
  for (let next = stack.pop(); next !== undefined; next = stack.pop()) {
    count += 1n;
    if (next instanceof DataList) stack.push(...next.list);
    else if (next instanceof DataMap) {
      for (const { fst, snd } of (next as DataMap<Data, Data>).map) {
        stack.push(fst, snd);
      }
    } else if (next instanceof DataConstr) stack.push(...next.fields);
  }
  return count;
};

/** A count costed by its absolute value, saturated to signed 64 bits. */
const literalSize = (literal: bigint): bigint => {
  const magnitude = literal < 0n ? -literal : literal;
  return magnitude > MAX_SIGNED_64 ? MAX_SIGNED_64 : magnitude;
};

/**
 * The sizes Cardano's cost functions take for PV 11 builtin `tag`. A size
 * that no cost function of the builtin reads is 0.
 */
export const midgardCekPv11BuiltinCostSizes = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): readonly bigint[] => {
  const arg = (index: number): MidgardCekDirectValueWitness => {
    const value = arguments_[index];
    if (value === undefined) throw new Error("V1 PV 11 builtin arity");
    return value;
  };
  switch (tag) {
    case 87n:
      return [0, 1, 2].map((index) =>
        midgardCekIntegerMemorySize(integerOf(arg(index))),
      );
    case 88n:
      sequenceOf(arg(1), "list");
      return [literalSize(integerOf(arg(0))), 0n];
    case 89n:
      sequenceOf(arg(0), "array");
      return [0n];
    case 90n:
      return [BigInt(sequenceOf(arg(0), "list").items.length)];
    case 91n:
      sequenceOf(arg(0), "array");
      integerOf(arg(1));
      return [0n, 0n];
    case 92n:
    case 93n:
      sequenceOf(arg(1), "list");
      return [BigInt(sequenceOf(arg(0), "list").items.length), 0n];
    case 94n:
      return [0n, 0n, 0n, midgardCekValueMaxDepth(valueOf(arg(3)))];
    case 95n:
      return [0n, 0n, midgardCekValueMaxDepth(valueOf(arg(2)))];
    case 96n:
    case 97n:
      return [
        midgardCekValueEntryCount(valueOf(arg(0))),
        midgardCekValueEntryCount(valueOf(arg(1))),
      ];
    case 98n:
      return [midgardCekValueEntryCount(valueOf(arg(0)))];
    case 99n: {
      const { type, payload } = decoded(arg(0));
      if (type.kind !== "data") {
        throw new Error("V1 PV 11 builtin requires Data");
      }
      return [midgardCekDataNodeCount(payload)];
    }
    case 100n:
      integerOf(arg(0));
      return [0n, midgardCekValueEntryCount(valueOf(arg(1)))];
    default:
      throw new Error("V1 builtin tag is not a PV 11 addition");
  }
};

const constantWitness = (
  type: MidgardCekConstantType,
  payload: Data,
): MidgardCekDirectValueWitness => {
  const witness: MidgardCekConstantWitness = Object.freeze({
    typeCbor: encodeMidgardCekConstantTypeCbor(type),
    payloadCbor: encodeMidgardCekPlutusData(payload),
  });
  decodeMidgardCekConstantWitness(witness);
  return Object.freeze({ kind: "constant" as const, witness });
};

/**
 * dropList, lengthOfArray, listToArray and indexArray on their committed
 * payloads: the result, "failure" for an index outside the array, or null
 * for any other tag. An array's payload is its elements' list, so the
 * builtins keep each element's payload as it is.
 */
export const evaluateMidgardCekPv11SequenceBuiltin = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekDirectValueWitness | "failure" | null => {
  if (tag < 88n || tag > 91n) return null;
  if (BigInt(arguments_.length) !== (tag === 88n || tag === 91n ? 2n : 1n)) {
    throw new Error("V1 PV 11 builtin arity");
  }
  if (tag === 88n) {
    const count = integerOf(arguments_[0]!);
    const { element, items } = sequenceOf(arguments_[1]!, "list");
    const dropped =
      count <= 0n
        ? items
        : count >= BigInt(items.length)
          ? []
          : items.slice(Number(count));
    return constantWitness({ kind: "list", element }, new DataList(dropped));
  }
  if (tag === 89n) {
    const { items } = sequenceOf(arguments_[0]!, "array");
    return constantWitness({ kind: "integer" }, new DataI(items.length));
  }
  if (tag === 90n) {
    const { element, items } = sequenceOf(arguments_[0]!, "list");
    return constantWitness({ kind: "array", element }, new DataList(items));
  }
  const { element, items } = sequenceOf(arguments_[0]!, "array");
  const index = integerOf(arguments_[1]!);
  if (index < 0n || index >= BigInt(items.length)) return "failure";
  return constantWitness(element, items[Number(index)]!);
};

const MIN_SIGNED_64 = -MAX_SIGNED_64 - 1n;

/**
 * Whether a failing PV 11 builtin was charged: every one is, except
 * indexArray on an index outside the signed 64-bit range, which Cardano
 * refuses to unlift before costing it.
 */
export const midgardCekPv11FailureIsCharged = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  if (!isMidgardCekPv11BuiltinTag(tag)) return false;
  if (tag !== 91n) return true;
  const index = integerOf(arguments_[1]!);
  return index >= MIN_SIGNED_64 && index <= MAX_SIGNED_64;
};
