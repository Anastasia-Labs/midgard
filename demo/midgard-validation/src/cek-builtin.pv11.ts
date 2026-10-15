import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";

import {
  computeMidgardCekPv11BuiltinBudget,
  midgardCekPv11BuiltinArgumentCount,
} from "./cek-builtin.pv11-cost.js";
import {
  cardanoBlsMultiScalarMul,
  cardanoExpModInteger,
} from "./cek-builtin.pv11-numeric.js";
import {
  cardanoInsertCoin,
  cardanoLookupCoin,
  cardanoScaleValue,
  cardanoUnionValue,
  cardanoValueContains,
  cardanoValueMaxDepth,
  type CardanoValue,
} from "./cek-builtin.pv11-value.js";
import {
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import {
  isMidgardCekCanonicalValueData,
  midgardCekValueEntryCount,
} from "./cek-constant.value.js";
import { type MidgardCekBuiltinBudget } from "./cek-cost.js";

export type MidgardCekPv11Constant = {
  readonly type: MidgardCekConstantType;
  readonly payload: Data;
};
export type MidgardCekPv11Evaluation =
  | {
      readonly kind: "success";
      readonly result: MidgardCekPv11Constant;
      readonly budget: MidgardCekBuiltinBudget;
    }
  | { readonly kind: "failure"; readonly budget: MidgardCekBuiltinBudget };

const integer = (argument: MidgardCekPv11Constant): bigint => {
  if (argument.type.kind !== "integer" || !(argument.payload instanceof DataI))
    throw new Error("PV 11 builtin requires an integer");
  return argument.payload.int;
};
const bytes = (argument: MidgardCekPv11Constant): Uint8Array => {
  if (argument.type.kind !== "bytes" || !(argument.payload instanceof DataB))
    throw new Error("PV 11 builtin requires bytes");
  return argument.payload.bytes;
};
const list = (
  argument: MidgardCekPv11Constant,
  kind: "list" | "array" = "list",
): Data[] => {
  if (argument.type.kind !== kind || !(argument.payload instanceof DataList))
    throw new Error(`PV 11 builtin requires ${kind}`);
  return argument.payload.list;
};
const value = (argument: MidgardCekPv11Constant): CardanoValue => {
  if (
    argument.type.kind !== "value" ||
    !isMidgardCekCanonicalValueData(argument.payload)
  )
    throw new Error("PV 11 builtin requires a canonical Value");
  return argument.payload as CardanoValue;
};
const intResult = (result: bigint): MidgardCekPv11Constant => ({
  type: { kind: "integer" },
  payload: new DataI(result),
});
const valueResult = (
  result: CardanoValue | "failure",
): MidgardCekPv11Constant | "failure" =>
  result === "failure" ? result : { type: { kind: "value" }, payload: result };
const INT64_MIN = -(1n << 63n);
const INT64_MAX = (1n << 63n) - 1n;

/** unValueData counts Data constructors, without scalar words or container overhead. */
const dataNodeCount = (payload: Data): bigint => {
  const pending = [payload];
  let total = 0n;
  while (pending.length > 0) {
    const current = pending.pop()!;
    total += 1n;
    if (current instanceof DataConstr) {
      for (const item of current.fields) pending.push(item);
    } else if (current instanceof DataList) {
      for (const item of current.list) pending.push(item);
    } else if (current instanceof DataMap) {
      for (const { fst, snd } of current.map) pending.push(fst, snd);
    }
  }
  return total;
};

export const midgardCekPv11BuiltinCostSizes = (
  tag: bigint,
  arguments_: readonly MidgardCekPv11Constant[],
): readonly bigint[] => {
  if (BigInt(arguments_.length) !== midgardCekPv11BuiltinArgumentCount(tag))
    throw new Error("invalid PV 11 builtin arity");
  const sizes = arguments_.map(({ type, payload }) =>
    midgardCekConstantMemorySize(type, payload),
  );
  if (tag === 88n) {
    const count = integer(arguments_[0]!);
    const magnitude = count < 0n ? -count : count;
    sizes[0] = magnitude > INT64_MAX ? INT64_MAX : magnitude;
    sizes[1] = BigInt(list(arguments_[1]!).length);
  }
  if (tag === 90n) sizes[0] = BigInt(list(arguments_[0]!).length);
  if (tag === 92n || tag === 93n) {
    sizes[0] = BigInt(list(arguments_[0]!).length);
    sizes[1] = BigInt(list(arguments_[1]!).length);
  }
  if (tag === 94n) sizes[3] = cardanoValueMaxDepth(value(arguments_[3]!));
  if (tag === 95n) sizes[2] = cardanoValueMaxDepth(value(arguments_[2]!));
  if (tag === 99n) sizes[0] = dataNodeCount(arguments_[0]!.payload);
  return sizes;
};

const evaluate = (
  tag: bigint,
  arguments_: readonly MidgardCekPv11Constant[],
): MidgardCekPv11Constant | "failure" => {
  const [first, second, third, fourth] = arguments_;
  switch (tag) {
    case 87n: {
      const result = cardanoExpModInteger(
        integer(first!),
        integer(second!),
        integer(third!),
      );
      return result === "failure" ? result : intResult(result);
    }
    case 88n: {
      const source = list(second!);
      const count = integer(first!);
      return {
        type: second!.type,
        payload: new DataList(
          count <= 0n
            ? source
            : source.slice(
                Number(
                  count > BigInt(source.length) ? BigInt(source.length) : count,
                ),
              ),
        ),
      };
    }
    case 89n:
      return intResult(BigInt(list(first!, "array").length));
    case 90n: {
      const source = list(first!);
      if (first!.type.kind !== "list")
        throw new Error("PV 11 listToArray requires list");
      return {
        type: { kind: "array", element: first!.type.element },
        payload: new DataList(source),
      };
    }
    case 91n: {
      const source = list(first!, "array");
      const index = integer(second!);
      if (index < 0n || index >= BigInt(source.length)) return "failure";
      if (first!.type.kind !== "array")
        throw new Error("PV 11 indexArray requires array");
      return { type: first!.type.element, payload: source[Number(index)]! };
    }
    case 92n:
    case 93n: {
      const group = tag === 92n ? "blsG1" : "blsG2";
      if (
        first!.type.kind !== "list" ||
        first!.type.element.kind !== "integer" ||
        second!.type.kind !== "list" ||
        second!.type.element.kind !== group
      )
        throw new Error("PV 11 MSM requires integer and point lists");
      const scalars = list(first!).map((item) =>
        integer({ type: { kind: "integer" }, payload: item }),
      );
      const points = list(second!).map((item) => {
        if (!(item instanceof DataB))
          throw new Error("PV 11 MSM point payload is not bytes");
        return item.bytes;
      });
      const result = cardanoBlsMultiScalarMul(group, scalars, points);
      return result === "failure"
        ? result
        : { type: { kind: group }, payload: new DataB(result) };
    }
    case 94n:
      return valueResult(
        cardanoInsertCoin(
          bytes(first!),
          bytes(second!),
          integer(third!),
          value(fourth!),
        ),
      );
    case 95n:
      return intResult(
        cardanoLookupCoin(bytes(first!), bytes(second!), value(third!)),
      );
    case 96n:
      return valueResult(cardanoUnionValue(value(first!), value(second!)));
    case 97n: {
      const result = cardanoValueContains(value(first!), value(second!));
      return result === "failure"
        ? result
        : {
            type: { kind: "boolean" },
            payload: new DataConstr(result ? 1 : 0, []),
          };
    }
    case 98n: {
      const source = value(first!);
      return midgardCekValueEntryCount(source) > 40_000n
        ? "failure"
        : { type: { kind: "data" }, payload: source };
    }
    case 99n:
      if (first!.type.kind !== "data")
        throw new Error("PV 11 unValueData requires Data");
      return isMidgardCekCanonicalValueData(first!.payload)
        ? { type: { kind: "value" }, payload: first!.payload }
        : "failure";
    case 100n:
      return valueResult(cardanoScaleValue(integer(first!), value(second!)));
    default:
      throw new Error("not a PV 11 builtin addition");
  }
};

/** Evaluates authenticated semantic constants, without a direct-proof payload cap. */
export const evaluateMidgardCekPv11Builtin = (
  tag: bigint,
  arguments_: readonly MidgardCekPv11Constant[],
): MidgardCekPv11Evaluation => {
  const sizes = midgardCekPv11BuiltinCostSizes(tag, arguments_);
  // indexArray unlifts its index as Int64, before the builtin charge.
  if (tag === 91n) {
    const index = integer(arguments_[1]!);
    if (index < INT64_MIN || index > INT64_MAX)
      return { kind: "failure", budget: { cpu: 0n, memory: 0n } };
  }
  const budget = computeMidgardCekPv11BuiltinBudget(tag, sizes);
  const result = evaluate(tag, arguments_);
  return result === "failure"
    ? { kind: "failure", budget }
    : { kind: "success", result, budget };
};
