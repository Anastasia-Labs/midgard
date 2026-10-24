/**
 * Constants across the pinned reference evaluator's boundary: Midgard's UPLC
 * constants in, the evaluator's `CEKConst` values out, and back. The constant
 * type tags agree except array and Value, which the evaluator numbers the
 * other way round and Midgard's constants never hold. Data is copied between
 * Midgard's classes and the evaluator's with an explicit stack.
 */
import {
  type Data as ReferenceData,
  DataB as ReferenceDataB,
  DataConstr as ReferenceDataConstr,
  DataI as ReferenceDataI,
  DataList as ReferenceDataList,
  DataMap as ReferenceDataMap,
  DataPair as ReferenceDataPair,
} from "@harmoniclabs/plutus-data";
import { CEKConst } from "@harmoniclabs/plutus-machine";
import {
  type ConstType as ReferenceConstType,
  UPLCConst as ReferenceUPLCConst,
} from "@harmoniclabs/uplc";

import {
  constPairTypeArguments,
  type ConstType,
  ConstTyTag,
  type ConstValue,
  type Pair,
  UPLCConst,
} from "./uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "./uplc-data.js";

const representable = (type: readonly number[]): void => {
  if (type.some((tag) => tag === ConstTyTag.array || tag === ConstTyTag.value))
    throw new Error("V1 constant has an unknown type tag");
};

type Copy<Target> = {
  readonly children: (node: object) => readonly object[];
  readonly build: (node: object, children: Target[]) => Target;
};

/** Copies a Data tree bottom-up with an explicit stack. */
const copyData = <Target>(root: object, copy: Copy<Target>): Target => {
  const work: { node: object; children: readonly object[] | null }[] = [
    { node: root, children: null },
  ];
  const built: Target[] = [];
  while (work.length > 0) {
    const next = work.pop()!;
    if (next.children === null) {
      const children = copy.children(next.node);
      work.push({ node: next.node, children });
      for (let index = children.length - 1; index >= 0; index -= 1)
        work.push({ node: children[index]!, children: null });
    } else {
      const children = built.splice(built.length - next.children.length);
      built.push(copy.build(next.node, children));
    }
  }
  return built[0]!;
};

const entries = (map: readonly { fst: object; snd: object }[]): object[] =>
  map.flatMap((entry) => [entry.fst, entry.snd]);

const pairs = <Value>(values: Value[]): { fst: Value; snd: Value }[] => {
  const result: { fst: Value; snd: Value }[] = [];
  for (let index = 0; index < values.length; index += 2)
    result.push({ fst: values[index]!, snd: values[index + 1]! });
  return result;
};

const toReference: Copy<ReferenceData> = {
  children: (node) =>
    node instanceof DataConstr
      ? node.fields
      : node instanceof DataList
        ? node.list
        : node instanceof DataMap
          ? entries(node.map)
          : [],
  build: (node, children) => {
    if (node instanceof DataConstr)
      return new ReferenceDataConstr(node.constr, children);
    if (node instanceof DataList) return new ReferenceDataList(children);
    if (node instanceof DataMap)
      return new ReferenceDataMap(
        pairs(children).map(({ fst, snd }) => new ReferenceDataPair(fst, snd)),
      );
    if (node instanceof DataI) return new ReferenceDataI(node.int);
    if (node instanceof DataB) return new ReferenceDataB(node.bytes);
    throw new Error("V1 Data constant contains invalid Data");
  },
};

const fromReference: Copy<Data> = {
  children: (node) =>
    node instanceof ReferenceDataConstr
      ? node.fields
      : node instanceof ReferenceDataList
        ? node.list
        : node instanceof ReferenceDataMap
          ? entries(node.map)
          : [],
  build: (node, children) => {
    if (node instanceof ReferenceDataConstr)
      return new DataConstr(node.constr, children);
    if (node instanceof ReferenceDataList) return new DataList(children);
    if (node instanceof ReferenceDataMap) return new DataMap(pairs(children));
    if (node instanceof ReferenceDataI) return new DataI(node.int);
    if (node instanceof ReferenceDataB) return new DataB(node.bytes);
    throw new Error("V1 Data constant contains invalid Data");
  },
};

/** Copies the Data inside a value of `type`; recursion follows the type. */
const copyValue = <Target>(
  type: readonly number[],
  value: ConstValue,
  copy: Copy<Target>,
): ConstValue => {
  switch (type[0]) {
    case ConstTyTag.data:
      return copyData(value as object, copy) as ConstValue;
    case ConstTyTag.list:
      return (value as readonly ConstValue[]).map((item) =>
        copyValue(type.slice(1), item, copy),
      );
    case ConstTyTag.pair: {
      const [first, second] = constPairTypeArguments(type as ConstType);
      const pair = value as Pair<ConstValue, ConstValue>;
      return {
        fst: copyValue(first, pair.fst, copy),
        snd: copyValue(second, pair.snd, copy),
      };
    }
    default:
      return value;
  }
};

/** The reference evaluator's value of a constant. */
export const referenceValueOf = (constant: {
  readonly type: readonly number[];
  readonly value: ConstValue;
}): CEKConst => {
  representable(constant.type);
  return CEKConst.fromUplc(
    new ReferenceUPLCConst(
      [...constant.type] as ReferenceConstType,
      copyValue(constant.type, constant.value, toReference) as never,
    ),
  );
};

/** The constant a reference evaluator value stands for. */
export const constantOfReferenceValue = (reference: CEKConst): UPLCConst => {
  representable(reference.type);
  return new UPLCConst(
    reference.type as readonly number[] as ConstType,
    copyValue(reference.type, reference.value as ConstValue, fromReference),
  );
};
