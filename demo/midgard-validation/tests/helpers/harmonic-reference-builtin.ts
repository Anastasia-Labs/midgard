/**
 * HarmonicLabs' builtin evaluator (`BnCEK`), which evaluated Midgard's
 * builtins before the pinned evaluator's `eval_builtin_raw`, kept for test
 * oracles only. Values cross between Midgard's UPLC constants and the
 * evaluator's `CEKConst`: the constant type tags agree except array and
 * Value, which the evaluator numbers the other way round and Midgard's
 * constants never hold. Data is copied with an explicit stack. Midgard holds
 * a BLS point as its compressed bytes; the evaluator holds the point.
 */
import {
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH,
} from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import {
  type Data as ReferenceData,
  DataB as ReferenceDataB,
  DataConstr as ReferenceDataConstr,
  DataI as ReferenceDataI,
  DataList as ReferenceDataList,
  DataMap as ReferenceDataMap,
  DataPair as ReferenceDataPair,
} from "@harmoniclabs/plutus-data";
import {
  BnCEK,
  CEKConst,
  CEKError,
  costModelV3ToBuiltinCosts,
  ExBudget,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";
import {
  type ConstType as ReferenceConstType,
  UPLCConst as ReferenceUPLCConst,
} from "@harmoniclabs/uplc";

import {
  harmonicBuiltinTag,
  MIDGARD_CEK_CARDANO_BLS_TAGS,
} from "../../src/cek-builtin.cardano-tags.js";
import {
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
  midgardCekConstantWitnessToUplc,
} from "../../src/cek-constant.js";
import {
  constPairTypeArguments,
  type ConstType,
  ConstTyTag,
  type ConstValue,
  type Pair,
  UPLCConst,
} from "../../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "../../src/uplc-data.js";

if (
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length !==
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH
) {
  throw new Error("V1 pinned Plutus V3 cost model must have 350 entries");
}

/** HarmonicLabs' Plutus V3 costing over the pinned cost model; unread. */
const REFERENCE_BUILTIN_COSTS = costModelV3ToBuiltinCosts(
  toCostModelV3([
    ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ] as unknown as Parameters<typeof toCostModelV3>[0]),
);

/** Runs Cardano's builtin `tag` (flat numbering) in HarmonicLabs' evaluator. */
export const runHarmonicReferenceBuiltin = (
  tag: number,
  arguments_: readonly CEKConst[],
): CEKConst | CEKError => {
  const builtinTag = harmonicBuiltinTag(tag);
  if (PartialBuiltin.getNRequiredArgsFor(builtinTag) !== arguments_.length) {
    throw new Error("V1 builtin argument count is incomplete");
  }
  const result = new BnCEK(
    REFERENCE_BUILTIN_COSTS,
    new ExBudget({ cpu: 0, mem: 0 }),
    [],
  ).eval(builtinTag, arguments_);
  if (!(result instanceof CEKConst) && !(result instanceof CEKError)) {
    throw new Error("V1 builtin did not saturate to a constant");
  }
  return result;
};

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

const BLS_TAGS: ReadonlySet<number> = new Set([
  ConstTyTag.bls12_381_G1_element,
  ConstTyTag.bls12_381_G2_element,
]);

/** A point between compressed bytes (Midgard) and the evaluator's point. */
const point = (tag: number, value: unknown, toEvaluator: boolean): unknown => {
  const g1 = tag === ConstTyTag.bls12_381_G1_element;
  const bls = MIDGARD_CEK_CARDANO_BLS_TAGS;
  const result = toEvaluator
    ? runHarmonicReferenceBuiltin(g1 ? bls.g1Uncompress : bls.g2Uncompress, [
        CEKConst.byteString(Uint8Array.from(value as Uint8Array)),
      ])
    : runHarmonicReferenceBuiltin(g1 ? bls.g1Compress : bls.g2Compress, [
        new CEKConst([tag] as ReferenceConstType, value as never),
      ]);
  if (result instanceof CEKError)
    throw new Error(`V1 BLS value does not convert: ${result.msg ?? ""}`);
  return result.value;
};

/** Copies the Data and points inside a value of `type`, following the type. */
const copyValue = <Target>(
  type: readonly number[],
  value: ConstValue,
  copy: Copy<Target>,
  toEvaluator: boolean,
): ConstValue => {
  switch (type[0]) {
    case ConstTyTag.data:
      return copyData(value as object, copy) as ConstValue;
    case ConstTyTag.list:
      return (value as readonly ConstValue[]).map((item) =>
        copyValue(type.slice(1), item, copy, toEvaluator),
      );
    case ConstTyTag.pair: {
      const [first, second] = constPairTypeArguments(type as ConstType);
      const pair = value as Pair<ConstValue, ConstValue>;
      return {
        fst: copyValue(first, pair.fst, copy, toEvaluator),
        snd: copyValue(second, pair.snd, copy, toEvaluator),
      };
    }
    default:
      return BLS_TAGS.has(type[0]!)
        ? (point(type[0]!, value, toEvaluator) as ConstValue)
        : value;
  }
};

/** HarmonicLabs' value of a Midgard constant (BLS points compressed). */
export const referenceValueOf = (constant: {
  readonly type: readonly number[];
  readonly value: ConstValue;
}): CEKConst => {
  representable(constant.type);
  return CEKConst.fromUplc(
    new ReferenceUPLCConst(
      [...constant.type] as ReferenceConstType,
      copyValue(constant.type, constant.value, toReference, true) as never,
    ),
  );
};

/** The Midgard constant a HarmonicLabs value stands for. */
export const constantOfReferenceValue = (reference: CEKConst): UPLCConst => {
  representable(reference.type);
  return new UPLCConst(
    reference.type as readonly number[] as ConstType,
    copyValue(
      reference.type,
      reference.value as ConstValue,
      fromReference,
      false,
    ),
  );
};

/** HarmonicLabs' value of a direct constant witness. */
export const referenceValueOfWitness = (
  witness: MidgardCekConstantWitness,
): CEKConst => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  if (decoded.type.kind === "blsG1" || decoded.type.kind === "blsG2") {
    if (!(decoded.payload instanceof DataB))
      throw new Error("V1 BLS payload is not bytes");
    return referenceValueOf({
      type: [
        decoded.type.kind === "blsG1"
          ? ConstTyTag.bls12_381_G1_element
          : ConstTyTag.bls12_381_G2_element,
      ],
      value: decoded.payload.bytes,
    });
  }
  return referenceValueOf(midgardCekConstantWitnessToUplc(witness));
};
