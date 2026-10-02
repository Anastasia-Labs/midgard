import {
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
} from "@al-ft/midgard-core";

import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  hashMidgardCekConstantWitness,
  hashMidgardCekSemanticConstantWitness,
  type MidgardCekConstantType,
  type MidgardCekConstantWitness,
  type MidgardCekSemanticConstantWitness,
} from "./cek-constant.js";

export type Bytes = Uint8Array;

export type MidgardCekRuntimeValueWitness =
  | {
      readonly kind: "constant";
      readonly witness: MidgardCekConstantWitness;
    }
  | {
      readonly kind: "semanticConstant";
      readonly witness: MidgardCekSemanticConstantWitness;
    }
  | {
      readonly kind: "lambda";
      readonly body: Bytes;
      readonly environment: Bytes;
    }
  | {
      readonly kind: "delay";
      readonly body: Bytes;
      readonly environment: Bytes;
    }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
    }
  | {
      readonly kind: "builtin";
      readonly tag: bigint;
      readonly forcesRemaining: bigint;
      readonly argumentsCount: bigint;
      readonly argumentsRoot: Bytes;
    }
  | {
      readonly kind: "blsMillerLoop";
      readonly expressionRoot: Bytes;
    };

export type MidgardCekConstantValueWitness = Extract<
  MidgardCekRuntimeValueWitness,
  { readonly kind: "constant" | "semanticConstant" }
>;

type RuntimeValueKind =
  | "any"
  | "integer"
  | "bytes"
  | "string"
  | "unit"
  | "boolean"
  | "list"
  | "pair"
  | "data"
  | "blsG1"
  | "blsG2"
  | "blsMillerLoop"
  | "listData"
  | "listDataPair"
  | "listInteger";

export const sameBytes = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

export const hashMidgardCekRuntimeValueWitness = (
  value: MidgardCekRuntimeValueWitness,
): Bytes => {
  switch (value.kind) {
    case "constant":
      return hashMidgardCekConstantWitness(value.witness);
    case "semanticConstant":
      return hashMidgardCekSemanticConstantWitness(value.witness);
    case "lambda":
      return hashMidgardCekValueNode({
        kind: "lambda",
        body: value.body,
        environment: value.environment,
      });
    case "delay":
      return hashMidgardCekValueNode({
        kind: "delay",
        body: value.body,
        environment: value.environment,
      });
    case "constr":
      return hashMidgardCekValueNode({
        kind: "constr",
        tag: value.tag,
        valuesCount: value.valuesCount,
        valuesRoot: value.valuesRoot,
      });
    case "builtin":
      return hashMidgardCekValueNode({
        kind: "builtin",
        tag: value.tag,
        forcesRemaining: value.forcesRemaining,
        argumentsCount: value.argumentsCount,
        argumentsRoot: value.argumentsRoot,
      });
    case "blsMillerLoop":
      return hashMidgardCekValueNode({
        kind: "blsMillerLoop",
        expressionRoot: value.expressionRoot,
      });
  }
};

export const hashMidgardCekRuntimeArguments = (
  arguments_: readonly MidgardCekRuntimeValueWitness[],
): { readonly root: Bytes; readonly count: bigint } => {
  let root: Bytes = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT;
  let count = 0n;
  for (const argument of arguments_) {
    count += 1n;
    root = hashMidgardCekSequenceNode({
      head: hashMidgardCekRuntimeValueWitness(argument),
      tail: root,
      length: count,
    });
  }
  return Object.freeze({ root, count });
};

const sameConstantType = (
  left: MidgardCekConstantType,
  right: MidgardCekConstantType,
): boolean => {
  if (left.kind !== right.kind) return false;
  if (left.kind === "list" && right.kind === "list") {
    return sameConstantType(left.element, right.element);
  }
  if (left.kind === "pair" && right.kind === "pair") {
    return (
      sameConstantType(left.first, right.first) &&
      sameConstantType(left.second, right.second)
    );
  }
  return true;
};

const constantType = (
  value: MidgardCekRuntimeValueWitness,
): MidgardCekConstantType | null =>
  value.kind === "constant"
    ? decodeMidgardCekConstantWitness(value.witness).type
    : value.kind === "semanticConstant"
      ? decodeMidgardCekConstantTypeCbor(value.witness.typeCbor)
      : null;

export const matchesKind = (
  value: MidgardCekRuntimeValueWitness,
  kind: RuntimeValueKind,
): boolean => {
  if (kind === "any") return true;
  if (kind === "blsMillerLoop") {
    return value.kind === "blsMillerLoop";
  }
  const type = constantType(value);
  if (type === null) return false;
  switch (kind) {
    case "integer":
    case "bytes":
    case "string":
    case "unit":
    case "boolean":
    case "data":
    case "blsG1":
    case "blsG2":
      return type.kind === kind;
    case "list":
      return type.kind === "list";
    case "pair":
      return type.kind === "pair";
    case "listData":
      return type.kind === "list" && type.element.kind === "data";
    case "listInteger":
      return type.kind === "list" && type.element.kind === "integer";
    case "listDataPair":
      return (
        type.kind === "list" &&
        type.element.kind === "pair" &&
        type.element.first.kind === "data" &&
        type.element.second.kind === "data"
      );
  }
};

export const argumentKinds = (tag: number): readonly RuntimeValueKind[] => {
  if (!Number.isInteger(tag) || tag < 0 || tag > 86) {
    throw new Error("V1 builtin tag is outside Plutus V3");
  }
  if (tag <= 9) return ["integer", "integer"];
  if (tag === 10) return ["bytes", "bytes"];
  if (tag === 11) return ["integer", "bytes"];
  if (tag === 12) return ["integer", "integer", "bytes"];
  if (tag === 13) return ["bytes"];
  if (tag === 14) return ["bytes", "integer"];
  if (tag <= 17) return ["bytes", "bytes"];
  if (tag <= 20) return ["bytes"];
  if (tag === 21) return ["bytes", "bytes", "bytes"];
  if (tag <= 23) return ["string", "string"];
  if (tag === 24) return ["string"];
  if (tag === 25) return ["bytes"];
  if (tag === 26) return ["boolean", "any", "any"];
  if (tag === 27) return ["unit", "any"];
  if (tag === 28) return ["string", "any"];
  if (tag <= 30) return ["pair"];
  if (tag === 31) return ["list", "any", "any"];
  if (tag === 32) return ["any", "list"];
  if (tag <= 35) return ["list"];
  if (tag === 36) return ["data", "any", "any", "any", "any", "any"];
  if (tag === 37) return ["integer", "listData"];
  if (tag === 38) return ["listDataPair"];
  if (tag === 39) return ["listData"];
  if (tag === 40) return ["integer"];
  if (tag === 41) return ["bytes"];
  if (tag <= 46) return ["data"];
  if (tag <= 48) return ["data", "data"];
  if (tag <= 50) return ["unit"];
  if (tag === 51) return ["data"];
  if (tag <= 53) return ["bytes", "bytes", "bytes"];
  if (tag === 54 || tag === 57) return ["blsG1", "blsG1"];
  if (tag === 55 || tag === 59) return ["blsG1"];
  if (tag === 56) return ["integer", "blsG1"];
  if (tag === 58) return ["bytes", "bytes"];
  if (tag === 60) return ["bytes"];
  if (tag === 61 || tag === 64) return ["blsG2", "blsG2"];
  if (tag === 62 || tag === 66) return ["blsG2"];
  if (tag === 63) return ["integer", "blsG2"];
  if (tag === 65) return ["bytes", "bytes"];
  if (tag === 67) return ["bytes"];
  if (tag === 68) return ["blsG1", "blsG2"];
  if (tag === 69 || tag === 70) {
    return ["blsMillerLoop", "blsMillerLoop"];
  }
  if (tag <= 72) return ["bytes"];
  if (tag === 73) return ["boolean", "integer", "integer"];
  if (tag === 74) return ["boolean", "bytes"];
  if (tag <= 77) return ["boolean", "bytes", "bytes"];
  if (tag === 78) return ["bytes"];
  if (tag === 79) return ["bytes", "integer"];
  if (tag === 80) return ["bytes", "listInteger", "boolean"];
  if (tag === 81) return ["integer", "integer"];
  if (tag <= 83) return ["bytes", "integer"];
  return ["bytes"];
};

export const mkConsIsWellTyped = (
  arguments_: readonly MidgardCekRuntimeValueWitness[],
): boolean => {
  if (arguments_.length !== 2) return false;
  const elementType = constantType(arguments_[0]);
  const listType = constantType(arguments_[1]);
  return (
    elementType !== null &&
    listType?.kind === "list" &&
    sameConstantType(elementType, listType.element)
  );
};

export type MidgardCekDirectValueWitness =
  | {
      readonly kind: "constant";
      readonly witness: MidgardCekConstantWitness;
    }
  | {
      readonly kind: "semanticConstant";
      readonly witness: MidgardCekSemanticConstantWitness;
    }
  | { readonly kind: "opaque"; readonly root: Bytes }
  | { readonly kind: "blsMillerLoop"; readonly expressionRoot: Bytes };

// An opaque presentation carries no type, so it fits only a polymorphic
// position; a constant or Miller-loop presentation fits as its runtime twin.
const directMatchesKind = (
  value: MidgardCekDirectValueWitness,
  kind: RuntimeValueKind,
): boolean =>
  value.kind === "opaque" ? kind === "any" : matchesKind(value, kind);

export const directArgumentsMatchKinds = (
  tag: number,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const kinds = argumentKinds(tag);
  return (
    arguments_.length === kinds.length &&
    arguments_.every((argument, index) =>
      directMatchesKind(argument, kinds[index]),
    )
  );
};

export const directWitnessPayloadBytes = (
  values: readonly (
    | MidgardCekRuntimeValueWitness
    | MidgardCekDirectValueWitness
  )[],
): bigint =>
  values.reduce(
    (total, value) =>
      total +
      (value.kind === "constant"
        ? BigInt(value.witness.payloadCbor.length)
        : 0n),
    0n,
  );
