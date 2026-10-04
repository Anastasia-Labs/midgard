/**
 * The node's builtin evaluation against the V1 CEK builtin golden channel:
 * every builtin tag, evaluated by the pinned Aiken evaluator with the ledger's
 * outcome recorded where the two differ. The node must reproduce every
 * expected value and every expected failure. Data results are compared
 * through their serialiseData bytes.
 */
import { readFileSync } from "node:fs";

import {
  hashMidgardCekBlsExpressionNode,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekBlsFinal,
  evaluateMidgardCekDirectBuiltin,
  type MidgardCekBlsExpressionWitness,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekPlutusData,
  hashMidgardCekConstantWitness,
  type MidgardCekConstantType,
  type MidgardCekConstantWitness,
  midgardCekConstantWitnessFromUplc,
} from "../src/cek-constant.js";

type FixtureType =
  | "int"
  | "bs"
  | "str"
  | "unit"
  | "bool"
  | "data"
  | "g1"
  | "g2"
  | "ml"
  | { readonly list: FixtureType }
  | { readonly pair: readonly [FixtureType, FixtureType] };

type FixtureData =
  | { readonly i: string }
  | { readonly b: string }
  | { readonly l: readonly FixtureData[] }
  | { readonly m: readonly (readonly [FixtureData, FixtureData])[] }
  | { readonly c: string; readonly f: readonly FixtureData[] };

type FixtureMl =
  | { readonly loop: readonly [string, string] }
  | { readonly mul: readonly [FixtureMl, FixtureMl] };

type FixtureArgument = { readonly ty: FixtureType; readonly v: unknown };

type FixtureOutcome =
  | { readonly kind: "error" }
  | {
      readonly kind: "value";
      readonly type: FixtureType;
      readonly value: unknown;
    };

type FixtureCase = {
  readonly tag: number;
  readonly note: string;
  readonly args: readonly FixtureArgument[];
  readonly expected: FixtureOutcome;
  readonly aikenEvaluator?: unknown;
  readonly reason?: string;
};

const fixture = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/cek-builtin-cardano-v1.generated.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as { readonly channel: string; readonly cases: readonly FixtureCase[] };

const fromHex = (value: string): Uint8Array =>
  Uint8Array.from(Buffer.from(value, "hex"));

const toData = (value: FixtureData): Data => {
  if ("i" in value) return new DataI(BigInt(value.i));
  if ("b" in value) return new DataB(fromHex(value.b));
  if ("l" in value) return new DataList(value.l.map(toData));
  if ("m" in value) {
    return new DataMap(
      value.m.map(([key, entry]) => new DataPair(toData(key), toData(entry))),
    );
  }
  return new DataConstr(BigInt(value.c), value.f.map(toData));
};

const uplcConstant = (ty: FixtureType, v: unknown): UPLCConst => {
  if (typeof ty === "object") {
    if ("list" in ty) {
      const items = (v as readonly unknown[]).map((item) =>
        uplcConstant(ty.list, item),
      );
      const element = uplcConstant(ty.list, sampleValue(ty.list)).type;
      return UPLCConst.listOf(element)(
        items.map((item) => item.value) as never,
      );
    }
    const [first, second] = v as readonly [unknown, unknown];
    const left = uplcConstant(ty.pair[0], first);
    const right = uplcConstant(ty.pair[1], second);
    return UPLCConst.pairOf(left.type, right.type)(left.value, right.value);
  }
  switch (ty) {
    case "int":
      return UPLCConst.int(BigInt(v as string));
    case "bs":
      return UPLCConst.byteString(fromHex(v as string));
    case "str":
      return UPLCConst.str(v as string);
    case "unit":
      return UPLCConst.unit;
    case "bool":
      return UPLCConst.bool(v as boolean);
    case "data":
      return UPLCConst.data(toData(v as FixtureData));
    case "g1":
    case "g2":
    case "ml":
      throw new Error(`no UPLC constant for ${ty}`);
  }
};

// A value of the type, used only to read the element type of an empty list.
const sampleValue = (ty: FixtureType): unknown => {
  if (typeof ty === "object") {
    return "list" in ty
      ? []
      : [sampleValue(ty.pair[0]), sampleValue(ty.pair[1])];
  }
  return {
    int: "0",
    bs: "",
    str: "",
    unit: null,
    bool: false,
    data: { i: "0" },
  }[ty as "int" | "bs" | "str" | "unit" | "bool" | "data"];
};

const blsWitness = (ty: "g1" | "g2", v: string): MidgardCekConstantWitness => ({
  typeCbor: Buffer.from(ty === "g1" ? "9f09ff" : "9f0aff", "hex"),
  payloadCbor: Buffer.from(encodeMidgardCekPlutusData(new DataB(fromHex(v)))),
});

const constantWitness = (
  argument: FixtureArgument,
): MidgardCekConstantWitness =>
  argument.ty === "g1" || argument.ty === "g2"
    ? blsWitness(argument.ty, argument.v as string)
    : midgardCekConstantWitnessFromUplc(uplcConstant(argument.ty, argument.v));

const blsExpression = (value: FixtureMl): MidgardCekBlsExpressionWitness =>
  "loop" in value
    ? {
        kind: "millerLoop",
        g1: blsWitness("g1", value.loop[0]),
        g2: blsWitness("g2", value.loop[1]),
      }
    : {
        kind: "multiply",
        left: blsExpression(value.mul[0]),
        right: blsExpression(value.mul[1]),
      };

const blsRoot = (expression: MidgardCekBlsExpressionWitness): Uint8Array =>
  expression.kind === "millerLoop"
    ? hashMidgardCekBlsExpressionNode({
        kind: "millerLoop",
        g1Value: hashMidgardCekConstantWitness(expression.g1),
        g2Value: hashMidgardCekConstantWitness(expression.g2),
      })
    : hashMidgardCekBlsExpressionNode({
        kind: "multiply",
        left: blsRoot(expression.left),
        right: blsRoot(expression.right),
      });

const fixtureType = (type: MidgardCekConstantType): FixtureType => {
  switch (type.kind) {
    case "integer":
      return "int";
    case "bytes":
      return "bs";
    case "string":
      return "str";
    case "unit":
      return "unit";
    case "boolean":
      return "bool";
    case "data":
      return "data";
    case "blsG1":
      return "g1";
    case "blsG2":
      return "g2";
    case "list":
      return { list: fixtureType(type.element) };
    case "pair":
      return { pair: [fixtureType(type.first), fixtureType(type.second)] };
    case "blsMillerLoopResult":
      throw new Error("a Miller loop result is never a builtin's value");
  }
};

const fixtureValue = (type: MidgardCekConstantType, payload: Data): unknown => {
  switch (type.kind) {
    case "integer":
      return (payload as DataI).int.toString(10);
    case "bytes":
    case "blsG1":
    case "blsG2":
      return Buffer.from((payload as DataB).bytes).toString("hex");
    case "string":
      return new TextDecoder("utf-8", { fatal: true }).decode(
        (payload as DataB).bytes,
      );
    case "unit":
      return null;
    case "boolean":
      return (payload as DataConstr).constr === 1n;
    case "data":
      return {
        cbor: Buffer.from(encodeMidgardCekPlutusData(payload)).toString("hex"),
      };
    case "list":
      return (payload as DataList).list.map((item) =>
        fixtureValue(type.element, item),
      );
    case "pair": {
      const [first, second] = (payload as DataConstr).fields;
      return [
        fixtureValue(type.first, first!),
        fixtureValue(type.second, second!),
      ];
    }
    case "blsMillerLoopResult":
      throw new Error("a Miller loop result is never a builtin's value");
  }
};

const outcomeOf = (result: MidgardCekDirectValueWitness): FixtureOutcome => {
  if (result.kind !== "constant") {
    throw new Error(`builtin returned a ${result.kind} value`);
  }
  const decoded = decodeMidgardCekConstantWitness(result.witness);
  return {
    kind: "value",
    type: fixtureType(decoded.type),
    value: fixtureValue(decoded.type, decoded.payload),
  };
};

const nodeOutcome = (entry: FixtureCase): FixtureOutcome => {
  if (entry.tag === 70) {
    const [left, right] = entry.args.map((argument) =>
      blsExpression(argument.v as FixtureMl),
    ) as [MidgardCekBlsExpressionWitness, MidgardCekBlsExpressionWitness];
    return outcomeOf(
      evaluateMidgardCekBlsFinal(blsRoot(left), blsRoot(right), left, right)
        .result,
    );
  }
  const evaluated = evaluateMidgardCekDirectBuiltin(
    BigInt(entry.tag),
    entry.args.map((argument) => ({
      kind: "constant",
      witness: constantWitness(argument),
    })),
  );
  return evaluated.kind === "failure"
    ? { kind: "error" }
    : outcomeOf(evaluated.result);
};

describe("V1 CEK builtin golden channel", () => {
  it("covers every builtin tag", () => {
    expect(fixture.channel).toBe("cek-builtin-cardano-v1");
    const covered = new Set(fixture.cases.map((entry) => entry.tag));
    // The Miller loop and its product are reached inside finalVerify.
    const reachedThroughFinalVerify = fixture.cases
      .filter((entry) => entry.tag === 70)
      .flatMap((entry) => JSON.stringify(entry.args))
      .join("");
    if (reachedThroughFinalVerify.includes('"loop"')) covered.add(68);
    if (reachedThroughFinalVerify.includes('"mul"')) covered.add(69);
    expect([...covered].sort((a, b) => a - b)).toEqual(
      Array.from(
        { length: Number(MIDGARD_CEK_MAX_BUILTIN_TAG) + 1 },
        (_, tag) => tag,
      ),
    );
  });

  it.each(
    fixture.cases.map((entry) => [entry.tag, entry.note, entry] as const),
  )("tag %i %s", (_tag, _note, entry) => {
    expect(nodeOutcome(entry)).toEqual(entry.expected);
  });
});
