/**
 * The builtin evaluator swap, checked: HarmonicLabs' `BnCEK`, which evaluated
 * Midgard's builtins before, against the pinned evaluator's
 * `eval_builtin_raw`, on every builtin application of the V1 builtin golden
 * channel (`cek-builtin-cardano-v1.generated.json`, tags 68 to 70 aside) and,
 * when `MIDGARD_BUILTIN_APPLICATIONS` names a recording, on every application
 * recorded there (one JSON line per application: the tag and each argument
 * as `eval_builtin_raw` takes it, `{ k: kind, h: hex }`). The two must give
 * the same value or both fail, except for the differences listed, each of
 * which the pinned evaluator decides as Cardano does. Where the builtin
 * fails, whether the evaluator charged it must match Midgard's own failure
 * charge. This file goes with the HarmonicLabs evaluator.
 */
import { createReadStream, readFileSync } from "node:fs";
import { createInterface } from "node:readline";

import { CEKError } from "@harmoniclabs/plutus-machine";
import { describe, expect, it } from "vitest";

import { MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS } from "../src/cek-builtin.cardano-exact.js";
import { evaluateMidgardCekDirectBuiltin } from "../src/cek-builtin.js";
import {
  evaluatePinnedBuiltin,
  MIDGARD_CEK_PINNED_BUILTIN_ERROR,
} from "../src/cek-builtin.pinned-evaluator.js";
import {
  encodeMidgardCekPlutusData,
  midgardCekConstantWitnessFromUplc,
} from "../src/cek-constant.js";
import { midgardCekBuiltinArgumentCount } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { decodeCardanoFlatProgramOrThrow } from "../src/cek-program.cardano-flat.decode.js";
import {
  constPairTypeArguments,
  constT,
  type ConstType,
  ConstTyTag,
  type ConstValue,
  type Pair,
  UPLCConst,
} from "../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "../src/uplc-data.js";
import {
  constantOfReferenceValue,
  referenceValueOf,
  runHarmonicReferenceBuiltin,
} from "./helpers/harmonic-reference-builtin.js";

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

type FixtureCase = {
  readonly tag: number;
  readonly note: string;
  readonly args: readonly { readonly ty: FixtureType; readonly v: unknown }[];
  readonly expected:
    | { readonly kind: "error" }
    | {
        readonly kind: "value";
        readonly type: FixtureType;
        readonly value: unknown;
      };
};

const fixture = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/cek-builtin-cardano-v1.generated.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as { readonly cases: readonly FixtureCase[] };

const fromHex = (value: string): Uint8Array =>
  Uint8Array.from(Buffer.from(value, "hex"));

const toData = (value: FixtureData): Data => {
  if ("i" in value) return new DataI(BigInt(value.i));
  if ("b" in value) return new DataB(fromHex(value.b));
  if ("l" in value) return new DataList(value.l.map(toData));
  if ("m" in value)
    return new DataMap(
      value.m.map(([key, entry]) => new DataPair(toData(key), toData(entry))),
    );
  return new DataConstr(BigInt(value.c), value.f.map(toData));
};

const typeOf = (ty: FixtureType): ConstType => {
  if (typeof ty === "object")
    return "list" in ty
      ? constT.listOf(typeOf(ty.list))
      : constT.pairOf(typeOf(ty.pair[0]), typeOf(ty.pair[1]));
  const leaf = {
    int: constT.int,
    bs: constT.byteStr,
    str: constT.str,
    unit: constT.unit,
    bool: constT.bool,
    data: constT.data,
    g1: constT.bls12_381_G1_element,
    g2: constT.bls12_381_G2_element,
    ml: constT.bls12_381_MlResult,
  }[ty];
  return leaf;
};

const valueOf = (ty: FixtureType, v: unknown): ConstValue => {
  if (typeof ty === "object") {
    if ("list" in ty)
      return (v as readonly unknown[]).map((item) => valueOf(ty.list, item));
    const [first, second] = v as readonly [unknown, unknown];
    return {
      fst: valueOf(ty.pair[0], first),
      snd: valueOf(ty.pair[1], second),
    };
  }
  switch (ty) {
    case "int":
      return BigInt(v as string);
    case "bs":
    case "g1":
    case "g2":
      return fromHex(v as string);
    case "str":
      return v as string;
    case "unit":
      return undefined;
    case "bool":
      return v as boolean;
    case "data":
      return toData(v as FixtureData);
    case "ml":
      throw new Error("no Miller loop arguments here");
  }
};

/** A value in a form the two evaluators' results compare in. */
const describeValue = (type: readonly number[], value: ConstValue): unknown => {
  switch (type[0]) {
    case ConstTyTag.int:
      return (value as bigint).toString();
    case ConstTyTag.byteStr:
    case ConstTyTag.bls12_381_G1_element:
    case ConstTyTag.bls12_381_G2_element:
      return Buffer.from(value as Uint8Array).toString("hex");
    case ConstTyTag.data:
      return encodeMidgardCekPlutusData(value as Data).toString("hex");
    case ConstTyTag.list:
      return (value as readonly ConstValue[]).map((item) =>
        describeValue(type.slice(1), item),
      );
    case ConstTyTag.pair: {
      const [first, second] = constPairTypeArguments(type as ConstType);
      const pair = value as Pair<ConstValue, ConstValue>;
      return [describeValue(first, pair.fst), describeValue(second, pair.snd)];
    }
    default:
      return value ?? null;
  }
};

const describeConstant = (constant: UPLCConst): string =>
  JSON.stringify([constant.type, describeValue(constant.type, constant.value)]);

/** A golden value in the form `describeValue` gives. */
const describeExpected = (ty: FixtureType, value: unknown): unknown => {
  if (typeof ty === "object") {
    if ("list" in ty)
      return (value as readonly unknown[]).map((item) =>
        describeExpected(ty.list, item),
      );
    const [first, second] = value as readonly [unknown, unknown];
    return [
      describeExpected(ty.pair[0], first),
      describeExpected(ty.pair[1], second),
    ];
  }
  return ty === "data" ? (value as { readonly cbor: string }).cbor : value;
};

type Outcome =
  | { readonly kind: "value"; readonly value: string }
  | { readonly kind: "failure" }
  | { readonly kind: "throw"; readonly message: string };

const harmonicOutcome = (
  tag: number,
  arguments_: readonly UPLCConst[],
): Outcome => {
  try {
    const result = runHarmonicReferenceBuiltin(
      tag,
      arguments_.map(referenceValueOf),
    );
    return result instanceof CEKError
      ? { kind: "failure" }
      : {
          kind: "value",
          value: describeConstant(constantOfReferenceValue(result)),
        };
  } catch (error) {
    return { kind: "throw", message: (error as Error).message };
  }
};

const pinnedOutcome = (
  tag: number,
  arguments_: readonly UPLCConst[],
): Outcome => {
  try {
    const result = evaluatePinnedBuiltin(tag, arguments_);
    return result.kind === "value"
      ? { kind: "value", value: describeConstant(result.value) }
      : { kind: "failure" };
  } catch (error) {
    return { kind: "throw", message: (error as Error).message };
  }
};

const WRITE_BITS = 80;
const SERIALISE_DATA = 51;

/**
 * Whether Midgard hands this application to the evaluator: the Cardano-exact
 * builtins, serialiseData and writeBits past its input bound are Midgard's
 * own and stay so.
 */
const reachesEvaluator = (
  tag: number,
  arguments_: readonly UPLCConst[],
): boolean =>
  !MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS.has(tag) &&
  tag !== SERIALISE_DATA &&
  !(
    tag === WRITE_BITS &&
    arguments_[0]?.value instanceof Uint8Array &&
    arguments_[0].value.length > 4_096
  );

type Difference = {
  readonly tag: number;
  readonly label: string;
  readonly harmonic: Outcome;
  readonly pinned: Outcome;
};

const same = (left: Outcome, right: Outcome): boolean =>
  left.kind === right.kind &&
  (left.kind !== "value" ||
    left.value === (right as { readonly value: string }).value);

/**
 * The two evaluators on fresh copies of one application (HarmonicLabs'
 * evaluator writes into some of its arguments), or null where they agree.
 */
const compare = (
  tag: number,
  label: string,
  arguments_: () => readonly UPLCConst[],
): Difference | null => {
  const harmonic = harmonicOutcome(tag, arguments_());
  const pinned = pinnedOutcome(tag, arguments_());
  return same(harmonic, pinned) ? null : { tag, label, harmonic, pinned };
};

const report = (differences: readonly Difference[]): void => {
  if (differences.length > 0)
    console.info(
      differences.map((difference) => JSON.stringify(difference)).join("\n"),
    );
};

/** A recorded argument as `eval_builtin_raw` takes it, as a constant. */
const recordedArgument = (argument: {
  readonly k: number;
  readonly h: string;
}): UPLCConst => {
  const bytes = fromHex(argument.h);
  const points = (size: number): Uint8Array[] =>
    Array.from({ length: bytes.length / size }, (_, index) =>
      bytes.slice(index * size, (index + 1) * size),
    );
  switch (argument.k) {
    case 0: {
      const { body } = decodeCardanoFlatProgramOrThrow(bytes);
      if (!(body instanceof UPLCConst)) throw new Error("not a constant");
      return body;
    }
    case 1:
      return new UPLCConst(constT.bls12_381_G1_element, bytes);
    case 2:
      return new UPLCConst(constT.bls12_381_G2_element, bytes);
    case 3:
      return new UPLCConst(
        constT.listOf(constT.bls12_381_G1_element),
        points(48),
      );
    case 4:
      return new UPLCConst(
        constT.listOf(constT.bls12_381_G2_element),
        points(96),
      );
    default:
      throw new Error(`unknown argument kind ${argument.k}`);
  }
};

const corpusCases = fixture.cases.filter(
  (entry) => entry.tag < 68 || entry.tag > 70,
);

const corpusArguments = (entry: FixtureCase): UPLCConst[] =>
  entry.args.map(
    (argument) =>
      new UPLCConst(typeOf(argument.ty), valueOf(argument.ty, argument.v)),
  );

/** Midgard's direct witness of a corpus argument. */
const directArgument = (constant: UPLCConst) => {
  const bls =
    constant.type[0] === ConstTyTag.bls12_381_G1_element
      ? "9f09ff"
      : constant.type[0] === ConstTyTag.bls12_381_G2_element
        ? "9f0aff"
        : null;
  return {
    kind: "constant" as const,
    witness:
      bls === null
        ? midgardCekConstantWitnessFromUplc(constant)
        : {
            typeCbor: Buffer.from(bls, "hex"),
            payloadCbor: encodeMidgardCekPlutusData(
              new DataB(constant.value as Uint8Array),
            ),
          },
  };
};

describe("HarmonicLabs BnCEK against the pinned evaluator", () => {
  it("agrees on every golden channel application Midgard evaluates there", () => {
    const differences: Difference[] = [];
    const owned: Difference[] = [];
    for (const entry of corpusCases) {
      const difference = compare(entry.tag, entry.note, () =>
        corpusArguments(entry),
      );
      if (difference === null) continue;
      (reachesEvaluator(entry.tag, corpusArguments(entry))
        ? differences
        : owned
      ).push(difference);
    }
    console.info(`golden: Midgard's own builtins differ on ${owned.length}`);
    report(owned);
    report(differences);
    expect(differences).toEqual([]);
  });

  it("gives every golden channel outcome", () => {
    const differences: string[] = [];
    for (const entry of corpusCases) {
      const pinned = pinnedOutcome(entry.tag, corpusArguments(entry));
      const expected: Outcome =
        entry.expected.kind === "error"
          ? { kind: "failure" }
          : {
              kind: "value",
              value: JSON.stringify([
                typeOf(entry.expected.type),
                describeExpected(entry.expected.type, entry.expected.value),
              ]),
            };
      if (!same(expected, pinned))
        differences.push(
          `${entry.tag} ${entry.note}: ${JSON.stringify(pinned).slice(0, 200)}`,
        );
    }
    console.info(differences.join("\n"));
    expect(differences).toEqual([]);
  });

  // Midgard's own failure charge decides the budget (unchanged here). The
  // pinned evaluator charges every failure past costing, as Plutus does; a
  // charge Midgard leaves out is on a failing application, which fails the
  // script either way. Midgard never charges what the evaluator refuses to
  // cost, and charges only failures the evaluator costs.
  it("charges a failing golden channel application no more than Plutus", () => {
    const overcharged: string[] = [];
    let uncharged = 0;
    for (const entry of corpusCases) {
      const pinned = evaluatePinnedBuiltin(entry.tag, corpusArguments(entry));
      if (pinned.kind !== "failure") continue;
      const midgard = evaluateMidgardCekDirectBuiltin(
        BigInt(entry.tag),
        corpusArguments(entry).map(directArgument),
      );
      const charged = midgard.budget.cpu !== 0n || midgard.budget.memory !== 0n;
      const costed =
        pinned.errorKind === MIDGARD_CEK_PINNED_BUILTIN_ERROR.failed;
      if (midgard.kind !== "failure" || (charged && !costed))
        overcharged.push(`${entry.tag} ${entry.note}`);
      if (!charged && costed) uncharged += 1;
    }
    console.info(`golden: failures Midgard leaves uncharged: ${uncharged}`);
    expect(overcharged).toEqual([]);
  });

  it("takes each builtin's arity from Midgard's table", () => {
    for (const entry of corpusCases)
      expect(BigInt(entry.args.length)).toBe(
        midgardCekBuiltinArgumentCount(BigInt(entry.tag)),
      );
  });

  const recording = process.env.MIDGARD_BUILTIN_APPLICATIONS;
  it.runIf(recording !== undefined)(
    "agrees on every recorded application Midgard evaluates there",
    { timeout: 3_600_000 },
    async () => {
      const differences: Difference[] = [];
      const owned: Difference[] = [];
      const counts = new Map<number, number>();
      const lines = createInterface({
        input: createReadStream(recording!),
        crlfDelay: Infinity,
      });
      let index = 0;
      for await (const line of lines) {
        index += 1;
        const { tag, args } = JSON.parse(line) as {
          readonly tag: number;
          readonly args: readonly { k: number; h: string }[];
        };
        counts.set(tag, (counts.get(tag) ?? 0) + 1);
        const arguments_ = () => args.map(recordedArgument);
        const difference = compare(tag, `recorded ${index}`, arguments_);
        if (difference === null) continue;
        (reachesEvaluator(tag, arguments_()) ? differences : owned).push(
          difference,
        );
      }
      console.info(
        JSON.stringify({
          recorded: index,
          perTag: Object.fromEntries([...counts].sort((a, b) => a[0] - b[0])),
          owned: owned.length,
          differences: differences.length,
        }),
      );
      report(owned);
      report(differences);
      expect(differences).toEqual([]);
    },
  );
});
