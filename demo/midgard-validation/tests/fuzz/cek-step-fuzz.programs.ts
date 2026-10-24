import { MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES } from "@al-ft/midgard-core";
import { getNRequiredForces } from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "../../src/cek-constant.js";
import { UPLCConst } from "../../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "../../src/uplc-data.js";
import {
  Builtin,
  Case,
  Constr,
  ErrorUPLC,
  type UPLCTerm,
} from "../../src/uplc-term.js";
import {
  type CekBuiltinData,
  makeRng,
  randomData,
  type Rng,
} from "../fixtures/cek-builtin-cardano-v1.arguments.mjs";
import {
  type CekBuiltinArgument,
  type CekBuiltinArgumentType,
  type CekBuiltinMl,
  edgeCases,
  randomArguments,
} from "../fixtures/cek-builtin-cardano-v1.cases.mjs";
import { app, delay, force, lam, seq, v } from "../helpers/cek-uplc-terms.js";
import { builtin } from "../helpers/uplc-builtins.js";

/**
 * Seeded UPLC programs for the step differential. Each case calls one
 * primary builtin (and up to two others) over the builtin golden channel's
 * per-tag argument mix and edge vectors, inside a random structural shape:
 * bindings, delays, constructor cases, partial applications, discarded side
 * computations. A share of cases substitutes an ill-typed argument, wraps the
 * program in a shape that fails at a machine arm, runs on a budget that
 * stops it early, or passes a Data constant at the source-constant bound, so
 * the failure arms and the size boundaries are reached as well.
 */

/** Tags reached only inside finalVerify's arguments (no literal result). */
const ARGUMENT_ONLY_TAGS = new Set([68, 69]);
const MAX_BUILTIN_TAG = 86;

/** Every builtin a case may take as its primary call. */
export const FUZZ_PRIMARY_TAGS: readonly number[] = Array.from(
  { length: MAX_BUILTIN_TAG + 1 },
  (_, tag) => tag,
).filter((tag) => !ARGUMENT_ONLY_TAGS.has(tag));

// Cardano's flat tags (Plutus `Builtins.hs`): 59 is G1 uncompress and 66 is
// G2 uncompress.
const G1_UNCOMPRESS = 59;
const G2_UNCOMPRESS = 66;
const MILLER_LOOP = 68;
const MUL_ML_RESULT = 69;
const ADD_INTEGER = 0;
// appendByteString: a builtin that takes no type argument.
const MONOMORPHIC_BUILTIN = 10;

const fromHex = (value: string): Uint8Array =>
  Uint8Array.from(Buffer.from(value, "hex"));

/** A channel-encoded Data value as Plutus Data. */
export const toData = (value: CekBuiltinData): Data => {
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

const sampleValue = (ty: CekBuiltinArgumentType): unknown => {
  if (typeof ty === "object") {
    return "list" in ty
      ? []
      : [sampleValue(ty.pair[0]), sampleValue(ty.pair[1])];
  }
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (ty) {
    case "int":
      return "0";
    case "data":
      return { i: "0" };
    case "bool":
      return false;
    case "unit":
      return null;
    default:
      return "";
  }
};

const uplcConstant = (
  ty: CekBuiltinArgumentType,
  value: unknown,
): UPLCConst => {
  if (typeof ty === "object") {
    if ("list" in ty) {
      const items = (value as readonly unknown[]).map((item) =>
        uplcConstant(ty.list, item),
      );
      const element = uplcConstant(ty.list, sampleValue(ty.list)).type;
      return UPLCConst.listOf(element)(
        items.map((item) => item.value) as never,
      );
    }
    const [first, second] = value as readonly [unknown, unknown];
    const left = uplcConstant(ty.pair[0], first);
    const right = uplcConstant(ty.pair[1], second);
    return UPLCConst.pairOf(left.type, right.type)(left.value, right.value);
  }
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (ty) {
    case "int":
      return UPLCConst.int(BigInt(value as string));
    case "bs":
      return UPLCConst.byteString(fromHex(value as string));
    case "str":
      return UPLCConst.str(value as string);
    case "unit":
      return UPLCConst.unit;
    case "bool":
      return UPLCConst.bool(value as boolean);
    case "data":
      return UPLCConst.data(toData(value as CekBuiltinData));
    default:
      throw new Error(`no UPLC constant of type ${String(ty)}`);
  }
};

/** `[(force^n (builtin tag)) args...]`. */
export const builtinCall = (
  tag: number,
  args: readonly UPLCTerm[],
): UPLCTerm => {
  let term: UPLCTerm = new Builtin(tag);
  for (let index = 0; index < getNRequiredForces(tag); index += 1) {
    term = force(term);
  }
  return app(term, ...args);
};

const pointTerm = (tag: number, value: string): UPLCTerm =>
  builtinCall(tag, [UPLCConst.byteString(fromHex(value))]);

const mlTerm = (value: CekBuiltinMl): UPLCTerm =>
  "loop" in value
    ? builtinCall(MILLER_LOOP, [
        pointTerm(G1_UNCOMPRESS, value.loop[0]),
        pointTerm(G2_UNCOMPRESS, value.loop[1]),
      ])
    : builtinCall(MUL_ML_RESULT, [mlTerm(value.mul[0]), mlTerm(value.mul[1])]);

const argumentTerm = (argument: CekBuiltinArgument): UPLCTerm => {
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (argument.ty) {
    case "g1":
      return pointTerm(G1_UNCOMPRESS, argument.v as string);
    case "g2":
      return pointTerm(G2_UNCOMPRESS, argument.v as string);
    case "ml":
      return mlTerm(argument.v as CekBuiltinMl);
    default:
      return uplcConstant(argument.ty, argument.v);
  }
};

/** A Data value whose Midgard encoding is at most `target` bytes, and near it. */
const dataNearBound = (rng: Rng, target: number): Data => {
  const shape = rng.int(3);
  const build = (length: number): Data => {
    const payload = new Uint8Array(length).fill(0x5a);
    if (shape === 0) return new DataB(payload);
    if (shape === 1) {
      return new DataList([
        new DataI(BigInt(rng.int(1000))),
        new DataB(payload),
      ]);
    }
    return new DataConstr(BigInt(rng.int(8)), [
      new DataMap([new DataPair(new DataI(1n), new DataB(payload))]),
    ]);
  };
  for (let length = target; length >= 0; length -= 1) {
    const candidate = build(length);
    if (encodeMidgardCekPlutusData(candidate).length <= target) {
      return candidate;
    }
  }
  throw new Error(`no Data value fits in ${target.toString()} bytes`);
};

/** A constant of a random simple type, for ill-typed argument substitution. */
const wrongTypeConstant = (rng: Rng): UPLCTerm =>
  rng.pick<UPLCTerm>([
    UPLCConst.unit,
    UPLCConst.bool(true),
    UPLCConst.int(BigInt(rng.int(1000)) - 500n),
    UPLCConst.byteString(new Uint8Array(rng.int(40)).fill(7)),
    UPLCConst.str("x"),
    UPLCConst.data(new DataI(5n)),
  ]);

/** Builtins of one Data argument, which take any Data near the bound. */
const DATA_ARGUMENT_TAGS = new Set([42, 43, 44, 45, 46, 51]);

/** The Data shape each destructor reads: unConstrData to unBData. */
const DESTRUCTED_SHAPE: Readonly<Record<number, string>> = {
  42: "c",
  43: "m",
  44: "l",
  45: "i",
  46: "b",
};

/** Random Data of `shape`, so a destructor's success arm is reached. */
const randomDataOfShape = (rng: Rng, shape: string): CekBuiltinData => {
  for (let draw = 0; draw < 64; draw += 1) {
    const value = randomData(rng, 3);
    if (shape in value) return value;
  }
  return randomData(rng, 3);
};

const argumentsOf = (rng: Rng, tag: number): readonly CekBuiltinArgument[] => {
  const shape = DESTRUCTED_SHAPE[tag];
  if (shape !== undefined && rng.chance(0.4)) {
    return [{ ty: "data", v: randomDataOfShape(rng, shape) }];
  }
  const edges = edgeCases(tag);
  return edges.length > 0 && rng.chance(0.3)
    ? rng.pick(edges).args
    : randomArguments(tag, rng);
};

const primaryCall = (rng: Rng, tag: number): UPLCTerm => {
  if (DATA_ARGUMENT_TAGS.has(tag) && rng.chance(0.2)) {
    const bound = MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES;
    const target = rng.chance(0.5) ? bound : bound - 1 - rng.int(64);
    return builtinCall(tag, [UPLCConst.data(dataNearBound(rng, target))]);
  }
  const terms = argumentsOf(rng, tag).map(argumentTerm);
  if (terms.length > 0 && rng.chance(0.1)) {
    terms[rng.int(terms.length)] = wrongTypeConstant(rng);
  }
  if (terms.length >= 2 && rng.chance(0.25)) {
    // Bind the partial application, then apply it to the last argument.
    const partial = builtinCall(tag, terms.slice(0, -1));
    return app(lam(app(v(0), terms.at(-1)!)), partial);
  }
  return builtinCall(tag, terms);
};

/** A random shape around `term` that keeps its value when it has one. */
const wrap = (rng: Rng, term: UPLCTerm, depth: number): UPLCTerm => {
  let wrapped = term;
  for (let level = 0; level < depth; level += 1) {
    const inner = wrapped;
    switch (rng.int(9)) {
      case 0:
        wrapped = app(lam(v(0)), inner);
        break;
      case 1:
        wrapped = force(delay(inner));
        break;
      case 2:
        wrapped = new Case(new Constr(0, [inner]), [lam(v(0))]);
        break;
      case 3:
        wrapped = seq(
          builtinCall(ADD_INTEGER, [UPLCConst.int(1n), UPLCConst.int(2n)]),
          inner,
        );
        break;
      case 4:
        wrapped = new Case(new Constr(1, []), [lam(UPLCConst.unit), inner]);
        break;
      case 5:
        // The body reads the binding from two scopes down.
        wrapped = app(lam(app(lam(v(1)), UPLCConst.unit)), inner);
        break;
      case 6:
        wrapped = force(
          app(
            force(builtin("ifThenElse")),
            UPLCConst.bool(true),
            delay(inner),
            delay(new ErrorUPLC()),
          ),
        );
        break;
      case 7:
        // A two-field constructor cased into a builtin branch:
        // chooseUnit () inner.
        wrapped = new Case(new Constr(0, [UPLCConst.unit, inner]), [
          force(builtin("chooseUnit")),
        ]);
        break;
      default:
        wrapped = app(lam(lam(v(1))), inner, delay(UPLCConst.unit));
    }
  }
  return wrapped;
};

/** A shape that fails at a machine arm rather than inside a builtin. */
const malformed = (rng: Rng, term: UPLCTerm): UPLCTerm => {
  switch (rng.int(9)) {
    case 0:
      return app(UPLCConst.unit, term);
    case 1:
      return seq(term, force(UPLCConst.int(3n)));
    case 2:
      return seq(term, new Case(UPLCConst.unit, [lam(v(0))]));
    case 3:
      return new Case(new Constr(3, [term]), [lam(v(0))]);
    case 4:
      return seq(term, new ErrorUPLC());
    case 5:
      return app(app(lam(v(0)), UPLCConst.int(1n)), term);
    case 7:
      return new Case(new Constr(0, [term]), [UPLCConst.int(1n)]);
    case 6:
      // The machine cases on constructor values only.
      return new Case(UPLCConst.int(BigInt(rng.int(2))), [term, term]);
    default:
      // Forces a partially applied builtin that takes no type argument.
      return seq(
        term,
        force(
          builtinCall(MONOMORPHIC_BUILTIN, [
            UPLCConst.byteString(new Uint8Array([1])),
          ]),
        ),
      );
  }
};

/** One program of the differential and the context it is applied to. */
export type CekStepFuzzProgram = {
  readonly label: string;
  /** The script: a function of the script context. */
  readonly term: UPLCTerm;
  readonly context: Data;
  /** Runs on a budget far below the transaction cap, to stop on it. */
  readonly tightBudget: boolean;
};

/** The case of `seed` whose primary builtin is `tag`. */
export const cekStepFuzzProgram = (
  seed: number,
  tag: number,
): CekStepFuzzProgram => {
  const rng = makeRng(seed);
  const calls = [primaryCall(rng, tag)];
  for (let extra = rng.int(3); extra > 0; extra -= 1) {
    calls.push(primaryCall(rng, rng.pick(FUZZ_PRIMARY_TAGS)));
  }
  const [primary, ...extras] = calls.map((call) => wrap(rng, call, rng.int(4)));
  // `seq` evaluates its kept term first: the primary call runs first, so a
  // failing extra call never hides it, then the extras in order. The value is
  // the primary call's.
  let body = extras.reduce((kept, extra) => seq(extra, kept), primary);
  if (rng.chance(0.1)) body = malformed(rng, body);
  if (rng.chance(0.15)) body = seq(v(0), body);
  return {
    label: `seed ${seed.toString()} builtin ${tag.toString()}`,
    term: lam(body),
    context: toData(randomData(rng, 3)),
    tightBudget: rng.chance(0.05),
  };
};
