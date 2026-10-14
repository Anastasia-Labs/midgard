import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
} from "./cek-constant.js";
import { type MidgardCekCoreStepWitness } from "./cek-machine.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { isPlutusDataMap } from "./plutus-data-narrowing.js";

/**
 * The equivalence-gated set: the builtins and CEK machine arms on which the
 * TypeScript trace builder and the on-chain machine have been shown to agree
 * step for step (the per-builtin coverage report of
 * `tests/fuzz/cek-step-fuzz.test.ts`). Admission refuses a Plutus transaction
 * whose verdict run touches anything outside it, so the node never admits a
 * transaction whose trace it could not stand behind in a dispute.
 *
 * The screen is local admission policy only. It never applies to a commit of
 * an already admitted transaction, to forced transactions, or to the
 * verification of another operator's block.
 *
 * The set is versioned: a new version is a new constant, added once the
 * coverage report covers what it adds. `tests/cek-gated-set.test.ts` holds
 * every version to the checked-in report.
 */
export type MidgardCekGatedSet = {
  readonly version: number;
  /** Builtin tags a run may name. */
  readonly builtins: readonly number[];
  /** Machine arms (step witness kinds) a run may take. */
  readonly arms: readonly MidgardCekCoreStepWitness["kind"][];
};

/** The finalVerify tag, which its BLS step names without a tag field. */
const FINAL_VERIFY_TAG = 70;

/**
 * The builtin a step touches: the builtin it reads, applies, forces or
 * executes. Null for a step that names no builtin. Constructor tags are not
 * builtin tags.
 */
export const midgardCekBuiltinTouchedBy = (
  witness: MidgardCekCoreStepWitness,
): number | null => {
  // Every other arm names no builtin.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (witness.kind) {
    case "computeBuiltin":
    case "returnApplyBuiltin":
    case "returnApplyValueBuiltin":
    case "returnForceBuiltin":
    case "executeBuiltinTypeFailure":
    case "executeBuiltinDirect":
    case "executeBuiltinSemantic":
    case "startBuiltinMapConversion":
    case "executeBuiltinSemanticFailure":
    case "executeBuiltinFailure":
      return Number(witness.tag);
    case "executeBuiltinBlsFinal":
      return FINAL_VERIFY_TAG;
    default:
      return null;
  }
};

/**
 * Unwinds a run at its first step outside the gated set. Not a verdict: the
 * caller refuses the transaction under its own code.
 */
export class MidgardCekOutsideGatedSet extends Error {
  constructor(
    readonly gatedSetVersion: number,
    readonly arm: MidgardCekCoreStepWitness["kind"],
    readonly builtin: number | null,
  ) {
    super(
      builtin === null
        ? `CEK arm ${arm} is outside gated set v${gatedSetVersion.toString(10)}`
        : `CEK builtin ${builtin.toString(10)} (${arm}) is outside gated set v${gatedSetVersion.toString(10)}`,
    );
    this.name = "MidgardCekOutsideGatedSet";
  }
}

/**
 * A step whose witnesses the on-chain verifier cannot decode. Refused like a
 * step outside the gated set.
 */
export class MidgardCekUndecodableStepWitness extends MidgardCekOutsideGatedSet {
  constructor(
    gatedSetVersion: number,
    arm: MidgardCekCoreStepWitness["kind"],
    builtin: number | null,
  ) {
    super(gatedSetVersion, arm, builtin);
    this.message = `CEK ${arm} step witness is outside what the on-chain verifier decodes`;
    this.name = "MidgardCekUndecodableStepWitness";
  }
}

/** The constant payloads a step reveals in full. */
const revealedConstants = (
  witness: MidgardCekCoreStepWitness,
): MidgardCekConstantWitness[] => {
  if (witness.kind === "computeConstant") return [witness.value];
  const values = [
    ...("arguments" in witness ? witness.arguments : []),
    ...("result" in witness ? [witness.result] : []),
  ];
  return values.flatMap((value) =>
    value.kind === "constant" ? [value.witness] : [],
  );
};

/** The integers the on-chain payload decoder reads: one CBOR head. */
const MIN_DECODABLE_INTEGER = -(2n ** 64n);
const MAX_DECODABLE_INTEGER = 2n ** 64n - 1n;
/** The largest constructor index with a compact CBOR tag. */
const MAX_DECODABLE_CONSTR_INDEX = 127n;

/** Whether the on-chain payload decoder reads `payloadCbor`. */
const payloadIsDecodable = (payloadCbor: Uint8Array): boolean => {
  let pending: Data[];
  try {
    pending = [plutusDataFromCborIterative(payloadCbor)];
  } catch {
    return false;
  }
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    if (item instanceof DataI) {
      if (item.int < MIN_DECODABLE_INTEGER || item.int > MAX_DECODABLE_INTEGER)
        return false;
    } else if (item instanceof DataConstr) {
      if (item.constr > MAX_DECODABLE_CONSTR_INDEX) return false;
      for (const field of item.fields) pending.push(field);
    } else if (item instanceof DataList) {
      for (const element of item.list) pending.push(element);
    } else if (isPlutusDataMap(item)) {
      for (const entry of item.map) pending.push(entry.fst, entry.snd);
    }
  }
  return true;
};

/**
 * A failure step the on-chain verifier cannot prove. Refused like a step
 * outside the gated set.
 */
export class MidgardCekUnprovableBuiltinFailure extends MidgardCekOutsideGatedSet {
  constructor(gatedSetVersion: number, builtin: number) {
    super(gatedSetVersion, "executeBuiltinFailure", builtin);
    this.message = `CEK builtin ${builtin.toString(10)} failure is outside what the on-chain verifier proves`;
    this.name = "MidgardCekUnprovableBuiltinFailure";
  }
}

const G1_UNCOMPRESS_TAG = 60;
const G2_UNCOMPRESS_TAG = 67;
const BLS12_381_FIELD_PRIME =
  0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaabn;

const bigEndian = (bytes: Uint8Array): bigint =>
  bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);

const modularPower = (base: bigint, exponent: bigint, modulus: bigint) => {
  let result = 1n;
  let square = base % modulus;
  for (let rest = exponent; rest > 0n; rest >>= 1n) {
    if ((rest & 1n) === 1n) result = (result * square) % modulus;
    square = (square * square) % modulus;
  }
  return result;
};

/**
 * Whether a compressed encoding's flags or coordinates are invalid, as the
 * on-chain failure proof reads them.
 */
const blsHeaderIsInvalid = (encoded: Uint8Array): boolean => {
  const first = encoded[0] ?? 0;
  const firstCoordinate = bigEndian(
    Uint8Array.of(first % 32, ...encoded.subarray(1, 48)),
  );
  const secondCoordinate = bigEndian(encoded.subarray(48, 96));
  if (first < 128) return true;
  if (first % 128 >= 64) {
    return (
      first % 64 >= 32 || firstCoordinate !== 0n || secondCoordinate !== 0n
    );
  }
  return (
    firstCoordinate >= BLS12_381_FIELD_PRIME ||
    secondCoordinate >= BLS12_381_FIELD_PRIME
  );
};

/** Whether a G1 encoding's x has no point on the curve y^2 = x^3 + 4. */
const blsG1IsOffCurve = (encoded: Uint8Array): boolean => {
  const first = encoded[0] ?? 0;
  if (first % 128 >= 64) return false;
  const x = bigEndian(Uint8Array.of(first % 32, ...encoded.subarray(1, 48)));
  const ySquared = (x * x * x + 4n) % BLS12_381_FIELD_PRIME;
  return (
    ySquared !== 0n &&
    modularPower(
      ySquared,
      (BLS12_381_FIELD_PRIME - 1n) / 2n,
      BLS12_381_FIELD_PRIME,
    ) !== 1n
  );
};

/**
 * Whether the on-chain verifier proves a BLS uncompress failure on
 * `argument`: for G1 a wrong length, an invalid header or a point off the
 * curve; for G2 a wrong length or an invalid header. Every other builtin
 * failure is outside this screen.
 */
const builtinFailureIsProvable = (
  tag: number,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  if (tag !== G1_UNCOMPRESS_TAG && tag !== G2_UNCOMPRESS_TAG) return true;
  const [argument] = arguments_;
  if (arguments_.length !== 1 || argument?.kind !== "constant") return false;
  const { payload } = decodeMidgardCekConstantWitness(argument.witness);
  if (!(payload instanceof DataB)) return false;
  const encoded = payload.bytes;
  if (tag === G1_UNCOMPRESS_TAG) {
    return (
      encoded.length !== 48 ||
      blsHeaderIsInvalid(encoded) ||
      blsG1IsOffCurve(encoded)
    );
  }
  return encoded.length !== 96 || blsHeaderIsInvalid(encoded);
};

/** A gated set prepared for per-step lookups. */
export type MidgardCekStepScreen = (witness: MidgardCekCoreStepWitness) => void;

/**
 * Throws `MidgardCekOutsideGatedSet` on a step outside `set`,
 * `MidgardCekUndecodableStepWitness` on a step whose witnesses the on-chain
 * verifier cannot decode, and `MidgardCekUnprovableBuiltinFailure` on a
 * builtin failure the on-chain verifier cannot prove.
 */
export const midgardCekStepScreen = (
  set: MidgardCekGatedSet,
): MidgardCekStepScreen => {
  const builtins = new Set(set.builtins);
  const arms = new Set<string>(set.arms);
  // A run reveals the same payload many times; each is decoded once.
  const decodable = new WeakSet<Uint8Array>();
  return (witness) => {
    const builtin = midgardCekBuiltinTouchedBy(witness);
    if (
      !arms.has(witness.kind) ||
      (builtin !== null && !builtins.has(builtin))
    ) {
      throw new MidgardCekOutsideGatedSet(set.version, witness.kind, builtin);
    }
    if (
      witness.kind === "executeBuiltinFailure" &&
      !builtinFailureIsProvable(Number(witness.tag), witness.arguments)
    ) {
      throw new MidgardCekUnprovableBuiltinFailure(
        set.version,
        Number(witness.tag),
      );
    }
    for (const { payloadCbor } of revealedConstants(witness)) {
      if (decodable.has(payloadCbor)) continue;
      if (!payloadIsDecodable(payloadCbor)) {
        throw new MidgardCekUndecodableStepWitness(
          set.version,
          witness.kind,
          builtin,
        );
      }
      decodable.add(payloadCbor);
    }
  };
};

/**
 * Gated set version 1: every builtin the coverage report executes to a result
 * (all of 0 to 86) except verifySchnorr (53), whose on-chain equivalence is
 * pending; and every arm the report agrees on.
 */
export const MIDGARD_CEK_GATED_SET_V1: MidgardCekGatedSet = Object.freeze({
  version: 1,
  builtins: Object.freeze([
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59,
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
    79, 80, 81, 82, 83, 84, 85, 86,
  ]),
  arms: Object.freeze<MidgardCekCoreStepWitness["kind"][]>([
    "applyCaseValue",
    "computeApplication",
    "computeBuiltin",
    "computeCase",
    "computeConstant",
    "computeConstrEmpty",
    "computeConstrNonempty",
    "computeContextConstant",
    "computeDelay",
    "computeError",
    "computeForce",
    "computeLambda",
    "computeVariable",
    "executeBuiltinBlsFinal",
    "executeBuiltinDirect",
    "executeBuiltinFailure",
    "executeBuiltinSemantic",
    "executeBuiltinSemanticFailure",
    "executeBuiltinTypeFailure",
    "finishBuiltinMapConversion",
    "lookupEnvironment",
    "returnApplyArgument",
    "returnApplyBuiltin",
    "returnApplyInvalid",
    "returnApplyLambda",
    "returnApplyValueBuiltin",
    "returnApplyValueInvalid",
    "returnApplyValueLambda",
    "returnCaseConstr",
    "returnCaseInvalid",
    "returnConstrDone",
    "returnConstrNext",
    "returnEmptyContinuation",
    "returnForceBuiltin",
    "returnForceDelay",
    "returnForceInvalid",
    "selectCaseBranch",
    "startBuiltinMapConversion",
    "stepBuiltinListToMap",
    "stepBuiltinMapToList",
  ]),
});
