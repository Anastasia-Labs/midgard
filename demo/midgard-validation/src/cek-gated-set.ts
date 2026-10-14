import { type MidgardCekCoreStepWitness } from "./cek-machine.js";

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

/** A gated set prepared for per-step lookups. */
export type MidgardCekStepScreen = (witness: MidgardCekCoreStepWitness) => void;

/** Throws `MidgardCekOutsideGatedSet` on a step outside `set`. */
export const midgardCekStepScreen = (
  set: MidgardCekGatedSet,
): MidgardCekStepScreen => {
  const builtins = new Set(set.builtins);
  const arms = new Set<string>(set.arms);
  return (witness) => {
    const builtin = midgardCekBuiltinTouchedBy(witness);
    if (
      !arms.has(witness.kind) ||
      (builtin !== null && !builtins.has(builtin))
    ) {
      throw new MidgardCekOutsideGatedSet(set.version, witness.kind, builtin);
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
