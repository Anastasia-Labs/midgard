/** The hops of a thread of ours through the one-step resolution of a CEK
 * core step past the CEK resolver: from the CEK core step binder to the
 * first arm of the step's arm chain (VerifyCoreStep), from each arm to the
 * next and from the last to the settle (VerifyArm), and from the settle to
 * the award (Settle). The binder writes the bound with progress 0 and no
 * facts; each arm carries it on with its progress one higher and the facts
 * one-step-core-chains.ts says it writes. */
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";

import type { ValidationTraceHop } from "./one-step-builders.js";
import {
  isValidationTraceArmHolder,
  type ValidationTraceArmHolder,
} from "./one-step-core-chains.js";
import {
  bytes,
  constr,
  continued,
  integer,
  stepDatum,
} from "./one-step-data.js";
import {
  hashSerialisedData,
  type ValidationTraceCoreStepPlan,
  type ValidationTraceOneStepPlan,
} from "./one-step-plan.js";

/** The holders of a CEK core step's resolution past the CEK resolver. */
export type ValidationTraceCoreHolder =
  | "core"
  | ValidationTraceArmHolder
  | "settle";

export const isValidationTraceCoreHolder = (
  holder: string,
): holder is ValidationTraceCoreHolder =>
  holder === "core" ||
  holder === "settle" ||
  isValidationTraceArmHolder(holder);

/** Where the bound a core holder keeps carries its progress and facts. */
const PROGRESS = [1, 0, 6];
const FACTS = [1, 0, 7];

/** The core step plan a core holder's hop needs. */
const corePlan = (
  plan: ValidationTraceOneStepPlan,
  holder: ValidationTraceCoreHolder,
): ValidationTraceCoreStepPlan => {
  if (plan.kind !== "core")
    throw new Error(
      `validation-trace ${holder} holds a thread whose step is not a CEK core step`,
    );
  return plan;
};

/** The first holder of the arm chain the plan's core step settles
 * through. */
export const validationTraceArmOf = (
  plan: ValidationTraceCoreStepPlan,
): ValidationTraceArmHolder => plan.core.hops.holders[0]!;

/**
 * The hop of the thread with datum `datum` of prover `prover`, held by the
 * core holder `holder`, in the one-step resolution `plan`.
 */
export const cekCoreHop = ({
  plan,
  holder,
  datum,
  prover,
}: {
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: ValidationTraceCoreHolder;
  readonly datum: string;
  readonly prover: string;
}): ValidationTraceHop => {
  const step = corePlan(plan, holder);
  const { core } = step;
  if (holder === "core")
    return {
      next: validationTraceArmOf(step),
      datum: stepDatum(
        prover,
        constr(0, [
          plutusConstrFieldCbor(datum, [1, 0]),
          core.preCbor,
          core.postCbor,
          bytes(hashSerialisedData(core.armWitnessCbor)),
          integer(core.arm),
          integer(core.group),
          integer(0),
          constr(0, []),
        ]),
      ),
      redeemer: (inputIndex, outputIndex) =>
        continued(
          constr(0, [
            integer(inputIndex),
            integer(outputIndex),
            step.transitionCbor,
            core.stepCbor,
          ]),
        ),
      proverOnly: true,
    };
  if (holder === "settle")
    return {
      next: "award",
      datum: stepDatum(prover, constr(0, [integer(1)])),
      redeemer: (inputIndex, outputIndex) =>
        continued(
          constr(0, [
            integer(inputIndex),
            integer(outputIndex),
            step.transitionCbor,
            core.armWitnessCbor,
          ]),
        ),
      proverOnly: false,
    };
  const { holders, facts } = core.hops;
  const index = holders.indexOf(holder);
  if (index < 0)
    throw new Error(
      `validation-trace ${holder} holds a thread whose CEK core step settles through other arms`,
    );
  // The arm carries the bound on with its progress one higher and the facts
  // it writes, to the next arm or, past the last, to the settle.
  return {
    next: holders[index + 1] ?? "settle",
    datum: replacePlutusConstrFieldCbor(
      replacePlutusConstrFieldCbor(datum, PROGRESS, integer(index + 1)),
      FACTS,
      facts[index]!,
    ),
    redeemer: (inputIndex, outputIndex) =>
      continued(
        constr(0, [
          integer(inputIndex),
          integer(outputIndex),
          core.armWitnessCbor,
        ]),
      ),
    proverOnly: true,
  };
};
