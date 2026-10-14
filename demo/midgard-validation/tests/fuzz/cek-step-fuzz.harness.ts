import {
  hashMidgardCekMachineState,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";

import {
  midgardCekCoreStepWitnessData,
  midgardCekMachineStateData,
} from "../../src/cek-data.js";
import {
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionStep,
  type MidgardCekStructuralExecution,
} from "../../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../../src/cek-machine.js";
import { makeRng } from "../fixtures/cek-builtin-cardano-v1.arguments.mjs";
import { CEK_CORE_STEP_TWISTS } from "../fixtures/cek-core-step-v1.vectors.mjs";
import {
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "../helpers/cek-uplc-terms.js";
import type {
  AikenStepArguments,
  AikenStepOracle,
  AikenVerdict,
} from "./cek-step-fuzz.oracle.js";
import type { CekStepFuzzProgram } from "./cek-step-fuzz.programs.js";

/**
 * One case end to end: the TypeScript builder runs the program in proof
 * mode; a deterministic sample of its steps, and one perturbed copy per
 * negative twist, goes to the Aiken machine; every disagreement is a
 * divergence. Step arguments are built through the step codecs
 * (`midgardCekMachineStateData`, `midgardCekCoreStepWitnessData`), so a state
 * format change reaches both sides without touching this file.
 */

/** The step bound of one case; every generated program halts far below it. */
const MAX_STEPS = 400_000;
/** The budget of a case meant to stop on its budget. */
const TIGHT_BUDGET = { cpu: 1_500_000n, memory: 5_000n } as const;
/** Ordinary steps sampled per case, beyond the first of each kind. */
const SAMPLED_ORDINARY_STEPS = 6;
/** Builtin-executing steps adjudicated per case. */
const MAX_EXECUTING_STEPS = 24;

type WitnessKind = MidgardCekExecutionStep["witness"]["kind"];

/** The arms that execute a builtin, and so name the builtin's tag. */
const EXECUTING_KINDS = new Set<WitnessKind>([
  "executeBuiltinDirect",
  "executeBuiltinSemantic",
  "executeBuiltinSemanticFailure",
  "executeBuiltinFailure",
  "executeBuiltinTypeFailure",
  "startBuiltinMapConversion",
]);
const FINAL_VERIFY_TAG = 70;

/** The builtin a step executes, or null. */
export const executedBuiltin = (
  step: MidgardCekExecutionStep,
): number | null => {
  const witness = step.witness;
  if (witness.kind === "executeBuiltinBlsFinal") return FINAL_VERIFY_TAG;
  return EXECUTING_KINDS.has(witness.kind) && "tag" in witness
    ? Number(witness.tag)
    : null;
};

const stateCbor = (state: MidgardCekMachineState): Uint8Array =>
  lucidDataToCborIterative(midgardCekMachineStateData(state) as never);

const stepArguments = (step: MidgardCekExecutionStep): AikenStepArguments => ({
  pre: stateCbor(step.pre),
  post: stateCbor(step.post),
  witness: lucidDataToCborIterative(
    midgardCekCoreStepWitnessData(step.witness) as never,
  ),
});

export type SampledStep = {
  readonly index: number;
  readonly kind: WitnessKind;
  readonly builtin: number | null;
  /** The negative twist applied, or null for a genuine step. */
  readonly twist: string | null;
  readonly arguments: AikenStepArguments;
  readonly postHash: string;
};

export type PreparedCase = {
  readonly program: CekStepFuzzProgram;
  readonly totalSteps: number;
  readonly stopReason: MidgardCekStructuralExecution["stopReason"] | "threw";
  readonly sampled: readonly SampledStep[];
  /** Defects found on the TypeScript side alone. */
  readonly local: readonly string[];
};

const runProgram = (
  program: CekStepFuzzProgram,
): MidgardCekStructuralExecution => {
  const prepared = prepareMidgardCekProgram(program.term, program.context);
  return executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: MAX_STEPS,
    executionBudget: program.tightBudget ? TIGHT_BUDGET : MIDGARD_CEK_TX_BUDGET,
  });
};

/** Indices of the steps a case adjudicates. */
const chooseSteps = (
  steps: readonly MidgardCekExecutionStep[],
  seed: number,
): readonly number[] => {
  const chosen = new Set<number>();
  const seenKinds = new Set<WitnessKind>();
  let executing = 0;
  steps.forEach((step, index) => {
    if (!seenKinds.has(step.witness.kind)) chosen.add(index);
    seenKinds.add(step.witness.kind);
    if (executedBuiltin(step) !== null && executing < MAX_EXECUTING_STEPS) {
      chosen.add(index);
      executing += 1;
    }
  });
  const rng = makeRng(seed);
  for (let pick = 0; pick < SAMPLED_ORDINARY_STEPS; pick += 1) {
    if (steps.length > 0) chosen.add(rng.int(steps.length));
  }
  return [...chosen].sort((left, right) => left - right);
};

const sampleOf = (
  step: MidgardCekExecutionStep,
  index: number,
  twist: string | null,
): SampledStep => ({
  index,
  kind: step.witness.kind,
  builtin: executedBuiltin(step),
  twist,
  arguments: stepArguments(step),
  postHash: hashMidgardCekMachineState(step.post).toString("hex"),
});

/** Runs a program and picks the steps to adjudicate. */
export const prepareCase = (
  program: CekStepFuzzProgram,
  sampleSeed: number,
): PreparedCase => {
  let execution: MidgardCekStructuralExecution;
  try {
    execution = runProgram(program);
  } catch (cause) {
    return {
      program,
      totalSteps: 0,
      stopReason: "threw",
      sampled: [],
      local: [`the builder threw: ${String(cause)}`],
    };
  }
  const steps = execution.steps;
  const local: string[] = [];
  const sampled = chooseSteps(steps, sampleSeed).map((index) => {
    const step = steps[index]!;
    if (!verifyMidgardCekCoreStep(step.pre, step.post, step.witness)) {
      local.push(`step ${index.toString()} fails the TypeScript verifier`);
    }
    return sampleOf(step, index, null);
  });
  if (steps.length > 0) {
    for (const twist of CEK_CORE_STEP_TWISTS) {
      const index = twist.select(steps);
      const twisted = twist.apply(steps[index]!);
      if (
        verifyMidgardCekCoreStep(twisted.pre, twisted.post, twisted.witness)
      ) {
        local.push(`twist ${twist.label} of step ${index.toString()} verifies`);
      }
      sampled.push(sampleOf(twisted, index, twist.label));
    }
  }
  return {
    program,
    totalSteps: steps.length,
    stopReason: execution.stopReason,
    sampled,
    local,
  };
};

/** One adjudicated step, with the Aiken machine's answer. */
export type AdjudicatedStep = SampledStep & {
  readonly verdict: AikenVerdict;
  readonly aikenPostHash: string | null;
};

export type CaseResult = {
  readonly prepared: PreparedCase;
  readonly steps: readonly AdjudicatedStep[];
  readonly divergences: readonly string[];
};

const divergenceOf = (step: AdjudicatedStep): string | null => {
  const at = `step ${step.index.toString()} (${step.kind})`;
  if (step.twist !== null) {
    return step.verdict === "accepted"
      ? `${at}: Aiken accepts twist ${step.twist}`
      : null;
  }
  if (step.verdict !== "accepted") return `${at}: Aiken ${step.verdict}`;
  return step.aikenPostHash === step.postHash
    ? null
    : `${at}: Aiken post hash ${String(step.aikenPostHash)}, TypeScript ${step.postHash}`;
};

/** Adjudicates prepared cases, batching every step into few evaluations. */
export const adjudicateCases = (
  oracle: AikenStepOracle,
  cases: readonly PreparedCase[],
): readonly CaseResult[] => {
  const all = cases.flatMap((entry) => entry.sampled);
  const verdicts = oracle.verify(all.map((step) => step.arguments));
  const genuine = all.filter((step) => step.twist === null);
  const hashes = oracle.hash(genuine.map((step) => step.arguments.post));
  const hashOf = new Map(genuine.map((step, index) => [step, hashes[index]]));
  let cursor = 0;
  return cases.map((prepared) => {
    const steps = prepared.sampled.map((step) => ({
      ...step,
      verdict: verdicts[cursor++] ?? "failed",
      aikenPostHash: hashOf.get(step) ?? null,
    }));
    return {
      prepared,
      steps,
      divergences: [
        ...prepared.local,
        ...steps
          .map(divergenceOf)
          .filter((detail): detail is string => detail !== null),
      ],
    };
  });
};
