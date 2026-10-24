import { Effect, Either } from "effect";

import { decodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import {
  type LocalScriptEvalResult,
  type LocalScriptEvaluation,
  RejectCodes,
  replayValidationVerdictEvent,
  type ValidationMachineReplayInput,
  ValidationTraceStopped,
} from "../../src/index.js";
import { plutusDataFromCborIterative } from "../../src/plutus-data-iterative.decode.js";
import { buildDeterministicValidationMachineTrace } from "../helpers/trace-commit-golden.js";
import { replayValidationMachineEvent } from "../validation-machine-event-replay.trace.js";
import {
  type PreparedCase,
  prepareExecution,
} from "./cek-step-fuzz.harness.js";
import {
  appliedProgramFlat,
  builtinTagsOf,
  type Reference,
} from "./tx-fuzz.reference.js";
import type { TxFuzzScenario } from "./tx-fuzz.scenario.js";

/**
 * The legs of one transaction case:
 *
 * - verdict: Phase A and B, the CEK executor in verdict mode;
 * - EQ: the full trace builder (proof trace, commit and open, cross-checked
 *   by `buildDeterministicValidationMachineTrace`), re-running Phase A and B
 *   itself, must build a trace whose verdict and code are the verdict leg's;
 * - EQ⁻: the same builder, handed the opposite classification, must stop on
 *   the disagreement;
 * - cost: proof mode spends exactly what verdict mode spends;
 * - reference (opt-in): the external evaluator's verdict and, on accept, its
 *   spent budget equal Phase B's;
 * - steps: the proof run's steps, sampled for the step oracle.
 *
 * Disagreements in a known class are counted under it, never as divergences.
 */

/**
 * Disagreements with an owner elsewhere, counted rather than failed. Each
 * names its owner and the fix that removes it; the class goes when that fix
 * composes. The CI rows pin every one to its seed (`tx-fuzz.pins.ts`).
 */
export type TxKnownClass =
  /**
   * A script naming a BLS builtin of flat tags 58–60 or 65–67, which the
   * reference leg skips. Owner: C1. Pending the C1 builtin tag numbering
   * (SH-2).
   */
  "blsTagNumbering";

/** The known classes only the opt-in reference leg reports. */
export const REFERENCE_LEG_CLASSES: ReadonlySet<TxKnownClass> = new Set([
  "blsTagNumbering",
]);

export type TxCaseResult = {
  readonly label: string;
  readonly verdict: "accepted" | "rejected" | "indeterminate";
  readonly rejectionCode: string | null;
  readonly divergences: readonly string[];
  readonly known: readonly TxKnownClass[];
  /** Whether the reference leg ran. */
  readonly referenced: boolean;
  /** The proof run's steps for the step oracle, when the script ran. */
  readonly steps: PreparedCase | null;
};

const CARDANO_RENUMBERED_BLS_TAGS = [58, 59, 60, 65, 66, 67];

const withoutCaptures = (
  input: ValidationMachineReplayInput,
): ValidationMachineReplayInput => {
  const { scriptEvaluations: _captures, ...rest } = input;
  return rest;
};

const sameResult = (
  left: LocalScriptEvalResult,
  right: LocalScriptEvalResult,
): boolean =>
  left.kind === right.kind &&
  (left.kind !== "accepted" ||
    (right.kind === "accepted" &&
      left.budget.cpu === right.budget.cpu &&
      left.budget.memory === right.budget.memory));

/**
 * The proof run reports a budget stop as its spend, for the caller to compare
 * with the declared budget; the verdict run stops out of budget itself. Both
 * modes agree when their verdicts under the declared budget agree, and, on
 * acceptance, their spends.
 */
const sameVerdict = (
  proof: LocalScriptEvaluation,
  fast: LocalScriptEvaluation,
): boolean =>
  scriptAccepted(proof)
    ? fast.result.kind === "accepted" && sameResult(proof.result, fast.result)
    : proof.result.kind === "indeterminate"
      ? fast.result.kind === "indeterminate"
      : fast.result.kind === "script_invalid";

/** Phase B's script verdict under the declared budget. */
const scriptAccepted = (evaluation: LocalScriptEvaluation): boolean =>
  evaluation.result.kind === "accepted" &&
  evaluation.executionBudget !== undefined &&
  evaluation.result.budget.cpu <= evaluation.executionBudget.cpu &&
  evaluation.result.budget.memory <= evaluation.executionBudget.memory;

const referenceLeg = (
  scenario: TxFuzzScenario,
  evaluation: LocalScriptEvaluation,
  reference: Reference,
  divergences: string[],
  known: TxKnownClass[],
): void => {
  const tags = builtinTagsOf(
    decodeMidgardCekCardanoFlatProgram(scenario.scriptFlat).body,
  );
  if (CARDANO_RENUMBERED_BLS_TAGS.some((tag) => tags.has(tag))) {
    known.push("blsTagNumbering");
    return;
  }
  const run = reference.run(
    appliedProgramFlat(scenario.scriptFlat, evaluation.contextCbor),
  );
  const midgard = scriptAccepted(evaluation);
  const inBudget =
    run.kind === "result" &&
    run.cpu <= scenario.exUnits.steps &&
    run.memory <= scenario.exUnits.memory;
  const referenceReturnsUnit =
    run.kind === "result" && run.result === "(con unit ())";
  const referenceAccepts = inBudget && referenceReturnsUnit;
  const phaseB = JSON.stringify(evaluation.result, (_key, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value,
  );
  if (midgard !== referenceAccepts) {
    divergences.push(
      `reference ${run.kind === "result" ? `${run.result.slice(0, 80)} at ${run.cpu.toString()}/${run.memory.toString()}` : run.kind}, Phase B ${midgard ? "accepts" : "rejects"} (${phaseB})`,
    );
    return;
  }
  if (
    midgard &&
    run.kind === "result" &&
    evaluation.result.kind === "accepted" &&
    (run.cpu !== evaluation.result.budget.cpu ||
      run.memory !== evaluation.result.budget.memory)
  ) {
    divergences.push(
      `reference spends ${run.cpu.toString()}/${run.memory.toString()}, Phase B (${phaseB})`,
    );
  }
};

/**
 * Runs one leg to its result or its stop. A leg that dies (a throw inside it,
 * such as the commit-mode golden check refusing what the proof build
 * accepted) stops like a failed one, so the case reports it as a divergence
 * and the row counts it instead of aborting.
 */
const settle = <A, E>(leg: Effect.Effect<A, E>) =>
  Effect.runPromise(
    Effect.either(Effect.catchAllDefect(leg, (defect) => Effect.fail(defect))),
  );

/** Runs every leg of one case. */
export const runTxCase = async (
  scenario: TxFuzzScenario,
  sampleSeed: number,
  reference: Reference | undefined,
): Promise<TxCaseResult> => {
  const divergences: string[] = [];
  const known: TxKnownClass[] = [];
  const verdictLeg = await settle(
    replayValidationVerdictEvent({
      ...scenario.event,
      cekExecutionMode: "verdict",
    }),
  );
  if (Either.isLeft(verdictLeg)) {
    divergences.push(`verdict leg stopped: ${String(verdictLeg.left)}`);
    return {
      label: scenario.label,
      verdict: "indeterminate",
      rejectionCode: null,
      divergences,
      known,
      referenced: false,
      steps: null,
    };
  }
  const verdict = verdictLeg.right;
  const rejectionCode = verdict.rejection?.code ?? null;
  const replay = await settle(replayValidationMachineEvent(scenario.event));
  let steps: PreparedCase | null = null;
  if (Either.isLeft(replay)) {
    divergences.push(`trace replay stopped: ${String(replay.left)}`);
  } else {
    const { replayInput } = replay.right;
    const independent = await settle(
      buildDeterministicValidationMachineTrace(withoutCaptures(replayInput)),
    );
    if (Either.isLeft(independent)) {
      divergences.push(
        `EQ: trace builder stopped: ${String(independent.left)}`,
      );
    } else if (
      independent.right.verdict !== verdict.verdict ||
      independent.right.rejectionCode !== rejectionCode
    ) {
      divergences.push(
        `EQ: trace ${independent.right.verdict}/${String(independent.right.rejectionCode)}, verdict leg ${verdict.verdict}/${String(rejectionCode)}`,
      );
    }
    const flipped: ValidationMachineReplayInput = {
      ...withoutCaptures(replayInput),
      expectedVerdict: verdict.verdict === "accepted" ? "rejected" : "accepted",
      expectedRejectionCode:
        verdict.verdict === "accepted" ? RejectCodes.PlutusScriptInvalid : null,
    };
    const opposite = await settle(
      buildDeterministicValidationMachineTrace(flipped),
    );
    if (
      Either.isRight(opposite) ||
      !(opposite.left instanceof ValidationTraceStopped) ||
      opposite.left.reason !== "disagreement"
    ) {
      divergences.push(
        `EQ⁻: the opposite classification ${Either.isRight(opposite) ? "built a trace" : `stopped otherwise: ${String(opposite.left)}`}`,
      );
    }
    const proofEvaluations = replayInput.scriptEvaluations ?? [];
    if (proofEvaluations.length !== verdict.scriptEvaluations.length) {
      divergences.push("proof and verdict modes ran different scripts");
    }
    proofEvaluations.forEach((proof, index) => {
      const fast = verdict.scriptEvaluations[index];
      if (fast !== undefined && !sameVerdict(proof, fast)) {
        divergences.push(
          `script ${index.toString()}: proof mode ${proof.result.kind}, verdict mode ${fast.result.kind}, or their budgets differ`,
        );
      }
    });
    const execution = proofEvaluations[0]?.execution;
    if (execution != null) {
      steps = prepareExecution(
        {
          label: scenario.label,
          term: decodeMidgardCekCardanoFlatProgram(scenario.scriptFlat).body,
          context: plutusDataFromCborIterative(
            proofEvaluations[0]!.contextCbor,
          ),
          tightBudget: false,
        },
        execution,
        sampleSeed,
      );
    }
  }
  const evaluation = verdict.scriptEvaluations[0];
  const referenced = reference !== undefined && evaluation !== undefined;
  if (referenced) {
    referenceLeg(scenario, evaluation, reference, divergences, known);
  }
  return {
    label: scenario.label,
    verdict: verdict.verdict,
    rejectionCode,
    divergences,
    known,
    referenced,
    steps,
  };
};
