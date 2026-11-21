/**
 * The opened trace's `steps`: a replay of the same transaction on the script
 * evaluations the opening ran. It runs no evaluator and keeps exactly the
 * opened trace, and a replay on supplied evaluations refuses any evaluation
 * its own run does not reproduce. The evaluator is counted at the pinned
 * UPLC evaluator.
 */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import { Effect, Either } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  commitValidationTraceV1,
  openValidationTraceV1,
  type ValidationMachineReplayInput,
  validationTraceStateHashV1,
} from "../src/index.js";
import type { LocalScriptEvaluation } from "../src/types.js";
import { replayValidationTrace } from "../src/validation-machine/trace-builder.js";
import { HashingValidationTraceSink } from "../src/validation-machine/trace-sink.js";
import { plutusV3SpendInput } from "./helpers/plutus-spend-input.js";
import {
  claimedRejected,
  failingProgram,
} from "./phase-b.indeterminate.fixture.js";

const evaluator = vi.hoisted(() => ({ runs: 0 }));

vi.mock("@lucid-evolution/uplc", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@lucid-evolution/uplc")>();
  return {
    ...actual,
    eval_program_raw: (
      ...args: Parameters<typeof actual.eval_program_raw>
    ): ReturnType<typeof actual.eval_program_raw> => {
      evaluator.runs += 1;
      return actual.eval_program_raw(...args);
    },
  };
});

describe("openValidationTraceV1: steps replay on the opening's script evaluations", () => {
  let input: ValidationMachineReplayInput;

  beforeAll(async () => {
    input = claimedRejected(await plutusV3SpendInput(failingProgram()));
  });

  it("runs the evaluator at the opening only, and keeps exactly the opened trace", async () => {
    evaluator.runs = 0;
    const opened = await Effect.runPromise(
      openValidationTraceV1(input, { keepWitnesses: () => true }),
    );
    expect(evaluator.runs).toBe(1);
    const steps = await Effect.runPromise(opened.steps(() => true));
    expect(evaluator.runs).toBe(1);
    expect(
      new Map(
        Object.entries(steps.witnesses).map(([index, witness]) => [
          Number(index),
          witness,
        ]),
      ),
    ).toStrictEqual(opened.witnesses);
    expect(Object.keys(steps.states)).toHaveLength(opened.stepCount + 1);
    for (let index = 0; index <= opened.stepCount; index += 1)
      expect(hashMidgardValidationMachineState(steps.states[index]!)).toEqual(
        validationTraceStateHashV1(opened, index),
      );
  });

  it("refuses a script evaluation its own run does not reproduce", async () => {
    const { scriptEvaluations } = await Effect.runPromise(
      replayValidationTrace(input, {
        mode: "commit",
        open: (frame) => new HashingValidationTraceSink(frame, () => undefined),
      }),
    );
    expect(scriptEvaluations).toHaveLength(1);
    const [evaluation] = scriptEvaluations;
    const commitOn = async (captures: readonly LocalScriptEvaluation[]) =>
      await Effect.runPromise(
        Effect.either(
          commitValidationTraceV1({ ...input, scriptEvaluations: captures }),
        ),
      );
    const fresh = await Effect.runPromise(
      Effect.either(commitValidationTraceV1(input)),
    );
    const reused = await commitOn(scriptEvaluations);
    expect(Either.isRight(fresh) && Either.isRight(reused)).toBe(true);
    if (Either.isRight(fresh) && Either.isRight(reused))
      expect(reused.right).toStrictEqual(fresh.right);
    const refusal = async (
      captures: readonly LocalScriptEvaluation[],
    ): Promise<string> => {
      const committed = await commitOn(captures);
      return Either.isLeft(committed) ? committed.left.message : "accepted";
    };
    expect(
      await refusal([
        { ...evaluation!, executionIndex: evaluation!.executionIndex + 1n },
      ]),
    ).toContain("cannot reuse the evaluator capture");
    expect(await refusal([])).toContain("cannot reuse the evaluator capture");
    expect(await refusal([evaluation!, evaluation!])).toContain(
      "did not consume every evaluator capture",
    );
  });
});
