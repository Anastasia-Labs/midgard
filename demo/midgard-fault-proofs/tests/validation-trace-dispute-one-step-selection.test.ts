/** The validation-trace dispute runner on a case where one defender's thread
 * comes to a CEK script selection, on the emulator. The runner resolves and
 * closes the operator's thread, whose step is a CEK compute step; the
 * successor's thread reaches ReadyForOneStep on the selection, whose
 * resolution needs the selected script's decoded record, and there the
 * runner waits under one-step-required, naming the step and what it needs,
 * and never enters its resolution. */
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../src/validation-trace-dispute/threads.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  defendTwoThreads,
  gameThread,
  SELECTION_STEP_FROM,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  isPendingFor,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import { threadOf } from "./support/validation-trace-dispute-scripted-defender.js";

let dispute: TwoThreadCase;
let waiting: FraudProofWorkflowRunResult;

const reasonOf = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? result.reason : "";

describe("validation-trace dispute runner: a thread on a CEK script selection", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: false,
      successorFrom: SELECTION_STEP_FROM,
    });
    const { scene, runner, a, b } = dispute;
    ({ result: waiting } = await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async (run) =>
        isPendingFor(run, VALIDATION_TRACE_ONE_STEP_REQUIRED) &&
        (await threadClosed(dispute, a.unit)) &&
        threadOf(await gameThread(scene, b.unit)).turn === "ReadyForOneStep",
      limit: 60,
    }));
  }, 1_800_000);

  it("resolves and closes the thread it drives", () => {
    const stages = dispute.prepared.map(({ stage }) => stage);
    expect(stages.filter((stage) => stage === "close_awarded")).toHaveLength(1);
  });

  it("waits under one-step-required naming the step and the decoded record it needs, and never enters that thread's resolution", async () => {
    const { scene, runner, b, prepared } = dispute;
    expect(reasonOf(waiting)).toContain(
      `between states ${SELECTION_STEP_FROM.toString()} and ${(SELECTION_STEP_FROM + 1).toString()}`,
    );
    expect(reasonOf(waiting)).toContain(
      "requires the selected script's authenticated decoded record",
    );
    // One entry into a resolution: the operator's thread.
    expect(
      prepared.filter(({ stage }) => stage === "enter_resolution"),
    ).toHaveLength(1);
    const thread = await gameThread(scene, b.unit);
    const before = prepared.length;
    const again = await runner.runOnce(scene.failing);
    expect(isPendingFor(again, VALIDATION_TRACE_ONE_STEP_REQUIRED)).toBe(true);
    expect(prepared.slice(before)).toStrictEqual([]);
    expect(await gameThread(scene, b.unit)).toStrictEqual(thread);
  }, 300_000);
});
