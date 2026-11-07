/** The validation-trace dispute runner at a phase step its deployment does
 * not yet drive: the thread waits at ReadyForOneStep under the
 * one-step-required alarm, which names the thread, the step and the
 * deadline its entry must land by, and the run keeps going without ever
 * entering the step's resolution. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTracePhase } from "../../src/validation-trace-dispute/one-step-phase.js";
import {
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  validationTraceCaseExpiryMs,
  validationTraceChallengerDeadlineMs,
} from "../../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  defendTwoThreads,
  liveCase,
  type TwoThreadCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  isPendingFor,
  playThreads,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
import { threadOf } from "./validation-trace-dispute-scripted-defender.js";

type Installs = Pick<
  Parameters<typeof defendTwoThreads>[0],
  "failingNativeMint" | "failingNativeSpend"
>;

/** Whether `result` waits under the one-step-required state on the thread
 * holding `unit`. */
const waitsOn = (result: FraudProofWorkflowRunResult, unit: string) =>
  isPendingFor(result, VALIDATION_TRACE_ONE_STEP_REQUIRED) &&
  "reason" in result &&
  result.reason.includes(`thread ${unit} at `);

/**
 * The rows of the step of `phase` after state `step` of the failing
 * transaction `installs` shapes, whose semantic resolver `semantic` the
 * deployment does not yet drive; `role` names the step in the rows.
 */
export const describeUndrivenStep = ({
  role,
  phase,
  semantic,
  step,
  installs = {},
}: {
  readonly role: string;
  readonly phase: ValidationTracePhase;
  readonly semantic: number;
  readonly step: number;
  readonly installs?: Installs;
}) =>
  describe(`validation-trace dispute runner: the ${role} step of ${phase}, not yet driven`, () => {
    let dispute: TwoThreadCase;
    let waited: FraudProofWorkflowRunResult;

    beforeAll(async () => {
      dispute = await defendTwoThreads({
        withRemoval: false,
        successorFrom: step,
        phases: true,
        ...installs,
      });
      const { scene, runner, a, b } = dispute;
      ({ result: waited } = await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async (run) => waitsOn(run, b.unit),
        limit: 80,
      }));
    }, 1_800_000);

    it("raises the one-step-required alarm on the thread, naming the step and the deadline its entry must land by", async () => {
      const { scene, deployment, b } = dispute;
      const place = await threadPlace(scene, deployment, b.unit);
      expect(place!.holder).toBe("game");
      const thread = threadOf(place!.utxo);
      expect(thread.turn).toBe("ReadyForOneStep");
      expect(thread.low_index).toBe(BigInt(step));
      const caseDatum = Data.from((await liveCase(dispute)).datum!, SDK.Case);
      const deadline = Math.min(
        validationTraceCaseExpiryMs(caseDatum),
        validationTraceChallengerDeadlineMs(thread),
      );
      const state = validationTraceRunState(waited);
      expect("namedState" in state && state.namedState?.alarm).toBe(true);
      expect("reason" in waited && waited.reason).toContain(
        `thread ${b.unit} at ${place!.utxo.txHash}#${place!.utxo.outputIndex.toString()} is ReadyForOneStep between states ${step.toString()} and ${(step + 1).toString()}, and must enter its resolution by ${deadline.toString()}; the step from state ${step.toString()} is a ${phase} step for semantic resolver ${semantic.toString()}, whose resolution the runner does not drive, so it waits`,
      );
    }, 300_000);

    it("keeps running under the alarm and never enters the step's resolution", async () => {
      const { scene, deployment, runner, prepared, a, b } = dispute;
      const before = await threadPlace(scene, deployment, b.unit);
      const { result } = await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async (run) => waitsOn(run, b.unit),
        limit: 1,
      });
      expect(waitsOn(result, b.unit)).toBe(true);
      const after = await threadPlace(scene, deployment, b.unit);
      expect(after!.utxo.txHash).toBe(before!.utxo.txHash);
      expect(after!.utxo.outputIndex).toBe(before!.utxo.outputIndex);
      expect(
        prepared.filter(
          ({ stage, consumes }) =>
            stage === "enter_resolution" &&
            consumes ===
              `${before!.utxo.txHash}#${before!.utxo.outputIndex.toString()}`,
        ),
      ).toStrictEqual([]);
    }, 300_000);
  });
