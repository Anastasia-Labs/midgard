/** The validation-trace dispute runner when a stage of our case outside the
 * threads does not build, on the emulator. Init, Open and VerifySource
 * each raise the move-unbuilt alarm naming the stage, the block and the end
 * of the case open window, `block_end + M/2`; the case's close raises it
 * naming when the operator's bond unlocks, `started_at + M - M/2`. None is
 * sent; once the stage builds, the runner sends it. A stage the chain moved
 * under waits on the chain, without the alarm. */
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTraceStage } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_MOVE_UNBUILT } from "../src/validation-trace-dispute/threads.js";
import {
  validationTraceDefenseDeadlineMs,
  validationTraceRunState,
} from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { awaitLastSlotAtOrBefore } from "./support/validation-trace-dispute-one-step-case.js";
import { withLedgerParameters } from "./support/validation-trace-dispute-one-step-play.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  type Runner,
  runnerDeployment,
} from "./support/validation-trace-dispute-runner.js";

/** A transaction size limit under which none of the stages here builds. */
const UNBUILDABLE = 500;

let scene: Scene;
let runner: Runner;
/** The end of the case open window on the challenged block. */
let openWindowEnd: number;

const reasonOf = (result: FraudProofWorkflowRunResult) => {
  if (result.kind !== "pending") throw new Error("the run is not pending");
  return result.reason;
};

/** The intents the runner journaled for `stage`. */
const intentsFor = (
  result: FraudProofWorkflowRunResult,
  stage: ValidationTraceStage,
) =>
  "entries" in result
    ? result.entries.filter(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.actionId.startsWith(
            `validation-trace-${stage.replaceAll("_", "-")}:`,
          ),
      )
    : [];

/** Runs the runner under the unbuildable size limit until it waits under
 * the move-unbuilt alarm; returns the run. */
const runUnbuilt = async () =>
  await withLedgerParameters(scene, { maxTxSize: UNBUILDABLE }, async () => {
    const history: string[] = [];
    for (let attempt = 0; attempt < 4; attempt += 1) {
      const result = await runner.runOnce(scene.failing);
      history.push("reason" in result ? result.reason : result.kind);
      if (
        result.kind === "pending" &&
        result.reason.startsWith(`${VALIDATION_TRACE_MOVE_UNBUILT}: `)
      )
        return result;
    }
    throw new Error(`no move-unbuilt alarm:\n${history.join("\n")}`);
  });

/** Asserts `result` raises the move-unbuilt alarm for `stage`, which must
 * land by `deadline`, named `what`, and that it sent no `stage`. */
const expectAlarm = (
  result: FraudProofWorkflowRunResult,
  stage: ValidationTraceStage,
  deadline: number,
  what: string,
) => {
  expect(reasonOf(result)).toContain(
    `${VALIDATION_TRACE_MOVE_UNBUILT}: the ${stage} move of our case on block ${scene.failing.evidence.headerHash} does not build, and must land by ${deadline.toString()}, ${what}: `,
  );
  const runState = validationTraceRunState(result);
  expect("namedState" in runState && runState.namedState).toStrictEqual({
    name: VALIDATION_TRACE_MOVE_UNBUILT,
    alarm: true,
  });
  expect(intentsFor(result, stage)).toStrictEqual([]);
};

/** One run under the ledger's own limit, which sends `stage`. */
const sends = async (stage: ValidationTraceStage) => {
  const result = await runner.runOnce(scene.failing);
  expect(intentsFor(result, stage)).toHaveLength(1);
};

const WINDOW = "the end of the case open window";

describe("validation-trace dispute runner: our init, open, verify_source and close, when they do not build", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    runner = makeRunner(scene);
    const { h } = scene;
    const unit =
      h.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      scene.failing.evidence.headerHash;
    const [block] = await h.proverLucid.utxosAtWithUnit(
      h.contracts.stateQueue.spendingScriptAddress,
      unit,
    );
    const { blockTime } = await Effect.runPromise(
      SDK.validationDisputeChallengedBlock(
        block!,
        h.contracts.stateQueue.policyId,
      ),
    );
    openWindowEnd = Number(blockTime) + MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS;
  }, 1_800_000);

  it("raises no alarm when the chain moved under a stage: a challenged block the init no longer finds leaves the run waiting on the chain, not on an unbuilt move", async () => {
    const deployment = runnerDeployment(scene);
    // The block is looked for where no state-queue node is.
    const moved = makeRunner(scene, {
      deployment: {
        ...deployment,
        stateQueue: {
          ...deployment.stateQueue,
          spendingScriptAddress: scene.chain.case.spendingScriptAddress,
        },
      },
    });
    const result = await moved.runOnce(scene.failing);
    expect(reasonOf(result)).toContain(
      `validation-trace challenged block ${scene.failing.evidence.headerHash} is not on chain`,
    );
    expect(reasonOf(result)).not.toContain(VALIDATION_TRACE_MOVE_UNBUILT);
    const runState = validationTraceRunState(result);
    expect("namedState" in runState && runState.namedState).toBeFalsy();
    expect(intentsFor(result, "init")).toStrictEqual([]);
  }, 600_000);

  it("raises the move-unbuilt alarm when our init does not build, naming the end of the case open window it feeds, and sends it once it builds", async () => {
    expectAlarm(await runUnbuilt(), "init", openWindowEnd, WINDOW);
    await sends("init");
  }, 600_000);

  it("raises the move-unbuilt alarm when our open does not build, naming the end of the case open window, and sends it once it builds", async () => {
    expectAlarm(await runUnbuilt(), "open", openWindowEnd, WINDOW);
    await sends("open");
  }, 600_000);

  it("raises the move-unbuilt alarm when our verify_source does not build, naming the end of the case open window, and sends it once it builds", async () => {
    expectAlarm(await runUnbuilt(), "verify_source", openWindowEnd, WINDOW);
    await sends("verify_source");
  }, 600_000);

  it("raises the move-unbuilt alarm when the close of our undefended case does not build, naming when the operator's bond unlocks, and sends it once it builds", async () => {
    const { h } = scene;
    // The VerifySource confirms, and the case waits out its defense.
    const [caseUtxo] = await h.proverLucid.utxosAt(
      scene.chain.case.spendingScriptAddress,
    );
    const caseDatum = Data.from(caseUtxo!.datum!, SDK.Case);
    const started = Number(caseDatum.started_at);
    awaitLastSlotAtOrBefore(
      scene,
      validationTraceDefenseDeadlineMs(caseDatum) + 1_000,
    );
    expectAlarm(
      await runUnbuilt(),
      "close",
      started +
        MIDGARD_CONSENSUS_LIMITS.blockMaturityMs -
        MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS,
      "the operator's bond unlock",
    );
    await sends("close");
  }, 600_000);
});
