/** The validation-trace dispute runner waiting on its counterparty, on the
 * emulator: once our case is open and undefended, a run reports
 * `awaiting_counterparty` with the case's on-chain defense deadline, and a
 * session hands its turn back at once instead of polling; once the operator
 * defends, the run awaits the defender's move on the thread the Defend
 * opened, until the defender's clock ends. */
import {
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { awaitingValidationTraceCounterparty } from "../src/validation-trace-dispute/waiting.js";
import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { continuePendingWorkflow } from "../src/workflow/pending-continuation.js";
import { defendAsOperator } from "./support/validation-trace-dispute-defend.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import { DEPLOYMENT_FINGERPRINT } from "./workflow.make-adapter.js";

let scene: Scene;
let runner: Runner;
let caseOutRef: string;

const liveCase = async (): Promise<UTxO> => {
  const [found] = await scene.h.proverLucid.utxosByOutRef([
    parseOwnOutRef(caseOutRef),
  ]);
  if (found === undefined) throw new Error("the case is not live");
  return found;
};

describe("validation-trace dispute runner: waiting on the counterparty", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    runner = makeRunner(scene);
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const verify = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    );
    caseOutRef = verify!.outputs[0]!.outRef;
  }, 1_800_000);

  it("reports an undefended case of ours as awaiting its counterparty until the on-chain defense deadline", async () => {
    const result = awaitingValidationTraceCounterparty(
      await runner.runOnce(scene.failing),
    );
    const caseDatum = Data.from((await liveCase()).datum!, SDK.Case);
    expect(caseDatum.open_threads).toBe(0n);
    expect(result).toMatchObject({
      kind: "awaiting_counterparty",
      outRef: caseOutRef,
      responseDeadline:
        Number(caseDatum.started_at) +
        MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS +
        MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
    });
  });

  // A session that kept polling would run to the 30-minute quiet bound.
  it("hands the session's turn back at once while it awaits, without a continuation stall", async () => {
    const { h } = scene;
    const before = h.emulator.now();
    h.emulator.awaitSlot(31 * 60);
    expect(h.emulator.now() - before).toBeGreaterThan(30 * 60_000);
    let runs = 0;
    const result = await continuePendingWorkflow({
      invocation: {
        mode: "run",
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        category: "validationTraceDispute",
        headerHash: scene.failing.evidence.headerHash,
      },
      journal: runner.journal,
      execute: async () => {
        runs += 1;
        return awaitingValidationTraceCounterparty(
          await runner.runOnce(scene.failing),
        );
      },
    });
    expect(runs).toBe(1);
    expect(result).toMatchObject({
      kind: "awaiting_counterparty",
      outRef: caseOutRef,
    });
    if (!("reason" in result)) throw new Error("no reason");
    expect(result.reason).not.toContain(
      FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
    );
  }, 120_000);

  it("awaits the defender's move on the thread once the operator's Defend lands, until the defender's clock ends", async () => {
    const defense = await defendAsOperator(scene, await liveCase());
    const result = awaitingValidationTraceCounterparty(
      await runner.runOnce(scene.failing),
    );
    const [thread] = await scene.h.proverLucid.utxosByOutRef([
      parseOwnOutRef(`${defense}#1`),
    ]);
    const datum = Data.from(thread!.datum!, SDK.CaseThreadDatum).data!;
    expect(result).toMatchObject({
      kind: "awaiting_counterparty",
      outRef: `${defense}#1`,
      responseDeadline: Number(datum.turn_started_at + datum.defender_clock_ms),
    });
  });
});
