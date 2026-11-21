/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip: with
 * both threads of our case closed and each staging output still live,
 * another prover removes the block against its own record, and our runner
 * is restarted there.
 *
 * A fresh process asks first for what the process that was not stopped
 * asks for (support/validation-trace-dispute-restart-probe.ts); a fresh
 * process then runs our case to its end: each staging output is spent
 * back to our wallet by exactly one transaction the ledger took, no thread
 * is Inited and no second bond locked, and the run ends resolved by their
 * record, waiting on no spend back's finality: the process follows each
 * spend back until it is final, so once each spend back rolls back, our
 * next runs send it again, the same signed bytes, sign nothing new for the
 * outputs, and end resolved by their record again. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../src/validation-trace-dispute/one-step-staging-journal.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { liveCase } from "./support/validation-trace-dispute-one-step-case.js";
import {
  type OurRun,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import {
  expectSameNextAfterRestart,
  recordingAsks,
} from "./support/validation-trace-dispute-restart-probe.js";

const other = stagedOtherRecord();
const { drive } = other;
const seen: string[] = [];

const CASE_LIVE = "spend backs held while our case is live";

/** Holds each spend back our workflow would take, as a wait, while our case
 * is live on L1: what `other.holding` holds until our close is confirmed,
 * read from the chain. */
const holdingWhileCaseLive = (
  adapter: FraudProofFamilyWorkflowAdapter,
): FraudProofFamilyWorkflowAdapter => ({
  ...adapter,
  observe: async (context) => {
    const observed = await adapter.observe(context);
    if (
      observed.kind !== "action_required" ||
      observed.action.input.schemaVersion !== VALIDATION_TRACE_STAGING_RECLAIM
    )
      return observed;
    const live = await liveCase(drive.dispute()).then(
      () => true,
      () => false,
    );
    return live ? { kind: "pending", reason: CASE_LIVE } : observed;
  },
});

describe("validation-trace dispute runner restarted once another prover removes the block against its own record, at a staged step", () => {
  beforeAll(async () => {
    await other.setUp();
    drive.restart({
      wrap: (adapter) => other.holding(recordingAsks(seen)(adapter)),
    });
  }, 1_800_000);

  afterAll(other.tearDown);

  it("asks first, as a fresh process, for what the process that was not stopped asks for, and as a fresh process spends each staging output back once, ends resolved by their record, and spends the outputs back again, the same bytes, once those spend backs roll back", async () => {
    const stagings = await other.playToClose();
    const theirRecord = await other.removeByTheirs();
    const { scene, prepared } = drive.dispute();
    await expectSameNextAfterRestart({
      processOver: drive.processOver,
      runner: drive.dispute().runner,
      seen,
      block: scene.failing,
      opened: true,
    });
    drive.restart({ wrap: holdingWhileCaseLive });
    const runs: OurRun[] = [];
    const resolved = await other.runToResolution(stagings, runs);
    expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
    const sends = other.spendBackSends();
    for (const outRef of stagings) {
      const { hash } = await other.expectSpentBack(outRef);
      expect([...(sends.get(outRef) ?? [])], outRef).toStrictEqual([hash]);
    }
    for (const stage of ["init", "open", "verify_source"] as const)
      expect(
        prepared.filter((move) => move.stage === stage),
        stage,
      ).toHaveLength(1);
    await other.spentBackAgainAfterRollback();
  }, 1_800_000);
});
