import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  GENERIC_JOURNEY_PLAN,
  genericJourneyTiming,
  healthyJourneyReplayTiming,
  journeyTimingForCategory,
  readTransitionTraceJourneyExecutionTiming,
  readTransitionTraceJourneyTiming,
  requireHealthyReplayFitsDeadline,
  TRANSITION_TRACE_JOURNEY_PLAN,
  transitionTraceJourneyExecutionTiming,
  transitionTraceJourneyTiming,
  verifyTransitionTraceJourneyOutputPlan,
} from "./journey-timing.js";

const preprod = {
  slotLengthSeconds: 1,
  activeSlotsCoeff: 0.05,
  confirmationDepth: 30,
};

describe("single-deposit transition-trace journey timing", () => {
  it("counts the finite proof path and confirms its actual output fold shape", () => {
    // Published fixture: enterprise key address, 10 ADA, no assets or inline datum.
    const output = Buffer.from(
      `a200581d60${"11".repeat(28)}01821a00989680a0`,
      "hex",
    );
    const plan = verifyTransitionTraceJourneyOutputPlan(output);
    expect(plan).toBe(TRANSITION_TRACE_JOURNEY_PLAN);
    expect(plan.checkpointPhases).toEqual([6, 0, 2, 7, 10, 8, 3, 9, 4, 5]);
    expect(
      Math.ceil(plan.scanPrimitiveSteps / plan.scanStepsPerTransaction),
    ).toBe(1);
    expect(
      Math.ceil(plan.valuePrimitiveSteps / plan.valueStepsPerTransaction),
    ).toBe(1);
    const initAndRoute = 2;
    const checkpoints = plan.checkpointPhases.length - 1;
    const terminalAndRemoval = 2;
    expect(
      initAndRoute +
        plan.proofPreimagePublications +
        plan.outputPreimagePublications +
        checkpoints +
        terminalAndRemoval,
    ).toBe(15);
    expect(plan.dependentTransactions).toBe(15);
  });

  it("refuses output shape drift instead of silently retaining the old count", () => {
    const smallerValue = Buffer.from(
      `a200581d60${"11".repeat(28)}018201a0`,
      "hex",
    );
    expect(() => verifyTransitionTraceJourneyOutputPlan(smallerValue)).toThrow(
      "audited journey timing plan",
    );
  });

  it("budgets actual block cadence and keeps the whole journey beyond correction", () => {
    const timing = transitionTraceJourneyTiming(preprod);
    expect(timing.expectedConfirmationMs).toBe(150 * 60_000);
    expect(timing.confirmationAllowanceMs).toBe(300 * 60_000);
    expect(timing.correctionTimeoutMs).toBe(20_250_000);
    expect(timing.journeyTimeoutMs).toBe(30_210_030);
    expect(timing.plan.successorTransactions).toBe(4);
    expect(timing.allowances.successorRegistrationMs).toBe(30);
    expect(timing.cadence.confirmationDepth).toBe(30);
    // Absent an explicit action depth, every stage keeps waiting for release
    // finality, which is the behavior before inclusion-gated actions.
    expect(timing.cadence.actionDepth).toBe(30);
  });

  it("budgets action stages from the action depth and the stamp from finality", () => {
    const inclusion = transitionTraceJourneyTiming({
      ...preprod,
      actionDepth: 1,
    });
    const finality = transitionTraceJourneyTiming(preprod);
    expect(inclusion.cadence.actionDepth).toBe(1);
    expect(inclusion.cadence.confirmationDepth).toBe(30);
    // One block at f=0.05 and one-second slots: twenty seconds per inclusion.
    expect(inclusion.expectedConfirmationMs).toBe(15 * 20_000);
    expect(inclusion.transactionAllowanceMs).toBe(40_000 + 120_000 + 30_000);
    expect(inclusion.correctionTimeoutMs).toBe(15 * 190_000);
    // The single release-finality window is unchanged by the action depth.
    expect(inclusion.allowances.finalizedEvidenceStampMs).toBe(1_230_000);
    expect(inclusion.allowances).toEqual(finality.allowances);
    expect(inclusion.journeyTimeoutMs).toBeLessThan(finality.journeyTimeoutMs);
    expect(
      inclusion.journeyTimeoutMs -
        inclusion.allowances.finalizedEvidenceStampMs,
    ).toBeGreaterThan(0);
  });

  it("refuses an action depth outside the release finality window", () => {
    for (const actionDepth of [0, 1.5, 31, Number.NaN])
      expect(() =>
        transitionTraceJourneyTiming({ ...preprod, actionDepth }),
      ).toThrow("Invalid journey cadence");
  });

  it("scales with slot duration, block probability, and finality independently", () => {
    const baseline =
      transitionTraceJourneyTiming(preprod).confirmationAllowanceMs;
    expect(
      transitionTraceJourneyTiming({ ...preprod, slotLengthSeconds: 2 })
        .confirmationAllowanceMs,
    ).toBe(2 * baseline);
    expect(
      transitionTraceJourneyTiming({ ...preprod, activeSlotsCoeff: 0.1 })
        .confirmationAllowanceMs,
    ).toBe(baseline / 2);
    expect(
      transitionTraceJourneyTiming({ ...preprod, confirmationDepth: 60 })
        .confirmationAllowanceMs,
    ).toBe(2 * baseline);
    expect(
      transitionTraceJourneyTiming({ ...preprod, actionDepth: 15 })
        .confirmationAllowanceMs,
    ).toBe(baseline / 2);
    const staged = transitionTraceJourneyTiming({
      ...preprod,
      fixtureStagingAllowanceMs: 123_000,
    });
    expect(staged.journeyTimeoutMs).toBe(
      transitionTraceJourneyTiming(preprod).journeyTimeoutMs + 123_000,
    );
    expect(staged.correctionTimeoutMs).toBe(
      transitionTraceJourneyTiming(preprod).correctionTimeoutMs,
    );
  });

  it.each([
    { activeSlotsCoeff: 0 },
    { activeSlotsCoeff: 1.01 },
    { activeSlotsCoeff: Number.NaN },
    { slotLengthSeconds: 0 },
    { slotLengthSeconds: Number.POSITIVE_INFINITY },
    { confirmationDepth: 0 },
    { confirmationDepth: 1.5 },
    { fixtureStagingAllowanceMs: -1 },
  ])("rejects invalid cadence or allowance %j", (invalid) => {
    expect(() =>
      transitionTraceJourneyTiming({ ...preprod, ...invalid }),
    ).toThrow("Invalid journey cadence");
  });

  it("rejects timer overflow instead of turning a long deadline into an immediate one", () => {
    expect(() =>
      transitionTraceJourneyTiming({ ...preprod, activeSlotsCoeff: 0.000001 }),
    ).toThrow("supported timer range");
  });

  it("refuses unverified public manifests before deriving a top-level timer", async () => {
    const directory = await mkdtemp(join(tmpdir(), "journey-timing-"));
    try {
      await mkdir(join(directory, "genesis"));
      await mkdir(join(directory, "deploymentInfo"));
      await writeFile(
        join(directory, "genesis/shelley-genesis.json"),
        JSON.stringify({ slotLength: 1, activeSlotsCoeff: 0.05 }),
      );
      await writeFile(
        join(directory, "deploymentInfo/manifest.json"),
        JSON.stringify({ l1Finality: { confirmationDepth: 30 } }),
      );
      await expect(
        readTransitionTraceJourneyTiming(directory),
      ).rejects.toThrow();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
    await expect(
      readTransitionTraceJourneyTiming("relative-run"),
    ).rejects.toThrow("must be absolute");
  });
});

describe("generic journey timing for families without an audited plan", () => {
  it("budgets the same cadence formula over the finite generic transaction bound", () => {
    const timing = genericJourneyTiming(preprod);
    const audited = transitionTraceJourneyTiming(preprod);
    expect(timing.plan).toBe(GENERIC_JOURNEY_PLAN);
    expect(GENERIC_JOURNEY_PLAN.dependentTransactions).toBeGreaterThan(
      TRANSITION_TRACE_JOURNEY_PLAN.dependentTransactions,
    );
    // Two finality windows plus build and RPC for one dependent transaction.
    expect(timing.transactionAllowanceMs).toBe(1_350_000);
    expect(timing.transactionAllowanceMs).toBe(audited.transactionAllowanceMs);
    expect(timing.correctionTimeoutMs).toBe(24 * 1_350_000);
    expect(timing.journeyTimeoutMs).toBe(42_360_030);
    expect(timing.journeyTimeoutMs).toBeGreaterThan(audited.journeyTimeoutMs);
    expect(timing.allowances).toEqual(audited.allowances);
  });

  it("keeps the audited plan for transitionTrace and uses the generic plan elsewhere", () => {
    expect(journeyTimingForCategory("transitionTrace", preprod).plan).toBe(
      TRANSITION_TRACE_JOURNEY_PLAN,
    );
    expect(journeyTimingForCategory("zeroInput", preprod)).toEqual(
      genericJourneyTiming(preprod),
    );
    expect(
      journeyTimingForCategory("executionNativeScriptInvalid", preprod).plan,
    ).toBe(GENERIC_JOURNEY_PLAN);
  });
});

describe("fixed native backlog replay allowance", () => {
  const workload = {
    ...preprod,
    tipBlockNo: 1700,
    beforeReplayAllowanceMs: 0,
    observationAllowanceMs: 90_000,
  };

  it("includes the entire origin prefix and arrivals while the serial replay drains", () => {
    const plan = healthyJourneyReplayTiming(workload);
    expect(plan.replayOrigin).toBe("origin");
    expect(plan.initialBacklogBlocks).toBe(1701);
    expect(plan.replayUtilization).toBe(0.5);
    expect(plan.timeoutMs).toBe(34_220_000);
    expect(plan.timeoutMs).toBeGreaterThanOrEqual(
      (plan.initialBacklogBlocks + plan.blocksDuringReplay) *
        plan.blockAllowanceMs +
        plan.observationAllowanceMs,
    );
    // Sparse finality receipts cannot prove how many expensive callbacks remain.
    expect(plan.initialBacklogBlocks).toBeGreaterThan(1700 - 750);
  });

  it("includes new blocks during the full proof and successor window before replay", () => {
    const timing = transitionTraceJourneyTiming(preprod);
    const outer = transitionTraceJourneyExecutionTiming(
      timing,
      {
        blockNo: 1700,
        slot: 34_000,
        blockHash: "11".repeat(32),
      },
      1000,
    );
    expect(outer.healthyReplay.beforeReplayAllowanceMs).toBe(
      timing.journeyTimeoutMs - timing.allowances.healthySuccessorObservationMs,
    );
    expect(outer.healthyReplay.blocksBeforeReplay).toBeGreaterThan(0);
    expect(outer.journeyTimeoutMs).toBe(
      outer.healthyReplay.beforeReplayAllowanceMs +
        outer.healthyReplay.timeoutMs,
    );
    expect(outer.deadlineMonotonicMs).toBe(1000 + outer.journeyTimeoutMs);
    const runtime = healthyJourneyReplayTiming({
      ...workload,
      tipBlockNo: workload.tipBlockNo + outer.healthyReplay.blocksBeforeReplay,
    });
    expect(runtime.timeoutMs).toBeLessThanOrEqual(
      outer.healthyReplay.timeoutMs,
    );
    expect(
      requireHealthyReplayFitsDeadline({
        deadlineMonotonicMs: outer.deadlineMonotonicMs,
        nowMonotonicMs: 1000 + outer.healthyReplay.beforeReplayAllowanceMs,
        timeoutMs: runtime.timeoutMs,
      }),
    ).toBeGreaterThanOrEqual(runtime.timeoutMs);
  });

  it("refuses a fixed outer deadline that no longer contains the captured workload", () => {
    const timeoutMs = healthyJourneyReplayTiming(workload).timeoutMs;
    expect(() =>
      requireHealthyReplayFitsDeadline({
        deadlineMonotonicMs: timeoutMs,
        nowMonotonicMs: 1,
        timeoutMs,
      }),
    ).toThrow("fixed outer deadline");
  });

  it.each([0.1, 0.2, 1])(
    "refuses unstable replay utilization at active coefficient %s",
    (activeSlotsCoeff) => {
      expect(() =>
        healthyJourneyReplayTiming({ ...workload, activeSlotsCoeff }),
      ).toThrow("cannot catch up");
    },
  );

  it.each([0, -1, Number.NaN, Number.POSITIVE_INFINITY])(
    "refuses invalid slot duration %s",
    (slotLengthSeconds) => {
      expect(() =>
        healthyJourneyReplayTiming({ ...workload, slotLengthSeconds }),
      ).toThrow("Invalid healthy replay");
    },
  );

  it("bounds counts and native timer overflow", () => {
    for (const tipBlockNo of [-1, 0.5, Number.MAX_SAFE_INTEGER + 1])
      expect(() =>
        healthyJourneyReplayTiming({ ...workload, tipBlockNo }),
      ).toThrow("Invalid");
    expect(() =>
      healthyJourneyReplayTiming({ ...workload, tipBlockNo: 1_000_000 }),
    ).toThrow("supported timer range");
  });

  it("accounts for slower production independently of replay service cost", () => {
    const base = healthyJourneyReplayTiming(workload);
    const slower = healthyJourneyReplayTiming({
      ...workload,
      slotLengthSeconds: 2,
    });
    expect(slower.initialBacklogBlocks).toBe(base.initialBacklogBlocks);
    expect(slower.replayUtilization).toBe(0.25);
    expect(slower.timeoutMs).toBeLessThan(base.timeoutMs);
  });
});

const actualTimingRun = process.env.MIDGARD_WATCHER_JOURNEY_TIMING_RUN_DIR;
it.skipIf(actualTimingRun === undefined)(
  "plans from the actual read-only node tip before suite startup",
  async () => {
    const plan = await readTransitionTraceJourneyExecutionTiming(
      actualTimingRun!,
    );
    expect(plan.capturedTip.blockNo).toBeGreaterThan(0);
    expect(plan.capturedTip.blockHash).toMatch(/^[0-9a-f]{64}$/);
    expect(plan.healthyReplay.initialBacklogBlocks).toBe(
      plan.capturedTip.blockNo + 1,
    );
    expect(plan.deadlineMonotonicMs - plan.capturedAtMonotonicMs).toBeCloseTo(
      plan.journeyTimeoutMs,
      3,
    );
    console.info("Actual read-only journey timing preflight", {
      tip: plan.capturedTip,
      cadence: plan.cadence,
      correctionTimeoutMs: plan.correctionTimeoutMs,
      healthyReplay: plan.healthyReplay,
      journeyTimeoutMs: plan.journeyTimeoutMs,
    });
  },
  15_000,
);
