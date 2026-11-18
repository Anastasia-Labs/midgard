import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import {
  REGISTRATION_DURATION_MS,
  USER_EVENT_TX_TTL_MS,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { DEPLOYMENT_PARAMETERS } from "../../src/devnet-stack/node-env.js";
import {
  GENERIC_JOURNEY_PLAN,
  genericJourneyTiming,
  healthyJourneyReplayTiming,
  journeyExecutionTiming,
  journeyTimingForCategory,
  readJourneyExecutionTiming,
  readJourneyTiming,
  stackStagingShiftCover,
  stackStagingShiftWorkMs,
} from "./journey-timing.js";

const preprod = {
  slotLengthSeconds: 1,
  activeSlotsCoeff: 0.05,
  confirmationDepth: 30,
};
/** The selected deployment profile's registration window, in the budget. */
const registrationMs = Number(REGISTRATION_DURATION_MS);

/** Every allowance in a journey's limit but its successor's transactions. */
const fixedAllowancesMs = (
  allowances: ReturnType<typeof genericJourneyTiming>["allowances"],
) =>
  allowances.authorityStartupMs +
  allowances.watcherStartupMs +
  allowances.automaticDecisionMs +
  allowances.healthySuccessorObservationMs +
  allowances.successorRegistrationMs +
  allowances.fixtureStagingMs +
  allowances.finalizedEvidenceStampMs;

describe("journey timing", () => {
  it("budgets actual block cadence for every stage but the correction", () => {
    const timing = genericJourneyTiming(preprod);
    // Two finality windows plus build and RPC for one dependent transaction.
    expect(timing.transactionAllowanceMs).toBe(1_350_000);
    expect(timing.journeyTimeoutMs).toBe(9_960_000 + registrationMs);
    expect(timing.plan.successorTransactions).toBe(4);
    expect(timing.allowances.successorRegistrationMs).toBe(registrationMs);
    expect(timing.cadence.confirmationDepth).toBe(30);
    // Absent an explicit action depth, every stage keeps waiting for release
    // finality, which is the behavior before inclusion-gated actions.
    expect(timing.cadence.actionDepth).toBe(30);
  });

  it("budgets action stages from the action depth and the stamp from finality", () => {
    const inclusion = genericJourneyTiming({
      ...preprod,
      actionDepth: 1,
    });
    const finality = genericJourneyTiming(preprod);
    expect(inclusion.cadence.actionDepth).toBe(1);
    expect(inclusion.cadence.confirmationDepth).toBe(30);
    // One block at f=0.05 and one-second slots: twenty seconds per inclusion.
    expect(inclusion.transactionAllowanceMs).toBe(40_000 + 120_000 + 30_000);
    // The single release-finality window is unchanged by the action depth.
    expect(inclusion.allowances.finalizedEvidenceStampMs).toBe(1_230_000);
    expect(inclusion.allowances).toEqual(finality.allowances);
    expect(inclusion.journeyTimeoutMs).toBeLessThan(finality.journeyTimeoutMs);
  });

  it("refuses an action depth outside the release finality window", () => {
    for (const actionDepth of [0, 1.5, 31, Number.NaN])
      expect(() => genericJourneyTiming({ ...preprod, actionDepth })).toThrow(
        "Invalid journey cadence",
      );
  });

  it("scales with slot duration, block probability, and finality independently", () => {
    // A transaction's confirmation allowance: all of it but build and RPC.
    const confirmationMs = (cadence: typeof preprod & object) =>
      genericJourneyTiming(cadence).transactionAllowanceMs - 150_000;
    const baseline = confirmationMs(preprod);
    expect(confirmationMs({ ...preprod, slotLengthSeconds: 2 })).toBe(
      2 * baseline,
    );
    expect(confirmationMs({ ...preprod, activeSlotsCoeff: 0.1 })).toBe(
      baseline / 2,
    );
    expect(confirmationMs({ ...preprod, confirmationDepth: 60 })).toBe(
      2 * baseline,
    );
    expect(
      genericJourneyTiming({ ...preprod, actionDepth: 15 })
        .transactionAllowanceMs - 150_000,
    ).toBe(baseline / 2);
  });

  it.each([
    { activeSlotsCoeff: 0 },
    { activeSlotsCoeff: 1.01 },
    { activeSlotsCoeff: Number.NaN },
    { slotLengthSeconds: 0 },
    { slotLengthSeconds: Number.POSITIVE_INFINITY },
    { confirmationDepth: 0 },
    { confirmationDepth: 1.5 },
  ])("rejects invalid cadence or allowance %j", (invalid) => {
    expect(() => genericJourneyTiming({ ...preprod, ...invalid })).toThrow(
      "Invalid journey cadence",
    );
  });

  it("rejects timer overflow instead of turning a long deadline into an immediate one", () => {
    expect(() =>
      genericJourneyTiming({ ...preprod, activeSlotsCoeff: 0.000001 }),
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
        readJourneyTiming(directory, "transitionTrace"),
      ).rejects.toThrow();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
    await expect(
      readJourneyTiming("relative-run", "transitionTrace"),
    ).rejects.toThrow("must be absolute");
  });
});

/**
 * No family's limit counts its proof's transactions, at Preprod's cadence
 * or the fast devnet's: every limit is its successor's transactions and the
 * fixed allowances, the same for every family, however long its proof.
 */
describe.each([0.05, 0.25])(
  "every family's plan at active slot coefficient %s",
  (activeSlotsCoeff) => {
    const cadence = {
      ...preprod,
      activeSlotsCoeff,
      actionDepth: 1,
      disputeTiming: DEPLOYMENT_PROFILES["preprod-rehearsal"].timing,
    };

    it("leaves the correction out of the limit", () => {
      const timing = genericJourneyTiming(cadence);
      expect(timing.plan).toBe(GENERIC_JOURNEY_PLAN);
      expect(timing.journeyTimeoutMs).toBe(
        GENERIC_JOURNEY_PLAN.successorTransactions *
          timing.transactionAllowanceMs +
          fixedAllowancesMs(timing.allowances),
      );
    });

    it("gives every family the generic plan, and an undefended case's wait no share of the limit", () => {
      const generic = genericJourneyTiming(cadence);
      for (const category of [
        "transitionTrace",
        "zeroInput",
        "executionNativeScriptInvalid",
      ])
        expect(journeyTimingForCategory(category, cadence)).toEqual(generic);
      const dispute = journeyTimingForCategory(
        "validationTraceDispute",
        cadence,
      );
      expect(dispute.allowances.undefendedCaseMs).toBeGreaterThan(0);
      expect(dispute.journeyTimeoutMs).toBe(generic.journeyTimeoutMs);
    });
  },
);

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
    expect(plan.replayUtilization).toBeCloseTo(0.05, 12);
    expect(plan.timeoutMs).toBe(1_886_316);
    expect(plan.timeoutMs).toBeGreaterThanOrEqual(
      (plan.initialBacklogBlocks + plan.blocksDuringReplay) *
        plan.blockAllowanceMs +
        plan.observationAllowanceMs,
    );
    // Sparse finality receipts cannot prove how many expensive callbacks remain.
    expect(plan.initialBacklogBlocks).toBeGreaterThan(1700 - 750);
  });

  it("includes new blocks during the full proof and successor window before replay", () => {
    const timing = genericJourneyTiming(preprod);
    const outer = journeyExecutionTiming(timing, {
      blockNo: 1700,
      slot: 34_000,
      blockHash: "11".repeat(32),
    });
    expect(outer.healthyReplay.beforeReplayAllowanceMs).toBe(
      timing.journeyTimeoutMs - timing.allowances.healthySuccessorObservationMs,
    );
    expect(outer.healthyReplay.blocksBeforeReplay).toBeGreaterThan(0);
    expect(outer.journeyTimeoutMs).toBe(
      outer.healthyReplay.beforeReplayAllowanceMs +
        outer.healthyReplay.timeoutMs,
    );
    const runtime = healthyJourneyReplayTiming({
      ...workload,
      tipBlockNo: workload.tipBlockNo + outer.healthyReplay.blocksBeforeReplay,
    });
    expect(runtime.timeoutMs).toBeLessThanOrEqual(
      outer.healthyReplay.timeoutMs,
    );
  });

  it("plans the local devnet's four-second cadence at half utilization or less", () => {
    const plan = healthyJourneyReplayTiming({
      ...workload,
      activeSlotsCoeff: 0.25,
    });
    expect(plan.replayUtilization).toBeLessThanOrEqual(0.5);
    expect(plan.timeoutMs).toBeGreaterThanOrEqual(
      (plan.initialBacklogBlocks + plan.blocksDuringReplay) *
        plan.blockAllowanceMs +
        plan.observationAllowanceMs,
    );
  });

  it.each([1])(
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
      healthyJourneyReplayTiming({ ...workload, tipBlockNo: 10_000_000 }),
    ).toThrow("supported timer range");
  });

  it("accounts for slower production independently of replay service cost", () => {
    const base = healthyJourneyReplayTiming(workload);
    const slower = healthyJourneyReplayTiming({
      ...workload,
      slotLengthSeconds: 2,
    });
    expect(slower.initialBacklogBlocks).toBe(base.initialBacklogBlocks);
    expect(slower.replayUtilization).toBeCloseTo(0.025, 12);
    expect(slower.timeoutMs).toBeLessThan(base.timeoutMs);
  });
});

const actualTimingRun = process.env.MIDGARD_WATCHER_JOURNEY_TIMING_RUN_DIR;
// Every family shares the generic plan; the validation-trace dispute's also
// holds its undefended case's wait, which it reads from the deployment's
// dispute clocks.
it.skipIf(actualTimingRun === undefined)(
  "plans from the actual read-only node tip and deployment before suite startup",
  async () => {
    const plan = await readJourneyExecutionTiming(
      actualTimingRun!,
      "validationTraceDispute",
    );
    expect(plan.allowances.undefendedCaseMs).toBeGreaterThan(0);
    expect(plan.capturedTip.blockNo).toBeGreaterThan(0);
    expect(plan.capturedTip.blockHash).toMatch(/^[0-9a-f]{64}$/);
    expect(plan.healthyReplay.initialBacklogBlocks).toBe(
      plan.capturedTip.blockNo + 1,
    );
    console.info("Actual read-only journey timing preflight", {
      tip: plan.capturedTip,
      cadence: plan.cadence,
      healthyReplay: plan.healthyReplay,
      journeyTimeoutMs: plan.journeyTimeoutMs,
    });
  },
  15_000,
);

describe("an attached stack's staging work in operator-2's shift", () => {
  const inclusion = genericJourneyTiming({ ...preprod, actionDepth: 1 });
  /** The event-history protection the stack deploys with. */
  const stackProtectionMs = Number(
    DEPLOYMENT_PARAMETERS.MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS,
  );

  it("covers each block's commit and its attestation's init and apply, one dependent transaction each", () => {
    const oneBlock = stackStagingShiftWorkMs(inclusion, 1);
    expect(oneBlock).toBe(3 * inclusion.transactionAllowanceMs);
    expect(stackStagingShiftWorkMs(inclusion, 2)).toBe(2 * oneBlock);
  });

  it("refuses staging that commits no block", () => {
    for (const blocks of [0, -1, 1.5])
      expect(() => stackStagingShiftWorkMs(inclusion, blocks)).toThrow(
        /blocks/u,
      );
  });

  it("holds each user event's validity, then one event wait shared by all of them", () => {
    const eventWaitMs =
      DEPLOYMENT_PROFILES["preprod-testing"].timing.event_wait_ms;
    type Events = Readonly<{ eventWaitMs: number; protectionMs: number }>;
    const eventsWork = (count: number, events: Events) =>
      stackStagingShiftWorkMs(inclusion, 2, { count, ...events }) -
      stackStagingShiftWorkMs(inclusion, 2);
    const events: Events = { eventWaitMs, protectionMs: stackProtectionMs };
    expect(eventsWork(1, events)).toBe(USER_EVENT_TX_TTL_MS + eventWaitMs);
    // A later event adds its own validity and its wait for the event before
    // it to leave its history protection, never a second event wait.
    const later = (input: Events) =>
      eventsWork(2, input) - eventsWork(1, input);
    expect(later(events)).toBeGreaterThan(USER_EVENT_TX_TTL_MS);
    expect(later({ ...events, eventWaitMs: 2 * eventWaitMs })).toBe(
      later(events),
    );
    expect(eventsWork(3, events) - eventsWork(2, events)).toBe(later(events));
    expect(
      later({ ...events, protectionMs: stackProtectionMs + 1_000 }) -
        later(events),
    ).toBe(1_000);
    for (const count of [-1, 0.5])
      expect(() =>
        stackStagingShiftWorkMs(inclusion, 1, { count, ...events }),
      ).toThrow(/user events/u);
  });

  it("fits doubleWithdraw's two withdrawals in one preprod-rehearsal shift", () => {
    const profile = DEPLOYMENT_PROFILES["preprod-rehearsal"].timing;
    const timing = journeyTimingForCategory("doubleWithdraw", {
      ...preprod,
      actionDepth: 1,
    });
    const cover = stackStagingShiftCover({
      category: "doubleWithdraw",
      timing,
      historyBlocks: 0,
      userEvents: 2,
      eventHistoryProtectionMs: stackProtectionMs,
      profile,
    });
    expect(cover).toBeLessThanOrEqual(profile.operator_shift_ms);
  });

  it("still refuses, on the preprod-rehearsal shift, staging whose work exceeds it", () => {
    const profile = DEPLOYMENT_PROFILES["preprod-rehearsal"].timing;
    const historyBlocks = Math.ceil(
      profile.operator_shift_ms / stackStagingShiftWorkMs(inclusion, 1),
    );
    expect(() =>
      stackStagingShiftCover({
        category: "doubleWithdraw",
        timing: inclusion,
        historyBlocks,
        userEvents: 2,
        eventHistoryProtectionMs: stackProtectionMs,
        profile,
      }),
    ).toThrow(/doubleWithdraw staging holds operator-2's shift/u);
  });

  it("refuses, by family, staging that no shift of the profile holds", () => {
    const work = {
      category: "doubleWithdraw",
      timing: inclusion,
      historyBlocks: 0,
      userEvents: 2,
      eventHistoryProtectionMs: stackProtectionMs,
    } as const;
    const rehearsal = DEPLOYMENT_PROFILES["preprod-rehearsal"].timing;
    const coverMs = stackStagingShiftWorkMs(inclusion, 1, {
      count: 2,
      eventWaitMs: rehearsal.event_wait_ms,
      protectionMs: stackProtectionMs,
    });
    expect(
      stackStagingShiftCover({
        ...work,
        profile: { ...rehearsal, operator_shift_ms: coverMs },
      }),
    ).toBe(coverMs);
    expect(() =>
      stackStagingShiftCover({
        ...work,
        profile: { ...rehearsal, operator_shift_ms: coverMs - 1 },
      }),
    ).toThrow(/doubleWithdraw staging holds operator-2's shift/u);
  });
});
