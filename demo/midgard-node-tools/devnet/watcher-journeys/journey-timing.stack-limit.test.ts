import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import {
  awaitOperatorShift,
  type OperatorShiftAppointment,
} from "midgard-watcher/tests/support/published-block-actor.operator-shift";
import { describe, expect, it } from "vitest";

import { JOURNEY_FIXTURE_CANDIDATES } from "./catalogue.js";
import {
  journeyTimingForCategory,
  stackJourneyTimeoutMs,
  stackRunTimeoutsMs,
  type StackShiftCoverInput,
} from "./journey-timing.js";
import { JOURNEY_RESTART_TIMEOUT_MS } from "./operations.js";
import {
  OPERATOR_COMPOSE_TIMEOUT_MS,
  OPERATOR_READINESS_READ_TIMEOUT_MS,
  OPERATOR_RESUME_POLL_INTERVAL_MS,
} from "./operator-pause.js";
import { stackTraceShiftWorkMs } from "./stack-trace-staging.js";

/** A twin stack's event-history protection, as its manifest carries it. */
const EVENT_HISTORY_PROTECTION_MS = 120_000;

/** The registered fixture's own staging shift cover on a stack. */
const fixtureCoverMs = (
  category: string,
  input: Omit<StackShiftCoverInput, "eventHistoryProtectionMs">,
) => {
  const fixture = JOURNEY_FIXTURE_CANDIDATES.find(
    (candidate) => candidate.category === category,
  );
  if (fixture === undefined) throw new Error(`No fixture for ${category}`);
  return fixture.stackShiftCoverMs({
    ...input,
    eventHistoryProtectionMs: {
      deposit: EVENT_HISTORY_PROTECTION_MS,
      withdrawal: EVENT_HISTORY_PROTECTION_MS,
    },
  });
};

/** Preprod's cadence: 1 s slots, 5% active, 30-block finality. */
const preprod = {
  slotLengthSeconds: 1,
  activeSlotsCoeff: 0.05,
  confirmationDepth: 30,
};

describe("an attached stack's per-family time limit", () => {
  const profile = DEPLOYMENT_PROFILES["preprod-rehearsal"].timing;
  // The refusal-only families end at the committee's refusal, never at a
  // proof; transitionTrace has its own audited plan.
  it.each(["daHashPreimage", "canonicalDecodability", "l2TxMistag"])(
    "gives %s its computed budget, one of our node's shifts and twice its own staging cover at rehearsal cadence",
    (category) => {
      const timing = journeyTimingForCategory(category, {
        ...preprod,
        actionDepth: 1,
      });
      const coverMs = fixtureCoverMs(category, { timing, profile });
      const limitMs = stackJourneyTimeoutMs(timing, profile, coverMs);
      // At worst the rest of operator-2's shift, our node's whole shift and
      // the staging's own cover, on top of everything the family's plan
      // waits for.
      expect(limitMs).toBe(
        timing.journeyTimeoutMs + profile.operator_shift_ms + 2 * coverMs,
      );
      // A refused fault ages out one attestation timeout after its block,
      // and its removal lands within one more
      // (stack-da-publication.ts, refusal).
      expect(
        profile.operator_shift_ms +
          2 * coverMs +
          2 * profile.da_attestation_timeout_ms,
      ).toBeLessThanOrEqual(limitMs);
    },
  );

  // twin4: the Preprod testing profile, 10-block finality, the watcher
  // acting at inclusion.
  const testing = DEPLOYMENT_PROFILES["preprod-testing"];
  const traceTiming = journeyTimingForCategory("transitionTrace", {
    ...preprod,
    confirmationDepth: testing.l1_finality.confirmation_depth,
    actionDepth: 1,
  });
  const trace = JOURNEY_FIXTURE_CANDIDATES.find(
    (candidate) => candidate.category === "transitionTrace",
  )!;

  it("takes transitionTrace's cover from the work its attached staging holds", () => {
    // The trace's staging holds out only the deposit history's protection.
    const coverMs = trace.stackShiftCoverMs({
      timing: traceTiming,
      eventHistoryProtectionMs: { deposit: 60_000, withdrawal: 600_000 },
      profile: testing.timing,
    });
    expect(coverMs).toBe(
      stackTraceShiftWorkMs({
        transactionAllowanceMs: traceTiming.transactionAllowanceMs,
        eventWaitMs: testing.timing.event_wait_ms,
        eventHistoryProtectionMs: 60_000,
        operatorShiftMs: testing.timing.operator_shift_ms,
      }),
    );
    expect(stackJourneyTimeoutMs(traceTiming, testing.timing, coverMs)).toBe(
      traceTiming.journeyTimeoutMs +
        testing.timing.operator_shift_ms +
        2 * coverMs,
    );
  });

  it("refuses transitionTrace's limit where no shift holds its staging", () => {
    // The rehearsal profile's event wait leaves the trace more work than one
    // of its shifts; its staging refuses it before any L1 work.
    expect(() =>
      fixtureCoverMs("transitionTrace", {
        timing: journeyTimingForCategory("transitionTrace", {
          ...preprod,
          actionDepth: 1,
        }),
        profile,
      }),
    ).toThrow(/cannot stage it on this stack/u);
  });

  // Operator-2's node is stopped through the fault phase, so nobody defends
  // the case the watcher opens; the case validator lets it close Convicted
  // only past its defense deadline, the side clock C plus the longest move
  // validity range L_d after the case starts (validation-case-v1), and the
  // watcher journals nothing while it waits.
  it("gives validationTraceDispute its computed budget plus the undefended case's defense window at rehearsal cadence", () => {
    const cadence = { ...preprod, actionDepth: 1 };
    const generic = journeyTimingForCategory("zeroInput", cadence);
    const timing = journeyTimingForCategory("validationTraceDispute", {
      ...cadence,
      disputeTiming: profile,
    });
    const defenseWindowMs =
      profile.dispute_side_clock_ms + profile.dispute_max_validity_range_ms;
    // One day and two minutes on the rehearsal profile.
    expect(defenseWindowMs).toBe(86_520_000);
    // The window the progress gate holds the journal's silence open for,
    // from the case's start; the Close then gets one transaction allowance.
    expect(timing.allowances.undefendedCaseMs).toBe(defenseWindowMs);
    expect(timing.transactionAllowanceMs).toBe(generic.transactionAllowanceMs);
    expect(timing.correctionTimeoutMs).toBe(
      generic.correctionTimeoutMs + defenseWindowMs,
    );
    const coverMs = fixtureCoverMs("validationTraceDispute", {
      timing,
      profile,
    });
    expect(stackJourneyTimeoutMs(timing, profile, coverMs)).toBe(
      generic.journeyTimeoutMs +
        defenseWindowMs +
        profile.operator_shift_ms +
        2 * coverMs,
    );
    // Every other family has no such window.
    expect(generic.allowances.undefendedCaseMs).toBe(0);
  });

  it("refuses validationTraceDispute's budget without the deployment's dispute clocks", () => {
    expect(() =>
      journeyTimingForCategory("validationTraceDispute", {
        ...preprod,
        actionDepth: 1,
      }),
    ).toThrow(/dispute clocks/u);
  });

  it("refuses a limit no timer can hold", () => {
    expect(() =>
      stackJourneyTimeoutMs(
        { journeyTimeoutMs: 2_147_483_647 - profile.operator_shift_ms },
        profile,
        1,
      ),
    ).toThrow(/timer range/u);
  });

  it("refuses a staging cover one shift cannot hold", () => {
    for (const coverMs of [0, profile.operator_shift_ms + 1, 0.5])
      expect(() =>
        stackJourneyTimeoutMs({ journeyTimeoutMs: 1 }, profile, coverMs),
      ).toThrow(/does not fit one/u);
  });
});

describe("a family the deployment's profile leaves out of its catalogue", () => {
  // The Preprod testing profile, as an attached twin stack's finalized
  // manifest carries it: 10-block finality, the watcher acting at inclusion.
  const testing = DEPLOYMENT_PROFILES["preprod-testing"];
  const cadence = {
    ...preprod,
    confirmationDepth: testing.l1_finality.confirmation_depth,
    actionDepth: 1,
    disputeTiming: testing.timing,
    deploymentProfile: testing,
  };

  it("refuses it by name and profile before any family is staged", () => {
    expect(testing.excluded_fraud_proof_categories).toContain(
      "validationTraceDispute",
    );
    expect(() =>
      journeyTimingForCategory("validationTraceDispute", cadence),
    ).toThrow(/validationTraceDispute.*preprod-testing/u);
  });

  it("refuses every family a profile leaves out and budgets every family it installs", () => {
    const mainnet = DEPLOYMENT_PROFILES.mainnet;
    const mainnetCadence = {
      ...cadence,
      disputeTiming: mainnet.timing,
      deploymentProfile: mainnet,
    };
    expect(mainnet.excluded_fraud_proof_categories.length).toBeGreaterThan(1);
    for (const category of mainnet.excluded_fraud_proof_categories)
      expect(() => journeyTimingForCategory(category, mainnetCadence)).toThrow(
        new RegExp(`${category}.*mainnet`, "u"),
      );
    for (const category of ["transitionTrace", "doubleWithdraw", "zeroInput"])
      expect(
        journeyTimingForCategory(category, cadence).journeyTimeoutMs,
      ).toBeGreaterThan(0);
    // The rehearsal profile installs the execution dispute.
    const rehearsal = DEPLOYMENT_PROFILES["preprod-rehearsal"];
    expect(
      journeyTimingForCategory("validationTraceDispute", {
        ...cadence,
        disputeTiming: rehearsal.timing,
        deploymentProfile: rehearsal,
      }).allowances.undefendedCaseMs,
    ).toBeGreaterThan(0);
  });
});

const OPERATOR_2 = "02".repeat(28);
const OUR_NODE = "01".repeat(28);

/**
 * How long operator-2 waits for a shift that holds `coverMs` of staging, on
 * a scheduler two operators take in turn: a shift that ends is taken next
 * by the other operator, whose own node refreshes first at its end, and a
 * refresh due in another's shift is answered as the node answers it.
 */
const shiftWaitMs = async (
  start: Readonly<{ appointment: OperatorShiftAppointment; nowMs: number }>,
  coverMs: number,
  shiftMs: number,
) => {
  let clock = start.nowMs;
  let current = start.appointment;
  await awaitOperatorShift(OPERATOR_2, coverMs, {
    readAppointment: async () => current,
    refresh: async () => {
      const ended = current!;
      const endMs = Number(ended.startTime) + shiftMs;
      if (ended.operator === OPERATOR_2) {
        // Our node takes the next shift at the end of operator-2's.
        current = { operator: OUR_NODE, startTime: BigInt(endMs) };
        throw new Error("lost the refresh to our node");
      }
      current = { operator: OPERATOR_2, startTime: BigInt(clock) };
      return { kind: "aligned" };
    },
    now: () => clock,
    awaitLedgerTime: async (timeMs) => {
      clock = Math.max(clock, timeMs);
    },
    shiftDurationMs: BigInt(shiftMs),
    onStage: () => {},
  });
  return clock - start.nowMs;
};

describe("an attached stack's limit against operator-2's worst shift wait", () => {
  // twin4: the Preprod testing profile (15-minute maturity, 30-minute
  // shift), 10-block finality, the watcher acting at inclusion.
  const testing = DEPLOYMENT_PROFILES["preprod-testing"];
  const shiftMs = testing.timing.operator_shift_ms;
  const timing = journeyTimingForCategory("withdrawnInput", {
    ...preprod,
    confirmationDepth: testing.l1_finality.confirmation_depth,
    actionDepth: 1,
    disputeTiming: testing.timing,
    deploymentProfile: testing,
  });
  // withdrawnInput stages a history block and one user event: the longest
  // cover of the registered families on this profile.
  const coverMs = fixtureCoverMs("withdrawnInput", {
    timing,
    profile: testing.timing,
  });

  it("covers operator-2's shift ending just short of the staging, our node's whole shift, then the staging", async () => {
    expect(coverMs).toBeGreaterThan(shiftMs / 2);
    const startMs = 100_000_000;
    const waitMs = await shiftWaitMs(
      {
        appointment: { operator: OPERATOR_2, startTime: BigInt(startMs) },
        // Operator-2's shift has just under the staging's cover left.
        nowMs: startMs + shiftMs - coverMs + 1,
      },
      coverMs,
      shiftMs,
    );
    // The remainder, our node's shift, and the first millisecond of
    // operator-2's next shift.
    expect(waitMs).toBe(coverMs - 1 + shiftMs + 1);
    const limitMs = stackJourneyTimeoutMs(timing, testing.timing, coverMs);
    // The family's own budget, then the wait, then the staging's own shift.
    expect(limitMs - timing.journeyTimeoutMs).toBeGreaterThanOrEqual(
      waitMs + coverMs,
    );
    expect(limitMs).toBe(timing.journeyTimeoutMs + shiftMs + 2 * coverMs);
  });

  it("covers our node's whole shift, then the staging", async () => {
    const startMs = 100_000_000;
    const waitMs = await shiftWaitMs(
      {
        appointment: { operator: OUR_NODE, startTime: BigInt(startMs) },
        nowMs: startMs,
      },
      coverMs,
      shiftMs,
    );
    expect(waitMs).toBe(shiftMs + 1);
    expect(
      stackJourneyTimeoutMs(timing, testing.timing, coverMs) -
        timing.journeyTimeoutMs,
    ).toBeGreaterThanOrEqual(waitMs + coverMs);
  });
});

describe("an attached stack run's time limits", () => {
  // Only the run's last family resumes operator-2's node (journey.test.ts).
  // The resume starts the node with one compose command, which is killed at
  // its timeout, then reads /readyz until its deadline: past the deadline it
  // may still sleep one poll interval and make one more read before it
  // refuses (createOperatorPause, composeOperatorNodeControl).
  const resumeWaitMs =
    OPERATOR_COMPOSE_TIMEOUT_MS +
    JOURNEY_RESTART_TIMEOUT_MS +
    OPERATOR_RESUME_POLL_INTERVAL_MS +
    OPERATOR_READINESS_READ_TIMEOUT_MS;

  it("gives only the last family the resume's whole wait", () => {
    expect(stackRunTimeoutsMs([1_000, 2_000, 3_000])).toEqual([
      1_000,
      2_000,
      3_000 + resumeWaitMs,
    ]);
    expect(stackRunTimeoutsMs([4_000])).toEqual([4_000 + resumeWaitMs]);
    expect(stackRunTimeoutsMs([])).toEqual([]);
  });

  it("refuses a last family's limit the resume pushes past any timer", () => {
    expect(() =>
      stackRunTimeoutsMs([1_000, 2_147_483_647 - resumeWaitMs + 1]),
    ).toThrow(/timer range/u);
  });
});
