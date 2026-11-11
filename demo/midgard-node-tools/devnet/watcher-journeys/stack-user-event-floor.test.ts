import {
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { JOURNEY_FIXTURE_CANDIDATES } from "./catalogue.js";
import {
  lateShiftMinimumLeadMs,
  planLateShiftFaultCommit,
} from "./honest-successor.observables.js";
import {
  journeyTimingForCategory,
  stackStagingShiftWorkMs,
} from "./journey-timing.js";
import {
  type StackLateShiftCommit,
  stackLateShiftFaultCommit,
} from "./stack-late-shift.js";
import {
  stackUserEventFloorOffsetMs,
  type StackUserEventTiming,
} from "./stack-user-event-floor.js";
import {
  eventTiming,
  gateOn,
  OPERATOR_2,
  operator2Shift,
  OUR_NODE,
  type ProfileName,
  SHIFT_START_MS,
} from "./stack-user-event-floor.test-support.js";

/** The profile's cadence: 1 s slots, 5% active, the watcher acting at inclusion. */
const journeyTiming = (category: string, name: ProfileName) => {
  const deploymentProfile = DEPLOYMENT_PROFILES[name];
  return journeyTimingForCategory(category, {
    slotLengthSeconds: 1,
    activeSlotsCoeff: 0.05,
    confirmationDepth: deploymentProfile.l1_finality.confirmation_depth,
    actionDepth: 1,
    disputeTiming: deploymentProfile.timing,
    deploymentProfile,
  });
};

/** The scheduler's threshold for a user event due at `inclusionTimeMs`. */
const strikeThreshold = (
  timing: StackUserEventTiming,
  shiftStartMs: number,
  inclusionTimeMs: number,
) =>
  SDK.computeInactivityThreshold({
    shiftStartMs: BigInt(shiftStartMs),
    stateQueueTailEndTimeMs: 0n,
    neglectedEvent: {
      kind: "Deposit",
      inclusionTimeMs: BigInt(inclusionTimeMs),
    },
    params: {
      ...SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS,
      shiftDurationMs: BigInt(timing.shiftDurationMs),
      newShiftInactivityGracePeriodMs: BigInt(timing.graceMs),
      userEventsNegligenceTimeoutMs: BigInt(timing.negligenceTimeoutMs),
    },
  });

/** A real 1 s slot grid, the Lucid emulator's, as twin4's chain has. */
let lucid: LucidEvolution;
beforeAll(async () => {
  lucid = await Lucid(
    new Emulator([generateEmulatorAccount({ lovelace: 1_000_000_000n })]),
    "Custom",
  );
});

/**
 * The soonest the SDK makes an event built at `builtMs` due, over every
 * place in a slot the build can fall: its validity end
 * (`resolveUserEventValidTo`, at that instant on the emulator's slot grid),
 * then its inclusion (`resolveEventInclusionTime`). The SDK compiles one
 * profile's event wait, so another profile's replaces it.
 */
const soonestDueMs = (timing: StackUserEventTiming, builtMs: number) => {
  const slotStartMs = SDK.slotToUnixTimeForLucid(lucid, 1_000)!;
  const slotMs = SDK.slotToUnixTimeForLucid(lucid, 1_001)! - slotStartMs;
  let soonestMs = Number.POSITIVE_INFINITY;
  for (let intoSlotMs = 0; intoSlotMs < slotMs; intoSlotMs++) {
    const nowMs = slotStartMs + intoSlotMs;
    const validToMs = SDK.resolveUserEventValidTo(
      lucid,
      timing.eventTtlMs,
      () => nowMs,
    );
    const dueMs =
      SDK.resolveEventInclusionTime(
        validToMs,
        SELECTED_DEPLOYMENT_PROFILE.network,
      ) -
      SDK.EVENT_WAIT_DURATION_MS +
      timing.eventWaitMs;
    soonestMs = Math.min(soonestMs, builtMs + dueMs - nowMs);
  }
  return soonestMs;
};

describe("the publication floor of operator-2's shift", () => {
  it.each([
    ["preprod-testing", 240_000],
    ["preprod-rehearsal", 540_000],
  ] as const)(
    "leaves no event built from it strikeable in one of %s's shifts",
    (name, floorMs) => {
      const timing = eventTiming(name);
      expect(stackUserEventFloorOffsetMs(timing)).toBe(floorMs);
      const shiftEndMs = BigInt(SHIFT_START_MS + timing.shiftDurationMs);
      const builtAt = (offsetMs: number) =>
        strikeThreshold(
          timing,
          SHIFT_START_MS,
          soonestDueMs(timing, SHIFT_START_MS + floorMs + offsetMs),
        );
      // At the floor and after it, the threshold falls at or after the
      // shift's end, where the scheduler refuses a strike. At the floor it
      // is the shift's end itself: the floor has no margin.
      expect(builtAt(0).thresholdMs).toBe(shiftEndMs);
      for (const offsetMs of [0, 1]) {
        const threshold = builtAt(offsetMs);
        expect(threshold).toMatchObject({
          kind: "unsatisfiable",
          reason: "threshold-not-before-shift-end",
        });
        expect(threshold.thresholdMs).toBeGreaterThanOrEqual(shiftEndMs);
      }
      // A millisecond before it, an event built a millisecond short of a
      // slot's end lets another operator strike operator-2 before its shift
      // ends, and so does any earlier build.
      for (const offsetMs of [-1, -2, -1_000]) {
        const threshold = builtAt(offsetMs);
        expect(threshold.kind).toBe("threshold");
        expect(threshold.thresholdMs).toBe(shiftEndMs + BigInt(offsetMs));
      }
    },
  );

  it("is the shift's start where no event built in it can fall due in time", () => {
    expect(
      stackUserEventFloorOffsetMs({
        ...eventTiming("preprod-testing"),
        shiftDurationMs: 1_000_000,
      }),
    ).toBe(0);
  });
});

describe("the gate before each of the drill's user events", () => {
  const timing = eventTiming("preprod-testing");

  it("waits for the floor, then reads the queue tail's undelivered events", async () => {
    const { gate, ledger } = gateOn({
      timing,
      appointment: operator2Shift,
      nowMs: SHIFT_START_MS + 1_000,
    });
    await gate.beforeEvent(OPERATOR_2, 42n);
    expect(ledger.nowMs).toBe(SHIFT_START_MS + 240_000);
    expect(ledger.stages).toEqual([
      `awaiting ${(SHIFT_START_MS + 240_000).toString()} in operator-2's shift before publishing a user event, so that it cannot get operator-2 struck`,
    ]);
    expect(ledger.tailsRead).toEqual([42n]);
  });

  it("does not wait once the floor has passed", async () => {
    const nowMs = SHIFT_START_MS + 900_000;
    const { gate, ledger } = gateOn({
      timing,
      appointment: operator2Shift,
      nowMs,
    });
    await gate.beforeEvent(OPERATOR_2, 0n);
    expect(ledger.nowMs).toBe(nowMs);
    expect(ledger.stages).toEqual([]);
    // Built that late, the event is still due too late for a strike.
    expect(
      strikeThreshold(timing, SHIFT_START_MS, soonestDueMs(timing, nowMs)).kind,
    ).toBe("unsatisfiable");
  });

  it("also waits for the wall clock the builders date validity from to reach the floor", async () => {
    const { gate, ledger } = gateOn({
      timing,
      appointment: operator2Shift,
      nowMs: SHIFT_START_MS + 1_000,
      wallAheadMs: -5_000,
    });
    await gate.beforeEvent(OPERATOR_2, 0n);
    // The wall clock, 5 s behind the ledger, reaches the floor 5 s after it.
    expect(ledger.nowMs - 5_000).toBe(SHIFT_START_MS + 240_000);
    expect(ledger.tailsRead).toEqual([0n]);
  });

  it.each([
    [undefined, "no appointment"],
    [{ operator: OUR_NODE, startTime: BigInt(SHIFT_START_MS) }, OUR_NODE],
  ])(
    "refuses outside operator-2's shift before waiting or reading",
    async (appointment, holder) => {
      const { gate, ledger } = gateOn({
        timing,
        appointment,
        nowMs: SHIFT_START_MS,
      });
      await expect(gate.beforeEvent(OPERATOR_2, 0n)).rejects.toThrow(
        new RegExp(`does not appoint operator-2.*${holder}`, "u"),
      );
      expect(ledger.nowMs).toBe(SHIFT_START_MS);
      expect(ledger.tailsRead).toEqual([]);
    },
  );

  // The fault's commit starts no later than the shortest late-shift lead
  // before the shift's end, and lands a commit's allowance after that.
  const commitLandsMs =
    SHIFT_START_MS +
    timing.shiftDurationMs -
    lateShiftMinimumLeadMs(190_000) +
    1 +
    190_000;
  // An event due this long into the shift is struck for at the latest commit
  // landing.
  const dueAtCommitLanding =
    commitLandsMs - timing.negligenceTimeoutMs - SHIFT_START_MS;
  const undelivered = (dueMs: number): SDK.NeglectedUserEventClaim => ({
    kind: "Deposit",
    utxo: {
      txHash: "ab".repeat(32),
      outputIndex: 3,
      address: "addr_test1",
      assets: { lovelace: 1n },
    },
    inclusionTimeMs: BigInt(SHIFT_START_MS + dueMs),
  });

  it("refuses an undelivered user event another operator could strike operator-2 for before the fault's commit lands", async () => {
    for (const dueMs of [0, 300_000, dueAtCommitLanding]) {
      const { gate } = gateOn({
        timing,
        appointment: operator2Shift,
        nowMs: SHIFT_START_MS,
        undelivered: undelivered(dueMs),
      });
      await expect(gate.beforeEvent(OPERATOR_2, 0n)).rejects.toThrow(
        /undelivered Deposit (ab){32}#3.*refusing before the drill publishes its user events/u,
      );
    }
  });

  it("admits an undelivered user event no strike can cite before the fault's commit lands", async () => {
    for (const [dueMs, tailEndTimeMs] of [
      [dueAtCommitLanding + 1, 0n],
      [timing.shiftDurationMs, 0n],
      // A committed block already covers it.
      [0, BigInt(SHIFT_START_MS)],
    ] as const) {
      const { gate } = gateOn({
        timing,
        appointment: operator2Shift,
        nowMs: SHIFT_START_MS,
        undelivered: undelivered(dueMs),
      });
      await expect(
        gate.beforeEvent(OPERATOR_2, tailEndTimeMs),
      ).resolves.toBeUndefined();
    }
  });
});

/** Whether the profile leaves the family out of its catalogue. */
const leftOut = (name: ProfileName, category: string) =>
  (
    DEPLOYMENT_PROFILES[name]
      .excluded_fraud_proof_categories as readonly string[]
  ).includes(category);

/** The registered fixture's staging shift cover on a twin stack. */
const fixtureCoverMs = (category: string, name: ProfileName) => {
  const fixture = JOURNEY_FIXTURE_CANDIDATES.find(
    (candidate) => candidate.category === category,
  );
  if (fixture === undefined) throw new Error(`No fixture for ${category}`);
  return fixture.stackShiftCoverMs({
    timing: journeyTiming(category, name),
    // A twin stack's event-history protection, as its manifest carries it.
    eventHistoryProtectionMs: { deposit: 120_000, withdrawal: 120_000 },
    profile: DEPLOYMENT_PROFILES[name].timing,
  });
};

describe("an attached family's staging cover with its events built from the floor", () => {
  it.each([
    ["preprod-testing", 1_170_000, 1_411_000, 1_500_000],
    ["preprod-rehearsal", 2_970_000, 3_211_000, 3_000_000],
  ] as const)(
    "counts the floor in each family's cover on %s",
    (name, oneEventMs, twoEventsMs, historyBlockMs) => {
      // One event, as fabricatedDeposit and the transaction families
      // publish; two, as doubleWithdraw does; and a history block committed
      // while the floor is awaited, then one event, as withdrawnInput does.
      expect(fixtureCoverMs("fabricatedDeposit", name)).toBe(oneEventMs);
      expect(fixtureCoverMs("minAda", name)).toBe(oneEventMs);
      expect(fixtureCoverMs("doubleWithdraw", name)).toBe(twoEventsMs);
      expect(fixtureCoverMs("withdrawnInput", name)).toBe(historyBlockMs);
      expect(fixtureCoverMs("zeroInput", name)).toBe(570_000);
    },
  );

  it.each(["preprod-testing", "preprod-rehearsal"] as const)(
    "fits a family awaiting one user event, as fabricatedDeposit does, in one of %s's shifts",
    (name) => {
      const profile = DEPLOYMENT_PROFILES[name].timing;
      const timing = eventTiming(name);
      const cover = fixtureCoverMs("fabricatedDeposit", name);
      const { transactionAllowanceMs } = journeyTiming(
        "fabricatedDeposit",
        name,
      );
      expect(cover).toBe(
        stackStagingShiftWorkMs({ transactionAllowanceMs }, 1, {
          count: 1,
          eventWaitMs: profile.event_wait_ms,
          protectionMs: 120_000,
          floorMs: stackUserEventFloorOffsetMs(timing),
        }),
      );
      expect(cover).toBe(
        stackUserEventFloorOffsetMs(timing) +
          SDK.USER_EVENT_TX_TTL_MS +
          profile.event_wait_ms +
          3 * transactionAllowanceMs,
      );
      expect(cover).toBeLessThanOrEqual(profile.operator_shift_ms);
    },
  );

  it.each(["preprod-testing", "preprod-rehearsal"] as const)(
    "fits every family %s installs in one of its shifts",
    (name) => {
      const profile = DEPLOYMENT_PROFILES[name];
      for (const { category } of JOURNEY_FIXTURE_CANDIDATES) {
        if (leftOut(name, category)) continue;
        // The rehearsal profile's event wait leaves the trace more work than
        // one of its shifts, as before the floor.
        if (name === "preprod-rehearsal" && category === "transitionTrace")
          expect(() => fixtureCoverMs(category, name)).toThrow(
            /cannot stage it on this stack/u,
          );
        else
          expect(fixtureCoverMs(category, name)).toBeLessThanOrEqual(
            profile.timing.operator_shift_ms,
          );
      }
    },
  );
});

describe("twin4's fault commit at a 360,000 ms late-shift lead", () => {
  const name = "preprod-testing";
  const timing = eventTiming(name);
  const { transactionAllowanceMs } = journeyTiming("fabricatedDeposit", name);
  const shiftEndMs = SHIFT_START_MS + timing.shiftDurationMs;

  // Staging that starts at the shift's start waits for the floor; staging
  // that starts as late as its cover allows finds the floor passed. Either
  // way its event is due before the commit, which starts at the lead.
  it.each([
    ["at the shift's start", SHIFT_START_MS],
    [
      "as late as its cover allows",
      shiftEndMs - fixtureCoverMs("fabricatedDeposit", name),
    ],
  ])(
    "commits fabricatedDeposit's fault in its window with staging %s",
    async (_, stagingStartMs) => {
      expect(lateShiftMinimumLeadMs(transactionAllowanceMs)).toBe(250_000);
      const { gate, ledger } = gateOn({
        timing,
        appointment: operator2Shift,
        nowMs: stagingStartMs,
        commitAllowanceMs: transactionAllowanceMs,
      });
      await gate.beforeEvent(OPERATOR_2, 0n);
      const builtMs = ledger.nowMs;
      expect(builtMs).toBeGreaterThanOrEqual(SHIFT_START_MS + 240_000);
      // Its validity, at most a second past its time to live, then the wait.
      const dueMs = builtMs + timing.eventTtlMs + 1_000 + timing.eventWaitMs;
      expect(
        strikeThreshold(timing, SHIFT_START_MS, soonestDueMs(timing, builtMs))
          .kind,
      ).toBe("unsatisfiable");
      // The fault's block takes a commit and an attestation's two steps
      // after the event is due; the cover leaves at least those.
      ledger.nowMs = Math.max(dueMs, shiftEndMs - 3 * transactionAllowanceMs);
      const commits: StackLateShiftCommit[] = [];
      await stackLateShiftFaultCommit({
        category: "fabricatedDeposit",
        leadMs: 360_000,
        readAppointment: async () => operator2Shift,
        shiftDurationMs: timing.shiftDurationMs,
        commitAllowanceMs: transactionAllowanceMs,
        now: () => ledger.nowMs,
        awaitLedgerTime: async (timeMs) => {
          ledger.nowMs = Math.max(ledger.nowMs, timeMs);
        },
        record: async (commit) => {
          commits.push(commit);
        },
        onStage: () => {},
      })(OPERATOR_2);
      expect(commits).toEqual([
        expect.objectContaining({
          notBeforeMs: shiftEndMs - 360_000,
          notAfterMs: shiftEndMs - 250_000 + 1,
          commitStartMs: shiftEndMs - 360_000,
        }),
      ]);
      expect(dueMs).toBeLessThan(shiftEndMs - 360_000);
    },
  );

  // No cover depends on the lead: each leaves the fault's own block (a
  // commit and its attestation) at the end of operator-2's shift, which is
  // longer than the lead, so staging finishes before the window opens.
  it("plans every family's commit inside [end - 360,000, end - 249,999]", () => {
    for (const { category } of JOURNEY_FIXTURE_CANDIDATES) {
      if (leftOut(name, category)) continue;
      const allowanceMs = journeyTiming(category, name).transactionAllowanceMs;
      expect(fixtureCoverMs(category, name)).toBeLessThanOrEqual(
        timing.shiftDurationMs,
      );
      expect(
        planLateShiftFaultCommit({
          shift: { startMs: SHIFT_START_MS, endMs: shiftEndMs },
          leadMs: 360_000,
          commitAllowanceMs: allowanceMs,
        }),
      ).toEqual({
        notBeforeMs: shiftEndMs - 360_000,
        notAfterMs: shiftEndMs - 250_000 + 1,
      });
      // The trace's cover leaves the shortest lead for its commit instead,
      // so its commit starts by the window's end.
      if (category !== "transitionTrace")
        expect(3 * allowanceMs).toBeGreaterThanOrEqual(360_000);
    }
  });
});
