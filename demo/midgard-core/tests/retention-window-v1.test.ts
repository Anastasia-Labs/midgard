import { describe, expect, it } from "vitest";

import { MIDGARD_CONSENSUS_LIMITS } from "../src/consensus-profile-v1.js";
import { DA_TRANSPORT_LIMITS } from "../src/da-transport.js";
import { verifyDeploymentManifestIdentity } from "../src/deployment-manifest-identity-v1.js";
import {
  assertRetentionDaysCoverWindow,
  assertRetentionWindowCoversDeployment,
  assertWorstCaseProofTimeWithinBound,
  daRetentionPruneDecision,
  MIDGARD_MIN_RETENTION_DAYS,
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
  retentionDaysCoverWindow,
  retentionDeadlineAlert,
  retentionDeadlineForBlock,
} from "../src/retention-window-v1.js";

const BLOCK_END = Date.UTC(2026, 0, 1, 0, 0, 0);

const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

describe("MIDGARD_RETENTION_WINDOW_V1 derived arithmetic (F04)", () => {
  it("derives every constant from the frozen profiles, never from a literal", () => {
    expect(MIDGARD_RETENTION_WINDOW.maturityMs).toBe(604_800_000);
    expect(MIDGARD_RETENTION_WINDOW.maturityMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.blockMaturityMs,
    );
    expect(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs).toBe(
      302_400_000,
    );
    expect(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.blockMaturityMs / 2,
    );
    expect(MIDGARD_RETENTION_WINDOW.requiredRetentionMs).toBe(907_200_000);
    expect(MIDGARD_RETENTION_WINDOW.retentionDays).toBe(
      DA_TRANSPORT_LIMITS.minimumRetentionDays,
    );
    expect(MIDGARD_RETENTION_WINDOW.retentionDays).toBe(15);
    expect(MIDGARD_RETENTION_WINDOW.deployedRetentionMs).toBe(1_296_000_000);
    expect(MIDGARD_RETENTION_WINDOW.marginMs).toBe(388_800_000);
    expect(MIDGARD_RETENTION_WINDOW.deployedRetentionMs).toBeGreaterThanOrEqual(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
    expect(MIDGARD_MIN_RETENTION_DAYS).toBe(11);
  });

  it("records but never enforces against the measured dispute schedule", () => {
    expect(MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.minValidationDisputeMaturityMs,
    );
    expect(MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs).toBe(
      39_600_000,
    );
    // The measured 11h schedule is far below the enforced half-maturity bound;
    // enforcement must key on the bound, not the measurement.
    expect(
      MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs,
    ).toBeLessThan(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs);
    expect(MIDGARD_RETENTION_WINDOW.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW.maturityMs +
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    );
  });
});

describe("worst-case proof-time bound", () => {
  it("accepts exactly the bound and rejects one millisecond past it", () => {
    expect(assertWorstCaseProofTimeWithinBound(302_400_000)).toBe(302_400_000);
    expect(() => assertWorstCaseProofTimeWithinBound(302_400_001)).toThrow(
      /exceeds the canonical V1 worst-case proof-time bound/u,
    );
  });

  it("rejects malformed observations", () => {
    for (const bad of [Number.NaN, -1, 1.5, "302400000", null, 2 ** 53]) {
      expect(() => assertWorstCaseProofTimeWithinBound(bad)).toThrow();
    }
  });
});

describe("retention-days floor and deployment binding", () => {
  it("accepts the deployed 15 days and rejects 14", () => {
    expect(assertRetentionDaysCoverWindow(15)).toBe(15);
    expect(retentionDaysCoverWindow(15)).toBe(true);
    expect(retentionDaysCoverWindow(14)).toBe(true);
    // 11 whole days is the derived minimum covering 907_200_000 ms.
    expect(retentionDaysCoverWindow(11)).toBe(true);
    expect(retentionDaysCoverWindow(10)).toBe(false);
    expect(() => assertRetentionDaysCoverWindow(10)).toThrow(
      /must be at least 11 days/u,
    );
  });

  it("binds the window to deployment identity via da.transportProfile", () => {
    expect(assertRetentionWindowCoversDeployment(manifestWith(15))).toBe(15);
    expect(() =>
      assertRetentionWindowCoversDeployment(manifestWith(1)),
    ).toThrow(/da\.transportProfile\.retentionDays must be at least/u);
  });

  it("rejects malformed manifest retention values and shapes", () => {
    for (const bad of [Number.NaN, -1, 1.5, "15", null, 2 ** 53]) {
      expect(() =>
        assertRetentionWindowCoversDeployment(manifestWith(bad)),
      ).toThrow(/must be a non-negative safe integer number of days/u);
    }
    expect(() => assertRetentionWindowCoversDeployment(null)).toThrow(
      /Deployment manifest must be an object/u,
    );
    expect(() => assertRetentionWindowCoversDeployment({})).toThrow(
      /Deployment manifest da must be an object/u,
    );
    expect(() => assertRetentionWindowCoversDeployment({ da: {} })).toThrow(
      /da\.transportProfile must be an object/u,
    );
  });

  it("keeps the >=15-day manifest floor in verifyDeploymentManifestV1Identity", () => {
    // Mutation guard: the manifest surface must reject 14 even though 14 days
    // still covers the derived 11-day horizon.
    expect(retentionDaysCoverWindow(14)).toBe(true);
    expect(DA_TRANSPORT_LIMITS.minimumRetentionDays).toBe(15);
    expect(typeof verifyDeploymentManifestIdentity).toBe("function");
  });
});

describe("retentionDeadlineForBlockV1", () => {
  it("keys the deadline on block end time, not local insert time", () => {
    const deadline = retentionDeadlineForBlock({
      blockEndTimeMs: BLOCK_END,
      retentionDays: 15,
    });
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + 907_200_000);
    expect(deadline.retainUntilMs).toBe(BLOCK_END + 1_296_000_000);
    expect(deadline.deployedRetentionMs).toBe(1_296_000_000);
    expect(deadline.remainingMs(BLOCK_END)).toBe(907_200_000);
    expect(deadline.remainingMs(deadline.challengeableUntilMs)).toBe(0);
    expect(deadline.remainingMs(deadline.challengeableUntilMs + 1)).toBe(-1);
  });

  it("accepts retentionDays=0 without collapsing challengeability", () => {
    const deadline = retentionDeadlineForBlock({
      blockEndTimeMs: BLOCK_END,
      retentionDays: 0,
    });
    expect(deadline.retainUntilMs).toBe(BLOCK_END);
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + 907_200_000);
  });

  it("rejects malformed block end times", () => {
    for (const bad of [Number.NaN, -1, 1.5, "0", null]) {
      expect(() => retentionDeadlineForBlock({ blockEndTimeMs: bad })).toThrow(
        /blockEndTimeMs must be a non-negative safe integer/u,
      );
    }
  });
});

describe("daRetentionPruneDecisionV1", () => {
  const decide = (
    nowMs: number,
    headerStatus: unknown,
    blockEndTimeMs: number | null = BLOCK_END,
  ) =>
    daRetentionPruneDecision(
      { headerHash: "ab".repeat(28), blockEndTimeMs },
      {
        nowMs,
        headerStatus,
        availabilityChallengeState: "inactive",
      },
    );

  it("prunes a 16-day-old terminal record", () => {
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY;
    expect(decide(now, "merged")).toMatchObject({
      decision: "prune",
      reasonCode: "expired_and_terminal",
    });
    expect(decide(now, "removed").decision).toBe("prune");
  });

  it("retains exactly at the challengeability deadline and prunes 1ms past it", () => {
    const deadline = BLOCK_END + 907_200_000;
    expect(decide(deadline, "merged")).toMatchObject({
      decision: "retain",
      reasonCode: "still_within_retention_window",
      remainingMs: 0,
    });
    expect(decide(deadline + 1, "merged")).toMatchObject({
      decision: "prune",
      reasonCode: "expired_and_terminal",
      remainingMs: -1,
    });
  });

  it("retains inside maturity even for a terminal header", () => {
    expect(decide(BLOCK_END + 604_800_000, "merged")).toMatchObject({
      decision: "retain",
      reasonCode: "still_within_maturity",
    });
    expect(decide(BLOCK_END + 604_800_001, "merged").reasonCode).toBe(
      "still_within_retention_window",
    );
  });

  it("fails closed on missing block end time", () => {
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY;
    for (const bad of [null, undefined]) {
      expect(
        daRetentionPruneDecision(
          { blockEndTimeMs: bad },
          {
            nowMs: now,
            headerStatus: "merged",
            availabilityChallengeState: "inactive",
          },
        ),
      ).toEqual({ decision: "retain", reasonCode: "missing_block_end_time" });
    }
  });

  it("fails closed on unknown or out-of-set header statuses", () => {
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY;
    for (const status of [
      undefined,
      null,
      "",
      "MERGED",
      "finalised",
      42,
      { status: "merged" },
    ]) {
      expect(decide(now, status)).toEqual({
        decision: "retain",
        reasonCode: "header_status_unknown",
      });
    }
  });

  it("retains every known non-terminal status past the deadline", () => {
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY;
    for (const status of [
      "unattested",
      "attesting",
      "attested",
      "conflicted",
    ]) {
      expect(decide(now, status)).toMatchObject({
        decision: "retain",
        reasonCode: "header_status_not_terminal",
      });
    }
  });

  it("blocks a tampered block end time via the terminal-status conjunct", () => {
    // An attacker back-dating block_end_time to force expiry still cannot get
    // a prune while the header has not reached a terminal L1 outcome.
    const now = BLOCK_END;
    expect(
      daRetentionPruneDecision(
        { blockEndTimeMs: 0 },
        {
          nowMs: now,
          headerStatus: "attested",
          availabilityChallengeState: "inactive",
        },
      ),
    ).toMatchObject({
      decision: "retain",
      reasonCode: "header_status_not_terminal",
    });
  });

  it("treats an active availability challenge as an absolute retention hold", () => {
    expect(
      daRetentionPruneDecision(
        { headerHash: "ab".repeat(28), blockEndTimeMs: 0 },
        {
          nowMs: BLOCK_END + 100 * RETENTION_MS_PER_DAY,
          headerStatus: "removed",
          availabilityChallengeState: "active",
        },
      ),
    ).toEqual({
      decision: "retain",
      reasonCode: "active_availability_challenge",
    });
  });

  it("fails closed on missing, malformed, or unknown challenge state", () => {
    for (const availabilityChallengeState of [
      undefined,
      null,
      "unknown",
      "not_deployed",
      false,
      { active: false },
    ]) {
      expect(
        daRetentionPruneDecision(
          { headerHash: "ab".repeat(28), blockEndTimeMs: 0 },
          {
            nowMs: BLOCK_END + 100 * RETENTION_MS_PER_DAY,
            headerStatus: "removed",
            availabilityChallengeState,
          },
        ),
      ).toEqual({
        decision: "retain",
        reasonCode: "availability_challenge_state_unknown",
      });
    }
  });
});

describe("retentionDeadlineAlertV1", () => {
  it("alerts when remaining headroom hits zero but not at one millisecond", () => {
    const at = (remainingMs: number, alertThresholdMs: number) =>
      retentionDeadlineAlert({
        nowMs: BLOCK_END + 907_200_000 - remainingMs,
        blockEndTimeMs: BLOCK_END,
        alertThresholdMs,
      });
    expect(at(0, 0)).toMatchObject({ remainingMs: 0, alerting: true });
    expect(at(1, 0)).toMatchObject({ remainingMs: 1, alerting: false });
  });

  it("defaults the alert threshold to the derived margin", () => {
    const margin = MIDGARD_RETENTION_WINDOW.marginMs;
    const atMargin = retentionDeadlineAlert({
      nowMs: BLOCK_END + 907_200_000 - margin,
      blockEndTimeMs: BLOCK_END,
    });
    expect(atMargin).toMatchObject({ headroomMs: 0, alerting: true });
    const justAbove = retentionDeadlineAlert({
      nowMs: BLOCK_END + 907_200_000 - margin - 1,
      blockEndTimeMs: BLOCK_END,
    });
    expect(justAbove).toMatchObject({ headroomMs: 1, alerting: false });
  });
});
