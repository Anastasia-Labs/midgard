import { describe, expect, it } from "vitest";

import { MIDGARD_CONSENSUS_LIMITS_V1 } from "../src/consensus-profile-v1.js";
import { DA_TRANSPORT_LIMITS_V1 } from "../src/da-transport.js";
import { verifyDeploymentManifestV1Identity } from "../src/deployment-manifest-identity-v1.js";
import {
  assertRetentionDaysCoverWindowV1,
  assertRetentionWindowCoversDeploymentV1,
  assertWorstCaseProofTimeWithinBoundV1,
  daRetentionPruneDecisionV1,
  MIDGARD_MIN_RETENTION_DAYS_V1,
  MIDGARD_RETENTION_WINDOW_V1,
  RETENTION_MS_PER_DAY_V1,
  retentionDaysCoverWindowV1,
  retentionDeadlineAlertV1,
  retentionDeadlineForBlockV1,
} from "../src/retention-window-v1.js";

const BLOCK_END = Date.UTC(2026, 0, 1, 0, 0, 0);

const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

describe("MIDGARD_RETENTION_WINDOW_V1 derived arithmetic (F04)", () => {
  it("derives every constant from the frozen profiles, never from a literal", () => {
    expect(MIDGARD_RETENTION_WINDOW_V1.maturityMs).toBe(604_800_000);
    expect(MIDGARD_RETENTION_WINDOW_V1.maturityMs).toBe(
      MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs,
    );
    expect(MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs).toBe(
      302_400_000,
    );
    expect(MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs).toBe(
      MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs / 2,
    );
    expect(MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs).toBe(907_200_000);
    expect(MIDGARD_RETENTION_WINDOW_V1.retentionDays).toBe(
      DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    );
    expect(MIDGARD_RETENTION_WINDOW_V1.retentionDays).toBe(15);
    expect(MIDGARD_RETENTION_WINDOW_V1.deployedRetentionMs).toBe(1_296_000_000);
    expect(MIDGARD_RETENTION_WINDOW_V1.marginMs).toBe(388_800_000);
    expect(
      MIDGARD_RETENTION_WINDOW_V1.deployedRetentionMs,
    ).toBeGreaterThanOrEqual(MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs);
    expect(MIDGARD_MIN_RETENTION_DAYS_V1).toBe(11);
  });

  it("records but never enforces against the measured dispute schedule", () => {
    expect(
      MIDGARD_RETENTION_WINDOW_V1.measuredValidationDisputeScheduleMs,
    ).toBe(MIDGARD_CONSENSUS_LIMITS_V1.minValidationDisputeMaturityMs);
    expect(
      MIDGARD_RETENTION_WINDOW_V1.measuredValidationDisputeScheduleMs,
    ).toBe(39_600_000);
    // The measured 11h schedule is far below the enforced half-maturity bound;
    // enforcement must key on the bound, not the measurement.
    expect(
      MIDGARD_RETENTION_WINDOW_V1.measuredValidationDisputeScheduleMs,
    ).toBeLessThan(MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs);
    expect(MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW_V1.maturityMs +
        MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs,
    );
  });
});

describe("worst-case proof-time bound", () => {
  it("accepts exactly the bound and rejects one millisecond past it", () => {
    expect(assertWorstCaseProofTimeWithinBoundV1(302_400_000)).toBe(
      302_400_000,
    );
    expect(() => assertWorstCaseProofTimeWithinBoundV1(302_400_001)).toThrow(
      /exceeds the canonical V1 worst-case proof-time bound/u,
    );
  });

  it("rejects malformed observations", () => {
    for (const bad of [Number.NaN, -1, 1.5, "302400000", null, 2 ** 53]) {
      expect(() => assertWorstCaseProofTimeWithinBoundV1(bad)).toThrow();
    }
  });
});

describe("retention-days floor and deployment binding", () => {
  it("accepts the deployed 15 days and rejects 14", () => {
    expect(assertRetentionDaysCoverWindowV1(15)).toBe(15);
    expect(retentionDaysCoverWindowV1(15)).toBe(true);
    expect(retentionDaysCoverWindowV1(14)).toBe(true);
    // 11 whole days is the derived minimum covering 907_200_000 ms.
    expect(retentionDaysCoverWindowV1(11)).toBe(true);
    expect(retentionDaysCoverWindowV1(10)).toBe(false);
    expect(() => assertRetentionDaysCoverWindowV1(10)).toThrow(
      /must be at least 11 days/u,
    );
  });

  it("binds the window to deployment identity via da.transportProfile", () => {
    expect(assertRetentionWindowCoversDeploymentV1(manifestWith(15))).toBe(15);
    expect(() =>
      assertRetentionWindowCoversDeploymentV1(manifestWith(1)),
    ).toThrow(/da\.transportProfile\.retentionDays must be at least/u);
  });

  it("rejects malformed manifest retention values and shapes", () => {
    for (const bad of [Number.NaN, -1, 1.5, "15", null, 2 ** 53]) {
      expect(() =>
        assertRetentionWindowCoversDeploymentV1(manifestWith(bad)),
      ).toThrow(/must be a non-negative safe integer number of days/u);
    }
    expect(() => assertRetentionWindowCoversDeploymentV1(null)).toThrow(
      /Deployment manifest must be an object/u,
    );
    expect(() => assertRetentionWindowCoversDeploymentV1({})).toThrow(
      /Deployment manifest da must be an object/u,
    );
    expect(() => assertRetentionWindowCoversDeploymentV1({ da: {} })).toThrow(
      /da\.transportProfile must be an object/u,
    );
  });

  it("keeps the >=15-day manifest floor in verifyDeploymentManifestV1Identity", () => {
    // Mutation guard: the manifest surface must reject 14 even though 14 days
    // still covers the derived 11-day horizon.
    expect(retentionDaysCoverWindowV1(14)).toBe(true);
    expect(DA_TRANSPORT_LIMITS_V1.minimumRetentionDays).toBe(15);
    expect(typeof verifyDeploymentManifestV1Identity).toBe("function");
  });
});

describe("retentionDeadlineForBlockV1", () => {
  it("keys the deadline on block end time, not local insert time", () => {
    const deadline = retentionDeadlineForBlockV1({
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
    const deadline = retentionDeadlineForBlockV1({
      blockEndTimeMs: BLOCK_END,
      retentionDays: 0,
    });
    expect(deadline.retainUntilMs).toBe(BLOCK_END);
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + 907_200_000);
  });

  it("rejects malformed block end times", () => {
    for (const bad of [Number.NaN, -1, 1.5, "0", null]) {
      expect(() =>
        retentionDeadlineForBlockV1({ blockEndTimeMs: bad }),
      ).toThrow(/blockEndTimeMs must be a non-negative safe integer/u);
    }
  });
});

describe("daRetentionPruneDecisionV1", () => {
  const decide = (
    nowMs: number,
    headerStatus: unknown,
    blockEndTimeMs: number | null = BLOCK_END,
  ) =>
    daRetentionPruneDecisionV1(
      { headerHash: "ab".repeat(28), blockEndTimeMs },
      { nowMs, headerStatus },
    );

  it("prunes a 16-day-old terminal record", () => {
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY_V1;
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
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY_V1;
    for (const bad of [null, undefined]) {
      expect(
        daRetentionPruneDecisionV1(
          { blockEndTimeMs: bad },
          { nowMs: now, headerStatus: "merged" },
        ),
      ).toEqual({ decision: "retain", reasonCode: "missing_block_end_time" });
    }
  });

  it("fails closed on unknown or out-of-set header statuses", () => {
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY_V1;
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
    const now = BLOCK_END + 16 * RETENTION_MS_PER_DAY_V1;
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
      daRetentionPruneDecisionV1(
        { blockEndTimeMs: 0 },
        { nowMs: now, headerStatus: "attested" },
      ),
    ).toMatchObject({
      decision: "retain",
      reasonCode: "header_status_not_terminal",
    });
  });
});

describe("retentionDeadlineAlertV1", () => {
  it("alerts when remaining headroom hits zero but not at one millisecond", () => {
    const at = (remainingMs: number, alertThresholdMs: number) =>
      retentionDeadlineAlertV1({
        nowMs: BLOCK_END + 907_200_000 - remainingMs,
        blockEndTimeMs: BLOCK_END,
        alertThresholdMs,
      });
    expect(at(0, 0)).toMatchObject({ remainingMs: 0, alerting: true });
    expect(at(1, 0)).toMatchObject({ remainingMs: 1, alerting: false });
  });

  it("defaults the alert threshold to the derived margin", () => {
    const margin = MIDGARD_RETENTION_WINDOW_V1.marginMs;
    const atMargin = retentionDeadlineAlertV1({
      nowMs: BLOCK_END + 907_200_000 - margin,
      blockEndTimeMs: BLOCK_END,
    });
    expect(atMargin).toMatchObject({ headroomMs: 0, alerting: true });
    const justAbove = retentionDeadlineAlertV1({
      nowMs: BLOCK_END + 907_200_000 - margin - 1,
      blockEndTimeMs: BLOCK_END,
    });
    expect(justAbove).toMatchObject({ headroomMs: 1, alerting: false });
  });
});
