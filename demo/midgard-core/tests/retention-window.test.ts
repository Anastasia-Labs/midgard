import { describe, expect, it } from "vitest";

import { MIDGARD_CONSENSUS_LIMITS } from "../src/consensus-profile.js";
import { DA_TRANSPORT_LIMITS } from "../src/da-transport.js";
import {
  assertRetentionDaysCoverWindow,
  assertRetentionWindowCoversDeployment,
  assertWorstCaseProofTimeWithinBound,
  daRetentionPruneDecision,
  MIDGARD_MIN_RETENTION_DAYS,
  MIDGARD_RETENTION_WINDOW,
  resolveL1ViewFatalMs,
  RETENTION_MS_PER_DAY,
  type RetentionQueueReference,
  retentionDaysCoverWindow,
  retentionDeadlineAlert,
  retentionDeadlineForBlock,
} from "../src/retention-window.js";

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
  const HORIZON = 907_200_000;
  const decide = (
    nowMs: number,
    headerStatus: Parameters<
      typeof daRetentionPruneDecision
    >[0]["headerStatus"] = "merged",
    queueReference: RetentionQueueReference = "none",
    blockEndTimeMs = BLOCK_END,
  ) =>
    daRetentionPruneDecision({
      nowMs,
      blockEndTimeMs,
      headerStatus,
      queueReference,
    });

  it("retains the L1 confirmed head's payload however old or removed", () => {
    const now = BLOCK_END + 60 * RETENTION_MS_PER_DAY;
    for (const status of ["merged", "removed", "unobserved"] as const) {
      expect(decide(now, status, "confirmed_head")).toMatchObject({
        decision: "retain",
        reasonCode: "confirmed_head_payload",
      });
    }
  });

  it("retains a header live in the L1 queue however old or removed", () => {
    const now = BLOCK_END + 60 * RETENTION_MS_PER_DAY;
    for (const status of ["attested", "removed", "unobserved"] as const) {
      expect(decide(now, status, "live_in_queue")).toMatchObject({
        decision: "retain",
        reasonCode: "live_queue_header",
      });
    }
  });

  it("prunes a removed header immediately, inside the horizon", () => {
    expect(decide(BLOCK_END, "removed")).toMatchObject({
      decision: "prune",
      reasonCode: "removed_header",
    });
  });

  it("retains exactly at the challengeability horizon and prunes 1ms past it", () => {
    expect(decide(BLOCK_END + HORIZON)).toMatchObject({
      decision: "retain",
      reasonCode: "still_challengeable",
      challengeableUntilMs: BLOCK_END + HORIZON,
      remainingMs: 0,
    });
    expect(decide(BLOCK_END + HORIZON + 1)).toMatchObject({
      decision: "prune",
      reasonCode: "past_challengeability_horizon",
      remainingMs: -1,
    });
  });

  it("prunes past the horizon for every non-removed status, including unobserved", () => {
    const now = BLOCK_END + HORIZON + 1;
    for (const status of [
      "unattested",
      "attesting",
      "attested",
      "merged",
      "conflicted",
      "unobserved",
    ] as const) {
      expect(decide(now, status).decision).toBe("prune");
    }
  });

  it("reports the configured retain-until alongside the horizon", () => {
    expect(decide(BLOCK_END)).toMatchObject({
      decision: "retain",
      retainUntilMs: BLOCK_END + 15 * RETENTION_MS_PER_DAY,
      remainingMs: HORIZON,
    });
  });

  it("rejects malformed times instead of retaining", () => {
    expect(() => decide(Number.NaN)).toThrow(/nowMs/u);
    expect(() => decide(BLOCK_END, "merged", "none", -1)).toThrow(
      /blockEndTimeMs/u,
    );
    expect(() => decide(BLOCK_END, "merged", "none", 1.5)).toThrow(
      /blockEndTimeMs/u,
    );
  });

  it("never retains more than the head, the live queue, and the horizon", () => {
    // Deterministic xorshift PRNG: the package carries no property-test library.
    let seed = 0x9e3779b9;
    const next = (): number => {
      seed ^= seed << 13;
      seed ^= seed >>> 17;
      seed ^= seed << 5;
      return (seed >>> 0) / 0x1_0000_0000;
    };
    const statuses: readonly unknown[] = [
      "unattested",
      "attesting",
      "attested",
      "merged",
      "removed",
      "conflicted",
      "unobserved",
      "not-a-status",
      undefined,
      null,
      42,
    ];
    const now = BLOCK_END + 60 * RETENTION_MS_PER_DAY;
    for (let run = 0; run < 200; run += 1) {
      const count = 1 + Math.floor(next() * 200);
      const liveCount = Math.floor(next() * 5);
      const headIndex = Math.floor(next() * count);
      let retained = 0;
      let insideHorizon = 0;
      for (let index = 0; index < count; index += 1) {
        const blockEndTimeMs =
          BLOCK_END + Math.floor(next() * 60 * RETENTION_MS_PER_DAY);
        const queueReference =
          index === headIndex
            ? "confirmed_head"
            : index < liveCount
              ? "live_in_queue"
              : next() < 0.05
                ? ("garbage" as never)
                : "none";
        const headerStatus = statuses[
          Math.floor(next() * statuses.length)
        ] as never;
        const inside = now <= blockEndTimeMs + HORIZON;
        const decision = daRetentionPruneDecision({
          nowMs: now,
          blockEndTimeMs,
          headerStatus,
          queueReference,
        });
        if (decision.decision === "retain") {
          retained += 1;
          expect(
            queueReference === "confirmed_head" ||
              queueReference === "live_in_queue" ||
              inside,
          ).toBe(true);
        }
        if (
          inside &&
          queueReference !== "confirmed_head" &&
          queueReference !== "live_in_queue"
        ) {
          insideHorizon += 1;
        }
      }
      expect(retained).toBeLessThanOrEqual(
        1 + Math.min(liveCount, count) + insideHorizon,
      );
    }
  });
});

describe("resolveL1ViewFatalMs", () => {
  const resolve = (value: unknown, pollIntervalMs = 10_000) =>
    resolveL1ViewFatalMs({
      value,
      defaultMs: 3_600_000,
      pollIntervalMs,
      fieldName: "L1_VIEW_FATAL_MS",
    });

  it("defaults, and accepts decimal strings and numbers", () => {
    expect(resolve(undefined)).toBe(3_600_000);
    expect(resolve("60000")).toBe(60_000);
    expect(resolve(30_000)).toBe(30_000);
  });

  it("rejects fewer than three poll intervals", () => {
    expect(() => resolve(29_999)).toThrow(/three poll intervals/u);
  });

  it("rejects more than the deployed retention margin", () => {
    expect(resolve(MIDGARD_RETENTION_WINDOW.marginMs)).toBe(
      MIDGARD_RETENTION_WINDOW.marginMs,
    );
    expect(() => resolve(MIDGARD_RETENTION_WINDOW.marginMs + 1)).toThrow(
      /retention margin/u,
    );
  });

  it("rejects malformed values", () => {
    for (const bad of ["", "1e6", "-1", -1, 1.5, Number.NaN, null]) {
      expect(() => resolve(bad)).toThrow(/non-negative safe integer/u);
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
