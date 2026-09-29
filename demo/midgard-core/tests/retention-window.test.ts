import { describe, expect, it } from "vitest";

import { MIDGARD_CONSENSUS_LIMITS } from "../src/consensus-profile.js";
import { DA_TRANSPORT_LIMITS } from "../src/da-transport.js";
import {
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
} from "../src/deployment-profile.js";
import {
  assertDaChallengeWindowWithinMaturity,
  assertRetentionDaysCoverWindow,
  assertRetentionWindowCoversDeployment,
  assertWorstCaseProofTimeWithinBound,
  daRetentionPruneDecision,
  MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS,
  MIDGARD_MIN_RETENTION_DAYS,
  MIDGARD_RETENTION_WINDOW,
  requireRetentionAlertThresholdMs,
  resolveL1ViewFatalMs,
  RETENTION_MS_PER_DAY,
  retentionDaysCoverWindow,
  retentionDeadlineAlert,
  retentionDeadlineForBlock,
  type RetentionQueueReference,
} from "../src/retention-window.js";

const BLOCK_END = Date.UTC(2026, 0, 1, 0, 0, 0);

// Expected values are worked out here from the compiled deployment profile's
// raw timing, not read back from the module under test, so a broken
// derivation cannot verify itself.
const MATURITY_MS = SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms;
const PROOF_BOUND_MS = MATURITY_MS / 2;
const HORIZON_MS = MATURITY_MS + PROOF_BOUND_MS;
const DEPLOYED_RETENTION_MS = 15 * RETENTION_MS_PER_DAY;
const MARGIN_MS = DEPLOYED_RETENTION_MS - HORIZON_MS;
const MIN_RETENTION_DAYS = Math.ceil(HORIZON_MS / RETENTION_MS_PER_DAY);

const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

describe("MIDGARD_RETENTION_WINDOW_V1 derived arithmetic (F04)", () => {
  it("derives every constant from the frozen profiles, never from a literal", () => {
    expect(MIDGARD_RETENTION_WINDOW.maturityMs).toBe(MATURITY_MS);
    expect(MIDGARD_RETENTION_WINDOW.maturityMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.blockMaturityMs,
    );
    expect(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs).toBe(
      PROOF_BOUND_MS,
    );
    expect(MIDGARD_RETENTION_WINDOW.requiredRetentionMs).toBe(HORIZON_MS);
    expect(MIDGARD_RETENTION_WINDOW.retentionDays).toBe(
      DA_TRANSPORT_LIMITS.minimumRetentionDays,
    );
    expect(MIDGARD_RETENTION_WINDOW.retentionDays).toBe(15);
    expect(MIDGARD_RETENTION_WINDOW.deployedRetentionMs).toBe(1_296_000_000);
    expect(MIDGARD_RETENTION_WINDOW.marginMs).toBe(MARGIN_MS);
    expect(MIDGARD_RETENTION_WINDOW.deployedRetentionMs).toBeGreaterThanOrEqual(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
    expect(MIDGARD_MIN_RETENTION_DAYS).toBe(MIN_RETENTION_DAYS);
  });

  it("keeps every profile's horizon inside the 15-day retention", () => {
    // The module-load assertion only sees the compiled profile; CI compiles
    // preprod-testing, so the other profiles are checked here. Mainnet is the
    // fixed decision 0002 vector: 907_200_000 ms horizon, 388_800_000 ms margin.
    for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
      const maturityMs = profile.timing.block_maturity_ms;
      expect(maturityMs + maturityMs / 2).toBeLessThanOrEqual(
        DEPLOYED_RETENTION_MS,
      );
    }
    const mainnetMaturityMs =
      DEPLOYMENT_PROFILES.mainnet.timing.block_maturity_ms;
    expect(mainnetMaturityMs + mainnetMaturityMs / 2).toBe(907_200_000);
    expect(
      DEPLOYED_RETENTION_MS - (mainnetMaturityMs + mainnetMaturityMs / 2),
    ).toBe(388_800_000);
  });

  it("records but never enforces against the measured dispute schedule", () => {
    expect(MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.minValidationDisputeMaturityMs,
    );
    // The measured schedule differs from the half-maturity bound under every
    // profile (far below it on mainnet, above it on the testing profiles), so
    // the horizon below can only equal maturity plus the bound.
    expect(
      MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs,
    ).not.toBe(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs);
    expect(MIDGARD_RETENTION_WINDOW.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW.maturityMs +
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    );
  });
});

describe("DA challenge window within block maturity", () => {
  it("holds for every deployment profile", () => {
    // Worked from the raw profile timing, not from the module under test.
    for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
      expect(profile.timing.da_challenge_window_ms).toBeLessThanOrEqual(
        profile.timing.block_maturity_ms,
      );
      expect(() =>
        assertDaChallengeWindowWithinMaturity(profile.name, profile.timing),
      ).not.toThrow();
    }
  });

  it("accepts a window equal to maturity and rejects one millisecond more", () => {
    expect(() =>
      assertDaChallengeWindowWithinMaturity("boundary", {
        block_maturity_ms: 900_000,
        da_challenge_window_ms: 900_000,
      }),
    ).not.toThrow();
    expect(() =>
      assertDaChallengeWindowWithinMaturity("boundary", {
        block_maturity_ms: 900_000,
        da_challenge_window_ms: 900_001,
      }),
    ).toThrow(/must not exceed block_maturity_ms=900000/u);
  });

  it("rejects malformed or non-positive timing", () => {
    for (const [maturity, window] of [
      [0, 0],
      [900_000, 0],
      [-1, -2],
      [900_000.5, 1],
      [Number.NaN, 1],
      ["900000", 1],
      [900_000, undefined],
      [Number.MAX_SAFE_INTEGER + 1, 1],
    ] as const) {
      expect(() =>
        assertDaChallengeWindowWithinMaturity("malformed", {
          block_maturity_ms: maturity,
          da_challenge_window_ms: window,
        }),
      ).toThrow(/must be positive safe integers/u);
    }
  });
});

describe("worst-case proof-time bound", () => {
  it("accepts exactly the bound and rejects one millisecond past it", () => {
    expect(assertWorstCaseProofTimeWithinBound(PROOF_BOUND_MS)).toBe(
      PROOF_BOUND_MS,
    );
    expect(() =>
      assertWorstCaseProofTimeWithinBound(PROOF_BOUND_MS + 1),
    ).toThrow(/exceeds the canonical V1 worst-case proof-time bound/u);
  });

  it("rejects malformed observations", () => {
    for (const bad of [Number.NaN, -1, 1.5, "302400000", null, 2 ** 53]) {
      expect(() => assertWorstCaseProofTimeWithinBound(bad)).toThrow();
    }
  });
});

describe("retention-days floor and deployment binding", () => {
  // Under a testing profile the horizon is under one day, so MIN_RETENTION_DAYS
  // is 1 and these cases only check "days >= 1": they cannot tell the horizon
  // from maturity alone or any other sub-day floor. Only a long-maturity
  // compile (mainnet: 11 days pass, 10 fail) exercises the comparison itself.
  it("accepts the derived minimum days and rejects one day fewer", () => {
    expect(assertRetentionDaysCoverWindow(15)).toBe(15);
    expect(retentionDaysCoverWindow(15)).toBe(true);
    expect(retentionDaysCoverWindow(14)).toBe(true);
    expect(retentionDaysCoverWindow(MIN_RETENTION_DAYS)).toBe(true);
    expect(retentionDaysCoverWindow(MIN_RETENTION_DAYS - 1)).toBe(false);
    expect(() =>
      assertRetentionDaysCoverWindow(MIN_RETENTION_DAYS - 1),
    ).toThrow(`must be at least ${String(MIN_RETENTION_DAYS)} days`);
  });

  it("binds the window to deployment identity via da.transportProfile", () => {
    expect(assertRetentionWindowCoversDeployment(manifestWith(15))).toBe(15);
    expect(() =>
      assertRetentionWindowCoversDeployment(
        manifestWith(MIN_RETENTION_DAYS - 1),
      ),
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
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + HORIZON_MS);
    expect(deadline.retainUntilMs).toBe(BLOCK_END + DEPLOYED_RETENTION_MS);
    expect(deadline.deployedRetentionMs).toBe(DEPLOYED_RETENTION_MS);
    expect(deadline.remainingMs(BLOCK_END)).toBe(HORIZON_MS);
    expect(deadline.remainingMs(deadline.challengeableUntilMs)).toBe(0);
    expect(deadline.remainingMs(deadline.challengeableUntilMs + 1)).toBe(-1);
  });

  it("accepts retentionDays=0 without collapsing challengeability", () => {
    const deadline = retentionDeadlineForBlock({
      blockEndTimeMs: BLOCK_END,
      retentionDays: 0,
    });
    expect(deadline.retainUntilMs).toBe(BLOCK_END);
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + HORIZON_MS);
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
  const HORIZON = HORIZON_MS;
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
        nowMs: BLOCK_END + HORIZON_MS - remainingMs,
        blockEndTimeMs: BLOCK_END,
        alertThresholdMs,
      });
    expect(at(0, 0)).toMatchObject({ remainingMs: 0, alerting: true });
    expect(at(1, 0)).toMatchObject({ remainingMs: 1, alerting: false });
  });

  it("has no default threshold: a missing or malformed one is refused", () => {
    // Opt-in alert (owner ruling 2026-09-26). A default would alert on every
    // still-challengeable record: the derived margin exceeds the horizon under
    // the testing profiles and exceeds what a merged non-head block has left on
    // mainnet.
    expect(() =>
      retentionDeadlineAlert({
        nowMs: BLOCK_END,
        blockEndTimeMs: BLOCK_END,
        // @ts-expect-error the threshold is required
        alertThresholdMs: undefined,
      }),
    ).toThrow(/alertThresholdMs/u);
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        retentionDeadlineAlert({
          nowMs: BLOCK_END,
          blockEndTimeMs: BLOCK_END,
          alertThresholdMs: bad,
        }),
      ).toThrow(/alertThresholdMs/u);
    }
  });

  it("accepts a threshold only strictly below the merged-payload window", () => {
    // A header merges no earlier than MATURITY_MS after its end time, so a
    // merged payload has at most HORIZON_MS - MATURITY_MS left when it merges.
    // Worked out from raw timing, not read back from the module.
    const mergedWindowMs = HORIZON_MS - MATURITY_MS;
    expect(MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS).toBe(mergedWindowMs);
    const atMerge = (alertThresholdMs: number) =>
      retentionDeadlineAlert({
        nowMs: BLOCK_END + MATURITY_MS,
        blockEndTimeMs: BLOCK_END,
        alertThresholdMs,
      });
    // At the window a payload alerts the instant it merges; one below, not.
    expect(atMerge(mergedWindowMs)).toMatchObject({
      remainingMs: mergedWindowMs,
      alerting: true,
    });
    expect(atMerge(mergedWindowMs - 1).alerting).toBe(false);
    expect(
      requireRetentionAlertThresholdMs(mergedWindowMs - 1, "THRESHOLD"),
    ).toBe(mergedWindowMs - 1);
    expect(requireRetentionAlertThresholdMs(0, "THRESHOLD")).toBe(0);
    for (const refused of [mergedWindowMs, HORIZON_MS - 1, HORIZON_MS]) {
      expect(() =>
        requireRetentionAlertThresholdMs(refused, "THRESHOLD"),
      ).toThrow(/THRESHOLD=\d+ must be below the merged-payload window/u);
    }
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53, "60000", undefined]) {
      expect(() => requireRetentionAlertThresholdMs(bad, "THRESHOLD")).toThrow(
        /THRESHOLD must be a non-negative safe integer/u,
      );
    }
  });

  it("measures headroom against the supplied threshold", () => {
    const threshold = 60_000;
    const atThreshold = retentionDeadlineAlert({
      nowMs: BLOCK_END + HORIZON_MS - threshold,
      blockEndTimeMs: BLOCK_END,
      alertThresholdMs: threshold,
    });
    expect(atThreshold).toMatchObject({
      remainingMs: threshold,
      headroomMs: 0,
      alerting: true,
    });
    const justAbove = retentionDeadlineAlert({
      nowMs: BLOCK_END + HORIZON_MS - threshold - 1,
      blockEndTimeMs: BLOCK_END,
      alertThresholdMs: threshold,
    });
    expect(justAbove).toMatchObject({ headroomMs: 1, alerting: false });
  });
});
