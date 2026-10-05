import { describe, expect, it } from "vitest";

import {
  daRetentionPruneDecision,
  MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS,
  MIDGARD_RETENTION_WINDOW,
  requireRetentionAlertThresholdMs,
  resolveL1ViewFatalMs,
  RETENTION_MS_PER_DAY,
  retentionDeadlineAlert,
  type RetentionQueueReference,
} from "../src/retention-window.js";
import {
  BLOCK_END,
  HORIZON_MS,
  MATURITY_MS,
} from "./retention-window.midgard-retention-window-v1-derived-arithmetic-f04.js";

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
      terminalRecoveryFinal: true,
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

  it.each(["merged", "removed"] as const)(
    "retains a provisional %s even past the wall-clock horizon",
    (headerStatus) => {
      for (const terminalRecoveryFinal of [undefined, false]) {
        expect(
          daRetentionPruneDecision({
            nowMs: BLOCK_END + HORIZON + 1,
            blockEndTimeMs: BLOCK_END,
            headerStatus,
            queueReference: "none",
            terminalRecoveryFinal,
          }),
        ).toMatchObject({
          decision: "retain",
          reasonCode: "terminal_recovery_pending",
        });
      }
    },
  );

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
          terminalRecoveryFinal: true,
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
