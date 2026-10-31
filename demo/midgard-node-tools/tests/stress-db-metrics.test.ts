import { describe, expect, it } from "vitest";

import {
  buildSteadyStateStageResult,
  percentile,
  trimByEventFraction,
} from "../src/commands/stress-db-metrics.js";
import { collectEnvironmentFingerprint } from "../src/commands/stress-environment-fingerprint.js";

describe("stress DB metrics", () => {
  it("computes nearest-rank percentiles", () => {
    expect(percentile([1, 2, 3, 4, 5], 0.5)).toBe(3);
    expect(percentile([1, 2, 3, 4, 5], 0.95)).toBe(5);
    expect(percentile([], 0.99)).toBeNull();
  });

  it("trims by event fraction rather than wall-clock duration", () => {
    expect(trimByEventFraction([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.1)).toEqual([
      2, 3, 4, 5, 6, 7, 8, 9,
    ]);
  });

  it("builds steady-state rates and latency percentiles over the trimmed rows", () => {
    const base = Date.parse("2026-01-01T00:00:00.000Z");
    const rows = Array.from({ length: 10 }, (_unused, index) => ({
      observedAt: new Date(base + index * 1_000),
      latencyMs: index * 10,
    }));

    const result = buildSteadyStateStageResult({
      stage: "validation_terminal",
      rows,
      offeredCount: 10,
      trimFraction: 0.1,
      timestamp: (row) => row.observedAt,
      latencyMs: (row) => row.latencyMs,
    });

    expect(result.rawCount).toBe(10);
    expect(result.trimmedCount).toBe(8);
    expect(result.rawPerSecond).toBeCloseTo(10 / 9);
    expect(result.steadyStatePerSecond).toBeCloseTo(8 / 7);
    expect(result.latency).toMatchObject({
      p50Ms: 40,
      p95Ms: 80,
      p99Ms: 80,
      sampleCount: 8,
    });
    expect(result.windowTrim).toEqual({
      discardedHeadMs: 1_000,
      discardedTailMs: 1_000,
    });
  });
});

describe("stress environment fingerprint", () => {
  it("records fixed throughput knobs and hashes config profiles", async () => {
    const fingerprint = await collectEnvironmentFingerprint({
      cwd: "/tmp/does-not-exist",
      env: {
        MIDGARD_BUILD_GIT_SHA: "abc123",
        STRESS_LOAD_GENERATOR_PLACEMENT: "separate-host",
        STRESS_LOADGEN_COHOSTED: "false",
        STRESS_CLOCK_OFFSET_MS: "3",
      },
      configProfile: { b: 2, a: 1 },
      calibrationProofRef: "./noop-calibration.json",
    });

    expect(fingerprint.gitSha).toBe("abc123");
    expect(fingerprint.loadGeneratorPlacement).toBe("separate-host");
    expect(fingerprint.loadGenCoHosted).toBe(false);
    expect(fingerprint.clockOffsetMs).toBe(3);
    expect(fingerprint.calibrationProofRef).toBe("./noop-calibration.json");
    expect(fingerprint.configProfileHash).toMatch(/^[0-9a-f]{64}$/u);
    expect(fingerprint.fixedKnobs).toMatchObject({
      nodePostgresPoolMaxConnections: 20,
      validationBatchHardCap: 1600,
      validationMinBatch: 128,
      validationPhaseAMaxEffectiveConcurrency: 8,
    });
  });
});
