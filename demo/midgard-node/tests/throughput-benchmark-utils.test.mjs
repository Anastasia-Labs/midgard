import { describe, expect, it } from "vitest";
import {
  classifyLikelyBottleneck,
  classifyLikelyBottleneckWithEvidence,
  createPhaseRecorder,
  gaugeSlopePerSec,
  isDrainComplete,
  maxRollingRate,
  summarizeCounterWindow,
  summarizeLatency,
  summarizeRollingRates,
} from "../scripts/throughput-benchmark-utils.mjs";

describe("throughput benchmark utilities", () => {
  it("computes fixed rolling rates for monotonic counters", () => {
    const samples = [
      { timestampMs: 0, counters: { accept: 0, submit: 0 } },
      { timestampMs: 1_000, counters: { accept: 100, submit: 200 } },
      { timestampMs: 2_000, counters: { accept: 250, submit: 450 } },
      { timestampMs: 7_000, counters: { accept: 750, submit: 950 } },
    ];

    expect(maxRollingRate(samples, "accept", 1_000)).toBe(150);
    expect(maxRollingRate(samples, "submit", 1_000)).toBe(250);

    const rates = summarizeRollingRates(samples, ["accept"], [1_000, 5_000]);
    expect(rates.accept["1s"]).toBe(150);
    expect(rates.accept["5s"]).toBe(125);
  });

  it("summarizes measured windows without including surrounding phases", () => {
    const summary = summarizeCounterWindow({
      startCounters: { accept: 10, submit: 20, reject: 0 },
      endCounters: { accept: 210, submit: 240, reject: 1 },
      elapsedMs: 2_000,
      counterKeys: ["accept", "submit", "reject"],
    });

    expect(summary.accept.delta).toBe(200);
    expect(summary.accept.ratePerSec).toBe(100);
    expect(summary.submit.delta).toBe(220);
    expect(summary.submit.ratePerSec).toBe(110);
    expect(summary.reject.delta).toBe(1);
  });

  it("detects drain completion from accepted plus rejected deltas", () => {
    expect(
      isDrainComplete({
        submitted: 10,
        acceptedDelta: 9,
        rejectedDelta: 1,
      }),
    ).toBe(true);
    expect(
      isDrainComplete({
        submitted: 10,
        acceptedDelta: 9,
        rejectedDelta: 0,
      }),
    ).toBe(false);
  });

  it("classifies benchmark client limits before node limits", () => {
    expect(
      classifyLikelyBottleneck({
        submitted: 100,
        submitErrors: 0,
        acceptedDelta: 100,
        rejectedDelta: 0,
        commitTxDelta: 100,
        mergeBlockDelta: 1,
        targetAcceptedTps: 1_000,
        avgAcceptedTps: 900,
        clientSelfCheck: {
          required: true,
          targetRate: 2_000,
          achievedRate: 1_500,
        },
        endCounters: {},
        waitForCommit: false,
        waitForMerge: false,
      }),
    ).toBe("benchmark-client limited");
  });

  it("returns bottleneck evidence with the selected rule", () => {
    const result = classifyLikelyBottleneckWithEvidence({
      submitted: 100,
      submitErrors: 0,
      queueFullResponses: 2,
      acceptedDelta: 98,
      rejectedDelta: 0,
      commitTxDelta: 0,
      mergeBlockDelta: 0,
      targetAcceptedTps: 100,
      avgAcceptedTps: 98,
      clientSelfCheck: null,
      endCounters: {},
      waitForCommit: false,
      waitForMerge: false,
    });

    expect(result.label).toBe("HTTP ingress limited");
    expect(result.evidence.queueFullResponses).toBe(2);
    expect(result.rule).toMatch(/submit errors|queue-full/);
  });

  it("classifies queue backlog separately from HTTP submission errors", () => {
    expect(
      classifyLikelyBottleneck({
        submitted: 100,
        submitErrors: 0,
        acceptedDelta: 40,
        rejectedDelta: 0,
        commitTxDelta: 0,
        mergeBlockDelta: 0,
        targetAcceptedTps: 100,
        avgAcceptedTps: 40,
        clientSelfCheck: null,
        endCounters: { validationQueueDepth: 60 },
        waitForCommit: false,
        waitForMerge: false,
      }),
    ).toBe("queue scheduling limited");

    expect(
      classifyLikelyBottleneck({
        submitted: 100,
        submitErrors: 1,
        acceptedDelta: 99,
        rejectedDelta: 0,
        commitTxDelta: 0,
        mergeBlockDelta: 0,
        targetAcceptedTps: 100,
        avgAcceptedTps: 99,
        clientSelfCheck: null,
        endCounters: {},
        waitForCommit: false,
        waitForMerge: false,
      }),
    ).toBe("HTTP ingress limited");
  });

  it("records phase durations with an injectable clock", () => {
    let now = 0;
    const recorder = createPhaseRecorder(() => now);
    recorder.start("setup");
    now = 10;
    recorder.start("measured");
    now = 40;
    recorder.end();

    expect(recorder.list()).toEqual([
      { name: "setup", startMs: 0, endMs: 10, durationMs: 10 },
      { name: "measured", startMs: 10, endMs: 40, durationMs: 30 },
    ]);
  });

  it("computes gauge slope in units per second", () => {
    const samples = [
      { timestampMs: 0, counters: { validationQueueDepth: 0 } },
      { timestampMs: 1_000, counters: { validationQueueDepth: 10 } },
      { timestampMs: 2_000, counters: { validationQueueDepth: 20 } },
    ];

    expect(gaugeSlopePerSec(samples, "validationQueueDepth")).toBe(10);
  });

  it("summarizes latency percentiles and handles empty samples", () => {
    expect(summarizeLatency([])).toEqual({
      count: 0,
      min: null,
      p50: null,
      p95: null,
      p99: null,
      max: null,
      mean: null,
    });
    expect(summarizeLatency([5, 1, 10, 20]).p50).toBe(5);
    expect(summarizeLatency([5, 1, 10, 20]).p95).toBe(20);
  });
});
