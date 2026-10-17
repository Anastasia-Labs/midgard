import "./e2e-stress-l2-throughput.e2e-stress-l2-throughput-runner.js";

import { describe, expect, it } from "vitest";

import {
  parseOpenLoopCorpusNdjson,
  planOpenLoopCorpus,
} from "../src/commands/stress-open-loop.js";
import {
  buildStressMetrics,
  computeMetricWindow,
} from "../src/commands/stress-stage-metrics.js";
import {
  corpusRow,
  txHashForIndex,
} from "./e2e-stress-l2-throughput.e2e-stress-l2-throughput-config.js";

describe("open-loop corpus planning", () => {
  it("rejects duplicate input outrefs within a corpus slice", () => {
    const rows = [
      corpusRow(1),
      {
        ...corpusRow(2),
        selectedInputOutref: corpusRow(1).selectedInputOutref,
      },
    ];
    const parsed = parseOpenLoopCorpusNdjson(
      rows.map((row) => JSON.stringify(row)).join("\n"),
    );

    expect(() =>
      planOpenLoopCorpus({
        rows: parsed,
        targetRateTps: 2,
        durationMs: 1000,
        warmupCount: 0,
        cooldownCount: 0,
        corpusShape: "fanout",
        corpusSliceId: "slice-a",
      }),
    ).toThrow("duplicate selected input");
  });
});

describe("stress stage metrics", () => {
  it("uses null rate instead of infinity for a zero-duration window", () => {
    const metric = computeMetricWindow({
      count: 1,
      expectedCount: 1,
      startedAt: "2026-01-01T00:00:00.000Z",
      finishedAt: "2026-01-01T00:00:00.000Z",
      source: "test",
      precision: "artifact_timestamp",
    });

    expect(metric).toMatchObject({
      status: "complete",
      durationMs: 0,
      perSecond: null,
      notes: ["zero_duration_window"],
    });
  });

  it("marks missing DB admission rows as partial", () => {
    const firstTxHash = txHashForIndex(91);
    const secondTxHash = txHashForIndex(92);
    const metrics = buildStressMetrics({
      requestedCount: 2,
      submittedCount: 2,
      acceptedCount: 2,
      observedCommittedCount: 0,
      startedAt: "2026-01-01T00:00:00.000Z",
      submissionFinishedAt: "2026-01-01T00:00:02.000Z",
      finishedAt: "2026-01-01T00:00:03.000Z",
      transactions: [
        {
          txHash: firstTxHash,
          submission: {
            status: "submitted",
            submittedAt: "2026-01-01T00:00:01.000Z",
          },
          acceptance: {
            status: "accepted",
            acceptedAt: "2026-01-01T00:00:02.000Z",
          },
          finality: { status: "not_observed" },
        },
        {
          txHash: secondTxHash,
          submission: {
            status: "submitted",
            submittedAt: "2026-01-01T00:00:01.500Z",
          },
          acceptance: {
            status: "accepted",
            acceptedAt: "2026-01-01T00:00:02.500Z",
          },
          finality: { status: "not_observed" },
        },
      ],
      dbSources: {
        l2Admissions: [
          {
            txHash: firstTxHash,
            status: "accepted",
            firstSeenAt: "2026-01-01T00:00:01.000Z",
            validationStartedAt: "2026-01-01T00:00:01.500Z",
            terminalAt: "2026-01-01T00:00:02.000Z",
          },
        ],
        l1Commits: [],
        immutableObservations: [],
        residue: [],
      },
    });

    expect(metrics.l2Admission).toMatchObject({
      status: "partial",
      count: 1,
      missingCount: 1,
      precision: "db_timestamp",
    });
  });
});
