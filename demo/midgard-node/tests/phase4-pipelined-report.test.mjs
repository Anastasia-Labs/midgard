import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  evaluatePhase4PipelinedReport,
  PHASE4_ONE_HOUR_SCENARIO,
  verifyPhase4PipelinedReportFile,
} from "../scripts/verify-phase4-pipelined-report.mjs";
import {
  canonicalJsonSha256,
  PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
} from "../scripts/phase4-environment-fingerprint-lib.mjs";

const hash = (digit) => digit.repeat(64);

const environmentFingerprint = () => {
  const document = {
    schemaVersion: "midgard-phase4-environment-v1",
    node: {
      cpuSet: "0-7",
      nanoCpus: 4_000_000_000,
      imageId: `sha256:${hash("a")}`,
      memoryLimitBytes: 8_589_934_592,
    },
    loadGenerator: {
      cpuSet: "8-15",
      nanoCpus: 4_000_000_000,
      imageId: `sha256:${hash("b")}`,
      memoryLimitBytes: 4_294_967_296,
      placement: "separate-container",
      cohosted: true,
    },
    postgres: {
      cpuSet: "16-23",
      nanoCpus: 4_000_000_000,
      imageId: `sha256:${hash("c")}`,
      memoryLimitBytes: 8_589_934_592,
    },
    provider: { kind: "Kupmios", routeSha256: hash("8") },
    deploymentManifest: { sha256: hash("9") },
    clockOffsetMs: 0,
  };
  return {
    path: "/evidence/environment.json",
    sha256: hash("7"),
    artifactSchemaVersion: PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
    documentSha256: canonicalJsonSha256(document),
    document,
  };
};

const reportFixture = ({
  measuredSec = 3_600,
  cadenceP50 = 19_000,
  confirmationP50 = 15_000,
  overlapP50 = 0.95,
  hitRate = 0.97,
  blockTxCount = 50_000,
  stateQueueSlope = 0,
  daQueueSlope = -0.001,
} = {}) => ({
  benchmark: "midgard-l2-throughput",
  version: 2,
  scenario: PHASE4_ONE_HOUR_SCENARIO,
  scenarioClass: "B",
  metadata: {
    git: {
      commit: "a".repeat(40),
      statusSha256: hash("1"),
      trackedDiffSha256: hash("2"),
      benchmarkScriptSha256: hash("3"),
      sourceTreeSha256: hash("4"),
      sourceTreeFileCount: 123,
    },
    runtime: { nodeVersion: "v22.22.2" },
  },
  config: {
    formalBenchmark: true,
    measuredSec,
    waitForCommit: true,
    waitForMerge: true,
    loadGenerator: {
      placement: "separate-container",
      cohosted: true,
      clockOffsetMs: 0,
    },
    phase4: {
      blockTxTarget: 50_000,
      configuredCommitMaxL2TxCount: 50_000,
      speculativeCommitBuild: true,
      environmentFingerprint: environmentFingerprint(),
    },
  },
  workload: {
    corpusManifest: {
      schemaVersion: "midgard-stress-corpus-manifest-v1",
      durationMs: 3_600_000,
      files: {
        corpus: { sha256: hash("5") },
        index: { sha256: hash("6") },
      },
    },
  },
  summary: {
    measuredElapsedSec: measuredSec,
    primaryStageNames: ["measured"],
  },
  stages: [
    {
      name: "measured",
      commitBlockDelta: 120,
      phase4Metrics: {
        histograms: {
          commitCadenceMs: { count: 119, p50: cadenceP50 },
          l1ConfirmationWaitMs: { count: 120, p50: confirmationP50 },
        },
        overlapEfficiency: { count: 120, p50: overlapP50 },
        hitRate,
        observedBlockTxCount: {
          sampleCount: 120,
          min: blockTxCount,
          max: blockTxCount,
          last: blockTxCount,
        },
        queueMetricPresence: {
          stateQueueBlocks: true,
          daPublicationBacklog: true,
        },
        queueSlopesPerSec: {
          stateQueueBlocks: stateQueueSlope,
          daPublicationBacklog: daQueueSlope,
        },
      },
    },
  ],
});

describe("Phase 4 pipelined one-hour report verifier", () => {
  it("passes the Stage C target verdict and binds the full source identity", () => {
    const result = evaluatePhase4PipelinedReport(reportFixture());
    expect(result).toMatchObject({
      passed: true,
      verdict: "stage_c_target_met",
      artifactIdentity: {
        sourceTreeSha256: hash("4"),
        sourceTreeFileCount: 123,
        environmentFingerprintSha256: hash("7"),
      },
    });
    expect(result.metrics.stageCThroughputTps).toBeCloseTo(2_631.5789, 3);
  });

  it("passes with an explicit confirmation-latency-binding verdict", () => {
    const result = evaluatePhase4PipelinedReport(
      reportFixture({ cadenceP50: 25_000, confirmationP50: 20_001 }),
    );
    expect(result.passed).toBe(true);
    expect(result.verdict).toBe("confirmation_latency_binding");
    expect(result.metrics.stageCThroughputTps).toBe(2_000);
  });

  it("fails closed on short duration, wrong block size, or growing queues", () => {
    const result = evaluatePhase4PipelinedReport(
      reportFixture({
        measuredSec: 3_599,
        blockTxCount: 49_999,
        stateQueueSlope: 0.01,
        daQueueSlope: 0.02,
      }),
    );
    expect(result.passed).toBe(false);
    expect(result.reasons).toEqual(
      expect.arrayContaining([
        expect.stringContaining("configured duration"),
        expect.stringContaining("measured duration"),
        expect.stringContaining("not exactly 50,000"),
        expect.stringContaining("state-queue backlog slope"),
        expect.stringContaining("DA publication backlog slope"),
      ]),
    );
  });

  it("fails when source identity or the confirmation baseline is missing", () => {
    const report = reportFixture();
    report.metadata.git.sourceTreeSha256 = null;
    report.stages[0].phase4Metrics.histograms.l1ConfirmationWaitMs = {
      count: 0,
      p50: null,
    };
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toEqual(
      expect.arrayContaining([
        "git/script artifact identity is incomplete",
        expect.stringContaining("confirmation baseline"),
      ]),
    );
  });

  it("fails when the environment document is changed without rehashing", () => {
    const report = reportFixture();
    report.config.phase4.environmentFingerprint.document.provider.kind =
      "tampered";
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain(
      "Phase 4 environment fingerprint artifact is missing or invalid",
    );
  });

  it("fails an overlapping or wrong CPU allocation", () => {
    const report = reportFixture();
    const fingerprint = report.config.phase4.environmentFingerprint;
    fingerprint.document.node.cpuSet = "0-6,8";
    fingerprint.documentSha256 = canonicalJsonSha256(fingerprint.document);
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain("node resource profile is invalid");
  });

  it("fails an undersized memory allocation", () => {
    const report = reportFixture();
    const fingerprint = report.config.phase4.environmentFingerprint;
    fingerprint.document.postgres.memoryLimitBytes = 8_589_934_591;
    fingerprint.documentSha256 = canonicalJsonSha256(fingerprint.document);
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain("postgres resource profile is invalid");
  });

  it("fails a missing or incorrect CPU quota", () => {
    const report = reportFixture();
    const fingerprint = report.config.phase4.environmentFingerprint;
    fingerprint.document.loadGenerator.nanoCpus = 0;
    fingerprint.documentSha256 = canonicalJsonSha256(fingerprint.document);
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain(
      "loadGenerator resource profile is invalid",
    );
  });

  it("fingerprints the exact report bytes", () => {
    const directory = fs.mkdtempSync(path.join(os.tmpdir(), "phase4-report-"));
    const reportPath = path.join(directory, "report.json");
    const bytes = Buffer.from(`${JSON.stringify(reportFixture())}\n`);
    fs.writeFileSync(reportPath, bytes);
    const result = verifyPhase4PipelinedReportFile(reportPath);
    expect(result.passed).toBe(true);
    expect(result.reportSha256).toBe(
      createHash("sha256").update(bytes).digest("hex"),
    );
  });
});
