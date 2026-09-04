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
  decodePhase4EnvironmentArtifactV1,
  decodePhase4EnvironmentDocumentV1,
  PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
} from "../scripts/phase4-environment-fingerprint-lib.mjs";

const hash = (digit) => digit.repeat(64);

const environmentFingerprint = () => {
  const document = {
    schemaVersion: "midgard-phase4-environment-v1",
    capturedAtIso: "2026-07-14T05:00:00.000Z",
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
    deploymentManifest: {
      path: "/evidence/contract-deployment-info.json",
      sha256: hash("9"),
    },
    clockOffsetMs: 0,
  };
  const artifact = {
    schemaVersion: PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
    documentSha256: canonicalJsonSha256(document),
    document,
  };
  const directory = fs.mkdtempSync(
    path.join(os.tmpdir(), "phase4-environment-"),
  );
  const artifactPath = path.join(directory, "environment.json");
  const artifactBytes = Buffer.from(`${JSON.stringify(artifact, null, 2)}\n`);
  fs.writeFileSync(artifactPath, artifactBytes);
  return {
    path: artifactPath,
    sha256: createHash("sha256").update(artifactBytes).digest("hex"),
    artifactSchemaVersion: PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
    documentSha256: artifact.documentSha256,
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
  version: 1,
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
  it("decodes only the exact canonical Phase 4 environment V1 artifact", () => {
    const fingerprint = environmentFingerprint();
    const artifact = {
      schemaVersion: fingerprint.artifactSchemaVersion,
      documentSha256: fingerprint.documentSha256,
      document: fingerprint.document,
    };
    expect(decodePhase4EnvironmentArtifactV1(artifact)).toEqual(artifact);
    expect(decodePhase4EnvironmentDocumentV1(artifact.document)).toEqual(
      artifact.document,
    );

    const mutations = [
      { ...artifact, schemaVersion: "midgard-phase4-environment-artifact-v2" },
      { ...artifact, unexpected: true },
      {
        ...artifact,
        document: { ...artifact.document, capturedAtIso: undefined },
      },
      {
        ...artifact,
        document: { ...artifact.document, capturedAtIso: "2026-07-14" },
      },
      {
        ...artifact,
        document: {
          ...artifact.document,
          node: { ...artifact.document.node, cpuSet: "0,1,2,3,4,5,6,7" },
        },
      },
      {
        ...artifact,
        document: {
          ...artifact.document,
          provider: { ...artifact.document.provider, unknown: true },
        },
      },
      {
        schemaVersion: "midgard-phase4-local-genesis-ledger-v1",
        documentSha256: artifact.documentSha256,
        document: artifact.document,
      },
    ];
    for (const mutation of mutations) {
      expect(() => decodePhase4EnvironmentArtifactV1(mutation)).toThrow();
    }
  });

  it("passes the Stage C target verdict and binds the full source identity", () => {
    const report = reportFixture();
    const result = evaluatePhase4PipelinedReport(report);
    expect(result).toMatchObject({
      passed: true,
      verdict: "stage_c_target_met",
      artifactIdentity: {
        sourceTreeSha256: hash("4"),
        sourceTreeFileCount: 123,
        environmentFingerprintSha256:
          report.config.phase4.environmentFingerprint.sha256,
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

  it("fails closed on unknown, missing, and noncanonical environment fields even with an attacker-selected digest", () => {
    const mutations = [
      (document) => {
        document.unknown = true;
      },
      (document) => {
        delete document.capturedAtIso;
      },
      (document) => {
        document.node.unknown = true;
      },
      (document) => {
        document.capturedAtIso = "2026-07-14T05:00:00Z";
      },
      (document) => {
        document.node.cpuSet = "0,1,2,3,4,5,6,7";
      },
    ];
    for (const mutate of mutations) {
      const report = reportFixture();
      const fingerprint = report.config.phase4.environmentFingerprint;
      mutate(fingerprint.document);
      fingerprint.documentSha256 = hash("f");
      const result = evaluatePhase4PipelinedReport(report);
      expect(result.passed).toBe(false);
      expect(result.reasons).toContain(
        "Phase 4 environment fingerprint artifact is missing or invalid",
      );
    }
  });

  it("fails closed on an unknown, missing, or byte-unbound environment wrapper field", () => {
    const reports = [reportFixture(), reportFixture(), reportFixture()];
    reports[0].config.phase4.environmentFingerprint.unknown = true;
    delete reports[1].config.phase4.environmentFingerprint.path;
    reports[2].config.phase4.environmentFingerprint.sha256 = hash("f");
    for (const report of reports) {
      const result = evaluatePhase4PipelinedReport(report);
      expect(result.passed).toBe(false);
      expect(result.reasons).toContain(
        "Phase 4 environment fingerprint artifact is missing or invalid",
      );
    }
  });

  it("fails an overlapping or wrong CPU allocation", () => {
    const report = reportFixture();
    const fingerprint = report.config.phase4.environmentFingerprint;
    fingerprint.document.node.cpuSet = "0-6,8";
    fingerprint.documentSha256 = canonicalJsonSha256(fingerprint.document);
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain(
      "Phase 4 environment fingerprint artifact is missing or invalid",
    );
  });

  it("fails an undersized memory allocation", () => {
    const report = reportFixture();
    const fingerprint = report.config.phase4.environmentFingerprint;
    fingerprint.document.postgres.memoryLimitBytes = 8_589_934_591;
    fingerprint.documentSha256 = canonicalJsonSha256(fingerprint.document);
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain(
      "Phase 4 environment fingerprint artifact is missing or invalid",
    );
  });

  it("fails a missing or incorrect CPU quota", () => {
    const report = reportFixture();
    const fingerprint = report.config.phase4.environmentFingerprint;
    fingerprint.document.loadGenerator.nanoCpus = 0;
    fingerprint.documentSha256 = canonicalJsonSha256(fingerprint.document);
    const result = evaluatePhase4PipelinedReport(report);
    expect(result.passed).toBe(false);
    expect(result.reasons).toContain(
      "Phase 4 environment fingerprint artifact is missing or invalid",
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
