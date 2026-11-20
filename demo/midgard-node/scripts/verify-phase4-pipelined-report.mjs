#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";

import {
  canonicalJsonSha256,
  PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
  PHASE4_ENVIRONMENT_SCHEMA,
  PHASE4_RESOURCE_PROFILE,
  parseCpuSet,
  sameCpuSet,
  validImageId,
} from "./phase4-environment-fingerprint-lib.mjs";

export const PHASE4_ONE_HOUR_SCENARIO = "phase4-pipelined-commits-one-hour-v1";
export const PHASE4_MIN_DURATION_SEC = 3_600;
export const PHASE4_BLOCK_TX_TARGET = 50_000;
export const PHASE4_MAX_CADENCE_TAIL_MS = 5_000;
export const PHASE4_MIN_OVERLAP = 0.9;
export const PHASE4_MIN_HIT_RATE = 0.95;
export const PHASE4_MIN_STAGE_C_TPS = 2_500;
export const PHASE4_CONFIRMATION_BINDING_MS = 19_000;

const finite = (value) =>
  typeof value === "number" && Number.isFinite(value) ? value : null;

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

const sha256Hex = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);

const gitSha = (value) =>
  typeof value === "string" && /^[0-9a-f]{40}$/u.test(value);

const selectedPrimaryStage = (report, reasons) => {
  const names = report?.summary?.primaryStageNames;
  if (!Array.isArray(names) || names.length !== 1) {
    reasons.push("exactly one primary measured stage is required");
    return null;
  }
  const stage = report?.stages?.find((entry) => entry?.name === names[0]);
  if (stage === undefined) {
    reasons.push(`primary stage ${String(names[0])} is missing`);
    return null;
  }
  return stage;
};

const validateEnvironmentFingerprint = (fingerprint, config, reasons) => {
  const document = fingerprint?.document;
  if (
    !sha256Hex(fingerprint?.sha256) ||
    fingerprint?.artifactSchemaVersion !== PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA ||
    !sha256Hex(fingerprint?.documentSha256) ||
    fingerprint.documentSha256 !== canonicalJsonSha256(document) ||
    document?.schemaVersion !== PHASE4_ENVIRONMENT_SCHEMA
  ) {
    reasons.push(
      "Phase 4 environment fingerprint artifact is missing or invalid",
    );
    return;
  }
  for (const component of ["node", "loadGenerator", "postgres"]) {
    const value = document?.[component];
    const expected = PHASE4_RESOURCE_PROFILE[component];
    if (
      !sameCpuSet(parseCpuSet(value?.cpuSet), expected.cpus) ||
      value?.nanoCpus !== expected.nanoCpus ||
      !validImageId(value?.imageId) ||
      !Number.isSafeInteger(value?.memoryLimitBytes) ||
      value.memoryLimitBytes < expected.minMemoryLimitBytes
    ) {
      reasons.push(`${component} resource profile is invalid`);
    }
  }
  if (
    document?.loadGenerator?.placement !== config?.loadGenerator?.placement ||
    document?.loadGenerator?.cohosted !== config?.loadGenerator?.cohosted
  ) {
    reasons.push("load-generator fingerprint does not match report placement");
  }
  if (
    typeof document?.provider?.kind !== "string" ||
    document.provider.kind.length === 0 ||
    !sha256Hex(document?.provider?.routeSha256)
  ) {
    reasons.push("provider route identity is incomplete");
  }
  if (!sha256Hex(document?.deploymentManifest?.sha256)) {
    reasons.push("deployment manifest identity is incomplete");
  }
  if (
    finite(document?.clockOffsetMs) === null ||
    document.clockOffsetMs !== config?.loadGenerator?.clockOffsetMs
  ) {
    reasons.push(
      "clock offset fingerprint does not match report configuration",
    );
  }
};

export const evaluatePhase4PipelinedReport = (report) => {
  const reasons = [];
  if (report?.benchmark !== "midgard-l2-throughput" || report?.version !== 2) {
    reasons.push("unexpected benchmark schema");
  }
  if (report?.scenario !== PHASE4_ONE_HOUR_SCENARIO) {
    reasons.push(`scenario must be ${PHASE4_ONE_HOUR_SCENARIO}`);
  }
  if (report?.scenarioClass !== "B") {
    reasons.push("Phase 4 gate requires production-shaped scenarioClass B");
  }
  if (report?.config?.formalBenchmark !== true) {
    reasons.push("formalBenchmark must be true");
  }
  if (finite(report?.config?.measuredSec) < PHASE4_MIN_DURATION_SEC) {
    reasons.push(`configured duration is below ${PHASE4_MIN_DURATION_SEC}s`);
  }
  if (finite(report?.summary?.measuredElapsedSec) < PHASE4_MIN_DURATION_SEC) {
    reasons.push(`measured duration is below ${PHASE4_MIN_DURATION_SEC}s`);
  }
  if (report?.config?.phase4?.blockTxTarget !== PHASE4_BLOCK_TX_TARGET) {
    reasons.push(`block target must be ${PHASE4_BLOCK_TX_TARGET}`);
  }
  if (
    report?.config?.phase4?.configuredCommitMaxL2TxCount !==
    PHASE4_BLOCK_TX_TARGET
  ) {
    reasons.push(`COMMIT_MAX_L2_TX_COUNT must be ${PHASE4_BLOCK_TX_TARGET}`);
  }
  if (report?.config?.phase4?.speculativeCommitBuild !== true) {
    reasons.push("SPECULATIVE_COMMIT_BUILD must be true");
  }
  validateEnvironmentFingerprint(
    report?.config?.phase4?.environmentFingerprint,
    report?.config,
    reasons,
  );
  if (
    report?.config?.waitForCommit !== true ||
    report?.config?.waitForMerge !== true
  ) {
    reasons.push("waitForCommit and waitForMerge must both be true");
  }

  const git = report?.metadata?.git;
  if (
    !gitSha(git?.commit) ||
    !sha256Hex(git?.statusSha256) ||
    !sha256Hex(git?.trackedDiffSha256) ||
    !sha256Hex(git?.benchmarkScriptSha256) ||
    !sha256Hex(git?.sourceTreeSha256) ||
    !Number.isSafeInteger(git?.sourceTreeFileCount) ||
    git.sourceTreeFileCount <= 0
  ) {
    reasons.push("git/script artifact identity is incomplete");
  }
  if (typeof report?.metadata?.runtime?.nodeVersion !== "string") {
    reasons.push("runtime nodeVersion identity is missing");
  }
  const manifest = report?.workload?.corpusManifest;
  if (
    manifest?.schemaVersion !== "midgard-stress-corpus-manifest-v1" ||
    !sha256Hex(manifest?.files?.corpus?.sha256) ||
    !sha256Hex(manifest?.files?.index?.sha256)
  ) {
    reasons.push("corpus artifact identity is incomplete");
  }
  if (finite(manifest?.durationMs) < PHASE4_MIN_DURATION_SEC * 1_000) {
    reasons.push("corpus manifest duration is below one hour");
  }

  const stage = selectedPrimaryStage(report, reasons);
  const phase4 = stage?.phase4Metrics;
  const cadenceP50Ms = finite(phase4?.histograms?.commitCadenceMs?.p50);
  const confirmationP50Ms = finite(
    phase4?.histograms?.l1ConfirmationWaitMs?.p50,
  );
  const overlapP50 = finite(phase4?.overlapEfficiency?.p50);
  const hitRate = finite(phase4?.hitRate);
  const cadenceCount = finite(phase4?.histograms?.commitCadenceMs?.count) ?? 0;
  const confirmationCount =
    finite(phase4?.histograms?.l1ConfirmationWaitMs?.count) ?? 0;

  if (cadenceP50Ms === null || cadenceCount < 2) {
    reasons.push("commit cadence p50 requires at least two observations");
  }
  if (confirmationP50Ms === null || confirmationCount < 2) {
    reasons.push(
      "L1 confirmation baseline p50 requires at least two observations",
    );
  }
  if (
    cadenceP50Ms !== null &&
    confirmationP50Ms !== null &&
    cadenceP50Ms > confirmationP50Ms + PHASE4_MAX_CADENCE_TAIL_MS
  ) {
    reasons.push(
      `cadence p50 ${cadenceP50Ms}ms exceeds confirmation p50 ${confirmationP50Ms}ms + ${PHASE4_MAX_CADENCE_TAIL_MS}ms`,
    );
  }
  if (overlapP50 === null || overlapP50 < PHASE4_MIN_OVERLAP) {
    reasons.push(`overlap p50 must be >= ${PHASE4_MIN_OVERLAP}`);
  }
  if (hitRate === null || hitRate < PHASE4_MIN_HIT_RATE) {
    reasons.push(`speculation hit rate must be >= ${PHASE4_MIN_HIT_RATE}`);
  }

  const observedBlockTxCount = phase4?.observedBlockTxCount;
  if (
    observedBlockTxCount?.sampleCount < 1 ||
    observedBlockTxCount?.min !== PHASE4_BLOCK_TX_TARGET ||
    observedBlockTxCount?.max !== PHASE4_BLOCK_TX_TARGET ||
    observedBlockTxCount?.last !== PHASE4_BLOCK_TX_TARGET
  ) {
    reasons.push("observed non-zero commit blocks are not exactly 50,000 tx");
  }
  if ((finite(stage?.commitBlockDelta) ?? 0) < 2) {
    reasons.push("at least two submitted commit blocks are required");
  }

  const queuePresence = phase4?.queueMetricPresence;
  const queueSlopes = phase4?.queueSlopesPerSec;
  if (
    queuePresence?.stateQueueBlocks !== true ||
    queuePresence?.daPublicationBacklog !== true
  ) {
    reasons.push("state-queue and DA backlog metrics must both be present");
  }
  const stateQueueSlope = finite(queueSlopes?.stateQueueBlocks);
  const daQueueSlope = finite(queueSlopes?.daPublicationBacklog);
  if (stateQueueSlope === null || stateQueueSlope > 0) {
    reasons.push("state-queue backlog slope must be <= 0");
  }
  if (daQueueSlope === null || daQueueSlope > 0) {
    reasons.push("DA publication backlog slope must be <= 0");
  }

  const stageCThroughputTps =
    cadenceP50Ms !== null && cadenceP50Ms > 0
      ? PHASE4_BLOCK_TX_TARGET / (cadenceP50Ms / 1_000)
      : null;
  const confirmationBinding =
    stageCThroughputTps !== null &&
    stageCThroughputTps < PHASE4_MIN_STAGE_C_TPS &&
    confirmationP50Ms !== null &&
    confirmationP50Ms > PHASE4_CONFIRMATION_BINDING_MS;
  if (
    stageCThroughputTps === null ||
    (stageCThroughputTps < PHASE4_MIN_STAGE_C_TPS && !confirmationBinding)
  ) {
    reasons.push(
      `Stage C throughput must be >= ${PHASE4_MIN_STAGE_C_TPS} TPS or confirmation p50 must exceed ${PHASE4_CONFIRMATION_BINDING_MS}ms`,
    );
  }

  return {
    passed: reasons.length === 0,
    reasons,
    verdict: confirmationBinding
      ? "confirmation_latency_binding"
      : "stage_c_target_met",
    metrics: {
      cadenceP50Ms,
      confirmationP50Ms,
      overlapP50,
      hitRate,
      stageCThroughputTps,
      stateQueueSlopePerSec: stateQueueSlope,
      daPublicationBacklogSlopePerSec: daQueueSlope,
    },
    artifactIdentity: {
      gitCommit: git?.commit ?? null,
      gitStatusSha256: git?.statusSha256 ?? null,
      trackedDiffSha256: git?.trackedDiffSha256 ?? null,
      benchmarkScriptSha256: git?.benchmarkScriptSha256 ?? null,
      sourceTreeSha256: git?.sourceTreeSha256 ?? null,
      sourceTreeFileCount: git?.sourceTreeFileCount ?? null,
      corpusSha256: manifest?.files?.corpus?.sha256 ?? null,
      corpusIndexSha256: manifest?.files?.index?.sha256 ?? null,
      environmentFingerprintSha256:
        report?.config?.phase4?.environmentFingerprint?.sha256 ?? null,
    },
  };
};

export const verifyPhase4PipelinedReportFile = (reportPath) => {
  const bytes = fs.readFileSync(reportPath);
  const report = JSON.parse(bytes.toString("utf8"));
  return {
    ...evaluatePhase4PipelinedReport(report),
    reportPath: path.resolve(reportPath),
    reportSha256: sha256(bytes),
  };
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);
if (isMain) {
  const reportPath = process.argv[2];
  if (reportPath === undefined) {
    console.error("usage: verify-phase4-pipelined-report.mjs <report.json>");
    process.exitCode = 2;
  } else {
    const result = verifyPhase4PipelinedReportFile(reportPath);
    console.log(JSON.stringify(result, null, 2));
    if (!result.passed) process.exitCode = 1;
  }
}
