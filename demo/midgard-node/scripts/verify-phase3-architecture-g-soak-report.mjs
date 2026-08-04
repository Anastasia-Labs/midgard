#!/usr/bin/env node

import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  evaluateClosureIdentity,
  evaluateClosureIdentityArtifacts,
  sameSourceIdentity,
  scanSubmitRecords,
  SHA256,
  sha256File,
  summarizePhase3WorkloadReport,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA,
  phase3SoakSourceIdentitySha256,
} from "./phase3-architecture-g-soak-preflight.mjs";
import {
  CORPUS_PREFIX_EVIDENCE_SCHEMA,
  loadCorpusIndex,
  scanCorpusPrefixEvidence,
  selectCorpusIndexEntries,
} from "./throughput-valid-stress-corpus.mjs";
import {
  PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
  PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA,
  validatePhase3NodePreLifecycleRevalidationDocument,
  validatePhase3LoadGeneratorIsolationDocument,
  validateTrustedPhase3DockerRuntimeArtifacts,
} from "./phase3-architecture-g-load-generator-isolation.mjs";

export const PHASE3_ARCHITECTURE_G_SOAK_SCHEMA =
  "midgard-phase3-architecture-g-live-soak-v4";
export const PHASE3_ARCHITECTURE_G_SOAK_SCENARIO =
  "phase3-architecture-g-live-soak-24h-v1";
export const PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC = 86_400;
export const PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS = 60_000;
export const PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS = 90_000;
export const PHASE3_OWNER_MAX_RESIDENT_NODES = 2_000_000;
export const PHASE3_OWNER_MAX_RESIDENT_BYTES = 2 * 1024 ** 3;
export const PHASE3_GENERATED_MAX_NODES = 1_000_000;
export const PHASE3_GENERATED_MAX_BYTES = 1024 ** 3;
export const PHASE3_PROCESS_MAX_DAILY_GROWTH_RATIO = 0.1;
export const PHASE3_MAX_AUDIT_AGE_MS = 6 * 60 * 60_000 + 90_000;
export const PHASE3_ARCHITECTURE_G_TARGET_TPS = 5_000;
export const PHASE3_OFFERED_RATE_MIN_RATIO = 0.98;
export const PHASE3_ACCEPTED_RATE_MIN_RATIO = 0.99;
export const PHASE3_NODE_SATURATION_MIN_RATIO = 1;
export const PHASE3_WORKLOAD_LIFECYCLE_GRACE_MS = 15 * 60_000;
export const PHASE3_DRAIN_TIMEOUT_SEC = 600;

const finite = (value) =>
  typeof value === "number" && Number.isFinite(value) ? value : null;

const integer = (value) => (Number.isSafeInteger(value) ? value : null);

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

const normalizedImageId = (value) =>
  typeof value === "string" ? value.replace(/^sha256:/u, "") : value;

const slopePerSecond = (samples, select) => {
  if (samples.length < 2) return null;
  let points;
  try {
    points = samples.map((sample) => ({
      x: sample?.elapsedMs / 1_000,
      y: select(sample),
    }));
  } catch {
    return null;
  }
  if (points.some(({ x, y }) => finite(x) === null || finite(y) === null)) {
    return null;
  }
  const xMean = points.reduce((sum, point) => sum + point.x, 0) / points.length;
  const yMean = points.reduce((sum, point) => sum + point.y, 0) / points.length;
  let numerator = 0;
  let denominator = 0;
  for (const point of points) {
    numerator += (point.x - xMean) * (point.y - yMean);
    denominator += (point.x - xMean) ** 2;
  }
  return denominator > 0 ? numerator / denominator : null;
};

const maximumFinite = (samples, select) => {
  let values;
  try {
    values = samples.map(select);
  } catch {
    return null;
  }
  return values.length > 0 && values.every((value) => finite(value) !== null)
    ? Math.max(...values)
    : null;
};

const identityReasons = (identity) => {
  return evaluateClosureIdentity(identity);
};

const counterDelta = (samples, field, reasons) => {
  const values = samples.map((sample) => finite(sample?.metrics?.[field]));
  if (values.some((value) => value === null)) {
    reasons.push(`${field} is missing or non-finite`);
    return null;
  }
  for (let index = 1; index < values.length; index += 1) {
    if (values[index] < values[index - 1]) {
      reasons.push(`${field} reset during the soak`);
      return null;
    }
  }
  return values.at(-1) - values[0];
};

export const evaluatePhase3ArchitectureGSoakReport = (
  report,
  { allowTestOnlyDuration = false } = {},
) => {
  const reasons = [];
  if (report?.schemaVersion !== PHASE3_ARCHITECTURE_G_SOAK_SCHEMA) {
    reasons.push("unexpected Phase 3 soak report schema");
  }
  if (report?.scenario !== PHASE3_ARCHITECTURE_G_SOAK_SCENARIO) {
    reasons.push(`scenario must be ${PHASE3_ARCHITECTURE_G_SOAK_SCENARIO}`);
  }
  const testOnly = report?.testOnly === true;
  if (testOnly && !allowTestOnlyDuration) {
    reasons.push(
      "test-only soak reports can never satisfy the production gate",
    );
  }
  const requiredDurationSec =
    testOnly && allowTestOnlyDuration
      ? finite(report?.configuredDurationSec)
      : PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC;
  const workloadCorpus = report?.workload?.reportSummary?.corpus;
  const boundCorpus = report?.identity?.phase1?.corpus;
  const workloadSummary = report?.workload?.reportSummary;
  const corpusPreflight = report?.identity?.corpusPreflight;
  const workloadPreflight = workloadCorpus?.preflight;
  const isolation = report?.identity?.loadGeneratorIsolation;
  const nodePreLifecycleRevalidation =
    report?.identity?.nodePreLifecycleRevalidation;
  const workloadIsolation = workloadSummary?.loadGenerator?.isolation;
  const primaryStages = Array.isArray(workloadSummary?.primaryStageMeasurements)
    ? workloadSummary.primaryStageMeasurements
    : [];
  const primaryStage = primaryStages[0];
  const maximumMeasuredOverrunSec =
    PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS / 1_000;
  if (
    requiredDurationSec === null ||
    requiredDurationSec <= 0 ||
    (!testOnly &&
      report?.configuredDurationSec !== PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC)
  ) {
    reasons.push(
      `configured duration must be exactly ${PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC}s`,
    );
  }
  if (
    !testOnly &&
    report?.sampleIntervalMs !== PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS
  ) {
    reasons.push(
      `sample interval must be exactly ${PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS}ms`,
    );
  }
  if (
    integer(report?.sampleIntervalMs) === null ||
    report.sampleIntervalMs <= 0 ||
    report.sampleIntervalMs > PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS
  ) {
    reasons.push("sample interval is invalid");
  }
  reasons.push(...identityReasons(report?.identity));
  if (
    isolation?.schemaVersion !== PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA ||
    typeof isolation?.path !== "string" ||
    !path.isAbsolute(isolation.path) ||
    !SHA256.test(isolation?.sha256 ?? "") ||
    !Number.isSafeInteger(isolation?.bytes) ||
    isolation.bytes <= 0 ||
    workloadSummary?.loadGenerator?.placement !== "measured-cgroup" ||
    workloadSummary?.loadGenerator?.cohosted !== true ||
    workloadSummary?.loadGenerator?.clockOffsetMs !== 0 ||
    workloadSummary?.calibration !== null ||
    isolation?.nodeContainerId !== report?.identity?.phase1?.nodeContainerId ||
    normalizedImageId(isolation?.nodeImageId) !==
      normalizedImageId(report?.identity?.phase1?.nodeImageId) ||
    !Number.isSafeInteger(isolation?.nodeHostPid) ||
    isolation.nodeHostPid <= 0 ||
    !/^\d+$/u.test(isolation?.nodeStartTicks ?? "") ||
    !Number.isSafeInteger(isolation?.loadGeneratorEffectiveUid) ||
    isolation.loadGeneratorEffectiveUid <= 0 ||
    typeof isolation?.readyUrl !== "string" ||
    typeof isolation?.metricsUrl !== "string" ||
    !SHA256.test(isolation?.dockerClientSha256 ?? "") ||
    typeof isolation?.dockerClientRealPath !== "string" ||
    !path.isAbsolute(isolation.dockerClientRealPath) ||
    typeof isolation?.dockerSocketRealPath !== "string" ||
    !path.isAbsolute(isolation.dockerSocketRealPath) ||
    !/^\d+$/u.test(isolation?.dockerSocketDev ?? "") ||
    !/^\d+$/u.test(isolation?.dockerSocketIno ?? "") ||
    typeof isolation?.dockerDaemonId !== "string" ||
    isolation.dockerDaemonId.length === 0 ||
    JSON.stringify(workloadIsolation) !== JSON.stringify(isolation)
  ) {
    reasons.push("measured load-generator isolation is missing or unbound");
  }
  if (
    nodePreLifecycleRevalidation?.schemaVersion !==
      PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA ||
    typeof nodePreLifecycleRevalidation?.path !== "string" ||
    !path.isAbsolute(nodePreLifecycleRevalidation.path) ||
    !SHA256.test(nodePreLifecycleRevalidation?.sha256 ?? "") ||
    !Number.isSafeInteger(nodePreLifecycleRevalidation?.bytes) ||
    nodePreLifecycleRevalidation.bytes <= 0 ||
    nodePreLifecycleRevalidation?.isolationPath !== isolation?.path ||
    nodePreLifecycleRevalidation?.isolationSha256 !== isolation?.sha256 ||
    nodePreLifecycleRevalidation?.nodeContainerId !==
      isolation?.nodeContainerId ||
    normalizedImageId(nodePreLifecycleRevalidation?.nodeImageId) !==
      normalizedImageId(isolation?.nodeImageId) ||
    nodePreLifecycleRevalidation?.nodeHostPid !== isolation?.nodeHostPid ||
    nodePreLifecycleRevalidation?.nodeStartTicks !==
      isolation?.nodeStartTicks ||
    nodePreLifecycleRevalidation?.nodeHealthStatus !== "healthy" ||
    nodePreLifecycleRevalidation?.readyUrl !== isolation?.readyUrl ||
    nodePreLifecycleRevalidation?.metricsUrl !== isolation?.metricsUrl ||
    nodePreLifecycleRevalidation?.dockerClientSha256 !==
      isolation?.dockerClientSha256 ||
    nodePreLifecycleRevalidation?.dockerSocketDev !==
      isolation?.dockerSocketDev ||
    nodePreLifecycleRevalidation?.dockerSocketIno !==
      isolation?.dockerSocketIno ||
    nodePreLifecycleRevalidation?.dockerDaemonId !== isolation?.dockerDaemonId
  ) {
    reasons.push("pre-lifecycle node reinspection is missing or unbound");
  }
  if (
    !sameSourceIdentity(report?.identity?.source, report?.sourceAtCompletion)
  ) {
    reasons.push("source tree changed during the live soak");
  }

  if (
    report?.termination?.completed !== true ||
    report?.termination?.reason !== "duration_completed" ||
    report?.termination?.workloadExitCode !== 0 ||
    report?.termination?.workloadSignal !== null ||
    report?.termination?.earlyExit !== false
  ) {
    reasons.push(
      "soak workload did not complete cleanly after the full duration",
    );
  }
  if (
    !SHA256.test(report?.workload?.scriptSha256 ?? "") ||
    !SHA256.test(report?.workload?.reportSha256 ?? "") ||
    !Number.isSafeInteger(report?.workload?.reportBytes) ||
    report.workload.reportBytes <= 0 ||
    workloadSummary?.scenario !== PHASE3_ARCHITECTURE_G_SOAK_SCENARIO ||
    workloadSummary?.scenarioClass !== "B" ||
    workloadSummary?.benchmarkMode !== "open" ||
    workloadSummary?.formalBenchmark !== true ||
    workloadSummary?.targetAcceptedTps !== PHASE3_ARCHITECTURE_G_TARGET_TPS ||
    workloadSummary?.openLoopRateTps !== PHASE3_ARCHITECTURE_G_TARGET_TPS ||
    workloadSummary?.measuredDurationSec !== report?.configuredDurationSec ||
    workloadSummary?.warmupTxs !== 0 ||
    workloadSummary?.warmupSec !== 0 ||
    workloadSummary?.cooldownSec !== 0 ||
    workloadSummary?.drainTimeoutSec !== PHASE3_DRAIN_TIMEOUT_SEC ||
    workloadSummary?.offeredRateMinRatio !== PHASE3_OFFERED_RATE_MIN_RATIO ||
    workloadSummary?.acceptedRateMinRatio !== PHASE3_ACCEPTED_RATE_MIN_RATIO ||
    workloadSummary?.nodeSaturationMinRatio !==
      PHASE3_NODE_SATURATION_MIN_RATIO ||
    workloadCorpus?.path !== boundCorpus?.path ||
    workloadCorpus?.indexPath !== boundCorpus?.indexPath ||
    workloadCorpus?.manifestPath !== boundCorpus?.manifestPath ||
    workloadCorpus?.sliceId !== boundCorpus?.sliceId ||
    workloadCorpus?.artifactIdentity?.corpusSha256 !==
      boundCorpus?.corpusSha256 ||
    workloadCorpus?.artifactIdentity?.indexSha256 !==
      boundCorpus?.indexSha256 ||
    workloadCorpus?.artifactIdentity?.manifestSha256 !==
      boundCorpus?.manifestSha256 ||
    workloadPreflight?.path !== corpusPreflight?.path ||
    workloadPreflight?.sha256 !== corpusPreflight?.sha256 ||
    workloadPreflight?.bytes !== corpusPreflight?.bytes ||
    workloadPreflight?.schemaVersion !== PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA ||
    workloadPreflight?.sourceTreeSha256 !==
      report?.identity?.source?.sourceTreeSha256 ||
    workloadPreflight?.sourceIdentitySha256 !==
      corpusPreflight?.sourceIdentitySha256 ||
    workloadPreflight?.phase1BindingSha256 !==
      report?.identity?.phase1?.sha256 ||
    workloadSummary?.submitErrors !== 0 ||
    workloadSummary?.rejectedDelta !== 0 ||
    workloadSummary?.missingRequiredMetrics?.length !== 0 ||
    workloadSummary?.allPrimaryStagesPassed !== true ||
    workloadSummary?.allPrimaryDrainsCompleted !== true ||
    primaryStages.length !== 1 ||
    primaryStage?.targetRateTps !== PHASE3_ARCHITECTURE_G_TARGET_TPS ||
    typeof report?.workload?.submitRecords?.path !== "string" ||
    !path.isAbsolute(report.workload.submitRecords.path) ||
    !SHA256.test(report?.workload?.submitRecords?.sha256 ?? "") ||
    !Number.isSafeInteger(report?.workload?.submitRecords?.bytes) ||
    report.workload.submitRecords.bytes <= 0 ||
    !Number.isSafeInteger(report?.workload?.submitRecords?.recordCount) ||
    report.workload.submitRecords.recordCount <= 0 ||
    !Number.isSafeInteger(report?.workload?.submitRecords?.successCount) ||
    !Number.isSafeInteger(report?.workload?.submitRecords?.errorCount) ||
    !SHA256.test(
      report?.workload?.submitRecords?.attemptSequenceSha256 ?? "",
    ) ||
    report?.workload?.submitRecords?.timeoutCount !== 0
  ) {
    reasons.push(
      "workload report is missing, failed, or recorded timeout/error residue",
    );
  }

  const preflightTiming = report?.preflight;
  const initialReadiness = preflightTiming?.initialReadiness;
  const preLifecycleRevalidation =
    preflightTiming?.nodePreLifecycleRevalidation;
  if (
    corpusPreflight?.schemaVersion !== PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA ||
    typeof corpusPreflight?.path !== "string" ||
    !path.isAbsolute(corpusPreflight.path) ||
    !SHA256.test(corpusPreflight?.sha256 ?? "") ||
    !Number.isSafeInteger(corpusPreflight?.bytes) ||
    corpusPreflight.bytes <= 0 ||
    corpusPreflight?.sourceTreeSha256 !==
      report?.identity?.source?.sourceTreeSha256 ||
    !SHA256.test(corpusPreflight?.sourceIdentitySha256 ?? "") ||
    corpusPreflight?.sourceIdentitySha256 !==
      phase3SoakSourceIdentitySha256(report?.identity?.source) ||
    corpusPreflight?.phase1BindingSha256 !== report?.identity?.phase1?.sha256 ||
    finite(preflightTiming?.startedAtMs) === null ||
    finite(preflightTiming?.completedAtMs) === null ||
    finite(preflightTiming?.durationMs) === null ||
    finite(preflightTiming?.lifecycleStartedAtMs) === null ||
    preflightTiming.completedAtMs < preflightTiming.startedAtMs ||
    preflightTiming.durationMs !==
      preflightTiming.completedAtMs - preflightTiming.startedAtMs ||
    preflightTiming.lifecycleStartedAtMs < preflightTiming.completedAtMs ||
    finite(initialReadiness?.observedAtMs) === null ||
    initialReadiness?.elapsedMs !== null ||
    initialReadiness.observedAtMs < preflightTiming.startedAtMs ||
    initialReadiness.observedAtMs > preflightTiming.completedAtMs ||
    JSON.stringify(preLifecycleRevalidation) !==
      JSON.stringify(nodePreLifecycleRevalidation) ||
    finite(nodePreLifecycleRevalidation?.observedAtMs) === null ||
    nodePreLifecycleRevalidation.observedAtMs < initialReadiness.observedAtMs ||
    nodePreLifecycleRevalidation.observedAtMs > preflightTiming.completedAtMs ||
    initialReadiness?.readiness?.httpStatus !== 200 ||
    initialReadiness?.readiness?.ready !== true ||
    !Array.isArray(initialReadiness?.readiness?.reasons) ||
    initialReadiness.readiness.reasons.length !== 0 ||
    [
      "auditDivergence",
      "auditAgeMs",
      "auditCompletedAtMs",
      "confirmedLedgerFullScanTotal",
      "validationWorkerTimeoutTotal",
      "l1ControlPlaneTimeoutTotal",
      "timeoutInsteadOfBackpressureTotal",
      "daPublicationBacklog",
      "mergeQueueDepth",
    ].some((field) => finite(initialReadiness?.metrics?.[field]) === null) ||
    initialReadiness?.metrics?.auditDivergence !== 0 ||
    typeof initialReadiness?.owner?.durableRoot !== "string" ||
    !SHA256.test(initialReadiness.owner.durableRoot) ||
    integer(initialReadiness?.owner?.residentNodes) === null ||
    integer(initialReadiness?.owner?.residentBytes) === null ||
    integer(initialReadiness?.owner?.rssBytes) === null ||
    integer(initialReadiness?.owner?.peakRssBytes) === null ||
    initialReadiness?.process?.pid !== isolation?.nodeHostPid ||
    initialReadiness?.process?.startTicks !== isolation?.nodeStartTicks ||
    integer(initialReadiness?.process?.rssBytes) === null ||
    initialReadiness.process.rssBytes <= 0 ||
    JSON.stringify(report?.samples?.[0]) !==
      JSON.stringify({ ...initialReadiness, elapsedMs: 0 }) ||
    report?.startedAtMs !== preflightTiming.lifecycleStartedAtMs
  ) {
    reasons.push(
      "preflight or initial readiness is missing, unbound, or overlaps lifecycle timing",
    );
  }

  const measuredElapsedSec = finite(workloadSummary?.measuredElapsedSec);
  const offeredRatePerSec = finite(workloadSummary?.offeredRatePerSec);
  const acceptedRatePerSec = finite(workloadSummary?.acceptedRatePerSec);
  const nodeSaturationRatio =
    offeredRatePerSec !== null &&
    acceptedRatePerSec !== null &&
    acceptedRatePerSec > 0
      ? offeredRatePerSec / acceptedRatePerSec
      : null;
  const primaryStageComputedSaturation =
    finite(primaryStage?.offeredRatePerSec) !== null &&
    finite(primaryStage?.acceptedRatePerSec) !== null &&
    primaryStage.acceptedRatePerSec > 0
      ? primaryStage.offeredRatePerSec / primaryStage.acceptedRatePerSec
      : null;
  if (
    measuredElapsedSec === null ||
    measuredElapsedSec < requiredDurationSec ||
    measuredElapsedSec > requiredDurationSec + maximumMeasuredOverrunSec
  ) {
    reasons.push("actual workload measured elapsed time missed the exact gate");
  }
  if (
    offeredRatePerSec === null ||
    offeredRatePerSec <
      PHASE3_ARCHITECTURE_G_TARGET_TPS * PHASE3_OFFERED_RATE_MIN_RATIO ||
    acceptedRatePerSec === null ||
    acceptedRatePerSec <
      PHASE3_ARCHITECTURE_G_TARGET_TPS * PHASE3_ACCEPTED_RATE_MIN_RATIO ||
    nodeSaturationRatio === null ||
    nodeSaturationRatio < PHASE3_NODE_SATURATION_MIN_RATIO
  ) {
    reasons.push("achieved offered/accepted/saturation rates missed the gate");
  }
  if (
    finite(primaryStage?.startedAtMs) === null ||
    finite(primaryStage?.endedAtMs) === null ||
    primaryStage.endedAtMs < primaryStage.startedAtMs ||
    finite(primaryStage?.measuredElapsedSec) === null ||
    Math.abs(
      (primaryStage.endedAtMs - primaryStage.startedAtMs) / 1_000 -
        primaryStage.measuredElapsedSec,
    ) > 0.001 ||
    primaryStage.measuredElapsedSec < requiredDurationSec ||
    primaryStage.measuredElapsedSec >
      requiredDurationSec + maximumMeasuredOverrunSec ||
    primaryStage.measuredElapsedSec !== measuredElapsedSec ||
    finite(primaryStage?.offeredRatePerSec) === null ||
    primaryStage.offeredRatePerSec <
      PHASE3_ARCHITECTURE_G_TARGET_TPS * PHASE3_OFFERED_RATE_MIN_RATIO ||
    primaryStage.offeredRatePerSec !== offeredRatePerSec ||
    finite(primaryStage?.acceptedRatePerSec) === null ||
    primaryStage.acceptedRatePerSec <
      PHASE3_ARCHITECTURE_G_TARGET_TPS * PHASE3_ACCEPTED_RATE_MIN_RATIO ||
    primaryStage.acceptedRatePerSec !== acceptedRatePerSec ||
    finite(primaryStage?.nodeSaturationRatio) === null ||
    primaryStage.nodeSaturationRatio < PHASE3_NODE_SATURATION_MIN_RATIO ||
    primaryStageComputedSaturation === null ||
    Math.abs(
      primaryStage.nodeSaturationRatio - primaryStageComputedSaturation,
    ) >
      Number.EPSILON * 8 ||
    primaryStage.nodeSaturationMinRatio !== PHASE3_NODE_SATURATION_MIN_RATIO ||
    primaryStage.nodeSaturationPassed !== true ||
    primaryStage.drainCompleted !== true ||
    finite(primaryStage?.drainElapsedMs) === null ||
    primaryStage.drainElapsedMs < 0
  ) {
    reasons.push("primary measured stage did not meet its achieved-rate gate");
  }

  const logicalSubmitAttempts = integer(workloadSummary?.logicalSubmitAttempts);
  const submitted = integer(workloadSummary?.submitted);
  const submitErrors = integer(workloadSummary?.submitErrors);
  const physicalSubmitAttempts = integer(
    workloadSummary?.physicalSubmitAttempts,
  );
  const submitRecords = report?.workload?.submitRecords;
  const corpusConsumption = workloadCorpus?.consumption;
  if (
    logicalSubmitAttempts === null ||
    logicalSubmitAttempts <= 0 ||
    submitted === null ||
    submitted < 0 ||
    submitErrors === null ||
    submitErrors < 0 ||
    logicalSubmitAttempts !== submitted + submitErrors ||
    primaryStage?.logicalSubmitAttempts !== logicalSubmitAttempts ||
    primaryStage?.physicalSubmitAttempts !== physicalSubmitAttempts ||
    primaryStage?.submitted !== submitted ||
    primaryStage?.submitErrors !== submitErrors ||
    physicalSubmitAttempts === null ||
    physicalSubmitAttempts < logicalSubmitAttempts ||
    submitRecords?.recordCount !== logicalSubmitAttempts ||
    submitRecords?.successCount !== submitted ||
    submitRecords?.errorCount !== submitErrors ||
    submitRecords?.successCount + submitRecords?.errorCount !==
      submitRecords?.recordCount ||
    corpusConsumption?.schemaVersion !== CORPUS_PREFIX_EVIDENCE_SCHEMA ||
    !Array.isArray(corpusConsumption?.chains) ||
    corpusConsumption.chains.length !==
      corpusPreflight?.selection?.indexEntryCount ||
    corpusConsumption?.rowCount !== logicalSubmitAttempts
  ) {
    reasons.push(
      "submit-record cardinality does not match explicit logical attempts",
    );
  }

  const samples = Array.isArray(report?.samples) ? report.samples : [];
  const requiredDurationMs = (requiredDurationSec ?? 0) * 1_000;
  const minimumSamples =
    Math.floor(requiredDurationMs / report?.sampleIntervalMs) + 1;
  if (samples.length < minimumSamples) {
    reasons.push(
      `periodic sample count ${samples.length.toString()} is below ${minimumSamples.toString()}`,
    );
  }
  let previousElapsedMs = null;
  let previousObservedAtMs = null;
  let expectedProcessStartTicks = null;
  let baselineChildRestarts = null;
  for (const [index, sample] of samples.entries()) {
    const elapsedMs = finite(sample?.elapsedMs);
    const observedAtMs = finite(sample?.observedAtMs);
    if (
      elapsedMs === null ||
      observedAtMs === null ||
      elapsedMs < 0 ||
      sample?.readiness?.httpStatus !== 200 ||
      sample?.readiness?.ready !== true ||
      !Array.isArray(sample?.readiness?.reasons) ||
      sample.readiness.reasons.length !== 0
    ) {
      reasons.push(
        `sample ${index.toString()} is stale, malformed, or not ready`,
      );
      continue;
    }
    if (previousElapsedMs === null) {
      if (
        elapsedMs !== 0 ||
        finite(report?.startedAtMs) === null ||
        observedAtMs > report.startedAtMs ||
        report.startedAtMs - observedAtMs >
          PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS
      ) {
        reasons.push("first sample was not captured at start");
      }
    } else {
      const elapsedGap = elapsedMs - previousElapsedMs;
      const wallGap = observedAtMs - previousObservedAtMs;
      if (
        elapsedGap <= 0 ||
        elapsedGap > PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS ||
        wallGap <= 0 ||
        wallGap > PHASE3_ARCHITECTURE_G_MAX_SAMPLE_GAP_MS
      ) {
        reasons.push(
          `sample ${index.toString()} exceeded the bounded cadence gap`,
        );
      }
    }
    if (
      index > 0 &&
      finite(report?.startedAtMs) !== null &&
      observedAtMs !== null &&
      elapsedMs !== null &&
      observedAtMs - report.startedAtMs !== elapsedMs
    ) {
      reasons.push(
        `sample ${index.toString()} elapsed time does not match wall time`,
      );
    }
    previousElapsedMs = elapsedMs;
    previousObservedAtMs = observedAtMs;

    const metrics = sample?.metrics;
    for (const field of [
      "auditDivergence",
      "confirmedLedgerFullScanTotal",
      "validationWorkerTimeoutTotal",
      "l1ControlPlaneTimeoutTotal",
      "timeoutInsteadOfBackpressureTotal",
      "daPublicationBacklog",
      "mergeQueueDepth",
    ]) {
      if (finite(metrics?.[field]) === null) {
        reasons.push(
          `sample ${index.toString()} metric ${field} is missing or NaN`,
        );
      }
    }
    if (metrics?.auditDivergence !== 0) {
      reasons.push(`sample ${index.toString()} observed MPF audit divergence`);
    }
    if (
      finite(metrics?.auditAgeMs) === null ||
      finite(metrics?.auditCompletedAtMs) === null ||
      metrics.auditAgeMs < 0 ||
      metrics.auditCompletedAtMs > observedAtMs ||
      Math.abs(observedAtMs - metrics.auditCompletedAtMs - metrics.auditAgeMs) >
        15_000 ||
      metrics.auditAgeMs > PHASE3_MAX_AUDIT_AGE_MS
    ) {
      reasons.push(`sample ${index.toString()} MPF audit evidence is stale`);
    }

    const owner = sample?.owner;
    if (
      typeof owner?.durableRoot !== "string" ||
      !/^[0-9a-f]{64}$/u.test(owner.durableRoot) ||
      integer(owner?.residentNodes) === null ||
      owner.residentNodes < 0 ||
      owner.residentNodes > PHASE3_OWNER_MAX_RESIDENT_NODES ||
      integer(owner?.residentBytes) === null ||
      owner.residentBytes < 0 ||
      owner.residentBytes > PHASE3_OWNER_MAX_RESIDENT_BYTES ||
      integer(owner?.rssBytes) === null ||
      owner.rssBytes <= 0 ||
      owner.rssBytes > PHASE3_OWNER_MAX_RESIDENT_BYTES ||
      integer(owner?.peakRssBytes) === null ||
      owner.peakRssBytes <= 0 ||
      owner.peakRssBytes > PHASE3_OWNER_MAX_RESIDENT_BYTES ||
      integer(owner?.generatedNodes) === null ||
      owner.generatedNodes < 0 ||
      owner.generatedNodes > PHASE3_GENERATED_MAX_NODES ||
      integer(owner?.generatedBytes) === null ||
      owner.generatedBytes < 0 ||
      owner.generatedBytes > PHASE3_GENERATED_MAX_BYTES ||
      integer(owner?.activeGenerations) === null ||
      owner.activeGenerations < 0 ||
      integer(owner?.childRestarts) === null ||
      owner.childRestarts < 0
    ) {
      reasons.push(
        `sample ${index.toString()} breached an Architecture G owner cap`,
      );
    }
    if (baselineChildRestarts === null) {
      baselineChildRestarts = owner?.childRestarts;
    } else if (owner?.childRestarts !== baselineChildRestarts) {
      reasons.push("Architecture G owner restarted during the soak");
    }

    const processSample = sample?.process;
    if (
      integer(processSample?.pid) === null ||
      processSample.pid <= 0 ||
      processSample.pid !== isolation?.nodeHostPid ||
      typeof processSample?.startTicks !== "string" ||
      !/^[0-9]+$/u.test(processSample.startTicks) ||
      processSample.startTicks !== isolation?.nodeStartTicks ||
      integer(processSample?.rssBytes) === null ||
      processSample.rssBytes <= 0
    ) {
      reasons.push(
        `sample ${index.toString()} process memory identity is invalid`,
      );
    }
    if (expectedProcessStartTicks === null) {
      expectedProcessStartTicks = processSample?.startTicks;
    } else if (processSample?.startTicks !== expectedProcessStartTicks) {
      reasons.push("node process restarted during the soak");
    }
  }

  const lastElapsedMs = finite(samples.at(-1)?.elapsedMs);
  if (lastElapsedMs === null || lastElapsedMs < requiredDurationMs) {
    reasons.push("soak ended before the required duration");
  }
  if (
    finite(report?.startedAtMs) === null ||
    finite(report?.completedAtMs) === null ||
    report.completedAtMs - report.startedAtMs < requiredDurationMs
  ) {
    reasons.push("report wall-clock duration is incomplete");
  }
  const observation = report?.observation;
  if (
    finite(observation?.workloadSpawnedAtMs) === null ||
    finite(observation?.workloadExitedAtMs) === null ||
    finite(observation?.firstSampleAtMs) === null ||
    finite(observation?.lastSampleAtMs) === null ||
    observation.firstSampleAtMs !== samples[0]?.observedAtMs ||
    observation.lastSampleAtMs !== samples.at(-1)?.observedAtMs ||
    observation.firstSampleAtMs > observation.workloadSpawnedAtMs ||
    observation.workloadSpawnedAtMs > primaryStage?.startedAtMs ||
    observation.workloadExitedAtMs <
      primaryStage?.endedAtMs + primaryStage?.drainElapsedMs ||
    observation.lastSampleAtMs < observation.workloadExitedAtMs ||
    observation.workloadExitedAtMs - observation.workloadSpawnedAtMs >
      requiredDurationMs + PHASE3_WORKLOAD_LIFECYCLE_GRACE_MS ||
    report?.completedAtMs < observation.lastSampleAtMs
  ) {
    reasons.push(
      "safety sampling does not enclose workload measurement and drain",
    );
  }

  const fullScanDelta = counterDelta(
    samples,
    "confirmedLedgerFullScanTotal",
    reasons,
  );
  const auditCompletionTimes = samples.map((sample) =>
    finite(sample?.metrics?.auditCompletedAtMs),
  );
  let backgroundAuditFullScanCount = 0;
  if (auditCompletionTimes.some((value) => value === null)) {
    reasons.push("background audit completion identity is missing");
  } else {
    for (let index = 1; index < auditCompletionTimes.length; index += 1) {
      if (auditCompletionTimes[index] < auditCompletionTimes[index - 1]) {
        reasons.push("background audit completion time moved backwards");
      } else if (
        auditCompletionTimes[index] > auditCompletionTimes[index - 1]
      ) {
        backgroundAuditFullScanCount += 1;
      }
    }
  }
  const unplannedConfirmedLedgerFullScanDelta =
    fullScanDelta === null
      ? null
      : fullScanDelta - backgroundAuditFullScanCount;
  const timeoutDelta = counterDelta(
    samples,
    "timeoutInsteadOfBackpressureTotal",
    reasons,
  );
  const validationWorkerTimeoutDelta = counterDelta(
    samples,
    "validationWorkerTimeoutTotal",
    reasons,
  );
  const l1ControlPlaneTimeoutDelta = counterDelta(
    samples,
    "l1ControlPlaneTimeoutTotal",
    reasons,
  );
  if (unplannedConfirmedLedgerFullScanDelta !== 0)
    reasons.push("unplanned hot-path confirmed-ledger full scans must be zero");
  if (
    timeoutDelta !== 0 ||
    validationWorkerTimeoutDelta !== 0 ||
    l1ControlPlaneTimeoutDelta !== 0
  ) {
    reasons.push("timeout-instead-of-backpressure incidents must be zero");
  }

  const daSlopePerSec = slopePerSecond(
    samples,
    (sample) => sample.metrics.daPublicationBacklog,
  );
  const mergeSlopePerSec = slopePerSecond(
    samples,
    (sample) => sample.metrics.mergeQueueDepth,
  );
  if (daSlopePerSec === null || daSlopePerSec > 0) {
    reasons.push("DA publication queue slope must be <= 0");
  }
  if (mergeSlopePerSec === null || mergeSlopePerSec > 0) {
    reasons.push("merge queue slope must be <= 0");
  }

  const firstProcessRss = finite(samples[0]?.process?.rssBytes);
  const lastProcessRss = finite(samples.at(-1)?.process?.rssBytes);
  const processMemoryGrowthRatio =
    firstProcessRss === null || lastProcessRss === null || firstProcessRss <= 0
      ? null
      : Math.max(0, lastProcessRss - firstProcessRss) / firstProcessRss;
  if (
    processMemoryGrowthRatio === null ||
    processMemoryGrowthRatio >= PHASE3_PROCESS_MAX_DAILY_GROWTH_RATIO
  ) {
    reasons.push("node process RSS growth must be strictly below 10% per day");
  }

  return {
    passed: reasons.length === 0,
    reasons: [...new Set(reasons)],
    metrics: {
      sampleCount: samples.length,
      observedDurationSec: (lastElapsedMs ?? 0) / 1_000,
      auditDivergenceMaximum: maximumFinite(
        samples,
        (sample) => sample.metrics.auditDivergence,
      ),
      confirmedLedgerFullScanDelta: fullScanDelta,
      backgroundAuditFullScanCount,
      unplannedConfirmedLedgerFullScanDelta,
      timeoutInsteadOfBackpressureDelta: timeoutDelta,
      validationWorkerTimeoutDelta,
      l1ControlPlaneTimeoutDelta,
      daPublicationBacklogSlopePerSec: daSlopePerSec,
      mergeQueueSlopePerSec: mergeSlopePerSec,
      maximumOwnerResidentNodes: maximumFinite(
        samples,
        (sample) => sample.owner.residentNodes,
      ),
      maximumOwnerRssBytes: maximumFinite(
        samples,
        (sample) => sample.owner.rssBytes,
      ),
      maximumOwnerPeakRssBytes: maximumFinite(
        samples,
        (sample) => sample.owner.peakRssBytes,
      ),
      maximumGeneratedNodes: maximumFinite(
        samples,
        (sample) => sample.owner.generatedNodes,
      ),
      maximumGeneratedBytes: maximumFinite(
        samples,
        (sample) => sample.owner.generatedBytes,
      ),
      processMemoryGrowthRatio,
    },
    identities: {
      sourceTreeSha256: report?.identity?.source?.sourceTreeSha256 ?? null,
      runtimeSha256: report?.identity?.runtime?.sha256 ?? null,
      deploymentSha256: report?.identity?.deployment?.sha256 ?? null,
      ownerBinarySha256: report?.identity?.ownerBinary?.sha256 ?? null,
      phase1BindingSha256: report?.identity?.phase1?.sha256 ?? null,
      corpusPreflightSha256: corpusPreflight?.sha256 ?? null,
      loadGeneratorIsolationSha256: isolation?.sha256 ?? null,
      nodePreLifecycleRevalidationSha256:
        nodePreLifecycleRevalidation?.sha256 ?? null,
    },
  };
};

export const verifyPhase3ArchitectureGSoakReportFile = async (reportPath) => {
  const bytes = fs.readFileSync(reportPath);
  const report = JSON.parse(bytes.toString("utf8"));
  const evaluation = evaluatePhase3ArchitectureGSoakReport(report);
  const artifactReasons = evaluateClosureIdentityArtifacts(report?.identity, {
    skipPhase1Corpus: true,
  });
  const checkArtifact = (
    label,
    artifactPath,
    expectedSha256,
    expectedBytes = null,
  ) => {
    try {
      const stat = fs.lstatSync(artifactPath);
      if (
        !path.isAbsolute(artifactPath) ||
        !stat.isFile() ||
        stat.isSymbolicLink() ||
        (expectedBytes !== null && stat.size !== expectedBytes) ||
        sha256File(artifactPath) !== expectedSha256
      ) {
        artifactReasons.push(
          `${label} artifact bytes do not match the bound SHA-256`,
        );
      }
    } catch {
      artifactReasons.push(
        `${label} artifact is unavailable for offline verification`,
      );
    }
  };
  checkArtifact(
    "runtime",
    report?.identity?.runtime?.path,
    report?.identity?.runtime?.sha256,
  );
  checkArtifact(
    "deployment",
    report?.identity?.deployment?.path,
    report?.identity?.deployment?.sha256,
  );
  checkArtifact(
    "Phase 1",
    report?.identity?.phase1?.path,
    report?.identity?.phase1?.sha256,
  );
  checkArtifact(
    "corpus preflight",
    report?.identity?.corpusPreflight?.path,
    report?.identity?.corpusPreflight?.sha256,
    report?.identity?.corpusPreflight?.bytes,
  );
  checkArtifact(
    "load-generator isolation",
    report?.identity?.loadGeneratorIsolation?.path,
    report?.identity?.loadGeneratorIsolation?.sha256,
    report?.identity?.loadGeneratorIsolation?.bytes,
  );
  checkArtifact(
    "pre-lifecycle node revalidation",
    report?.identity?.nodePreLifecycleRevalidation?.path,
    report?.identity?.nodePreLifecycleRevalidation?.sha256,
    report?.identity?.nodePreLifecycleRevalidation?.bytes,
  );
  let isolationDocument = null;
  try {
    isolationDocument = JSON.parse(
      fs.readFileSync(report?.identity?.loadGeneratorIsolation?.path, "utf8"),
    );
    validatePhase3LoadGeneratorIsolationDocument(isolationDocument, {
      expectedNodeContainerId: report?.identity?.phase1?.nodeContainerId,
      expectedNodeImageId: report?.identity?.phase1?.nodeImageId,
    });
    const isolationSummary = report?.identity?.loadGeneratorIsolation;
    if (
      isolationDocument.schemaVersion !== isolationSummary?.schemaVersion ||
      isolationDocument.placement !== isolationSummary?.placement ||
      isolationDocument.loadGenerator.cpusAllowedList !==
        isolationSummary?.loadGeneratorCpusAllowedList ||
      isolationDocument.loadGenerator.uid.effective !==
        isolationSummary?.loadGeneratorEffectiveUid ||
      isolationDocument.node.cpusAllowedList !==
        isolationSummary?.nodeCpusAllowedList ||
      isolationDocument.nodeContainer.phase1ContainerId !==
        isolationSummary?.nodeContainerId ||
      isolationDocument.nodeContainer.phase1ImageId !==
        isolationSummary?.nodeImageId ||
      isolationDocument.node.pid !== isolationSummary?.nodeHostPid ||
      isolationDocument.node.startTicks !== isolationSummary?.nodeStartTicks ||
      isolationDocument.nodeContainer.readyEndpoint.url !==
        isolationSummary?.readyUrl ||
      isolationDocument.nodeContainer.metricsEndpoint.url !==
        isolationSummary?.metricsUrl ||
      isolationDocument.docker.client.realPath !==
        isolationSummary?.dockerClientRealPath ||
      isolationDocument.docker.client.sha256 !==
        isolationSummary?.dockerClientSha256 ||
      isolationDocument.docker.socket.realPath !==
        isolationSummary?.dockerSocketRealPath ||
      isolationDocument.docker.socket.dev !==
        isolationSummary?.dockerSocketDev ||
      isolationDocument.docker.socket.ino !==
        isolationSummary?.dockerSocketIno ||
      isolationDocument.docker.daemon.id !== isolationSummary?.dockerDaemonId
    ) {
      artifactReasons.push(
        "load-generator isolation artifact diverges from report identity",
      );
    }
    validateTrustedPhase3DockerRuntimeArtifacts(isolationDocument.docker);
  } catch {
    artifactReasons.push(
      "load-generator isolation artifact is unavailable or invalid",
    );
  }
  try {
    const revalidationDocument = JSON.parse(
      fs.readFileSync(
        report?.identity?.nodePreLifecycleRevalidation?.path,
        "utf8",
      ),
    );
    validatePhase3NodePreLifecycleRevalidationDocument(
      revalidationDocument,
      isolationDocument,
    );
    const expected = report?.identity?.nodePreLifecycleRevalidation;
    if (
      revalidationDocument.schemaVersion !== expected?.schemaVersion ||
      revalidationDocument.observedAtMs !== expected?.observedAtMs ||
      revalidationDocument.isolation.path !== expected?.isolationPath ||
      revalidationDocument.isolation.sha256 !== expected?.isolationSha256 ||
      revalidationDocument.nodeContainer.phase1ContainerId !==
        expected?.nodeContainerId ||
      revalidationDocument.nodeContainer.phase1ImageId !==
        expected?.nodeImageId ||
      revalidationDocument.node.pid !== expected?.nodeHostPid ||
      revalidationDocument.node.startTicks !== expected?.nodeStartTicks ||
      revalidationDocument.nodeContainer.restartCount !==
        expected?.nodeRestartCount ||
      revalidationDocument.nodeContainer.healthStatus !==
        expected?.nodeHealthStatus ||
      revalidationDocument.nodeContainer.readyEndpoint.url !==
        expected?.readyUrl ||
      revalidationDocument.nodeContainer.metricsEndpoint.url !==
        expected?.metricsUrl ||
      revalidationDocument.docker.client.sha256 !==
        expected?.dockerClientSha256 ||
      revalidationDocument.docker.socket.dev !== expected?.dockerSocketDev ||
      revalidationDocument.docker.socket.ino !== expected?.dockerSocketIno ||
      revalidationDocument.docker.daemon.id !== expected?.dockerDaemonId
    ) {
      artifactReasons.push(
        "pre-lifecycle node revalidation artifact diverges from report identity",
      );
    }
  } catch {
    artifactReasons.push(
      "pre-lifecycle node revalidation artifact is unavailable or invalid",
    );
  }
  let preflight = null;
  try {
    preflight = JSON.parse(
      fs.readFileSync(report?.identity?.corpusPreflight?.path, "utf8"),
    );
    const expected = report?.identity?.corpusPreflight;
    if (
      preflight?.schemaVersion !== PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA ||
      preflight?.sourceIdentity?.sourceTreeSha256 !==
        report?.identity?.source?.sourceTreeSha256 ||
      JSON.stringify(preflight?.sourceIdentity) !==
        JSON.stringify(report?.identity?.source) ||
      preflight?.sourceIdentitySha256 !== expected?.sourceIdentitySha256 ||
      phase3SoakSourceIdentitySha256(preflight?.sourceIdentity) !==
        expected?.sourceIdentitySha256 ||
      preflight?.phase1Binding?.path !== report?.identity?.phase1?.path ||
      preflight?.phase1Binding?.sha256 !== report?.identity?.phase1?.sha256 ||
      JSON.stringify(preflight?.files) !== JSON.stringify(expected?.files) ||
      JSON.stringify(preflight?.selection) !==
        JSON.stringify(expected?.selection) ||
      JSON.stringify(preflight?.validation) !==
        JSON.stringify(expected?.validation)
    ) {
      artifactReasons.push(
        "corpus preflight contents do not match the bound report identity",
      );
    }
    for (const [label, file] of Object.entries(preflight?.files ?? {})) {
      const stat = fs.lstatSync(file.path);
      if (
        !stat.isFile() ||
        stat.isSymbolicLink() ||
        stat.size !== file.bytes ||
        stat.mtimeMs !== file.mtimeMs ||
        stat.dev.toString() !== file.dev ||
        stat.ino.toString() !== file.ino
      ) {
        artifactReasons.push(
          `${label} changed after the bound full corpus preflight`,
        );
      }
    }
  } catch {
    artifactReasons.push(
      "corpus preflight is unavailable or malformed for offline verification",
    );
  }
  checkArtifact(
    "owner binary",
    report?.identity?.ownerBinary?.path,
    report?.identity?.ownerBinary?.sha256,
  );
  checkArtifact(
    "owner SHA-256 manifest",
    report?.identity?.ownerBinary?.sha256ManifestPath,
    report?.identity?.ownerBinary?.sha256ManifestSha256,
  );
  checkArtifact(
    "workload script",
    report?.workload?.scriptPath,
    report?.workload?.scriptSha256,
  );
  checkArtifact(
    "workload report",
    report?.workload?.reportPath,
    report?.workload?.reportSha256,
    report?.workload?.reportBytes,
  );
  try {
    const recomputedSummary = summarizePhase3WorkloadReport(
      JSON.parse(fs.readFileSync(report?.workload?.reportPath, "utf8")),
    );
    if (
      JSON.stringify(recomputedSummary) !==
      JSON.stringify(report?.workload?.reportSummary)
    ) {
      artifactReasons.push(
        "workload summary does not match the bound workload report",
      );
    }
  } catch {
    artifactReasons.push("workload report cannot be independently summarized");
  }
  checkArtifact(
    "submit records",
    report?.workload?.submitRecords?.path,
    report?.workload?.submitRecords?.sha256,
    report?.workload?.submitRecords?.bytes,
  );
  let scannedSubmitRecords = null;
  try {
    scannedSubmitRecords = await scanSubmitRecords(
      report?.workload?.submitRecords?.path,
    );
    if (
      scannedSubmitRecords.sha256 !== report?.workload?.submitRecords?.sha256 ||
      scannedSubmitRecords.bytes !== report?.workload?.submitRecords?.bytes ||
      scannedSubmitRecords.recordCount !==
        report?.workload?.submitRecords?.recordCount ||
      scannedSubmitRecords.successCount !==
        report?.workload?.submitRecords?.successCount ||
      scannedSubmitRecords.errorCount !==
        report?.workload?.submitRecords?.errorCount ||
      scannedSubmitRecords.timeoutCount !==
        report?.workload?.submitRecords?.timeoutCount ||
      scannedSubmitRecords.attemptSequenceSha256 !==
        report?.workload?.submitRecords?.attemptSequenceSha256
    ) {
      artifactReasons.push(
        "submit-record scan does not match the bound evidence identity",
      );
    }
  } catch {
    artifactReasons.push(
      "submit-record evidence is unavailable or malformed for offline verification",
    );
  }
  try {
    const fullIndex = await loadCorpusIndex(
      report?.identity?.phase1?.corpus?.indexPath,
    );
    const selectedEntries = selectCorpusIndexEntries({
      index: fullIndex,
      corpusSliceId: report?.workload?.reportSummary?.corpus?.sliceId,
      corpusShape: report?.workload?.reportSummary?.corpus?.shape,
      maxChains: null,
    });
    const scannedCorpus = await scanCorpusPrefixEvidence({
      corpusPath: report?.identity?.phase1?.corpus?.path,
      fullIndex,
      selectedEntries,
      consumption: report?.workload?.reportSummary?.corpus?.consumption,
      expectedCorpusSha256: preflight?.files?.corpus?.sha256,
    });
    if (
      scannedCorpus.consumedRowCount !== scannedSubmitRecords?.recordCount ||
      scannedCorpus.corpusSha256 !==
        report?.identity?.phase1?.corpus?.corpusSha256
    ) {
      artifactReasons.push(
        "consumed corpus prefix does not match submit-attempt cardinality or bound identity",
      );
    }
  } catch (error) {
    artifactReasons.push(
      `consumed corpus prefix cannot be verified against the bound corpus: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  checkArtifact(
    "soak runner",
    report?.identity?.tooling?.runnerPath,
    report?.identity?.tooling?.runnerSha256,
  );
  checkArtifact(
    "soak verifier",
    report?.identity?.tooling?.verifierPath,
    report?.identity?.tooling?.verifierSha256,
  );
  const reasons = [...new Set([...evaluation.reasons, ...artifactReasons])];
  return {
    ...evaluation,
    passed: reasons.length === 0,
    reasons,
    reportPath: path.resolve(reportPath),
    reportSha256: sha256(bytes),
  };
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);
if (isMain) {
  const reportPath = process.argv[2];
  if (reportPath === undefined) {
    console.error(
      "usage: verify-phase3-architecture-g-soak-report.mjs <report.json>",
    );
    process.exitCode = 2;
  } else {
    verifyPhase3ArchitectureGSoakReportFile(reportPath)
      .then((verification) => {
        console.log(JSON.stringify(verification, null, 2));
        if (!verification.passed) process.exitCode = 1;
      })
      .catch((error) => {
        console.error(error instanceof Error ? error.message : String(error));
        process.exitCode = 1;
      });
  }
}
