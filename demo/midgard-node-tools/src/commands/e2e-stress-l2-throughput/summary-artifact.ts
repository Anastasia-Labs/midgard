import { isDeepStrictEqual } from "node:util";

import {
  arrayOf,
  booleanValue,
  exactLiteral,
  exactRecord,
  finiteNumber,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  nullableNonEmptyString,
  nullableNonNegativeNumber,
  oneOf,
  positiveInteger,
  stringArray,
} from "midgard-node/artifact-schema";

import { roundMetric } from "../stress-stage-metrics.js";
import { assertMeasurementPolicy } from "./config-artifact.js";
import { E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION } from "./constants.js";
import { measurementPolicyForConfig } from "./policy.js";
import { type E2EL2StressSummary } from "./types.js";

const parseLowerHex64 = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(parsed)) {
    throw new Error(`${label} must be 64 lowercase hexadecimal characters`);
  }
  return parsed;
};

const assertStressMetricWindow = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "status",
    "count",
    "startedAt",
    "finishedAt",
    "durationMs",
    "perSecond",
    "source",
    "precision",
    "missingCount",
    "notes",
  ]);
  const status = oneOf(input.status, `${label}.status`, [
    "complete",
    "partial",
    "unavailable",
  ]);
  const count = nonNegativeInteger(input.count, `${label}.count`);
  const startedAt =
    input.startedAt === null
      ? null
      : isoTimestamp(input.startedAt, `${label}.startedAt`);
  const finishedAt =
    input.finishedAt === null
      ? null
      : isoTimestamp(input.finishedAt, `${label}.finishedAt`);
  const durationMs = nullableNonNegativeNumber(
    input.durationMs,
    `${label}.durationMs`,
  );
  const perSecond = nullableNonNegativeNumber(
    input.perSecond,
    `${label}.perSecond`,
  );
  nonEmptyString(input.source, `${label}.source`);
  oneOf(input.precision, `${label}.precision`, [
    "db_timestamp",
    "observer_timestamp",
    "artifact_timestamp",
  ]);
  const missingCount = nonNegativeInteger(
    input.missingCount,
    `${label}.missingCount`,
  );
  const notes = stringArray(input.notes, `${label}.notes`);
  const hasCompleteRange = startedAt !== null && finishedAt !== null;
  const elapsedMs = hasCompleteRange
    ? Date.parse(finishedAt) - Date.parse(startedAt)
    : null;
  const expectedPerSecond =
    elapsedMs === null || elapsedMs <= 0
      ? null
      : roundMetric(count / (elapsedMs / 1_000));
  const unavailableIsCanonical =
    status === "unavailable" &&
    count === 0 &&
    startedAt === null &&
    finishedAt === null &&
    durationMs === null &&
    perSecond === null;
  const observedIsCanonical =
    status !== "unavailable" &&
    count > 0 &&
    ((hasCompleteRange &&
      elapsedMs! >= 0 &&
      durationMs === elapsedMs &&
      perSecond === expectedPerSecond &&
      (status === "complete" ? missingCount === 0 : missingCount > 0)) ||
      (!hasCompleteRange &&
        status === "partial" &&
        durationMs === null &&
        perSecond === null));
  if (
    (!unavailableIsCanonical && !observedIsCanonical) ||
    new Set(notes).size !== notes.length
  ) {
    throw new Error(`${label} status, window, rate, or notes are inconsistent`);
  }
};

const assertStressMetrics = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "clientSubmission",
    "durableAdmission",
    "l2Admission",
    "l1Commit",
    "immutableObservation",
    "fullFinality",
  ]);
  assertStressMetricWindow(input.clientSubmission, `${label}.clientSubmission`);
  assertStressMetricWindow(input.durableAdmission, `${label}.durableAdmission`);
  assertStressMetricWindow(input.l2Admission, `${label}.l2Admission`);
  const l1Commit = exactRecord(input.l1Commit, `${label}.l1Commit`, [
    "headers",
    "l2Transactions",
  ]);
  assertStressMetricWindow(l1Commit.headers, `${label}.l1Commit.headers`);
  assertStressMetricWindow(
    l1Commit.l2Transactions,
    `${label}.l1Commit.l2Transactions`,
  );
  assertStressMetricWindow(
    input.immutableObservation,
    `${label}.immutableObservation`,
  );
  assertStressMetricWindow(input.fullFinality, `${label}.fullFinality`);
};

const assertStagePercentiles = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "p50Ms",
    "p95Ms",
    "p99Ms",
    "sampleCount",
  ]);
  nullableNonNegativeNumber(input.p50Ms, `${label}.p50Ms`);
  nullableNonNegativeNumber(input.p95Ms, `${label}.p95Ms`);
  nullableNonNegativeNumber(input.p99Ms, `${label}.p99Ms`);
  nonNegativeInteger(input.sampleCount, `${label}.sampleCount`);
};

const assertSteadyStateStage = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "stage",
    "offeredCount",
    "rawCount",
    "trimmedCount",
    "rawPerSecond",
    "steadyStatePerSecond",
    "latency",
    "windowTrim",
    "precision",
    "notes",
  ]);
  nonEmptyString(input.stage, `${label}.stage`);
  nonNegativeInteger(input.offeredCount, `${label}.offeredCount`);
  nonNegativeInteger(input.rawCount, `${label}.rawCount`);
  nonNegativeInteger(input.trimmedCount, `${label}.trimmedCount`);
  nullableNonNegativeNumber(input.rawPerSecond, `${label}.rawPerSecond`);
  nullableNonNegativeNumber(
    input.steadyStatePerSecond,
    `${label}.steadyStatePerSecond`,
  );
  assertStagePercentiles(input.latency, `${label}.latency`);
  const windowTrim = exactRecord(input.windowTrim, `${label}.windowTrim`, [
    "discardedHeadMs",
    "discardedTailMs",
  ]);
  nonNegativeNumber(
    windowTrim.discardedHeadMs,
    `${label}.windowTrim.discardedHeadMs`,
  );
  nonNegativeNumber(
    windowTrim.discardedTailMs,
    `${label}.windowTrim.discardedTailMs`,
  );
  exactLiteral(input.precision, `${label}.precision`, "db_timestamp");
  stringArray(input.notes, `${label}.notes`);
};

const assertEnvironmentFingerprint = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "schemaVersion",
    "gitSha",
    "imageDigests",
    "hostCpu",
    "hostRamBytes",
    "hostname",
    "loadGenCoHosted",
    "loadGeneratorPlacement",
    "clockOffsetMs",
    "calibrationProofRef",
    "configProfileHash",
    "fixedKnobs",
    "capturedAt",
    "notes",
  ]);
  exactLiteral(input.schemaVersion, `${label}.schemaVersion`, 1);
  nullableNonEmptyString(input.gitSha, `${label}.gitSha`);
  const imageDigests = exactRecord(
    input.imageDigests,
    `${label}.imageDigests`,
    ["midgardNode", "postgres"],
  );
  nullableNonEmptyString(
    imageDigests.midgardNode,
    `${label}.imageDigests.midgardNode`,
  );
  nullableNonEmptyString(
    imageDigests.postgres,
    `${label}.imageDigests.postgres`,
  );
  const hostCpu = exactRecord(input.hostCpu, `${label}.hostCpu`, [
    "model",
    "count",
    "speedMhz",
  ]);
  nullableNonEmptyString(hostCpu.model, `${label}.hostCpu.model`);
  nonNegativeInteger(hostCpu.count, `${label}.hostCpu.count`);
  nullableNonNegativeNumber(hostCpu.speedMhz, `${label}.hostCpu.speedMhz`);
  nonNegativeNumber(input.hostRamBytes, `${label}.hostRamBytes`);
  nonEmptyString(input.hostname, `${label}.hostname`);
  if (input.loadGenCoHosted !== null) {
    booleanValue(input.loadGenCoHosted, `${label}.loadGenCoHosted`);
  }
  oneOf(input.loadGeneratorPlacement, `${label}.loadGeneratorPlacement`, [
    "separate-host",
    "separate-container",
    "node-container",
    "unknown",
  ]);
  if (input.clockOffsetMs !== null) {
    finiteNumber(input.clockOffsetMs, `${label}.clockOffsetMs`);
  }
  nullableNonEmptyString(
    input.calibrationProofRef,
    `${label}.calibrationProofRef`,
  );
  parseLowerHex64(input.configProfileHash, `${label}.configProfileHash`);
  const fixedKnobs = exactRecord(input.fixedKnobs, `${label}.fixedKnobs`, [
    "nodePostgresPoolMaxConnections",
    "validationBatchHardCap",
    "validationMinBatch",
    "validationPhaseAMaxEffectiveConcurrency",
  ]);
  positiveInteger(
    fixedKnobs.nodePostgresPoolMaxConnections,
    `${label}.fixedKnobs.nodePostgresPoolMaxConnections`,
  );
  positiveInteger(
    fixedKnobs.validationBatchHardCap,
    `${label}.fixedKnobs.validationBatchHardCap`,
  );
  positiveInteger(
    fixedKnobs.validationMinBatch,
    `${label}.fixedKnobs.validationMinBatch`,
  );
  positiveInteger(
    fixedKnobs.validationPhaseAMaxEffectiveConcurrency,
    `${label}.fixedKnobs.validationPhaseAMaxEffectiveConcurrency`,
  );
  isoTimestamp(input.capturedAt, `${label}.capturedAt`);
  stringArray(input.notes, `${label}.notes`);
};

const assertGroundTruthMetrics = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "schemaVersion",
    "window",
    "stages",
    "fingerprint",
  ]);
  exactLiteral(input.schemaVersion, `${label}.schemaVersion`, 1);
  const window = exactRecord(input.window, `${label}.window`, [
    "start",
    "end",
    "trimFraction",
  ]);
  isoTimestamp(window.start, `${label}.window.start`);
  isoTimestamp(window.end, `${label}.window.end`);
  nonNegativeNumber(window.trimFraction, `${label}.window.trimFraction`);
  const stages = exactRecord(input.stages, `${label}.stages`, [
    "admission",
    "validationStart",
    "validationTerminal",
    "mempoolPersist",
    "l1CommitHeader",
    "l1CommitConfirm",
    "immutableObservation",
    "fullFinality",
  ]);
  for (const stage of [
    "admission",
    "validationStart",
    "validationTerminal",
    "mempoolPersist",
    "l1CommitHeader",
    "l1CommitConfirm",
    "immutableObservation",
    "fullFinality",
  ] as const) {
    assertSteadyStateStage(stages[stage], `${label}.stages.${stage}`);
  }
  assertEnvironmentFingerprint(input.fingerprint, `${label}.fingerprint`);
};

const assertCorpusRow = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "txHash",
    "canonicalCborHex",
    "canonicalCborSha256",
    "canonicalCborByteLength",
    "senderWalletId",
    "selectedInputOutref",
    "outputOutrefs",
    "planShape",
    "parentTxHash",
    "corpusSliceId",
  ]);
  parseLowerHex64(input.txHash, `${label}.txHash`);
  const cborHex = nonEmptyString(
    input.canonicalCborHex,
    `${label}.canonicalCborHex`,
  );
  if (!/^(?:[0-9a-f]{2})+$/u.test(cborHex)) {
    throw new Error(`${label}.canonicalCborHex must be lowercase whole bytes`);
  }
  parseLowerHex64(input.canonicalCborSha256, `${label}.canonicalCborSha256`);
  positiveInteger(
    input.canonicalCborByteLength,
    `${label}.canonicalCborByteLength`,
  );
  nonEmptyString(input.senderWalletId, `${label}.senderWalletId`);
  nonEmptyString(input.selectedInputOutref, `${label}.selectedInputOutref`);
  stringArray(input.outputOutrefs, `${label}.outputOutrefs`);
  oneOf(input.planShape, `${label}.planShape`, ["fanout", "chain", "mixed"]);
  if (input.parentTxHash !== null) {
    parseLowerHex64(input.parentTxHash, `${label}.parentTxHash`);
  }
  nonEmptyString(input.corpusSliceId, `${label}.corpusSliceId`);
};

const assertCorpusPlan = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "rows",
    "requiredTransactionCount",
    "selectedTransactionCount",
    "corpusShape",
    "corpusSliceId",
  ]);
  arrayOf(input.rows, `${label}.rows`, (entry, entryLabel) => {
    assertCorpusRow(entry, entryLabel);
    return entry;
  });
  nonNegativeInteger(
    input.requiredTransactionCount,
    `${label}.requiredTransactionCount`,
  );
  nonNegativeInteger(
    input.selectedTransactionCount,
    `${label}.selectedTransactionCount`,
  );
  oneOf(input.corpusShape, `${label}.corpusShape`, [
    "fanout",
    "chain",
    "mixed",
  ]);
  nonEmptyString(input.corpusSliceId, `${label}.corpusSliceId`);
};

const assertScheduleSlip = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, ["p50", "p95", "p99", "max"]);
  nonNegativeNumber(input.p50, `${label}.p50`);
  nonNegativeNumber(input.p95, `${label}.p95`);
  nonNegativeNumber(input.p99, `${label}.p99`);
  nonNegativeNumber(input.max, `${label}.max`);
};

const assertOpenLoopSubmitSummary = (
  value: unknown,
  label: string,
  calibration: boolean,
): void => {
  const baseKeys = [
    "offeredCount",
    "submittedCount",
    "failedCount",
    "targetRateTps",
    "maxInFlight",
    "maxObservedInFlight",
    "startedAtIso",
    "finishedAtIso",
    "durationMs",
    "achievedRateTps",
    "submittedOfferedRatio",
    "scheduleSlipMs",
  ] as const;
  const calibrationKeys = [
    "endpoint",
    "minRequiredRateTps",
    "p95ScheduleSlipLimitMs",
    "p99ScheduleSlipLimitMs",
    "passed",
    "cpuUserMicros",
    "cpuSystemMicros",
    "notes",
  ] as const;
  const input = exactRecord(
    value,
    label,
    calibration ? [...baseKeys, ...calibrationKeys] : baseKeys,
    calibration ? ["eventLoopUtilization"] : [],
  );
  nonNegativeInteger(input.offeredCount, `${label}.offeredCount`);
  nonNegativeInteger(input.submittedCount, `${label}.submittedCount`);
  nonNegativeInteger(input.failedCount, `${label}.failedCount`);
  nonNegativeNumber(input.targetRateTps, `${label}.targetRateTps`);
  positiveInteger(input.maxInFlight, `${label}.maxInFlight`);
  nonNegativeInteger(input.maxObservedInFlight, `${label}.maxObservedInFlight`);
  isoTimestamp(input.startedAtIso, `${label}.startedAtIso`);
  isoTimestamp(input.finishedAtIso, `${label}.finishedAtIso`);
  nonNegativeNumber(input.durationMs, `${label}.durationMs`);
  nonNegativeNumber(input.achievedRateTps, `${label}.achievedRateTps`);
  nonNegativeNumber(
    input.submittedOfferedRatio,
    `${label}.submittedOfferedRatio`,
  );
  assertScheduleSlip(input.scheduleSlipMs, `${label}.scheduleSlipMs`);
  if (calibration) {
    nonEmptyString(input.endpoint, `${label}.endpoint`);
    nonNegativeNumber(input.minRequiredRateTps, `${label}.minRequiredRateTps`);
    nonNegativeNumber(
      input.p95ScheduleSlipLimitMs,
      `${label}.p95ScheduleSlipLimitMs`,
    );
    nonNegativeNumber(
      input.p99ScheduleSlipLimitMs,
      `${label}.p99ScheduleSlipLimitMs`,
    );
    booleanValue(input.passed, `${label}.passed`);
    if (input.eventLoopUtilization !== undefined) {
      nonNegativeNumber(
        input.eventLoopUtilization,
        `${label}.eventLoopUtilization`,
      );
    }
    nonNegativeNumber(input.cpuUserMicros, `${label}.cpuUserMicros`);
    nonNegativeNumber(input.cpuSystemMicros, `${label}.cpuSystemMicros`);
    stringArray(input.notes, `${label}.notes`);
  }
};

const assertOpenLoopPlacement = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "processPid",
    "cwd",
    "insideMidgardNodeProcess",
    "insideMidgardNodeContainer",
    "validForUpperBoundClaim",
    "notes",
  ]);
  positiveInteger(input.processPid, `${label}.processPid`);
  nonEmptyString(input.cwd, `${label}.cwd`);
  booleanValue(
    input.insideMidgardNodeProcess,
    `${label}.insideMidgardNodeProcess`,
  );
  booleanValue(
    input.insideMidgardNodeContainer,
    `${label}.insideMidgardNodeContainer`,
  );
  booleanValue(
    input.validForUpperBoundClaim,
    `${label}.validForUpperBoundClaim`,
  );
  stringArray(input.notes, `${label}.notes`);
};

const assertOpenLoopSummary = (value: unknown, label: string): void => {
  const input = exactRecord(
    value,
    label,
    [
      "targetRateTps",
      "durationMs",
      "maxInFlight",
      "corpus",
      "submission",
      "placement",
    ],
    ["calibration"],
  );
  nonNegativeNumber(input.targetRateTps, `${label}.targetRateTps`);
  positiveInteger(input.durationMs, `${label}.durationMs`);
  positiveInteger(input.maxInFlight, `${label}.maxInFlight`);
  assertCorpusPlan(input.corpus, `${label}.corpus`);
  assertOpenLoopSubmitSummary(input.submission, `${label}.submission`, false);
  if (input.calibration !== undefined) {
    assertOpenLoopSubmitSummary(
      input.calibration,
      `${label}.calibration`,
      true,
    );
  }
  assertOpenLoopPlacement(input.placement, `${label}.placement`);
};

const assertStressTransaction = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "index",
    "phase",
    "txHash",
    "senderAddress",
    "destinationAddress",
    "selectedInputs",
    "submission",
    "acceptance",
    "finality",
    "workerIndex",
    "walletSeedSource",
  ]);
  nonNegativeInteger(input.index, `${label}.index`);
  exactLiteral(input.phase, `${label}.phase`, "stress");
  if (input.txHash !== null) {
    parseLowerHex64(input.txHash, `${label}.txHash`);
  }
  nonEmptyString(input.senderAddress, `${label}.senderAddress`);
  nonEmptyString(input.destinationAddress, `${label}.destinationAddress`);
  stringArray(input.selectedInputs, `${label}.selectedInputs`);
  const submission = exactRecord(
    input.submission,
    `${label}.submission`,
    ["status", "submittedAt"],
    ["durationMs", "error"],
  );
  oneOf(submission.status, `${label}.submission.status`, [
    "submitted",
    "failed",
  ]);
  if (submission.submittedAt !== null) {
    isoTimestamp(submission.submittedAt, `${label}.submission.submittedAt`);
  }
  if (submission.durationMs !== undefined) {
    nonNegativeNumber(submission.durationMs, `${label}.submission.durationMs`);
  }
  if (submission.error !== undefined) {
    nonEmptyString(submission.error, `${label}.submission.error`);
  }
  const acceptance = exactRecord(
    input.acceptance,
    `${label}.acceptance`,
    ["status"],
    ["acceptedAt", "durationMs", "error"],
  );
  oneOf(acceptance.status, `${label}.acceptance.status`, [
    "accepted",
    "rejected",
    "timeout",
    "not_observed",
    "not_submitted",
  ]);
  if (acceptance.acceptedAt !== undefined) {
    isoTimestamp(acceptance.acceptedAt, `${label}.acceptance.acceptedAt`);
  }
  if (acceptance.durationMs !== undefined) {
    nonNegativeNumber(acceptance.durationMs, `${label}.acceptance.durationMs`);
  }
  if (acceptance.error !== undefined) {
    nonEmptyString(acceptance.error, `${label}.acceptance.error`);
  }
  const finality = exactRecord(
    input.finality,
    `${label}.finality`,
    ["status"],
    ["committedAt", "durationMs", "error"],
  );
  oneOf(finality.status, `${label}.finality.status`, [
    "committed",
    "rejected",
    "timeout",
    "not_observed",
  ]);
  if (finality.committedAt !== undefined) {
    isoTimestamp(finality.committedAt, `${label}.finality.committedAt`);
  }
  if (finality.durationMs !== undefined) {
    nonNegativeNumber(finality.durationMs, `${label}.finality.durationMs`);
  }
  if (finality.error !== undefined) {
    nonEmptyString(finality.error, `${label}.finality.error`);
  }
  nonNegativeInteger(input.workerIndex, `${label}.workerIndex`);
  nonEmptyString(input.walletSeedSource, `${label}.walletSeedSource`);
};

export function parseE2EL2StressSummary(value: unknown): E2EL2StressSummary {
  const label = "E2E L2 stress summary";
  const input = exactRecord(
    value,
    label,
    [
      "schemaVersion",
      "runId",
      "status",
      "loadModel",
      "workloadProfile",
      "classification",
      "rateSemantics",
      "burstCycleRatePerSecond",
      "mode",
      "measurementPolicy",
      "requestedCount",
      "notStartedCount",
      "submittedCount",
      "submissionFailedCount",
      "acceptedCount",
      "acceptanceNotObservedCount",
      "acceptanceTimedOutCount",
      "finalityTimedOutCount",
      "observedCommittedCount",
      "unknownFinalityCount",
      "rejectedCount",
      "concurrency",
      "finalityObserver",
      "startedAt",
      "submissionFinishedAt",
      "finishedAt",
      "submissionDurationMs",
      "durationMs",
      "metrics",
      "latencyMs",
      "artifactPaths",
      "transactions",
    ],
    [
      "interruptedReason",
      "corpusShape",
      "openLoop",
      "groundTruth",
      "fingerprint",
    ],
  );
  if (input.schemaVersion !== E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION}`,
    );
  }
  nonEmptyString(input.runId, `${label}.runId`);
  oneOf(input.status, `${label}.status`, ["completed", "interrupted"]);
  if (input.interruptedReason !== undefined) {
    nonEmptyString(input.interruptedReason, `${label}.interruptedReason`);
  }
  oneOf(input.loadModel, `${label}.loadModel`, [
    "closed-loop-smoke",
    "open-loop-upper-bound",
  ]);
  oneOf(input.workloadProfile, `${label}.workloadProfile`, [
    "synthetic-admission",
    "production-end-user",
  ]);
  if (input.corpusShape !== undefined) {
    oneOf(input.corpusShape, `${label}.corpusShape`, [
      "fanout",
      "chain",
      "mixed",
    ]);
  }
  oneOf(input.classification, `${label}.classification`, [
    "closed_loop_smoke",
    "full_pipeline_sustained",
    "ingress_ok_commit_failed",
    "admission_bottleneck",
    "validation_bottleneck",
    "commit_planner_bottleneck",
    "da_bottleneck",
    "merge_bottleneck",
    "provider_bottleneck",
    "observer_overloaded",
    "client_overloaded",
    "corpus_exhausted",
    "duplicate_submission",
  ]);
  oneOf(input.rateSemantics, `${label}.rateSemantics`, [
    "burst_cycle_rate",
    "offered_tps_uncalibrated",
  ]);
  nullableNonNegativeNumber(
    input.burstCycleRatePerSecond,
    `${label}.burstCycleRatePerSecond`,
  );
  oneOf(input.mode, `${label}.mode`, ["serial-chain", "parallel-fanout"]);
  assertMeasurementPolicy(
    input.measurementPolicy,
    `${label}.measurementPolicy`,
  );
  if (input.openLoop !== undefined) {
    assertOpenLoopSummary(input.openLoop, `${label}.openLoop`);
  }
  for (const countKey of [
    "requestedCount",
    "notStartedCount",
    "submittedCount",
    "submissionFailedCount",
    "acceptedCount",
    "acceptanceNotObservedCount",
    "acceptanceTimedOutCount",
    "finalityTimedOutCount",
    "observedCommittedCount",
    "unknownFinalityCount",
    "rejectedCount",
  ] as const) {
    nonNegativeInteger(input[countKey], `${label}.${countKey}`);
  }
  positiveInteger(input.concurrency, `${label}.concurrency`);
  const finalityObserver = exactRecord(
    input.finalityObserver,
    `${label}.finalityObserver`,
    [
      "mode",
      "maxConcurrentRequests",
      "maxObservedConcurrentRequests",
      "observedTransactionCount",
      "pollRequestCount",
      "batchCount",
      "errorCount",
    ],
  );
  oneOf(finalityObserver.mode, `${label}.finalityObserver.mode`, [
    "post-submit-bounded",
    "aggregate-window",
  ]);
  for (const countKey of [
    "maxConcurrentRequests",
    "maxObservedConcurrentRequests",
    "observedTransactionCount",
    "pollRequestCount",
    "batchCount",
    "errorCount",
  ] as const) {
    nonNegativeInteger(
      finalityObserver[countKey],
      `${label}.finalityObserver.${countKey}`,
    );
  }
  isoTimestamp(input.startedAt, `${label}.startedAt`);
  isoTimestamp(input.submissionFinishedAt, `${label}.submissionFinishedAt`);
  isoTimestamp(input.finishedAt, `${label}.finishedAt`);
  nonNegativeNumber(
    input.submissionDurationMs,
    `${label}.submissionDurationMs`,
  );
  nonNegativeNumber(input.durationMs, `${label}.durationMs`);
  assertStressMetrics(input.metrics, `${label}.metrics`);
  if (input.groundTruth !== undefined) {
    assertGroundTruthMetrics(input.groundTruth, `${label}.groundTruth`);
  }
  if (input.fingerprint !== undefined) {
    assertEnvironmentFingerprint(input.fingerprint, `${label}.fingerprint`);
  }
  const latency = exactRecord(input.latencyMs, `${label}.latencyMs`, [
    "submitP50",
    "submitP95",
    "acceptanceP50",
    "acceptanceP95",
    "commitP50",
    "commitP95",
  ]);
  for (const key of [
    "submitP50",
    "submitP95",
    "acceptanceP50",
    "acceptanceP95",
    "commitP50",
    "commitP95",
  ] as const) {
    nonNegativeNumber(latency[key], `${label}.latencyMs.${key}`);
  }
  const artifactPaths = exactRecord(
    input.artifactPaths,
    `${label}.artifactPaths`,
    ["configJson", "eventsNdjson", "summaryJson", "summaryMarkdown"],
    [
      "engineReportJson",
      "engineEventsNdjson",
      "submitRecordsNdjson",
      "noOpCalibrationJson",
    ],
  );
  for (const key of [
    "configJson",
    "eventsNdjson",
    "summaryJson",
    "summaryMarkdown",
  ] as const) {
    nonEmptyString(artifactPaths[key], `${label}.artifactPaths.${key}`);
  }
  for (const key of [
    "engineReportJson",
    "engineEventsNdjson",
    "submitRecordsNdjson",
    "noOpCalibrationJson",
  ] as const) {
    if (artifactPaths[key] !== undefined) {
      nonEmptyString(artifactPaths[key], `${label}.artifactPaths.${key}`);
    }
  }
  arrayOf(input.transactions, `${label}.transactions`, (entry, entryLabel) => {
    assertStressTransaction(entry, entryLabel);
    return entry;
  });
  const parsed = value as E2EL2StressSummary;
  const expectedPolicy = measurementPolicyForConfig({
    loadModel: parsed.loadModel,
    workloadProfile: parsed.workloadProfile,
  });
  const startedAtMs = Date.parse(parsed.startedAt);
  const submissionFinishedAtMs = Date.parse(parsed.submissionFinishedAt);
  const finishedAtMs = Date.parse(parsed.finishedAt);
  const expectedSubmissionDurationMs = submissionFinishedAtMs - startedAtMs;
  const expectedDurationMs = finishedAtMs - startedAtMs;
  const transactionIndexes = parsed.transactions.map(
    (transaction) => transaction.index,
  );
  const transactionHashes = parsed.transactions.flatMap((transaction) =>
    transaction.txHash === null ? [] : [transaction.txHash],
  );
  const expectedSubmittedCount = parsed.transactions.filter(
    (transaction) => transaction.submission.status === "submitted",
  ).length;
  const expectedSubmissionFailedCount = parsed.transactions.filter(
    (transaction) => transaction.submission.status === "failed",
  ).length;
  const expectedAcceptedCount = parsed.transactions.filter(
    (transaction) => transaction.acceptance.status === "accepted",
  ).length;
  const expectedAcceptanceNotObservedCount = parsed.transactions.filter(
    (transaction) => transaction.acceptance.status === "not_observed",
  ).length;
  const expectedAcceptanceTimedOutCount = parsed.transactions.filter(
    (transaction) => transaction.acceptance.status === "timeout",
  ).length;
  const expectedFinalityTimedOutCount = parsed.transactions.filter(
    (transaction) => transaction.finality.status === "timeout",
  ).length;
  const expectedObservedCommittedCount = parsed.transactions.filter(
    (transaction) => transaction.finality.status === "committed",
  ).length;
  const expectedUnknownFinalityCount = parsed.transactions.filter(
    (transaction) =>
      transaction.acceptance.status === "accepted" &&
      transaction.finality.status === "not_observed",
  ).length;
  const expectedRejectedCount = parsed.transactions.filter(
    (transaction) =>
      transaction.acceptance.status === "rejected" ||
      transaction.finality.status === "rejected",
  ).length;
  const expectedNotStartedCount =
    parsed.requestedCount - parsed.transactions.length;
  const statusReasonIsCanonical =
    (parsed.status === "completed" && parsed.interruptedReason === undefined) ||
    (parsed.status === "interrupted" && parsed.interruptedReason !== undefined);
  const loadModelLanguageIsCanonical =
    (parsed.loadModel === "closed-loop-smoke" &&
      parsed.openLoop === undefined &&
      parsed.corpusShape === undefined &&
      parsed.classification === "closed_loop_smoke" &&
      parsed.rateSemantics === "burst_cycle_rate" &&
      parsed.burstCycleRatePerSecond === parsed.metrics.l2Admission.perSecond &&
      parsed.finalityObserver.mode === "post-submit-bounded") ||
    (parsed.loadModel === "open-loop-upper-bound" &&
      parsed.openLoop !== undefined &&
      parsed.corpusShape !== undefined &&
      parsed.classification !== "closed_loop_smoke" &&
      parsed.rateSemantics === "offered_tps_uncalibrated" &&
      parsed.burstCycleRatePerSecond === null &&
      parsed.finalityObserver.mode === "aggregate-window");
  if (
    !statusReasonIsCanonical ||
    !loadModelLanguageIsCanonical ||
    !isDeepStrictEqual(parsed.measurementPolicy, expectedPolicy) ||
    expectedSubmissionDurationMs < 0 ||
    expectedDurationMs < expectedSubmissionDurationMs ||
    parsed.submissionDurationMs !== expectedSubmissionDurationMs ||
    parsed.durationMs !== expectedDurationMs ||
    expectedNotStartedCount < 0 ||
    parsed.notStartedCount !== expectedNotStartedCount ||
    parsed.submittedCount !== expectedSubmittedCount ||
    parsed.submissionFailedCount !== expectedSubmissionFailedCount ||
    parsed.acceptedCount !== expectedAcceptedCount ||
    parsed.acceptanceNotObservedCount !== expectedAcceptanceNotObservedCount ||
    parsed.acceptanceTimedOutCount !== expectedAcceptanceTimedOutCount ||
    parsed.finalityTimedOutCount !== expectedFinalityTimedOutCount ||
    parsed.observedCommittedCount !== expectedObservedCommittedCount ||
    parsed.unknownFinalityCount !== expectedUnknownFinalityCount ||
    parsed.rejectedCount !== expectedRejectedCount ||
    parsed.submittedCount + parsed.submissionFailedCount !==
      parsed.transactions.length ||
    new Set(transactionIndexes).size !== transactionIndexes.length ||
    transactionIndexes.some(
      (index, position) =>
        index < 0 ||
        index >= parsed.requestedCount ||
        (position > 0 && index <= transactionIndexes[position - 1]!),
    ) ||
    new Set(transactionHashes).size !== transactionHashes.length ||
    parsed.finalityObserver.maxObservedConcurrentRequests >
      parsed.finalityObserver.maxConcurrentRequests ||
    (parsed.finalityObserver.mode === "aggregate-window" &&
      (parsed.finalityObserver.maxConcurrentRequests !== 0 ||
        parsed.finalityObserver.maxObservedConcurrentRequests !== 0 ||
        parsed.finalityObserver.observedTransactionCount !==
          parsed.transactions.length ||
        parsed.finalityObserver.pollRequestCount !== 0)) ||
    (parsed.finalityObserver.mode === "post-submit-bounded" &&
      (parsed.finalityObserver.observedTransactionCount >
        parsed.acceptedCount ||
        parsed.finalityObserver.errorCount >
          parsed.finalityObserver.pollRequestCount ||
        parsed.finalityObserver.batchCount >
          parsed.finalityObserver.pollRequestCount))
  ) {
    throw new Error(
      `${label} status, policy, chronology, counts, identities, or observer evidence is inconsistent`,
    );
  }
  for (const [position, transaction] of parsed.transactions.entries()) {
    const transactionLabel = `${label}.transactions[${position.toString()}]`;
    const submitted = transaction.submission.status === "submitted";
    const submissionHasCanonicalFields =
      (submitted &&
        transaction.txHash !== null &&
        transaction.submission.submittedAt !== null &&
        transaction.submission.durationMs !== undefined &&
        transaction.submission.error === undefined &&
        transaction.selectedInputs.length > 0) ||
      (!submitted &&
        transaction.txHash === null &&
        transaction.submission.submittedAt === null &&
        transaction.submission.durationMs === undefined &&
        transaction.submission.error !== undefined &&
        transaction.selectedInputs.length === 0);
    const acceptanceHasCanonicalFields =
      (transaction.acceptance.status === "accepted" &&
        transaction.acceptance.error === undefined) ||
      (transaction.acceptance.status === "rejected" &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined) ||
      (transaction.acceptance.status === "timeout" &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined &&
        transaction.acceptance.error !== undefined) ||
      (transaction.acceptance.status === "not_observed" &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined) ||
      (transaction.acceptance.status === "not_submitted" &&
        !submitted &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined &&
        transaction.acceptance.error !== undefined);
    const finalityHasCanonicalFields =
      (transaction.finality.status === "committed" &&
        transaction.acceptance.status === "accepted" &&
        transaction.finality.committedAt !== undefined &&
        transaction.finality.durationMs !== undefined &&
        transaction.finality.error === undefined) ||
      (transaction.finality.status === "rejected" &&
        (transaction.acceptance.status === "accepted" ||
          transaction.acceptance.status === "rejected") &&
        transaction.finality.committedAt === undefined &&
        transaction.finality.durationMs === undefined &&
        transaction.finality.error !== undefined) ||
      (transaction.finality.status === "timeout" &&
        transaction.acceptance.status === "accepted" &&
        transaction.finality.committedAt === undefined &&
        transaction.finality.durationMs === undefined &&
        transaction.finality.error !== undefined) ||
      (transaction.finality.status === "not_observed" &&
        transaction.finality.committedAt === undefined &&
        transaction.finality.durationMs === undefined);
    const submittedAtMs =
      transaction.submission.submittedAt === null
        ? null
        : Date.parse(transaction.submission.submittedAt);
    const acceptedAtMs =
      transaction.acceptance.acceptedAt === undefined
        ? null
        : Date.parse(transaction.acceptance.acceptedAt);
    const committedAtMs =
      transaction.finality.committedAt === undefined
        ? null
        : Date.parse(transaction.finality.committedAt);
    if (
      !submissionHasCanonicalFields ||
      !acceptanceHasCanonicalFields ||
      !finalityHasCanonicalFields ||
      (submitted && transaction.acceptance.status === "not_submitted") ||
      (!submitted && transaction.acceptance.status !== "not_submitted") ||
      transaction.workerIndex >= parsed.concurrency ||
      new Set(transaction.selectedInputs).size !==
        transaction.selectedInputs.length ||
      (submittedAtMs !== null &&
        (submittedAtMs < startedAtMs ||
          submittedAtMs > submissionFinishedAtMs)) ||
      (acceptedAtMs !== null &&
        (submittedAtMs === null ||
          acceptedAtMs < submittedAtMs ||
          acceptedAtMs > finishedAtMs ||
          (transaction.acceptance.durationMs !== undefined &&
            transaction.acceptance.durationMs !==
              acceptedAtMs - submittedAtMs))) ||
      (committedAtMs !== null &&
        (submittedAtMs === null ||
          committedAtMs < submittedAtMs ||
          committedAtMs > finishedAtMs ||
          (acceptedAtMs !== null && committedAtMs < acceptedAtMs) ||
          transaction.finality.durationMs !== committedAtMs - submittedAtMs))
    ) {
      throw new Error(
        `${transactionLabel} status, identity, or chronology is inconsistent`,
      );
    }
  }
  const clientSubmission = parsed.metrics.clientSubmission;
  const expectedClientDurationMs = submissionFinishedAtMs - startedAtMs;
  const expectedClientPerSecond =
    parsed.submittedCount === 0 || expectedClientDurationMs <= 0
      ? null
      : roundMetric(parsed.submittedCount / (expectedClientDurationMs / 1_000));
  if (
    clientSubmission.count !== parsed.submittedCount ||
    clientSubmission.missingCount !==
      Math.max(0, parsed.requestedCount - parsed.submittedCount) ||
    (parsed.submittedCount === 0
      ? clientSubmission.status !== "unavailable"
      : clientSubmission.status !==
        (parsed.submittedCount >= parsed.requestedCount
          ? "complete"
          : "partial")) ||
    (parsed.submittedCount === 0
      ? clientSubmission.startedAt !== null ||
        clientSubmission.finishedAt !== null ||
        clientSubmission.durationMs !== null ||
        clientSubmission.perSecond !== null
      : clientSubmission.startedAt !== parsed.startedAt ||
        clientSubmission.finishedAt !== parsed.submissionFinishedAt ||
        clientSubmission.durationMs !== expectedClientDurationMs ||
        clientSubmission.perSecond !== expectedClientPerSecond) ||
    clientSubmission.source !== "stress_artifact.submissions" ||
    clientSubmission.precision !== "artifact_timestamp" ||
    parsed.metrics.durableAdmission.count > parsed.submittedCount ||
    parsed.metrics.l2Admission.count > parsed.submittedCount ||
    parsed.metrics.l1Commit.l2Transactions.count > parsed.submittedCount ||
    parsed.metrics.immutableObservation.count > parsed.submittedCount ||
    parsed.metrics.fullFinality.count > parsed.submittedCount
  ) {
    throw new Error(`${label} metric windows contradict the run summary`);
  }
  if (parsed.openLoop !== undefined) {
    const { corpus, submission, placement } = parsed.openLoop;
    const corpusHashes = corpus.rows.map((row) => row.txHash);
    const corpusInputOutrefs = corpus.rows.map(
      (row) => row.selectedInputOutref,
    );
    if (
      parsed.corpusShape !== corpus.corpusShape ||
      parsed.requestedCount !== corpus.selectedTransactionCount ||
      corpus.rows.length !== corpus.selectedTransactionCount ||
      corpus.requiredTransactionCount < corpus.selectedTransactionCount ||
      corpus.rows.some(
        (row) =>
          row.corpusSliceId !== corpus.corpusSliceId ||
          (corpus.corpusShape !== "mixed" &&
            row.planShape !== corpus.corpusShape),
      ) ||
      new Set(corpusHashes).size !== corpusHashes.length ||
      new Set(corpusInputOutrefs).size !== corpusInputOutrefs.length ||
      transactionHashes.some((hash) => !corpusHashes.includes(hash)) ||
      parsed.transactions.length !== submission.offeredCount ||
      parsed.submittedCount !== submission.submittedCount ||
      parsed.submissionFailedCount !== submission.failedCount ||
      submission.offeredCount !==
        submission.submittedCount + submission.failedCount ||
      submission.maxObservedInFlight > submission.maxInFlight ||
      submission.maxInFlight !== parsed.openLoop.maxInFlight ||
      submission.targetRateTps !== parsed.openLoop.targetRateTps ||
      submission.durationMs !==
        Date.parse(submission.finishedAtIso) -
          Date.parse(submission.startedAtIso) ||
      submission.scheduleSlipMs.p50 > submission.scheduleSlipMs.p95 ||
      submission.scheduleSlipMs.p95 > submission.scheduleSlipMs.p99 ||
      submission.scheduleSlipMs.p99 > submission.scheduleSlipMs.max ||
      placement.validForUpperBoundClaim !==
        (!placement.insideMidgardNodeProcess &&
          !placement.insideMidgardNodeContainer)
    ) {
      throw new Error(`${label}.openLoop evidence is internally inconsistent`);
    }
  }
  return parsed;
}
