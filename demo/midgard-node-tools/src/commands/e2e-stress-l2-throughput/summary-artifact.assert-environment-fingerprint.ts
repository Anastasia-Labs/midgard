import {
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

export const parseLowerHex64 = (value: unknown, label: string): string => {
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

export const assertStressMetrics = (value: unknown, label: string): void => {
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

export const assertEnvironmentFingerprint = (
  value: unknown,
  label: string,
): void => {
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

export const assertGroundTruthMetrics = (
  value: unknown,
  label: string,
): void => {
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
