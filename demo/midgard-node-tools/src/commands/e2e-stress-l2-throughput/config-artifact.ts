import {
  arrayOf,
  booleanValue,
  exactLiteral,
  exactRecord,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  nullableNonEmptyString,
  oneOf,
  positiveInteger,
} from "midgard-node/artifact-schema";

import { type E2EL2StressConfigArtifact } from "./config.js";
import { E2E_L2_STRESS_CONFIG_SCHEMA_VERSION } from "./constants.js";

export const assertMeasurementPolicy = (
  value: unknown,
  label: string,
): void => {
  const input = exactRecord(value, label, [
    "loadModel",
    "workloadProfile",
    "syntheticVsProduction",
    "advanceOn",
    "primaryStageMetric",
    "finalityObservation",
    "submissionWindowExcludesCommitDrain",
    "fullFinalityRequiresDrainProof",
  ]);
  oneOf(input.loadModel, `${label}.loadModel`, [
    "closed-loop-smoke",
    "open-loop-upper-bound",
  ]);
  oneOf(input.workloadProfile, `${label}.workloadProfile`, [
    "synthetic-admission",
    "production-end-user",
  ]);
  oneOf(input.syntheticVsProduction, `${label}.syntheticVsProduction`, [
    "synthetic_admission_diagnostic",
    "production_end_user_path",
  ]);
  oneOf(input.advanceOn, `${label}.advanceOn`, [
    "accepted",
    "scheduled_submit",
  ]);
  oneOf(input.primaryStageMetric, `${label}.primaryStageMetric`, [
    "metrics.l2Admission.perSecond",
    "metrics.durableAdmission.perSecond",
  ]);
  oneOf(input.finalityObservation, `${label}.finalityObservation`, [
    "post-submit-bounded",
    "aggregate-window",
  ]);
  exactLiteral(
    input.submissionWindowExcludesCommitDrain,
    `${label}.submissionWindowExcludesCommitDrain`,
    true,
  );
  exactLiteral(
    input.fullFinalityRequiresDrainProof,
    `${label}.fullFinalityRequiresDrainProof`,
    true,
  );
};

const parseDecimalString = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(parsed)) {
    throw new Error(`${label} must be an unsigned canonical decimal string`);
  }
  return parsed;
};

const assertStressWalletArtifact = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, ["seedSource", "address"]);
  nonEmptyString(input.seedSource, `${label}.seedSource`);
  nonEmptyString(input.address, `${label}.address`);
};

export function parseE2EL2StressConfigArtifact(
  value: unknown,
): E2EL2StressConfigArtifact {
  const label = "E2E L2 stress config";
  const input = exactRecord(
    value,
    label,
    [
      "schemaVersion",
      "runId",
      "loadModel",
      "workloadProfile",
      "corpusShape",
      "mode",
      "measurementPolicy",
      "count",
      "concurrency",
      "lovelace",
      "feeHeadroomLovelace",
      "nodeEndpoint",
      "corpusPath",
      "corpusSliceId",
      "targetRateTps",
      "openLoopDurationMs",
      "openLoopWarmupCount",
      "openLoopCooldownCount",
      "openLoopMaxInFlight",
      "noOpCalibrationEndpoint",
      "requireNoOpCalibration",
      "noOpCalibrationDurationMs",
      "aggregateObserverIntervalMs",
      "destination",
      "pollInitialIntervalMs",
      "pollMaxIntervalMs",
      "submitRequestTimeoutMs",
      "acceptanceTimeoutMs",
      "commitObservationTimeoutMs",
      "finalityObserverMaxConcurrentRequests",
      "maxSubmissionFailures",
      "network",
      "allowUnsafeBounds",
      "wallets",
    ],
    ["pollIntervalMs"],
  );
  if (input.schemaVersion !== E2E_L2_STRESS_CONFIG_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_L2_STRESS_CONFIG_SCHEMA_VERSION}`,
    );
  }
  nonEmptyString(input.runId, `${label}.runId`);
  oneOf(input.loadModel, `${label}.loadModel`, [
    "closed-loop-smoke",
    "open-loop-upper-bound",
  ]);
  oneOf(input.workloadProfile, `${label}.workloadProfile`, [
    "synthetic-admission",
    "production-end-user",
  ]);
  oneOf(input.corpusShape, `${label}.corpusShape`, [
    "fanout",
    "chain",
    "mixed",
  ]);
  oneOf(input.mode, `${label}.mode`, ["serial-chain", "parallel-fanout"]);
  assertMeasurementPolicy(
    input.measurementPolicy,
    `${label}.measurementPolicy`,
  );
  positiveInteger(input.count, `${label}.count`);
  positiveInteger(input.concurrency, `${label}.concurrency`);
  parseDecimalString(input.lovelace, `${label}.lovelace`);
  parseDecimalString(input.feeHeadroomLovelace, `${label}.feeHeadroomLovelace`);
  nonEmptyString(input.nodeEndpoint, `${label}.nodeEndpoint`);
  nullableNonEmptyString(input.corpusPath, `${label}.corpusPath`);
  nonEmptyString(input.corpusSliceId, `${label}.corpusSliceId`);
  nonNegativeNumber(input.targetRateTps, `${label}.targetRateTps`);
  positiveInteger(input.openLoopDurationMs, `${label}.openLoopDurationMs`);
  nonNegativeInteger(input.openLoopWarmupCount, `${label}.openLoopWarmupCount`);
  nonNegativeInteger(
    input.openLoopCooldownCount,
    `${label}.openLoopCooldownCount`,
  );
  positiveInteger(input.openLoopMaxInFlight, `${label}.openLoopMaxInFlight`);
  nullableNonEmptyString(
    input.noOpCalibrationEndpoint,
    `${label}.noOpCalibrationEndpoint`,
  );
  booleanValue(input.requireNoOpCalibration, `${label}.requireNoOpCalibration`);
  positiveInteger(
    input.noOpCalibrationDurationMs,
    `${label}.noOpCalibrationDurationMs`,
  );
  positiveInteger(
    input.aggregateObserverIntervalMs,
    `${label}.aggregateObserverIntervalMs`,
  );
  const destination = exactRecord(
    input.destination,
    `${label}.destination`,
    ["mode"],
    ["address"],
  );
  const destinationMode = oneOf(destination.mode, `${label}.destination.mode`, [
    "self",
    "explicit",
  ]);
  if (destinationMode === "self") {
    exactRecord(input.destination, `${label}.destination`, ["mode"]);
  } else {
    const explicitDestination = exactRecord(
      input.destination,
      `${label}.destination`,
      ["mode", "address"],
    );
    nonEmptyString(explicitDestination.address, `${label}.destination.address`);
  }
  if (input.pollIntervalMs !== undefined) {
    positiveInteger(input.pollIntervalMs, `${label}.pollIntervalMs`);
  }
  positiveInteger(
    input.pollInitialIntervalMs,
    `${label}.pollInitialIntervalMs`,
  );
  positiveInteger(input.pollMaxIntervalMs, `${label}.pollMaxIntervalMs`);
  positiveInteger(
    input.submitRequestTimeoutMs,
    `${label}.submitRequestTimeoutMs`,
  );
  positiveInteger(input.acceptanceTimeoutMs, `${label}.acceptanceTimeoutMs`);
  positiveInteger(
    input.commitObservationTimeoutMs,
    `${label}.commitObservationTimeoutMs`,
  );
  positiveInteger(
    input.finalityObserverMaxConcurrentRequests,
    `${label}.finalityObserverMaxConcurrentRequests`,
  );
  nonNegativeInteger(
    input.maxSubmissionFailures,
    `${label}.maxSubmissionFailures`,
  );
  oneOf(input.network, `${label}.network`, [
    "Mainnet",
    "Preview",
    "Preprod",
    "Custom",
  ]);
  booleanValue(input.allowUnsafeBounds, `${label}.allowUnsafeBounds`);
  arrayOf(input.wallets, `${label}.wallets`, (entry, entryLabel) => {
    assertStressWalletArtifact(entry, entryLabel);
    return entry;
  });
  return value as E2EL2StressConfigArtifact;
}
