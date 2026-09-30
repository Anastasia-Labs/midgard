import {
  arrayOf,
  booleanValue,
  exactLiteral,
  exactRecord,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  oneOf,
  positiveInteger,
  stringArray,
} from "midgard-node/artifact-schema";

import { parseLowerHex64 } from "./summary-artifact.assert-environment-fingerprint.js";

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

export const assertOpenLoopSummary = (value: unknown, label: string): void => {
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

export const assertStressTransaction = (
  value: unknown,
  label: string,
): void => {
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
