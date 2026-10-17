import { isDeepStrictEqual } from "node:util";

import {
  arrayOf,
  exactRecord,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  nullableNonNegativeNumber,
  oneOf,
  positiveInteger,
} from "midgard-node/artifact-schema";

import { roundMetric } from "../stress-stage-metrics.js";
import { assertMeasurementPolicy } from "./config-artifact.js";
import { E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION } from "./constants.js";
import { measurementPolicyForConfig } from "./policy.js";
import {
  assertEnvironmentFingerprint,
  assertGroundTruthMetrics,
  assertStressMetrics,
} from "./summary-artifact.assert-environment-fingerprint.js";
import {
  assertOpenLoopSummary,
  assertStressTransaction,
} from "./summary-artifact.assert-stress-transaction.js";
import { type E2EL2StressSummary } from "./types.js";

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
