import {
  type CleanRunGate,
  type DbEvidence,
  type RawEvidenceRef,
  type TransactionEvidence,
} from "../e2e/summary.js";
import {
  fullFinalityDrainRequested,
  stressMetricDetails,
  stressTxPhaseToEvidenceStatus,
} from "./e2e-finalize-summary.collector-step.js";
import { type E2EL2StressSummary } from "./e2e-stress-l2-throughput/index.js";

export const stressEvidenceFromSummary = ({
  stressSummary,
  stressSummaryPath,
}: {
  readonly stressSummary?: E2EL2StressSummary;
  readonly stressSummaryPath?: string;
}): {
  readonly acceptedStressCount: number;
  readonly db: readonly DbEvidence[];
  readonly cleanRunGates: readonly CleanRunGate[];
  readonly transactions: readonly TransactionEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
} => {
  if (stressSummary === undefined) {
    return {
      acceptedStressCount: 0,
      db: [],
      cleanRunGates: [],
      transactions: [],
      rawEvidence: [],
      notes: [],
    };
  }

  const stressTransactions = stressSummary.transactions.filter(
    (tx) => tx.phase === "stress",
  );
  const unresolvedCount = stressTransactions.filter(
    (tx) =>
      tx.txHash !== null &&
      tx.submission.status === "submitted" &&
      tx.acceptance.status !== "accepted" &&
      tx.acceptance.status !== "rejected",
  ).length;
  const acceptanceRejectedCount = stressTransactions.filter(
    (tx) => tx.acceptance.status === "rejected",
  ).length;
  const l2Admission = stressSummary.metrics.l2Admission;
  const fullFinality = stressSummary.metrics.fullFinality;
  const failedAcceptanceCount =
    acceptanceRejectedCount +
    stressSummary.submissionFailedCount +
    stressSummary.acceptanceTimedOutCount;
  const allRequestedAccepted =
    stressSummary.requestedCount > 0 &&
    l2Admission.count >= stressSummary.requestedCount;
  const stressGateSatisfied =
    allRequestedAccepted &&
    failedAcceptanceCount === 0 &&
    unresolvedCount === 0;
  const cleanRunStatus: CleanRunGate["status"] =
    unresolvedCount > 0
      ? "blocked"
      : failedAcceptanceCount > 0 || !allRequestedAccepted
        ? "failed"
        : "satisfied";
  const summarySource = stressSummaryPath ?? "e2e-stress-l2-throughput";

  return {
    acceptedStressCount: l2Admission.count,
    db: [
      {
        label: "stress_l2_acceptance",
        status: stressGateSatisfied ? "satisfied" : "failed",
        source: "e2e-stress-l2-throughput",
        details: {
          requested: stressSummary.requestedCount.toString(),
          submitted: stressSummary.submittedCount.toString(),
          submissionFailed: stressSummary.submissionFailedCount.toString(),
          accepted: l2Admission.count.toString(),
          artifactAccepted: stressSummary.acceptedCount.toString(),
          acceptanceTimedOut: stressSummary.acceptanceTimedOutCount.toString(),
          acceptanceRejected: acceptanceRejectedCount.toString(),
          observedCommitted: stressSummary.observedCommittedCount.toString(),
          finalityTimedOut: stressSummary.finalityTimedOutCount.toString(),
          rejected: stressSummary.rejectedCount.toString(),
          unresolved: unresolvedCount.toString(),
          rateSemantics: stressSummary.rateSemantics,
          burstCycleRatePerSecond:
            stressSummary.burstCycleRatePerSecond?.toString() ?? "",
          interruptedReason: stressSummary.interruptedReason ?? "",
          ...stressMetricDetails(
            stressSummary.metrics.clientSubmission,
            "clientSubmission",
          ),
          ...stressMetricDetails(l2Admission, "l2Admission"),
          ...stressMetricDetails(
            stressSummary.metrics.immutableObservation,
            "immutableObservation",
          ),
          ...stressMetricDetails(fullFinality, "fullFinality"),
          advanceOn: stressSummary.measurementPolicy.advanceOn,
          primaryStageMetric:
            stressSummary.measurementPolicy.primaryStageMetric,
          finalityObservation:
            stressSummary.measurementPolicy.finalityObservation,
          fullFinalityRequiresDrainProof:
            stressSummary.measurementPolicy.fullFinalityRequiresDrainProof.toString(),
          submissionWindowExcludesCommitDrain:
            stressSummary.measurementPolicy.submissionWindowExcludesCommitDrain.toString(),
          summaryPath: stressSummaryPath ?? "",
        },
      },
      ...(fullFinalityDrainRequested(fullFinality)
        ? [
            {
              label: "stress_l2_full_finality",
              status:
                fullFinality.status === "complete"
                  ? ("satisfied" as const)
                  : ("failed" as const),
              source: "e2e-stress-l2-throughput",
              details: {
                requested: stressSummary.requestedCount.toString(),
                accepted: l2Admission.count.toString(),
                ...stressMetricDetails(fullFinality, "fullFinality"),
              },
            },
          ]
        : []),
    ],
    cleanRunGates: [
      {
        label: "stress_l2_acceptance_clean_run",
        status: cleanRunStatus,
        source: "e2e-stress-l2-throughput",
        details: {
          requested: stressSummary.requestedCount.toString(),
          accepted: l2Admission.count.toString(),
          artifactAccepted: stressSummary.acceptedCount.toString(),
          submissionFailed: stressSummary.submissionFailedCount.toString(),
          acceptanceTimedOut: stressSummary.acceptanceTimedOutCount.toString(),
          acceptanceRejected: acceptanceRejectedCount.toString(),
          observedCommitted: stressSummary.observedCommittedCount.toString(),
          finalityTimedOut: stressSummary.finalityTimedOutCount.toString(),
          rejected: stressSummary.rejectedCount.toString(),
          unresolved: unresolvedCount.toString(),
          interruptedReason: stressSummary.interruptedReason ?? "",
        },
      },
    ],
    transactions: stressTransactions.map((tx) => ({
      label: `stress-l2-${tx.index.toString()}`,
      txHash: tx.txHash ?? `unknown-stress-l2-${tx.index.toString()}`,
      status: stressTxPhaseToEvidenceStatus(tx),
      source: summarySource,
    })),
    rawEvidence:
      stressSummaryPath === undefined
        ? []
        : [
            { label: "stress-summary", path: stressSummaryPath },
            {
              label: "stress-config",
              path: stressSummary.artifactPaths.configJson,
            },
            {
              label: "stress-events",
              path: stressSummary.artifactPaths.eventsNdjson,
            },
            {
              label: "stress-summary-md",
              path: stressSummary.artifactPaths.summaryMarkdown,
            },
          ],
    notes: [
      `stress_l2_acceptance accepted=${l2Admission.count.toString()}/${stressSummary.requestedCount.toString()} l2AdmissionStatus=${l2Admission.status} l2AdmissionRatePerSecond=${l2Admission.perSecond?.toString() ?? "unavailable"} rateSemantics=${stressSummary.rateSemantics} immutableObservation=${stressSummary.metrics.immutableObservation.count.toString()}/${l2Admission.count.toString()} fullFinality=${fullFinality.status}`,
    ],
  };
};
