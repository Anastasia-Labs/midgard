import { flattenStressMetricRows } from "../stress-stage-metrics.js";
import { type E2EL2StressSummary } from "./types.js";

const formatMetricValue = (value: number | null): string =>
  value === null ? "-" : value.toString();

const formatMetricNotes = (notes: readonly string[]): string =>
  notes.length === 0 ? "-" : notes.join(",");

export const renderStressSummaryMarkdown = (
  summary: E2EL2StressSummary,
): string => {
  const metricRows = flattenStressMetricRows(summary.metrics);
  const lines = [
    "# Midgard L2 Stress Summary",
    "",
    `- runId: ${summary.runId}`,
    `- status: ${summary.status}`,
    ...(summary.interruptedReason === undefined
      ? []
      : [`- interruptedReason: ${summary.interruptedReason}`]),
    `- loadModel: ${summary.loadModel}`,
    `- workloadProfile: ${summary.workloadProfile}`,
    `- classification: ${summary.classification}`,
    `- rateSemantics: ${summary.rateSemantics}`,
    ...(summary.rateSemantics !== "burst_cycle_rate"
      ? []
      : [
          `- burstCycleRatePerSecond: ${summary.burstCycleRatePerSecond?.toString() ?? "n/a"} (bounded by concurrency=${summary.concurrency.toString()}; NOT a production throughput measurement)`,
        ]),
    `- mode: ${summary.mode}`,
    ...(summary.corpusShape === undefined
      ? []
      : [`- corpusShape: ${summary.corpusShape}`]),
    `- advanceOn: ${summary.measurementPolicy.advanceOn}`,
    `- primaryStageMetric: ${summary.measurementPolicy.primaryStageMetric}`,
    `- finalityObservation: ${summary.measurementPolicy.finalityObservation}`,
    `- fullFinalityRequiresDrainProof: ${summary.measurementPolicy.fullFinalityRequiresDrainProof.toString()}`,
    `- requestedCount: ${summary.requestedCount.toString()}`,
    `- notStartedCount: ${summary.notStartedCount.toString()}`,
    `- submittedCount: ${summary.submittedCount.toString()}`,
    `- submissionFailedCount: ${summary.submissionFailedCount.toString()}`,
    `- acceptedCount: ${summary.acceptedCount.toString()}`,
    `- acceptanceNotObservedCount: ${summary.acceptanceNotObservedCount.toString()}`,
    `- acceptanceTimedOutCount: ${summary.acceptanceTimedOutCount.toString()}`,
    `- finalityTimedOutCount: ${summary.finalityTimedOutCount.toString()}`,
    `- observedCommittedCount: ${summary.observedCommittedCount.toString()}`,
    `- unknownFinalityCount: ${summary.unknownFinalityCount.toString()}`,
    `- rejectedCount: ${summary.rejectedCount.toString()}`,
    `- concurrency: ${summary.concurrency.toString()}`,
    `- finalityObserverMaxConcurrentRequests: ${summary.finalityObserver.maxConcurrentRequests.toString()}`,
    `- finalityObserverMaxObservedConcurrentRequests: ${summary.finalityObserver.maxObservedConcurrentRequests.toString()}`,
    `- finalityObserverPollRequestCount: ${summary.finalityObserver.pollRequestCount.toString()}`,
    "",
    "## Stage Metrics",
    "",
    "| metric | status | count | missing | duration_s | rate_per_s* | precision | source | notes |",
    "| --- | --- | ---: | ---: | ---: | ---: | --- | --- | --- |",
    ...metricRows.map(([label, metric]) => {
      const durationSeconds =
        metric.durationMs === null ? null : metric.durationMs / 1000;
      return `| ${label} | ${metric.status} | ${metric.count.toString()} | ${metric.missingCount.toString()} | ${formatMetricValue(durationSeconds)} | ${formatMetricValue(metric.perSecond)} | ${metric.precision} | ${metric.source} | ${formatMetricNotes(metric.notes)} |`;
    }),
    "",
    "* rate_per_s is a raw count/duration ratio; read it together with rateSemantics above. It is not production throughput when rateSemantics=burst_cycle_rate.",
    "",
    "## Transactions",
    "",
    "| index | submission | acceptance | finality | txHash | sender | destination | selectedInputs |",
    "| ---: | --- | --- | --- | --- | --- | --- | --- |",
    ...summary.transactions.map(
      (tx) =>
        `| ${tx.index.toString()} | ${tx.submission.status} | ${tx.acceptance.status} | ${tx.finality.status} | ${tx.txHash ?? "-"} | ${tx.senderAddress} | ${tx.destinationAddress} | ${tx.selectedInputs.join(",") || "-"} |`,
    ),
    "",
  ];
  return lines.join("\n");
};
