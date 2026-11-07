/**
 * What `POST /submit` records: admitted transactions, one latency histogram
 * and refusal counts by reason, each updated synchronously with no Effect
 * run, and refusal logs sampled to the first per reason per second.
 */
import { Duration, Metric, MetricLabel } from "effect";

export const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

export const submitHandlerLatencyTimer = Metric.timer(
  "submit_handler_latency",
  "Latency of POST /submit handler responses in milliseconds",
);

export const submitRefusalCounter = Metric.counter("submit_refusal_count", {
  description: "POST /submit requests refused, by reason",
  incremental: true,
});

/** One constant label list per tag, so the metric's hook cache always hits. */
const NO_LABELS: readonly MetricLabel.MetricLabel[] = [];
const reasonLabels = new Map<string, readonly MetricLabel.MetricLabel[]>();
const labelsOf = (reason: string) => {
  let labels = reasonLabels.get(reason);
  if (labels === undefined) {
    labels = [MetricLabel.make("reason", reason)];
    reasonLabels.set(reason, labels);
  }
  return labels;
};

export const recordSubmitAdmitted = (): void =>
  txCounter.unsafeUpdate(1n, NO_LABELS);

export const recordSubmitLatency = (milliseconds: number): void =>
  submitHandlerLatencyTimer.unsafeUpdate(
    Duration.millis(milliseconds),
    NO_LABELS,
  );

export const recordSubmitRefusal = (reason: string): void =>
  submitRefusalCounter.unsafeUpdate(1, labelsOf(reason));

/**
 * Logs the first refusal of each reason in every wall-clock second and
 * counts the rest, so a flood of junk costs a counter increment per request
 * and a few log lines per second. The next logged line of a reason says how
 * many of its refusals went unlogged since the previous one.
 */
export const createRefusalLogSampler = (
  log: (line: string) => void,
  now: () => number = Date.now,
) => {
  const seen = new Map<string, { second: number; unlogged: number }>();
  return (reason: string, detail: () => string): void => {
    const second = Math.floor(now() / 1_000);
    const entry = seen.get(reason);
    if (entry === undefined) seen.set(reason, { second, unlogged: 0 });
    else if (entry.second === second) {
      entry.unlogged += 1;
      return;
    }
    const unlogged = entry?.unlogged ?? 0;
    if (entry !== undefined) {
      entry.second = second;
      entry.unlogged = 0;
    }
    log(
      `▫️ Submit refused (${reason}): ${detail()}${
        unlogged > 0
          ? ` (${unlogged.toString()} more ${reason} refusals unlogged since the previous line)`
          : ""
      }`,
    );
  };
};
