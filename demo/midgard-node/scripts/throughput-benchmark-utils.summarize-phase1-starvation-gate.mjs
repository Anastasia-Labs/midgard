import { summarizeLatency } from "./throughput-benchmark-utils.summarize-latency.mjs";
import { interpolateCounterEvents } from "./throughput-benchmark-utils.summarize-phase1-stage-awindow-gate.mjs";

/**
 * Builds the fail-closed Phase 1 oldest-transaction starvation proof from the
 * same Prometheus samples used by the throughput report. Counter events are
 * linearly interpolated only when more than one successful commit lands
 * between polls; the interpolation method is retained in the report.
 */
export const summarizePhase1StarvationGate = ({
  samples,
  stageStartedAtMs,
  stageEndedAtMs,
  targetRateTps,
  overloadBaselineTps,
  commitTxDelta,
  commitBlockDelta,
  maxAgeMultiplier = 3,
  minOverloadRatio = 2,
  minDurationSec = 600,
}) => {
  const measuredSamples = samples.filter(
    (sample) =>
      sample.timestampMs >= stageStartedAtMs &&
      sample.timestampMs <= stageEndedAtMs,
  );
  const reasons = [];
  const durationSec = Math.max(0, stageEndedAtMs - stageStartedAtMs) / 1_000;
  if (durationSec < minDurationSec) {
    reasons.push(
      `measured_duration_sec ${durationSec.toFixed(3)} < ${minDurationSec}`,
    );
  }

  const missingOldestAgeSamples = measuredSamples.filter(
    (sample) =>
      sample.counters?.metricNames?.mempoolOldestTxAgeMs === null ||
      !Number.isFinite(Number(sample.counters?.mempoolOldestTxAgeMs)),
  ).length;
  if (measuredSamples.length < 2) {
    reasons.push(
      "mempool_oldest_tx_age_ms has fewer than two measured samples",
    );
  }
  if (missingOldestAgeSamples > 0) {
    reasons.push(
      `mempool_oldest_tx_age_ms missing_samples=${missingOldestAgeSamples}`,
    );
  }

  const oldestAgeValues = measuredSamples
    .map((sample) => Number(sample.counters?.mempoolOldestTxAgeMs))
    .filter(Number.isFinite);
  const oldestTxAgeMs = summarizeLatency(oldestAgeValues);
  const commitEventTimestampsMs = interpolateCounterEvents(
    measuredSamples,
    "commitBlock",
  );
  const commitIntervalsMs = commitEventTimestampsMs
    .slice(1)
    .map((timestampMs, index) => timestampMs - commitEventTimestampsMs[index]);
  const successfulCommitIntervalMs = summarizeLatency(commitIntervalsMs);
  if (successfulCommitIntervalMs.p95 === null) {
    reasons.push(
      "successful_commit_interval_p95_ms missing (fewer than two successful commit events)",
    );
  }

  const maxAllowedOldestTxAgeMs =
    successfulCommitIntervalMs.p95 === null
      ? null
      : successfulCommitIntervalMs.p95 * maxAgeMultiplier;
  if (oldestTxAgeMs.max === null) {
    reasons.push("mempool_oldest_tx_age_ms max missing");
  } else if (
    maxAllowedOldestTxAgeMs !== null &&
    oldestTxAgeMs.max > maxAllowedOldestTxAgeMs
  ) {
    reasons.push(
      `mempool_oldest_tx_age_ms max ${oldestTxAgeMs.max.toFixed(3)} > ${maxAllowedOldestTxAgeMs.toFixed(3)}`,
    );
  }

  const observedDecrease = oldestAgeValues.some(
    (value, index) => index > 0 && value < oldestAgeValues[index - 1],
  );
  const allZero = oldestAgeValues.every((value) => value === 0);
  if (oldestAgeValues.length > 0 && !allZero && !observedDecrease) {
    reasons.push(
      "mempool_oldest_tx_age_ms did not decrease during the measured overload window",
    );
  }

  const meanCommittedTxPerBlock =
    commitBlockDelta > 0 ? commitTxDelta / commitBlockDelta : null;
  const observedCommitCapacityTps =
    meanCommittedTxPerBlock !== null &&
    successfulCommitIntervalMs.p95 !== null &&
    successfulCommitIntervalMs.p95 > 0
      ? meanCommittedTxPerBlock / (successfulCommitIntervalMs.p95 / 1_000)
      : null;
  const observedOverloadRatio =
    Number.isFinite(overloadBaselineTps) && overloadBaselineTps > 0
      ? targetRateTps / overloadBaselineTps
      : null;
  if (observedOverloadRatio === null) {
    reasons.push("observed_overload_ratio missing");
  } else if (observedOverloadRatio < minOverloadRatio) {
    reasons.push(
      `observed_overload_ratio ${observedOverloadRatio.toFixed(4)} < ${minOverloadRatio}`,
    );
  }

  return {
    enabled: true,
    passed: reasons.length === 0,
    reasons,
    measuredDurationSec: durationSec,
    minDurationSec,
    measuredSampleCount: measuredSamples.length,
    missingOldestAgeSamples,
    oldestTxAgeMs,
    observedDecrease,
    allZero,
    successfulCommitEventCount: commitEventTimestampsMs.length,
    successfulCommitIntervalMs,
    commitEventTimestampMethod:
      "prometheus_counter_transition_linear_interpolation_within_poll_window",
    maxAgeMultiplier,
    maxAllowedOldestTxAgeMs,
    commitTxDelta,
    commitBlockDelta,
    meanCommittedTxPerBlock,
    targetRateTps,
    overloadBaselineTps,
    observedCommitCapacityTps,
    observedOverloadRatio,
    minOverloadRatio,
  };
};

export const summarizeL1Observation = (samples) => {
  const points = samples
    .map((sample) => ({
      timestampMs: Number(sample.timestampMs),
      tipSlot: Number(sample.counters?.l1TipSlot),
    }))
    .filter(
      (point) =>
        Number.isFinite(point.timestampMs) && Number.isFinite(point.tipSlot),
    );
  const tipChanges = [];
  for (const point of points) {
    const previous = tipChanges[tipChanges.length - 1];
    if (previous === undefined || previous.tipSlot !== point.tipSlot) {
      tipChanges.push(point);
    }
  }
  const interBlockTimeMs = summarizeLatency(
    tipChanges
      .slice(1)
      .map((point, index) => point.timestampMs - tipChanges[index].timestampMs),
  );
  return {
    source: "node_readyz.localLedgerSlot.currentSlot",
    sampleCount: points.length,
    startTipSlot: points[0]?.tipSlot ?? null,
    endTipSlot: points[points.length - 1]?.tipSlot ?? null,
    observedPreprodBlockCount: Math.max(0, tipChanges.length - 1),
    interBlockTimeMs,
  };
};

const histogramBucketUpperBound = (value) =>
  value === "+Inf" || value === null ? Number.POSITIVE_INFINITY : Number(value);

export const summarizeHistogramDelta = (start, end) => {
  const startBuckets = new Map(
    (start?.buckets ?? []).map((bucket) => [bucket.le, Number(bucket.value)]),
  );
  const buckets = (end?.buckets ?? [])
    .map((bucket) => ({
      le: bucket.le,
      value: Math.max(
        0,
        Number(bucket.value) - Number(startBuckets.get(bucket.le) ?? 0),
      ),
    }))
    .sort(
      (left, right) =>
        histogramBucketUpperBound(left.le) -
        histogramBucketUpperBound(right.le),
    );
  const count = Math.max(
    0,
    Number(end?.count ?? 0) - Number(start?.count ?? 0),
  );
  const sum = Math.max(0, Number(end?.sum ?? 0) - Number(start?.sum ?? 0));
  const percentile = (fraction) => {
    if (count <= 0) return null;
    const target = Math.ceil(count * fraction);
    const bucket = buckets.find((entry) => entry.value >= target);
    const upperBound = histogramBucketUpperBound(bucket?.le ?? null);
    return Number.isFinite(upperBound) ? upperBound : null;
  };
  return {
    count,
    sum,
    mean: count > 0 ? sum / count : null,
    p50: percentile(0.5),
    p95: percentile(0.95),
    p99: percentile(0.99),
    buckets,
  };
};
