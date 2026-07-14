export const BENCHMARK_WINDOWS_MS = [1_000, 5_000, 30_000];

export const terminalStatuses = new Set([
  "accepted",
  "pending_commit",
  "awaiting_local_recovery",
  "committed",
  "rejected",
]);

export const acceptedStatuses = new Set([
  "accepted",
  "pending_commit",
  "awaiting_local_recovery",
  "committed",
]);

const quantileFromSorted = (sorted, q) => {
  if (sorted.length === 0) {
    return null;
  }
  const index = Math.min(
    sorted.length - 1,
    Math.max(0, Math.ceil(q * sorted.length) - 1),
  );
  return sorted[index];
};

export const quantile = (values, q) =>
  quantileFromSorted(
    [...values].sort((a, b) => a - b),
    q,
  );

export const summarizeLatency = (values) => {
  if (values.length === 0) {
    return {
      count: 0,
      min: null,
      p50: null,
      p95: null,
      p99: null,
      max: null,
      mean: null,
    };
  }
  let total = 0;
  let min = Number.POSITIVE_INFINITY;
  let max = Number.NEGATIVE_INFINITY;
  for (const value of values) {
    total += value;
    min = Math.min(min, value);
    max = Math.max(max, value);
  }
  const sorted = [...values].sort((a, b) => a - b);
  return {
    count: values.length,
    min,
    p50: quantileFromSorted(sorted, 0.5),
    p95: quantileFromSorted(sorted, 0.95),
    p99: quantileFromSorted(sorted, 0.99),
    max,
    mean: total / values.length,
  };
};

export const deriveCalibratedClientCapacity = ({
  observedMaxInFlight,
  targetRateTps,
  assumedAcceptanceLatencyMs,
  activeChainCount,
  httpPipelining,
}) => {
  const workloadFloor = Math.ceil(
    (targetRateTps * assumedAcceptanceLatencyMs) / 1000,
  );
  if (workloadFloor > activeChainCount) {
    throw new Error(
      `calibrated client capacity requires ${workloadFloor.toString()} in-flight chains but only ${activeChainCount.toString()} are active`,
    );
  }
  const submitConcurrency = Math.max(observedMaxInFlight, workloadFloor);
  return {
    observedMaxInFlight,
    workloadFloor,
    submitConcurrency,
    httpConnections: Math.ceil(submitConcurrency / httpPipelining),
  };
};

export const counterDelta = (startCounters, endCounters, key) =>
  Number(endCounters[key] ?? 0) - Number(startCounters[key] ?? 0);

export const rateBetweenCounters = (
  startCounters,
  endCounters,
  key,
  elapsedMs,
) => {
  if (!Number.isFinite(elapsedMs) || elapsedMs <= 0) {
    return 0;
  }
  return counterDelta(startCounters, endCounters, key) / (elapsedMs / 1000);
};

export const isDrainComplete = ({ submitted, acceptedDelta, rejectedDelta }) =>
  acceptedDelta + rejectedDelta >= submitted;

export const findSampleAtOrBefore = (samples, timestampMs) => {
  let selected = null;
  for (const sample of samples) {
    if (sample.timestampMs <= timestampMs) {
      selected = sample;
    } else {
      break;
    }
  }
  return selected;
};

export const findSampleAtOrAfter = (samples, timestampMs) => {
  for (const sample of samples) {
    if (sample.timestampMs >= timestampMs) {
      return sample;
    }
  }
  return null;
};

export const maxRollingRate = (samples, counterKey, windowMs) => {
  if (samples.length < 2 || windowMs <= 0) {
    return 0;
  }
  let maxRate = 0;
  let startIndex = 0;
  for (let endIndex = 1; endIndex < samples.length; endIndex += 1) {
    const end = samples[endIndex];
    while (
      startIndex + 1 < endIndex &&
      samples[startIndex + 1].timestampMs <= end.timestampMs - windowMs
    ) {
      startIndex += 1;
    }
    const start = samples[startIndex];
    const elapsedMs = end.timestampMs - start.timestampMs;
    if (elapsedMs <= 0) {
      continue;
    }
    const delta =
      Number(end.counters[counterKey] ?? 0) -
      Number(start.counters[counterKey] ?? 0);
    maxRate = Math.max(maxRate, delta / (elapsedMs / 1000));
  }
  return maxRate;
};

export const summarizeRollingRates = (
  samples,
  counterKeys,
  windowsMs = BENCHMARK_WINDOWS_MS,
) => {
  const result = {};
  for (const key of counterKeys) {
    result[key] = {};
    for (const windowMs of windowsMs) {
      result[key][`${Math.round(windowMs / 1000)}s`] = maxRollingRate(
        samples,
        key,
        windowMs,
      );
    }
  }
  return result;
};

export const summarizeCounterWindow = ({
  startCounters,
  endCounters,
  elapsedMs,
  counterKeys,
}) => {
  const result = {};
  for (const key of counterKeys) {
    const delta = counterDelta(startCounters, endCounters, key);
    result[key] = {
      delta,
      ratePerSec: elapsedMs > 0 ? delta / (elapsedMs / 1000) : 0,
    };
  }
  return result;
};

export const summarizeSubmitSuccessStatuses = (statusCounts) => {
  const entries = Object.entries(statusCounts ?? {});
  const durablyAdmitted = Number(statusCounts?.["202"] ?? 0);
  const duplicateSuccesses = Number(statusCounts?.["200"] ?? 0);
  const otherSuccesses = entries.reduce(
    (sum, [status, count]) =>
      status === "200" || status === "202" || !status.startsWith("2")
        ? sum
        : sum + Number(count),
    0,
  );
  const reasons = [];
  if (duplicateSuccesses > 0) {
    reasons.push(`duplicate_successes=${duplicateSuccesses}`);
  }
  if (otherSuccesses > 0) {
    reasons.push(`other_successes=${otherSuccesses}`);
  }
  return {
    passed: reasons.length === 0,
    reasons,
    durablyAdmitted,
    duplicateSuccesses,
    otherSuccesses,
  };
};

/**
 * Evaluates the Phase 1 five-minute Stage-A gate from an observer-only
 * checkpoint taken while a longer open-loop stage continues to consume the
 * same corpus cursors. This deliberately accepts already-summarized latency
 * values so the live checkpoint only records array lengths; sorting millions
 * of samples happens after measured traffic has stopped.
 */
export const summarizePhase1StageAWindowGate = ({
  checkpointAvailable,
  checkpointError = null,
  checkpointRequestedAfterMs,
  checkpointObservedAfterMs,
  checkpointMaxJitterMs = 1_000,
  measuredDurationSec,
  minDurationSec = 300,
  targetRateTps,
  durablyAdmitted,
  acceptedDelta,
  rejectedDelta,
  duplicateSuccesses,
  otherSuccesses,
  submitErrors,
  queueFullResponses,
  submitLatencyMs,
  scheduleLagMs,
  scheduledStarts,
  missedStarts,
  offeredRateMinRatio = 0.98,
  acceptedRateMinRatio = 0.99,
  submitLatencyP99MaxMs = 1_000,
  scheduleLagP95MaxMs = 100,
  scheduleLagP99MaxMs = 250,
  missedStartMaxRatio = 0.001,
  missingRequiredMetrics = [],
  streamContinuity,
}) => {
  const reasons = [];
  if (!checkpointAvailable) {
    reasons.push(
      checkpointError === null
        ? "five-minute checkpoint missing"
        : `five-minute checkpoint failed: ${checkpointError}`,
    );
  }
  const targetWindowMs = minDurationSec * 1_000;
  if (!Number.isFinite(checkpointRequestedAfterMs)) {
    reasons.push("checkpoint_requested_after_ms missing");
  } else if (checkpointRequestedAfterMs < targetWindowMs) {
    reasons.push(
      `checkpoint_requested_after_ms ${checkpointRequestedAfterMs.toFixed(3)} < target ${targetWindowMs.toFixed(3)}`,
    );
  } else if (
    checkpointRequestedAfterMs >
    targetWindowMs + checkpointMaxJitterMs
  ) {
    reasons.push(
      `checkpoint_requested_after_ms ${checkpointRequestedAfterMs.toFixed(3)} > latest ${(
        targetWindowMs + checkpointMaxJitterMs
      ).toFixed(3)}`,
    );
  }
  if (!Number.isFinite(checkpointObservedAfterMs)) {
    reasons.push("checkpoint_observed_after_ms missing");
  } else if (
    checkpointObservedAfterMs < checkpointRequestedAfterMs ||
    checkpointObservedAfterMs > targetWindowMs + checkpointMaxJitterMs
  ) {
    reasons.push(
      `checkpoint_observed_after_ms ${checkpointObservedAfterMs.toFixed(3)} outside request/latest bounds`,
    );
  }
  if (!Number.isFinite(measuredDurationSec)) {
    reasons.push("measured_duration_sec missing");
  } else if (measuredDurationSec < minDurationSec) {
    reasons.push(
      `measured_duration_sec ${measuredDurationSec.toFixed(3)} < ${minDurationSec}`,
    );
  }

  const durationForRate =
    Number.isFinite(measuredDurationSec) && measuredDurationSec > 0
      ? measuredDurationSec
      : null;
  const durablyAdmittedPerSec =
    durationForRate === null ? null : durablyAdmitted / durationForRate;
  const acceptedPerSec =
    durationForRate === null ? null : acceptedDelta / durationForRate;
  if (
    durablyAdmittedPerSec === null ||
    durablyAdmittedPerSec < targetRateTps * offeredRateMinRatio
  ) {
    reasons.push(
      durablyAdmittedPerSec === null
        ? "durably_admitted_per_sec missing"
        : `durably_admitted_per_sec ${durablyAdmittedPerSec.toFixed(2)} < ${(targetRateTps * offeredRateMinRatio).toFixed(2)}`,
    );
  }
  if (
    acceptedPerSec === null ||
    acceptedPerSec < targetRateTps * acceptedRateMinRatio
  ) {
    reasons.push(
      acceptedPerSec === null
        ? "accepted_per_sec missing"
        : `accepted_per_sec ${acceptedPerSec.toFixed(2)} < ${(targetRateTps * acceptedRateMinRatio).toFixed(2)}`,
    );
  }
  if (duplicateSuccesses > 0) {
    reasons.push(`duplicate_successes=${duplicateSuccesses}`);
  }
  if (otherSuccesses > 0) {
    reasons.push(`other_successes=${otherSuccesses}`);
  }
  if (submitErrors > 0) {
    reasons.push(`submit_errors=${submitErrors}`);
  }
  if (queueFullResponses > 0) {
    reasons.push(`queue_full_responses=${queueFullResponses}`);
  }
  if (rejectedDelta > 0) {
    reasons.push(`unexpected_rejections=${rejectedDelta}`);
  }
  if (missingRequiredMetrics.length > 0) {
    reasons.push(
      `missing_required_metrics=${missingRequiredMetrics.join(",")}`,
    );
  }
  if (submitLatencyMs?.p99 === null || submitLatencyMs?.p99 === undefined) {
    reasons.push("submit_latency_p99_ms missing");
  } else if (submitLatencyMs.p99 > submitLatencyP99MaxMs) {
    reasons.push(
      `submit_latency_p99_ms ${submitLatencyMs.p99.toFixed(2)} > ${submitLatencyP99MaxMs}`,
    );
  }
  if (scheduleLagMs?.p95 === null || scheduleLagMs?.p95 === undefined) {
    reasons.push("schedule_lag_p95_ms missing");
  } else if (scheduleLagMs.p95 > scheduleLagP95MaxMs) {
    reasons.push(
      `schedule_lag_p95_ms ${scheduleLagMs.p95.toFixed(2)} > ${scheduleLagP95MaxMs}`,
    );
  }
  if (scheduleLagMs?.p99 === null || scheduleLagMs?.p99 === undefined) {
    reasons.push("schedule_lag_p99_ms missing");
  } else if (scheduleLagMs.p99 > scheduleLagP99MaxMs) {
    reasons.push(
      `schedule_lag_p99_ms ${scheduleLagMs.p99.toFixed(2)} > ${scheduleLagP99MaxMs}`,
    );
  }
  const missedStartRatio =
    scheduledStarts + missedStarts > 0
      ? missedStarts / (scheduledStarts + missedStarts)
      : null;
  if (missedStartRatio === null) {
    reasons.push("missed_start_ratio missing");
  } else if (missedStartRatio > missedStartMaxRatio) {
    reasons.push(
      `missed_start_ratio ${missedStartRatio.toFixed(6)} > ${missedStartMaxRatio}`,
    );
  }
  if (streamContinuity?.passed !== true) {
    reasons.push(
      `stream_continuity_failed: ${streamContinuity?.reason ?? "missing continuity proof"}`,
    );
  }

  return {
    enabled: true,
    passed: reasons.length === 0,
    reasons,
    checkpointAvailable,
    checkpointError,
    checkpointRequestedAfterMs,
    checkpointObservedAfterMs,
    checkpointMaxJitterMs,
    measuredDurationSec,
    minDurationSec,
    targetRateTps,
    durablyAdmitted,
    durablyAdmittedPerSec,
    acceptedDelta,
    acceptedPerSec,
    rejectedDelta,
    duplicateSuccesses,
    otherSuccesses,
    submitErrors,
    queueFullResponses,
    submitLatencyMs,
    submitLatencyP99MaxMs,
    scheduleLagMs,
    scheduleLagP95MaxMs,
    scheduleLagP99MaxMs,
    scheduledStarts,
    missedStarts,
    missedStartRatio,
    missedStartMaxRatio,
    offeredRateMinRatio,
    acceptedRateMinRatio,
    missingRequiredMetrics,
    streamContinuity,
  };
};

export const summarizeOpenLoopCheckpointProgress = ({
  targetRateTps,
  durationSec,
  dispatchedStarts,
}) => {
  const expectedStarts = Math.max(
    0,
    Math.ceil(Number(targetRateTps) * Number(durationSec)),
  );
  const scheduledStarts = Math.max(0, Math.floor(Number(dispatchedStarts)));
  return {
    expectedStarts,
    scheduledStarts,
    missedStarts: Math.max(0, expectedStarts - scheduledStarts),
  };
};

export const gaugeSlopePerSec = (samples, counterKey) => {
  const points = samples
    .map((sample) => ({
      x: Number(sample.timestampMs),
      y: Number(sample.counters[counterKey] ?? 0),
    }))
    .filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y));
  if (points.length < 2) {
    return 0;
  }
  const firstX = points[0].x;
  const normalized = points.map((point) => ({
    x: (point.x - firstX) / 1000,
    y: point.y,
  }));
  const meanX =
    normalized.reduce((sum, point) => sum + point.x, 0) / normalized.length;
  const meanY =
    normalized.reduce((sum, point) => sum + point.y, 0) / normalized.length;
  let numerator = 0;
  let denominator = 0;
  for (const point of normalized) {
    numerator += (point.x - meanX) * (point.y - meanY);
    denominator += (point.x - meanX) ** 2;
  }
  return denominator === 0 ? 0 : numerator / denominator;
};

const interpolateCounterEvents = (samples, counterKey) => {
  const events = [];
  for (let index = 1; index < samples.length; index += 1) {
    const previous = samples[index - 1];
    const current = samples[index];
    const previousValue = Number(previous.counters[counterKey] ?? 0);
    const currentValue = Number(current.counters[counterKey] ?? 0);
    const delta = Math.floor(currentValue - previousValue);
    const elapsedMs = current.timestampMs - previous.timestampMs;
    if (delta <= 0 || elapsedMs <= 0) {
      continue;
    }
    for (let ordinal = 1; ordinal <= delta; ordinal += 1) {
      events.push(previous.timestampMs + (elapsedMs * ordinal) / delta);
    }
  }
  return events;
};

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
    source: "node_readyz.localOgmiosSlot.currentSlot",
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

export const hasMissingRequiredMetrics = (counters) =>
  Array.isArray(counters?.missingMetrics) && counters.missingMetrics.length > 0;

export const classifyLikelyBottleneckWithEvidence = ({
  submitted,
  submitErrors,
  queueFullResponses = 0,
  acceptedDelta,
  rejectedDelta,
  commitTxDelta,
  mergeBlockDelta,
  targetAcceptedTps,
  avgAcceptedTps,
  clientSelfCheck,
  endCounters,
  waitForCommit,
  waitForMerge,
  scheduleLagMs = null,
  missedStarts = 0,
  inFlightHighWater = 0,
  submitConcurrency = 0,
  backlogSlopePerSec = 0,
  requiredMetricsMissing = [],
}) => {
  const evidence = {
    submitted,
    submitErrors,
    queueFullResponses,
    acceptedDelta,
    rejectedDelta,
    commitTxDelta,
    mergeBlockDelta,
    targetAcceptedTps,
    avgAcceptedTps,
    scheduleLagP95Ms: scheduleLagMs?.p95 ?? null,
    missedStarts,
    inFlightHighWater,
    submitConcurrency,
    backlogSlopePerSec,
    requiredMetricsMissing,
  };
  if (
    clientSelfCheck !== null &&
    clientSelfCheck.required === true &&
    clientSelfCheck.targetRate > 0 &&
    clientSelfCheck.achievedRate <
      (clientSelfCheck.minRequiredRate ?? clientSelfCheck.targetRate)
  ) {
    return {
      label: "benchmark-client limited",
      rule: "client self-check achieved rate below required rate",
      evidence,
    };
  }
  if (
    scheduleLagMs !== null &&
    scheduleLagMs.p95 !== null &&
    scheduleLagMs.p95 > 100
  ) {
    return {
      label: "benchmark-client limited",
      rule: "open-loop schedule lag p95 exceeded 100ms",
      evidence,
    };
  }
  if (
    submitConcurrency > 0 &&
    inFlightHighWater >= Math.floor(submitConcurrency * 0.98)
  ) {
    return {
      label: "benchmark-client limited",
      rule: "submit in-flight high-water reached configured concurrency",
      evidence,
    };
  }
  if (submitted <= 0) {
    return {
      label: "funding/workload exhausted",
      rule: "no transactions were submitted in the measured window",
      evidence,
    };
  }
  if (submitErrors > 0 || queueFullResponses > 0) {
    return {
      label: "HTTP ingress limited",
      rule: "measured-stage submit errors or queue-full responses were observed",
      evidence,
    };
  }
  if (requiredMetricsMissing.length > 0) {
    return {
      label: "metrics unavailable",
      rule: "required benchmark metrics were absent from Prometheus output",
      evidence,
    };
  }
  if (acceptedDelta + rejectedDelta < submitted) {
    const queueDepth = Number(endCounters.validationQueueDepth ?? 0);
    return {
      label: queueDepth > 0 ? "queue scheduling limited" : "Phase A/B limited",
      rule:
        queueDepth > 0
          ? "validation queue depth remained non-zero after measured submissions"
          : "accepted plus rejected count did not catch submitted count",
      evidence: {
        ...evidence,
        validationQueueDepth: queueDepth,
      },
    };
  }
  if (backlogSlopePerSec > 0.1) {
    return {
      label: "node throughput limited",
      rule: "validation backlog had a positive measured-window slope",
      evidence,
    };
  }
  if (rejectedDelta > 0) {
    return {
      label: "validation/workload limited",
      rule: "unexpected validation rejections were observed",
      evidence,
    };
  }
  if (waitForCommit && commitTxDelta < acceptedDelta) {
    return {
      label:
        Number(endCounters.unconfirmedSubmittedBlockPending ?? 0) > 0
          ? "L1 confirmation limited"
          : "commit limited",
      rule: "committed transaction count did not catch accepted count",
      evidence,
    };
  }
  if (waitForMerge && mergeBlockDelta <= 0 && commitTxDelta > 0) {
    return {
      label: "merge limited",
      rule: "commitment progressed but merge block counter did not advance",
      evidence,
    };
  }
  if (
    Number.isFinite(targetAcceptedTps) &&
    targetAcceptedTps > 0 &&
    avgAcceptedTps < targetAcceptedTps
  ) {
    return {
      label: "node throughput limited",
      rule: "average accepted TPS was below target accepted TPS",
      evidence,
    };
  }
  return {
    label: "no bottleneck detected",
    rule: "candidate met measured throughput and backlog criteria",
    evidence,
  };
};

export const classifyLikelyBottleneck = ({
  submitted,
  submitErrors,
  acceptedDelta,
  rejectedDelta,
  commitTxDelta,
  mergeBlockDelta,
  targetAcceptedTps,
  avgAcceptedTps,
  clientSelfCheck,
  endCounters,
  waitForCommit,
  waitForMerge,
}) => {
  if (
    clientSelfCheck !== null &&
    clientSelfCheck.required === true &&
    clientSelfCheck.targetRate > 0 &&
    clientSelfCheck.achievedRate <
      (clientSelfCheck.minRequiredRate ?? clientSelfCheck.targetRate)
  ) {
    return "benchmark-client limited";
  }
  if (submitted <= 0) {
    return "funding/workload exhausted";
  }
  if (submitErrors > 0) {
    return "HTTP ingress limited";
  }
  if (acceptedDelta + rejectedDelta < submitted) {
    const queueDepth = Number(endCounters.validationQueueDepth ?? 0);
    return queueDepth > 0 ? "queue scheduling limited" : "Phase A/B limited";
  }
  if (rejectedDelta > 0) {
    return "validation/workload limited";
  }
  if (waitForCommit && commitTxDelta < acceptedDelta) {
    return Number(endCounters.unconfirmedSubmittedBlockPending ?? 0) > 0
      ? "L1 confirmation limited"
      : "commit limited";
  }
  if (waitForMerge && mergeBlockDelta <= 0 && commitTxDelta > 0) {
    return "merge limited";
  }
  if (
    Number.isFinite(targetAcceptedTps) &&
    targetAcceptedTps > 0 &&
    avgAcceptedTps < targetAcceptedTps
  ) {
    return "node throughput limited";
  }
  return "no bottleneck detected";
};

export const createPhaseRecorder = (clock = () => Date.now()) => {
  const phases = [];
  let current = null;
  return {
    start(name) {
      if (current !== null) {
        current.endMs = clock();
        current.durationMs = current.endMs - current.startMs;
        phases.push(current);
      }
      current = { name, startMs: clock(), endMs: null, durationMs: null };
    },
    end() {
      if (current !== null) {
        current.endMs = clock();
        current.durationMs = current.endMs - current.startMs;
        phases.push(current);
        current = null;
      }
    },
    list() {
      return current === null ? [...phases] : [...phases, { ...current }];
    },
  };
};
