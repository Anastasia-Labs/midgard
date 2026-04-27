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

export const quantile = (values, q) => {
  if (values.length === 0) {
    return null;
  }
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.min(
    sorted.length - 1,
    Math.max(0, Math.ceil(q * sorted.length) - 1),
  );
  return sorted[index];
};

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
  const total = values.reduce((acc, value) => acc + value, 0);
  return {
    count: values.length,
    min: Math.min(...values),
    p50: quantile(values, 0.5),
    p95: quantile(values, 0.95),
    p99: quantile(values, 0.99),
    max: Math.max(...values),
    mean: total / values.length,
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
