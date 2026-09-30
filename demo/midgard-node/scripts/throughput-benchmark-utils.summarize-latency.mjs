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
