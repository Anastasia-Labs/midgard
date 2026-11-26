import { summarizeLatency } from "./throughput-benchmark-utils.summarize-latency.mjs";

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
