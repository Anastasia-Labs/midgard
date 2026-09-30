export type StressMetricStatus = "complete" | "partial" | "unavailable";

export type StressMetricPrecision =
  | "db_timestamp"
  | "observer_timestamp"
  | "artifact_timestamp";

export type StressMetricWindow = {
  readonly status: StressMetricStatus;
  readonly count: number;
  readonly startedAt: string | null;
  readonly finishedAt: string | null;
  readonly durationMs: number | null;
  readonly perSecond: number | null;
  readonly source: string;
  readonly precision: StressMetricPrecision;
  readonly missingCount: number;
  readonly notes: readonly string[];
};

export type StressMetrics = {
  readonly clientSubmission: StressMetricWindow;
  readonly durableAdmission: StressMetricWindow;
  readonly l2Admission: StressMetricWindow;
  readonly l1Commit: {
    readonly headers: StressMetricWindow;
    readonly l2Transactions: StressMetricWindow;
  };
  readonly immutableObservation: StressMetricWindow;
  readonly fullFinality: StressMetricWindow;
};

export type StressStageTransaction = {
  readonly txHash: string | null;
  readonly submission: {
    readonly status: "submitted" | "failed";
    readonly submittedAt: string | null;
  };
  readonly acceptance: {
    readonly status:
      | "accepted"
      | "rejected"
      | "timeout"
      | "not_observed"
      | "not_submitted";
    readonly acceptedAt?: string;
  };
  readonly finality: {
    readonly status: "committed" | "rejected" | "timeout" | "not_observed";
    readonly committedAt?: string;
  };
};

export type StressDbAdmissionRow = {
  readonly txHash: string;
  readonly status: string;
  readonly firstSeenAt: string;
  readonly validationStartedAt: string | null;
  readonly terminalAt: string | null;
};

export type StressDbL1CommitRow = {
  readonly txHash: string;
  readonly headerHash: string;
  readonly status: string;
  readonly createdAt: string;
  readonly observedConfirmedAt: string | null;
};

export type StressDbImmutableRow = {
  readonly txHash: string;
  readonly observedAt: string;
};

export type StressDbResidueRow = {
  readonly txHash: string;
  readonly source: "mempool" | "processed_mempool";
  readonly observedAt: string;
};

export type StressStageMetricDbSources = {
  readonly l2Admissions: readonly StressDbAdmissionRow[];
  readonly l1Commits: readonly StressDbL1CommitRow[];
  readonly immutableObservations: readonly StressDbImmutableRow[];
  readonly residue: readonly StressDbResidueRow[];
};

export type StressFullFinalityDrainProof = {
  readonly observedAt: string;
  readonly stateQueueHeaderHashes: readonly string[];
  readonly readyz?: unknown;
  readonly watcherReadyz?: unknown;
  readonly manualMergeInvoked: boolean;
};

export type BuildStressMetricsInput = {
  readonly requestedCount: number;
  readonly submittedCount: number;
  readonly acceptedCount: number;
  readonly observedCommittedCount: number;
  readonly startedAt: string;
  readonly submissionFinishedAt: string;
  readonly finishedAt: string;
  readonly transactions: readonly StressStageTransaction[];
  readonly dbSources?: StressStageMetricDbSources;
  readonly fullFinalityDrainProof?: StressFullFinalityDrainProof;
};

export type MetricRangeInput = {
  readonly count: number;
  readonly expectedCount: number;
  readonly startedAt: string | null | undefined;
  readonly finishedAt: string | null | undefined;
  readonly source: string;
  readonly precision: StressMetricPrecision;
  readonly notes?: readonly string[];
};

export type SqlAdmissionRow = {
  readonly tx_hash: string;
  readonly status: string;
  readonly first_seen_at: Date;
  readonly validation_started_at: Date | null;
  readonly terminal_at: Date | null;
};

export type SqlL1CommitRow = {
  readonly tx_hash: string;
  readonly header_hash: string;
  readonly status: string;
  readonly created_at: Date;
  readonly observed_confirmed_at_ms: bigint | number | string | null;
};

export type SqlImmutableRow = {
  readonly tx_hash: string;
  readonly observed_at: Date;
};

export type SqlResidueRow = {
  readonly tx_hash: string;
  readonly source: "mempool" | "processed_mempool";
  readonly observed_at: Date;
};

const TX_HASH_PATTERN = /^[0-9a-f]{64}$/i;

export const emptyStressMetricDbSources = (): StressStageMetricDbSources => ({
  l2Admissions: [],
  l1Commits: [],
  immutableObservations: [],
  residue: [],
});

export const roundMetric = (value: number): number | null =>
  Number.isFinite(value) ? Number(value.toFixed(6)) : null;

const uniqueStrings = (values: readonly string[]): readonly string[] => [
  ...new Set(values),
];

const parseTimestampMs = (value: string | null | undefined): number | null => {
  if (value === null || value === undefined) {
    return null;
  }
  const ms = Date.parse(value);
  return Number.isFinite(ms) ? ms : null;
};

export const isoFromDate = (value: Date | null | undefined): string | null =>
  value === null || value === undefined ? null : value.toISOString();

export const isoFromEpochMs = (
  value: bigint | number | string | null | undefined,
): string | null => {
  if (value === null || value === undefined) {
    return null;
  }
  const ms = Number(value);
  return Number.isFinite(ms) ? new Date(ms).toISOString() : null;
};

export const minIso = (
  values: readonly (string | null | undefined)[],
): string | null => {
  const timestamps = values
    .map(parseTimestampMs)
    .filter((value): value is number => value !== null);
  return timestamps.length === 0
    ? null
    : new Date(Math.min(...timestamps)).toISOString();
};

export const maxIso = (
  values: readonly (string | null | undefined)[],
): string | null => {
  const timestamps = values
    .map(parseTimestampMs)
    .filter((value): value is number => value !== null);
  return timestamps.length === 0
    ? null
    : new Date(Math.max(...timestamps)).toISOString();
};

export const normalizeHash = (value: string): string => value.toLowerCase();

export const txHashBuffers = (txHashes: readonly string[]): readonly Buffer[] =>
  uniqueStrings(txHashes.map(normalizeHash))
    .filter((txHash) => TX_HASH_PATTERN.test(txHash))
    .map((txHash) => Buffer.from(txHash, "hex"));

export const emptyUnavailableMetric = ({
  source,
  precision,
  missingCount = 0,
  notes = [],
}: {
  readonly source: string;
  readonly precision: StressMetricPrecision;
  readonly missingCount?: number;
  readonly notes?: readonly string[];
}): StressMetricWindow => ({
  status: "unavailable",
  count: 0,
  startedAt: null,
  finishedAt: null,
  durationMs: null,
  perSecond: null,
  source,
  precision,
  missingCount: Math.max(0, missingCount),
  notes: uniqueStrings([...notes, "no_observations"]),
});

export const computeMetricWindow = ({
  count,
  expectedCount,
  startedAt,
  finishedAt,
  source,
  precision,
  notes = [],
}: MetricRangeInput): StressMetricWindow => {
  const normalizedCount = Math.max(0, count);
  const missingCount = Math.max(0, expectedCount - normalizedCount);
  if (normalizedCount === 0) {
    return emptyUnavailableMetric({
      source,
      precision,
      missingCount,
      notes,
    });
  }

  const startedAtMs = parseTimestampMs(startedAt);
  const finishedAtMs = parseTimestampMs(finishedAt);
  if (
    startedAtMs === null ||
    finishedAtMs === null ||
    finishedAtMs < startedAtMs
  ) {
    return {
      status: "partial",
      count: normalizedCount,
      startedAt:
        startedAtMs === null ? null : new Date(startedAtMs).toISOString(),
      finishedAt:
        finishedAtMs === null ? null : new Date(finishedAtMs).toISOString(),
      durationMs: null,
      perSecond: null,
      source,
      precision,
      missingCount,
      notes: uniqueStrings([...notes, "metric_window_unavailable"]),
    };
  }

  const durationMs = finishedAtMs - startedAtMs;
  const perSecond =
    durationMs <= 0 ? null : roundMetric(normalizedCount / (durationMs / 1000));
  return {
    status: missingCount === 0 ? "complete" : "partial",
    count: normalizedCount,
    startedAt: new Date(startedAtMs).toISOString(),
    finishedAt: new Date(finishedAtMs).toISOString(),
    durationMs,
    perSecond,
    source,
    precision,
    missingCount,
    notes: uniqueStrings([
      ...notes,
      ...(durationMs <= 0 ? ["zero_duration_window"] : []),
    ]),
  };
};
