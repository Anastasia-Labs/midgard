import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

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

type MetricRangeInput = {
  readonly count: number;
  readonly expectedCount: number;
  readonly startedAt: string | null | undefined;
  readonly finishedAt: string | null | undefined;
  readonly source: string;
  readonly precision: StressMetricPrecision;
  readonly notes?: readonly string[];
};

type SqlAdmissionRow = {
  readonly tx_hash: string;
  readonly status: string;
  readonly first_seen_at: Date;
  readonly validation_started_at: Date | null;
  readonly terminal_at: Date | null;
};

type SqlL1CommitRow = {
  readonly tx_hash: string;
  readonly header_hash: string;
  readonly status: string;
  readonly created_at: Date;
  readonly observed_confirmed_at_ms: bigint | number | string | null;
};

type SqlImmutableRow = {
  readonly tx_hash: string;
  readonly observed_at: Date;
};

type SqlResidueRow = {
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

const isoFromDate = (value: Date | null | undefined): string | null =>
  value === null || value === undefined ? null : value.toISOString();

const isoFromEpochMs = (
  value: bigint | number | string | null | undefined,
): string | null => {
  if (value === null || value === undefined) {
    return null;
  }
  const ms = Number(value);
  return Number.isFinite(ms) ? new Date(ms).toISOString() : null;
};

const minIso = (
  values: readonly (string | null | undefined)[],
): string | null => {
  const timestamps = values
    .map(parseTimestampMs)
    .filter((value): value is number => value !== null);
  return timestamps.length === 0
    ? null
    : new Date(Math.min(...timestamps)).toISOString();
};

const maxIso = (
  values: readonly (string | null | undefined)[],
): string | null => {
  const timestamps = values
    .map(parseTimestampMs)
    .filter((value): value is number => value !== null);
  return timestamps.length === 0
    ? null
    : new Date(Math.max(...timestamps)).toISOString();
};

const normalizeHash = (value: string): string => value.toLowerCase();

const txHashBuffers = (txHashes: readonly string[]): readonly Buffer[] =>
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

export const metricFromArtifactRange = (
  input: Omit<MetricRangeInput, "precision">,
): StressMetricWindow =>
  computeMetricWindow({ ...input, precision: "artifact_timestamp" });

export const metricFromDbRange = (
  input: Omit<MetricRangeInput, "precision">,
): StressMetricWindow =>
  computeMetricWindow({ ...input, precision: "db_timestamp" });

const metricFromObserverRange = (
  input: Omit<MetricRangeInput, "precision">,
): StressMetricWindow =>
  computeMetricWindow({ ...input, precision: "observer_timestamp" });

const buildClientSubmissionMetric = ({
  requestedCount,
  submittedCount,
  startedAt,
  submissionFinishedAt,
}: BuildStressMetricsInput): StressMetricWindow =>
  metricFromArtifactRange({
    count: submittedCount,
    expectedCount: requestedCount,
    startedAt,
    finishedAt: submissionFinishedAt,
    source: "stress_artifact.submissions",
    notes:
      submittedCount >= requestedCount ? [] : ["client_submission_incomplete"],
  });

const submittedTxHashes = (
  transactions: readonly StressStageTransaction[],
): readonly string[] =>
  transactions
    .filter((tx) => tx.txHash !== null && tx.submission.status === "submitted")
    .map((tx) => normalizeHash(tx.txHash!));

const acceptedTxHashes = (
  transactions: readonly StressStageTransaction[],
): readonly string[] =>
  transactions
    .filter((tx) => tx.txHash !== null && tx.acceptance.status === "accepted")
    .map((tx) => normalizeHash(tx.txHash!));

const buildDurableAdmissionMetric = ({
  submittedCount,
  transactions,
  dbSources,
}: BuildStressMetricsInput): StressMetricWindow => {
  const expectedHashes = submittedTxHashes(transactions);
  if (dbSources === undefined) {
    return emptyUnavailableMetric({
      source: "tx_admissions",
      precision: "db_timestamp",
      missingCount: submittedCount,
      notes: ["db_metrics_unavailable"],
    });
  }
  const rows = dbSources.l2Admissions.filter((row) =>
    expectedHashes.includes(row.txHash),
  );
  return metricFromDbRange({
    count: rows.length,
    expectedCount: expectedHashes.length,
    startedAt: minIso(rows.map((row) => row.firstSeenAt)),
    finishedAt: maxIso(rows.map((row) => row.firstSeenAt)),
    source: "tx_admissions.first_seen_at",
    notes:
      rows.length === expectedHashes.length
        ? ["durable_enqueue_not_validation_acceptance"]
        : [
            "durable_enqueue_not_validation_acceptance",
            "missing_durable_admission_db_rows",
          ],
  });
};

const committedTransactions = (
  transactions: readonly StressStageTransaction[],
): readonly StressStageTransaction[] =>
  transactions.filter(
    (tx) => tx.txHash !== null && tx.finality.status === "committed",
  );

const buildL2AdmissionMetric = ({
  acceptedCount,
  requestedCount,
  transactions,
  dbSources,
}: BuildStressMetricsInput): StressMetricWindow => {
  const expectedHashes = acceptedTxHashes(transactions);
  if (dbSources !== undefined) {
    const acceptedRows = dbSources.l2Admissions.filter(
      (row) => row.status === "accepted" && expectedHashes.includes(row.txHash),
    );
    return metricFromDbRange({
      count: acceptedRows.length,
      expectedCount: expectedHashes.length,
      startedAt: minIso(acceptedRows.map((row) => row.firstSeenAt)),
      finishedAt: maxIso(acceptedRows.map((row) => row.terminalAt)),
      source: "tx_admissions",
      notes:
        acceptedRows.length === expectedHashes.length
          ? []
          : ["missing_l2_admission_db_rows"],
    });
  }

  const acceptedTransactions = transactions.filter(
    (tx) => tx.txHash !== null && tx.acceptance.status === "accepted",
  );
  return metricFromObserverRange({
    count: acceptedCount,
    expectedCount: requestedCount,
    startedAt: minIso(
      acceptedTransactions.map((tx) => tx.submission.submittedAt),
    ),
    finishedAt: maxIso(
      acceptedTransactions.map((tx) => tx.acceptance.acceptedAt),
    ),
    source: "stress_artifact.tx_status_acceptance_observer",
    notes: [
      "db_metrics_unavailable",
      "based_on_stress_observation_not_db_admission_timestamps",
    ],
  });
};

const buildL1CommitMetrics = ({
  transactions,
  dbSources,
}: BuildStressMetricsInput): StressMetrics["l1Commit"] => {
  const expectedHashes = acceptedTxHashes(transactions);
  const baseNotes = [
    "window_start_is_journal_created_at_not_submit_at",
    "not_exact_submit_to_confirmed",
  ];
  if (dbSources === undefined) {
    return {
      headers: emptyUnavailableMetric({
        source: "pending_block_finalizations+pending_block_finalization_txs",
        precision: "db_timestamp",
        missingCount: expectedHashes.length,
        notes: ["db_metrics_unavailable", ...baseNotes],
      }),
      l2Transactions: emptyUnavailableMetric({
        source: "pending_block_finalizations+pending_block_finalization_txs",
        precision: "db_timestamp",
        missingCount: expectedHashes.length,
        notes: ["db_metrics_unavailable", ...baseNotes],
      }),
    };
  }

  const rows = dbSources.l1Commits.filter((row) =>
    expectedHashes.includes(row.txHash),
  );
  const rowsByHeader = new Map<string, readonly StressDbL1CommitRow[]>();
  for (const row of rows) {
    rowsByHeader.set(row.headerHash, [
      ...(rowsByHeader.get(row.headerHash) ?? []),
      row,
    ]);
  }
  const committedHeaderHashes = [...rowsByHeader.entries()]
    .filter(([_headerHash, headerRows]) =>
      headerRows.some((row) => row.observedConfirmedAt !== null),
    )
    .map(([headerHash]) => headerHash);
  const committedHeaderHashSet = new Set(committedHeaderHashes);
  const committedRows = rows.filter((row) =>
    committedHeaderHashSet.has(row.headerHash),
  );
  const committedTxCount = new Set(committedRows.map((row) => row.txHash)).size;
  const missingMembership = Math.max(
    0,
    expectedHashes.length - new Set(rows.map((row) => row.txHash)).size,
  );
  const notes = [
    ...baseNotes,
    ...(missingMembership > 0 ? ["missing_l1_commit_membership"] : []),
  ];

  return {
    headers: metricFromDbRange({
      count: committedHeaderHashes.length,
      expectedCount: rowsByHeader.size,
      startedAt: minIso(committedRows.map((row) => row.createdAt)),
      finishedAt: maxIso(committedRows.map((row) => row.observedConfirmedAt)),
      source: "pending_block_finalizations+pending_block_finalization_txs",
      notes,
    }),
    l2Transactions: metricFromDbRange({
      count: committedTxCount,
      expectedCount: expectedHashes.length,
      startedAt: minIso(committedRows.map((row) => row.createdAt)),
      finishedAt: maxIso(committedRows.map((row) => row.observedConfirmedAt)),
      source: "pending_block_finalizations+pending_block_finalization_txs",
      notes,
    }),
  };
};

const buildImmutableObservationMetric = ({
  observedCommittedCount,
  transactions,
  dbSources,
}: BuildStressMetricsInput): StressMetricWindow => {
  const expectedHashes = acceptedTxHashes(transactions);
  if (dbSources !== undefined) {
    const rows = dbSources.immutableObservations.filter((row) =>
      expectedHashes.includes(row.txHash),
    );
    return metricFromDbRange({
      count: rows.length,
      expectedCount: expectedHashes.length,
      startedAt: minIso(rows.map((row) => row.observedAt)),
      finishedAt: maxIso(rows.map((row) => row.observedAt)),
      source: "immutable",
      notes:
        rows.length === expectedHashes.length
          ? ["immutable_observation_is_not_full_finality"]
          : [
              "immutable_observation_is_not_full_finality",
              "missing_immutable_rows",
            ],
    });
  }

  const committed = committedTransactions(transactions);
  return metricFromObserverRange({
    count: observedCommittedCount,
    expectedCount: expectedHashes.length,
    startedAt: minIso(committed.map((tx) => tx.submission.submittedAt)),
    finishedAt: maxIso(committed.map((tx) => tx.finality.committedAt)),
    source: "stress_artifact.tx_status_commit_observer",
    notes: [
      "db_metrics_unavailable",
      "tx_status_committed_is_not_full_finality",
    ],
  });
};

const readyzNotes = (label: string, body: unknown): readonly string[] => {
  if (typeof body !== "object" || body === null) {
    return [];
  }
  const ready = (body as { readonly ready?: unknown }).ready;
  if (ready === true) {
    return [];
  }
  const reasons = (body as { readonly reasons?: unknown }).reasons;
  if (Array.isArray(reasons) && reasons.length > 0) {
    return [`${label}_not_ready:${reasons.map(String).join(",")}`];
  }
  return [`${label}_not_ready`];
};

const buildFullFinalityMetric = ({
  transactions,
  dbSources,
  fullFinalityDrainProof,
}: BuildStressMetricsInput): StressMetricWindow => {
  const expectedHashes = acceptedTxHashes(transactions);
  if (fullFinalityDrainProof === undefined) {
    return emptyUnavailableMetric({
      source: "automatic_drain_proof",
      precision: "artifact_timestamp",
      missingCount: expectedHashes.length,
      notes: [
        "full_finality_drain_not_requested",
        "no_manual_merge_invoked_by_stress_harness",
      ],
    });
  }
  if (dbSources === undefined) {
    return emptyUnavailableMetric({
      source:
        "pending_block_finalizations+stateQueue+mempool+processed_mempool",
      precision: "artifact_timestamp",
      missingCount: expectedHashes.length,
      notes: ["db_metrics_unavailable"],
    });
  }

  const rows = dbSources.l1Commits.filter((row) =>
    expectedHashes.includes(row.txHash),
  );
  const headerByTx = new Map(rows.map((row) => [row.txHash, row.headerHash]));
  const finalizedHeaders = new Set(
    rows
      .filter((row) => row.status === "finalized")
      .map((row) => row.headerHash),
  );
  const queuedHeaders = new Set(
    fullFinalityDrainProof.stateQueueHeaderHashes.map(normalizeHash),
  );
  const residueHashes = new Set(dbSources.residue.map((row) => row.txHash));
  const finalTxHashes = expectedHashes.filter((txHash) => {
    const headerHash = headerByTx.get(txHash);
    return (
      headerHash !== undefined &&
      finalizedHeaders.has(headerHash) &&
      !queuedHeaders.has(headerHash) &&
      !residueHashes.has(txHash)
    );
  });
  const missingMembership = expectedHashes.filter(
    (txHash) => !headerByTx.has(txHash),
  ).length;
  const unfinalizedHeaders = [
    ...new Set(rows.map((row) => row.headerHash)),
  ].filter((headerHash) => !finalizedHeaders.has(headerHash)).length;
  const queuedStressHeaders = [
    ...new Set(rows.map((row) => row.headerHash)),
  ].filter((headerHash) => queuedHeaders.has(headerHash)).length;
  const firstSubmittedAt = minIso(
    transactions
      .filter((tx) => tx.txHash !== null && expectedHashes.includes(tx.txHash))
      .map((tx) => tx.submission.submittedAt),
  );
  return metricFromArtifactRange({
    count: finalTxHashes.length,
    expectedCount: expectedHashes.length,
    startedAt: firstSubmittedAt,
    finishedAt: fullFinalityDrainProof.observedAt,
    source: "pending_block_finalizations+stateQueue+mempool+processed_mempool",
    notes: [
      ...(missingMembership > 0 ? ["missing_l1_commit_membership"] : []),
      ...(unfinalizedHeaders > 0 ? ["unfinalized_stress_headers"] : []),
      ...(queuedStressHeaders > 0
        ? ["state_queue_contains_stress_headers"]
        : []),
      ...(residueHashes.size > 0 ? ["stress_tx_residue_present"] : []),
      ...readyzNotes("node_readyz", fullFinalityDrainProof.readyz),
      ...readyzNotes("watcher_readyz", fullFinalityDrainProof.watcherReadyz),
      ...(fullFinalityDrainProof.manualMergeInvoked
        ? ["manual_merge_invoked"]
        : ["no_manual_merge_invoked_by_stress_harness"]),
    ],
  });
};

export const buildStressMetrics = (
  input: BuildStressMetricsInput,
): StressMetrics => ({
  clientSubmission: buildClientSubmissionMetric(input),
  durableAdmission: buildDurableAdmissionMetric(input),
  l2Admission: buildL2AdmissionMetric(input),
  l1Commit: buildL1CommitMetrics(input),
  immutableObservation: buildImmutableObservationMetric(input),
  fullFinality: buildFullFinalityMetric(input),
});

export const flattenStressMetricRows = (
  metrics: StressMetrics,
): readonly (readonly [string, StressMetricWindow])[] => [
  ["client_submission", metrics.clientSubmission],
  ["durable_admission", metrics.durableAdmission],
  ["l2_admission", metrics.l2Admission],
  ["l1_commit_headers", metrics.l1Commit.headers],
  ["l1_commit_l2_txs", metrics.l1Commit.l2Transactions],
  ["immutable_observation", metrics.immutableObservation],
  ["full_finality", metrics.fullFinality],
];

export const collectStressStageMetricSourcesFromSql = (
  txHashes: readonly string[],
): Effect.Effect<StressStageMetricDbSources, never, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const txIds = txHashBuffers(txHashes);
    if (txIds.length === 0) {
      return emptyStressMetricDbSources();
    }
    const sql = yield* SqlClient.SqlClient;
    const [
      admissionRows,
      commitRows,
      immutableRows,
      mempoolRows,
      processedRows,
    ] = yield* Effect.all(
      [
        sql<SqlAdmissionRow>`SELECT
              encode(tx_id, 'hex') AS tx_hash,
              status,
              first_seen_at,
              validation_started_at,
              terminal_at
            FROM tx_admissions
            WHERE ${sql.in("tx_id", txIds)}`,
        sql<SqlL1CommitRow>`SELECT
              encode(member.member_id, 'hex') AS tx_hash,
              encode(pending.header_hash, 'hex') AS header_hash,
              pending.status,
              pending.created_at,
              pending.observed_confirmed_at_ms
            FROM pending_block_finalization_txs AS member
            JOIN pending_block_finalizations AS pending
              ON pending.header_hash = member.header_hash
            WHERE ${sql.in("member_id", txIds)}`,
        sql<SqlImmutableRow>`SELECT
              encode(tx_id, 'hex') AS tx_hash,
              time_stamp_tz AS observed_at
            FROM immutable
            WHERE ${sql.in("tx_id", txIds)}`,
        sql<SqlResidueRow>`SELECT
              encode(tx_id, 'hex') AS tx_hash,
              'mempool' AS source,
              time_stamp_tz AS observed_at
            FROM mempool
            WHERE ${sql.in("tx_id", txIds)}`,
        sql<SqlResidueRow>`SELECT
              encode(tx_id, 'hex') AS tx_hash,
              'processed_mempool' AS source,
              time_stamp_tz AS observed_at
            FROM processed_mempool
            WHERE ${sql.in("tx_id", txIds)}`,
      ],
      { concurrency: "unbounded" },
    );

    return {
      l2Admissions: admissionRows.map((row) => ({
        txHash: normalizeHash(row.tx_hash),
        status: row.status,
        firstSeenAt: row.first_seen_at.toISOString(),
        validationStartedAt: isoFromDate(row.validation_started_at),
        terminalAt: isoFromDate(row.terminal_at),
      })),
      l1Commits: commitRows.map((row) => ({
        txHash: normalizeHash(row.tx_hash),
        headerHash: normalizeHash(row.header_hash),
        status: row.status,
        createdAt: row.created_at.toISOString(),
        observedConfirmedAt: isoFromEpochMs(row.observed_confirmed_at_ms),
      })),
      immutableObservations: immutableRows.map((row) => ({
        txHash: normalizeHash(row.tx_hash),
        observedAt: row.observed_at.toISOString(),
      })),
      residue: [...mempoolRows, ...processedRows].map((row) => ({
        txHash: normalizeHash(row.tx_hash),
        source: row.source,
        observedAt: row.observed_at.toISOString(),
      })),
    };
  }).pipe(Effect.orDie);
