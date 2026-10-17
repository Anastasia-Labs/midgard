import {
  type BuildStressMetricsInput,
  computeMetricWindow,
  emptyUnavailableMetric,
  maxIso,
  type MetricRangeInput,
  minIso,
  normalizeHash,
  type StressDbL1CommitRow,
  type StressMetrics,
  type StressMetricWindow,
  type StressStageTransaction,
} from "./stress-stage-metrics.compute-metric-window.js";

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

export const buildClientSubmissionMetric = ({
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

export const acceptedTxHashes = (
  transactions: readonly StressStageTransaction[],
): readonly string[] =>
  transactions
    .filter((tx) => tx.txHash !== null && tx.acceptance.status === "accepted")
    .map((tx) => normalizeHash(tx.txHash!));

export const buildDurableAdmissionMetric = ({
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

export const buildL2AdmissionMetric = ({
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

export const buildL1CommitMetrics = ({
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

export const buildImmutableObservationMetric = ({
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

export const readyzNotes = (
  label: string,
  body: unknown,
): readonly string[] => {
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
