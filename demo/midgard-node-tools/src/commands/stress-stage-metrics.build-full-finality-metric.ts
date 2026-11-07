import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  acceptedTxHashes,
  buildClientSubmissionMetric,
  buildDurableAdmissionMetric,
  buildImmutableObservationMetric,
  buildL1CommitMetrics,
  buildL2AdmissionMetric,
  metricFromArtifactRange,
  readyzNotes,
} from "./stress-stage-metrics.build-l1-commit-metrics.js";
import {
  type BuildStressMetricsInput,
  emptyStressMetricDbSources,
  emptyUnavailableMetric,
  isoFromDate,
  isoFromEpochMs,
  minIso,
  normalizeHash,
  type SqlAdmissionRow,
  type SqlImmutableRow,
  type SqlL1CommitRow,
  type SqlResidueRow,
  type StressMetrics,
  type StressMetricWindow,
  type StressStageMetricDbSources,
  txHashBuffers,
} from "./stress-stage-metrics.compute-metric-window.js";

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
      .filter((row) => row.status === "locally_applied")
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
