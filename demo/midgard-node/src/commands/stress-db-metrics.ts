import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  collectEnvironmentFingerprint,
  type EnvironmentFingerprint,
} from "@/commands/stress-environment-fingerprint.js";

export type StageWindowQuery = {
  readonly windowStart: string;
  readonly windowEnd: string;
  readonly txHashSample?: readonly string[];
  readonly trimFraction: number;
  readonly offeredCount?: number;
  readonly calibrationProofRef?: string | null;
};

export type StagePercentiles = {
  readonly p50Ms: number | null;
  readonly p95Ms: number | null;
  readonly p99Ms: number | null;
  readonly sampleCount: number;
};

export type SteadyStateStageResult = {
  readonly stage: string;
  readonly offeredCount: number;
  readonly rawCount: number;
  readonly trimmedCount: number;
  readonly rawPerSecond: number | null;
  readonly steadyStatePerSecond: number | null;
  readonly latency: StagePercentiles;
  readonly windowTrim: {
    readonly discardedHeadMs: number;
    readonly discardedTailMs: number;
  };
  readonly precision: "db_timestamp";
  readonly notes: readonly string[];
};

export type GroundTruthMetrics = {
  readonly schemaVersion: 1;
  readonly window: {
    readonly start: string;
    readonly end: string;
    readonly trimFraction: number;
  };
  readonly stages: {
    readonly admission: SteadyStateStageResult;
    readonly validationStart: SteadyStateStageResult;
    readonly validationTerminal: SteadyStateStageResult;
    readonly mempoolPersist: SteadyStateStageResult;
    readonly l1CommitHeader: SteadyStateStageResult;
    readonly l1CommitConfirm: SteadyStateStageResult;
    readonly immutableObservation: SteadyStateStageResult;
    readonly fullFinality: SteadyStateStageResult;
  };
  readonly fingerprint: EnvironmentFingerprint;
};

type AdmissionWindowRow = {
  readonly tx_hash: string;
  readonly status: string;
  readonly first_seen_at: Date;
  readonly validation_started_at: Date | null;
  readonly terminal_at: Date | null;
};

type MempoolWindowRow = {
  readonly tx_hash: string;
  readonly observed_at: Date;
};

type L1CommitWindowRow = {
  readonly tx_hash: string;
  readonly header_hash: string;
  readonly status: string;
  readonly created_at: Date;
  readonly observed_confirmed_at_ms: bigint | number | string | null;
};

type ImmutableWindowRow = {
  readonly tx_hash: string;
  readonly observed_at: Date;
};

type CountRow = {
  readonly count: bigint | number | string;
};

type PendingStatusRow = {
  readonly header_hash: string;
  readonly status: string;
};

const ms = (date: Date): number => date.getTime();

const isoFromEpochMs = (
  value: bigint | number | string | null | undefined,
): string | null => {
  if (value === null || value === undefined) {
    return null;
  }
  const parsed = Number(value);
  return Number.isFinite(parsed) ? new Date(parsed).toISOString() : null;
};

export const percentile = (
  sortedValues: readonly number[],
  quantile: number,
): number | null => {
  if (sortedValues.length === 0) {
    return null;
  }
  const index = Math.min(
    sortedValues.length - 1,
    Math.max(0, Math.ceil(sortedValues.length * quantile) - 1),
  );
  return sortedValues[index]!;
};

const percentiles = (values: readonly number[]): StagePercentiles => {
  const sorted = [...values]
    .filter((value) => Number.isFinite(value) && value >= 0)
    .sort((left, right) => left - right);
  return {
    p50Ms: percentile(sorted, 0.5),
    p95Ms: percentile(sorted, 0.95),
    p99Ms: percentile(sorted, 0.99),
    sampleCount: sorted.length,
  };
};

export const trimByEventFraction = <A>(
  rows: readonly A[],
  trimFraction: number,
): readonly A[] => {
  if (rows.length === 0) {
    return [];
  }
  const bounded = Math.min(0.49, Math.max(0, trimFraction));
  const discard = Math.floor(rows.length * bounded);
  return rows.slice(discard, rows.length - discard);
};

const rate = (count: number, startedAt: Date | null, finishedAt: Date | null) => {
  if (count <= 1 || startedAt === null || finishedAt === null) {
    return null;
  }
  const elapsedMs = finishedAt.getTime() - startedAt.getTime();
  return elapsedMs > 0 ? Number((count / (elapsedMs / 1000)).toFixed(6)) : null;
};

const firstDate = <A>(
  rows: readonly A[],
  timestamp: (row: A) => Date | null,
): Date | null => timestamp(rows[0] as A) ?? null;

const lastDate = <A>(
  rows: readonly A[],
  timestamp: (row: A) => Date | null,
): Date | null => timestamp(rows[rows.length - 1] as A) ?? null;

export const buildSteadyStateStageResult = <A>({
  stage,
  rows,
  offeredCount,
  trimFraction,
  timestamp,
  latencyMs,
  notes = [],
}: {
  readonly stage: string;
  readonly rows: readonly A[];
  readonly offeredCount: number;
  readonly trimFraction: number;
  readonly timestamp: (row: A) => Date | null;
  readonly latencyMs: (row: A) => number | null;
  readonly notes?: readonly string[];
}): SteadyStateStageResult => {
  const trimmed = trimByEventFraction(rows, trimFraction);
  const rawStart = rows.length === 0 ? null : firstDate(rows, timestamp);
  const rawEnd = rows.length === 0 ? null : lastDate(rows, timestamp);
  const trimmedStart =
    trimmed.length === 0 ? null : firstDate(trimmed, timestamp);
  const trimmedEnd = trimmed.length === 0 ? null : lastDate(trimmed, timestamp);
  const headMs =
    rawStart !== null && trimmedStart !== null
      ? Math.max(0, trimmedStart.getTime() - rawStart.getTime())
      : 0;
  const tailMs =
    rawEnd !== null && trimmedEnd !== null
      ? Math.max(0, rawEnd.getTime() - trimmedEnd.getTime())
      : 0;
  return {
    stage,
    offeredCount,
    rawCount: rows.length,
    trimmedCount: trimmed.length,
    rawPerSecond: rate(rows.length, rawStart, rawEnd),
    steadyStatePerSecond: rate(trimmed.length, trimmedStart, trimmedEnd),
    latency: percentiles(
      trimmed.flatMap((row) => {
        const value = latencyMs(row);
        return value === null ? [] : [value];
      }),
    ),
    windowTrim: {
      discardedHeadMs: headMs,
      discardedTailMs: tailMs,
    },
    precision: "db_timestamp",
    notes,
  };
};

const normalizeHash = (value: string): string => value.toLowerCase();

export const collectGroundTruthMetricsFromSql = (
  query: StageWindowQuery,
): Effect.Effect<GroundTruthMetrics, never, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const start = new Date(query.windowStart);
    const end = new Date(query.windowEnd);
    const [
      admissionRows,
      mempoolRows,
      processedRows,
      commitRows,
      immutableRows,
      mempoolResidueRows,
      processedResidueRows,
      unfinalizedRows,
    ] = yield* Effect.all(
      [
        sql<AdmissionWindowRow>`SELECT
            encode(tx_id, 'hex') AS tx_hash,
            status,
            first_seen_at,
            validation_started_at,
            terminal_at
          FROM tx_admissions
          WHERE first_seen_at BETWEEN ${start} AND ${end}
          ORDER BY first_seen_at`,
        sql<MempoolWindowRow>`SELECT
            encode(tx_id, 'hex') AS tx_hash,
            time_stamp_tz AS observed_at
          FROM mempool
          WHERE time_stamp_tz BETWEEN ${start} AND ${end}
          ORDER BY time_stamp_tz`,
        sql<MempoolWindowRow>`SELECT
            encode(tx_id, 'hex') AS tx_hash,
            time_stamp_tz AS observed_at
          FROM processed_mempool
          WHERE time_stamp_tz BETWEEN ${start} AND ${end}
          ORDER BY time_stamp_tz`,
        sql<L1CommitWindowRow>`SELECT
            encode(member.member_id, 'hex') AS tx_hash,
            encode(pending.header_hash, 'hex') AS header_hash,
            pending.status,
            pending.created_at,
            pending.observed_confirmed_at_ms
          FROM pending_block_finalization_txs AS member
          JOIN pending_block_finalizations AS pending
            ON pending.header_hash = member.header_hash
          WHERE pending.created_at BETWEEN ${start} AND ${end}
          ORDER BY pending.created_at`,
        sql<ImmutableWindowRow>`SELECT
            encode(tx_id, 'hex') AS tx_hash,
            time_stamp_tz AS observed_at
          FROM immutable
          WHERE time_stamp_tz BETWEEN ${start} AND ${end}
          ORDER BY time_stamp_tz`,
        sql<CountRow>`SELECT COUNT(*)::bigint AS count
          FROM mempool
          WHERE time_stamp_tz BETWEEN ${start} AND ${end}`,
        sql<CountRow>`SELECT COUNT(*)::bigint AS count
          FROM processed_mempool
          WHERE time_stamp_tz BETWEEN ${start} AND ${end}`,
        sql<PendingStatusRow>`SELECT
            encode(header_hash, 'hex') AS header_hash,
            status
          FROM pending_block_finalizations
          WHERE created_at BETWEEN ${start} AND ${end}
            AND status <> 'finalized'`,
      ],
      { concurrency: "unbounded" },
    );

    const terminalByTx = new Map(
      admissionRows.flatMap((row) =>
        row.terminal_at === null
          ? []
          : [[normalizeHash(row.tx_hash), row.terminal_at] as const],
      ),
    );
    const commitConfirmByTx = new Map(
      commitRows.flatMap((row) => {
        const observedAt = isoFromEpochMs(row.observed_confirmed_at_ms);
        return observedAt === null
          ? []
          : [[normalizeHash(row.tx_hash), new Date(observedAt)] as const];
      }),
    );
    const commitRowsByHeader = [
      ...new Map(commitRows.map((row) => [row.header_hash, row])).values(),
    ];
    const acceptedRows = admissionRows.filter(
      (row) => row.status === "accepted" && row.terminal_at !== null,
    );
    const mempoolAllRows = [...mempoolRows, ...processedRows].sort(
      (left, right) => ms(left.observed_at) - ms(right.observed_at),
    );
    const offeredCount = query.offeredCount ?? query.txHashSample?.length ?? 0;
    const mempoolResidueCount = Number(mempoolResidueRows[0]?.count ?? 0);
    const processedResidueCount = Number(processedResidueRows[0]?.count ?? 0);
    const fingerprint = yield* Effect.promise(() =>
      collectEnvironmentFingerprint({
        calibrationProofRef: query.calibrationProofRef ?? null,
        configProfile: {
          windowStart: query.windowStart,
          windowEnd: query.windowEnd,
          trimFraction: query.trimFraction,
        },
      }),
    );

    return {
      schemaVersion: 1 as const,
      window: {
        start: query.windowStart,
        end: query.windowEnd,
        trimFraction: query.trimFraction,
      },
      stages: {
        admission: buildSteadyStateStageResult({
          stage: "admission",
          rows: admissionRows,
          offeredCount,
          trimFraction: query.trimFraction,
          timestamp: (row) => row.first_seen_at,
          latencyMs: () => null,
          notes: ["arrival_rate_only_no_client_latency_cross_host"],
        }),
        validationStart: buildSteadyStateStageResult({
          stage: "validation_start",
          rows: admissionRows.filter(
            (row) => row.validation_started_at !== null,
          ),
          offeredCount: admissionRows.length,
          trimFraction: query.trimFraction,
          timestamp: (row) => row.validation_started_at,
          latencyMs: (row) =>
            row.validation_started_at === null
              ? null
              : ms(row.validation_started_at) - ms(row.first_seen_at),
        }),
        validationTerminal: buildSteadyStateStageResult({
          stage: "validation_terminal",
          rows: admissionRows.filter((row) => row.terminal_at !== null),
          offeredCount: admissionRows.length,
          trimFraction: query.trimFraction,
          timestamp: (row) => row.terminal_at,
          latencyMs: (row) =>
            row.terminal_at === null || row.validation_started_at === null
              ? null
              : ms(row.terminal_at) - ms(row.validation_started_at),
          notes: [
            `status_breakdown_accepted_${acceptedRows.length.toString()}_rejected_${admissionRows.filter((row) => row.status === "rejected").length.toString()}`,
          ],
        }),
        mempoolPersist: buildSteadyStateStageResult({
          stage: "mempool_persist",
          rows: mempoolAllRows,
          offeredCount: acceptedRows.length,
          trimFraction: query.trimFraction,
          timestamp: (row) => row.observed_at,
          latencyMs: (row) => {
            const terminalAt = terminalByTx.get(normalizeHash(row.tx_hash));
            return terminalAt === undefined
              ? null
              : ms(row.observed_at) - ms(terminalAt);
          },
        }),
        l1CommitHeader: buildSteadyStateStageResult({
          stage: "l1_commit_header",
          rows: commitRowsByHeader,
          offeredCount: commitRowsByHeader.length,
          trimFraction: query.trimFraction,
          timestamp: (row) => row.created_at,
          latencyMs: (row) => {
            const confirmedAt = isoFromEpochMs(row.observed_confirmed_at_ms);
            return confirmedAt === null
              ? null
              : Date.parse(confirmedAt) - ms(row.created_at);
          },
          notes: ["capped_by_node_design_see_THROUGHPUT-2500-TPS-PLAN"],
        }),
        l1CommitConfirm: buildSteadyStateStageResult({
          stage: "l1_commit_l2_transactions",
          rows: commitRows,
          offeredCount: acceptedRows.length,
          trimFraction: query.trimFraction,
          timestamp: (row) =>
            isoFromEpochMs(row.observed_confirmed_at_ms) === null
              ? null
              : new Date(isoFromEpochMs(row.observed_confirmed_at_ms)!),
          latencyMs: (row) => {
            const confirmedAt = isoFromEpochMs(row.observed_confirmed_at_ms);
            return confirmedAt === null
              ? null
              : Date.parse(confirmedAt) - ms(row.created_at);
          },
          notes: ["capped_by_node_design_see_THROUGHPUT-2500-TPS-PLAN"],
        }),
        immutableObservation: buildSteadyStateStageResult({
          stage: "immutable_observation",
          rows: immutableRows,
          offeredCount: commitRows.length,
          trimFraction: query.trimFraction,
          timestamp: (row) => row.observed_at,
          latencyMs: (row) => {
            const confirmedAt = commitConfirmByTx.get(normalizeHash(row.tx_hash));
            return confirmedAt === undefined
              ? null
              : ms(row.observed_at) - ms(confirmedAt);
          },
          notes: ["immutable_observation_is_not_full_finality"],
        }),
        fullFinality: buildSteadyStateStageResult({
          stage: "full_finality",
          rows: unfinalizedRows.length === 0 ? immutableRows : [],
          offeredCount: acceptedRows.length,
          trimFraction: 0,
          timestamp: (row) => row.observed_at,
          latencyMs: () => null,
          notes: [
            ...(mempoolResidueCount + processedResidueCount === 0
              ? ["db_native_drain_check_no_residue"]
              : [
                  `residue_present_${(mempoolResidueCount + processedResidueCount).toString()}`,
                ]),
            ...(unfinalizedRows.length === 0
              ? []
              : [`${unfinalizedRows.length.toString()}_headers_not_finalized`]),
          ],
        }),
      },
      fingerprint,
    };
  }).pipe(Effect.orDie);
