import { createHash } from "node:crypto";
import { resolve } from "node:path";
import { performance } from "node:perf_hooks";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import { TxAdmissionsDB } from "../../src/database/index.js";
import { admissionWriterShardForTxId } from "../../src/services/admission-writer.js";

export const operatorEnabled =
  process.env.BENCH_PHASE1_GROUP_COMMIT_OPERATOR === "1";

export const runToken = process.env.BENCH_PHASE1_GROUP_COMMIT_RUN_TOKEN ?? "";

export const repetitions = Number(
  process.env.BENCH_PHASE1_GROUP_COMMIT_REPETITIONS ?? 15,
);

export const outputPath = resolve(
  process.env.BENCH_PHASE1_GROUP_COMMIT_OUTPUT_PATH ??
    "tests/benchmarks/output/phase1-group-commit-ab.json",
);

export const laneCount = 2;

export const rowsPerLane = 128;

export const mergedRows = laneCount * rowsPerLane;

export type Variant = "two_concurrent_128" | "one_ordered_256";

type DbStats = {
  readonly xactCommit: bigint;
  readonly xactRollback: bigint;
  readonly tuplesInserted: bigint;
  readonly tuplesUpdated: bigint;
  readonly walLsn: bigint;
};

export type AdmissionFixture = {
  readonly seedRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly laneRequests: readonly (readonly TxAdmissionsDB.ReservedAdmissionRequest[])[];
  readonly mergedRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly expectedKinds: readonly ("new" | "duplicate" | "conflict")[];
  readonly laneFirstNewRequests: readonly (readonly TxAdmissionsDB.ReservedAdmissionRequest[])[];
  readonly mergedFirstNewRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly duplicateGroupRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly existingRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
};

const percentile = (samples: readonly number[], fraction: number): number => {
  const ordered = [...samples].sort((left, right) => left - right);
  return (
    ordered[
      Math.max(
        0,
        Math.min(ordered.length - 1, Math.ceil(ordered.length * fraction) - 1),
      )
    ] ?? 0
  );
};

export const summarize = (samples: readonly number[]) => ({
  count: samples.length,
  p50: percentile(samples, 0.5),
  p95: percentile(samples, 0.95),
  p99: percentile(samples, 0.99),
  min: samples.length === 0 ? 0 : Math.min(...samples),
  max: samples.length === 0 ? 0 : Math.max(...samples),
});

const deterministicBytes = (label: string, length: number): Buffer => {
  const chunks: Buffer[] = [];
  let generated = 0;
  for (let index = 0; generated < length; index += 1) {
    const chunk = createHash("sha256")
      .update("phase1-group-commit-ab")
      .update("\0")
      .update(label)
      .update("\0")
      .update(index.toString())
      .digest();
    chunks.push(chunk);
    generated += chunk.length;
  }
  return Buffer.concat(chunks).subarray(0, length);
};

export const requestForLane = (
  lane: number,
  label: string,
  txIdLength = 32,
): TxAdmissionsDB.ReservedAdmissionRequest => {
  for (let suffix = 0; ; suffix += 1) {
    const txId = deterministicBytes(
      `${label}:tx-id:${suffix.toString()}`,
      txIdLength,
    );
    if (admissionWriterShardForTxId(txId, laneCount) === lane) {
      return {
        txId,
        txCanonicalCbor: deterministicBytes(`${label}:canonical`, 96),
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
        submitSource: "native",
      };
    }
  }
};

export const buildFixture = (): AdmissionFixture => {
  const seedRequests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  const laneRequests: TxAdmissionsDB.ReservedAdmissionRequest[][] = [];
  const laneExpectedKinds: ("new" | "duplicate" | "conflict")[][] = [];
  const laneFirstNewRequests: TxAdmissionsDB.ReservedAdmissionRequest[][] = [];
  const duplicateGroupRequests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  const existingRequests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  for (let lane = 0; lane < laneCount; lane += 1) {
    const duplicateGroup = requestForLane(
      lane,
      `lane-${lane.toString()}:duplicate-group`,
    );
    const duplicateGroupConflict = {
      ...duplicateGroup,
      txCanonicalCbor: deterministicBytes(
        `lane-${lane.toString()}:duplicate-group-conflict`,
        96,
      ),
    };
    const existing = requestForLane(lane, `lane-${lane.toString()}:existing`);
    const existingConflict = {
      ...existing,
      txCanonicalCbor: deterministicBytes(
        `lane-${lane.toString()}:existing-conflict`,
        96,
      ),
    };
    const requests = [
      duplicateGroup,
      { ...duplicateGroup },
      duplicateGroupConflict,
      existingConflict,
      { ...existing },
    ];
    const firstNew = [duplicateGroup];
    while (requests.length < rowsPerLane) {
      const unique = requestForLane(
        lane,
        `lane-${lane.toString()}:unique-${requests.length.toString()}`,
      );
      requests.push(unique);
      firstNew.push(unique);
    }
    seedRequests.push(existing);
    laneRequests.push(requests);
    laneExpectedKinds.push([
      "new",
      "duplicate",
      "conflict",
      "conflict",
      "duplicate",
      ...Array.from({ length: rowsPerLane - 5 }, () => "new" as const),
    ]);
    laneFirstNewRequests.push(firstNew);
    duplicateGroupRequests.push(duplicateGroup);
    existingRequests.push(existing);
  }

  const mergedRequests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  const expectedKinds: ("new" | "duplicate" | "conflict")[] = [];
  const mergedFirstNewRequests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  for (let index = 0; index < rowsPerLane; index += 1) {
    for (let lane = 0; lane < laneCount; lane += 1) {
      mergedRequests.push(laneRequests[lane]![index]!);
      const kind = laneExpectedKinds[lane]![index]!;
      expectedKinds.push(kind);
      if (kind === "new") {
        mergedFirstNewRequests.push(laneRequests[lane]![index]!);
      }
    }
  }
  return {
    seedRequests,
    laneRequests,
    mergedRequests,
    expectedKinds,
    laneFirstNewRequests,
    mergedFirstNewRequests,
    duplicateGroupRequests,
    existingRequests,
  };
};

export const classifyOutcomes = (
  outcomes: readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
): readonly ("new" | "duplicate" | "conflict")[] =>
  outcomes.map((outcome) =>
    outcome._tag === "Conflict" ? "conflict" : outcome.result.kind,
  );

const lsnToBigInt = (value: string): bigint => {
  const [high, low] = value.split("/");
  if (high === undefined || low === undefined) {
    throw new Error(`Invalid PostgreSQL WAL LSN ${JSON.stringify(value)}`);
  }
  return (BigInt(`0x${high}`) << 32n) + BigInt(`0x${low}`);
};

export const toBigInt = (value: bigint | number | string): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

const readDbStats = (observerSql: SqlClient.SqlClient) =>
  Effect.gen(function* () {
    yield* observerSql`SELECT pg_stat_clear_snapshot()`;
    const rows = yield* observerSql<{
      readonly xact_commit: bigint | number | string;
      readonly xact_rollback: bigint | number | string;
      readonly tup_inserted: bigint | number | string;
      readonly tup_updated: bigint | number | string;
      readonly wal_lsn: string;
    }>`SELECT
        stats.xact_commit,
        stats.xact_rollback,
        stats.tup_inserted,
        stats.tup_updated,
        pg_current_wal_insert_lsn()::text AS wal_lsn
      FROM pg_stat_database stats
      WHERE stats.datname = current_database()`;
    expect(rows).toHaveLength(1);
    const row = rows[0]!;
    return {
      xactCommit: toBigInt(row.xact_commit),
      xactRollback: toBigInt(row.xact_rollback),
      tuplesInserted: toBigInt(row.tup_inserted),
      tuplesUpdated: toBigInt(row.tup_updated),
      walLsn: lsnToBigInt(row.wal_lsn),
    } satisfies DbStats;
  });

export const resetAndSeed = (
  sql: SqlClient.SqlClient,
  fixture: AdmissionFixture,
) =>
  Effect.gen(function* () {
    yield* sql`TRUNCATE TABLE tx_rejections, tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;
    const seeded = yield* TxAdmissionsDB.admitReservedBatch(
      fixture.seedRequests,
    );
    expect(classifyOutcomes(seeded)).toEqual(["new", "new"]);
  });

const runVariant = (variant: Variant, fixture: AdmissionFixture) =>
  variant === "two_concurrent_128"
    ? Effect.all(
        fixture.laneRequests.map((requests) =>
          TxAdmissionsDB.admitReservedBatch(requests),
        ),
        { concurrency: "unbounded" },
      ).pipe(
        Effect.map((laneOutcomes) => {
          const merged: TxAdmissionsDB.ReservedAdmissionOutcome[] = [];
          for (let index = 0; index < rowsPerLane; index += 1) {
            for (let lane = 0; lane < laneCount; lane += 1) {
              merged.push(laneOutcomes[lane]![index]!);
            }
          }
          return merged;
        }),
      )
    : TxAdmissionsDB.admitReservedBatch(fixture.mergedRequests);

export const measureVariant = (
  variant: Variant,
  fixture: AdmissionFixture,
  batchSql: SqlClient.SqlClient,
  observerSql: SqlClient.SqlClient,
) =>
  observerSql.withTransaction(
    Effect.gen(function* () {
      const before = yield* readDbStats(observerSql);
      const startedAt = performance.now();
      const outcomes = yield* runVariant(variant, fixture).pipe(
        Effect.provideService(SqlClient.SqlClient, batchSql),
      );
      const durationMs = performance.now() - startedAt;
      const after = yield* readDbStats(observerSql);
      return {
        outcomes,
        durationMs,
        walBytes: Number(after.walLsn - before.walLsn),
        xactCommitDelta: Number(after.xactCommit - before.xactCommit),
        xactRollbackDelta: Number(after.xactRollback - before.xactRollback),
        tuplesInsertedDelta: Number(
          after.tuplesInserted - before.tuplesInserted,
        ),
        tuplesUpdatedDelta: Number(after.tuplesUpdated - before.tuplesUpdated),
      };
    }),
  );

export const expectStrictlyIncreasingArrival = (
  requests: readonly TxAdmissionsDB.ReservedAdmissionRequest[],
  arrivalByTxId: ReadonlyMap<string, bigint>,
) => {
  const arrivals = requests.map(
    (request) => arrivalByTxId.get(request.txId.toString("hex")) ?? -1n,
  );
  expect(arrivals.every((value) => value >= 0n)).toBe(true);
  expect(
    arrivals.every(
      (value, index) => index === 0 || value > arrivals[index - 1]!,
    ),
  ).toBe(true);
};
