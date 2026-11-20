import { createHash } from "node:crypto";
import { writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { performance } from "node:perf_hooks";

import { SqlClient } from "@effect/sql";
import { Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import { MigrationRunner, TxAdmissionsDB } from "@/database/index.js";
import { admissionWriterShardForTxId } from "@/services/admission-writer.js";
import { NodeConfig } from "@/services/config.js";
import { AdmissionSql, Database } from "@/services/database.js";

const operatorEnabled = process.env.BENCH_PHASE1_GROUP_COMMIT_OPERATOR === "1";
const runToken = process.env.BENCH_PHASE1_GROUP_COMMIT_RUN_TOKEN ?? "";
const repetitions = Number(
  process.env.BENCH_PHASE1_GROUP_COMMIT_REPETITIONS ?? 15,
);
const outputPath = resolve(
  process.env.BENCH_PHASE1_GROUP_COMMIT_OUTPUT_PATH ??
    "tests/benchmarks/output/phase1-group-commit-ab.json",
);
const laneCount = 2;
const rowsPerLane = 128;
const mergedRows = laneCount * rowsPerLane;

type Variant = "two_concurrent_128" | "one_ordered_256";

type DbStats = {
  readonly xactCommit: bigint;
  readonly xactRollback: bigint;
  readonly tuplesInserted: bigint;
  readonly tuplesUpdated: bigint;
  readonly walLsn: bigint;
};

type AdmissionFixture = {
  readonly seedRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly laneRequests: readonly (
    readonly TxAdmissionsDB.ReservedAdmissionRequest[]
  )[];
  readonly mergedRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly expectedKinds: readonly ("new" | "duplicate" | "conflict")[];
  readonly laneFirstNewRequests: readonly (
    readonly TxAdmissionsDB.ReservedAdmissionRequest[]
  )[];
  readonly mergedFirstNewRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly duplicateGroupRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
  readonly existingRequests: readonly TxAdmissionsDB.ReservedAdmissionRequest[];
};

const percentile = (samples: readonly number[], fraction: number): number => {
  const ordered = [...samples].sort((left, right) => left - right);
  return (
    ordered[Math.max(0, Math.min(ordered.length - 1, Math.ceil(ordered.length * fraction) - 1))] ??
    0
  );
};

const summarize = (samples: readonly number[]) => ({
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

const requestForLane = (
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
        submitSource: "native",
      };
    }
  }
};

const buildFixture = (): AdmissionFixture => {
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
    const existing = requestForLane(
      lane,
      `lane-${lane.toString()}:existing`,
    );
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

const classifyOutcomes = (
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

const toBigInt = (value: bigint | number | string): bigint =>
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

const resetAndSeed = (
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

const measureVariant = (
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

const expectStrictlyIncreasingArrival = (
  requests: readonly TxAdmissionsDB.ReservedAdmissionRequest[],
  arrivalByTxId: ReadonlyMap<string, bigint>,
) => {
  const arrivals = requests.map(
    (request) => arrivalByTxId.get(request.txId.toString("hex")) ?? -1n,
  );
  expect(arrivals.every((value) => value >= 0n)).toBe(true);
  expect(
    arrivals.every((value, index) => index === 0 || value > arrivals[index - 1]!),
  ).toBe(true);
};

const verifySemanticState = (
  variant: Variant,
  fixture: AdmissionFixture,
  outcomes: readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
) =>
  Effect.gen(function* () {
    expect(outcomes).toHaveLength(mergedRows);
    expect(classifyOutcomes(outcomes)).toEqual(fixture.expectedKinds);
    const sql = yield* SqlClient.SqlClient;
    const counts = yield* sql<{
      readonly admission_count: bigint | number | string;
      readonly payload_count: bigint | number | string;
    }>`SELECT
        (SELECT COUNT(*)::bigint FROM tx_admissions) AS admission_count,
        (SELECT COUNT(*)::bigint FROM tx_admission_payloads) AS payload_count`;
    expect(toBigInt(counts[0]?.admission_count ?? -1)).toBe(250n);
    expect(toBigInt(counts[0]?.payload_count ?? -1)).toBe(250n);

    for (let lane = 0; lane < laneCount; lane += 1) {
      const duplicateGroup =
        yield* TxAdmissionsDB.getByTxId(fixture.duplicateGroupRequests[lane]!.txId);
      expect(duplicateGroup?.request_count).toBe(2n);
      expect(duplicateGroup?.tx_canonical_cbor).toEqual(
        fixture.duplicateGroupRequests[lane]!.txCanonicalCbor,
      );
      const existing =
        yield* TxAdmissionsDB.getByTxId(fixture.existingRequests[lane]!.txId);
      expect(existing?.request_count).toBe(2n);
      expect(existing?.tx_canonical_cbor).toEqual(
        fixture.existingRequests[lane]!.txCanonicalCbor,
      );
    }

    const arrivalRows = yield* sql<{
      readonly tx_id_hex: string;
      readonly arrival_seq: bigint | number | string;
    }>`SELECT encode(tx_id, 'hex') AS tx_id_hex, arrival_seq
      FROM tx_admissions
      ORDER BY arrival_seq ASC`;
    const arrivalByTxId = new Map(
      arrivalRows.map((row) => [row.tx_id_hex, toBigInt(row.arrival_seq)]),
    );
    for (const laneRequests of fixture.laneFirstNewRequests) {
      expectStrictlyIncreasingArrival(laneRequests, arrivalByTxId);
    }
    if (variant === "one_ordered_256") {
      expectStrictlyIncreasingArrival(
        fixture.mergedFirstNewRequests,
        arrivalByTxId,
      );
    }
  });

const verifyRollbackParity = (
  variant: Variant,
  batchSql: SqlClient.SqlClient,
) =>
  Effect.gen(function* () {
    yield* batchSql`TRUNCATE TABLE tx_rejections, tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;
    const lanes = Array.from({ length: laneCount }, (_, lane) => {
      const requests = Array.from({ length: rowsPerLane }, (_, index) =>
        requestForLane(
          lane,
          `rollback:${variant}:lane-${lane.toString()}:${index.toString()}`,
        ),
      );
      requests[0] = requestForLane(
        lane,
        `rollback:${variant}:lane-${lane.toString()}:invalid`,
        31,
      );
      return requests;
    });
    if (variant === "two_concurrent_128") {
      const exits = yield* Effect.all(
        lanes.map((requests) =>
          Effect.exit(TxAdmissionsDB.admitReservedBatch(requests)),
        ),
        { concurrency: "unbounded" },
      );
      expect(exits).toHaveLength(2);
      expect(exits.every(Exit.isFailure)).toBe(true);
    } else {
      const merged = Array.from({ length: rowsPerLane }, (_, index) =>
        lanes.flatMap((requests) => requests[index]!),
      ).flat();
      expect(merged).toHaveLength(mergedRows);
      expect(
        Exit.isFailure(
          yield* Effect.exit(TxAdmissionsDB.admitReservedBatch(merged)),
        ),
      ).toBe(true);
    }
    const counts = yield* batchSql<{
      readonly admission_count: bigint | number | string;
      readonly payload_count: bigint | number | string;
    }>`SELECT
        (SELECT COUNT(*)::bigint FROM tx_admissions) AS admission_count,
        (SELECT COUNT(*)::bigint FROM tx_admission_payloads) AS payload_count`;
    expect(toBigInt(counts[0]?.admission_count ?? -1)).toBe(0n);
    expect(toBigInt(counts[0]?.payload_count ?? -1)).toBe(0n);
  });

describe("Phase 1 global group-commit PostgreSQL A/B diagnostic", () => {
  it.skipIf(!operatorEnabled)(
    "compares two concurrent 128-row commits with one ordered 256-row commit",
    async () => {
      const database = process.env.POSTGRES_DB ?? "";
      expect(runToken).toMatch(/^[a-z0-9_]+$/u);
      expect(database).toBe(`midgard_phase1_group_commit_${runToken}`);
      expect(process.env.POSTGRES_HOST).toMatch(/^(127\.0\.0\.1|localhost|::1)$/u);
      expect(Number(process.env.POSTGRES_PORT)).not.toBe(5_433);
      expect(Number.isSafeInteger(repetitions) && repetitions >= 9).toBe(true);
      expect(outputPath).toContain(runToken);

      const report = await Effect.runPromise(
        Effect.gen(function* () {
          const batchSql = yield* SqlClient.SqlClient;
          const observerSql = yield* AdmissionSql;
          const identity = yield* batchSql<{
            readonly database: string;
            readonly server_version: string;
          }>`SELECT current_database() AS database,
              current_setting('server_version') AS server_version`;
          expect(identity[0]?.database).toBe(database);
          expect(identity[0]?.server_version.startsWith("15.")).toBe(true);
          yield* batchSql`DROP SCHEMA public CASCADE; CREATE SCHEMA public`;
          yield* MigrationRunner.migrate({
            appVersion: "phase1-group-commit-ab",
            actor: "phase1-group-commit-ab",
          });
          const fixture = buildFixture();
          expect(fixture.mergedRequests).toHaveLength(mergedRows);
          expect(fixture.laneRequests.map((requests) => requests.length)).toEqual([
            rowsPerLane,
            rowsPerLane,
          ]);
          const requestDigest = createHash("sha256");
          for (const request of fixture.mergedRequests) {
            requestDigest.update(request.txId);
            requestDigest.update(request.txCanonicalCbor);
          }

          yield* verifyRollbackParity("two_concurrent_128", batchSql);
          yield* verifyRollbackParity("one_ordered_256", batchSql);

          const samples: {
            readonly repetition: number;
            readonly position: number;
            readonly variant: Variant;
            readonly durationMs: number;
            readonly walBytes: number;
            readonly xactCommitDelta: number;
            readonly xactRollbackDelta: number;
            readonly tuplesInsertedDelta: number;
            readonly tuplesUpdatedDelta: number;
          }[] = [];
          for (let repetition = 0; repetition < repetitions; repetition += 1) {
            const order: readonly Variant[] =
              repetition % 2 === 0
                ? ["two_concurrent_128", "one_ordered_256"]
                : ["one_ordered_256", "two_concurrent_128"];
            for (const [position, variant] of order.entries()) {
              yield* resetAndSeed(batchSql, fixture);
              const measured = yield* measureVariant(
                variant,
                fixture,
                batchSql,
                observerSql,
              );
              expect(measured.walBytes).toBeGreaterThan(0);
              expect(measured.xactCommitDelta).toBe(
                variant === "two_concurrent_128" ? 2 : 1,
              );
              expect(measured.xactRollbackDelta).toBe(0);
              yield* verifySemanticState(variant, fixture, measured.outcomes);
              samples.push({
                repetition,
                position,
                variant,
                durationMs: measured.durationMs,
                walBytes: measured.walBytes,
                xactCommitDelta: measured.xactCommitDelta,
                xactRollbackDelta: measured.xactRollbackDelta,
                tuplesInsertedDelta: measured.tuplesInsertedDelta,
                tuplesUpdatedDelta: measured.tuplesUpdatedDelta,
              });
            }
          }
          const byVariant = (variant: Variant) =>
            samples.filter((sample) => sample.variant === variant);
          const summarizeVariant = (variant: Variant) => {
            const variantSamples = byVariant(variant);
            return {
              repetitions: variantSamples.length,
              commitDurationMs: summarize(
                variantSamples.map((sample) => sample.durationMs),
              ),
              walBytes: summarize(
                variantSamples.map((sample) => sample.walBytes),
              ),
              xactCommitDelta: variantSamples.map(
                (sample) => sample.xactCommitDelta,
              ),
              xactRollbackDelta: variantSamples.map(
                (sample) => sample.xactRollbackDelta,
              ),
              tuplesInsertedDelta: variantSamples.map(
                (sample) => sample.tuplesInsertedDelta,
              ),
              tuplesUpdatedDelta: variantSamples.map(
                (sample) => sample.tuplesUpdatedDelta,
              ),
            };
          };
          return {
            generatedAtIso: new Date().toISOString(),
            database,
            serverVersion: identity[0]!.server_version,
            nodeVersion: process.version,
            runToken,
            repetitions,
            laneCount,
            rowsPerLane,
            mergedRows,
            requestSha256: requestDigest.digest("hex"),
            expectedOutcomeCounts: {
              new: fixture.expectedKinds.filter((kind) => kind === "new").length,
              duplicate: fixture.expectedKinds.filter(
                (kind) => kind === "duplicate",
              ).length,
              conflict: fixture.expectedKinds.filter(
                (kind) => kind === "conflict",
              ).length,
            },
            rollbackParity: {
              twoConcurrent128: "both lane statements failed; zero rows",
              oneOrdered256: "merged statement failed; zero rows",
            },
            runOrder: Array.from({ length: repetitions }, (_, repetition) =>
              repetition % 2 === 0
                ? ["two_concurrent_128", "one_ordered_256"]
                : ["one_ordered_256", "two_concurrent_128"],
            ),
            samples,
            summary: {
              twoConcurrent128: summarizeVariant("two_concurrent_128"),
              oneOrdered256: summarizeVariant("one_ordered_256"),
            },
          };
        }).pipe(
          Effect.provide(Database.layer),
          Effect.provide(NodeConfig.layer),
        ),
      );
      await writeFile(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      expect(report.expectedOutcomeCounts).toEqual({
        new: 248,
        duplicate: 4,
        conflict: 4,
      });
      expect(report.summary.twoConcurrent128.repetitions).toBe(repetitions);
      expect(report.summary.oneOrdered256.repetitions).toBe(repetitions);
    },
    900_000,
  );
});
