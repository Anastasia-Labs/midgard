import "../utils.js";

import { createHash, randomInt } from "node:crypto";
import { writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { performance } from "node:perf_hooks";

import { RejectCodes } from "@al-ft/midgard-validation/types";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  MempoolDB,
  MigrationRunner,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../../src/database/index.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";

const operatorEnabled = process.env.BENCH_PHASE1_OPERATOR === "1";
const pageSize = Number(
  process.env.BENCH_PHASE1_PAGE_SIZE ??
    process.env.MEMPOOL_RETRIEVE_PAGE_SIZE ??
    20_000,
);
const measuredRuns = Number(process.env.BENCH_PHASE1_MEASURED_RUNS ?? 40);
const warmupRuns = Number(process.env.BENCH_PHASE1_WARMUP_RUNS ?? 10);
const snapshotUpperBound = new Date("2030-01-01T00:00:00.000Z");
const rejectionRows = Number(process.env.BENCH_PHASE1_REJECTION_ROWS ?? 1_000);
const rejectionRepeats = Number(
  process.env.BENCH_PHASE1_REJECTION_REPEATS ?? 5,
);
const outputPath = resolve(
  process.env.BENCH_PHASE1_OUTPUT_PATH ??
    "tests/benchmarks/output/phase1-query-write.json",
);
const pagePayloadMode = "mempool_inline" as const;
const maxPageDepthP95Ratio = 1.1;
const minRejectionSpeedup = 10;

const p95 = (samples: readonly number[]): number =>
  [...samples].sort((left, right) => left - right)[
    Math.max(0, Math.ceil(samples.length * 0.95) - 1)
  ] ?? 0;

const median = (samples: readonly number[]): number => {
  const ordered = [...samples].sort((left, right) => left - right);
  const middle = Math.floor(ordered.length / 2);
  return ordered.length % 2 === 0
    ? ((ordered[middle - 1] ?? 0) + (ordered[middle] ?? 0)) / 2
    : (ordered[middle] ?? 0);
};

const bytes = (label: string, length: number): Buffer => {
  const chunks: Buffer[] = [];
  while (Buffer.concat(chunks).length < length) {
    chunks.push(
      createHash("sha256").update(label).update(String(chunks.length)).digest(),
    );
  }
  return Buffer.concat(chunks).subarray(0, length);
};

const measurePageSamples = (sql: SqlClient.SqlClient, depth: number) =>
  Effect.gen(function* () {
    yield* sql`TRUNCATE ${sql(MempoolDB.tableName)}, ${sql(
      TxAdmissionsDB.tableName,
    )} RESTART IDENTITY CASCADE`;
    const insertedAt = new Date("2026-07-10T00:00:00.000Z");
    for (let offset = 0; offset < depth; offset += 1_000) {
      const count = Math.min(1_000, depth - offset);
      const entries = Array.from({ length: count }, (_, index) => {
        const ordinal = offset + index;
        const txCanonicalCbor = bytes(
          `phase1-page-cbor-${ordinal.toString()}`,
          64,
        );
        return {
          txId: bytes(`phase1-page-tx-${ordinal.toString()}`, 32),
          txCanonicalCbor,
          insertedAt: new Date(
            insertedAt.getTime() + Math.floor(ordinal / 1_000),
          ),
        };
      });
      yield* sql`INSERT INTO ${sql(TxAdmissionsDB.tableName)} ${sql.insert(
        entries.map((entry) => ({
          tx_id: entry.txId,
          status: TxAdmissionsDB.Status.Accepted,
          terminal_at: entry.insertedAt,
          submit_source: "native",
        })),
      )}`;
      yield* sql`INSERT INTO ${sql(MempoolDB.tableName)} ${sql.insert(
        entries.map((entry) => ({
          tx_id: entry.txId,
          tx: entry.txCanonicalCbor,
          time_stamp_tz: entry.insertedAt,
        })),
      )}`;
    }
    yield* sql`ANALYZE ${sql(MempoolDB.tableName)}`;
    for (let warmup = 0; warmup < warmupRuns; warmup += 1) {
      yield* MempoolDB.retrievePage({
        limit: pageSize,
        upTo: snapshotUpperBound,
      });
    }
    const samples: number[] = [];
    for (let run = 0; run < measuredRuns; run += 1) {
      const startedAt = performance.now();
      const page = yield* MempoolDB.retrievePage({
        limit: pageSize,
        upTo: snapshotUpperBound,
      });
      samples.push(performance.now() - startedAt);
      expect(page.entries).toHaveLength(pageSize);
    }
    return samples;
  });

const seedValidatingRejections = (
  sql: SqlClient.SqlClient,
  leaseOwner: string,
) =>
  Effect.gen(function* () {
    yield* sql`TRUNCATE ${sql(TxRejectionsDB.tableName)}, ${sql(
      TxAdmissionsDB.tableName,
    )} CASCADE`;
    const rejected = Array.from({ length: rejectionRows }, (_, index) => ({
      txId: bytes(`phase1-rejection-${index.toString()}`, 32),
      code:
        index % 2 === 0 ? RejectCodes.InputNotFound : RejectCodes.DoubleSpend,
      detail: index % 3 === 0 ? `detail-${index.toString()}` : null,
    }));
    yield* sql`INSERT INTO ${sql(TxAdmissionsDB.tableName)} ${sql.insert(
      rejected.map((entry, index) => ({
        tx_id: entry.txId,
        arrival_seq: BigInt(index + 1),
        status: TxAdmissionsDB.Status.Validating,
        submit_source: "native",
        lease_owner: leaseOwner,
        lease_expires_at: new Date(Date.now() + 60_000),
      })),
    )}`;
    return rejected;
  });

const legacyMarkRejected = (
  sql: SqlClient.SqlClient,
  leaseOwner: string,
  rejected: readonly {
    readonly txId: Buffer;
    readonly code: string;
    readonly detail: string | null;
  }[],
) =>
  sql.withTransaction(
    Effect.gen(function* () {
      yield* sql`INSERT INTO ${sql(TxRejectionsDB.tableName)} ${sql.insert(
        rejected.map((entry) => ({
          tx_id: entry.txId,
          reject_code: entry.code,
          reject_detail: entry.detail,
        })),
      )} ON CONFLICT (tx_id) DO UPDATE SET tx_id = ${sql(
        TxRejectionsDB.tableName,
      )}.tx_id`;
      for (const entry of rejected) {
        yield* sql`UPDATE ${sql(TxAdmissionsDB.tableName)}
          SET status = 'rejected', reject_code = ${entry.code},
              reject_detail = ${entry.detail}, lease_owner = NULL,
              lease_expires_at = NULL, terminal_at = NOW(), updated_at = NOW()
          WHERE tx_id = ${entry.txId} AND status = 'validating'
            AND lease_owner = ${leaseOwner}`;
      }
    }),
  );

const measureWithRollback = <A, E, R>(
  sql: SqlClient.SqlClient,
  operation: Effect.Effect<A, E, R>,
): Effect.Effect<number, E, R> =>
  Effect.gen(function* () {
    const rollbackMarker = Symbol("phase1-benchmark-rollback");
    let elapsedMs: number | null = null;
    yield* sql
      .withTransaction(
        Effect.gen(function* () {
          const startedAt = performance.now();
          yield* operation;
          elapsedMs = performance.now() - startedAt;
          return yield* Effect.fail(rollbackMarker);
        }),
      )
      .pipe(
        Effect.catchAll((error) =>
          error === rollbackMarker ? Effect.void : Effect.fail(error as E),
        ),
      );
    if (elapsedMs === null) {
      return yield* Effect.dieMessage(
        "Phase 1 rejection benchmark did not record a measured operation",
      );
    }
    return elapsedMs;
  });

describe("Phase 1 query/write report contract", () => {
  it("pins the inline payload mode and original acceptance thresholds", () => {
    expect(pagePayloadMode).toBe("mempool_inline");
    expect(maxPageDepthP95Ratio).toBe(1.1);
    expect(minRejectionSpeedup).toBe(10);
  });
});

describe("Phase 1 real-Postgres query/write operator benchmark", () => {
  it.skipIf(!operatorEnabled)(
    "keeps first-page retrieval flat and batches 1000 per-row rejections at least 10x",
    async () => {
      const database = process.env.POSTGRES_DB ?? "";
      expect(database).toMatch(/^midgard_phase1_bench[a-z0-9_]*$/u);
      expect(Number.isSafeInteger(pageSize) && pageSize > 0).toBe(true);
      expect(Number.isSafeInteger(measuredRuns) && measuredRuns > 0).toBe(true);
      expect(Number.isSafeInteger(warmupRuns) && warmupRuns > 0).toBe(true);
      expect(Number.isSafeInteger(rejectionRows) && rejectionRows > 0).toBe(
        true,
      );
      expect(
        Number.isSafeInteger(rejectionRepeats) && rejectionRepeats >= 3,
      ).toBe(true);
      const report = await Effect.runPromise(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DROP SCHEMA public CASCADE; CREATE SCHEMA public`;
          yield* MigrationRunner.migrate({
            appVersion: "phase1-query-write-benchmark",
            actor: "phase1-query-write-benchmark",
          });
          type PageDepth = "onePage" | "threePages" | "tenPages";
          const depthByLabel: Record<PageDepth, number> = {
            onePage: pageSize,
            threePages: pageSize * 3,
            tenPages: pageSize * 10,
          };
          // Rotate every depth through every run position so JIT, GC, and
          // filesystem-cache drift cannot systematically bias the larger
          // tables. Each individual measurement still runs against a freshly
          // seeded, analyzed table and a warmed query plan.
          const pageRunOrders: readonly (readonly PageDepth[])[] = [
            ["onePage", "threePages", "tenPages"],
            ["threePages", "tenPages", "onePage"],
            ["tenPages", "onePage", "threePages"],
          ];
          const pageSamplesMs: Record<PageDepth, number[]> = {
            onePage: [],
            threePages: [],
            tenPages: [],
          };
          for (const order of pageRunOrders) {
            for (const label of order) {
              pageSamplesMs[label].push(
                ...(yield* measurePageSamples(sql, depthByLabel[label])),
              );
            }
          }
          const onePageP95Ms = p95(pageSamplesMs.onePage);
          const threePageP95Ms = p95(pageSamplesMs.threePages);
          const tenPageP95Ms = p95(pageSamplesMs.tenPages);

          const leaseOwner = "phase1-rejection-benchmark";
          const rejectedRows = yield* seedValidatingRejections(sql, leaseOwner);
          const legacySamplesMs: number[] = [];
          const batchedSamplesMs: number[] = [];
          const rejectionRunOrder: (readonly [
            "legacy" | "batched",
            "legacy" | "batched",
          ])[] = [];
          for (let repeat = 0; repeat < rejectionRepeats; repeat += 1) {
            const order =
              randomInt(2) === 0
                ? (["legacy", "batched"] as const)
                : (["batched", "legacy"] as const);
            rejectionRunOrder.push(order);
            for (const variant of order) {
              const elapsedMs =
                variant === "legacy"
                  ? yield* measureWithRollback(
                      sql,
                      legacyMarkRejected(sql, leaseOwner, rejectedRows),
                    )
                  : yield* measureWithRollback(
                      sql,
                      TxAdmissionsDB.markRejected({
                        rows: rejectedRows.map((entry) => ({
                          tx_id: entry.txId,
                        })),
                        leaseOwner,
                        rejectedTxs: rejectedRows,
                      }),
                    );
              (variant === "legacy" ? legacySamplesMs : batchedSamplesMs).push(
                elapsedMs,
              );
              const restored = yield* sql<{
                readonly validating_count: bigint;
                readonly rejection_count: bigint;
              }>`SELECT
                  (SELECT COUNT(*)::bigint
                    FROM ${sql(TxAdmissionsDB.tableName)}
                    WHERE status = 'validating'
                      AND lease_owner = ${leaseOwner}) AS validating_count,
                  (SELECT COUNT(*)::bigint
                    FROM ${sql(TxRejectionsDB.tableName)}) AS rejection_count`;
              expect(BigInt(restored[0]?.validating_count ?? -1n)).toBe(
                BigInt(rejectionRows),
              );
              expect(BigInt(restored[0]?.rejection_count ?? -1n)).toBe(0n);
            }
          }
          const legacyMedianMs = median(legacySamplesMs);
          const batchedMedianMs = median(batchedSamplesMs);
          return {
            generatedAtIso: new Date().toISOString(),
            database,
            pagePayloadMode,
            pageTimestampShape: "ties_1000",
            pageSnapshotUpperBoundIso: snapshotUpperBound.toISOString(),
            pageSize,
            measuredRuns,
            warmupRuns,
            pageMeasuredRunsPerDepth: measuredRuns * pageRunOrders.length,
            pageRunOrders,
            pageSamplesMs,
            pageP95Ms: {
              onePage: onePageP95Ms,
              threePages: threePageP95Ms,
              tenPages: tenPageP95Ms,
            },
            pageDepthRatios: {
              threeToOne: threePageP95Ms / onePageP95Ms,
              tenToOne: tenPageP95Ms / onePageP95Ms,
            },
            rejectionRows,
            rejectionRepeats,
            rejectionRunOrder,
            legacyMarkRejectedSamplesMs: legacySamplesMs,
            batchedMarkRejectedSamplesMs: batchedSamplesMs,
            legacyMarkRejectedMedianMs: legacyMedianMs,
            batchedMarkRejectedMedianMs: batchedMedianMs,
            rejectionSpeedup: legacyMedianMs / batchedMedianMs,
          };
        }).pipe(
          Effect.provide(Database.layer),
          Effect.provide(NodeConfig.layer),
        ),
      );
      await writeFile(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      expect(report.pagePayloadMode).toBe("mempool_inline");
      expect(report.pageDepthRatios.threeToOne).toBeLessThanOrEqual(
        maxPageDepthP95Ratio,
      );
      expect(report.pageDepthRatios.tenToOne).toBeLessThanOrEqual(
        maxPageDepthP95Ratio,
      );
      expect(report.rejectionSpeedup).toBeGreaterThanOrEqual(
        minRejectionSpeedup,
      );
    },
    300_000,
  );
});
