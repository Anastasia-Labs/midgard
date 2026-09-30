import "node:fs";
import "node:os";
import "node:path";
import "node:perf_hooks";
import "node:url";
import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-validation";
import "@effect/sql";
import "effect";
import "vitest";
import "../../../midgard-validation/tests/validation-fixtures.js";
import "../../src/database/index.js";
import "../../src/services/validation-pool.js";
import "../../src/workers/utils/validation-pool.js";
import "../utils.js";
import "./phase2-cpu-topology.js";
import "./validation-worker-isolation.run-pool-phase-a.js";

import fs from "node:fs";
import { availableParallelism, cpus, hostname } from "node:os";
import { resolve } from "node:path";
import { performance } from "node:perf_hooks";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  processedTxFromValidatedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  MempoolDB,
  MempoolLedgerDB,
  MigrationRunner,
  TxAdmissionsDB,
} from "../../src/database/index.js";
import { FixedValidationWorkerPool } from "../../src/services/validation-pool.js";
import { provideDatabaseLayers } from "../utils.js";
import {
  readPhase2ContainerIdentity,
  readPhase2CpuTopology,
} from "./phase2-cpu-topology.js";
import {
  assertGate,
  assertLeakSoak,
  batchSize,
  benchmarkDatabaseNamePattern,
  buildCorpus,
  chunkSize,
  durationMs,
  expectedNodeImage,
  expectedNodeImageId,
  normalizePhaseB,
  outputPath,
  percentile,
  phaseAConfig,
  poolSize,
  quick,
  runDatabaseDiagnostic,
  runPoolPhaseA,
  steadyStateWarmupMs,
  targetTps,
  workerEntry,
} from "./validation-worker-isolation.run-pool-phase-a.js";

describe("Phase 2 validation-worker isolation benchmark", () => {
  it(
    "measures the valid-signed CPU validation diagnostic",
    async () => {
      if (assertGate) {
        throw new Error(
          "BENCH_ASSERT_PHASE2 is only supported by the sustained real-Postgres Stage B operator benchmark",
        );
      }
      const cpuTopology = await readPhase2CpuTopology();
      const containerIdentity = assertLeakSoak
        ? await readPhase2ContainerIdentity(
            expectedNodeImage,
            cpuTopology.logicalCpuIds,
          )
        : undefined;
      const containerIdentityProved =
        containerIdentity?.proved === true &&
        expectedNodeImage === "node:22.22.2" &&
        /^sha256:[0-9a-f]{64}$/u.test(expectedNodeImageId) &&
        containerIdentity.imageId === expectedNodeImageId &&
        process.version === "v22.22.2";
      if (
        assertLeakSoak &&
        (durationMs !== 86_400_000 ||
          steadyStateWarmupMs !== 300_000 ||
          !cpuTopology.pinnedEightCore ||
          !containerIdentityProved ||
          poolSize !== 6 ||
          batchSize !== 512 ||
          chunkSize !== 64 ||
          targetTps !== 2_500)
      ) {
        throw new Error(
          "BENCH_ASSERT_PHASE2_LEAK_SOAK requires an exact 5m steady-state warmup followed by an exact requested 24h measured window in a proved node:22.22.2 AutoRemove container whose immutable image ID exactly matches BENCH_PHASE2_NODE_IMAGE_ID, eight pinned physical cores, six workers, batch size 512, chunk size 64, and target 2500 TPS",
        );
      }
      if (!Number.isFinite(steadyStateWarmupMs) || steadyStateWarmupMs < 0) {
        throw new Error(
          "BENCH_PHASE2_STEADY_STATE_WARMUP_MS must be a non-negative finite number",
        );
      }
      expect(fs.existsSync(workerEntry)).toBe(true);
      const { queued, preState } = buildCorpus();
      const inlineStartedAt = performance.now();
      const inline = await Effect.runPromise(
        runPhaseAValidation(queued, phaseAConfig),
      );
      const inlineMs = performance.now() - inlineStartedAt;
      expect(inline.rejected).toHaveLength(0);
      const inlinePhaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(inline.accepted, preState, {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
        }),
      );
      expect(inlinePhaseB.rejected).toHaveLength(0);

      const pool = new FixedValidationWorkerPool(
        poolSize,
        poolSize * 4,
        30_000,
        workerEntry,
        { config: phaseAConfig, signatureVerifier: "node" },
      );
      const batchDurations: number[] = [];
      const phaseADurations: number[] = [];
      let accepted = 0;
      let rejected = 0;
      let serializedMs = 0;
      let phaseAMs = 0;
      const rssSamples: Array<{
        readonly elapsedMs: number;
        readonly rssBytes: number;
        readonly processRssPerWorkerAverageBytes: number;
      }> = [];
      const workerMemorySamples: Array<{
        readonly elapsedMs: number;
        readonly workers: Awaited<
          ReturnType<FixedValidationWorkerPool["workerMemoryStatistics"]>
        >;
      }> = [];
      let rssBaselineBytes = 0;
      let rssFinalBytes = 0;
      let wallMs = 0;
      let gateStartedAt = 0;
      let steadyStateWarmupMsObserved = 0;
      let steadyStateWarmupAccepted = 0;
      let steadyStateWarmupRejected = 0;
      let steadyStateWarmupBatches = 0;
      let rssSampleTimer: NodeJS.Timeout | undefined;
      let workerMemorySampleChain = Promise.resolve();
      const captureWorkerMemory = async (elapsedMs?: number): Promise<void> => {
        const workers = await pool.workerMemoryStatistics();
        workerMemorySamples.push({
          elapsedMs: elapsedMs ?? performance.now() - gateStartedAt,
          workers,
        });
      };
      try {
        await pool.start();
        const warmPhaseA = await runPoolPhaseA(pool, queued);
        const warmPhaseB = await Effect.runPromise(
          runPhaseBValidationWithPatch(warmPhaseA.result.accepted, preState, {
            nowCardanoSlotNo: 0n,
            bucketConcurrency: poolSize,
          }),
        );
        expect(warmPhaseA.result.rejected).toStrictEqual(inline.rejected);
        expect(normalizePhaseB(warmPhaseB)).toStrictEqual(
          normalizePhaseB(inlinePhaseB),
        );
        if (steadyStateWarmupMs > 0) {
          const steadyStateWarmupStartedAt = performance.now();
          do {
            const warmupWorker = await runPoolPhaseA(pool, queued);
            expect(
              warmupWorker.result.accepted.map((tx) =>
                tx.ledgerTx.txId.toString("hex"),
              ),
            ).toStrictEqual(
              inline.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
            );
            const steadyStatePhaseB = await Effect.runPromise(
              runPhaseBValidationWithPatch(
                warmupWorker.result.accepted,
                preState,
                {
                  nowCardanoSlotNo: 0n,
                  bucketConcurrency: poolSize,
                },
              ),
            );
            expect(normalizePhaseB(steadyStatePhaseB)).toStrictEqual(
              normalizePhaseB(inlinePhaseB),
            );
            steadyStateWarmupAccepted += steadyStatePhaseB.accepted.length;
            steadyStateWarmupRejected +=
              warmupWorker.result.rejected.length +
              steadyStatePhaseB.rejected.length;
            steadyStateWarmupBatches += 1;
            if (assertLeakSoak) {
              const targetElapsedMs =
                (steadyStateWarmupAccepted / targetTps) * 1_000;
              const actualElapsedMs =
                performance.now() - steadyStateWarmupStartedAt;
              const waitMs = targetElapsedMs - actualElapsedMs;
              if (waitMs > 0) {
                await new Promise((resolveWait) =>
                  setTimeout(resolveWait, waitMs),
                );
              }
            }
            steadyStateWarmupMsObserved =
              performance.now() - steadyStateWarmupStartedAt;
          } while (steadyStateWarmupMsObserved < steadyStateWarmupMs);
        }
        rssBaselineBytes = process.memoryUsage().rss;
        rssSamples.push({
          elapsedMs: 0,
          rssBytes: rssBaselineBytes,
          processRssPerWorkerAverageBytes: rssBaselineBytes / poolSize,
        });
        await captureWorkerMemory(0);
        gateStartedAt = performance.now();
        rssSampleTimer = setInterval(() => {
          const elapsedMs = performance.now() - gateStartedAt;
          const rssBytes = process.memoryUsage().rss;
          rssSamples.push({
            elapsedMs,
            rssBytes,
            processRssPerWorkerAverageBytes: rssBytes / poolSize,
          });
          workerMemorySampleChain = workerMemorySampleChain.then(() =>
            captureWorkerMemory(),
          );
        }, 60_000);
        do {
          const batchStartedAt = performance.now();
          const phaseAStartedAt = performance.now();
          const worker = await runPoolPhaseA(pool, queued);
          const phaseADuration = performance.now() - phaseAStartedAt;
          phaseAMs += phaseADuration;
          phaseADurations.push(phaseADuration);
          serializedMs += worker.serializeMs + worker.deserializeMs;
          expect(
            worker.result.accepted.map((tx) =>
              tx.ledgerTx.txId.toString("hex"),
            ),
          ).toStrictEqual(
            inline.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
          );
          const phaseB = await Effect.runPromise(
            runPhaseBValidationWithPatch(worker.result.accepted, preState, {
              nowCardanoSlotNo: 0n,
              bucketConcurrency: poolSize,
            }),
          );
          expect(normalizePhaseB(phaseB)).toStrictEqual(
            normalizePhaseB(inlinePhaseB),
          );
          accepted += phaseB.accepted.length;
          rejected += worker.result.rejected.length + phaseB.rejected.length;
          batchDurations.push(performance.now() - batchStartedAt);
          if (assertLeakSoak) {
            const targetElapsedMs = (accepted / targetTps) * 1_000;
            const actualElapsedMs = performance.now() - gateStartedAt;
            const waitMs = targetElapsedMs - actualElapsedMs;
            if (waitMs > 0) {
              await new Promise((resolveWait) =>
                setTimeout(resolveWait, waitMs),
              );
            }
          }
        } while (performance.now() - gateStartedAt < durationMs);
        wallMs = performance.now() - gateStartedAt;
        if (rssSampleTimer !== undefined) {
          clearInterval(rssSampleTimer);
          rssSampleTimer = undefined;
        }
        await workerMemorySampleChain;
        await captureWorkerMemory(wallMs);
        rssFinalBytes = process.memoryUsage().rss;
      } finally {
        if (rssSampleTimer !== undefined) clearInterval(rssSampleTimer);
        await workerMemorySampleChain;
        await pool.close();
      }

      rssSamples.push({
        elapsedMs: wallMs,
        rssBytes: rssFinalBytes,
        processRssPerWorkerAverageBytes: rssFinalBytes / poolSize,
      });
      const rssGrowthRatio =
        Math.max(0, rssFinalBytes - rssBaselineBytes) /
        Math.max(1, rssBaselineBytes);
      const baselineWorkerMemory = workerMemorySamples[0]?.workers ?? [];
      const finalWorkerMemory = workerMemorySamples.at(-1)?.workers ?? [];
      const workerMemoryGrowth = baselineWorkerMemory.map((baseline) => {
        const final = finalWorkerMemory.find(
          (candidate) => candidate.workerIndex === baseline.workerIndex,
        );
        const stableIdentity = final?.threadId === baseline.threadId;
        const growthRatio =
          final === undefined
            ? Number.POSITIVE_INFINITY
            : Math.max(
                0,
                final.comparableFootprintBytes -
                  baseline.comparableFootprintBytes,
              ) / Math.max(1, baseline.comparableFootprintBytes);
        return {
          workerIndex: baseline.workerIndex,
          baselineThreadId: baseline.threadId,
          finalThreadId: final?.threadId ?? -1,
          stableIdentity,
          baselineComparableFootprintBytes: baseline.comparableFootprintBytes,
          finalComparableFootprintBytes: final?.comparableFootprintBytes ?? 0,
          growthRatio,
        };
      });
      const everyWorkerMemoryGrowthUnderTenPercent =
        workerMemoryGrowth.length === poolSize &&
        workerMemoryGrowth.every(
          (worker) => worker.stableIdentity && worker.growthRatio < 0.1,
        );
      const cpuValidationTps = accepted / (wallMs / 1_000);
      const steadyStateWarmupAcceptedTps =
        steadyStateWarmupAccepted /
        Math.max(0.001, steadyStateWarmupMsObserved / 1_000);
      const phaseAValidationTps = accepted / (phaseAMs / 1_000);
      const p99BatchMs = percentile(batchDurations, 0.99);
      const firstPoolPhaseAMs = phaseADurations[0] ?? Number.POSITIVE_INFINITY;
      const phaseASpeedup = inlineMs / firstPoolPhaseAMs;
      const serializationRatio = serializedMs / Math.max(1, phaseAMs);
      const pinnedEightCore = cpuTopology.pinnedEightCore && poolSize === 6;
      const report = {
        generatedAtIso: new Date().toISOString(),
        host: hostname(),
        cpuModel: cpus()[0]?.model ?? "unknown",
        availableParallelism: availableParallelism(),
        nodeVersion: process.version,
        expectedNodeImage,
        expectedNodeImageId,
        nodeImage: containerIdentity?.image ?? expectedNodeImage,
        nodeImageId: containerIdentity?.imageId,
        containerIdentity,
        containerIdentityProved,
        affinityLogicalCpuIds: cpuTopology.logicalCpuIds,
        affinityPhysicalCoreIds: cpuTopology.physicalCoreIds,
        pinnedEightCore,
        quick,
        batchSize,
        poolSize,
        signatureVerifier: "node",
        chunkSize,
        durationMsRequested: durationMs,
        durationMsObserved: wallMs,
        accepted,
        rejected,
        batches: batchDurations.length,
        cpuValidationTps,
        phaseAValidationTps,
        averagePhaseAMs: phaseAMs / Math.max(1, batchDurations.length),
        acceptedTps: cpuValidationTps,
        p99BatchMs,
        inlinePhaseAMs: inlineMs,
        phaseASpeedup,
        serializationRatio,
        targetTps: assertLeakSoak ? targetTps : null,
        steadyStateWarmupMsRequested: steadyStateWarmupMs,
        steadyStateWarmupMsObserved,
        steadyStateWarmupAccepted,
        steadyStateWarmupRejected,
        steadyStateWarmupBatches,
        steadyStateWarmupAcceptedTps,
        memoryMeasurementExcludesWarmup: true,
        verdictMatchesInline: true,
        rssBaselineBytes,
        rssFinalBytes,
        rssGrowthRatio,
        rssSamples,
        workerMemorySamples,
        workerMemoryGrowth,
        everyWorkerMemoryGrowthUnderTenPercent,
        leakSoakGateAsserted:
          assertLeakSoak &&
          containerIdentityProved &&
          pinnedEightCore &&
          batchSize === 512 &&
          chunkSize === 64 &&
          targetTps === 2_500 &&
          durationMs === 86_400_000 &&
          steadyStateWarmupMs === 300_000 &&
          steadyStateWarmupMsObserved >= steadyStateWarmupMs &&
          steadyStateWarmupAccepted === batchSize * steadyStateWarmupBatches &&
          steadyStateWarmupRejected === 0 &&
          steadyStateWarmupAcceptedTps >= targetTps * 0.999 &&
          wallMs >= 86_400_000 &&
          accepted === batchSize * batchDurations.length &&
          cpuValidationTps >= targetTps &&
          rejected === 0 &&
          rssGrowthRatio < 0.1 &&
          everyWorkerMemoryGrowthUnderTenPercent,
        gateAsserted: false,
      };
      fs.mkdirSync(resolve(outputPath, ".."), { recursive: true });
      fs.writeFileSync(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      console.log(JSON.stringify(report));
      if (assertLeakSoak) {
        expect(rejected).toBe(0);
        expect(wallMs).toBeGreaterThanOrEqual(86_400_000);
        expect(steadyStateWarmupMsObserved).toBeGreaterThanOrEqual(
          steadyStateWarmupMs,
        );
        expect(cpuValidationTps).toBeGreaterThanOrEqual(2_500);
        expect(rssGrowthRatio).toBeLessThan(0.1);
        expect(everyWorkerMemoryGrowthUnderTenPercent).toBe(true);
        expect(report.leakSoakGateAsserted).toBe(true);
      }
    },
    Math.max(420_000, steadyStateWarmupMs + durationMs + 120_000),
  );

  it.skipIf(!runDatabaseDiagnostic)(
    "measures the preloaded Postgres claim through accepted persistence path",
    async () => {
      const databaseName = process.env.POSTGRES_DB ?? "";
      if (!benchmarkDatabaseNamePattern.test(databaseName)) {
        throw new Error(
          `Refusing destructive benchmark setup for POSTGRES_DB=${JSON.stringify(databaseName)}; use a midgard_phase2_bench_* database`,
        );
      }
      expect(fs.existsSync(workerEntry)).toBe(true);
      const { queued, preStateRows } = buildCorpus();
      const databaseBatchSize = Math.min(
        queued.length,
        Number(
          process.env.BENCH_PHASE2_DATABASE_BATCH_SIZE ?? (quick ? 256 : 4_096),
        ),
      );
      const databaseQueued = queued.slice(0, databaseBatchSize);
      const leaseOwner = `phase2-benchmark-${process.pid}`;
      const pool = new FixedValidationWorkerPool(
        poolSize,
        poolSize * 4,
        30_000,
        workerEntry,
        { config: phaseAConfig, signatureVerifier: "node" },
      );

      await pool.start();
      let databaseReport: {
        readonly batchSize: number;
        readonly stageBMs: number;
        readonly throughputTps: number;
        readonly acceptedRows: number;
        readonly mempoolRows: bigint;
      };
      try {
        databaseReport = await Effect.runPromise(
          provideDatabaseLayers(
            Effect.gen(function* () {
              const sql = yield* SqlClient.SqlClient;
              yield* MigrationRunner.assertCompatible;
              yield* sql`TRUNCATE TABLE
                tx_rejections,
                tx_admissions,
                mempool_tx_deltas,
                mempool,
                mempool_ledger
                RESTART IDENTITY CASCADE`;
              yield* MempoolLedgerDB.insert(
                preStateRows.slice(0, databaseBatchSize),
              );
              const inserted = yield* Effect.forEach(
                databaseQueued,
                (queuedTx) =>
                  TxAdmissionsDB.tryInsert({
                    txId: queuedTx.txId,
                    txCanonicalCbor: queuedTx.txCbor,
                    programMaterialSidecarCbor:
                      encodeMidgardCekProgramMaterialSidecar([]),
                    submitSource: "native",
                  }),
                { concurrency: 16 },
              );
              expect(inserted.every((row) => row !== null)).toBe(true);

              const stageStartedAt = performance.now();
              const claimed = yield* TxAdmissionsDB.claimBatch({
                limit: databaseBatchSize,
                leaseOwner,
                leaseDurationMs: 30_000,
              });
              expect(claimed).toHaveLength(databaseBatchSize);

              const phaseA = yield* Effect.tryPromise(() =>
                runPoolPhaseA(
                  pool,
                  claimed.map((row) => ({
                    txId: row.tx_id,
                    txCbor: row.tx_canonical_cbor,
                    arrivalSeq: row.arrival_seq,
                    createdAt: row.first_seen_at,
                  })),
                ),
              );
              expect(phaseA.result.rejected).toHaveLength(0);
              const databaseLedger = yield* MempoolLedgerDB.retrieveSpendable;
              const databasePreState = new Map(
                databaseLedger.map((entry) => [
                  entry.outref.toString("hex"),
                  entry.output,
                ]),
              );
              const phaseB = yield* runPhaseBValidationWithPatch(
                phaseA.result.accepted,
                databasePreState,
                {
                  nowCardanoSlotNo: 0n,
                  bucketConcurrency: poolSize,
                },
              );
              expect(phaseB.rejected).toHaveLength(0);
              expect(phaseB.accepted).toHaveLength(databaseBatchSize);
              yield* TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs: phaseB.accepted.map(processedTxFromValidatedTx),
              });

              const stageBMs = performance.now() - stageStartedAt;
              const acceptedRows = yield* sql<{
                readonly count: bigint | string;
              }>`SELECT COUNT(*) AS count FROM tx_admissions WHERE status = 'accepted'`;
              const mempoolRows = yield* MempoolDB.retrieveTxCount;
              return {
                batchSize: databaseBatchSize,
                stageBMs,
                throughputTps: databaseBatchSize / (stageBMs / 1_000),
                acceptedRows: Number(acceptedRows[0]?.count ?? 0),
                mempoolRows,
              };
            }),
          ),
        );
      } finally {
        await pool.close();
      }

      expect(databaseReport.acceptedRows).toBe(databaseBatchSize);
      expect(databaseReport.mempoolRows).toBe(BigInt(databaseBatchSize));
      const existing = JSON.parse(
        fs.readFileSync(outputPath, "utf8"),
      ) as Record<string, unknown>;
      fs.writeFileSync(
        outputPath,
        `${JSON.stringify(
          {
            ...existing,
            realPostgresPreloadedBatch: {
              ...databaseReport,
              mempoolRows: databaseReport.mempoolRows.toString(),
            },
          },
          null,
          2,
        )}\n`,
      );
      console.log(
        JSON.stringify({
          realPostgresPreloadedBatch: {
            ...databaseReport,
            mempoolRows: databaseReport.mempoolRows.toString(),
          },
        }),
      );
    },
    180_000,
  );
});
