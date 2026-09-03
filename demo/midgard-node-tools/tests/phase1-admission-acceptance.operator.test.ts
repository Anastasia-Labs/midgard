import "midgard-node/tests/utils";

import { execFile, execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { createServer } from "node:http";
import { connect } from "node:net";
import { resolve } from "node:path";
import { promisify } from "node:util";

import { RejectCodes } from "@al-ft/midgard-validation";
import { HttpServer } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import {
  Cause,
  Duration,
  Effect,
  Exit,
  Fiber,
  Layer,
  Metric,
  Schedule,
} from "effect";
import {
  buildSubmitRouter,
  submitBodyReadDurationTimer,
  submitDurableAdmissionDurationTimer,
  submitHandlerLatencyTimer,
  submitNormalizeDurationTimer,
  submitResponseDurationTimer,
} from "midgard-node/commands/listen-router";
import { MigrationRunner, TxAdmissionsDB } from "midgard-node/database/index";
import {
  admissionBacklogGaugeFiber,
  refreshAdmissionBacklogGauge,
} from "midgard-node/fibers/admission-backlog-gauge";
import {
  admissionWriteBatchDurationTimer,
  admissionWriteBatchRowsHistogram,
  admissionWriteQueueDepthGauge,
  admissionWriteQueueMaxDepthGauge,
  AdmissionWriter,
} from "midgard-node/services/admission-writer";
import { NodeConfig } from "midgard-node/services/config";
import { admissionAsDefaultSqlLayer } from "midgard-node/services/database";
import { Globals } from "midgard-node/services/globals";
import { provideDatabaseLayers } from "midgard-node/tests/utils";
import { describe, expect, it } from "vitest";

import {
  evaluateStageAAdmissionGate,
  type LogicalCpuTopology,
  parseLinuxCpuTopology,
  requirePhase1AdmissionIsolation,
  type StageAAdmissionReport,
} from "../src/e2e/phase1-admission-acceptance.js";

const execFileAsync = promisify(execFile);
const operatorEnabled = process.env.PHASE1_ADMISSION_OPERATOR === "1";

type ClientStage = {
  readonly requestedRateTps: number;
  readonly requestedDurationSec: number;
  readonly measuredDurationMs: number;
  readonly offered: number;
  readonly accepted202: number;
  readonly duplicate200: number;
  readonly rejectedOrFailed: number;
  readonly acceptedTps: number;
  readonly latencyMs: { readonly p99: number | null };
};

type ClientReport = {
  readonly nodeVersion: string;
  readonly corpus: {
    readonly sha256: string;
    readonly expectedSha256: string;
  };
  readonly topology: LogicalCpuTopology & {
    readonly distinctPhysicalCoreCount: number;
  };
  readonly warmup: ClientStage;
  readonly measured: ClientStage;
};

const expandCpuList = (value: string): number[] => {
  const cpus: number[] = [];
  for (const part of value.split(",")) {
    const [startText, endText] = part.trim().split("-");
    const start = Number(startText);
    const end = endText === undefined ? start : Number(endText);
    for (let cpu = start; cpu <= end; cpu += 1) cpus.push(cpu);
  }
  return cpus;
};

const processTopology = (): LogicalCpuTopology => {
  const status = readFileSync("/proc/self/status", "utf8");
  const allowed = /^Cpus_allowed_list:\s*(.+)$/mu.exec(status)?.[1]?.trim();
  if (allowed === undefined) {
    throw new Error("/proc/self/status has no Cpus_allowed_list");
  }
  return parseLinuxCpuTopology(
    execFileSync("lscpu", ["-p=CPU,CORE,SOCKET"], { encoding: "utf8" }),
    expandCpuList(allowed),
  );
};

const waitForSocket = (
  host: string,
  port: number,
  timeoutMs: number,
): Promise<void> =>
  new Promise((resolveWait, rejectWait) => {
    const deadline = Date.now() + timeoutMs;
    const attempt = () => {
      const socket = connect({ host, port });
      socket.once("connect", () => {
        socket.destroy();
        resolveWait();
      });
      socket.once("error", (cause) => {
        socket.destroy();
        if (Date.now() >= deadline) rejectWait(cause);
        else setTimeout(attempt, 50);
      });
    };
    attempt();
  });

const runOpenLoopClient = async (
  clientCpuSet: string,
  signal: AbortSignal,
): Promise<ClientReport> => {
  const script = resolve("scripts/phase1-admission-open-loop.mjs");
  const { stdout, stderr } = await execFileAsync(
    "taskset",
    ["-c", clientCpuSet, process.execPath, script],
    {
      cwd: resolve("."),
      env: process.env,
      maxBuffer: 16 * 1024 * 1024,
      timeout: 420_000,
      signal,
    },
  );
  if (stderr.trim().length > 0) process.stderr.write(stderr);
  const line = stdout.trim().split(/\r?\n/u).at(-1);
  if (line === undefined) throw new Error("open-loop client emitted no report");
  return JSON.parse(line) as ClientReport;
};

const stageAGateReport = (
  client: ClientReport,
  serverTopology: LogicalCpuTopology,
): StageAAdmissionReport => ({
  measuredDurationMs: client.measured.measuredDurationMs,
  offered: client.measured.offered,
  accepted202: client.measured.accepted202,
  duplicate200: client.measured.duplicate200,
  rejectedOrFailed: client.measured.rejectedOrFailed,
  latencyMs: client.measured.latencyMs,
  corpus: client.corpus,
  topology: serverTopology,
});

const readSubmitMetricSnapshot = Effect.gen(function* () {
  const [
    handler,
    bodyRead,
    normalize,
    durableAdmission,
    response,
    admissionBatchDuration,
    admissionBatchRows,
    admissionQueueDepth,
    admissionQueueMaxDepth,
  ] = yield* Effect.all([
    Metric.value(submitHandlerLatencyTimer),
    Metric.value(submitBodyReadDurationTimer),
    Metric.value(submitNormalizeDurationTimer),
    Metric.value(submitDurableAdmissionDurationTimer),
    Metric.value(submitResponseDurationTimer),
    Metric.value(admissionWriteBatchDurationTimer),
    Metric.value(admissionWriteBatchRowsHistogram),
    Metric.value(admissionWriteQueueDepthGauge),
    Metric.value(admissionWriteQueueMaxDepthGauge),
  ]);
  const summarize = (state: {
    readonly count: number;
    readonly sum: number;
    readonly min: number;
    readonly max: number;
  }) => ({
    count: state.count,
    sumMs: state.sum,
    averageMs: state.count === 0 ? null : state.sum / state.count,
    minMs: state.min,
    maxMs: state.max,
  });
  const summarizeHistogram = (state: {
    readonly buckets: ReadonlyArray<readonly [number, number]>;
    readonly count: number;
    readonly sum: number;
    readonly min: number;
    readonly max: number;
  }) => {
    const p99Rank = Math.ceil(state.count * 0.99);
    return {
      ...summarize(state),
      p99UpperBound:
        state.buckets.find(([, count]) => count >= p99Rank)?.[0] ?? null,
      cumulativeBuckets: state.buckets.map(([upperBound, count]) => ({
        upperBound,
        count,
      })),
    };
  };
  return {
    handler: summarize(handler),
    bodyRead: summarize(bodyRead),
    normalize: summarize(normalize),
    durableAdmission: summarize(durableAdmission),
    response: summarize(response),
    admissionWriter: {
      batchCommitDuration: summarizeHistogram(admissionBatchDuration),
      batchRows: summarizeHistogram(admissionBatchRows),
      queueDepth: admissionQueueDepth.value,
      maxQueueDepth: admissionQueueMaxDepth.value,
    },
  };
});

describe("Phase 1 real-socket admission acceptance", () => {
  it.skipIf(!operatorEnabled)(
    "sustains the production submit router above 5k TPS on isolated PostgreSQL",
    async () => {
      const isolation = requirePhase1AdmissionIsolation(process.env);
      const clientCpuSet = process.env.PHASE1_ADMISSION_CLIENT_CPUSET ?? "";
      if (!/^\d+(?:,\d+){7}$/u.test(clientCpuSet)) {
        throw new Error(
          "PHASE1_ADMISSION_CLIENT_CPUSET must list exactly eight logical CPUs",
        );
      }
      const resultPath = resolve(
        process.env.PHASE1_ADMISSION_OPERATOR_REPORT ??
          "tests/benchmarks/output/phase1-admission-operator.json",
      );
      if (!resultPath.includes(isolation.runToken)) {
        throw new Error(
          "PHASE1_ADMISSION_OPERATOR_REPORT must include PHASE1_ADMISSION_RUN_TOKEN",
        );
      }
      const clientReportPath = resolve(
        process.env.PHASE1_ADMISSION_CLIENT_REPORT ?? "",
      );
      if (!clientReportPath.includes(isolation.runToken)) {
        throw new Error(
          "PHASE1_ADMISSION_CLIENT_REPORT must include PHASE1_ADMISSION_RUN_TOKEN",
        );
      }
      const selfCheckPath = resolve(
        process.env.PHASE1_ADMISSION_SELF_CHECK_REPORT ?? "",
      );
      const selfCheck = JSON.parse(
        readFileSync(selfCheckPath, "utf8"),
      ) as ClientReport;
      const requestedRate = Number(
        process.env.PHASE1_ADMISSION_RATE_TPS ?? "5250",
      );
      const shortProof = process.env.PHASE1_ADMISSION_SHORT_PROOF === "1";
      const diagnosticNoDrain =
        process.env.PHASE1_ADMISSION_DIAGNOSTIC_NO_DRAIN === "1";
      if (diagnosticNoDrain && !shortProof) {
        throw new Error("no-drain mode is restricted to short diagnostics");
      }
      const requiredNodeVersion =
        process.env.PHASE1_ADMISSION_REQUIRED_NODE_VERSION ?? "v22.22.2";
      expect(process.version).toBe(requiredNodeVersion);
      expect(selfCheck.nodeVersion).toBe(requiredNodeVersion);
      expect(selfCheck.measured.acceptedTps).toBeGreaterThanOrEqual(
        requestedRate * 2 * 0.98,
      );
      expect(selfCheck.measured.rejectedOrFailed).toBe(0);
      expect(selfCheck.topology.distinctPhysicalCoreCount).toBe(8);

      const serverTopology = processTopology();
      expect(serverTopology.logicalCpuIds).toHaveLength(8);
      expect(new Set(serverTopology.physicalCoreIds).size).toBe(8);
      expect(
        serverTopology.logicalCpuIds.some((cpu) =>
          expandCpuList(clientCpuSet).includes(cpu),
        ),
      ).toBe(false);

      const result = await Effect.runPromise(
        provideDatabaseLayers(
          Effect.gen(function* () {
            const baseConfig = yield* NodeConfig;
            const nodeConfig = {
              ...baseConfig,
              PORT: isolation.httpPort,
            };
            if (diagnosticNoDrain) {
              const requiredCapacity = Math.ceil(
                Number(process.env.PHASE1_ADMISSION_RATE_TPS ?? "5250") *
                  Number(process.env.PHASE1_ADMISSION_DURATION_SEC ?? "30") +
                  Number(
                    process.env.PHASE1_ADMISSION_WARMUP_RATE_TPS ?? "1000",
                  ) *
                    Number(process.env.PHASE1_ADMISSION_WARMUP_SEC ?? "5"),
              );
              if (nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG < requiredCapacity) {
                throw new Error(
                  `diagnostic MAX_DURABLE_ADMISSION_BACKLOG must be at least ${requiredCapacity.toString()}`,
                );
              }
            }
            return yield* Effect.gen(function* () {
              const sql = yield* SqlClient.SqlClient;
              const admissionWriter = yield* AdmissionWriter;
              const identity = yield* sql<{
                readonly database: string;
                readonly port: number;
              }>`SELECT current_database() AS database, inet_server_port() AS port`;
              expect(identity).toEqual([
                { database: isolation.database, port: 5432 },
              ]);
              yield* MigrationRunner.migrate({
                appVersion: "phase1-admission-acceptance",
                actor: "phase1-admission-acceptance",
              });
              const existing = yield* sql<{
                readonly count: bigint | number | string;
              }>`SELECT COUNT(*) AS count FROM tx_admissions`;
              expect(Number(existing[0]?.count ?? -1)).toBe(0);

              yield* refreshAdmissionBacklogGauge;
              const gaugeFiber = yield* Effect.forkScoped(
                admissionBacklogGaugeFiber(
                  Schedule.spaced(
                    Duration.millis(nodeConfig.ADMISSION_BACKLOG_REFRESH_MS),
                  ),
                ),
              );
              const appLayer = HttpServer.serve(
                buildSubmitRouter(Effect.void, true),
              ).pipe(Layer.provide(admissionAsDefaultSqlLayer));
              const serverLayer = Layer.provide(
                appLayer,
                NodeHttpServer.layer(createServer, {
                  port: isolation.httpPort,
                  host: isolation.httpHost,
                }),
              );
              const serverFiber = yield* Effect.forkScoped(
                Layer.launch(serverLayer),
              );
              yield* Effect.tryPromise(() =>
                waitForSocket(isolation.httpHost, isolation.httpPort, 10_000),
              );

              const drainLoops = Number(
                process.env.PHASE1_ADMISSION_DRAIN_LOOPS ?? "2",
              );
              if (drainLoops !== 1) {
                throw new Error(
                  "PHASE1_ADMISSION_DRAIN_LOOPS must be 1 to preserve the production single-active processor invariant",
                );
              }
              let drained = 0;
              const drainFibers = yield* Effect.forEach(
                diagnosticNoDrain
                  ? []
                  : Array.from({ length: drainLoops }, (_, index) => index),
                (index) =>
                  Effect.forkScoped(
                    Effect.forever(
                      Effect.gen(function* () {
                        const leaseOwner = `phase1-admission-drain-${index.toString()}`;
                        const rows = yield* TxAdmissionsDB.claimBatch({
                          limit: 1_000,
                          leaseOwner,
                          leaseDurationMs: 30_000,
                        });
                        if (rows.length === 0) {
                          yield* Effect.sleep(Duration.millis(2));
                          return;
                        }
                        yield* TxAdmissionsDB.markRejected({
                          rows,
                          leaseOwner,
                          rejectedTxs: rows.map((row) => ({
                            txId: row.tx_id,
                            code: RejectCodes.PlutusEvaluationUnavailable,
                            detail:
                              "Operator-only terminal drain after durable Stage A admission",
                          })),
                        });
                        drained += rows.length;
                      }),
                    ),
                  ),
                { concurrency: "unbounded" },
              );
              let maxBacklog = 0;
              let maxOldestQueuedAgeMs = 0;
              let pgMaxConnectionsObserved = 0;
              let pgMaxActiveConnectionsObserved = 0;
              let pgMaxWaitingConnectionsObserved = 0;
              const pgActivitySampleCounts = new Map<string, number>();
              const serverCpuBefore = process.cpuUsage();
              const serverRssBeforeBytes = process.memoryUsage().rss;
              let serverPeakRssBytes = serverRssBeforeBytes;
              const samplerFiber = yield* Effect.forkScoped(
                Effect.forever(
                  Effect.gen(function* () {
                    const [backlog, oldest, pgActivity] = yield* Effect.all([
                      TxAdmissionsDB.countBacklog,
                      TxAdmissionsDB.oldestQueuedAgeMs,
                      sql<{
                        readonly state: string;
                        readonly wait_event_type: string | null;
                        readonly wait_event: string | null;
                        readonly count: bigint | number | string;
                      }>`SELECT
                          COALESCE(state, '<null>') AS state,
                          wait_event_type,
                          wait_event,
                          COUNT(*) AS count
                        FROM pg_stat_activity
                        WHERE datname = current_database()
                          AND pid <> pg_backend_pid()
                        GROUP BY state, wait_event_type, wait_event`,
                    ]);
                    maxBacklog = Math.max(maxBacklog, Number(backlog));
                    maxOldestQueuedAgeMs = Math.max(
                      maxOldestQueuedAgeMs,
                      oldest ?? 0,
                    );
                    let connections = 0;
                    let activeConnections = 0;
                    let waitingConnections = 0;
                    for (const row of pgActivity) {
                      const count = Number(row.count);
                      connections += count;
                      if (row.state === "active") activeConnections += count;
                      if (row.wait_event_type !== null) {
                        waitingConnections += count;
                      }
                      const key = [
                        row.state,
                        row.wait_event_type ?? "<none>",
                        row.wait_event ?? "<none>",
                      ].join("/");
                      pgActivitySampleCounts.set(
                        key,
                        (pgActivitySampleCounts.get(key) ?? 0) + count,
                      );
                    }
                    pgMaxConnectionsObserved = Math.max(
                      pgMaxConnectionsObserved,
                      connections,
                    );
                    pgMaxActiveConnectionsObserved = Math.max(
                      pgMaxActiveConnectionsObserved,
                      activeConnections,
                    );
                    pgMaxWaitingConnectionsObserved = Math.max(
                      pgMaxWaitingConnectionsObserved,
                      waitingConnections,
                    );
                    serverPeakRssBytes = Math.max(
                      serverPeakRssBytes,
                      process.memoryUsage().rss,
                    );
                    yield* Effect.sleep(Duration.seconds(1));
                  }),
                ),
              );

              const clientEffect = Effect.tryPromise((signal) =>
                runOpenLoopClient(clientCpuSet, signal),
              ).pipe(
                Effect.tapErrorCause((cause) =>
                  Effect.sync(() =>
                    process.stderr.write(
                      `phase1_admission_client_failure ${Cause.pretty(cause)}\n`,
                    ),
                  ),
                ),
              );
              const drainFailure =
                drainFibers.length === 0
                  ? Effect.never
                  : Effect.all(
                      drainFibers.map((fiber) => Fiber.join(fiber)),
                      { concurrency: "unbounded" },
                    ).pipe(
                      Effect.tapErrorCause((cause) =>
                        Effect.sync(() =>
                          process.stderr.write(
                            `phase1_admission_drain_failure ${Cause.pretty(cause)}\n`,
                          ),
                        ),
                      ),
                      Effect.flatMap(() =>
                        Effect.dieMessage("all admission drain fibers exited"),
                      ),
                    );
              const serverFailure = Fiber.await(serverFiber).pipe(
                Effect.flatMap((exit) =>
                  Exit.isFailure(exit)
                    ? Effect.sync(() =>
                        process.stderr.write(
                          `phase1_admission_server_failure ${Cause.pretty(exit.cause)}\n`,
                        ),
                      ).pipe(
                        Effect.zipRight(
                          Effect.dieMessage(
                            `admission HTTP server fiber failed: ${Cause.pretty(exit.cause)}`,
                          ),
                        ),
                      )
                    : Effect.dieMessage("admission HTTP server fiber exited"),
                ),
              );
              const client = yield* Effect.raceFirst(
                clientEffect,
                Effect.raceFirst(drainFailure, serverFailure),
              );
              if (!diagnosticNoDrain) {
                for (let attempt = 0; attempt < 300; attempt += 1) {
                  if ((yield* TxAdmissionsDB.countBacklog) === 0n) break;
                  yield* Effect.sleep(Duration.millis(100));
                }
              }
              const remainingBacklog = yield* TxAdmissionsDB.countBacklog;
              if (!diagnosticNoDrain) expect(remainingBacklog).toBe(0n);
              const submitMetrics = yield* readSubmitMetricSnapshot;
              const admissionWriterStats = yield* admissionWriter.stats;
              expect(admissionWriterStats.pending).toBe(0);
              expect(admissionWriterStats.capacityUsed).toBe(0);
              expect(admissionWriterStats.waitingCapacity).toBe(0);
              expect(admissionWriterStats.queueDepth).toBe(0);
              expect(
                admissionWriterStats.lanes.every(
                  (lane) =>
                    lane.stages.input === 0 &&
                    lane.stages.prepared === 0 &&
                    lane.stages.persisting === 0 &&
                    lane.stages.completion === 0,
                ),
              ).toBe(true);
              expect(
                admissionWriterStats.shards.reduce(
                  (sum, shard) => sum + shard.rows,
                  0,
                ),
              ).toBe(client.warmup.accepted202 + client.measured.accepted202);
              expect(
                admissionWriterStats.shards.every(
                  (shard) => shard.batches > 0 && shard.rows > 0,
                ),
              ).toBe(true);
              const serverCpu = process.cpuUsage(serverCpuBefore);
              const serverProcessCpuMs =
                (serverCpu.user + serverCpu.system) / 1_000;
              yield* Fiber.interrupt(samplerFiber);
              yield* Fiber.interrupt(gaugeFiber);
              yield* Fiber.interrupt(serverFiber);
              yield* Effect.forEach(drainFibers, Fiber.interrupt, {
                concurrency: "unbounded",
                discard: true,
              });

              const statusCounts = yield* sql<{
                readonly status: string;
                readonly count: bigint | number | string;
              }>`SELECT status, COUNT(*) AS count
                FROM tx_admissions
                GROUP BY status
                ORDER BY status`;
              const databaseSize = yield* sql<{
                readonly bytes: bigint | number | string;
              }>`SELECT pg_database_size(current_database()) AS bytes`;
              const gateInput = stageAGateReport(client, serverTopology);
              const gate = evaluateStageAAdmissionGate(gateInput);
              const combined = {
                generatedAtIso: new Date().toISOString(),
                mode: diagnosticNoDrain
                  ? "diagnostic-admission-only"
                  : "asserted-production-drain",
                isolation,
                serverRuntime: { nodeVersion: process.version },
                serverTopology,
                clientTopology: client.topology,
                selfCheck,
                client,
                submitMetrics,
                admissionWriterStats,
                serverResources: {
                  processCpuMs: serverProcessCpuMs,
                  averageCpuCores:
                    serverProcessCpuMs / client.measured.measuredDurationMs,
                  rssBeforeBytes: serverRssBeforeBytes,
                  rssAfterBytes: process.memoryUsage().rss,
                  peakRssBytes: serverPeakRssBytes,
                },
                database: {
                  clientEndpoint: {
                    host: isolation.postgresHost,
                    port: isolation.postgresPort,
                  },
                  identity: identity[0],
                  sizeBytes: Number(databaseSize[0]?.bytes ?? 0),
                  statusCounts: statusCounts.map((row) => ({
                    status: row.status,
                    count: Number(row.count),
                  })),
                  drained,
                  maxBacklog,
                  maxOldestQueuedAgeMs,
                  finalBacklog: remainingBacklog.toString(),
                  postgresActivity: {
                    maxConnectionsObserved: pgMaxConnectionsObserved,
                    maxActiveConnectionsObserved:
                      pgMaxActiveConnectionsObserved,
                    maxWaitingConnectionsObserved:
                      pgMaxWaitingConnectionsObserved,
                    sampledConnectionStates: Object.fromEntries(
                      [...pgActivitySampleCounts.entries()].sort(
                        ([left], [right]) => left.localeCompare(right),
                      ),
                    ),
                  },
                },
                gate,
              };
              writeFileSync(
                resultPath,
                `${JSON.stringify(combined, null, 2)}\n`,
              );
              return combined;
            }).pipe(Effect.provideService(NodeConfig, nodeConfig));
          }).pipe(Effect.provide(Globals.Default), Effect.scoped),
        ),
      );

      if (shortProof) {
        expect(
          result.gate.reasons.filter(
            (reason) => reason !== "measured duration is below five minutes",
          ),
        ).toEqual([]);
        expect(result.client.measured.requestedDurationSec).toBeLessThan(300);
      } else {
        expect(result.gate.reasons).toEqual([]);
        expect(result.gate.passed).toBe(true);
      }
      if (diagnosticNoDrain) {
        expect(result.database.drained).toBe(0);
        expect(Number(result.database.finalBacklog)).toBe(
          result.client.warmup.accepted202 + result.client.measured.accepted202,
        );
      } else {
        expect(result.database.drained).toBe(
          result.client.warmup.accepted202 + result.client.measured.accepted202,
        );
      }
    },
    480_000,
  );
});
