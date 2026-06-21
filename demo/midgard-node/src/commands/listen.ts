/**
 * Runtime entrypoint for the long-running midgard node process.
 * This module wires startup invariants, the HTTP server, and background fibers,
 * but should stay free of endpoint logic and other domain-specific details.
 */
import { createServer } from "node:http";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { NodeSdk } from "@effect/opentelemetry";
import { HttpServer } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { Cause, Duration, Effect, Layer, pipe, Schedule } from "effect";

import { buildListenRouter } from "@/commands/listen-router.js";
import {
  ensureProtocolInitializedOnStartup,
  hydratePendingBlockFinalizationOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "@/commands/listen-startup.js";
import { shouldRunGenesisOnStartup } from "@/commands/startup-policy.js";
import { InitDB, MutationJobsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  blockCommitmentFiber,
  blockConfirmationFiber,
  fetchAndInsertDepositUTxOs,
  fetchAndInsertDepositUTxOsFiber,
  fetchAndInsertTxOrderUTxOs,
  fetchAndInsertTxOrderUTxOsFiber,
  fetchAndInsertWithdrawalUTxOs,
  fetchAndInsertWithdrawalUTxOsFiber,
  mergeFiber,
  monitorMempoolFiber,
  projectDepositsToMempoolLedger,
  projectDepositsToMempoolLedgerFiber,
  retentionSweeperFiber,
  txQueueProcessorFiber,
} from "@/fibers/index.js";
import * as Genesis from "@/genesis.js";
import {
  ConfigError,
  Database,
  DatabaseInitializationError,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "@/workers/commit-block-header/da-payload-backfill.js";

const logStartupFailure = (message: string) => (error: unknown) =>
  Effect.logError(`${message}: ${formatUnknownError(error)}`);

const isRetryableStartupProviderError = (error: unknown): boolean => {
  const message = formatUnknownError(error, {
    includeCause: true,
  }).toLowerCase();
  return (
    message.includes("failed to fetch ") ||
    message.includes("failed to query ") ||
    message.includes("fetch failed") ||
    message.includes("status code 503") ||
    message.includes("response code 503") ||
    message.includes("status 503") ||
    message.includes("service unavailable") ||
    message.includes("temporarily unavailable") ||
    message.includes("timeout") ||
    message.includes("timed out") ||
    message.includes("socket") ||
    message.includes("econnrefused") ||
    message.includes("econnreset") ||
    message.includes("rate limit") ||
    message.includes("too many requests")
  );
};

const runStartupProviderStepWithRetry = <A, E, R>(
  label: string,
  step: Effect.Effect<A, E, R>,
  options: { readonly maxAttempts: number; readonly retryDelayMs: number },
): Effect.Effect<A, E, R> =>
  Effect.gen(function* () {
    const maxAttempts = Math.max(1, Math.floor(options.maxAttempts));
    const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
    let lastError: E | undefined;

    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      const result = yield* Effect.either(step);
      if (result._tag === "Right") {
        if (attempt > 1) {
          yield* Effect.logInfo(
            `${label} became available after ${attempt.toString()} attempt(s).`,
          );
        }
        return result.right;
      }

      lastError = result.left;
      if (!isRetryableStartupProviderError(lastError)) {
        return yield* Effect.fail(lastError);
      }
      if (attempt < maxAttempts) {
        yield* Effect.logWarning(
          `${label} failed with a retryable provider error (attempt ${attempt.toString()}/${maxAttempts.toString()}); retrying in ${retryDelayMs.toString()}ms. cause=${formatUnknownError(lastError, { includeCause: true })}`,
        );
        if (retryDelayMs > 0) {
          yield* Effect.sleep(Duration.millis(retryDelayMs));
        }
      }
    }

    return yield* Effect.fail(lastError as E);
  });

/**
 * Boots the long-running Midgard node runtime.
 *
 * The effect wires database initialization, protocol startup checks, optional
 * genesis bootstrapping, the HTTP server, and the background fibers that keep
 * the node progressing.
 */
export const runNode = (
  withMonitoring?: boolean,
): Effect.Effect<
  void,
  ConfigError | DatabaseError | DatabaseInitializationError,
  NodeConfig | Database | MidgardContracts | Lucid | Globals
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;

    yield* InitDB.program.pipe(Effect.provide(Database.layer));
    yield* ensureProtocolInitializedOnStartup;
    const startupProviderRetry = {
      maxAttempts: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS,
      retryDelayMs: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS,
    } as const;
    yield* runStartupProviderStepWithRetry(
      "Startup state-queue boundary seed",
      seedLatestLocalBlockBoundaryOnStartup,
      startupProviderRetry,
    ).pipe(
      Effect.tapError(
        logStartupFailure("Startup state-queue boundary seed failed"),
      ),
      Effect.mapError(
        (e) =>
          new DatabaseInitializationError({
            message: "Startup state-queue boundary seed failed",
            cause: e,
          }),
      ),
    );
    yield* hydratePendingBlockFinalizationOnStartup;
    const unfinishedMutationJobs = yield* MutationJobsDB.retrieveUnfinished;
    if (unfinishedMutationJobs.length > 0) {
      return yield* Effect.fail(
        new DatabaseInitializationError({
          message:
            "Startup found unfinished local mutation jobs; refusing to serve until recovery is performed",
          cause: unfinishedMutationJobs.map((job) => ({
            jobId: job[MutationJobsDB.Columns.JOB_ID],
            kind: job[MutationJobsDB.Columns.KIND],
            status: job[MutationJobsDB.Columns.STATUS],
            updatedAt: job[MutationJobsDB.Columns.UPDATED_AT].toISOString(),
            lastError: job[MutationJobsDB.Columns.LAST_ERROR],
          })),
        }),
      );
    }
    yield* runStartupProviderStepWithRetry(
      "Startup deposit catch-up",
      fetchAndInsertDepositUTxOs,
      startupProviderRetry,
    ).pipe(
      Effect.tapError(logStartupFailure("Startup deposit catch-up failed")),
      Effect.mapError(
        (e) =>
          new DatabaseInitializationError({
            message: "Startup deposit catch-up failed",
            cause: e,
          }),
      ),
    );
    yield* projectDepositsToMempoolLedger.pipe(
      Effect.tapError(
        logStartupFailure("Startup deposit projection reconciliation failed"),
      ),
      Effect.mapError(
        (e) =>
          new DatabaseInitializationError({
            message: "Startup deposit projection reconciliation failed",
            cause: e,
          }),
      ),
    );
    yield* runStartupProviderStepWithRetry(
      "Startup withdrawal catch-up",
      fetchAndInsertWithdrawalUTxOs,
      startupProviderRetry,
    ).pipe(
      Effect.tapError(logStartupFailure("Startup withdrawal catch-up failed")),
      Effect.mapError(
        (e) =>
          new DatabaseInitializationError({
            message: "Startup withdrawal catch-up failed",
            cause: e,
          }),
      ),
    );
    yield* runStartupProviderStepWithRetry(
      "Startup tx-order catch-up",
      fetchAndInsertTxOrderUTxOs,
      startupProviderRetry,
    ).pipe(
      Effect.tapError(logStartupFailure("Startup tx-order catch-up failed")),
      Effect.mapError(
        (e) =>
          new DatabaseInitializationError({
            message: "Startup tx-order catch-up failed",
            cause: e,
          }),
      ),
    );
    yield* backfillMissingDaPayloadsFromFinalizedJournals({ limit: 100 }).pipe(
      Effect.tap((summary) =>
        summary.scanned === 0
          ? Effect.void
          : Effect.logInfo(
              `Startup DA payload backfill scanned=${summary.scanned.toString()},backfilled=${summary.backfilled.length.toString()},skipped=${summary.skipped.length.toString()}`,
            ),
      ),
      Effect.catchAll((error) =>
        Effect.logWarning(
          `Startup DA payload backfill skipped after error: ${formatUnknownError(error)}`,
        ),
      ),
    );

    if (
      shouldRunGenesisOnStartup({
        network: nodeConfig.NETWORK,
        runGenesisOnStartup: nodeConfig.RUN_GENESIS_ON_STARTUP,
      })
    ) {
      yield* Effect.logInfo(
        "Scheduling genesis startup program in background.",
      );
      yield* Effect.forkDaemon(
        Genesis.program.pipe(
          Effect.tapErrorCause((cause) =>
            Effect.logError(
              `Startup genesis program failed: ${Cause.pretty(cause)}`,
            ),
          ),
          Effect.catchAllCause(() => Effect.void),
        ),
      );
    } else {
      yield* Effect.logInfo(
        "Skipping genesis on startup (disabled or mainnet).",
      );
    }

    const appThread = Layer.launch(
      Layer.provide(
        HttpServer.serve(buildListenRouter(withMonitoring)),
        NodeHttpServer.layer(createServer, { port: nodeConfig.PORT }),
      ),
    );

    /**
     * Builds a fixed Effect schedule from a millisecond interval.
     */
    const mkSchedule = (millisBetweenRuns: number) =>
      Schedule.spaced(Duration.millis(millisBetweenRuns));

    const program = Effect.all(
      [
        appThread,
        blockCommitmentFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT),
        ),
        blockConfirmationFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION),
        ),
        fetchAndInsertDepositUTxOsFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
        ),
        fetchAndInsertWithdrawalUTxOsFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
        ),
        fetchAndInsertTxOrderUTxOsFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
        ),
        projectDepositsToMempoolLedgerFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
        ),
        retentionSweeperFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_RETENTION_SWEEPS),
        ),
        mergeFiber(mkSchedule(nodeConfig.WAIT_BETWEEN_MERGE_TXS)),
        withMonitoring ? monitorMempoolFiber(mkSchedule(1000)) : Effect.void,
        txQueueProcessorFiber(mkSchedule(500)),
      ],
      {
        concurrency: "unbounded",
      },
    );

    if (withMonitoring) {
      const prometheusExporter = new PrometheusExporter(
        {
          port: nodeConfig.PROM_METRICS_PORT,
        },
        () => {
          console.log(
            `Prometheus metrics available at http://0.0.0.0:${nodeConfig.PROM_METRICS_PORT}/metrics`,
          );
        },
      );

      const originalStop = prometheusExporter.stopServer;
      prometheusExporter.stopServer = async function () {
        Effect.runSync(Effect.logInfo("Prometheus exporter is stopping!"));
        return originalStop();
      };

      const MetricsLive = NodeSdk.layer(() => ({
        resource: { serviceName: "midgard-node" },
        metricReader: prometheusExporter,
        spanProcessor: new BatchSpanProcessor(
          new OTLPTraceExporter({ url: nodeConfig.OLTP_EXPORTER_URL }),
        ),
      }));

      yield* pipe(
        program,
        Effect.withSpan("midgard"),
        Effect.provide(MetricsLive),
        Effect.catchAllCause(Effect.logError),
      );
    } else {
      yield* pipe(
        program,
        Effect.withSpan("midgard"),
        Effect.catchAllCause(Effect.logError),
      );
    }
  });
