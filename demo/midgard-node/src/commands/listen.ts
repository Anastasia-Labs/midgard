/**
 * Runtime entrypoint for the long-running midgard node process.
 * This module wires startup invariants, the HTTP server, and background fibers,
 * but should stay free of endpoint logic and other domain-specific details.
 */
export { extractStateQueueErrorCode } from "@/commands/listen-response.js";

import { InitDB } from "@/database/index.js";
import {
  Database,
  DatabaseInitializationError,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { buildListenRouter } from "@/commands/listen-router.js";
import {
  ensureProtocolInitializedOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "@/commands/listen-startup.js";
import { shouldRunGenesisOnStartup } from "@/commands/startup-policy.js";
import { fetchAndInsertDepositUTxOs, fetchAndInsertDepositUTxOsFiber, blockConfirmationFiber, blockCommitmentFiber, mergeFiber, monitorMempoolFiber, retentionSweeperFiber, txQueueProcessorFiber } from "@/fibers/index.js";
import { QueuedTxPayload } from "@/validation/index.js";
import * as Genesis from "@/genesis.js";
import { NodeSdk } from "@effect/opentelemetry";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { Cause, Duration, Effect, Layer, Queue, Schedule, pipe } from "effect";
import { HttpServer } from "@effect/platform";
import { createServer } from "node:http";
import { NodeHttpServer } from "@effect/platform-node";
import { DatabaseError } from "@/database/utils/common.js";

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
  DatabaseError | DatabaseInitializationError,
  NodeConfig | Database | MidgardContracts | Lucid | Globals
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;

    const txQueue = yield* Queue.dropping<QueuedTxPayload>(
      Math.max(1, nodeConfig.MAX_SUBMIT_QUEUE_SIZE),
    );

    yield* InitDB.program.pipe(Effect.provide(Database.layer));
    yield* ensureProtocolInitializedOnStartup;
    yield* seedLatestLocalBlockBoundaryOnStartup;
    yield* fetchAndInsertDepositUTxOs.pipe(
      Effect.tapError((e) =>
        Effect.logWarning(
          `Startup deposit catch-up failed: ${JSON.stringify(e)}`,
        ),
      ),
      Effect.catchAll(() => Effect.void),
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
        HttpServer.serve(buildListenRouter(txQueue)),
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
        retentionSweeperFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_RETENTION_SWEEPS),
        ),
        mergeFiber(mkSchedule(nodeConfig.WAIT_BETWEEN_MERGE_TXS)),
        withMonitoring ? monitorMempoolFiber(mkSchedule(1000)) : Effect.void,
        txQueueProcessorFiber(mkSchedule(500), txQueue, withMonitoring),
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
