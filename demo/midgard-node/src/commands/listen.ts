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
import { SqlClient } from "@effect/sql";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import {
  Cause,
  Duration,
  Effect,
  Layer,
  Option,
  pipe,
  Ref,
  Schedule,
} from "effect";

import {
  closeDaLibp2pPublicationTransport,
  startDaLibp2pRetainedPayloadServerFromEnv,
} from "../da/libp2p-producer.js";
import {
  assertDaHardeningProviderStartup,
  prepareDaHardeningStartup,
  runDaIdentityGatedStartupSequence,
} from "../da/startup.js";
import { DaPayloadsDB, InitDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { assertPhase1AcceptCrashCheckpointConfiguration } from "../e2e/phase1-accept-crash-checkpoint.js";
import {
  admissionBacklogGaugeFiber,
  attestationTimeoutCorrectionFiber,
  blockCommitmentFiber,
  blockConfirmationFiber,
  daPublicationReconcilerFiber,
  fetchAndInsertTxOrderUTxOs,
  fetchAndInsertTxOrderUTxOsFiber,
  mergeFiber,
  monitorMempoolFiber,
  mpfPayloadAuditFiber,
  operatorWatchdogFiber,
  refreshAdmissionBacklogGauge,
  retentionSweeperFiber,
  speculativeCommitBuilderFiber,
  speculativeCommitSubmitterFiber,
  txQueueProcessorFiber,
  userEventBarrierRefresherFiber,
} from "../fibers/index.js";
import * as Genesis from "../genesis.js";
import { isRetryableProviderError } from "../provider-retry.js";
import { makeProductionEventHistoryOwner } from "../services/event-history-runtime.js";
import {
  admissionAsDefaultSqlLayer,
  AdmissionSql,
  AdmissionWriter,
  BatchSql,
  ConfigError,
  Database,
  DatabaseInitializationError,
  Globals,
  Lucid,
  mempoolLedgerCacheLayer,
  MidgardContracts,
  NodeConfig,
  validationPoolLayer,
  WriteBehind,
  writeBehindFiber,
} from "../services/index.js";
import { initializeArchitectureGOwner } from "../services/native-mpf-startup.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../workers/commit-block-header/da-payload-backfill.js";
import { buildListenRouter } from "./listen-router.js";
import {
  assertStartupMutationJobsRecoverable,
  ensureProtocolInitializedOnStartup,
  hydratePendingBlockFinalizationOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "./listen-startup.js";
import { shouldRunGenesisOnStartup } from "./startup-policy.js";

const logStartupFailure = (message: string) => (error: unknown) =>
  Effect.logError(`${message}: ${formatUnknownError(error)}`);

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
      if (!isRetryableProviderError(lastError)) {
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

const retainedPayloadServerThread = (
  retrieveByHeaderHash: (
    headerHash: Buffer,
  ) => Promise<DaPayloadsDB.Row | undefined>,
): Effect.Effect<void, never> =>
  Effect.tryPromise({
    try: () =>
      startDaLibp2pRetainedPayloadServerFromEnv({ retrieveByHeaderHash }),
    catch: (cause) => cause,
  }).pipe(
    Effect.tap((server) =>
      server.configured
        ? Effect.logInfo(
            `DA libp2p retained-payload server listening deployment_fingerprint=${server.deploymentFingerprint},local_peer_id=${server.localPeerId},listen=${server.listenMultiaddrs?.join(",") ?? ""},announce=${server.announceMultiaddrs?.join(",") ?? ""}`,
          )
        : Effect.logInfo(
            `DA libp2p retained-payload server skipped: ${server.reason ?? "not configured"}`,
          ),
    ),
    Effect.flatMap((server) =>
      server.configured
        ? Effect.never.pipe(
            Effect.ensuring(
              server.close === undefined
                ? Effect.void
                : Effect.promise(() => server.close!()).pipe(
                    Effect.catchAll((error) =>
                      Effect.logWarning(
                        `DA libp2p retained-payload server stop failed: ${formatUnknownError(error)}`,
                      ),
                    ),
                  ),
            ),
          )
        : Effect.void,
    ),
    Effect.catchAll((error) =>
      Effect.logWarning(
        `DA libp2p retained-payload server disabled after startup failure: ${formatUnknownError(error)}`,
      ),
    ),
  );

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
  | ConfigError
  | DatabaseError
  | DatabaseInitializationError
  | import("../services/validation-pool.js").ValidationWorkerError,
  | NodeConfig
  | Database
  | AdmissionSql
  | AdmissionWriter
  | BatchSql
  | import("../services/midgard-contracts.js").ContractDeploymentIdentity
  | MidgardContracts
  | Lucid
  | WriteBehind
  | Globals
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const globals = yield* Globals;

    yield* assertPhase1AcceptCrashCheckpointConfiguration;
    const startupProviderRetry = {
      maxAttempts: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS,
      retryDelayMs: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS,
    } as const;
    yield* runDaIdentityGatedStartupSequence({
      localPreflight: prepareDaHardeningStartup.pipe(
        Effect.tapError(
          logStartupFailure("Startup DA identity preflight failed"),
        ),
      ),
      initializeDatabase: InitDB.program.pipe(Effect.provide(Database.layer)),
      initializeProtocol: ensureProtocolInitializedOnStartup,
      providerAssertions: (preflight) =>
        runStartupProviderStepWithRetry(
          "Startup DA provider assertions",
          assertDaHardeningProviderStartup(preflight),
          startupProviderRetry,
        ).pipe(
          Effect.tapError(
            logStartupFailure("Startup DA provider assertions failed"),
          ),
        ),
    });

    let startupPrepared = false;
    yield* Effect.addFinalizer(() =>
      Effect.gen(function* () {
        const owned = yield* Ref.get(globals.NATIVE_MPF_OWNER);
        if (owned !== undefined) {
          yield* Effect.promise(() => owned.close());
          yield* Ref.set(globals.NATIVE_MPF_OWNER, undefined);
        }
      }),
    );
    const historyOwner = yield* makeProductionEventHistoryOwner({
      expectedGenesisLosslessSha256:
        nodeConfig.L1_HISTORY_GENESIS_LOSSLESS_SHA256,
      transport: {
        ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
        kupoUrl: nodeConfig.L1_KUPO_KEY,
        timeoutMs: 30_000,
        blockScanLimit: 100_000,
        maximumResponseBytes: 16 * 1024 * 1024,
        maximumTransactionBytes: 65_536,
      },
      heartbeatIntervalMs: 10_000,
      retainedPointLimit: 2_160,
      maximumReceiptBytes: 16 * 1024 * 1024,
      leaseDurationMs: 60_000,
      prepareCompletion: (_checkpoint, preparation) =>
        Effect.gen(function* () {
          yield* preparation.assertCurrent;
          if (startupPrepared) return;
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
          yield* assertStartupMutationJobsRecoverable;
          yield* runStartupProviderStepWithRetry(
            "Startup tx-order catch-up",
            fetchAndInsertTxOrderUTxOs,
            startupProviderRetry,
          ).pipe(
            Effect.tapError(
              logStartupFailure("Startup tx-order catch-up failed"),
            ),
            Effect.mapError(
              (e) =>
                new DatabaseInitializationError({
                  message: "Startup tx-order catch-up failed",
                  cause: e,
                }),
            ),
          );
          yield* backfillMissingDaPayloadsFromFinalizedJournals({
            limit: 100,
          }).pipe(
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
          // Source advancement may supersede preparation after resource creation.
          // Keep the existing owner for the next attempt instead of reopening Level.
          if ((yield* Ref.get(globals.NATIVE_MPF_OWNER)) === undefined) {
            yield* initializeArchitectureGOwner(
              globals,
              nodeConfig,
              preparation,
            ).pipe(
              Effect.mapError(
                (cause) =>
                  new DatabaseInitializationError({
                    message: "Architecture G native owner startup failed",
                    cause,
                  }),
              ),
            );
          }
          yield* preparation.assertCurrent;
          startupPrepared = true;
        }),
    }).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseInitializationError({
            message: "Authenticated history owner startup failed",
            cause,
          }),
      ),
    );
    yield* Ref.set(globals.EVENT_HISTORY_OWNER, historyOwner);
    yield* historyOwner.awaitReady.pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseInitializationError({
            message: "Authenticated history owner did not become ready",
            cause,
          }),
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

    yield* refreshAdmissionBacklogGauge;

    const httpApplicationLayer = HttpServer.serve(
      buildListenRouter(withMonitoring),
    ).pipe(Layer.provide(admissionAsDefaultSqlLayer));
    const appThread = Layer.launch(
      Layer.provide(
        httpApplicationLayer,
        NodeHttpServer.layer(createServer, { port: nodeConfig.PORT }),
      ),
    );
    const sql = yield* SqlClient.SqlClient;
    const retrieveRetainedDaPayload = (headerHash: Buffer) =>
      Effect.runPromise(
        DaPayloadsDB.retrieveByHeaderHash(headerHash).pipe(
          Effect.provideService(SqlClient.SqlClient, sql),
          Effect.map((payload) =>
            Option.isSome(payload) ? payload.value : undefined,
          ),
        ),
      );

    /**
     * Builds a fixed Effect schedule from a millisecond interval.
     */
    const mkSchedule = (millisBetweenRuns: number) =>
      Schedule.spaced(Duration.millis(millisBetweenRuns));

    const program = Effect.all(
      [
        admissionBacklogGaugeFiber(
          mkSchedule(nodeConfig.ADMISSION_BACKLOG_REFRESH_MS),
        ),
        historyOwner.awaitStopped,
        writeBehindFiber,
        appThread,
        retainedPayloadServerThread(retrieveRetainedDaPayload),
        daPublicationReconcilerFiber(
          mkSchedule(nodeConfig.MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS),
        ),
        blockCommitmentFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT),
        ),
        blockConfirmationFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION),
        ),
        operatorWatchdogFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT),
        ),
        nodeConfig.SPECULATIVE_COMMIT_BUILD
          ? userEventBarrierRefresherFiber(
              mkSchedule(nodeConfig.USER_EVENT_BARRIER_REFRESH_MS),
            )
          : Effect.void,
        nodeConfig.SPECULATIVE_COMMIT_BUILD
          ? speculativeCommitBuilderFiber
          : Effect.void,
        nodeConfig.SPECULATIVE_COMMIT_BUILD
          ? speculativeCommitSubmitterFiber
          : Effect.void,
        fetchAndInsertTxOrderUTxOsFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
        ),
        retentionSweeperFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_RETENTION_SWEEPS),
        ),
        mergeFiber(mkSchedule(nodeConfig.WAIT_BETWEEN_MERGE_TXS)),
        attestationTimeoutCorrectionFiber(
          mkSchedule(nodeConfig.WAIT_BETWEEN_MERGE_TXS),
        ),
        mpfPayloadAuditFiber,
        withMonitoring ? monitorMempoolFiber(mkSchedule(1000)) : Effect.void,
        txQueueProcessorFiber(mkSchedule(nodeConfig.TX_QUEUE_POLL_INTERVAL_MS)),
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
        // One fiber's failure fails the whole group; the process must exit
        // non-zero on it, so the cause is surfaced to the runtime, not logged
        // and dropped.
        Effect.orDie,
        Effect.ensuring(
          Effect.all(
            [
              Effect.promise(closeDaLibp2pPublicationTransport).pipe(
                Effect.catchAll(() => Effect.void),
              ),
            ],
            { discard: true },
          ),
        ),
      );
    } else {
      yield* pipe(
        program,
        Effect.withSpan("midgard"),
        Effect.orDie,
        Effect.ensuring(
          Effect.all(
            [
              Effect.promise(closeDaLibp2pPublicationTransport).pipe(
                Effect.catchAll(() => Effect.void),
              ),
            ],
            { discard: true },
          ),
        ),
      );
    }
  }).pipe(
    Effect.scoped,
    Effect.provide(validationPoolLayer),
    Effect.provide(mempoolLedgerCacheLayer),
  );
