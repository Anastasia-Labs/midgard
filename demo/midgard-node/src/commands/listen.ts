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
import {
  DaPayloadsDB,
  InitDB,
  MempoolLedgerDB,
  MpfEngineStateDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { assertPhase1AcceptCrashCheckpointConfiguration } from "../e2e/phase1-accept-crash-checkpoint.js";
import {
  admissionBacklogGaugeFiber,
  attestationTimeoutCorrectionFiber,
  blockCommitmentFiber,
  blockConfirmationFiber,
  daPublicationReconcilerFiber,
  fetchAndInsertDepositUTxOs,
  fetchAndInsertDepositUTxOsFiber,
  fetchAndInsertTxOrderUTxOs,
  fetchAndInsertTxOrderUTxOsFiber,
  fetchAndInsertWithdrawalUTxOs,
  fetchAndInsertWithdrawalUTxOsFiber,
  mergeFiber,
  monitorMempoolFiber,
  mpfPayloadAuditFiber,
  projectDepositsToMempoolLedger,
  projectDepositsToMempoolLedgerFiber,
  refreshAdmissionBacklogGauge,
  retentionSweeperFiber,
  speculativeCommitBuilderFiber,
  speculativeCommitSubmitterFiber,
  txQueueProcessorFiber,
  userEventBarrierRefresherFiber,
} from "../fibers/index.js";
import * as Genesis from "../genesis.js";
import { MidgardMpf, utxoToLedgerInsertMaterial } from "../mpf/index.js";
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
  type NodeConfigDep,
  ProductionNativeMpfOwnerService,
  validationPoolLayer,
  WriteBehind,
  writeBehindFiber,
} from "../services/index.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../workers/commit-block-header/da-payload-backfill.js";
import { buildListenRouter } from "./listen-router.js";
import {
  ensureProtocolInitializedOnStartup,
  hydratePendingBlockFinalizationOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "./listen-startup.js";
import { shouldRunGenesisOnStartup } from "./startup-policy.js";

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

const initializeArchitectureGOwner = (
  globals: Globals,
  nodeConfig: NodeConfigDep,
): Effect.Effect<
  ProductionNativeMpfOwnerService | undefined,
  unknown,
  Database
> =>
  Effect.gen(function* () {
    if (nodeConfig.MPF_ENGINE !== "architecture_g") return undefined;

    const bootstrap = yield* MidgardMpf.create(
      "architecture-g-bootstrap",
      nodeConfig.LEDGER_MPF_DB_PATH,
      {
        engine: "overlay",
        spillThresholdBytes: nodeConfig.MPF_OVERLAY_SPILL_BYTES,
      },
    );
    const initialized = yield* Effect.either(
      Effect.gen(function* () {
        if (yield* bootstrap.rootIsEmpty()) {
          const genesisEntries = yield* Effect.forEach(
            nodeConfig.GENESIS_UTXOS,
            (utxo) =>
              utxoToLedgerInsertMaterial(utxo).pipe(
                Effect.map(({ ledgerOp, outputCbor }) => ({
                  op: ledgerOp,
                  ledgerEntry: {
                    [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(
                      utxo.txHash,
                      "hex",
                    ),
                    [MempoolLedgerDB.Columns.OUTREF]: ledgerOp.key,
                    [MempoolLedgerDB.Columns.OUTPUT]: outputCbor,
                    [MempoolLedgerDB.Columns.ADDRESS]: utxo.address,
                    [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
                  } satisfies MempoolLedgerDB.EntryNoTimeStamp,
                })),
              ),
          );
          if (genesisEntries.length === 0) {
            return yield* Effect.fail(
              new Error(
                "Architecture G cannot initialize an empty ledger owner without genesis UTxOs",
              ),
            );
          }
          yield* MempoolLedgerDB.insert(
            genesisEntries.map(({ ledgerEntry }) => ledgerEntry),
          );
          yield* bootstrap.applyBatch(genesisEntries.map(({ op }) => op));
        }
        const root = yield* bootstrap.rootHex();
        yield* MpfEngineStateDB.stampLedgerMigration(root);
      }),
    );
    yield* bootstrap.close().pipe(Effect.catchAll(() => Effect.void));
    if (initialized._tag === "Left")
      return yield* Effect.fail(initialized.left);

    const owner = yield* Effect.tryPromise({
      try: () =>
        ProductionNativeMpfOwnerService.create({
          levelPath: nodeConfig.LEDGER_MPF_DB_PATH,
          binaryPath: nodeConfig.MPF_NATIVE_OWNER_BINARY_PATH,
          binarySha256: nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
          maxFrameBytes: nodeConfig.MPF_NATIVE_OWNER_MAX_FRAME_BYTES,
          maxChunkBytes: nodeConfig.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES,
          requestTimeoutMs: nodeConfig.MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS,
          restartLimit: nodeConfig.MPF_NATIVE_OWNER_RESTART_LIMIT,
          sidecarPath: nodeConfig.MPF_NATIVE_OWNER_SIDECAR_PATH,
        }),
      catch: (cause) => cause,
    });
    const startup = yield* Effect.either(
      Effect.gen(function* () {
        const active = yield* PendingBlockFinalizationsDB.retrieveActive();
        if (Option.isSome(active)) {
          const journal = active.value;
          const replay = journal.nativeMpfReplay;
          const submitted =
            journal[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH] !==
            null;
          if (submitted && replay === undefined) {
            return yield* Effect.fail(
              new Error(
                `Architecture G active submitted journal is missing replay data: header_hash=${journal[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex")}`,
              ),
            );
          }
          if (submitted && replay !== undefined) {
            yield* Effect.tryPromise({
              try: () =>
                owner.recover({
                  schema: 1,
                  ownerBinarySha256: replay.ownerBinarySha256.toString("hex"),
                  baseRoot: replay.baseRoot.toString("hex"),
                  candidateRoot: replay.candidateRoot.toString("hex"),
                  eventLog: replay.eventLog,
                  eventLogDigest: replay.eventLogDigest.toString("hex"),
                  eventRoots: replay.eventRoots,
                  eventCount: replay.eventCount,
                }),
              catch: (cause) => cause,
            });
          }
        }
        yield* Ref.set(globals.NATIVE_MPF_OWNER, owner);
      }),
    );
    if (startup._tag === "Left") {
      yield* Effect.promise(() => owner.close()).pipe(
        Effect.catchAll(() => Effect.void),
      );
      return yield* Effect.fail(startup.left);
    }
    return owner;
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

    yield* refreshAdmissionBacklogGauge;
    const nativeMpfOwner = yield* initializeArchitectureGOwner(
      globals,
      nodeConfig,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseInitializationError({
            message: "Architecture G native owner startup failed",
            cause,
          }),
      ),
    );

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

    const closeNativeMpfOwner =
      nativeMpfOwner === undefined
        ? Effect.void
        : Effect.promise(() => nativeMpfOwner.close()).pipe(
            Effect.catchAll((error) =>
              Effect.logError(
                `Architecture G native owner shutdown failed: ${formatUnknownError(error)}`,
              ),
            ),
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
        Effect.ensuring(
          Effect.all(
            [
              Effect.promise(closeDaLibp2pPublicationTransport).pipe(
                Effect.catchAll(() => Effect.void),
              ),
              closeNativeMpfOwner,
            ],
            { discard: true },
          ),
        ),
      );
    } else {
      yield* pipe(
        program,
        Effect.withSpan("midgard"),
        Effect.catchAllCause(Effect.logError),
        Effect.ensuring(
          Effect.all(
            [
              Effect.promise(closeDaLibp2pPublicationTransport).pipe(
                Effect.catchAll(() => Effect.void),
              ),
              closeNativeMpfOwner,
            ],
            { discard: true },
          ),
        ),
      );
    }
  }).pipe(
    Effect.provide(validationPoolLayer),
    Effect.provide(mempoolLedgerCacheLayer),
  );
