import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { NodeSdk } from "@effect/opentelemetry";
import { SqlClient } from "@effect/sql";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { Cause, Effect, Fiber, Option, pipe, Ref } from "effect";

import { closeDaLibp2pPublicationTransport } from "../da/libp2p-producer.js";
import {
  assertDaHardeningProviderStartup,
  prepareDaHardeningStartup,
  runDaIdentityGatedStartupSequence,
} from "../da/startup.js";
import { restoreRetainedStatePins } from "../database/cekProgramMaterial.restore-retained-state-pins.js";
import { PredecessorLeaseWait } from "../database/eventHistoryAuthority.js";
import { DaPayloadsDB, InitDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { assertPhase1AcceptCrashCheckpointConfiguration } from "../e2e/phase1-accept-crash-checkpoint.js";
import { refreshAdmissionBacklogGauge } from "../fibers/index.js";
import * as Genesis from "../genesis.js";
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
} from "../services/index.js";
import {
  IntentJournal,
  IntentJournalWithoutFollower,
  makeIntentJournal,
} from "../services/intent-journal.js";
import { startL1Follower } from "../services/l1-follower.js";
import {
  initializeArchitectureGOwner,
  requirePinnedNativeOwnerBinary,
} from "../services/native-mpf-startup.js";
import { settlementWalletAddress } from "../services/settlement.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../workers/commit-block-header/da-payload-backfill.js";
import { runNodeFiberSet } from "./listen.node-fibers.js";
import {
  logStartupFailure,
  retainedPayloadServerThread,
  runStartupProviderStepWithRetry,
} from "./listen.retained-payload-server-thread.js";
import type { StartupHttp } from "./listen.startup-http.js";
import { buildListenRouter } from "./listen-router.js";
import { awaitLandedStateQueueOnStartup } from "./listen-startup.await-landed-state-queue.js";
import {
  assertStartupMutationJobsRecoverable,
  ensureProtocolInitializedOnStartup,
  hydratePendingBlockFinalizationOnStartup,
  releaseStateQueueLeasesOfPreviousNodeProcess,
  seedLatestLocalBlockBoundaryOnStartup,
} from "./listen-startup.js";
import { releaseLedgerStoreLeaseOfPreviousNodeProcess } from "./listen-startup.release-ledger-store-lease-of-previous-node-process.js";
import { reportHistorySyncReasons } from "./listen-startup.report-history-sync.js";
import { shouldRunGenesisOnStartup } from "./startup-policy.js";

/**
 * Boots the long-running Midgard node runtime.
 *
 * The effect wires database initialization, protocol startup checks, optional
 * genesis bootstrapping, the HTTP server, and the background fibers that keep
 * the node progressing.
 */
export const runNode = (
  startup: StartupHttp,
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

    yield* startup.setStage("local_preflight");
    yield* Effect.try({
      try: () => settlementWalletAddress(nodeConfig),
      catch: (cause) =>
        new ConfigError({
          message: "Automatic settlement wallet configuration is invalid",
          cause,
          fieldsAndValues: [],
        }),
    });

    yield* assertPhase1AcceptCrashCheckpointConfiguration;
    // The ledger MPF is always the Architecture G owner. Refuse to start before
    // any durable work when its binary or sidecar is not pinned.
    yield* requirePinnedNativeOwnerBinary(nodeConfig).pipe(
      Effect.mapError(
        (cause) =>
          new ConfigError({
            message: cause.message,
            cause,
            fieldsAndValues: [
              [
                "MPF_NATIVE_OWNER_BINARY_PATH",
                nodeConfig.MPF_NATIVE_OWNER_BINARY_PATH,
              ],
              [
                "MPF_NATIVE_OWNER_SIDECAR_PATH",
                nodeConfig.MPF_NATIVE_OWNER_SIDECAR_PATH,
              ],
            ],
          }),
      ),
    );
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
      initializeDatabase: startup
        .setStage("database_initialization")
        .pipe(
          Effect.zipRight(InitDB.program.pipe(Effect.provide(Database.layer))),
        ),
      // Protocol initialization runs before the follower starts: nothing
      // would reconcile its intents, so it submits unjournaled and awaits
      // each confirmation itself.
      initializeProtocol: startup
        .setStage("protocol_initialization")
        .pipe(
          Effect.zipRight(
            ensureProtocolInitializedOnStartup.pipe(
              Effect.provide(IntentJournalWithoutFollower),
            ),
          ),
        ),
      providerAssertions: (preflight) =>
        startup
          .setStage("provider_assertions")
          .pipe(
            Effect.zipRight(
              runStartupProviderStepWithRetry(
                "Startup DA provider assertions",
                assertDaHardeningProviderStartup(preflight),
                startupProviderRetry,
              ).pipe(
                Effect.tapError(
                  logStartupFailure("Startup DA provider assertions failed"),
                ),
              ),
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
    // One journal for the node process, opened once its database is: its
    // refusals are `/readyz` holds.
    const intentJournal = yield* makeIntentJournal;
    // The L1 follower (N1) starts first: the history owner's recovery
    // ingests events at its view, and its driver writes the event rows.
    yield* startL1Follower.pipe(
      Effect.provideService(IntentJournal, intentJournal),
    );
    // Startup recovery seeds the commit base from the landed state queue
    // (P1, N2): wait, unready and never exiting, until the follower is at
    // the tip and P1 is healthy there.
    yield* startup.setStage("l1_follower_catch_up");
    yield* awaitLandedStateQueueOnStartup((reasons) =>
      startup.setStage("l1_follower_catch_up", reasons),
    );
    yield* startup.setStage("history_initialization");
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
          yield* startup.setStage("recovery_preparation");
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
          yield* restoreRetainedStatePins.pipe(
            Effect.mapError(
              (cause) =>
                new DatabaseInitializationError({
                  message: "Startup retained script material recovery failed",
                  cause,
                }),
            ),
          );
          yield* hydratePendingBlockFinalizationOnStartup;
          yield* releaseStateQueueLeasesOfPreviousNodeProcess;
          yield* releaseLedgerStoreLeaseOfPreviousNodeProcess;
          yield* assertStartupMutationJobsRecoverable;
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
      // A predecessor killed without releasing its history lease is waited
      // out, not treated as a live owner; a lease still renewed past one
      // duration plus the margin is one, and startup fails as before.
      Effect.provideService(PredecessorLeaseWait, {
        marginMs: 10_000,
        pollIntervalMs: 2_000,
      }),
      Effect.mapError(
        (cause) =>
          new DatabaseInitializationError({
            message: "Authenticated history owner startup failed",
            cause,
          }),
      ),
    );
    yield* Ref.set(globals.EVENT_HISTORY_OWNER, historyOwner);
    yield* startup.setStage("history_sync");
    // A held owner (a failed landed-block rebase it retries) keeps startup
    // here, unready by name, never exiting.
    const historySyncReport = yield* Effect.fork(
      reportHistorySyncReasons((reasons) =>
        startup.setStage("history_sync", reasons),
      ),
    );
    yield* historyOwner.awaitReady.pipe(
      Effect.ensuring(Fiber.interrupt(historySyncReport)),
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
          Effect.provideService(IntentJournal, intentJournal),
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

    const publishHttp = startup
      .publish(buildListenRouter(withMonitoring))
      .pipe(Effect.provide(admissionAsDefaultSqlLayer));

    // Membership comes from the follower's operator-set hook; no duty waits
    // on its first run. A removed operator's duties are held through
    // `HaltSource.operatorMembership` and the process stays up (§7.5 R7).
    const program = publishHttp.pipe(
      Effect.zipRight(
        Effect.all(
          runNodeFiberSet({
            nodeConfig,
            withMonitoring,
            startupFibers: {
              historyOwnerStopped: historyOwner.awaitStopped,
              retainedPayloadServer: retainedPayloadServerThread(
                retrieveRetainedDaPayload,
              ),
            },
          }),
          {
            concurrency: "unbounded",
          },
        ),
      ),
      Effect.provideService(IntentJournal, intentJournal),
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
