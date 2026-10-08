import { NodeSdk } from "@effect/opentelemetry";
import { SqlClient } from "@effect/sql";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { Cause, Effect, Option, pipe, Ref } from "effect";

import { closeDaLibp2pPublicationTransport } from "../da/libp2p-producer.js";
import {
  assertDaHardeningProviderStartup,
  prepareDaHardeningStartup,
  runDaIdentityGatedStartupSequence,
} from "../da/startup.js";
import { DaPayloadsDB, InitDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { assertPhase1AcceptCrashCheckpointConfiguration } from "../e2e/phase1-accept-crash-checkpoint.js";
import { refreshAdmissionBacklogGauge } from "../fibers/index.js";
import * as Genesis from "../genesis.js";
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
import { requirePinnedNativeOwnerBinary } from "../services/native-mpf-startup.js";
import { settlementWalletAddress } from "../services/settlement.js";
import { runNodeFiberSet } from "./listen.node-fibers.js";
import {
  logStartupFailure,
  retainedPayloadServerThread,
  runStartupProviderStepWithRetry,
} from "./listen.retained-payload-server-thread.js";
import type { StartupHttp } from "./listen.startup-http.js";
import { buildListenRouter } from "./listen-router.js";
import { awaitFollowerViewOnStartup } from "./listen-startup.await-follower-view.js";
import { awaitLandedStateQueueOnStartup } from "./listen-startup.await-landed-state-queue.js";
import { ensureProtocolInitializedOnStartup } from "./listen-startup.js";
import { prepareNodeOnStartup } from "./listen-startup.prepare-node.js";
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
    // The L1 follower (N1): its driver writes the event rows, and its first
    // recompute runs the startup preparation, starts the native MPF owner
    // and recomputes the node's derived state from the follower's view.
    yield* startL1Follower({ startupPreparation: prepareNodeOnStartup }).pipe(
      Effect.provideService(IntentJournal, intentJournal),
    );
    // Startup recovery seeds the commit base from the landed state queue
    // (P1, N2): wait, unready and never exiting, until the follower is at
    // the tip and P1 is healthy there.
    yield* startup.setStage("l1_follower_catch_up");
    yield* awaitLandedStateQueueOnStartup((reasons) =>
      startup.setStage("l1_follower_catch_up", reasons),
    );
    // Then until the driver published its first view: what holds it (a
    // failed startup preparation or rebase it retries) is named, and the
    // process stays up.
    yield* startup.setStage("follower_view_apply");
    yield* awaitFollowerViewOnStartup((reasons) =>
      startup.setStage("follower_view_apply", reasons),
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
