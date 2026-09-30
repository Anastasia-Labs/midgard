import "./index.registration-6.js";

import { SqlClient } from "@effect/sql";
import { Effect, pipe } from "effect";
import {
  assertUserCliWalletIsOperationallyIsolated,
  collectStringOption,
  failCli,
  parseStringListOption,
  writeJson,
} from "midgard-node/commands/cli-runtime";
import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
} from "midgard-node/commands/command-utils";
import * as SubmitL2Transfer from "midgard-node/commands/submit-l2-transfer";
import * as Services from "midgard-node/services/index";

import * as E2EStressL2ThroughputCommand from "./commands/e2e-stress-l2-throughput/index.js";
import { collectGroundTruthMetricsFromSql } from "./commands/stress-db-metrics.js";
import { collectEnvironmentFingerprint } from "./commands/stress-environment-fingerprint.js";
import { collectStressStageMetricSourcesFromSql } from "./commands/stress-stage-metrics.js";
import { stressNetworkFromEnvironment } from "./environment.js";
import { program, stressCliLoggerLayer } from "./index.registration.js";

program
  .command("e2e-stress-l2-throughput")
  .description(
    "Run opt-in bounded L2 transfer stress for an e2e deployment and write stress artifacts",
  )
  .option(
    "--endpoint <url>",
    "Midgard node HTTP endpoint used for /utxos, /submit, and /tx-status",
    defaultMidgardNodeEndpoint(),
  )
  .option(
    "--mode <mode>",
    "Stress mode: serial-chain or parallel-fanout",
    "serial-chain",
  )
  .option(
    "--load-model <model>",
    "Stress load model: closed-loop-smoke or open-loop-upper-bound",
    "closed-loop-smoke",
  )
  .option(
    "--workload-profile <profile>",
    "Workload profile label: synthetic-admission or production-end-user",
  )
  .option(
    "--corpus-shape <shape>",
    "Open-loop corpus shape: fanout, chain, or mixed",
    "fanout",
  )
  .option(
    "--tx-corpus <path>",
    "Open-loop tx-corpus.ndjson path with prebuilt canonical CBOR rows",
  )
  .option(
    "--corpus-slice-id <id>",
    "Open-loop corpus slice id to use for this rate step",
    "default",
  )
  .option(
    "--target-rate-tps <rate>",
    "Open-loop target submit rate in transactions per second",
    "100",
  )
  .option(
    "--open-loop-duration-ms <ms>",
    "Open-loop measured submission duration in milliseconds",
    "10000",
  )
  .option(
    "--open-loop-warmup-count <count>",
    "Open-loop corpus warmup transaction count reserved before the measured window",
    "0",
  )
  .option(
    "--open-loop-cooldown-count <count>",
    "Open-loop corpus cooldown transaction count reserved after the measured window",
    "0",
  )
  .option(
    "--open-loop-max-in-flight <count>",
    "Open-loop maximum concurrent POST /submit requests",
    "256",
  )
  .option(
    "--no-op-calibration-endpoint <url>",
    "No-op endpoint with the POST /submit request shape, used for client calibration",
  )
  .option(
    "--require-no-op-calibration",
    "Fail open-loop runs unless no-op calibration is configured and passes",
  )
  .option(
    "--no-op-calibration-duration-ms <ms>",
    "No-op calibration duration in milliseconds",
    "5000",
  )
  .option(
    "--aggregate-observer-interval-ms <ms>",
    "Aggregate observer sampling interval during open-loop submission",
    "1000",
  )
  .option("--count <count>", "Number of stress transfers to submit", "25")
  .option("--concurrency <count>", "Maximum concurrent stress workers", "1")
  .option(
    "--lovelace <amount>",
    "Lovelace amount for each stress transfer",
    "1000000",
  )
  .option(
    "--fee-headroom-lovelace <amount>",
    "Extra lovelace required above --lovelace when preflighting parallel-fanout wallet funding",
    "500000",
  )
  .option(
    "--wallet-seed-phrase <seedPhrase>",
    "Optional primary seed phrase used directly instead of reading from an environment variable",
  )
  .option(
    "--wallet-seed-phrase-env <envVar>",
    "Environment variable containing the primary wallet seed phrase",
    DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--stress-wallet-seed-phrase-env <envVar>",
    "Environment variable for an independent pre-funded stress wallet; repeat for parallel-fanout",
    collectStringOption,
    [],
  )
  .option(
    "--l2-address <address>",
    "Optional destination L2 address; defaults to each sender's own address",
  )
  .option("--run-id <id>", "Stable e2e run id for stress artifacts")
  .option("--out-dir <path>", "Output directory for stress artifacts")
  .option(
    "--poll-interval-ms <ms>",
    "Fixed /tx-status polling interval; omit to use adaptive backoff (poll-initial-interval-ms to poll-max-interval-ms)",
  )
  .option(
    "--poll-initial-interval-ms <ms>",
    "Initial adaptive poll interval (ignored if --poll-interval-ms is set)",
    "75",
  )
  .option(
    "--poll-max-interval-ms <ms>",
    "Adaptive poll interval cap (ignored if --poll-interval-ms is set)",
    "1000",
  )
  .option(
    "--submit-request-timeout-ms <ms>",
    "Per-transfer timeout for the submit request phase",
    "300000",
  )
  .option(
    "--acceptance-timeout-ms <ms>",
    "Per-transfer timeout for /tx-status to reach accepted-or-later",
    "600000",
  )
  .option(
    "--commit-observation-timeout-ms <ms>",
    "Per-transfer background timeout for observing committed status",
    "600000",
  )
  .option(
    "--finality-observer-max-concurrent-requests <count>",
    "Maximum concurrent /tx-status requests used by post-submit finality observation",
    "4",
  )
  .option(
    "--unsafe-allow-large-stress",
    "Explicitly allow count/concurrency above the default safety caps",
  )
  .option(
    "--max-submission-failures <count>",
    "Abort the run once this many transfer submissions/builds fail (default: 0, zero tolerance)",
    "0",
  )
  .option("--json", "Print JSON result")
  .action(async (options) => {
    const abortController = new AbortController();
    const interrupt = (signalName: NodeJS.Signals): void => {
      if (!abortController.signal.aborted) {
        abortController.abort(
          new Error(
            `received ${signalName}; writing interrupted stress summary`,
          ),
        );
      }
    };
    process.once("SIGINT", interrupt);
    process.once("SIGTERM", interrupt);
    try {
      const stressConfig = E2EStressL2ThroughputCommand.parseE2EL2StressConfig({
        endpoint: options.endpoint,
        loadModel: options.loadModel,
        workloadProfile: options.workloadProfile,
        mode: options.mode,
        corpusShape: options.corpusShape,
        corpusPath: options.txCorpus,
        corpusSliceId: options.corpusSliceId,
        targetRateTps: options.targetRateTps,
        openLoopDurationMs: options.openLoopDurationMs,
        openLoopWarmupCount: options.openLoopWarmupCount,
        openLoopCooldownCount: options.openLoopCooldownCount,
        openLoopMaxInFlight: options.openLoopMaxInFlight,
        noOpCalibrationEndpoint: options.noOpCalibrationEndpoint,
        requireNoOpCalibration: options.requireNoOpCalibration === true,
        noOpCalibrationDurationMs: options.noOpCalibrationDurationMs,
        aggregateObserverIntervalMs: options.aggregateObserverIntervalMs,
        count: options.count,
        concurrency: options.concurrency,
        lovelace: options.lovelace,
        feeHeadroomLovelace: options.feeHeadroomLovelace,
        walletSeedPhrase: options.walletSeedPhrase,
        walletSeedPhraseEnv: options.walletSeedPhraseEnv,
        stressWalletSeedPhraseEnvs: parseStringListOption(
          options.stressWalletSeedPhraseEnv,
          "--stress-wallet-seed-phrase-env",
        ),
        l2Address: options.l2Address,
        runId: options.runId,
        outDir: options.outDir,
        pollIntervalMs: options.pollIntervalMs,
        pollInitialIntervalMs: options.pollInitialIntervalMs,
        pollMaxIntervalMs: options.pollMaxIntervalMs,
        submitRequestTimeoutMs: options.submitRequestTimeoutMs,
        acceptanceTimeoutMs: options.acceptanceTimeoutMs,
        commitObservationTimeoutMs: options.commitObservationTimeoutMs,
        finalityObserverMaxConcurrentRequests:
          options.finalityObserverMaxConcurrentRequests,
        maxSubmissionFailures: options.maxSubmissionFailures,
        network: stressNetworkFromEnvironment(),
        allowUnsafeBounds: options.unsafeAllowLargeStress === true,
      });
      const stressProgram = Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const sql = yield* SqlClient.SqlClient;
        const nodeConfig = yield* Services.NodeConfig;
        const writeBehind = yield* Services.WriteBehind;
        const deploymentIdentity = yield* Services.ContractDeploymentIdentity;
        return yield* Effect.tryPromise({
          try: () =>
            E2EStressL2ThroughputCommand.runE2EL2StressThroughput(
              stressConfig,
              {
                submitTransfer: async (request) =>
                  await Effect.runPromise(
                    SubmitL2Transfer.submitL2TransferProgram({
                      config: request.config,
                      resolvedWalletSeedPhrase:
                        request.resolvedWalletSeedPhrase,
                      assertWalletAddress: (walletAddress) =>
                        assertUserCliWalletIsOperationallyIsolated({
                          commandName: "e2e-stress-l2-throughput",
                          walletAddress,
                          operatorMainAddress: lucidService.operatorMainAddress,
                          operatorMergeAddress:
                            lucidService.operatorMergeAddress,
                          referenceScriptsAddress:
                            lucidService.referenceScriptsWalletAddress,
                        }),
                    }).pipe(
                      Effect.provideService(Services.Lucid, lucidService),
                      Effect.provideService(SqlClient.SqlClient, sql),
                      Effect.provideService(Services.NodeConfig, nodeConfig),
                      Effect.provideService(Services.WriteBehind, writeBehind),
                      Effect.provideService(
                        Services.ContractDeploymentIdentity,
                        deploymentIdentity,
                      ),
                    ),
                  ),
                collectStageMetricSources: async ({ txHashes }) =>
                  await Effect.runPromise(
                    collectStressStageMetricSourcesFromSql(txHashes).pipe(
                      Effect.provideService(SqlClient.SqlClient, sql),
                    ),
                  ),
                collectGroundTruthMetrics: async ({
                  windowStart,
                  windowEnd,
                  txHashSample,
                  offeredCount,
                  calibrationProofRef,
                }) =>
                  await Effect.runPromise(
                    collectGroundTruthMetricsFromSql({
                      windowStart,
                      windowEnd,
                      txHashSample,
                      offeredCount,
                      trimFraction: 0.1,
                      calibrationProofRef,
                    }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
                  ),
                collectEnvironmentFingerprint: async ({
                  calibrationProofRef,
                }) =>
                  await collectEnvironmentFingerprint({
                    calibrationProofRef: calibrationProofRef ?? null,
                    configProfile: {
                      maxDurableAdmissionBacklog:
                        nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
                      waitBetweenBlockCommitment:
                        nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT,
                      waitBetweenBlockConfirmation:
                        nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION,
                      waitBetweenMergeTxs: nodeConfig.WAIT_BETWEEN_MERGE_TXS,
                      validationBatchSize: nodeConfig.VALIDATION_BATCH_SIZE,
                      validationPhaseAConcurrency:
                        nodeConfig.VALIDATION_PHASE_A_CONCURRENCY,
                    },
                  }),
                collectAggregateObserverSample: async ({ at }) =>
                  await Effect.runPromise(
                    Effect.gen(function* () {
                      const [
                        admissionRows,
                        mempoolRows,
                        processedRows,
                        pendingRows,
                      ] = yield* Effect.all(
                        [
                          sql<{
                            readonly status: string;
                            readonly count: bigint | number | string;
                          }>`SELECT status, COUNT(*)::bigint AS count FROM tx_admissions GROUP BY status ORDER BY status`,
                          sql<{
                            readonly count: bigint | number | string;
                          }>`SELECT COUNT(*)::bigint AS count FROM mempool`,
                          sql<{
                            readonly count: bigint | number | string;
                          }>`SELECT COUNT(*)::bigint AS count FROM processed_mempool`,
                          sql<{
                            readonly status: string;
                            readonly count: bigint | number | string;
                          }>`SELECT status, COUNT(*)::bigint AS count FROM pending_block_finalizations GROUP BY status ORDER BY status`,
                        ],
                        { concurrency: "unbounded" },
                      );
                      return {
                        at,
                        txAdmissions: Object.fromEntries(
                          admissionRows.map((row) => [
                            row.status,
                            BigInt(row.count).toString(),
                          ]),
                        ),
                        mempoolTxCount: BigInt(
                          mempoolRows[0]?.count ?? 0,
                        ).toString(),
                        processedMempoolTxCount: BigInt(
                          processedRows[0]?.count ?? 0,
                        ).toString(),
                        pendingBlockFinalizations: Object.fromEntries(
                          pendingRows.map((row) => [
                            row.status,
                            BigInt(row.count).toString(),
                          ]),
                        ),
                      };
                    }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
                  ),
                abortSignal: abortController.signal,
              },
            ),
          catch: (cause) =>
            cause instanceof Error ? cause : new Error(String(cause)),
        });
      });
      const result = await Effect.runPromise(
        pipe(
          stressProgram,
          Effect.provide(Services.WriteBehindLive),
          Effect.provide(Services.NodeConfig.layer),
          Effect.provide(Services.Database.layer),
          Effect.provide(Services.Lucid.Default),
          Effect.provide(Services.MidgardContractServices),
          Effect.provide(stressCliLoggerLayer),
        ),
      );
      writeJson(result.summary);
      if (result.summary.status === "interrupted") {
        process.exitCode = 130;
      }
    } catch (error) {
      failCli("e2e-stress-l2-throughput", error);
    } finally {
      process.off("SIGINT", interrupt);
      process.off("SIGTERM", interrupt);
    }
  });
