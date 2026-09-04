#!/usr/bin/env node

/**
 * midgard-node-tools: the e2e, stress, and acceptance tooling that drives a
 * Midgard node from the outside. It is a separate binary on purpose — none of
 * these commands belong in the operator's `dist/index.js` (AGENTS.md: demo and
 * benchmark behavior must be explicit, isolated, and unavailable by default).
 *
 * midgard-node is compiled into this bundle from source through its
 * `midgard-source` exports condition; the operator package publishes no
 * per-module dist for anything else to resolve.
 */

import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { SqlClient } from "@effect/sql";
import { Command } from "commander";
import { Effect, Logger, pipe } from "effect";
import {
  assertUserCliWalletIsOperationallyIsolated,
  collectStringOption,
  failCli,
  parseE2EEnvInheritanceOption,
  parsePositiveIntegerOption,
  parseStringListOption,
  provideDatabaseServices,
  provideDatabaseTxServices,
  provideNodeRuntimeServices,
  provideTxServices,
  runCliEffect,
  tapJson,
  writeJson,
} from "midgard-node/commands/cli-runtime";
import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  fetchNodeTxStatus,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "midgard-node/commands/command-utils";
import * as SubmitL2Transfer from "midgard-node/commands/submit-l2-transfer";
import { parseEnvOverrides } from "midgard-node/e2e/env";
import {
  fetchAndInsertDepositUTxOs,
  projectDepositsToMempoolLedger,
} from "midgard-node/fibers/index";
import { loadRuntimeDotenv } from "midgard-node/runtime-env";
import * as Services from "midgard-node/services/index";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "midgard-node/transactions/reference-scripts";
import * as SubmitDeposit from "midgard-node/transactions/submit-deposit";

import packageJson from "../package.json" with { type: "json" };
import * as E2EFinalizeSummaryCommand from "./commands/e2e-finalize-summary.js";
import { runPipelinedCommitProcessAcceptance } from "./commands/e2e-pipelined-commit-process-acceptance.js";
import * as E2EProcessCleanupCommand from "./commands/e2e-process-cleanup.js";
import * as E2EServiceCommand from "./commands/e2e-service.js";
import * as E2EStressL2ThroughputCommand from "./commands/e2e-stress-l2-throughput.js";
import * as Phase4GenesisLedgerCommand from "./commands/phase4-genesis-ledger.js";
import * as Phase4T1RecoveryCommand from "./commands/phase4-t1-recovery.js";
import * as StressCorpusCommand from "./commands/stress-corpus-generate.js";
import { collectGroundTruthMetricsFromSql } from "./commands/stress-db-metrics.js";
import { collectEnvironmentFingerprint } from "./commands/stress-environment-fingerprint.js";
import { collectStressStageMetricSourcesFromSql } from "./commands/stress-stage-metrics.js";
import * as StressWalletsCommand from "./commands/stress-wallets.js";
import { runCommandStep } from "./e2e/runner.js";
import {
  l1KupmiosEnvironment,
  stressNetworkFromEnvironment,
} from "./environment.js";

loadRuntimeDotenv();
const VERSION = packageJson.version;

const program = new Command();

program
  .name("midgard-node-tools")
  .version(VERSION)
  .description(
    "Midgard node e2e, stress, and acceptance tooling. Every command here drives a node from the outside; none of them ship in the operator binary.",
  );

const E2E_TX_STATUSES = new Set([
  "submitted",
  "confirmed",
  "queued",
  "accepted",
  "committed",
  "rejected",
  "unknown",
] as const);

type E2ETxStatus = NonNullable<
  E2EFinalizeSummaryCommand.FinalizeSummaryOptions["transactions"]
>[number]["status"];

const E2E_TX_HASH_PATTERN = /^[0-9a-f]{64}$/i;

const E2E_TX_LABEL_PATTERN = /^[A-Za-z0-9][A-Za-z0-9_.-]*$/;

const parseTxEvidenceOption = (
  value: string,
): NonNullable<
  E2EFinalizeSummaryCommand.FinalizeSummaryOptions["transactions"]
>[number] => {
  const [label, txHash, status, ...sourceParts] = value.split(":");
  const normalizedStatus = status?.toLowerCase();
  const source = sourceParts.join(":").trim();
  if (
    label === undefined ||
    label.length === 0 ||
    txHash === undefined ||
    !E2E_TX_HASH_PATTERN.test(txHash) ||
    status === undefined ||
    normalizedStatus === undefined ||
    !E2E_TX_STATUSES.has(normalizedStatus as E2ETxStatus) ||
    sourceParts.length === 0 ||
    !E2E_TX_LABEL_PATTERN.test(label) ||
    source.length === 0 ||
    source.toLowerCase().includes("observedtxhashes")
  ) {
    throw new Error(
      "--tx must use label:64hexTxHash:status:source with a non-raw source and status one of submitted, confirmed, queued, accepted, committed, rejected, unknown",
    );
  }
  return {
    label,
    txHash: txHash.toLowerCase(),
    status: normalizedStatus as E2ETxStatus,
    source,
  };
};

const parseTxEvidenceOptions = (
  values: unknown,
): NonNullable<
  E2EFinalizeSummaryCommand.FinalizeSummaryOptions["transactions"]
> =>
  Array.isArray(values)
    ? values.map((value) => {
        if (typeof value !== "string") {
          throw new Error("--tx must be provided as a string.");
        }
        return parseTxEvidenceOption(value);
      })
    : [];

const stressCliLoggerLayer = Logger.replace(
  Logger.defaultLogger,
  Logger.withConsoleError(Logger.logfmtLogger),
);

program
  .command("create-l2-wallet")
  .description(
    "Generate or read persisted L2 stress wallets and write seed env exports",
  )
  .option("--count <count>", "Number of L2 stress wallets to create", "1")
  .option("--start-index <index>", "First wallet index to create", "1")
  .option(
    "--out-dir <path>",
    "Directory that stores stress wallet JSON/env/args files",
    StressWalletsCommand.DEFAULT_STRESS_WALLET_DIR,
  )
  .option(
    "--env-prefix <prefix>",
    "Environment variable prefix for generated seed phrases",
    StressWalletsCommand.DEFAULT_STRESS_WALLET_ENV_PREFIX,
  )
  .option(
    "--network <network>",
    "Override network; defaults to NETWORK/Preprod",
  )
  .option("--reuse-existing", "Read existing wallet files instead of failing")
  .option("--overwrite", "Replace existing wallet files with new seed phrases")
  .action(async (options) => {
    try {
      const result = await StressWalletsCommand.createL2Wallets({
        count: StressWalletsCommand.parseStressWalletCount(
          options.count,
          "--count",
        ),
        startIndex: StressWalletsCommand.parseStressWalletCount(
          options.startIndex,
          "--start-index",
        ),
        outDir: options.outDir,
        envPrefix: options.envPrefix,
        network: StressWalletsCommand.parseStressWalletNetwork(options.network),
        reuseExisting: options.reuseExisting === true,
        overwrite: options.overwrite === true,
      });
      writeJson(result);
    } catch (error) {
      failCli("create-l2-wallet", error);
    }
  });

program
  .command("stress-wallets:prepare")
  .description(
    "Fund, project, and verify persisted L2 stress wallets for parallel-fanout benchmarks",
  )
  .requiredOption("--count <count>", "Number of stress wallets to prepare")
  .requiredOption(
    "--lovelace-per-wallet <amount>",
    "Projected L2 lovelace funding required for each wallet",
  )
  .option(
    "--endpoint <url>",
    "Midgard node HTTP endpoint used for /utxos verification",
    defaultMidgardNodeEndpoint(),
  )
  .option("--start-index <index>", "First wallet index to prepare", "1")
  .option(
    "--out-dir <path>",
    "Directory that stores stress wallet JSON/env/args files",
    StressWalletsCommand.DEFAULT_STRESS_WALLET_DIR,
  )
  .option(
    "--env-prefix <prefix>",
    "Environment variable prefix for generated seed phrases",
    StressWalletsCommand.DEFAULT_STRESS_WALLET_ENV_PREFIX,
  )
  .option(
    "--network <network>",
    "Override network; defaults to NETWORK/Preprod",
  )
  .option(
    "--funding-wallet-seed-phrase-env <envVar>",
    "Environment variable containing the L1 wallet seed phrase used to submit deposits",
    "L1_OPERATOR_SEED_PHRASE",
  )
  .option(
    "--projection-wait-ms <ms>",
    "Delay after submitted deposits before projecting L1 deposit events",
    StressWalletsCommand.DEFAULT_PROJECTION_WAIT_MS.toString(),
  )
  .option(
    "--verify-timeout-ms <ms>",
    "Maximum time to poll /utxos for projected L2 funding",
    StressWalletsCommand.DEFAULT_VERIFY_TIMEOUT_MS.toString(),
  )
  .option(
    "--poll-interval-ms <ms>",
    "Polling interval while verifying projected L2 funding",
    StressWalletsCommand.DEFAULT_VERIFY_POLL_INTERVAL_MS.toString(),
  )
  .option("--create-missing", "Create missing wallet files before funding")
  .option(
    "--force-fund-existing",
    "Submit a new deposit even when a wallet already has spendable L2 funding",
  )
  .action(
    async (options: {
      readonly count: string;
      readonly lovelacePerWallet: string;
      readonly endpoint: string;
      readonly startIndex: string;
      readonly outDir: string;
      readonly envPrefix: string;
      readonly network?: string;
      readonly fundingWalletSeedPhraseEnv: string;
      readonly projectionWaitMs: string;
      readonly verifyTimeoutMs: string;
      readonly pollIntervalMs: string;
      readonly createMissing?: boolean;
      readonly forceFundExisting?: boolean;
    }) => {
      let fundingWalletSeedPhrase: ResolvedWalletSeedPhrase;
      try {
        fundingWalletSeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhraseEnv: options.fundingWalletSeedPhraseEnv,
        });
      } catch (error) {
        failCli("stress-wallets:prepare", error);
        return;
      }

      try {
        const result = await StressWalletsCommand.prepareStressWallets(
          {
            count: StressWalletsCommand.parseStressWalletCount(
              options.count,
              "--count",
            ),
            lovelacePerWallet: StressWalletsCommand.parseStressWalletLovelace(
              options.lovelacePerWallet,
              "--lovelace-per-wallet",
            ),
            nodeEndpoint: options.endpoint,
            startIndex: StressWalletsCommand.parseStressWalletCount(
              options.startIndex,
              "--start-index",
            ),
            outDir: options.outDir,
            envPrefix: options.envPrefix,
            network: StressWalletsCommand.parseStressWalletNetwork(
              options.network,
            ),
            createMissing: options.createMissing === true,
            forceFundExisting: options.forceFundExisting === true,
            projectionWaitMs:
              StressWalletsCommand.parseStressWalletNonNegativeMs(
                options.projectionWaitMs,
                "--projection-wait-ms",
              ),
            verifyTimeoutMs:
              StressWalletsCommand.parseStressWalletNonNegativeMs(
                options.verifyTimeoutMs,
                "--verify-timeout-ms",
              ),
            pollIntervalMs: StressWalletsCommand.parseStressWalletNonNegativeMs(
              options.pollIntervalMs,
              "--poll-interval-ms",
            ),
          },
          {
            submitDeposit: async ({ wallet, lovelace }) =>
              Effect.runPromise(
                provideDatabaseTxServices(
                  Effect.gen(function* () {
                    const lucidService = yield* Services.Lucid;
                    const contracts = yield* Services.MidgardContracts;
                    yield* Effect.sync(() =>
                      lucidService.api.selectWallet.fromSeed(
                        fundingWalletSeedPhrase.seedPhrase,
                      ),
                    );
                    const walletAddress = yield* Effect.tryPromise({
                      try: () => lucidService.api.wallet().address(),
                      catch: (cause) =>
                        Promise.reject(
                          new Error(
                            `Failed to resolve stress funding wallet address: ${String(cause)}`,
                          ),
                        ),
                    });
                    yield* Effect.sync(() =>
                      assertUserCliWalletIsOperationallyIsolated({
                        commandName: "stress-wallets:prepare",
                        walletAddress,
                        operatorMainAddress: lucidService.operatorMainAddress,
                        operatorMergeAddress: lucidService.operatorMergeAddress,
                        referenceScriptsAddress:
                          lucidService.referenceScriptsWalletAddress,
                      }),
                    );
                    const depositReferenceScripts =
                      yield* fetchReferenceScriptUtxosProgram(
                        lucidService.api,
                        lucidService.referenceScriptsAddress,
                        referenceScriptTargetsByCommand(contracts).deposit,
                        contracts.referenceScriptAuth,
                      ).pipe(
                        Effect.map((resolved) => ({
                          depositMinting: referenceScriptByName(
                            resolved,
                            "deposit minting",
                          ),
                        })),
                      );
                    const depositConfig =
                      SubmitDeposit.parseSubmitDepositConfig({
                        l2Address: wallet.l2Address,
                        lovelace: lovelace.toString(10),
                        assetSpecs: [],
                      });
                    const submitted =
                      yield* SubmitDeposit.submitDepositWithMetadataProgram(
                        lucidService.api,
                        contracts,
                        {
                          ...depositConfig,
                          referenceScripts: depositReferenceScripts,
                        },
                      );
                    return {
                      txHash: submitted.txHash,
                      depositEventId: submitted.metadata.depositEventId,
                    };
                  }),
                ),
              ),
            projectDeposits: async () =>
              Effect.runPromise(
                provideNodeRuntimeServices(
                  fetchAndInsertDepositUTxOs.pipe(
                    Effect.andThen(projectDepositsToMempoolLedger),
                  ),
                ),
              ),
          },
        );
        writeJson(result);
      } catch (error) {
        failCli("stress-wallets:prepare", error);
      }
    },
  );

program
  .command("stress-wallets:fanout")
  .description(
    "Fund persisted L2 stress wallets from one already-funded L2 treasury wallet through a bounded L2 fan-out tree",
  )
  .requiredOption("--count <count>", "Number of stress wallets to fund")
  .requiredOption(
    "--lovelace-per-wallet <amount>",
    "Minimum verified lovelace per final stress wallet",
  )
  .option(
    "--treasury-wallet-seed-phrase-env <envVar>",
    "Environment variable containing the already-funded L2 treasury seed phrase",
    DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--endpoint <url>",
    "Midgard node endpoint used for /submit, /tx-status, and /utxos",
    defaultMidgardNodeEndpoint(),
  )
  .option("--start-index <index>", "First wallet index to use", "1")
  .option("--out-dir <path>", "Stress wallet directory", ".stress-wallets")
  .option(
    "--env-prefix <prefix>",
    "Environment variable prefix for generated stress wallet records",
    "STRESS_WALLET_SEED_PHRASE",
  )
  .option("--network <network>", "Wallet network; defaults to NETWORK env")
  .option("--create-missing", "Create missing wallet records before fanout")
  .option("--branch-factor <count>", "Fan-out tree branching factor", "16")
  .option(
    "--max-in-flight <count>",
    "Maximum parent wallets funding children concurrently per level",
    "32",
  )
  .option(
    "--fee-headroom-lovelace <amount>",
    "Per-transfer lovelace headroom reserved inside subtree budgets",
    "500000",
  )
  .option(
    "--acceptance-timeout-ms <ms>",
    "Per-transfer timeout waiting for accepted-or-later tx status",
    "300000",
  )
  .option(
    "--poll-initial-interval-ms <ms>",
    "Initial adaptive /tx-status poll interval",
    "250",
  )
  .option(
    "--poll-max-interval-ms <ms>",
    "Maximum adaptive /tx-status poll interval",
    "5000",
  )
  .action(
    async (options: {
      readonly count: string;
      readonly lovelacePerWallet: string;
      readonly treasuryWalletSeedPhraseEnv: string;
      readonly endpoint: string;
      readonly startIndex: string;
      readonly outDir: string;
      readonly envPrefix: string;
      readonly network?: string;
      readonly createMissing?: boolean;
      readonly branchFactor: string;
      readonly maxInFlight: string;
      readonly feeHeadroomLovelace: string;
      readonly acceptanceTimeoutMs: string;
      readonly pollInitialIntervalMs: string;
      readonly pollMaxIntervalMs: string;
    }) => {
      let treasurySeedPhrase: ResolvedWalletSeedPhrase;
      try {
        treasurySeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhraseEnv: options.treasuryWalletSeedPhraseEnv,
        });
      } catch (error) {
        failCli("stress-wallets:fanout", error);
        return;
      }

      try {
        const result = await Effect.runPromise(
          StressWalletsCommand.runWithSharedFanoutContext<
            Awaited<
              ReturnType<typeof StressWalletsCommand.fanoutStressWallets>
            >,
            | Services.Lucid
            | SqlClient.SqlClient
            | Services.BatchSql
            | Services.AdmissionSql
            | Services.WriteBehind
            | Services.NodeConfig
            | Services.ContractDeploymentIdentity
          >((runShared) =>
            StressWalletsCommand.fanoutStressWallets(
              {
                count: StressWalletsCommand.parseStressWalletCount(
                  options.count,
                  "--count",
                ),
                lovelacePerWallet:
                  StressWalletsCommand.parseStressWalletLovelace(
                    options.lovelacePerWallet,
                    "--lovelace-per-wallet",
                  ),
                treasurySeedPhrase: treasurySeedPhrase.seedPhrase,
                nodeEndpoint: options.endpoint,
                startIndex: StressWalletsCommand.parseStressWalletCount(
                  options.startIndex,
                  "--start-index",
                ),
                outDir: options.outDir,
                envPrefix: options.envPrefix,
                network: StressWalletsCommand.parseStressWalletNetwork(
                  options.network,
                ),
                createMissing: options.createMissing === true,
                branchFactor: StressWalletsCommand.parseStressWalletCount(
                  options.branchFactor,
                  "--branch-factor",
                ),
                maxInFlight: StressWalletsCommand.parseStressWalletCount(
                  options.maxInFlight,
                  "--max-in-flight",
                ),
                feeHeadroomLovelace:
                  StressWalletsCommand.parseStressWalletNonNegativeLovelace(
                    options.feeHeadroomLovelace,
                    "--fee-headroom-lovelace",
                  ),
                acceptanceTimeoutMs:
                  StressWalletsCommand.parseStressWalletNonNegativeMs(
                    options.acceptanceTimeoutMs,
                    "--acceptance-timeout-ms",
                  ),
                pollInitialIntervalMs:
                  StressWalletsCommand.parseStressWalletCount(
                    options.pollInitialIntervalMs,
                    "--poll-initial-interval-ms",
                  ),
                pollMaxIntervalMs: StressWalletsCommand.parseStressWalletCount(
                  options.pollMaxIntervalMs,
                  "--poll-max-interval-ms",
                ),
              },
              {
                submitTransfer: async ({ source, destination, lovelace }) => {
                  const sourceSeedPhrase =
                    source.kind === "treasury"
                      ? source.seedPhrase
                      : source.wallet.seedPhrase;
                  const sourceLabel =
                    source.kind === "treasury"
                      ? treasurySeedPhrase.resolvedFrom
                      : source.wallet.envName;
                  const transferConfig =
                    SubmitL2Transfer.parseSubmitL2TransferConfig({
                      l2Address: destination.l2Address,
                      lovelace: lovelace.toString(10),
                      assetSpecs: [],
                      nodeEndpoint: options.endpoint,
                      submissionMode: "api",
                    });
                  return runShared(
                    Effect.gen(function* () {
                      const lucidService = yield* Services.Lucid;
                      const submitted =
                        yield* SubmitL2Transfer.submitL2TransferProgram({
                          config: transferConfig,
                          apiSubmitRetryPolicy:
                            SubmitL2Transfer.FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
                          resolvedWalletSeedPhrase: {
                            seedPhrase: sourceSeedPhrase,
                            resolvedFrom: sourceLabel,
                          },
                          assertWalletAddress: (walletAddress) =>
                            assertUserCliWalletIsOperationallyIsolated({
                              commandName: "stress-wallets:fanout",
                              walletAddress,
                              operatorMainAddress:
                                lucidService.operatorMainAddress,
                              operatorMergeAddress:
                                lucidService.operatorMergeAddress,
                              referenceScriptsAddress:
                                lucidService.referenceScriptsWalletAddress,
                            }),
                        });
                      return {
                        txHash: submitted.txId,
                        status: submitted.status,
                      };
                    }),
                  );
                },
                fetchTxStatus: async (nodeEndpoint, txHash) => {
                  const response = await fetch(
                    `${nodeEndpoint}/tx-status?tx_hash=${encodeURIComponent(txHash)}`,
                  );
                  const body = (await response.json()) as {
                    readonly status?: unknown;
                  };
                  if (!response.ok || typeof body.status !== "string") {
                    throw new Error(
                      `Failed to read /tx-status for ${txHash}: ${response.status.toString()}`,
                    );
                  }
                  return body.status;
                },
              },
            ),
          ).pipe(
            Effect.provide(Services.WriteBehindLive),
            Effect.provide(Services.Lucid.Default),
            Effect.provide(Services.Database.layer),
            Effect.provide(Services.NodeConfig.layer),
            Effect.provide(Services.MidgardContractServices),
          ),
        );
        writeJson(result);
      } catch (error) {
        failCli("stress-wallets:fanout", error);
      }
    },
  );

program
  .command("stress-wallets:consolidate")
  .description(
    "Consolidate persisted L2 stress-wallet balances into a distinct L2 treasury with resumable, exact accounting",
  )
  .requiredOption("--count <count>", "Number of persisted stress wallets")
  .option(
    "--treasury-wallet-seed-phrase-env <envVar>",
    "Environment variable containing the destination L2 treasury seed phrase",
    DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--endpoint <url>",
    "Midgard node endpoint used for /submit, /tx-status, and /utxos",
    defaultMidgardNodeEndpoint(),
  )
  .option("--start-index <index>", "First wallet index to use", "1")
  .option("--out-dir <path>", "Stress wallet directory", ".stress-wallets")
  .option(
    "--env-prefix <prefix>",
    "Environment variable prefix recorded by the stress wallets",
    "STRESS_WALLET_SEED_PHRASE",
  )
  .option("--network <network>", "Wallet network; defaults to NETWORK env")
  .option(
    "--reserve-lovelace <amount>",
    "Amount excluded from each source transfer (fees are paid from that reserve)",
    "100000",
  )
  .option(
    "--required-treasury-lovelace <amount>",
    "Fail before submission unless the projected treasury reaches this amount",
  )
  .option(
    "--max-in-flight <count>",
    "Maximum independent source transfers in flight",
    "32",
  )
  .option(
    "--acceptance-timeout-ms <ms>",
    "Per-transfer timeout waiting for committed status",
    "300000",
  )
  .option(
    "--readiness-timeout-ms <ms>",
    "Timeout waiting for full node readiness between batches",
    "300000",
  )
  .option(
    "--verification-timeout-ms <ms>",
    "Timeout waiting for exact post-transfer UTxO accounting",
    "300000",
  )
  .option(
    "--request-timeout-ms <ms>",
    "Per-request deadline for /readyz, /tx-status, /utxos, and /submit",
    "30000",
  )
  .option(
    "--poll-initial-interval-ms <ms>",
    "Initial adaptive /tx-status poll interval",
    "250",
  )
  .option(
    "--poll-max-interval-ms <ms>",
    "Maximum adaptive /tx-status poll interval",
    "5000",
  )
  .action(
    async (options: {
      readonly count: string;
      readonly treasuryWalletSeedPhraseEnv: string;
      readonly endpoint: string;
      readonly startIndex: string;
      readonly outDir: string;
      readonly envPrefix: string;
      readonly network?: string;
      readonly reserveLovelace: string;
      readonly requiredTreasuryLovelace?: string;
      readonly maxInFlight: string;
      readonly acceptanceTimeoutMs: string;
      readonly readinessTimeoutMs: string;
      readonly verificationTimeoutMs: string;
      readonly requestTimeoutMs: string;
      readonly pollInitialIntervalMs: string;
      readonly pollMaxIntervalMs: string;
    }) => {
      let treasurySeedPhrase: ResolvedWalletSeedPhrase;
      try {
        treasurySeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhraseEnv: options.treasuryWalletSeedPhraseEnv,
        });
      } catch (error) {
        failCli("stress-wallets:consolidate", error);
        return;
      }
      try {
        const result = await Effect.runPromise(
          StressWalletsCommand.runWithSharedFanoutContext<
            Awaited<
              ReturnType<typeof StressWalletsCommand.consolidateStressWallets>
            >,
            | Services.Lucid
            | SqlClient.SqlClient
            | Services.BatchSql
            | Services.AdmissionSql
            | Services.WriteBehind
            | Services.NodeConfig
            | Services.ContractDeploymentIdentity
          >((runShared) =>
            StressWalletsCommand.consolidateStressWallets(
              {
                count: StressWalletsCommand.parseStressWalletCount(
                  options.count,
                  "--count",
                ),
                treasurySeedPhrase: treasurySeedPhrase.seedPhrase,
                nodeEndpoint: options.endpoint,
                startIndex: StressWalletsCommand.parseStressWalletCount(
                  options.startIndex,
                  "--start-index",
                ),
                outDir: options.outDir,
                envPrefix: options.envPrefix,
                network: StressWalletsCommand.parseStressWalletNetwork(
                  options.network,
                ),
                reserveLovelace:
                  StressWalletsCommand.parseStressWalletNonNegativeLovelace(
                    options.reserveLovelace,
                    "--reserve-lovelace",
                  ),
                requiredTreasuryLovelace:
                  options.requiredTreasuryLovelace === undefined
                    ? undefined
                    : StressWalletsCommand.parseStressWalletLovelace(
                        options.requiredTreasuryLovelace,
                        "--required-treasury-lovelace",
                      ),
                maxInFlight: StressWalletsCommand.parseStressWalletCount(
                  options.maxInFlight,
                  "--max-in-flight",
                ),
                acceptanceTimeoutMs:
                  StressWalletsCommand.parseStressWalletNonNegativeMs(
                    options.acceptanceTimeoutMs,
                    "--acceptance-timeout-ms",
                  ),
                readinessTimeoutMs:
                  StressWalletsCommand.parseStressWalletNonNegativeMs(
                    options.readinessTimeoutMs,
                    "--readiness-timeout-ms",
                  ),
                verificationTimeoutMs:
                  StressWalletsCommand.parseStressWalletNonNegativeMs(
                    options.verificationTimeoutMs,
                    "--verification-timeout-ms",
                  ),
                requestTimeoutMs: StressWalletsCommand.parseStressWalletCount(
                  options.requestTimeoutMs,
                  "--request-timeout-ms",
                ),
                pollInitialIntervalMs:
                  StressWalletsCommand.parseStressWalletCount(
                    options.pollInitialIntervalMs,
                    "--poll-initial-interval-ms",
                  ),
                pollMaxIntervalMs: StressWalletsCommand.parseStressWalletCount(
                  options.pollMaxIntervalMs,
                  "--poll-max-interval-ms",
                ),
              },
              {
                prepareTransfer: async ({
                  source,
                  treasuryAddress,
                  lovelace,
                }) => {
                  const transferConfig =
                    SubmitL2Transfer.parseSubmitL2TransferConfig({
                      l2Address: treasuryAddress,
                      lovelace: lovelace.toString(10),
                      assetSpecs: [],
                      nodeEndpoint: options.endpoint,
                      submissionMode: "api",
                      submitRequestTimeoutMs:
                        StressWalletsCommand.parseStressWalletCount(
                          options.requestTimeoutMs,
                          "--request-timeout-ms",
                        ),
                      utxoRequestTimeoutMs:
                        StressWalletsCommand.parseStressWalletCount(
                          options.requestTimeoutMs,
                          "--request-timeout-ms",
                        ),
                    });
                  return runShared(
                    Effect.gen(function* () {
                      const lucidService = yield* Services.Lucid;
                      const prepared =
                        yield* SubmitL2Transfer.prepareL2TransferProgram({
                          config: transferConfig,
                          resolvedWalletSeedPhrase: {
                            seedPhrase: source.seedPhrase,
                            resolvedFrom: source.envName,
                          },
                          assertWalletAddress: (walletAddress) =>
                            assertUserCliWalletIsOperationallyIsolated({
                              commandName: "stress-wallets:consolidate",
                              walletAddress,
                              operatorMainAddress:
                                lucidService.operatorMainAddress,
                              operatorMergeAddress:
                                lucidService.operatorMergeAddress,
                              referenceScriptsAddress:
                                lucidService.referenceScriptsWalletAddress,
                            }),
                        });
                      return {
                        txHash: prepared.txId,
                        signedTxCbor: prepared.signedTxCbor,
                        selectedInputs: prepared.selectedInputs,
                      };
                    }),
                  );
                },
                submitPreparedTransfer: async ({
                  nodeEndpoint,
                  txHash,
                  signedTxCbor,
                }) => {
                  const submitted = await runShared(
                    SubmitL2Transfer.submitNativeTransferTx(
                      nodeEndpoint,
                      signedTxCbor,
                      txHash,
                      StressWalletsCommand.parseStressWalletCount(
                        options.requestTimeoutMs,
                        "--request-timeout-ms",
                      ),
                      SubmitL2Transfer.FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
                    ),
                  );
                  return {
                    txHash: submitted.txId,
                    status: submitted.status,
                  };
                },
                fetchTxStatus: (nodeEndpoint, txHash) =>
                  fetchNodeTxStatus(
                    nodeEndpoint,
                    txHash,
                    StressWalletsCommand.parseStressWalletCount(
                      options.requestTimeoutMs,
                      "--request-timeout-ms",
                    ),
                  ),
              },
            ),
          ).pipe(
            Effect.provide(Services.WriteBehindLive),
            Effect.provide(Services.Lucid.Default),
            Effect.provide(Services.Database.layer),
            Effect.provide(Services.NodeConfig.layer),
            Effect.provide(Services.MidgardContractServices),
          ),
        );
        writeJson(result);
      } catch (error) {
        failCli("stress-wallets:consolidate", error);
      }
    },
  );

program
  .command("stress-wallets:terminal-drain")
  .description(
    "Prepare or execute a crash-safe exact-zero sweep of every persisted L2 stress wallet into a distinct L2 treasury",
  )
  .requiredOption("--count <count>", "Number of persisted stress wallets")
  .option(
    "--treasury-wallet-seed-phrase-env <envVar>",
    "Environment variable containing the destination L2 treasury seed phrase",
    DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--endpoint <url>",
    "Midgard node endpoint",
    defaultMidgardNodeEndpoint(),
  )
  .option("--start-index <index>", "First wallet index", "1")
  .option("--out-dir <path>", "Stress wallet directory", ".stress-wallets")
  .option(
    "--env-prefix <prefix>",
    "Stress wallet environment prefix",
    "STRESS_WALLET_SEED_PHRASE",
  )
  .option("--network <network>", "Wallet network; defaults to NETWORK env")
  .option(
    "--fee-cap-lovelace <amount>",
    "Maximum fee allowed for each terminal sweep",
    "100000",
  )
  .option(
    "--max-fee-iterations <count>",
    "Maximum monotonic signed-byte fee convergence iterations",
    "32",
  )
  .option(
    "--max-in-flight <count>",
    "Maximum parallel read/prepare operations",
    "32",
  )
  .option(
    "--prepare-only",
    "Durably prepare and validate all wallet transactions, then stop before submission",
  )
  .option(
    "--acceptance-timeout-ms <ms>",
    "Per-transfer commitment timeout",
    "300000",
  )
  .option(
    "--verification-timeout-ms <ms>",
    "Exact-zero/conservation verification timeout",
    "300000",
  )
  .option("--request-timeout-ms <ms>", "Per-request deadline", "30000")
  .option(
    "--poll-initial-interval-ms <ms>",
    "Initial status poll interval",
    "250",
  )
  .option("--poll-max-interval-ms <ms>", "Maximum status poll interval", "5000")
  .action(
    async (options: {
      readonly count: string;
      readonly treasuryWalletSeedPhraseEnv: string;
      readonly endpoint: string;
      readonly startIndex: string;
      readonly outDir: string;
      readonly envPrefix: string;
      readonly network?: string;
      readonly feeCapLovelace: string;
      readonly maxFeeIterations: string;
      readonly maxInFlight: string;
      readonly prepareOnly?: boolean;
      readonly acceptanceTimeoutMs: string;
      readonly verificationTimeoutMs: string;
      readonly requestTimeoutMs: string;
      readonly pollInitialIntervalMs: string;
      readonly pollMaxIntervalMs: string;
    }) => {
      let treasurySeedPhrase: ResolvedWalletSeedPhrase;
      try {
        treasurySeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhraseEnv: options.treasuryWalletSeedPhraseEnv,
        });
      } catch (error) {
        failCli("stress-wallets:terminal-drain", error);
        return;
      }
      try {
        const parsedNetwork = StressWalletsCommand.parseStressWalletNetwork(
          options.network,
        );
        const requestTimeoutMs = StressWalletsCommand.parseStressWalletCount(
          options.requestTimeoutMs,
          "--request-timeout-ms",
        );
        const result = await Effect.runPromise(
          StressWalletsCommand.runWithSharedFanoutContext<
            Awaited<
              ReturnType<typeof StressWalletsCommand.terminalDrainStressWallets>
            >,
            | Services.Lucid
            | SqlClient.SqlClient
            | Services.BatchSql
            | Services.AdmissionSql
            | Services.WriteBehind
            | Services.NodeConfig
            | Services.ContractDeploymentIdentity
          >(async (runShared) => {
            const fees = await runShared(
              Effect.gen(function* () {
                const config = yield* Services.NodeConfig;
                return { minFeeA: config.MIN_FEE_A, minFeeB: config.MIN_FEE_B };
              }),
            );
            return StressWalletsCommand.terminalDrainStressWallets(
              {
                count: StressWalletsCommand.parseStressWalletCount(
                  options.count,
                  "--count",
                ),
                treasurySeedPhrase: treasurySeedPhrase.seedPhrase,
                nodeEndpoint: options.endpoint,
                startIndex: StressWalletsCommand.parseStressWalletCount(
                  options.startIndex,
                  "--start-index",
                ),
                outDir: options.outDir,
                envPrefix: options.envPrefix,
                network: parsedNetwork,
                minFeeA: fees.minFeeA,
                minFeeB: fees.minFeeB,
                feeCapLovelace:
                  StressWalletsCommand.parseStressWalletNonNegativeLovelace(
                    options.feeCapLovelace,
                    "--fee-cap-lovelace",
                  ),
                maxFeeIterations: StressWalletsCommand.parseStressWalletCount(
                  options.maxFeeIterations,
                  "--max-fee-iterations",
                ),
                maxInFlight: StressWalletsCommand.parseStressWalletCount(
                  options.maxInFlight,
                  "--max-in-flight",
                ),
                prepareOnly: options.prepareOnly ?? false,
                acceptanceTimeoutMs:
                  StressWalletsCommand.parseStressWalletNonNegativeMs(
                    options.acceptanceTimeoutMs,
                    "--acceptance-timeout-ms",
                  ),
                verificationTimeoutMs:
                  StressWalletsCommand.parseStressWalletNonNegativeMs(
                    options.verificationTimeoutMs,
                    "--verification-timeout-ms",
                  ),
                requestTimeoutMs,
                pollInitialIntervalMs:
                  StressWalletsCommand.parseStressWalletCount(
                    options.pollInitialIntervalMs,
                    "--poll-initial-interval-ms",
                  ),
                pollMaxIntervalMs: StressWalletsCommand.parseStressWalletCount(
                  options.pollMaxIntervalMs,
                  "--poll-max-interval-ms",
                ),
              },
              {
                prepareTransfer: async ({ source, treasuryAddress }) => {
                  const prepared = await runShared(
                    Effect.gen(function* () {
                      const lucidService = yield* Services.Lucid;
                      return yield* SubmitL2Transfer.prepareL2TerminalDrainProgram(
                        {
                          destinationAddress: treasuryAddress,
                          nodeEndpoint: options.endpoint,
                          requestTimeoutMs,
                          networkId: parsedNetwork === "Mainnet" ? 1n : 0n,
                          feeCap:
                            StressWalletsCommand.parseStressWalletNonNegativeLovelace(
                              options.feeCapLovelace,
                              "--fee-cap-lovelace",
                            ),
                          maxFeeIterations:
                            StressWalletsCommand.parseStressWalletCount(
                              options.maxFeeIterations,
                              "--max-fee-iterations",
                            ),
                          resolvedWalletSeedPhrase: {
                            seedPhrase: source.seedPhrase,
                            resolvedFrom: source.envName,
                          },
                          assertWalletAddress: (walletAddress) =>
                            assertUserCliWalletIsOperationallyIsolated({
                              commandName: "stress-wallets:terminal-drain",
                              walletAddress,
                              operatorMainAddress:
                                lucidService.operatorMainAddress,
                              operatorMergeAddress:
                                lucidService.operatorMergeAddress,
                              referenceScriptsAddress:
                                lucidService.referenceScriptsWalletAddress,
                            }),
                        },
                      );
                    }),
                  );
                  return {
                    txHash: prepared.txId,
                    signedTxCbor: prepared.signedTxCbor,
                    selectedInputs: prepared.selectedInputs,
                    requestedLovelace: prepared.requestedLovelace,
                    feeLovelace: prepared.feeLovelace,
                    signedTxBytes: prepared.signedTxBytes,
                  };
                },
                submitPreparedTransfer: async ({
                  nodeEndpoint,
                  txHash,
                  signedTxCbor,
                }) => {
                  const submitted = await runShared(
                    SubmitL2Transfer.submitNativeTransferTx(
                      nodeEndpoint,
                      signedTxCbor,
                      txHash,
                      requestTimeoutMs,
                      SubmitL2Transfer.FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
                    ),
                  );
                  return { txHash: submitted.txId, status: submitted.status };
                },
                fetchTxStatus: (nodeEndpoint, txHash) =>
                  fetchNodeTxStatus(nodeEndpoint, txHash, requestTimeoutMs),
              },
            );
          }).pipe(
            Effect.provide(Services.WriteBehindLive),
            Effect.provide(Services.Lucid.Default),
            Effect.provide(Services.Database.layer),
            Effect.provide(Services.NodeConfig.layer),
            Effect.provide(Services.MidgardContractServices),
          ),
        );
        writeJson(result);
      } catch (error) {
        failCli("stress-wallets:terminal-drain", error);
      }
    },
  );

program
  .command("stress-corpus-generate")
  .description(
    "Pre-build and verify an offline NDJSON corpus of signed Midgard L2 stress transactions",
  )
  .requiredOption("--target-rate-tps <rate>", "Target offered TPS")
  .requiredOption("--duration-ms <ms>", "Measured run duration in milliseconds")
  .option("--warmup-count <count>", "Warmup rows to reserve", "0")
  .option("--cooldown-count <count>", "Cooldown rows to reserve", "0")
  .option("--wallet-count <count>", "Override generated chain count")
  .option("--safety-factor <factor>", "Sizing safety factor", "1.1")
  .option("--amount-lovelace <amount>", "Self-transfer amount", "1000000")
  .option("--min-fee-a <amount>", "Midgard MIN_FEE_A; defaults to env")
  .option("--min-fee-b <amount>", "Midgard MIN_FEE_B; defaults to env")
  .option(
    "--max-submit-tx-cbor-bytes <bytes>",
    "Midgard MAX_SUBMIT_TX_CBOR_BYTES; defaults to env",
  )
  .option(
    "--assumed-acceptance-latency-ms <ms>",
    "Acceptance-latency bound used for wallet-count safety checks",
    "1000",
  )
  .option("--wallets-dir <path>", "Prepared stress wallet directory")
  .option("--out-dir <path>", "Output directory for corpus artifacts")
  .option("--workers <count>", "Worker thread count")
  .option("--slices <count>", "Number of corpus slice ids", "1")
  .option(
    "--slice-wallet-counts <counts>",
    "Comma-separated wallet counts for ordered, dependency-isolated slices (must sum to --wallet-count)",
  )
  .option("--corpus-slice-id-prefix <id>", "Corpus slice id prefix", "default")
  .option(
    "--rebuild-sample-rate <rate>",
    "Fraction of chains to rebuild and byte-compare during verification",
    "0.001",
  )
  .option(
    "--funding-source <source>",
    "Funding source mode: existing or fanout",
    "existing",
  )
  .option("--network <network>", "Mainnet or Preprod; defaults to NETWORK env")
  .option("--yes", "Confirm generation")
  .action(async (options) => {
    try {
      const config =
        StressCorpusCommand.parseStressCorpusGenerateConfig(options);
      const result = await StressCorpusCommand.generateStressCorpus(config);
      writeJson(result);
    } catch (error) {
      failCli("stress-corpus-generate", error);
    }
  });

program
  .command("stress-corpus-verify")
  .description("Stream-verify a generated Midgard stress corpus and sidecars")
  .requiredOption("--corpus-path <path>", "Corpus NDJSON path")
  .option("--index-path <path>", "Corpus index path")
  .option("--manifest-path <path>", "Corpus manifest path")
  .option(
    "--result-out <path>",
    "Write a SHA-bindable standalone verification result artifact",
  )
  .option(
    "--rebuild-wallets-dir <path>",
    "Prepared stress wallet directory for rebuild-sample verification",
  )
  .option(
    "--rebuild-sample-rate <rate>",
    "Fraction of chains to rebuild and byte-compare when --rebuild-wallets-dir is set",
    "0.001",
  )
  .option("--amount-lovelace <amount>", "Self-transfer amount", "1000000")
  .option("--min-fee-a <amount>", "Midgard MIN_FEE_A; defaults to env")
  .option("--min-fee-b <amount>", "Midgard MIN_FEE_B; defaults to env")
  .option(
    "--max-submit-tx-cbor-bytes <bytes>",
    "Midgard MAX_SUBMIT_TX_CBOR_BYTES; defaults to env",
  )
  .option("--network <network>", "Mainnet or Preprod; defaults to NETWORK env")
  .action(async (options) => {
    try {
      const config = StressCorpusCommand.parseStressCorpusVerifyConfig(options);
      const result = await StressCorpusCommand.verifyStressCorpus(config);
      writeJson(result);
    } catch (error) {
      failCli("stress-corpus-verify", error);
    }
  });

program
  .command("phase4-genesis-ledger")
  .description(
    "Explicitly seed or verify the complete configured L2 genesis set and A/B funding in an isolated Phase 4 local-devnet database",
  )
  .option("--seed", "Seed an empty run-scoped mempool ledger")
  .option(
    "--verify-only",
    "Require the complete byte-identical configured genesis ledger without mutating it",
  )
  .action((opts) => {
    const seed = opts.seed === true;
    const verifyOnly = opts.verifyOnly === true;
    if (seed === verifyOnly) {
      failCli(
        "phase4-genesis-ledger",
        new Error("Specify exactly one of --seed or --verify-only"),
      );
      return;
    }
    const mainEffect = Phase4GenesisLedgerCommand.phase4GenesisLedgerProgram({
      mode: seed ? "seed" : "verify",
    }).pipe(Effect.provide(Services.Database.layerWithNodeConfig), tapJson());
    runCliEffect(mainEffect);
  });

program
  .command("e2e-finalize-summary")
  .description(
    "Collect final e2e endpoint/database evidence and write summary.json plus summary.md",
  )
  .option("--out-dir <path>", "Output directory for summary artifacts")
  .option("--run-id <id>", "Stable run id for the summary")
  .option("--mode <mode>", "Run mode: attach, resume, fresh, or unknown")
  .option("--node-url <url>", "Midgard node URL")
  .option(
    "--admin-api-key-env <name>",
    "Environment variable that contains the admin API key",
    "ADMIN_API_KEY",
  )
  .option("--node-log <path>", "Raw Midgard node log artifact to link")
  .option(
    "--step-summary <path>",
    "Structured e2e-run-step summary JSON file to include; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--tx <label:txHash:status:source>",
    "Transaction evidence to include in the summary; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--stress-summary <path>",
    "Optional e2e-stress-l2-throughput summary.json artifact to include as a functional gate",
  )
  .option(
    "--state-correction-evidence <path>",
    "Launch-scope fault-proof aggregate claim; cannot satisfy acceptance without the independent source options",
  )
  .option(
    "--state-correction-deployment-manifest <path>",
    "Finalized Preprod deployment manifest independently loaded for state-correction acceptance",
  )
  .option(
    "--state-correction-blueprint <path>",
    "Aiken blueprint independently hashed for state-correction acceptance",
  )
  .option(
    "--state-correction-catalogue <path>",
    "Fraud-proof catalogue JSON independently matched to the deployment manifest",
  )
  .option(
    "--state-correction-parameters <path>",
    "Cardano protocol-parameter snapshot independently digested for state-correction acceptance",
  )
  .option(
    "--state-correction-release-evidence <path>",
    "Release-evidence artifact independently hashed for state-correction acceptance",
  )
  .option(
    "--state-correction-workflow-journal <directory>",
    "Immutable completed family workflow journal directory; repeat once per launch-scope family",
    collectStringOption,
    [],
  )
  .option(
    "--state-correction-l1-observation <path>",
    "Authenticated local Kupmios/Ogmios L1 transaction observation; repeat for every required transaction",
    collectStringOption,
    [],
  )
  .option(
    "--state-correction-recovery-observation <path>",
    "Raw structured recovery drill observation; repeat in canonical recovery-matrix order",
    collectStringOption,
    [],
  )
  .option(
    "--state-correction-final-snapshot <path>",
    "Authenticated final chain/queue/economic/withdrawal/classification snapshot",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mode =
      opts.mode === "attach" ||
      opts.mode === "resume" ||
      opts.mode === "fresh" ||
      opts.mode === "unknown"
        ? opts.mode
        : "unknown";
    const adminApiKey =
      typeof opts.adminApiKeyEnv === "string"
        ? process.env[opts.adminApiKeyEnv]
        : undefined;
    const stateCorrectionWorkflowJournalDirectories = parseStringListOption(
      opts.stateCorrectionWorkflowJournal,
      "--state-correction-workflow-journal",
    );
    const stateCorrectionL1ObservationPaths = parseStringListOption(
      opts.stateCorrectionL1Observation,
      "--state-correction-l1-observation",
    );
    const stateCorrectionRecoveryObservationPaths = parseStringListOption(
      opts.stateCorrectionRecoveryObservation,
      "--state-correction-recovery-observation",
    );
    const stateCorrectionSingleSourceValues = [
      opts.stateCorrectionDeploymentManifest,
      opts.stateCorrectionBlueprint,
      opts.stateCorrectionCatalogue,
      opts.stateCorrectionParameters,
      opts.stateCorrectionReleaseEvidence,
      opts.stateCorrectionFinalSnapshot,
    ];
    const hasAnyStateCorrectionIndependentSource =
      stateCorrectionSingleSourceValues.some(
        (value) => typeof value === "string",
      ) ||
      stateCorrectionWorkflowJournalDirectories.length > 0 ||
      stateCorrectionL1ObservationPaths.length > 0 ||
      stateCorrectionRecoveryObservationPaths.length > 0;
    const hasAllStateCorrectionIndependentSources =
      stateCorrectionSingleSourceValues.every(
        (value) => typeof value === "string",
      ) &&
      stateCorrectionWorkflowJournalDirectories.length > 0 &&
      stateCorrectionL1ObservationPaths.length > 0 &&
      stateCorrectionRecoveryObservationPaths.length > 0;
    if (
      hasAnyStateCorrectionIndependentSource &&
      !hasAllStateCorrectionIndependentSources
    ) {
      throw new Error(
        "State-correction independent reconciliation requires manifest, blueprint, catalogue, parameters, release evidence, at least one workflow journal, at least one authenticated L1 observation, at least one recovery observation, and the final snapshot together.",
      );
    }
    const mainEffect = provideDatabaseServices(
      E2EFinalizeSummaryCommand.finalizeE2ESummaryProgram({
        ...(typeof opts.outDir === "string" ? { outDir: opts.outDir } : {}),
        ...(typeof opts.runId === "string" ? { runId: opts.runId } : {}),
        mode,
        ...(typeof opts.nodeUrl === "string" ? { nodeUrl: opts.nodeUrl } : {}),
        ...(adminApiKey === undefined ? {} : { adminApiKey }),
        ...(typeof opts.nodeLog === "string"
          ? { nodeLogPath: opts.nodeLog }
          : {}),
        stepSummaryPaths: parseStringListOption(
          opts.stepSummary,
          "--step-summary",
        ),
        transactions: parseTxEvidenceOptions(opts.tx),
        ...(typeof opts.stressSummary === "string"
          ? { stressSummaryPath: opts.stressSummary }
          : {}),
        ...(typeof opts.stateCorrectionEvidence === "string"
          ? {
              stateCorrectionEvidencePath: opts.stateCorrectionEvidence,
            }
          : {}),
        ...(hasAllStateCorrectionIndependentSources
          ? {
              stateCorrectionIndependentSourcePaths: {
                deploymentManifestPath:
                  opts.stateCorrectionDeploymentManifest as string,
                blueprintPath: opts.stateCorrectionBlueprint as string,
                cataloguePath: opts.stateCorrectionCatalogue as string,
                parametersPath: opts.stateCorrectionParameters as string,
                releaseEvidencePath:
                  opts.stateCorrectionReleaseEvidence as string,
                workflowJournalDirectories:
                  stateCorrectionWorkflowJournalDirectories,
                l1ObservationPaths: stateCorrectionL1ObservationPaths,
                recoveryObservationPaths:
                  stateCorrectionRecoveryObservationPaths,
                finalSnapshotPath: opts.stateCorrectionFinalSnapshot as string,
              },
              stateCorrectionLocalAuthorityConfig: l1KupmiosEnvironment(),
            }
          : {}),
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

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

program
  .command("e2e-clean-owned-process-group")
  .description(
    "Fail-closed cleanup of a detached e2e process group after validating its durable /proc ownership record",
  )
  .requiredOption("--record <path>", "Owned process-group record path")
  .requiredOption(
    "--run-token-env <name>",
    "Environment variable containing the private run token",
  )
  .action(
    async (options: {
      readonly record: string;
      readonly runTokenEnv: string;
    }) => {
      try {
        const result =
          await E2EProcessCleanupCommand.cleanupOwnedProcessGroupFromEnv({
            recordPath: options.record,
            runTokenEnv: options.runTokenEnv,
          });
        writeJson(result);
        if (!result.success) process.exitCode = 1;
      } catch (error) {
        failCli("e2e-clean-owned-process-group", error);
      }
    },
  );

program
  .command("phase4-t1-probe")
  .description(
    "Gated read-only canonical state_queue probe for the matched local-devnet T1 recovery gate",
  )
  .requiredOption(
    "--snapshot-identity-sha256 <hex>",
    "Matched-snapshot identity SHA-256",
  )
  .requiredOption("--attempt-id <id>", "Fresh T1 recovery attempt identity")
  .requiredOption(
    "--evidence-out <absolute-path>",
    "Fresh output file for exact probe evidence",
  )
  .option("--expected-tip-header-hash <hex>", "Expected 28-byte L2 tip hash")
  .option(
    "--expected-present-header-hash <hex>",
    "28-byte L2 header hash that must be canonical",
  )
  .option(
    "--expected-absent-header-hash <hex>",
    "28-byte L2 header hash that must not be canonical",
  )
  .action((opts) => {
    const mainEffect = provideTxServices(
      Phase4T1RecoveryCommand.phase4T1ProbeProgram({
        snapshotIdentitySha256: opts.snapshotIdentitySha256,
        attemptId: opts.attemptId,
        expectedTipHeaderHash: opts.expectedTipHeaderHash,
        expectedPresentHeaderHash: opts.expectedPresentHeaderHash,
        expectedAbsentHeaderHash: opts.expectedAbsentHeaderHash,
      }).pipe(
        Effect.tap((evidence) =>
          Effect.promise(() =>
            Phase4T1RecoveryCommand.writePhase4T1Evidence(
              opts.evidenceOut,
              evidence,
            ),
          ),
        ),
      ),
    );
    runCliEffect(mainEffect);
  });

program
  .command("phase4-t1-advance")
  .description(
    "Gated authenticated no-op canonical L2 advance for the matched local-devnet T1 recovery gate",
  )
  .requiredOption(
    "--snapshot-identity-sha256 <hex>",
    "Matched-snapshot identity SHA-256",
  )
  .requiredOption("--attempt-id <id>", "Fresh T1 recovery attempt identity")
  .requiredOption(
    "--expected-base-header-hash <hex>",
    "Expected 28-byte canonical L2 base B",
  )
  .requiredOption(
    "--abandoned-header-hash <hex>",
    "Submitted 28-byte L2 header N that must be absent",
  )
  .requiredOption(
    "--minimum-end-time-ms <ms>",
    "Minimum end time F must reach to advance past N",
  )
  .requiredOption(
    "--evidence-out <absolute-path>",
    "Fresh output file for exact canonical-advance evidence",
  )
  .action((opts) => {
    const mainEffect = provideTxServices(
      Phase4T1RecoveryCommand.phase4T1AdvanceProgram({
        snapshotIdentitySha256: opts.snapshotIdentitySha256,
        attemptId: opts.attemptId,
        expectedBaseHeaderHash: opts.expectedBaseHeaderHash,
        abandonedHeaderHash: opts.abandonedHeaderHash,
        minimumEndTimeMs: parsePositiveIntegerOption(
          opts.minimumEndTimeMs,
          "--minimum-end-time-ms",
        ),
      }).pipe(
        Effect.tap((evidence) =>
          Effect.promise(() =>
            Phase4T1RecoveryCommand.writePhase4T1Evidence(
              opts.evidenceOut,
              evidence,
            ),
          ),
        ),
      ),
    );
    runCliEffect(mainEffect);
  });

program
  .command("e2e-pipelined-commit-process-acceptance")
  .description(
    "Run the operator-enabled Phase 4 crash/restart and two-node process acceptance matrix against a matched local-devnet snapshot",
  )
  .action(async () => {
    try {
      console.log(
        JSON.stringify(await runPipelinedCommitProcessAcceptance(), null, 2),
      );
    } catch (error) {
      failCli("e2e-pipelined-commit-process-acceptance", error);
    }
  });

program
  .command("e2e-run-step")
  .description("Run one acceptance command through the structured e2e runner")
  .requiredOption("--id <id>", "Step id")
  .requiredOption("--cwd <path>", "Working directory")
  .requiredOption("--raw-log <path>", "Raw log path")
  .option("--summary-out <path>", "Write the step summary JSON to this path")
  .option("--timeout-ms <ms>", "Step timeout in milliseconds")
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .argument("<command>", "Command to execute")
  .argument("[args...]", "Command arguments")
  .action(async (command, args, opts) => {
    const timeoutMs =
      typeof opts.timeoutMs === "string"
        ? parsePositiveIntegerOption(opts.timeoutMs, "--timeout-ms")
        : undefined;
    try {
      const summary = await runCommandStep({
        id: opts.id,
        command,
        args,
        cwd: opts.cwd,
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        env: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        envInheritance: parseE2EEnvInheritanceOption(opts.envInheritance),
        rawLogPath: opts.rawLog,
        ...(timeoutMs === undefined ? {} : { timeoutMs }),
      });
      if (typeof opts.summaryOut === "string" && opts.summaryOut.length > 0) {
        await mkdir(dirname(opts.summaryOut), { recursive: true });
        await writeFile(
          opts.summaryOut,
          `${JSON.stringify(summary, null, 2)}\n`,
          "utf8",
        );
      }
      writeJson(summary);
      if (summary.status !== "success") {
        process.exitCode = 1;
      }
    } catch (error) {
      failCli("e2e-run-step", error);
    }
  });

program
  .command("e2e-start-service")
  .description(
    "Start a long-running e2e service, write a PID file, and wait for readiness",
  )
  .requiredOption("--service <name>", "Service label")
  .requiredOption("--cwd <path>", "Working directory")
  .requiredOption("--raw-log <path>", "Raw log path")
  .requiredOption("--pid-file <path>", "PID file path")
  .requiredOption("--ready-url <url>", "Readiness endpoint URL")
  .option("--health-url <url>", "Optional health endpoint URL")
  .option("--ready-timeout-ms <ms>", "Readiness timeout", "120000")
  .option("--poll-interval-ms <ms>", "Readiness polling interval", "5000")
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .argument("<command>", "Command to execute")
  .argument("[args...]", "Command arguments")
  .action(async (command, args, opts) => {
    try {
      const summary = await E2EServiceCommand.startManagedService({
        service: opts.service,
        command,
        args,
        cwd: opts.cwd,
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        env: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        envInheritance: parseE2EEnvInheritanceOption(opts.envInheritance),
        rawLogPath: opts.rawLog,
        pidFilePath: opts.pidFile,
        readyUrl: opts.readyUrl,
        ...(typeof opts.healthUrl === "string"
          ? { healthUrl: opts.healthUrl }
          : {}),
        readyTimeoutMs: parsePositiveIntegerOption(
          opts.readyTimeoutMs,
          "--ready-timeout-ms",
        ),
        pollIntervalMs: parsePositiveIntegerOption(
          opts.pollIntervalMs,
          "--poll-interval-ms",
        ),
      });
      writeJson(summary);
    } catch (error) {
      failCli("e2e-start-service", error);
    }
  });

program.parse(process.argv);
