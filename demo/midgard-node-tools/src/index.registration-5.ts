import "./index.registration-4.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {
  assertUserCliWalletIsOperationallyIsolated,
  failCli,
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
import * as Services from "midgard-node/services/index";

import * as StressWalletsCommand from "./commands/stress-wallets/index.js";
import { program } from "./index.registration.js";

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
