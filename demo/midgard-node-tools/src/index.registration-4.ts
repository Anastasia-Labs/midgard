import "./index.registration-3.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {
  assertUserCliWalletIsOperationallyIsolated,
  failCli,
  ToolLucidLive,
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
            Effect.provide(ToolLucidLive),
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
