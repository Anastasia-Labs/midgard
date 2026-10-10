import "./index.registration-2.js";

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
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "midgard-node/commands/command-utils";
import * as SubmitL2Transfer from "midgard-node/commands/submit-l2-transfer";
import * as Services from "midgard-node/services/index";

import * as StressWalletsCommand from "./commands/stress-wallets/index.js";
import { program } from "./index.registration.js";

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
            Effect.provide(ToolLucidLive),
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
