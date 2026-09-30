import { randomUUID } from "node:crypto";

import { Effect } from "effect";
import {
  assertUserCliWalletIsOperationallyIsolated,
  failCli,
  provideDatabaseTxServices,
  writeJson,
} from "midgard-node/commands/cli-runtime";
import {
  defaultMidgardNodeEndpoint,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "midgard-node/commands/command-utils";
import * as Services from "midgard-node/services/index";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "midgard-node/transactions/reference-scripts";
import * as SubmitDeposit from "midgard-node/transactions/submit-deposit";

import * as StressWalletsCommand from "./commands/stress-wallets/index.js";
import { program } from "./index.registration.js";

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
    "Delay after submitted deposits before polling /utxos for their funding",
    StressWalletsCommand.DEFAULT_PROJECTION_WAIT_MS.toString(),
  )
  .option(
    "--verify-timeout-ms <ms>",
    "Maximum time to poll /utxos for confirmed L2 funding",
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
                    const submissionId = `stress-wallet-${randomUUID()}`;
                    yield* Effect.logInfo(
                      `Deposit submission ID: ${submissionId}`,
                    );
                    const submitted =
                      yield* SubmitDeposit.submitDepositWithMetadataProgram(
                        lucidService.api,
                        contracts,
                        {
                          ...depositConfig,
                          referenceScripts: depositReferenceScripts,
                        },
                        submissionId,
                      );
                    return {
                      txHash: submitted.txHash,
                      depositEventId: submitted.metadata.depositEventId,
                    };
                  }),
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
