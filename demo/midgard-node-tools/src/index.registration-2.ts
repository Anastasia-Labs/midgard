import { randomUUID } from "node:crypto";
import { join } from "node:path";

import { defaultUserCliDeps, runDeposit } from "@al-ft/midgard-user-cli";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  assertUserCliWalletIsOperationallyIsolated,
  failCli,
  writeJson,
} from "midgard-node/commands/cli-runtime";
import {
  defaultMidgardNodeEndpoint,
  resolveWalletSeedPhrase,
} from "midgard-node/commands/command-utils";
import * as Services from "midgard-node/services/index";

import * as StressWalletsCommand from "./commands/stress-wallets/index.js";
import { l1KupmiosEnvironment } from "./environment.js";
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
    "--manifest <path>",
    "Deployment manifest the deposits target (env MIDGARD_DEPLOYMENT_MANIFEST)",
  )
  .option(
    "--kupo-url <url>",
    "Kupo the deposits read L1 through; it must match every address",
    l1KupmiosEnvironment().kupoUrl,
  )
  .option(
    "--ogmios-url <url>",
    "Ogmios the deposits read and submit through",
    l1KupmiosEnvironment().ogmiosUrl,
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
      readonly manifest?: string;
      readonly kupoUrl: string;
      readonly ogmiosUrl: string;
      readonly projectionWaitMs: string;
      readonly verifyTimeoutMs: string;
      readonly pollIntervalMs: string;
      readonly createMissing?: boolean;
      readonly forceFundExisting?: boolean;
    }) => {
      try {
        // Operator tooling: never fund stress wallets from a node wallet.
        const fundingWalletSeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhraseEnv: options.fundingWalletSeedPhraseEnv,
        });
        const operational = await Effect.runPromise(
          Services.NodeConfig.pipe(Effect.provide(Services.NodeConfig.layer)),
        );
        assertUserCliWalletIsOperationallyIsolated({
          commandName: "stress-wallets:prepare",
          walletAddress: walletFromSeed(fundingWalletSeedPhrase.seedPhrase, {
            network: operational.NETWORK,
          }).address,
          operatorMainAddress: walletFromSeed(
            operational.L1_OPERATOR_SEED_PHRASE,
            { network: operational.NETWORK },
          ).address,
          operatorMergeAddress: walletFromSeed(
            operational.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
            { network: operational.NETWORK },
          ).address,
          referenceScriptsAddress: operational.L1_REFERENCE_SCRIPT_ADDRESS,
        });
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
            // One deposit at a time, each awaited to confirmation (prepare
            // loops in order): the funding wallet never runs two at once.
            submitDeposit: async ({ wallet, lovelace }) => {
              const outcome = await runDeposit(
                {
                  submissionId: `stress-wallet-${randomUUID()}`,
                  manifest: options.manifest,
                  l1: "kupmios",
                  kupoUrl: options.kupoUrl,
                  ogmiosUrl: options.ogmiosUrl,
                  recordDir: join(options.outDir, "deposit-records"),
                  walletSeedPhraseEnv: options.fundingWalletSeedPhraseEnv,
                  l2Address: wallet.l2Address,
                  lovelace: lovelace.toString(10),
                  asset: [],
                },
                { ...defaultUserCliDeps(), stdout: () => {} },
              );
              return {
                txHash: outcome.admissionTxHash,
                depositEventId: outcome.eventId,
              };
            },
          },
        );
        writeJson(result);
      } catch (error) {
        failCli("stress-wallets:prepare", error);
      }
    },
  );
