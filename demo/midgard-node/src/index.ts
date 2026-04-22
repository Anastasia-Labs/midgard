#!/usr/bin/env node

import { Command } from "commander";
import { ENV_VARS_GUIDE, chalk } from "@/utils.js";
import { runNode } from "@/commands/index.js";
import { auditBlocksImmutableProgram } from "@/commands/audit-blocks-immutable.js";
import * as ContractDeploymentInfo from "@/commands/contract-deployment-info.js";
import * as AddressFromSeed from "@/commands/address-from-seed.js";
import * as L1UtxosCommand from "@/commands/l1-utxos.js";
import * as SubmitL2Transfer from "@/commands/submit-l2-transfer.js";
import * as UtxosCommand from "@/commands/utxos.js";
import * as Services from "@/services/index.js";
import {
  fetchAndInsertDepositUTxOs,
  projectDepositsToMempoolLedger,
} from "@/fibers/index.js";
import * as RegisterActiveOperator from "@/transactions/register-active-operator.js";
import * as Initialization from "@/transactions/initialization.js";
import * as SubmitDeposit from "@/transactions/submit-deposit.js";
import packageJson from "../package.json" with { type: "json" };
import { Effect, pipe } from "effect";
import dotenv from "dotenv";
import { NodeRuntime } from "@effect/platform-node";
import { DatabaseError } from "@/database/utils/common.js";
import { SqlError } from "@effect/sql";

dotenv.config();
const VERSION = packageJson.version;

const program = new Command();

const provideTxServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Services.NodeConfig | Services.MidgardContracts | Services.Lucid
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.MidgardContracts.Default),
    Effect.provide(Services.Lucid.Default),
  );

const provideDatabaseServices = <A, E>(
  effect: Effect.Effect<A, E, Services.Database>,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> => pipe(effect, Effect.provide(Services.Database.layer));

const provideDatabaseConfigServices = <A, E>(
  effect: Effect.Effect<A, E, Services.Database | Services.NodeConfig>,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.NodeConfig.layer),
  );

const provideNodeRuntimeServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Database
    | Services.MidgardContracts
    | Services.Lucid
    | Services.Globals
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContracts.Default),
    Effect.provide(Services.Lucid.Default),
    Effect.provide(Services.Globals.Default),
  );

const assertUserCliWalletIsOperationallyIsolated = ({
  commandName,
  walletAddress,
  operatorMainAddress,
  operatorMergeAddress,
  referenceScriptsAddress,
}: {
  readonly commandName: string;
  readonly walletAddress: string;
  readonly operatorMainAddress: string;
  readonly operatorMergeAddress: string;
  readonly referenceScriptsAddress: string;
}): void => {
  const conflictingRoles = [
    ["operator-main", operatorMainAddress],
    ["operator-merge", operatorMergeAddress],
    ["reference-scripts", referenceScriptsAddress],
  ]
    .filter(([, address]) => address === walletAddress)
    .map(([role]) => role);
  if (conflictingRoles.length > 0) {
    throw new Error(
      `${commandName} requires a user wallet that is distinct from operational node wallets; conflicting roles=${conflictingRoles.join(",")}, address=${walletAddress}`,
    );
  }
};

program.version(VERSION).description(
  `
  ${chalk.red(
    `                       @#
                         @@%#
                        %@@@%#
                       %%%%%%##
                      %%%%%%%%%#
                     %%%%%%%%%%%#
                    %%%%%%%%%%####
                   %%%%%%%%%#######
                  %%%%%%%%  ########
                 %%%%%%%%%  #########
                %%%%%%%%%%  ##########
               %%%%%%%%%%    ##########
              %%%%%%%%%%      ##########
             %%%%%%%%%%        ##########
            %%%%%%%%%%          ##########
           %%%%%%%%%%            ##########
          ###%%%%%%%              ##########
         #########                  #########

   ${chalk.bgGray(
     "    " +
       chalk.bold(
         chalk.whiteBright("A  N  A  S  T  A  S  I  A") +
           "     " +
           chalk.redBright("L  A  B  S"),
       ) +
       "    ",
   )}
  `,
  )}
          ${"Midgard Node – Demo CLI Application"}
  ${ENV_VARS_GUIDE}`,
);

program
  .command("l1-utxos")
  .description(
    "Fetch and print Cardano L1 UTxOs for an address through Blockfrost",
  )
  .requiredOption(
    "--address <address>",
    "Cardano payment address to query from Blockfrost",
  )
  .option(
    "--blockfrost-api-url <url>",
    "Override Blockfrost API base URL; defaults to L1_BLOCKFROST_API_URL",
  )
  .option(
    "--blockfrost-key <key>",
    "Override Blockfrost API key; defaults to L1_BLOCKFROST_KEY",
  )
  .action(async (_args, options) => {
    let address: string;
    let blockfrostConfig: { apiUrl: string; apiKey: string };
    try {
      address = UtxosCommand.parseAddressArgument(options.opts().address);
      blockfrostConfig = L1UtxosCommand.resolveBlockfrostConfig({
        apiUrl: options.opts().blockfrostApiUrl,
        apiKey: options.opts().blockfrostKey,
      });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`l1-utxos: ${message}`);
      process.exitCode = 1;
      return;
    }

    try {
      const result = await L1UtxosCommand.fetchAllBlockfrostAddressUtxos({
        address,
        ...blockfrostConfig,
      });
      process.stdout.write(`${L1UtxosCommand.formatL1UtxosResult(result)}\n`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`l1-utxos: ${message}`);
      process.exitCode = 1;
    }
  });

program
  .command("address-from-seed")
  .description(
    "Derive the Cardano address for a seed phrase using the network implied by the configured Blockfrost URL",
  )
  .requiredOption(
    "--seed-phrase <seedPhrase>",
    "Quoted BIP-39 seed phrase used to derive the payment address",
  )
  .option(
    "--blockfrost-api-url <url>",
    "Override Blockfrost API base URL; defaults to L1_BLOCKFROST_API_URL",
  )
  .action(async (_args, options) => {
    try {
      const blockfrostApiUrl = AddressFromSeed.resolveBlockfrostApiUrl({
        blockfrostApiUrl: options.opts().blockfrostApiUrl,
      });
      const network =
        AddressFromSeed.inferNetworkFromBlockfrostApiUrl(blockfrostApiUrl);
      const address = AddressFromSeed.deriveAddressFromSeedPhrase(
        options.opts().seedPhrase,
        network,
      );
      process.stdout.write(`${address}\n`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`address-from-seed: ${message}`);
      process.exitCode = 1;
    }
  });

program
  .command("listen")
  .option(
    "-m, --with-monitoring",
    "Flag for enabling interactions with monitoring services",
  )
  .action(async (_args, options) => {
    console.log("🌳 Midgard");

    const { withMonitoring } = options.opts();
    const mainEffect = provideNodeRuntimeServices(runNode(withMonitoring));

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("init")
  .description(
    "Initialize hub-oracle, state_queue, registered/active/retired operators, and scheduler roots",
  )
  .option(
    "--contract-deployment-info-output <path>",
    "Optional override path for the contract deployment info JSON written after initialization completes",
  )
  .action(async (_args, options) => {
    const { contractDeploymentInfoOutput } = options.opts();
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const txHash = yield* Initialization.program;
        const manifestOutputPath =
          typeof contractDeploymentInfoOutput === "string"
            ? contractDeploymentInfoOutput
            : ContractDeploymentInfo.defaultContractDeploymentInfoOutputPath();
        const manifestPath =
          yield* ContractDeploymentInfo.writeLiveContractDeploymentInfoProgram(
            manifestOutputPath,
          );
        yield* Effect.logInfo(
          `contract deployment info written: ${manifestPath}`,
        );
        return txHash;
      }).pipe(
        Effect.tap((txHash) => Effect.logInfo(`init completed: txHash=${txHash}`)),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("export-contract-deployment-info")
  .description(
    "Write contract deployment info JSON for the currently configured live validator bundle",
  )
  .requiredOption(
    "--out <path>",
    "Destination filepath for the contract deployment info JSON",
  )
  .action(async (_args, options) => {
    const { out } = options.opts();
    const mainEffect = provideTxServices(
      ContractDeploymentInfo.writeLiveContractDeploymentInfoProgram(out).pipe(
        Effect.tap((outputPath) =>
          Effect.logInfo(
            `export-contract-deployment-info completed: ${outputPath}`,
          ),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

for (const commandName of RegisterActiveOperator.REFERENCE_SCRIPT_COMMAND_NAMES) {
  program
    .command(`deploy-reference-script-${commandName}`)
    .description(`Publish reference scripts for ${commandName}`)
    .action(async () => {
      const mainEffect = provideTxServices(
        Effect.gen(function* () {
          const lucidService = yield* Services.Lucid;
          const contracts = yield* Services.MidgardContracts;
          yield* lucidService.switchToOperatorsMainWallet;
          yield* lucidService.switchToReferenceScriptWallet;
          return yield* RegisterActiveOperator.deployReferenceScriptCommandProgram(
            lucidService.referenceScriptsApi,
            contracts,
            commandName,
            lucidService.api,
          );
        }).pipe(
          Effect.tap((published) =>
            Effect.logInfo(
              `deploy-reference-script-${commandName} completed: ${JSON.stringify(
                published.map(({ name, utxo }) => ({
                  name,
                  outRef: `${utxo.txHash}#${utxo.outputIndex}`,
                })),
              )}`,
            ),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
    });
}

program
  .command("register-active-operator")
  .description(
    "Register operator bond and activate the current operator wallet in the active-operators set",
  )
  .action(async () => {
    const mainEffect = pipe(
      RegisterActiveOperator.program,
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
      Effect.tap((result) =>
        Effect.logInfo(
          `register-active-operator completed: ${JSON.stringify(result)}`,
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("submit-deposit")
  .description(
    "Submit an L1 deposit to the Midgard deposit contract using the selected signer wallet",
  )
  .requiredOption(
    "--l2-address <address>",
    "Destination L2 address that will receive the deposited value",
  )
  .requiredOption(
    "--lovelace <amount>",
    "Amount to deposit, expressed as a positive integer number of lovelace",
  )
  .option(
    "--l2-datum <hex>",
    "Optional L2 inline datum bytes as hex",
  )
  .option(
    "--wallet-seed-phrase-env <envVar>",
    "Environment variable containing the seed phrase for the wallet that should sign the deposit transaction",
    "L1_OPERATOR_SEED_PHRASE",
  )
  .argument(
    "[assetSpecs...]",
    "Optional additional assets in policyId.assetName:amount form (hex policy/asset name, integer amount)",
  )
  .action(
    async (
      assetSpecs: string[],
      options: {
        readonly l2Address: string;
        readonly l2Datum?: string;
        readonly lovelace: string;
        readonly walletSeedPhraseEnv: string;
      },
    ) => {
    let depositConfig: SubmitDeposit.SubmitDepositConfig;
    let walletSeedPhrase: string;
    try {
      const { l2Address, l2Datum, lovelace, walletSeedPhraseEnv } = options;
      depositConfig = SubmitDeposit.parseSubmitDepositConfig({
        l2Address,
        l2Datum,
        lovelace,
        assetSpecs,
      });
      const normalizedSeedPhraseEnv = walletSeedPhraseEnv.trim();
      if (normalizedSeedPhraseEnv.length === 0) {
        throw new Error("Wallet seed phrase env var name must not be empty.");
      }
      walletSeedPhrase = process.env[normalizedSeedPhraseEnv]?.trim() ?? "";
      if (walletSeedPhrase.length === 0) {
        throw new Error(
          `Environment variable "${normalizedSeedPhraseEnv}" does not contain a wallet seed phrase.`,
        );
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`submit-deposit: ${message}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        yield* Effect.sync(() =>
          lucidService.api.selectWallet.fromSeed(walletSeedPhrase),
        );
        const walletAddress = yield* Effect.tryPromise({
          try: () => lucidService.api.wallet().address(),
          catch: (cause) =>
            Promise.reject(
              new Error(
                `Failed to resolve submit-deposit wallet address: ${String(cause)}`,
              ),
            ),
        });
        yield* Effect.sync(() =>
          assertUserCliWalletIsOperationallyIsolated({
            commandName: "submit-deposit",
            walletAddress,
            operatorMainAddress: lucidService.operatorMainAddress,
            operatorMergeAddress: lucidService.operatorMergeAddress,
            referenceScriptsAddress: lucidService.referenceScriptsAddress,
          }),
        );
        return yield* SubmitDeposit.submitDepositProgram(
          lucidService.api,
          contracts,
          depositConfig,
        );
      }).pipe(
        Effect.tap((txHash) =>
          Effect.logInfo(`submit-deposit completed: txHash=${txHash}`),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
    },
  );

program
  .command("submit-l2-transfer")
  .alias("submit-tx")
  .description(
    "Build, sign, and submit a Midgard-native L2 transfer from USER_WALLET by default or a provided seed phrase",
  )
  .requiredOption(
    "--l2-address <address>",
    "Destination L2 address that will receive the Midgard transfer",
  )
  .requiredOption(
    "--lovelace <amount>",
    "Amount to send, expressed as a positive integer number of lovelace",
  )
  .option(
    "--wallet-seed-phrase <seedPhrase>",
    "Optional seed phrase used directly for the signing wallet instead of reading from an environment variable",
  )
  .option(
    "--wallet-seed-phrase-env <envVar>",
    "Environment variable containing the seed phrase for the wallet that should sign the Midgard transfer",
    SubmitL2Transfer.DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--endpoint <url>",
    "Midgard node HTTP endpoint used for /utxos and /submit",
    SubmitL2Transfer.defaultMidgardNodeEndpoint(),
  )
  .option(
    "--submission-mode <mode>",
    'Transfer submission mode: "api" posts to /submit, "local" validates and inserts directly into the local Midgard mempool tables',
    "api",
  )
  .argument(
    "[assetSpecs...]",
    "Optional additional assets in policyId.assetName:amount form (hex policy/asset name, integer amount)",
  )
  .action(
    async (
      assetSpecs: string[],
      options: {
        readonly l2Address: string;
        readonly lovelace: string;
        readonly walletSeedPhrase?: string;
        readonly walletSeedPhraseEnv: string;
        readonly endpoint: string;
        readonly submissionMode: string;
      },
    ) => {
      let transferConfig: SubmitL2Transfer.SubmitL2TransferConfig;
      let resolvedWalletSeedPhrase: SubmitL2Transfer.ResolvedWalletSeedPhrase;
      try {
        transferConfig = SubmitL2Transfer.parseSubmitL2TransferConfig({
          l2Address: options.l2Address,
          lovelace: options.lovelace,
          assetSpecs,
          nodeEndpoint: options.endpoint,
          submissionMode: options.submissionMode,
        });
        resolvedWalletSeedPhrase = SubmitL2Transfer.resolveWalletSeedPhrase({
          walletSeedPhrase: options.walletSeedPhrase,
          walletSeedPhraseEnv: options.walletSeedPhraseEnv,
        });
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        console.error(`submit-l2-transfer: ${message}`);
        process.exitCode = 1;
        return;
      }

      const mainEffect = pipe(
        Effect.gen(function* () {
          const lucidService = yield* Services.Lucid;
          const result = yield* SubmitL2Transfer.submitL2TransferProgram({
            config: transferConfig,
            resolvedWalletSeedPhrase,
            assertWalletAddress: (walletAddress) =>
              assertUserCliWalletIsOperationallyIsolated({
                commandName: "submit-l2-transfer",
                walletAddress,
                operatorMainAddress: lucidService.operatorMainAddress,
                operatorMergeAddress: lucidService.operatorMergeAddress,
                referenceScriptsAddress: lucidService.referenceScriptsAddress,
              }),
          });
          return result;
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(
                `${SubmitL2Transfer.formatSubmitL2TransferResult(result)}\n`,
              );
            }),
          ),
          Effect.tapError((error) =>
            Effect.logError(
              `submit-l2-transfer failed: ${error instanceof Error ? error.message : String(error)}`,
            ),
          ),
        ),
        Effect.provide(Services.Lucid.Default),
        Effect.provide(Services.Database.layer),
        Effect.provide(Services.NodeConfig.layer),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
    },
  );

program
  .command("utxos")
  .description(
    "Print the current Midgard ledger UTxOs and summed asset totals for an address",
  )
  .requiredOption(
    "--address <address>",
    "Cardano payment address to query in the local Midgard ledger view",
  )
  .action(async (_args, options) => {
    let address: string;
    try {
      address = UtxosCommand.parseAddressArgument(options.opts().address);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`utxos: ${message}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideDatabaseServices(
      UtxosCommand.utxosProgram(address).pipe(
        Effect.flatMap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${UtxosCommand.formatUtxosResult(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("project-deposits-once")
  .description(
    "Fetch deposit events from L1 once and project all deposits due by now into the local Midgard mempool ledger",
  )
  .action(async () => {
    const mainEffect = provideNodeRuntimeServices(
      fetchAndInsertDepositUTxOs.pipe(
        Effect.andThen(projectDepositsToMempoolLedger),
        Effect.tap(() =>
          Effect.logInfo("project-deposits-once completed successfully"),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("audit-blocks-immutable")
  .description(
    "Audit BlocksDB -> ImmutableDB linkage and native tx payload integrity",
  )
  .option(
    "--repair",
    "Apply conservative repair by deleting affected block links and malformed immutable tx rows",
  )
  .option(
    "--max-issues <n>",
    "Maximum number of issues to print in logs",
    (value) => Number.parseInt(value, 10),
    20,
  )
  .action(async (_args, options) => {
    const { repair, maxIssues } = options.opts();
    const mainEffect = pipe(
      auditBlocksImmutableProgram({
        repair: repair === true,
        maxIssuesToLog:
          Number.isFinite(maxIssues) && maxIssues > 0 ? maxIssues : 20,
      }),
      Effect.tap((summary) =>
        Effect.logInfo(
          `audit-blocks-immutable summary: ${JSON.stringify(summary)}`,
        ),
      ),
      Effect.provide(Services.Database.layer),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program.parse(process.argv);
