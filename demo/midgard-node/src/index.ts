#!/usr/bin/env node

import { Command } from "commander";
import { ENV_VARS_GUIDE, chalk } from "@/utils.js";
import { runNode } from "@/commands/index.js";
import { auditBlocksImmutableProgram } from "@/commands/audit-blocks-immutable.js";
import * as UtxosCommand from "@/commands/utxos.js";
import * as Services from "@/services/index.js";
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
  .command("listen")
  .option(
    "-m, --with-monitoring",
    "Flag for enabling interactions with monitoring services",
  )
  .action(async (_args, options) => {
    console.log("🌳 Midgard");

    const { withMonitoring } = options.opts();
    const mainEffect: Effect.Effect<
      void,
      | DatabaseError
      | SqlError.SqlError
      | Services.ConfigError
      | Services.DatabaseInitializationError,
      never
    > = pipe(
      runNode(withMonitoring),
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.Database.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
      Effect.provide(Services.Globals.Default),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("init")
  .description(
    "Initialize hub-oracle, state_queue, registered/active/retired operators, and scheduler roots",
  )
  .action(async () => {
    const mainEffect = pipe(
      Initialization.program,
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
      Effect.tap((txHash) =>
        Effect.logInfo(`init completed: txHash=${txHash}`),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("deploy-scheduler-and-hub")
  .description(
    "Deploy the hub-oracle and scheduler witnesses together in a single transaction",
  )
  .action(async () => {
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        const nodeConfig = yield* Services.NodeConfig;
        yield* lucidService.switchToOperatorsMainWallet;
        return yield* Initialization.deploySchedulerAndHubProgram(
          lucidService.api,
          contracts,
          nodeConfig,
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(`deploy-scheduler-and-hub completed: ${result}`),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("deploy-state-queue")
  .description("Deploy only the state_queue root node")
  .option(
    "--genesis-time-ms <ms>",
    "Explicit state_queue genesis time in Unix milliseconds",
  )
  .action(async (_args, options) => {
    const { genesisTimeMs } = options.opts();
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        yield* lucidService.switchToOperatorsMainWallet;
        return yield* Initialization.deployStateQueueProgram(
          lucidService.api,
          contracts,
          genesisTimeMs === undefined ? undefined : BigInt(genesisTimeMs),
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(`deploy-state-queue completed: ${result}`),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("deploy-registered-operators")
  .description("Deploy only the registered-operators root node")
  .action(async () => {
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        yield* lucidService.switchToOperatorsMainWallet;
        return yield* Initialization.deployRegisteredOperatorsProgram(
          lucidService.api,
          contracts,
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(`deploy-registered-operators completed: ${result}`),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("deploy-active-operators")
  .description("Deploy only the active-operators root node")
  .action(async () => {
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        yield* lucidService.switchToOperatorsMainWallet;
        return yield* Initialization.deployActiveOperatorsProgram(
          lucidService.api,
          contracts,
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(`deploy-active-operators completed: ${result}`),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("deploy-retired-operators")
  .description("Deploy only the retired-operators root node")
  .action(async () => {
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        yield* lucidService.switchToOperatorsMainWallet;
        return yield* Initialization.deployRetiredOperatorsProgram(
          lucidService.api,
          contracts,
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(`deploy-retired-operators completed: ${result}`),
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
