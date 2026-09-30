import { Effect } from "effect";

import * as AddressFromSeed from "./commands/address-from-seed.js";
import {
  failCli,
  provideDatabaseServices,
  provideNodeRuntimeServices,
  runCliEffect,
  writeJson,
} from "./commands/cli-runtime.js";
import { parseAddressArgument } from "./commands/command-utils.js";
import * as DaBondCommand from "./commands/da-bond.js";
import * as DaBondFiles from "./commands/da-bond-files.js";
import * as L1ProviderPreflightCommand from "./commands/l1-provider-preflight.js";
import * as L1UtxosCommand from "./commands/l1-utxos.js";
import { runNode } from "./commands/listen.js";
import * as MigrationRunner from "./database/migrations/runner.js";
import {
  daBond,
  daBondChainOptions,
  program,
  VERSION,
} from "./index.registration.js";
import * as Services from "./services/index.js";

daBondChainOptions(
  daBond
    .command("top-up")
    .description(
      "Add lovelace to the pool from a wallet (anyone may top up); signs and submits",
    ),
)
  .requiredOption("--amount <lovelace>", "Lovelace to add to the pool")
  .requiredOption(
    "--wallet-seed-env <name>",
    "Environment variable holding the funding wallet's mnemonic or bech32 payment key",
  )
  .action(
    async (
      options: DaBondCommand.DaBondChainOptions & {
        amount: string;
        walletSeedEnv: string;
      },
    ) => {
      try {
        writeJson(await DaBondCommand.runDaBondTopUp(options));
      } catch (error) {
        failCli("da-bond top-up", error);
      }
    },
  );

const daBondWithdraw = daBond
  .command("withdraw")
  .description(
    "Build an unsigned owner-quorum withdrawal step; owners witness it with da-bond witness, then da-bond assemble submits it",
  );

for (const step of ["begin", "cancel", "complete"] as const) {
  const command = daBondChainOptions(
    daBondWithdraw
      .command(step)
      .description(
        step === "begin"
          ? "Build BeginWithdraw: Bonded -> Withdrawing, unlock_at = upper bound + withdraw delay"
          : step === "cancel"
            ? "Build CancelWithdraw: Withdrawing -> Bonded"
            : "Build CompleteWithdraw: at or after unlock_at, pay --amount to --to",
      ),
  )
    .requiredOption(
      "--fee-address <bech32>",
      "Key address whose UTxOs pay the fee and collateral; its key must also witness",
    )
    .requiredOption(
      "--signers <keyhash,...>",
      "The DA params owners who will sign; each becomes a required signer, so list exactly those",
    )
    .requiredOption(
      "--build-unsigned <file>",
      "Write the unsigned transaction to this new file; nothing is submitted",
    );
  if (step === "begin") {
    command.option(
      "--valid-for-ms <ms>",
      "Validity range length; unlock_at counts from its end (default and maximum: the max validity range)",
    );
  }
  if (step === "complete") {
    command
      .requiredOption("--amount <lovelace>", "Lovelace to withdraw")
      .requiredOption("--to <bech32>", "Address that receives --amount");
  }
  command.action(
    async (
      options: DaBondCommand.DaBondChainOptions &
        DaBondCommand.DaBondWithdrawBuildOptions,
    ) => {
      try {
        writeJson(await DaBondCommand.runDaBondWithdrawBuild(step, options));
      } catch (error) {
        failCli(`da-bond withdraw ${step}`, error);
      }
    },
  );
}

daBond
  .command("witness")
  .description(
    "Offline: witness an unsigned da-bond transaction with one owner's (or the fee payer's) key",
  )
  .argument(
    "<unsigned-file>",
    "File written by da-bond withdraw --build-unsigned",
  )
  .requiredOption(
    "--key-env <name>",
    "Environment variable holding a bech32 ed25519_sk/ed25519e_sk key or a mnemonic",
  )
  .option(
    "--out <file>",
    "Write the witness to this new file instead of stdout",
  )
  .action(
    async (unsignedFile: string, options: DaBondFiles.DaBondWitnessOptions) => {
      try {
        writeJson(
          await DaBondFiles.runDaBondWitnessCommand(unsignedFile, options),
        );
      } catch (error) {
        failCli("da-bond witness", error);
      }
    },
  );

daBondChainOptions(
  daBond
    .command("assemble")
    .description(
      "Check the owner quorum, merge the witnesses and submit; refuses below update_threshold",
    )
    .argument(
      "<unsigned-file>",
      "File written by da-bond withdraw --build-unsigned",
    )
    .argument("<witness-files...>", "Witness files written by da-bond witness"),
).action(
  async (
    unsignedFile: string,
    witnessFiles: string[],
    options: DaBondCommand.DaBondChainOptions,
  ) => {
    try {
      writeJson(
        await DaBondCommand.runDaBondAssemble(
          options,
          unsignedFile,
          witnessFiles,
        ),
      );
    } catch (error) {
      failCli("da-bond assemble", error);
    }
  },
);

program
  .command("l1-utxos")
  .description(
    "Fetch and print Cardano L1 UTxOs for an address through local Kupmios",
  )
  .requiredOption(
    "--address <address>",
    "Cardano payment address to query from local Kupmios",
  )
  .option("--kupo-url <url>", "Override Kupo URL; defaults to L1_KUPO_KEY")
  .option(
    "--ogmios-url <url>",
    "Override Ogmios URL; defaults to L1_OGMIOS_KEY",
  )
  .option("--network <network>", "Override network; defaults to NETWORK")
  .action(async (_args, options) => {
    let address: string;
    let kupmiosConfig: L1UtxosCommand.KupmiosConfig;
    try {
      address = parseAddressArgument(options.opts().address);
      kupmiosConfig = L1UtxosCommand.resolveKupmiosConfig({
        kupoUrl: options.opts().kupoUrl,
        ogmiosUrl: options.opts().ogmiosUrl,
        network: options.opts().network,
      });
    } catch (error) {
      failCli("l1-utxos", error);
      return;
    }

    try {
      const result = await L1UtxosCommand.fetchKupmiosAddressUtxos({
        address,
        ...kupmiosConfig,
      });
      writeJson(result);
    } catch (error) {
      failCli("l1-utxos", error);
    }
  });

program
  .command("l1-provider-preflight")
  .description(
    "Check the configured L1 provider route and fail before state-changing work when no source is healthy",
  )
  .option("--json", "Print machine-readable JSON", true)
  .action(async () => {
    const mainEffect = Effect.gen(function* () {
      const nodeConfig = yield* Services.NodeConfig;
      const report = yield* Effect.tryPromise(() =>
        L1ProviderPreflightCommand.runL1ProviderPreflight({
          config: nodeConfig,
        }),
      );
      yield* Effect.sync(() => {
        writeJson(report);
      });
      if (!report.ok) {
        return yield* Effect.fail(
          new Error("No configured L1 provider source passed preflight"),
        );
      }
      return report;
    }).pipe(Effect.provide(Services.NodeConfig.layer));

    runCliEffect(mainEffect);
  });

program
  .command("address-from-seed")
  .description(
    "Derive the Cardano address for a seed phrase on an explicit network",
  )
  .requiredOption(
    "--seed-phrase <seedPhrase>",
    "Quoted BIP-39 seed phrase used to derive the payment address",
  )
  .option("--network <network>", "Override network; defaults to NETWORK")
  .action(async (_args, options) => {
    try {
      const network = AddressFromSeed.resolveNetwork({
        network: options.opts().network,
      });
      const address = AddressFromSeed.deriveAddressFromSeedPhrase(
        options.opts().seedPhrase,
        network,
      );
      process.stdout.write(`${address}\n`);
    } catch (error) {
      failCli("address-from-seed", error);
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

    runCliEffect(mainEffect);
  });

program
  .command("db:migrate")
  .description("Apply pending Midgard node schema migrations explicitly")
  .action(async () => {
    const mainEffect = provideDatabaseServices(
      MigrationRunner.migrate({
        appVersion: VERSION,
        actor: "midgard-node db:migrate",
      }).pipe(
        Effect.tap((status) =>
          Effect.sync(() => {
            process.stdout.write(`${MigrationRunner.formatStatus(status)}\n`);
          }),
        ),
      ),
    );

    runCliEffect(mainEffect);
  });

program
  .command("db:status")
  .description("Print Midgard node schema migration status")
  .option("--json", "Print machine-readable JSON status", true)
  .action(async () => {
    const mainEffect = provideDatabaseServices(
      MigrationRunner.getStatus.pipe(
        Effect.tap((status) =>
          Effect.sync(() => {
            process.stdout.write(`${MigrationRunner.formatStatus(status)}\n`);
          }),
        ),
      ),
    );

    runCliEffect(mainEffect);
  });
