import { Command } from "commander";
import { Logger } from "effect";
import { failCli, writeJson } from "midgard-node/commands/cli-runtime";
import { loadRuntimeDotenv } from "midgard-node/runtime-env";

import packageJson from "../package.json" with { type: "json" };
import * as StressWalletsCommand from "./commands/stress-wallets/index.js";

loadRuntimeDotenv();

const VERSION = packageJson.version;

export const program = new Command();

program
  .name("midgard-node-tools")
  .version(VERSION)
  .description(
    "Midgard node e2e, stress, and acceptance tooling. Every command here drives a node from the outside; none of them ship in the operator binary.",
  );

export const stressCliLoggerLayer = Logger.replace(
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
