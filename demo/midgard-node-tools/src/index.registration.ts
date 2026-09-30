import { Command } from "commander";
import { Logger } from "effect";
import { failCli, writeJson } from "midgard-node/commands/cli-runtime";
import { loadRuntimeDotenv } from "midgard-node/runtime-env";

import packageJson from "../package.json" with { type: "json" };
import * as E2EFinalizeSummaryCommand from "./commands/e2e-finalize-summary.js";
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

export const parseTxEvidenceOptions = (
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
