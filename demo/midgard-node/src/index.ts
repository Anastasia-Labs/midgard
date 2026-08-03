#!/usr/bin/env node

import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import { assertReferenceScriptAuthMinimumRemaining } from "@al-ft/midgard-sdk";
import { NodeRuntime } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import { getAddressDetails, type Network } from "@lucid-evolution/lucid";
import { Command } from "commander";
import { Effect, Logger, pipe } from "effect";

import * as AddressFromSeed from "@/commands/address-from-seed.js";
import { auditBlocksImmutableProgram } from "@/commands/audit-blocks-immutable.js";
import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  fetchNodeTxStatus,
  formatJson,
  parseAddressArgument,
  parseEventId,
  parseHexBytes,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "@/commands/command-utils.js";
import * as ContractDeploymentInfo from "@/commands/contract-deployment-info.js";
import * as DeploymentRunStateCommand from "@/commands/deployment-run-state.js";
import * as E2EFinalizeSummaryCommand from "@/commands/e2e-finalize-summary.js";
import { runPipelinedCommitProcessAcceptance } from "@/commands/e2e-pipelined-commit-process-acceptance.js";
import * as E2EProcessCleanupCommand from "@/commands/e2e-process-cleanup.js";
import * as E2EServiceCommand from "@/commands/e2e-service.js";
import * as E2EStressL2ThroughputCommand from "@/commands/e2e-stress-l2-throughput.js";
import * as EventSettlementProofCommand from "@/commands/event-settlement-proof.js";
import * as FetchWithdrawalsOnceCommand from "@/commands/fetch-withdrawals-once.js";
import * as L1ProviderPreflightCommand from "@/commands/l1-provider-preflight.js";
import * as L1UtxosCommand from "@/commands/l1-utxos.js";
import { runNode } from "@/commands/listen.js";
import { runMpfAudit } from "@/commands/mpf-audit.js";
import { mpfReplayProgram } from "@/commands/mpf-replay.js";
import * as Phase4GenesisLedgerCommand from "@/commands/phase4-genesis-ledger.js";
import * as Phase4T1RecoveryCommand from "@/commands/phase4-t1-recovery.js";
import * as PrepareHubOracleNonce from "@/commands/prepare-hub-oracle-nonce.js";
import * as ReconcileCommand from "@/commands/reconcile.js";
import * as ReserveInspectionCommand from "@/commands/reserve-inspection.js";
import * as ReservePayoutCommand from "@/commands/reserve-payout.js";
import * as StressCorpusCommand from "@/commands/stress-corpus-generate.js";
import { collectGroundTruthMetricsFromSql } from "@/commands/stress-db-metrics.js";
import { collectEnvironmentFingerprint } from "@/commands/stress-environment-fingerprint.js";
import { collectStressStageMetricSourcesFromSql } from "@/commands/stress-stage-metrics.js";
import * as StressWalletsCommand from "@/commands/stress-wallets.js";
import * as SubmitL2Transfer from "@/commands/submit-l2-transfer.js";
import * as SubmitWithdrawalCommand from "@/commands/submit-withdrawal.js";
import * as UtxosCommand from "@/commands/utxos.js";
import * as WithdrawalStatusCommand from "@/commands/withdrawal-status.js";
import {
  type DaLibp2pPreflightMode,
  runDaLibp2pPreflightFromEnv,
} from "@/da/libp2p-producer.js";
import {
  DA_LIBP2P_RUNTIME_PROFILES,
  type DaLibp2pRuntimeManifestOptions,
  type DaLibp2pRuntimeManifestTarget,
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "@/da/libp2p-runtime-manifest.js";
import * as MigrationRunner from "@/database/migrations/runner.js";
import {
  buildE2EProcessEnv,
  type E2EEnvInheritance,
  parseEnvOverrides,
} from "@/e2e/env.js";
import { runCommandStep } from "@/e2e/runner.js";
import {
  fetchAndInsertDepositUTxOs,
  projectDepositsToMempoolLedger,
} from "@/fibers/index.js";
import { loadRuntimeDotenv } from "@/runtime-env.js";
import * as Services from "@/services/index.js";
import * as DaAttestation from "@/transactions/da-attestation.js";
import * as Initialization from "@/transactions/initialization.js";
import * as PhasMembershipRegistration from "@/transactions/phas-membership-registration.js";
import {
  fetchReferenceScriptUtxosProgram,
  planReferenceScriptCommandProgram,
  REFERENCE_SCRIPT_SWEEP_DEFAULT_MAX_ASSETS_PER_TOKEN_OUTPUT,
  REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
  referenceScriptWalletStatusProgram,
  sweepReferenceScriptWalletProgram,
} from "@/transactions/reference-scripts.js";
import * as RegisterActiveOperator from "@/transactions/register-active-operator.js";
import * as SubmitDeposit from "@/transactions/submit-deposit.js";
import { chalk, ENV_VARS_GUIDE } from "@/utils.js";
import { commitExplicitBlockHeaderProgram } from "@/workers/commit-block-header.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "@/workers/commit-block-header/da-payload-backfill.js";

import packageJson from "../package.json" with { type: "json" };

loadRuntimeDotenv();
const VERSION = packageJson.version;

const program = new Command();

const parseMerkleRootOption = (value: unknown, label: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be 32 bytes of hex`);
  }
  try {
    return normalizeHex(value, { byteLength: 32, trim: false });
  } catch {
    throw new Error(`${label} must be 32 bytes of hex`);
  }
};

const parseOptionalHeaderHashOption = (value: unknown): string | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string") {
    throw new Error("--header-hash must be 28 bytes of hex");
  }
  return normalizeHex(value, { byteLength: 28, trim: false });
};

const parseOptionalEndTimeMs = (value: unknown): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error("--end-time-ms must be a non-negative integer");
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error("--end-time-ms must be a safe non-negative integer");
  }
  return parsed;
};

const parsePositiveIntegerOption = (value: unknown, label: string): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a safe positive integer`);
  }
  return parsed;
};

const parseNonNegativeIntegerOption = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a safe non-negative integer`);
  }
  return parsed;
};

const parsePositiveBigIntOption = (value: unknown, label: string): bigint => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer`);
  }
  const parsed = BigInt(value);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero`);
  }
  return parsed;
};

const collectStringOption = (
  value: string,
  previous: string[] = [],
): string[] => [...previous, value];

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

const parseTxEvidenceOptions = (
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

const parseStringListOption = (values: unknown, label: string): string[] =>
  Array.isArray(values)
    ? values.map((value) => {
        if (typeof value !== "string" || value.length === 0) {
          throw new Error(`${label} must be a non-empty string.`);
        }
        return value;
      })
    : [];

const parseE2EEnvInheritanceOption = (
  value: unknown,
): E2EEnvInheritance | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (value === "process" || value === "none") {
    return value;
  }
  throw new Error("--env-inheritance must be process or none");
};

const parseDaLibp2pPreflightMode = (value: unknown): DaLibp2pPreflightMode => {
  if (value === "bind-listen" || value === "dial-only") {
    return value;
  }
  throw new Error("--mode must be bind-listen or dial-only");
};

const DA_LIBP2P_RUNTIME_TARGETS = new Set(["producer", "watcher"]);

const parseDaLibp2pRuntimeTarget = (
  value: unknown,
): DaLibp2pRuntimeManifestTarget => {
  if (typeof value === "string" && DA_LIBP2P_RUNTIME_TARGETS.has(value)) {
    return value as DaLibp2pRuntimeManifestTarget;
  }
  throw new Error("--target must be producer or watcher");
};

const parseDaLibp2pRuntimeProfile = (
  value: unknown,
): DaLibp2pRuntimeManifestOptions["profile"] => {
  if (
    typeof value === "string" &&
    (DA_LIBP2P_RUNTIME_PROFILES as readonly string[]).includes(value)
  ) {
    return value as DaLibp2pRuntimeManifestOptions["profile"];
  }
  throw new Error(
    `--profile must be one of ${DA_LIBP2P_RUNTIME_PROFILES.join(", ")}`,
  );
};

const parseDaLibp2pCommitteeMember = (
  value: string,
): DaLibp2pRuntimeManifestOptions["committeeMembers"][number] => {
  const [signerIndexRaw, daVkey, keySource, rolesRaw, ...extra] =
    value.split(",");
  if (
    signerIndexRaw === undefined ||
    daVkey === undefined ||
    keySource === undefined ||
    rolesRaw === undefined ||
    extra.length > 0
  ) {
    throw new Error(
      "--committee-member must use signerIndex,daVkey,libp2pKeySource,role+role",
    );
  }
  const roles = rolesRaw
    .split("+")
    .map((role) => role.trim())
    .filter((role) => role.length > 0);
  if (roles.length === 0) {
    throw new Error("--committee-member roles must be non-empty");
  }
  return {
    signerIndex: parseNonNegativeIntegerOption(
      signerIndexRaw,
      "--committee-member signerIndex",
    ),
    daVkey,
    libp2pPrivateKeySource: keySource,
    roles,
  };
};

const parseDaLibp2pCommitteeMembers = (
  values: unknown,
): DaLibp2pRuntimeManifestOptions["committeeMembers"] =>
  parseStringListOption(values, "--committee-member").map((value) =>
    parseDaLibp2pCommitteeMember(value),
  );

const expectedNetworkIdForAddress = (network: Network): number | undefined => {
  if (network === "Mainnet") {
    return 1;
  }
  if (network === "Preprod" || network === "Preview") {
    return 0;
  }
  return undefined;
};

const parseL1AddressOption = (
  value: unknown,
  label: string,
  network: Network,
): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} must be a non-empty Cardano address`);
  }
  const normalized = value.trim();
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(normalized);
  } catch (cause) {
    throw new Error(`Invalid ${label} "${normalized}": ${String(cause)}`);
  }
  const expectedNetworkId = expectedNetworkIdForAddress(network);
  if (
    expectedNetworkId !== undefined &&
    details.networkId !== expectedNetworkId
  ) {
    throw new Error(`${label} must target the configured ${network} network`);
  }
  return details.address.bech32;
};

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const failCli = (label: string, error: unknown): void => {
  console.error(`${label}: ${errorMessage(error)}`);
  process.exitCode = 1;
};

const writeJson = (value: unknown): void => {
  process.stdout.write(`${formatJson(value)}\n`);
};

function tapJson(): <A, E, R>(
  effect: Effect.Effect<A, E, R>,
) => Effect.Effect<A, E, R>;
function tapJson<A>(
  project: (value: A) => unknown,
): <E, R>(effect: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;
function tapJson<A>(project?: (value: A) => unknown) {
  return <E, R>(effect: Effect.Effect<A, E, R>) =>
    effect.pipe(
      Effect.tap((value: A) =>
        Effect.sync(() =>
          writeJson(project === undefined ? value : project(value)),
        ),
      ),
    );
}

const runCliEffect = <A, E>(effect: Effect.Effect<A, E, never>): void => {
  NodeRuntime.runMain(effect, { teardown: undefined });
};

const stressCliLoggerLayer = Logger.replace(
  Logger.defaultLogger,
  Logger.withConsoleError(Logger.logfmtLogger),
);

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

const provideReferenceScriptDeploymentServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Services.NodeConfig | Services.Lucid | Services.AlwaysSucceedsContract
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.AlwaysSucceedsContract.Default),
    Effect.provide(Services.Lucid.Default),
  );

const provideLucidOnlyServices = <A, E>(
  effect: Effect.Effect<A, E, Services.NodeConfig | Services.Lucid>,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Lucid.Default),
  );

const provideDatabaseServices = <A, E>(
  effect: Effect.Effect<A, E, Services.Database>,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> => pipe(effect, Effect.provide(Services.Database.layer));

const provideNodeRuntimeServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Database
    | Services.AdmissionWriter
    | Services.AdmissionSql
    | Services.BatchSql
    | Services.WriteBehind
    | Services.ContractDeploymentIdentity
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
    Effect.provide(Services.AdmissionWriterLive),
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContractServices),
    Effect.provide(Services.Lucid.Default),
    Effect.provide(Services.Globals.Default),
  );

const provideDatabaseTxServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Database
    | Services.WriteBehind
    | Services.ContractDeploymentIdentity
    | Services.MidgardContracts
    | Services.Lucid
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContractServices),
    Effect.provide(Services.Lucid.Default),
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
    "Delay after submitted deposits before projecting L1 deposit events",
    StressWalletsCommand.DEFAULT_PROJECTION_WAIT_MS.toString(),
  )
  .option(
    "--verify-timeout-ms <ms>",
    "Maximum time to poll /utxos for projected L2 funding",
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
                    const submitted =
                      yield* SubmitDeposit.submitDepositWithMetadataProgram(
                        lucidService.api,
                        contracts,
                        {
                          ...depositConfig,
                          referenceScripts: depositReferenceScripts,
                        },
                      );
                    return {
                      txHash: submitted.txHash,
                      depositEventId: submitted.metadata.depositEventId,
                    };
                  }),
                ),
              ),
            projectDeposits: async () =>
              Effect.runPromise(
                provideNodeRuntimeServices(
                  fetchAndInsertDepositUTxOs.pipe(
                    Effect.andThen(projectDepositsToMempoolLedger),
                  ),
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
                      submissionMode: "api",
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
            Effect.provide(Services.Lucid.Default),
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
                      submissionMode: "api",
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
            Effect.provide(Services.Lucid.Default),
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

program
  .command("stress-corpus-generate")
  .description(
    "Pre-build and verify an offline NDJSON corpus of signed Midgard L2 stress transactions",
  )
  .requiredOption("--target-rate-tps <rate>", "Target offered TPS")
  .requiredOption("--duration-ms <ms>", "Measured run duration in milliseconds")
  .option("--warmup-count <count>", "Warmup rows to reserve", "0")
  .option("--cooldown-count <count>", "Cooldown rows to reserve", "0")
  .option("--wallet-count <count>", "Override generated chain count")
  .option("--safety-factor <factor>", "Sizing safety factor", "1.1")
  .option("--amount-lovelace <amount>", "Self-transfer amount", "1000000")
  .option("--min-fee-a <amount>", "Midgard MIN_FEE_A; defaults to env")
  .option("--min-fee-b <amount>", "Midgard MIN_FEE_B; defaults to env")
  .option(
    "--max-submit-tx-cbor-bytes <bytes>",
    "Midgard MAX_SUBMIT_TX_CBOR_BYTES; defaults to env",
  )
  .option(
    "--assumed-acceptance-latency-ms <ms>",
    "Acceptance-latency bound used for wallet-count safety checks",
    "1000",
  )
  .option("--wallets-dir <path>", "Prepared stress wallet directory")
  .option("--out-dir <path>", "Output directory for corpus artifacts")
  .option("--workers <count>", "Worker thread count")
  .option("--slices <count>", "Number of corpus slice ids", "1")
  .option(
    "--slice-wallet-counts <counts>",
    "Comma-separated wallet counts for ordered, dependency-isolated slices (must sum to --wallet-count)",
  )
  .option("--corpus-slice-id-prefix <id>", "Corpus slice id prefix", "default")
  .option(
    "--rebuild-sample-rate <rate>",
    "Fraction of chains to rebuild and byte-compare during verification",
    "0.001",
  )
  .option(
    "--funding-source <source>",
    "Funding source mode: existing or fanout",
    "existing",
  )
  .option("--network <network>", "Mainnet or Preprod; defaults to NETWORK env")
  .option("--yes", "Confirm generation")
  .action(async (options) => {
    try {
      const config =
        StressCorpusCommand.parseStressCorpusGenerateConfig(options);
      const result = await StressCorpusCommand.generateStressCorpus(config);
      writeJson(result);
    } catch (error) {
      failCli("stress-corpus-generate", error);
    }
  });

program
  .command("stress-corpus-verify")
  .description("Stream-verify a generated Midgard stress corpus and sidecars")
  .requiredOption("--corpus-path <path>", "Corpus NDJSON path")
  .option("--index-path <path>", "Corpus index path")
  .option("--manifest-path <path>", "Corpus manifest path")
  .option(
    "--result-out <path>",
    "Write a SHA-bindable standalone verification result artifact",
  )
  .option(
    "--rebuild-wallets-dir <path>",
    "Prepared stress wallet directory for rebuild-sample verification",
  )
  .option(
    "--rebuild-sample-rate <rate>",
    "Fraction of chains to rebuild and byte-compare when --rebuild-wallets-dir is set",
    "0.001",
  )
  .option("--amount-lovelace <amount>", "Self-transfer amount", "1000000")
  .option("--min-fee-a <amount>", "Midgard MIN_FEE_A; defaults to env")
  .option("--min-fee-b <amount>", "Midgard MIN_FEE_B; defaults to env")
  .option(
    "--max-submit-tx-cbor-bytes <bytes>",
    "Midgard MAX_SUBMIT_TX_CBOR_BYTES; defaults to env",
  )
  .option("--network <network>", "Mainnet or Preprod; defaults to NETWORK env")
  .action(async (options) => {
    try {
      const config = StressCorpusCommand.parseStressCorpusVerifyConfig(options);
      const result = await StressCorpusCommand.verifyStressCorpus(config);
      writeJson(result);
    } catch (error) {
      failCli("stress-corpus-verify", error);
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

program
  .command("db:verify")
  .description(
    "Verify the database schema is compatible with this Midgard node binary",
  )
  .action(async () => {
    const mainEffect = provideDatabaseServices(
      MigrationRunner.assertCompatible.pipe(
        Effect.tap(() =>
          Effect.sync(() => {
            process.stdout.write("schema compatibility verified\n");
          }),
        ),
      ),
    );

    runCliEffect(mainEffect);
  });

program
  .command("db:checksum")
  .description("Print the compiled schema migration manifest checksums")
  .action(async () => {
    process.stdout.write(`${MigrationRunner.formatChecksum()}\n`);
  });

program
  .command("phase4-genesis-ledger")
  .description(
    "Explicitly seed or verify the complete configured L2 genesis set and A/B funding in an isolated Phase 4 local-devnet database",
  )
  .option("--seed", "Seed an empty run-scoped mempool ledger")
  .option(
    "--verify-only",
    "Require the complete byte-identical configured genesis ledger without mutating it",
  )
  .action((opts) => {
    const seed = opts.seed === true;
    const verifyOnly = opts.verifyOnly === true;
    if (seed === verifyOnly) {
      failCli(
        "phase4-genesis-ledger",
        new Error("Specify exactly one of --seed or --verify-only"),
      );
      return;
    }
    const mainEffect = Phase4GenesisLedgerCommand.phase4GenesisLedgerProgram({
      mode: seed ? "seed" : "verify",
    }).pipe(Effect.provide(Services.Database.layerWithNodeConfig), tapJson());
    runCliEffect(mainEffect);
  });

program
  .command("db:backfill-da-payloads")
  .description(
    "Safely materialize missing DA payload rows from finalized pending-block journals",
  )
  .option(
    "--header-hash <hex>",
    "Optional 28-byte finalized block header hash to backfill",
  )
  .option(
    "--limit <count>",
    "Maximum number of missing finalized journals to scan",
    "100",
  )
  .action(async (_args, options) => {
    let headerHash: string | undefined;
    let limit: number;
    try {
      headerHash = parseOptionalHeaderHashOption(options.opts().headerHash);
      limit = parsePositiveIntegerOption(options.opts().limit, "--limit");
    } catch (error) {
      failCli("db:backfill-da-payloads", error);
      return;
    }

    const mainEffect = provideDatabaseServices(
      backfillMissingDaPayloadsFromFinalizedJournals({
        headerHash:
          headerHash === undefined ? undefined : Buffer.from(headerHash, "hex"),
        limit,
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

const reconcile = program
  .command("reconcile")
  .description(
    "Inspect and optionally repair idempotent e2e recovery milestones",
  );

reconcile
  .command("phas-registered")
  .description("Reconcile PHAS membership reward-account registration")
  .option("--repair", "Run the idempotent PHAS registration repair if missing")
  .option("--json", "Print machine-readable JSON output", true)
  .action(async (options: { readonly repair?: boolean }) => {
    const mainEffect = provideLucidOnlyServices(
      ReconcileCommand.reconcilePhasRegisteredProgram({
        repair: options.repair === true,
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

reconcile
  .command("reference-scripts-complete")
  .description("Reconcile node-runtime reference-script publication")
  .option(
    "--manifest <path>",
    "Deployment manifest path; the configured MidgardContracts manifest is used for verification",
  )
  .option("--scope <scope>", "Reference-script scope", "node-runtime")
  .option("--repair", "Publish only missing node-runtime reference scripts")
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: { readonly scope?: string; readonly repair?: boolean }) => {
      if ((options.scope ?? "node-runtime") !== "node-runtime") {
        failCli(
          "reconcile reference-scripts-complete",
          new Error("only --scope node-runtime is supported"),
        );
        return;
      }
      const mainEffect = provideTxServices(
        ReconcileCommand.reconcileReferenceScriptsCompleteProgram({
          repair: options.repair === true,
        }).pipe(tapJson()),
      );

      runCliEffect(mainEffect);
    },
  );

reconcile
  .command("deployment-manifest")
  .description(
    "Reconcile the deployment manifest after a confirmed protocol initialization",
  )
  .requiredOption(
    "--out <path>",
    "Destination filepath for the contract deployment info JSON",
  )
  .requiredOption(
    "--init-tx-hash <hex>",
    "32-byte protocol initialization transaction hash",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: { readonly out: string; readonly initTxHash: string }) => {
      let initTxHash: string;
      try {
        initTxHash = parseHexBytes(
          options.initTxHash,
          "initTxHash",
          32,
        ).toString("hex");
      } catch (error) {
        failCli("reconcile deployment-manifest", error);
        return;
      }

      const mainEffect = provideTxServices(
        ContractDeploymentInfo.reconcileInitializedDeploymentManifestProgram({
          outputPath: options.out,
          initTxHash,
        }).pipe(tapJson()),
      );

      runCliEffect(mainEffect);
    },
  );

reconcile
  .command("deposit-projected")
  .description(
    "Reconcile deposit visibility and projection into the L2 mempool ledger",
  )
  .option("--event-id <hex>", "Canonical OutputReference CBOR deposit event id")
  .option("--cardano-tx-hash <hex>", "32-byte Cardano deposit transaction hash")
  .option(
    "--repair",
    "Reconcile visible deposit UTxOs and project due deposits",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: {
      readonly eventId?: string;
      readonly cardanoTxHash?: string;
      readonly repair?: boolean;
    }) => {
      let eventId: Buffer | undefined;
      let cardanoTxHash: Buffer | undefined;
      try {
        eventId =
          options.eventId === undefined
            ? undefined
            : parseEventId(options.eventId, "eventId");
        cardanoTxHash =
          options.cardanoTxHash === undefined
            ? undefined
            : parseHexBytes(options.cardanoTxHash, "cardanoTxHash", 32);
        if (eventId === undefined && cardanoTxHash === undefined) {
          throw new Error("Provide --event-id or --cardano-tx-hash.");
        }
      } catch (error) {
        failCli("reconcile deposit-projected", error);
        return;
      }

      const mainEffect = provideNodeRuntimeServices(
        ReconcileCommand.reconcileDepositProjectedProgram({
          eventId,
          cardanoTxHash,
          repair: options.repair === true,
        }).pipe(tapJson()),
      );

      runCliEffect(mainEffect);
    },
  );

reconcile
  .command("tx-committed")
  .description("Reconcile an L2 transaction's local commit status")
  .requiredOption("--tx-hash <hex>", "32-byte Midgard L2 transaction id")
  .option("--json", "Print machine-readable JSON output", true)
  .action(async (options: { readonly txHash: string }) => {
    let txHash: Buffer;
    try {
      txHash = parseHexBytes(options.txHash, "txHash", 32);
    } catch (error) {
      failCli("reconcile tx-committed", error);
      return;
    }
    const mainEffect = provideDatabaseServices(
      ReconcileCommand.reconcileTxCommittedProgram({ txHash }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

reconcile
  .command("da-attested")
  .description("Reconcile DA payload and copied watcher attestation status")
  .requiredOption("--header-hash <hex>", "28-byte block header hash")
  .option("--watcher-url <url>", "Copied DA node base URL")
  .option(
    "--contract-deployment-info <path>",
    "Finalized V1 contract deployment info path used to derive the watcher deployment fingerprint",
  )
  .option(
    "--repair",
    "Backfill missing local DA payload rows from finalized journals",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: {
      readonly headerHash: string;
      readonly watcherUrl?: string;
      readonly contractDeploymentInfo?: string;
      readonly repair?: boolean;
    }) => {
      let headerHash: Buffer;
      let deploymentFingerprint: string | undefined;
      try {
        headerHash = parseHexBytes(options.headerHash, "headerHash", 28);
        deploymentFingerprint =
          typeof options.contractDeploymentInfo === "string"
            ? ContractDeploymentInfo.readFinalizedDeploymentIdentity(
                options.contractDeploymentInfo,
              ).manifestId
            : undefined;
      } catch (error) {
        failCli("reconcile da-attested", error);
        return;
      }
      const mainEffect = provideDatabaseTxServices(
        ReconcileCommand.reconcileDaAttestedProgram({
          headerHash,
          watcherUrl: options.watcherUrl,
          deploymentFingerprint,
          repair: options.repair === true,
        }).pipe(tapJson()),
      );

      runCliEffect(mainEffect);
    },
  );

reconcile
  .command("block-committed")
  .description(
    "Reconcile block commitment in canonical state_queue/local journals",
  )
  .requiredOption("--header-hash <hex>", "28-byte block header hash")
  .option("--json", "Print machine-readable JSON output", true)
  .action(async (options: { readonly headerHash: string }) => {
    let headerHash: Buffer;
    try {
      headerHash = parseHexBytes(options.headerHash, "headerHash", 28);
    } catch (error) {
      failCli("reconcile block-committed", error);
      return;
    }
    const mainEffect = provideDatabaseTxServices(
      ReconcileCommand.reconcileBlockCommittedProgram({ headerHash }).pipe(
        tapJson(),
      ),
    );

    runCliEffect(mainEffect);
  });

reconcile
  .command("local-finalization")
  .description(
    "Reconcile local finalization for a canonical committed block header",
  )
  .requiredOption("--header-hash <hex>", "28-byte block header hash")
  .option(
    "--repair",
    "Replay local finalization from the durable pending-finalization journal",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: {
      readonly headerHash: string;
      readonly repair?: boolean;
    }) => {
      let headerHash: Buffer;
      try {
        headerHash = parseHexBytes(options.headerHash, "headerHash", 28);
      } catch (error) {
        failCli("reconcile local-finalization", error);
        return;
      }
      const mainEffect = provideDatabaseTxServices(
        ReconcileCommand.reconcileLocalFinalizationProgram({
          headerHash,
          repair: options.repair === true,
        }).pipe(tapJson()),
      );

      runCliEffect(mainEffect);
    },
  );

reconcile
  .command("merge-complete")
  .description("Reconcile merge completion for a committed block header")
  .requiredOption("--header-hash <hex>", "28-byte block header hash")
  .option("--repair", "Trigger the existing idempotent merge action if queued")
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: {
      readonly headerHash: string;
      readonly repair?: boolean;
    }) => {
      let headerHash: Buffer;
      try {
        headerHash = parseHexBytes(options.headerHash, "headerHash", 28);
      } catch (error) {
        failCli("reconcile merge-complete", error);
        return;
      }
      const mainEffect = provideNodeRuntimeServices(
        ReconcileCommand.reconcileMergeCompleteProgram({
          headerHash,
          repair: options.repair === true,
        }).pipe(tapJson()),
      );

      runCliEffect(mainEffect);
    },
  );

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
            {
              hubOracleOneShotStatus: "consumed_by_init",
              steps: {
                initProtocol: {
                  status: "complete",
                  txHash,
                },
              },
            },
          );
        yield* Effect.logInfo(
          `contract deployment info written: ${manifestPath}`,
        );
        return txHash;
      }).pipe(
        Effect.tap((txHash) =>
          Effect.logInfo(`init completed: txHash=${txHash}`),
        ),
      ),
    );

    runCliEffect(mainEffect);
  });

program
  .command("deployment-status")
  .description("Print live protocol deployment status for configured contracts")
  .action(async () => {
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const manifestVerification = yield* Effect.either(
          ContractDeploymentInfo.verifyConfiguredDeploymentManifestIfPresentProgram,
        );
        const lucidService = yield* Services.Lucid;
        const contracts = yield* Services.MidgardContracts;
        const status = yield* Initialization.fetchProtocolDeploymentStatus(
          lucidService.api,
          contracts,
        );
        process.stdout.write(
          `${formatJson({
            manifest:
              manifestVerification._tag === "Right"
                ? (manifestVerification.right ?? {
                    ok: false,
                    mismatches: ["deployment manifest file not found"],
                    recommendation: "fresh_redeploy_required",
                  })
                : {
                    ok: false,
                    mismatches: [formatUnknownError(manifestVerification.left)],
                    recommendation: "fresh_redeploy_required",
                  },
            protocol: status,
          })}\n`,
        );
      }),
    );

    runCliEffect(mainEffect);
  });

program
  .command("e2e-finalize-summary")
  .description(
    "Collect final e2e endpoint/database evidence and write summary.json plus summary.md",
  )
  .option("--out-dir <path>", "Output directory for summary artifacts")
  .option("--run-id <id>", "Stable run id for the summary")
  .option("--mode <mode>", "Run mode: attach, resume, fresh, or unknown")
  .option("--node-url <url>", "Midgard node URL")
  .option(
    "--admin-api-key-env <name>",
    "Environment variable that contains the admin API key",
    "ADMIN_API_KEY",
  )
  .option("--node-log <path>", "Raw Midgard node log artifact to link")
  .option(
    "--step-summary <path>",
    "Structured e2e-run-step summary JSON file to include; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--tx <label:txHash:status:source>",
    "Transaction evidence to include in the summary; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--stress-summary <path>",
    "Optional e2e-stress-l2-throughput summary.json artifact to include as a functional gate",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mode =
      opts.mode === "attach" ||
      opts.mode === "resume" ||
      opts.mode === "fresh" ||
      opts.mode === "unknown"
        ? opts.mode
        : "unknown";
    const adminApiKey =
      typeof opts.adminApiKeyEnv === "string"
        ? process.env[opts.adminApiKeyEnv]
        : undefined;
    const mainEffect = provideDatabaseServices(
      E2EFinalizeSummaryCommand.finalizeE2ESummaryProgram({
        ...(typeof opts.outDir === "string" ? { outDir: opts.outDir } : {}),
        ...(typeof opts.runId === "string" ? { runId: opts.runId } : {}),
        mode,
        ...(typeof opts.nodeUrl === "string" ? { nodeUrl: opts.nodeUrl } : {}),
        ...(adminApiKey === undefined ? {} : { adminApiKey }),
        ...(typeof opts.nodeLog === "string"
          ? { nodeLogPath: opts.nodeLog }
          : {}),
        stepSummaryPaths: parseStringListOption(
          opts.stepSummary,
          "--step-summary",
        ),
        transactions: parseTxEvidenceOptions(opts.tx),
        ...(typeof opts.stressSummary === "string"
          ? { stressSummaryPath: opts.stressSummary }
          : {}),
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("e2e-stress-l2-throughput")
  .description(
    "Run opt-in bounded L2 transfer stress for an e2e deployment and write stress artifacts",
  )
  .option(
    "--endpoint <url>",
    "Midgard node HTTP endpoint used for /utxos, /submit, and /tx-status",
    defaultMidgardNodeEndpoint(),
  )
  .option(
    "--mode <mode>",
    "Stress mode: serial-chain or parallel-fanout",
    "serial-chain",
  )
  .option(
    "--load-model <model>",
    "Stress load model: closed-loop-smoke or open-loop-upper-bound",
    "closed-loop-smoke",
  )
  .option(
    "--workload-profile <profile>",
    "Workload profile label: synthetic-admission or production-end-user",
  )
  .option(
    "--corpus-shape <shape>",
    "Open-loop corpus shape: fanout, chain, or mixed",
    "fanout",
  )
  .option(
    "--tx-corpus <path>",
    "Open-loop tx-corpus.ndjson path with prebuilt canonical CBOR rows",
  )
  .option(
    "--corpus-slice-id <id>",
    "Open-loop corpus slice id to use for this rate step",
    "default",
  )
  .option(
    "--target-rate-tps <rate>",
    "Open-loop target submit rate in transactions per second",
    "100",
  )
  .option(
    "--open-loop-duration-ms <ms>",
    "Open-loop measured submission duration in milliseconds",
    "10000",
  )
  .option(
    "--open-loop-warmup-count <count>",
    "Open-loop corpus warmup transaction count reserved before the measured window",
    "0",
  )
  .option(
    "--open-loop-cooldown-count <count>",
    "Open-loop corpus cooldown transaction count reserved after the measured window",
    "0",
  )
  .option(
    "--open-loop-max-in-flight <count>",
    "Open-loop maximum concurrent POST /submit requests",
    "256",
  )
  .option(
    "--no-op-calibration-endpoint <url>",
    "No-op endpoint with the POST /submit request shape, used for client calibration",
  )
  .option(
    "--require-no-op-calibration",
    "Fail open-loop runs unless no-op calibration is configured and passes",
  )
  .option(
    "--no-op-calibration-duration-ms <ms>",
    "No-op calibration duration in milliseconds",
    "5000",
  )
  .option(
    "--aggregate-observer-interval-ms <ms>",
    "Aggregate observer sampling interval during open-loop submission",
    "1000",
  )
  .option("--count <count>", "Number of stress transfers to submit", "25")
  .option("--concurrency <count>", "Maximum concurrent stress workers", "1")
  .option(
    "--lovelace <amount>",
    "Lovelace amount for each stress transfer",
    "1000000",
  )
  .option(
    "--fee-headroom-lovelace <amount>",
    "Extra lovelace required above --lovelace when preflighting parallel-fanout wallet funding",
    "500000",
  )
  .option(
    "--wallet-seed-phrase <seedPhrase>",
    "Optional primary seed phrase used directly instead of reading from an environment variable",
  )
  .option(
    "--wallet-seed-phrase-env <envVar>",
    "Environment variable containing the primary wallet seed phrase",
    DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--stress-wallet-seed-phrase-env <envVar>",
    "Environment variable for an independent pre-funded stress wallet; repeat for parallel-fanout",
    collectStringOption,
    [],
  )
  .option(
    "--l2-address <address>",
    "Optional destination L2 address; defaults to each sender's own address",
  )
  .option("--run-id <id>", "Stable e2e run id for stress artifacts")
  .option("--out-dir <path>", "Output directory for stress artifacts")
  .option(
    "--poll-interval-ms <ms>",
    "Fixed /tx-status polling interval; omit to use adaptive backoff (poll-initial-interval-ms to poll-max-interval-ms)",
  )
  .option(
    "--poll-initial-interval-ms <ms>",
    "Initial adaptive poll interval (ignored if --poll-interval-ms is set)",
    "75",
  )
  .option(
    "--poll-max-interval-ms <ms>",
    "Adaptive poll interval cap (ignored if --poll-interval-ms is set)",
    "1000",
  )
  .option(
    "--submit-request-timeout-ms <ms>",
    "Per-transfer timeout for the submit request phase",
    "300000",
  )
  .option(
    "--acceptance-timeout-ms <ms>",
    "Per-transfer timeout for /tx-status to reach accepted-or-later",
    "600000",
  )
  .option(
    "--commit-observation-timeout-ms <ms>",
    "Per-transfer background timeout for observing committed status",
    "600000",
  )
  .option(
    "--finality-observer-max-concurrent-requests <count>",
    "Maximum concurrent /tx-status requests used by post-submit finality observation",
    "4",
  )
  .option(
    "--unsafe-allow-large-stress",
    "Explicitly allow count/concurrency above the default safety caps",
  )
  .option(
    "--max-submission-failures <count>",
    "Abort the run once this many transfer submissions/builds fail (default: 0, zero tolerance)",
    "0",
  )
  .option("--json", "Print JSON result")
  .action(async (options) => {
    const abortController = new AbortController();
    const interrupt = (signalName: NodeJS.Signals): void => {
      if (!abortController.signal.aborted) {
        abortController.abort(
          new Error(
            `received ${signalName}; writing interrupted stress summary`,
          ),
        );
      }
    };
    process.once("SIGINT", interrupt);
    process.once("SIGTERM", interrupt);
    try {
      const stressConfig = E2EStressL2ThroughputCommand.parseE2EL2StressConfig({
        endpoint: options.endpoint,
        loadModel: options.loadModel,
        workloadProfile: options.workloadProfile,
        mode: options.mode,
        corpusShape: options.corpusShape,
        corpusPath: options.txCorpus,
        corpusSliceId: options.corpusSliceId,
        targetRateTps: options.targetRateTps,
        openLoopDurationMs: options.openLoopDurationMs,
        openLoopWarmupCount: options.openLoopWarmupCount,
        openLoopCooldownCount: options.openLoopCooldownCount,
        openLoopMaxInFlight: options.openLoopMaxInFlight,
        noOpCalibrationEndpoint: options.noOpCalibrationEndpoint,
        requireNoOpCalibration: options.requireNoOpCalibration === true,
        noOpCalibrationDurationMs: options.noOpCalibrationDurationMs,
        aggregateObserverIntervalMs: options.aggregateObserverIntervalMs,
        count: options.count,
        concurrency: options.concurrency,
        lovelace: options.lovelace,
        feeHeadroomLovelace: options.feeHeadroomLovelace,
        walletSeedPhrase: options.walletSeedPhrase,
        walletSeedPhraseEnv: options.walletSeedPhraseEnv,
        stressWalletSeedPhraseEnvs: parseStringListOption(
          options.stressWalletSeedPhraseEnv,
          "--stress-wallet-seed-phrase-env",
        ),
        l2Address: options.l2Address,
        runId: options.runId,
        outDir: options.outDir,
        pollIntervalMs: options.pollIntervalMs,
        pollInitialIntervalMs: options.pollInitialIntervalMs,
        pollMaxIntervalMs: options.pollMaxIntervalMs,
        submitRequestTimeoutMs: options.submitRequestTimeoutMs,
        acceptanceTimeoutMs: options.acceptanceTimeoutMs,
        commitObservationTimeoutMs: options.commitObservationTimeoutMs,
        finalityObserverMaxConcurrentRequests:
          options.finalityObserverMaxConcurrentRequests,
        maxSubmissionFailures: options.maxSubmissionFailures,
        network: process.env.NETWORK === "Mainnet" ? "Mainnet" : "Preprod",
        allowUnsafeBounds: options.unsafeAllowLargeStress === true,
      });
      const stressProgram = Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        const sql = yield* SqlClient.SqlClient;
        const nodeConfig = yield* Services.NodeConfig;
        const writeBehind = yield* Services.WriteBehind;
        const deploymentIdentity = yield* Services.ContractDeploymentIdentity;
        return yield* Effect.tryPromise({
          try: () =>
            E2EStressL2ThroughputCommand.runE2EL2StressThroughput(
              stressConfig,
              {
                submitTransfer: async (request) =>
                  await Effect.runPromise(
                    SubmitL2Transfer.submitL2TransferProgram({
                      config: request.config,
                      resolvedWalletSeedPhrase:
                        request.resolvedWalletSeedPhrase,
                      assertWalletAddress: (walletAddress) =>
                        assertUserCliWalletIsOperationallyIsolated({
                          commandName: "e2e-stress-l2-throughput",
                          walletAddress,
                          operatorMainAddress: lucidService.operatorMainAddress,
                          operatorMergeAddress:
                            lucidService.operatorMergeAddress,
                          referenceScriptsAddress:
                            lucidService.referenceScriptsWalletAddress,
                        }),
                    }).pipe(
                      Effect.provideService(Services.Lucid, lucidService),
                      Effect.provideService(SqlClient.SqlClient, sql),
                      Effect.provideService(Services.NodeConfig, nodeConfig),
                      Effect.provideService(Services.WriteBehind, writeBehind),
                      Effect.provideService(
                        Services.ContractDeploymentIdentity,
                        deploymentIdentity,
                      ),
                    ),
                  ),
                collectStageMetricSources: async ({ txHashes }) =>
                  await Effect.runPromise(
                    collectStressStageMetricSourcesFromSql(txHashes).pipe(
                      Effect.provideService(SqlClient.SqlClient, sql),
                    ),
                  ),
                collectGroundTruthMetrics: async ({
                  windowStart,
                  windowEnd,
                  txHashSample,
                  offeredCount,
                  calibrationProofRef,
                }) =>
                  await Effect.runPromise(
                    collectGroundTruthMetricsFromSql({
                      windowStart,
                      windowEnd,
                      txHashSample,
                      offeredCount,
                      trimFraction: 0.1,
                      calibrationProofRef,
                    }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
                  ),
                collectEnvironmentFingerprint: async ({
                  calibrationProofRef,
                }) =>
                  await collectEnvironmentFingerprint({
                    calibrationProofRef: calibrationProofRef ?? null,
                    configProfile: {
                      maxDurableAdmissionBacklog:
                        nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
                      waitBetweenBlockCommitment:
                        nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT,
                      waitBetweenBlockConfirmation:
                        nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION,
                      waitBetweenMergeTxs: nodeConfig.WAIT_BETWEEN_MERGE_TXS,
                      validationBatchSize: nodeConfig.VALIDATION_BATCH_SIZE,
                      validationPhaseAConcurrency:
                        nodeConfig.VALIDATION_PHASE_A_CONCURRENCY,
                    },
                  }),
                collectAggregateObserverSample: async ({ at }) =>
                  await Effect.runPromise(
                    Effect.gen(function* () {
                      const [
                        admissionRows,
                        mempoolRows,
                        processedRows,
                        pendingRows,
                      ] = yield* Effect.all(
                        [
                          sql<{
                            readonly status: string;
                            readonly count: bigint | number | string;
                          }>`SELECT status, COUNT(*)::bigint AS count FROM tx_admissions GROUP BY status ORDER BY status`,
                          sql<{
                            readonly count: bigint | number | string;
                          }>`SELECT COUNT(*)::bigint AS count FROM mempool`,
                          sql<{
                            readonly count: bigint | number | string;
                          }>`SELECT COUNT(*)::bigint AS count FROM processed_mempool`,
                          sql<{
                            readonly status: string;
                            readonly count: bigint | number | string;
                          }>`SELECT status, COUNT(*)::bigint AS count FROM pending_block_finalizations GROUP BY status ORDER BY status`,
                        ],
                        { concurrency: "unbounded" },
                      );
                      return {
                        at,
                        txAdmissions: Object.fromEntries(
                          admissionRows.map((row) => [
                            row.status,
                            BigInt(row.count).toString(),
                          ]),
                        ),
                        mempoolTxCount: BigInt(
                          mempoolRows[0]?.count ?? 0,
                        ).toString(),
                        processedMempoolTxCount: BigInt(
                          processedRows[0]?.count ?? 0,
                        ).toString(),
                        pendingBlockFinalizations: Object.fromEntries(
                          pendingRows.map((row) => [
                            row.status,
                            BigInt(row.count).toString(),
                          ]),
                        ),
                      };
                    }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
                  ),
                abortSignal: abortController.signal,
              },
            ),
          catch: (cause) =>
            cause instanceof Error ? cause : new Error(String(cause)),
        });
      });
      const result = await Effect.runPromise(
        pipe(
          stressProgram,
          Effect.provide(Services.WriteBehindLive),
          Effect.provide(Services.NodeConfig.layer),
          Effect.provide(Services.Database.layer),
          Effect.provide(Services.Lucid.Default),
          Effect.provide(Services.MidgardContractServices),
          Effect.provide(stressCliLoggerLayer),
        ),
      );
      writeJson(result.summary);
      if (result.summary.status === "interrupted") {
        process.exitCode = 130;
      }
    } catch (error) {
      failCli("e2e-stress-l2-throughput", error);
    } finally {
      process.off("SIGINT", interrupt);
      process.off("SIGTERM", interrupt);
    }
  });

program
  .command("e2e-clean-owned-process-group")
  .description(
    "Fail-closed cleanup of a detached e2e process group after validating its durable /proc ownership record",
  )
  .requiredOption("--record <path>", "Owned process-group record path")
  .requiredOption(
    "--run-token-env <name>",
    "Environment variable containing the private run token",
  )
  .action(
    async (options: {
      readonly record: string;
      readonly runTokenEnv: string;
    }) => {
      try {
        const result =
          await E2EProcessCleanupCommand.cleanupOwnedProcessGroupFromEnv({
            recordPath: options.record,
            runTokenEnv: options.runTokenEnv,
          });
        writeJson(result);
        if (!result.success) process.exitCode = 1;
      } catch (error) {
        failCli("e2e-clean-owned-process-group", error);
      }
    },
  );

program
  .command("phase4-t1-probe")
  .description(
    "Gated read-only canonical state_queue probe for the matched local-devnet T1 recovery gate",
  )
  .requiredOption(
    "--snapshot-identity-sha256 <hex>",
    "Matched-snapshot identity SHA-256",
  )
  .requiredOption("--attempt-id <id>", "Fresh T1 recovery attempt identity")
  .requiredOption(
    "--evidence-out <absolute-path>",
    "Fresh output file for exact probe evidence",
  )
  .option("--expected-tip-header-hash <hex>", "Expected 28-byte L2 tip hash")
  .option(
    "--expected-present-header-hash <hex>",
    "28-byte L2 header hash that must be canonical",
  )
  .option(
    "--expected-absent-header-hash <hex>",
    "28-byte L2 header hash that must not be canonical",
  )
  .action((opts) => {
    const mainEffect = provideTxServices(
      Phase4T1RecoveryCommand.phase4T1ProbeProgram({
        snapshotIdentitySha256: opts.snapshotIdentitySha256,
        attemptId: opts.attemptId,
        expectedTipHeaderHash: opts.expectedTipHeaderHash,
        expectedPresentHeaderHash: opts.expectedPresentHeaderHash,
        expectedAbsentHeaderHash: opts.expectedAbsentHeaderHash,
      }).pipe(
        Effect.tap((evidence) =>
          Effect.promise(() =>
            Phase4T1RecoveryCommand.writePhase4T1Evidence(
              opts.evidenceOut,
              evidence,
            ),
          ),
        ),
      ),
    );
    runCliEffect(mainEffect);
  });

program
  .command("phase4-t1-advance")
  .description(
    "Gated authenticated no-op canonical L2 advance for the matched local-devnet T1 recovery gate",
  )
  .requiredOption(
    "--snapshot-identity-sha256 <hex>",
    "Matched-snapshot identity SHA-256",
  )
  .requiredOption("--attempt-id <id>", "Fresh T1 recovery attempt identity")
  .requiredOption(
    "--expected-base-header-hash <hex>",
    "Expected 28-byte canonical L2 base B",
  )
  .requiredOption(
    "--abandoned-header-hash <hex>",
    "Submitted 28-byte L2 header N that must be absent",
  )
  .requiredOption(
    "--minimum-end-time-ms <ms>",
    "Minimum end time F must reach to advance past N",
  )
  .requiredOption(
    "--evidence-out <absolute-path>",
    "Fresh output file for exact canonical-advance evidence",
  )
  .action((opts) => {
    const mainEffect = provideTxServices(
      Phase4T1RecoveryCommand.phase4T1AdvanceProgram({
        snapshotIdentitySha256: opts.snapshotIdentitySha256,
        attemptId: opts.attemptId,
        expectedBaseHeaderHash: opts.expectedBaseHeaderHash,
        abandonedHeaderHash: opts.abandonedHeaderHash,
        minimumEndTimeMs: parsePositiveIntegerOption(
          opts.minimumEndTimeMs,
          "--minimum-end-time-ms",
        ),
      }).pipe(
        Effect.tap((evidence) =>
          Effect.promise(() =>
            Phase4T1RecoveryCommand.writePhase4T1Evidence(
              opts.evidenceOut,
              evidence,
            ),
          ),
        ),
      ),
    );
    runCliEffect(mainEffect);
  });

program
  .command("e2e-pipelined-commit-process-acceptance")
  .description(
    "Run the operator-enabled Phase 4 crash/restart and two-node process acceptance matrix against a matched local-devnet snapshot",
  )
  .action(async () => {
    try {
      console.log(
        JSON.stringify(await runPipelinedCommitProcessAcceptance(), null, 2),
      );
    } catch (error) {
      failCli("e2e-pipelined-commit-process-acceptance", error);
    }
  });

program
  .command("e2e-run-step")
  .description("Run one acceptance command through the structured e2e runner")
  .requiredOption("--id <id>", "Step id")
  .requiredOption("--cwd <path>", "Working directory")
  .requiredOption("--raw-log <path>", "Raw log path")
  .option("--summary-out <path>", "Write the step summary JSON to this path")
  .option("--timeout-ms <ms>", "Step timeout in milliseconds")
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .argument("<command>", "Command to execute")
  .argument("[args...]", "Command arguments")
  .action(async (command, args, opts) => {
    const timeoutMs =
      typeof opts.timeoutMs === "string"
        ? parsePositiveIntegerOption(opts.timeoutMs, "--timeout-ms")
        : undefined;
    try {
      const summary = await runCommandStep({
        id: opts.id,
        command,
        args,
        cwd: opts.cwd,
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        env: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        envInheritance: parseE2EEnvInheritanceOption(opts.envInheritance),
        rawLogPath: opts.rawLog,
        ...(timeoutMs === undefined ? {} : { timeoutMs }),
      });
      if (typeof opts.summaryOut === "string" && opts.summaryOut.length > 0) {
        await mkdir(dirname(opts.summaryOut), { recursive: true });
        await writeFile(
          opts.summaryOut,
          `${JSON.stringify(summary, null, 2)}\n`,
          "utf8",
        );
      }
      writeJson(summary);
      if (summary.status !== "success") {
        process.exitCode = 1;
      }
    } catch (error) {
      failCli("e2e-run-step", error);
    }
  });

program
  .command("da-libp2p-generate-manifest")
  .description("Generate a target-specific libp2p DA runtime manifest")
  .requiredOption("--target <target>", "Runtime target: producer or watcher")
  .requiredOption(
    "--profile <profile>",
    `Address profile: ${DA_LIBP2P_RUNTIME_PROFILES.join(", ")}`,
  )
  .requiredOption(
    "--contract-deployment-info <path>",
    "Finalized V1 contract deployment info path; deployment.fingerprint is derived from manifestId",
  )
  .requiredOption(
    "--producer-libp2p-key-source <source>",
    "Producer DA_LIBP2P_PRIVATE_KEY_SOURCE",
  )
  .requiredOption(
    "--public-retained-da-libp2p-key-source <source>",
    "Dedicated non-signer DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE",
  )
  .requiredOption("--threshold <n>", "DA committee threshold")
  .option(
    "--committee-member <spec>",
    "Committee member as signerIndex,daVkey,libp2pKeySource,role+role; repeatable",
    collectStringOption,
    [],
  )
  .requiredOption("--network <network>", "Exact Cardano network label")
  .option("--local-signer-index <n>", "Local watcher signer index")
  .option("--producer-port <port>", "Producer retrieval libp2p port")
  .option("--watcher-port <port>", "Watcher libp2p port")
  .option("--public-retained-da-port <port>", "Public retained-DA libp2p port")
  .option("--producer-service-name <name>", "Compose producer service DNS name")
  .option("--watcher-service-name <name>", "Compose watcher service DNS name")
  .option("--producer-public-host <host>", "Public producer DNS/IP")
  .option("--watcher-public-host <host>", "Public watcher DNS/IP")
  .option(
    "--public-retained-da-public-host <host>",
    "Public retained-DA DNS/IP (defaults to --watcher-public-host)",
  )
  .option("--out <path>", "Write manifest JSON to this path")
  .action(async (options) => {
    const opts = typeof options.opts === "function" ? options.opts() : options;
    try {
      const committeeMembers = parseDaLibp2pCommitteeMembers(
        opts.committeeMember,
      );
      if (committeeMembers.length === 0) {
        throw new Error("at least one --committee-member is required");
      }
      const manifest = await generateDaLibp2pRuntimeManifest({
        target: parseDaLibp2pRuntimeTarget(opts.target),
        profile: parseDaLibp2pRuntimeProfile(opts.profile),
        contractDeploymentInfoPath: opts.contractDeploymentInfo,
        producerPrivateKeySource: opts.producerLibp2pKeySource,
        publicRetainedDaPrivateKeySource: opts.publicRetainedDaLibp2pKeySource,
        committeeMembers,
        threshold: parsePositiveIntegerOption(opts.threshold, "--threshold"),
        network: opts.network,
        ...(typeof opts.localSignerIndex === "string"
          ? {
              localSignerIndex: parseNonNegativeIntegerOption(
                opts.localSignerIndex,
                "--local-signer-index",
              ),
            }
          : {}),
        ...(typeof opts.producerPort === "string"
          ? {
              producerPort: parsePositiveIntegerOption(
                opts.producerPort,
                "--producer-port",
              ),
            }
          : {}),
        ...(typeof opts.watcherPort === "string"
          ? {
              watcherPort: parsePositiveIntegerOption(
                opts.watcherPort,
                "--watcher-port",
              ),
            }
          : {}),
        ...(typeof opts.publicRetainedDaPort === "string"
          ? {
              publicRetainedDaPort: parsePositiveIntegerOption(
                opts.publicRetainedDaPort,
                "--public-retained-da-port",
              ),
            }
          : {}),
        ...(typeof opts.producerServiceName === "string"
          ? { producerServiceName: opts.producerServiceName }
          : {}),
        ...(typeof opts.watcherServiceName === "string"
          ? { watcherServiceName: opts.watcherServiceName }
          : {}),
        ...(typeof opts.producerPublicHost === "string"
          ? { producerPublicHost: opts.producerPublicHost }
          : {}),
        ...(typeof opts.watcherPublicHost === "string"
          ? { watcherPublicHost: opts.watcherPublicHost }
          : {}),
        ...(typeof opts.publicRetainedDaPublicHost === "string"
          ? { publicRetainedDaPublicHost: opts.publicRetainedDaPublicHost }
          : {}),
      });
      if (typeof opts.out === "string" && opts.out.length > 0) {
        await writeDaLibp2pRuntimeManifest(opts.out, manifest);
      }
      writeJson(manifest);
    } catch (error) {
      failCli("da-libp2p-generate-manifest", error);
    }
  });

program
  .command("da-libp2p-preflight")
  .description("Probe libp2p DA committee reachability from the producer")
  .option("--json", "Print machine-readable JSON", true)
  .option(
    "--mode <mode>",
    "Preflight mode: bind-listen validates producer listener binding before startup; dial-only probes peers without binding after startup",
    "bind-listen",
  )
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .action(async (options) => {
    const opts = typeof options.opts === "function" ? options.opts() : options;
    try {
      const { env } = await buildE2EProcessEnv({
        cwd: process.cwd(),
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        overrides: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        inherit: parseE2EEnvInheritanceOption(opts.envInheritance),
      });
      const report = await runDaLibp2pPreflightFromEnv(env, {
        mode: parseDaLibp2pPreflightMode(opts.mode),
      });
      writeJson(report);
      if (!report.passed) {
        process.exitCode = 1;
      }
    } catch (error) {
      failCli("da-libp2p-preflight", error);
    }
  });

program
  .command("e2e-start-service")
  .description(
    "Start a long-running e2e service, write a PID file, and wait for readiness",
  )
  .requiredOption("--service <name>", "Service label")
  .requiredOption("--cwd <path>", "Working directory")
  .requiredOption("--raw-log <path>", "Raw log path")
  .requiredOption("--pid-file <path>", "PID file path")
  .requiredOption("--ready-url <url>", "Readiness endpoint URL")
  .option("--health-url <url>", "Optional health endpoint URL")
  .option("--ready-timeout-ms <ms>", "Readiness timeout", "120000")
  .option("--poll-interval-ms <ms>", "Readiness polling interval", "5000")
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .argument("<command>", "Command to execute")
  .argument("[args...]", "Command arguments")
  .action(async (command, args, opts) => {
    try {
      const summary = await E2EServiceCommand.startManagedService({
        service: opts.service,
        command,
        args,
        cwd: opts.cwd,
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        env: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        envInheritance: parseE2EEnvInheritanceOption(opts.envInheritance),
        rawLogPath: opts.rawLog,
        pidFilePath: opts.pidFile,
        readyUrl: opts.readyUrl,
        ...(typeof opts.healthUrl === "string"
          ? { healthUrl: opts.healthUrl }
          : {}),
        readyTimeoutMs: parsePositiveIntegerOption(
          opts.readyTimeoutMs,
          "--ready-timeout-ms",
        ),
        pollIntervalMs: parsePositiveIntegerOption(
          opts.pollIntervalMs,
          "--poll-interval-ms",
        ),
      });
      writeJson(summary);
    } catch (error) {
      failCli("e2e-start-service", error);
    }
  });

program
  .command("prepare-hub-oracle-one-shot-nonce")
  .description(
    "Create a fresh marked operator-wallet UTxO for HUB_ORACLE_ONE_SHOT_* in a new deployment",
  )
  .option(
    "--amount-lovelace <lovelace>",
    "Lovelace to lock in the marked nonce output",
    PrepareHubOracleNonce.DEFAULT_NONCE_LOVELACE.toString(10),
  )
  .option(
    "--dry-run",
    "Only inspect operator-wallet readiness; do not submit a transaction",
  )
  .option(
    "--run-state <path>",
    "Deployment run-state path used to prevent accidental identity replacement",
  )
  .option(
    "--fresh-redeploy",
    "Allow creation of a replacement deployment identity",
  )
  .option(
    "--fresh-redeploy-reason <text>",
    "Required reason when --fresh-redeploy is used",
  )
  .option("--json", "Print machine-readable JSON")
  .action(async (_args, options) => {
    const opts = options.opts();
    const runOptions =
      DeploymentRunStateCommand.resolveDeploymentRunCliOptions(opts);
    let amountLovelace: bigint;
    try {
      amountLovelace = PrepareHubOracleNonce.parseNonceLovelaceOption(
        opts.amountLovelace,
      );
    } catch (error) {
      failCli("prepare-hub-oracle-one-shot-nonce", error);
      return;
    }

    let pendingAttempt: DeploymentRunStateCommand.PendingHubOracleNonceAttempt | null =
      null;
    if (!opts.dryRun && !runOptions.freshRedeploy) {
      try {
        pendingAttempt =
          await DeploymentRunStateCommand.loadPendingHubOracleNonceAttempt({
            options: runOptions,
          });
      } catch (error) {
        failCli("prepare-hub-oracle-one-shot-nonce", error);
        return;
      }
    }

    if (pendingAttempt !== null) {
      const attempt = pendingAttempt;
      const mainEffect = provideLucidOnlyServices(
        Effect.gen(function* () {
          const nodeConfig = yield* Services.NodeConfig;
          const result =
            yield* PrepareHubOracleNonce.reconcileHubOracleOneShotNonceAttemptProgram(
              attempt,
              {
                onTxHashConfirmed: (confirmedAttempt, confirmationStatus) =>
                  Effect.tryPromise({
                    try: () =>
                      DeploymentRunStateCommand.recordHubOracleNonceTxHashConfirmed(
                        {
                          options: runOptions,
                          network: nodeConfig.NETWORK,
                          txHash: confirmedAttempt.txHash,
                          address: confirmedAttempt.address,
                          lovelace: confirmedAttempt.lovelace,
                          inlineDatum: confirmedAttempt.inlineDatum,
                          confirmationStatus,
                        },
                      ),
                    catch: (cause) =>
                      cause instanceof Error
                        ? cause
                        : new Error(
                            `Failed to record confirmed hub-oracle nonce tx in run state: ${String(cause)}`,
                          ),
                  }),
              },
            );
          yield* Effect.tryPromise({
            try: () =>
              DeploymentRunStateCommand.recordHubOracleNonce({
                options: runOptions,
                network: nodeConfig.NETWORK,
                txHash: result.txHash,
                outputIndex: result.outputIndex,
                outRef: result.outRef,
              }),
            catch: (cause) =>
              cause instanceof Error
                ? cause
                : new Error(
                    `Failed to record hub-oracle nonce in run state: ${String(cause)}`,
                  ),
          });
          return result;
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              if (opts.json) {
                writeJson(result);
                return;
              }
              process.stdout.write(
                [
                  `reconciled hub-oracle one-shot nonce: ${result.outRef}`,
                  `HUB_ORACLE_ONE_SHOT_TX_HASH=${result.txHash}`,
                  `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX=${result.outputIndex.toString()}`,
                  `confirmationStatus=${result.confirmationStatus}`,
                  `address=${result.address}`,
                  `lovelace=${result.lovelace}`,
                ].join("\n") + "\n",
              );
            }),
          ),
        ),
      );
      runCliEffect(mainEffect);
      return;
    }

    try {
      if (!opts.dryRun) {
        await DeploymentRunStateCommand.guardHubOracleNonceCreation({
          options: runOptions,
        });
      }
    } catch (error) {
      failCli("prepare-hub-oracle-one-shot-nonce", error);
      return;
    }

    if (opts.dryRun) {
      const mainEffect = provideLucidOnlyServices(
        PrepareHubOracleNonce.inspectOperatorWalletForNonceProgram(
          amountLovelace,
        ).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              if (opts.json) {
                writeJson(result);
                return;
              }
              process.stdout.write(
                [
                  `operator address=${result.address}`,
                  `requested nonce lovelace=${result.requestedNonceLovelace}`,
                  `spendable utxos=${result.spendableUtxos.length.toString()}`,
                  `total spendable lovelace=${result.totalSpendableLovelace}`,
                ].join("\n") + "\n",
              );
            }),
          ),
        ),
      );
      runCliEffect(mainEffect);
      return;
    }

    const mainEffect = provideLucidOnlyServices(
      Effect.gen(function* () {
        const nodeConfig = yield* Services.NodeConfig;
        const result =
          yield* PrepareHubOracleNonce.prepareHubOracleOneShotNonceProgram(
            amountLovelace,
            {
              onSubmitted: (attempt) =>
                Effect.tryPromise({
                  try: () =>
                    DeploymentRunStateCommand.recordHubOracleNonceSubmitted({
                      options: runOptions,
                      network: nodeConfig.NETWORK,
                      txHash: attempt.txHash,
                      address: attempt.address,
                      lovelace: attempt.lovelace,
                      inlineDatum: attempt.inlineDatum,
                    }),
                  catch: (cause) =>
                    cause instanceof Error
                      ? cause
                      : new Error(
                          `Failed to record submitted hub-oracle nonce in run state: ${String(cause)}`,
                        ),
                }),
              onTxHashConfirmed: (attempt, confirmationStatus) =>
                Effect.tryPromise({
                  try: () =>
                    DeploymentRunStateCommand.recordHubOracleNonceTxHashConfirmed(
                      {
                        options: runOptions,
                        network: nodeConfig.NETWORK,
                        txHash: attempt.txHash,
                        address: attempt.address,
                        lovelace: attempt.lovelace,
                        inlineDatum: attempt.inlineDatum,
                        confirmationStatus,
                      },
                    ),
                  catch: (cause) =>
                    cause instanceof Error
                      ? cause
                      : new Error(
                          `Failed to record confirmed hub-oracle nonce tx in run state: ${String(cause)}`,
                        ),
                }),
            },
          );
        yield* Effect.tryPromise({
          try: () =>
            DeploymentRunStateCommand.recordHubOracleNonce({
              options: runOptions,
              network: nodeConfig.NETWORK,
              txHash: result.txHash,
              outputIndex: result.outputIndex,
              outRef: result.outRef,
            }),
          catch: (cause) =>
            cause instanceof Error
              ? cause
              : new Error(
                  `Failed to record hub-oracle nonce in run state: ${String(cause)}`,
                ),
        });
        return result;
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            if (opts.json) {
              writeJson(result);
              return;
            }
            process.stdout.write(
              [
                `prepared hub-oracle one-shot nonce: ${result.outRef}`,
                `HUB_ORACLE_ONE_SHOT_TX_HASH=${result.txHash}`,
                `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX=${result.outputIndex.toString()}`,
                `confirmationStatus=${result.confirmationStatus}`,
                `address=${result.address}`,
                `lovelace=${result.lovelace}`,
              ].join("\n") + "\n",
            );
          }),
        ),
      ),
    );
    runCliEffect(mainEffect);
  });

program
  .command("register-phas-membership-reward-account")
  .description(
    "Explicitly register the canonical PHAS membership reward account for an existing deployment",
  )
  .action(async () => {
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        yield* lucidService.switchToOperatorsMainWallet;
        return yield* PhasMembershipRegistration.ensurePhasMembershipRewardAccountRegisteredProgram(
          lucidService.api,
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(
            `register-phas-membership-reward-account completed: ${formatJson(
              result,
            )}`,
          ),
        ),
      ),
    );

    runCliEffect(mainEffect);
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

    runCliEffect(mainEffect);
  });

for (const commandName of RegisterActiveOperator.REFERENCE_SCRIPT_COMMAND_NAMES) {
  program
    .command(`deploy-reference-script-${commandName}`)
    .description(`Publish reference scripts for ${commandName}`)
    .option(
      "--contract-deployment-info-output <path>",
      "Optional override path for the contract deployment info JSON written after reference-script deployment completes",
    )
    .option(
      "--plan-only",
      "Print the reference-script deployment plan without publishing transactions",
    )
    .option(
      "--run-state <path>",
      "Deployment run-state path used to resume reference-script auth policy identity",
    )
    .option(
      "--fresh-redeploy",
      "Create a replacement reference-script auth policy instead of reusing run-state/manifest identity",
    )
    .option(
      "--fresh-redeploy-reason <text>",
      "Required reason when --fresh-redeploy is used",
    )
    .action(async (_args, options) => {
      const commandOptions = options.opts();
      const { contractDeploymentInfoOutput, planOnly } = commandOptions;
      const runOptions =
        DeploymentRunStateCommand.resolveDeploymentRunCliOptions(
          commandOptions,
        );
      const mainEffect = provideReferenceScriptDeploymentServices(
        Effect.gen(function* () {
          const nodeConfig = yield* Services.NodeConfig;
          const lucidService = yield* Services.Lucid;
          yield* lucidService.switchToOperatorsMainWallet;
          yield* lucidService.switchToReferenceScriptWallet;
          const manifestOutputPath =
            typeof contractDeploymentInfoOutput === "string"
              ? contractDeploymentInfoOutput
              : ContractDeploymentInfo.defaultContractDeploymentInfoOutputPath();
          const authPolicy =
            yield* DeploymentRunStateCommand.resolveReferenceScriptAuthPolicyProgram(
              {
                options: runOptions,
                lucid: lucidService.referenceScriptsApi,
                network: nodeConfig.NETWORK,
                hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
                hubOracleOneShotOutputIndex:
                  nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
                timelockDurationMs:
                  nodeConfig.REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
                manifestOutputPath,
                persistRunState: planOnly !== true,
              },
            );
          const baseContracts = yield* Services.AlwaysSucceedsContract;
          const contracts =
            yield* Services.withRealStateQueueAndOperatorContracts(
              nodeConfig.NETWORK,
              baseContracts,
              {
                txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
                outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
              },
              {
                referenceScriptAuth: authPolicy,
              },
            );
          yield* Effect.try({
            try: () =>
              assertReferenceScriptAuthMinimumRemaining({
                policy: contracts.referenceScriptAuth,
                nowMs: Date.now(),
                minRemainingMs:
                  nodeConfig.REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
                scopeName: `deploy-reference-script-${commandName}`,
                targetNames: referenceScriptTargetsByCommand(contracts)[
                  commandName
                ].map(({ name }) => name),
              }),
            catch: (cause) =>
              cause instanceof Error
                ? cause
                : new Error(
                    `Reference-script auth guard failed: ${String(cause)}`,
                  ),
          });
          if (planOnly === true) {
            const plan = yield* planReferenceScriptCommandProgram(
              lucidService.referenceScriptsApi,
              contracts,
              commandName,
              contracts.referenceScriptAuth,
              lucidService.referenceScriptsAddress,
            );
            writeJson(plan);
            return { mode: "plan" as const, plan };
          }
          const published =
            yield* RegisterActiveOperator.deployReferenceScriptCommandProgram(
              lucidService.referenceScriptsApi,
              contracts,
              commandName,
              contracts.referenceScriptAuth,
              lucidService.api,
              lucidService.referenceScriptsAddress,
              nodeConfig.REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
              new Set([
                `${nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH}#${nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX.toString()}`,
              ]),
            );
          return { mode: "publish" as const, published };
        }).pipe(
          Effect.tap((result) => {
            if (result.mode === "plan") {
              return Effect.logInfo(
                `deploy-reference-script-${commandName} plan-only completed: ${formatJson(result.plan)}`,
              );
            }
            return Effect.logInfo(
              `deploy-reference-script-${commandName} completed: ${JSON.stringify(
                result.published.map(({ name, utxo }) => ({
                  name,
                  outRef: `${utxo.txHash}#${utxo.outputIndex}`,
                })),
              )}`,
            );
          }),
        ),
      );

      runCliEffect(mainEffect);
    });
}

program
  .command("reference-script-wallet-status")
  .description(
    "Print total, plain ADA-only, and scriptRef/token-bearing balances for L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS",
  )
  .option("--json", "Print machine-readable JSON", true)
  .action(async () => {
    const mainEffect = provideLucidOnlyServices(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        yield* lucidService.switchToReferenceScriptWallet;
        return yield* referenceScriptWalletStatusProgram(
          lucidService.referenceScriptsApi,
          lucidService.referenceScriptsAddress,
        );
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("sweep-reference-script-wallet")
  .description(
    "Retire published reference-script UTxOs, quarantine non-ADA assets, and consolidate recovered ADA back to the reference-script wallet",
  )
  .option(
    "--burn-address <address>",
    "L1 address that receives non-ADA assets; this quarantines tokens unless their minting policies are still burnable",
  )
  .option(
    "--execute",
    "Submit the sweep transaction. Without this flag the command only prints the plan.",
  )
  .option(
    "--i-am-retiring-reference-scripts",
    "Required with --execute; confirms the published reference scripts at L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS are no longer live.",
  )
  .option(
    "--include-plain",
    "Also collect plain ADA-only UTxOs at L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS for full wallet consolidation.",
  )
  .option(
    "--token-output-lovelace <lovelace>",
    "Lovelace attached to each token quarantine output.",
    (value) => {
      parsePositiveBigIntOption(value, "--token-output-lovelace");
      return value.trim();
    },
    REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE.toString(),
  )
  .option(
    "--max-assets-per-token-output <count>",
    "Maximum non-ADA asset units per token quarantine output.",
    (value) =>
      parsePositiveIntegerOption(value, "--max-assets-per-token-output"),
    REFERENCE_SCRIPT_SWEEP_DEFAULT_MAX_ASSETS_PER_TOKEN_OUTPUT,
  )
  .action(async (_args, options) => {
    const opts = options.opts() as {
      readonly burnAddress?: string;
      readonly execute?: boolean;
      readonly iAmRetiringReferenceScripts?: boolean;
      readonly includePlain?: boolean;
      readonly tokenOutputLovelace: string;
      readonly maxAssetsPerTokenOutput: number;
    };
    const mainEffect = provideLucidOnlyServices(
      Effect.gen(function* () {
        const nodeConfig = yield* Services.NodeConfig;
        const lucidService = yield* Services.Lucid;
        yield* lucidService.switchToReferenceScriptWallet;
        const burnAddress = yield* Effect.try({
          try: () =>
            opts.burnAddress === undefined
              ? undefined
              : parseL1AddressOption(
                  opts.burnAddress,
                  "--burn-address",
                  nodeConfig.NETWORK,
                ),
          catch: (cause) =>
            cause instanceof Error
              ? cause
              : new Error(`Failed to parse --burn-address: ${String(cause)}`),
        });
        const tokenOutputLovelace = yield* Effect.try({
          try: () =>
            parsePositiveBigIntOption(
              opts.tokenOutputLovelace,
              "--token-output-lovelace",
            ),
          catch: (cause) =>
            cause instanceof Error
              ? cause
              : new Error(
                  `Failed to parse --token-output-lovelace: ${String(cause)}`,
                ),
        });
        return yield* sweepReferenceScriptWalletProgram(
          lucidService.referenceScriptsApi,
          lucidService.referenceScriptsAddress,
          {
            burnAddress,
            execute: opts.execute === true,
            acknowledgeRetirement: opts.iAmRetiringReferenceScripts === true,
            includePlainUtxos: opts.includePlain === true,
            tokenOutputLovelace,
            maxAssetsPerTokenOutput: opts.maxAssetsPerTokenOutput,
          },
        );
      }).pipe(
        Effect.tap((result) =>
          Effect.logInfo(
            `sweep-reference-script-wallet completed: ${formatJson(result)}`,
          ),
        ),
      ),
    );

    runCliEffect(mainEffect);
  });

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

    runCliEffect(mainEffect);
  });

program
  .command("activate-operator")
  .description(
    "Activate an already registered operator wallet without rerunning registration or deregistration",
  )
  .action(async () => {
    const mainEffect = pipe(
      RegisterActiveOperator.activateProgram,
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
      Effect.tap((result) =>
        Effect.logInfo(
          `activate-operator completed: ${JSON.stringify(result)}`,
        ),
      ),
    );

    runCliEffect(mainEffect);
  });

program
  .command("commit-explicit-block-header")
  .description(
    "Commit a state_queue block header with caller-supplied roots using the live operator path",
  )
  .requiredOption("--utxos-root <hex>", "Committed UTxO MPF root")
  .requiredOption(
    "--transactions-root <hex>",
    "Committed Midgard-native transaction MPF root",
  )
  .requiredOption("--deposits-root <hex>", "Committed deposits MPF root")
  .requiredOption("--withdrawals-root <hex>", "Committed withdrawals MPF root")
  .option(
    "--l2-transaction-count <n>",
    "L2 transaction count to commit in the header (must be > 0 for a non-empty transactions root)",
  )
  .option(
    "--transition-trace-root <hex>",
    "Committed transition-trace MPF root (required non-empty when total event count > 0)",
  )
  .option(
    "--event-to-step-root <hex>",
    "Committed event-to-step MPF root (required non-empty when total event count > 0)",
  )
  .option(
    "--end-time-ms <ms>",
    "Optional candidate block end time in POSIX milliseconds",
  )
  .requiredOption(
    "--unsafe-commit-caller-supplied-roots",
    "Acknowledge that this submits caller-supplied roots and is only for explicit fault-proof drills",
  )
  .option("--no-await-confirmation", "Submit without waiting for confirmation")
  .action(async (opts) => {
    const params = {
      utxosRoot: parseMerkleRootOption(opts.utxosRoot, "--utxos-root"),
      transactionsRoot: parseMerkleRootOption(
        opts.transactionsRoot,
        "--transactions-root",
      ),
      depositsRoot: parseMerkleRootOption(opts.depositsRoot, "--deposits-root"),
      withdrawalsRoot: parseMerkleRootOption(
        opts.withdrawalsRoot,
        "--withdrawals-root",
      ),
      l2TransactionCount:
        opts.l2TransactionCount === undefined
          ? undefined
          : BigInt(opts.l2TransactionCount),
      transitionTraceRoot:
        opts.transitionTraceRoot === undefined
          ? undefined
          : parseMerkleRootOption(
              opts.transitionTraceRoot,
              "--transition-trace-root",
            ),
      eventToStepRoot:
        opts.eventToStepRoot === undefined
          ? undefined
          : parseMerkleRootOption(opts.eventToStepRoot, "--event-to-step-root"),
      endTimeMs: parseOptionalEndTimeMs(opts.endTimeMs),
      awaitConfirmation: opts.awaitConfirmation !== false,
    };
    const mainEffect = provideTxServices(
      commitExplicitBlockHeaderProgram(params).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("attest-state-queue-once")
  .description(
    "Mint, threshold-sign, and attach DA attestations for queued state_queue headers",
  )
  .option(
    "--header-hash <hex>",
    "Optional 28-byte state_queue header hash to attest; defaults to all unattested queued headers",
  )
  .action(async (_args, options) => {
    let headerHash: string | undefined;
    try {
      headerHash = parseOptionalHeaderHashOption(options.opts().headerHash);
    } catch (error) {
      failCli("attest-state-queue-once", error);
      return;
    }

    const mainEffect = provideTxServices(
      DaAttestation.attestStateQueueOnceProgram({ headerHash }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
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
  .option("--l2-datum <hex>", "Optional L2 inline datum bytes as hex")
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
      let resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
      try {
        const { l2Address, l2Datum, lovelace, walletSeedPhraseEnv } = options;
        depositConfig = SubmitDeposit.parseSubmitDepositConfig({
          l2Address,
          l2Datum,
          lovelace,
          assetSpecs,
        });
        resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhraseEnv,
        });
      } catch (error) {
        failCli("submit-deposit", error);
        return;
      }

      const mainEffect = provideDatabaseTxServices(
        Effect.gen(function* () {
          const lucidService = yield* Services.Lucid;
          const contracts = yield* Services.MidgardContracts;
          yield* Effect.sync(() =>
            lucidService.api.selectWallet.fromSeed(
              resolvedWalletSeedPhrase.seedPhrase,
            ),
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
          return yield* SubmitDeposit.submitDepositWithMetadataProgram(
            lucidService.api,
            contracts,
            { ...depositConfig, referenceScripts: depositReferenceScripts },
          );
        }).pipe(
          tapJson(),
          Effect.tap((result) =>
            Effect.logInfo(`submit-deposit completed: txHash=${result.txHash}`),
          ),
        ),
      );

      runCliEffect(mainEffect);
    },
  );

program
  .command("reconcile-deposit-submission")
  .description(
    "Reconcile a previously submitted deposit transaction before retrying after a confirmation timeout",
  )
  .requiredOption("--tx-hash <hex>", "32-byte Cardano transaction hash")
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: { readonly txHash: string; readonly json?: boolean }) => {
      let txHash: string;
      try {
        txHash = normalizeHex(options.txHash, {
          byteLength: 32,
          trim: false,
        });
      } catch (error) {
        failCli("reconcile-deposit-submission: invalid --tx-hash", error);
        return;
      }

      const mainEffect = provideDatabaseTxServices(
        SubmitDeposit.reconcileDepositSubmissionAttemptProgram(txHash).pipe(
          tapJson(),
        ),
      );

      runCliEffect(mainEffect);
    },
  );

program
  .command("submit-l2-transfer")
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
    DEFAULT_WALLET_SEED_ENV,
  )
  .option(
    "--endpoint <url>",
    "Midgard node HTTP endpoint used for /utxos and /submit",
    defaultMidgardNodeEndpoint(),
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
      let resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
      try {
        transferConfig = SubmitL2Transfer.parseSubmitL2TransferConfig({
          l2Address: options.l2Address,
          lovelace: options.lovelace,
          assetSpecs,
          nodeEndpoint: options.endpoint,
          submissionMode: options.submissionMode,
        });
        resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
          walletSeedPhrase: options.walletSeedPhrase,
          walletSeedPhraseEnv: options.walletSeedPhraseEnv,
        });
      } catch (error) {
        failCli("submit-l2-transfer", error);
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
                referenceScriptsAddress:
                  lucidService.referenceScriptsWalletAddress,
              }),
          });
          return result;
        }).pipe(
          tapJson(),
          Effect.tapError((error) =>
            Effect.logError(
              `submit-l2-transfer failed: ${errorMessage(error)}`,
            ),
          ),
        ),
        Effect.provide(Services.WriteBehindLive),
        Effect.provide(Services.Lucid.Default),
        Effect.provide(Services.Database.layer),
        Effect.provide(Services.NodeConfig.layer),
        Effect.provide(Services.MidgardContractServices),
      );

      runCliEffect(mainEffect);
    },
  );

program
  .command("submit-withdrawal")
  .description(
    "Submit an authenticated L1 withdrawal order for a selected Midgard L2 UTxO",
  )
  .requiredOption(
    "--l2-out-ref <txHash#outputIndex>",
    "Midgard L2 UTxO to withdraw, in txHash#outputIndex form",
  )
  .requiredOption(
    "--l1-address <address>",
    "Cardano L1 address that should receive the payout",
  )
  .option(
    "--wallet-seed-phrase <seedPhrase>",
    "Optional seed phrase used directly for the withdrawal signer",
  )
  .option(
    "--wallet-seed-phrase-env <envVar>",
    "Environment variable containing the withdrawal signer seed phrase",
    "USER_WALLET",
  )
  .option(
    "--l1-datum <hex>",
    "Optional inline payout datum as Plutus data CBOR",
  )
  .option(
    "--refund-address <address>",
    "Optional refund address for invalid withdrawals; defaults to --l1-address",
  )
  .option(
    "--refund-datum <hex>",
    "Optional invalid-withdrawal refund datum as Plutus data CBOR",
  )
  .option(
    "--order-lovelace <amount>",
    "Optional lovelace held by the withdrawal order UTxO",
  )
  .option(
    "--endpoint <url>",
    "Midgard node HTTP endpoint used for L2 UTxO lookup",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = pipe(
      Effect.gen(function* () {
        const lucidService = yield* Services.Lucid;
        return yield* SubmitWithdrawalCommand.submitWithdrawalCommandProgram({
          config: {
            walletSeedPhrase: opts.walletSeedPhrase,
            walletSeedPhraseEnv: opts.walletSeedPhraseEnv,
            l2OutRef: opts.l2OutRef,
            l1Address: opts.l1Address,
            l1Datum: opts.l1Datum,
            refundAddress: opts.refundAddress,
            refundDatum: opts.refundDatum,
            orderLovelace: opts.orderLovelace,
            endpoint: opts.endpoint,
          },
          assertWalletAddress: (walletAddress) =>
            assertUserCliWalletIsOperationallyIsolated({
              commandName: "submit-withdrawal",
              walletAddress,
              operatorMainAddress: lucidService.operatorMainAddress,
              operatorMergeAddress: lucidService.operatorMergeAddress,
              referenceScriptsAddress:
                lucidService.referenceScriptsWalletAddress,
            }),
        });
      }).pipe(tapJson()),
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
    );

    runCliEffect(mainEffect);
  });

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
      address = parseAddressArgument(options.opts().address);
    } catch (error) {
      failCli("utxos", error);
      return;
    }

    const mainEffect = provideDatabaseServices(
      UtxosCommand.utxosProgram(address).pipe(
        Effect.flatMap((result) =>
          Effect.sync(() => {
            writeJson(result);
          }),
        ),
      ),
    );

    runCliEffect(mainEffect);
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

    runCliEffect(mainEffect);
  });

program
  .command("fetch-withdrawals-once")
  .description(
    "Fetch visible withdrawal order UTxOs from L1 once and reconcile them into withdrawal_utxos",
  )
  .action(async () => {
    const mainEffect = provideNodeRuntimeServices(
      FetchWithdrawalsOnceCommand.fetchWithdrawalsOnceProgram.pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("resolve-event-settlement-proof")
  .description(
    "Resolve a deposit, withdrawal, or tx-order event's settlement UTxO and membership proof",
  )
  .requiredOption(
    "--kind <kind>",
    'Event kind: "deposit", "withdrawal", or "tx-order"',
  )
  .requiredOption("--event-id <hex>", "Canonical OutputReference CBOR event id")
  .action(async (_args, options) => {
    const opts = options.opts();
    let lookup: EventSettlementProofCommand.EventSettlementProofLookup;
    try {
      lookup = EventSettlementProofCommand.parseEventSettlementProofLookup({
        kind: opts.kind,
        eventId: opts.eventId,
      });
    } catch (error) {
      failCli("resolve-event-settlement-proof", error);
      return;
    }

    const mainEffect = provideDatabaseTxServices(
      EventSettlementProofCommand.resolveEventSettlementProofProgram(
        lookup,
      ).pipe(
        tapJson(
          EventSettlementProofCommand.serializeEventSettlementProofResolution,
        ),
      ),
    );

    runCliEffect(mainEffect);
  });

program
  .command("absorb-confirmed-deposit-to-reserve")
  .description("Absorb a confirmed deposit event into the Midgard reserve")
  .requiredOption(
    "--deposit-event-id <hex>",
    "Canonical OutputReference CBOR deposit event id",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = provideDatabaseTxServices(
      ReservePayoutCommand.absorbConfirmedDepositToReserveProgram({
        eventId: opts.depositEventId,
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("initialize-payout")
  .description("Initialize payout for a valid confirmed withdrawal event")
  .requiredOption(
    "--withdrawal-event-id <hex>",
    "Canonical OutputReference CBOR withdrawal event id",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = provideDatabaseTxServices(
      ReservePayoutCommand.initializePayoutProgram({
        eventId: opts.withdrawalEventId,
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("add-reserve-funds-to-payout")
  .description("Move reserve funds into an initialized payout accumulator")
  .requiredOption(
    "--withdrawal-event-id <hex>",
    "Canonical OutputReference CBOR withdrawal event id",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = provideDatabaseTxServices(
      ReservePayoutCommand.addReserveFundsToPayoutProgram({
        eventId: opts.withdrawalEventId,
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("conclude-payout")
  .description("Conclude a fully funded payout to the withdrawal target")
  .requiredOption(
    "--withdrawal-event-id <hex>",
    "Canonical OutputReference CBOR withdrawal event id",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = provideDatabaseTxServices(
      ReservePayoutCommand.concludePayoutProgram({
        eventId: opts.withdrawalEventId,
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("withdrawal-status")
  .description("Print the local status of a withdrawal event")
  .option(
    "--event-id <hex>",
    "Canonical OutputReference CBOR withdrawal event id",
  )
  .option("--l1-tx-hash <hex>", "Withdrawal order L1 transaction hash")
  .action(async (_args, options) => {
    const opts = options.opts();
    let lookup: WithdrawalStatusCommand.WithdrawalStatusLookup;
    try {
      lookup = WithdrawalStatusCommand.parseWithdrawalStatusLookup({
        eventId: opts.eventId,
        l1TxHash: opts.l1TxHash,
      });
    } catch (error) {
      failCli("withdrawal-status", error);
      return;
    }

    const mainEffect = provideDatabaseTxServices(
      WithdrawalStatusCommand.withdrawalStatusProgram(lookup).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("reserve-utxos")
  .description("Print typed reserve-address UTxOs and aggregate assets")
  .action(async () => {
    const mainEffect = provideTxServices(
      ReserveInspectionCommand.reserveUtxosProgram.pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });

program
  .command("payout-status")
  .description("Print payout accumulator status for a withdrawal event")
  .requiredOption(
    "--withdrawal-event-id <hex>",
    "Canonical OutputReference CBOR withdrawal event id",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = provideDatabaseTxServices(
      ReserveInspectionCommand.payoutStatusProgram(opts.withdrawalEventId).pipe(
        tapJson(),
      ),
    );

    runCliEffect(mainEffect);
  });

program
  .command("mpf-audit")
  .description(
    "Recompute the confirmed-ledger MPF root and halt commits on divergence",
  )
  .option(
    "--acknowledge-clean",
    "clear a sticky divergence only after this invocation completes a clean audit",
    false,
  )
  .action(async (opts: { acknowledgeClean: boolean }) => {
    const mainEffect = pipe(
      runMpfAudit({ acknowledgeClean: opts.acknowledgeClean }),
      Effect.tap((result) =>
        Effect.logInfo(`mpf-audit summary: ${JSON.stringify(result)}`),
      ),
      Effect.flatMap((result) =>
        result.diverged
          ? Effect.fail(
              new Error(
                `MPF audit divergence: persisted=${result.persistedRoot},recomputed=${result.recomputedRoot}`,
              ),
            )
          : Effect.succeed(result),
      ),
      Effect.provide(Services.Database.layer),
      Effect.provide(Services.NodeConfig.layer),
    );
    runCliEffect(mainEffect);
  });

program
  .command("mpf-replay")
  .description(
    "Replay a recorded MPF NDJSON corpus through legacy, overlay, and Architecture G using insert/fromlist fixtures",
  )
  .argument("<corpus-path>", "NDJSON corpus path")
  .action(async (corpusPath: string) => {
    runCliEffect(
      mpfReplayProgram(corpusPath).pipe(
        Effect.tap((summary) =>
          Effect.logInfo(`mpf-replay summary: ${JSON.stringify(summary)}`),
        ),
      ),
    );
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

    runCliEffect(mainEffect);
  });

program.parse(process.argv);
