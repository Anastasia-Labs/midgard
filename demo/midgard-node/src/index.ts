#!/usr/bin/env node

import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import {
  assertReferenceScriptAuthMinimumRemaining,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { NodeRuntime } from "@effect/platform-node";
import { getAddressDetails, type Network } from "@lucid-evolution/lucid";
import { Command } from "commander";
import dotenv from "dotenv";
import { Effect, pipe } from "effect";

import * as AddressFromSeed from "@/commands/address-from-seed.js";
import { auditBlocksImmutableProgram } from "@/commands/audit-blocks-immutable.js";
import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
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
import * as E2EServiceCommand from "@/commands/e2e-service.js";
import * as EventSettlementProofCommand from "@/commands/event-settlement-proof.js";
import * as FetchWithdrawalsOnceCommand from "@/commands/fetch-withdrawals-once.js";
import * as L1ProviderPreflightCommand from "@/commands/l1-provider-preflight.js";
import * as L1UtxosCommand from "@/commands/l1-utxos.js";
import { runNode } from "@/commands/listen.js";
import * as PrepareHubOracleNonce from "@/commands/prepare-hub-oracle-nonce.js";
import * as ReconcileCommand from "@/commands/reconcile.js";
import * as ReserveInspectionCommand from "@/commands/reserve-inspection.js";
import * as ReservePayoutCommand from "@/commands/reserve-payout.js";
import * as SubmitL2Transfer from "@/commands/submit-l2-transfer.js";
import * as SubmitWithdrawalCommand from "@/commands/submit-withdrawal.js";
import * as UtxosCommand from "@/commands/utxos.js";
import * as WithdrawalStatusCommand from "@/commands/withdrawal-status.js";
import * as MigrationRunner from "@/database/migrations/runner.js";
import { runCommandStep } from "@/e2e/runner.js";
import {
  fetchAndInsertDepositUTxOs,
  projectDepositsToMempoolLedger,
} from "@/fibers/index.js";
import { runProviderStepWithRetry } from "@/provider-retry.js";
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

dotenv.config();
const VERSION = packageJson.version;

const REFERENCE_SCRIPT_MANIFEST_FETCH_RETRY = {
  maxAttempts: 8,
  baseDelayMs: 750,
  maxDelayMs: 8_000,
  jitterRatio: 0.25,
} as const;

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

const parseTxEvidenceOption = (
  value: string,
): NonNullable<
  E2EFinalizeSummaryCommand.FinalizeSummaryOptions["transactions"]
>[number] => {
  const [label, txHash, status, ...sourceParts] = value.split(":");
  if (
    label === undefined ||
    label.length === 0 ||
    txHash === undefined ||
    txHash.length === 0 ||
    status === undefined ||
    !E2E_TX_STATUSES.has(status as E2ETxStatus) ||
    sourceParts.length === 0
  ) {
    throw new Error(
      "--tx must use label:txHash:status:source with status one of submitted, confirmed, queued, accepted, committed, rejected, unknown",
    );
  }
  return {
    label,
    txHash,
    status: status as E2ETxStatus,
    source: sourceParts.join(":"),
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

const provideDatabaseTxServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Database
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
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContracts.Default),
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
      console.error(`l1-utxos: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }

    try {
      const result = await L1UtxosCommand.fetchKupmiosAddressUtxos({
        address,
        ...kupmiosConfig,
      });
      process.stdout.write(`${formatJson(result)}\n`);
    } catch (error) {
      console.error(`l1-utxos: ${errorMessage(error)}`);
      process.exitCode = 1;
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
        process.stdout.write(`${formatJson(report)}\n`);
      });
      if (!report.ok) {
        return yield* Effect.fail(
          new Error("No configured L1 provider source passed preflight"),
        );
      }
      return report;
    }).pipe(Effect.provide(Services.NodeConfig.layer));

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`address-from-seed: ${errorMessage(error)}`);
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("db:checksum")
  .description("Print the compiled schema migration manifest checksums")
  .action(async () => {
    process.stdout.write(`${MigrationRunner.formatChecksum()}\n`);
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
      console.error(`db:backfill-da-payloads: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideDatabaseServices(
      backfillMissingDaPayloadsFromFinalizedJournals({
        headerHash:
          headerHash === undefined ? undefined : Buffer.from(headerHash, "hex"),
        limit,
      }).pipe(
        Effect.tap((summary) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(summary)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
        console.error(
          "reconcile reference-scripts-complete: only --scope node-runtime is supported",
        );
        process.exitCode = 1;
        return;
      }
      const mainEffect = provideTxServices(
        ReconcileCommand.reconcileReferenceScriptsCompleteProgram({
          repair: options.repair === true,
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
        console.error(`reconcile deployment-manifest: ${errorMessage(error)}`);
        process.exitCode = 1;
        return;
      }

      const mainEffect = provideTxServices(
        ContractDeploymentInfo.reconcileInitializedDeploymentManifestProgram({
          outputPath: options.out,
          initTxHash,
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
        console.error(`reconcile deposit-projected: ${errorMessage(error)}`);
        process.exitCode = 1;
        return;
      }

      const mainEffect = provideNodeRuntimeServices(
        ReconcileCommand.reconcileDepositProjectedProgram({
          eventId,
          cardanoTxHash,
          repair: options.repair === true,
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`reconcile tx-committed: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }
    const mainEffect = provideDatabaseServices(
      ReconcileCommand.reconcileTxCommittedProgram({ txHash }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

reconcile
  .command("da-attested")
  .description("Reconcile DA payload and copied watcher attestation status")
  .requiredOption("--header-hash <hex>", "28-byte block header hash")
  .option("--watcher-url <url>", "Copied DA node base URL")
  .option(
    "--deployment-fingerprint <fingerprint>",
    "Copied DA deployment fingerprint",
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
      readonly deploymentFingerprint?: string;
      readonly repair?: boolean;
    }) => {
      let headerHash: Buffer;
      try {
        headerHash = parseHexBytes(options.headerHash, "headerHash", 28);
      } catch (error) {
        console.error(`reconcile da-attested: ${errorMessage(error)}`);
        process.exitCode = 1;
        return;
      }
      const mainEffect = provideDatabaseServices(
        ReconcileCommand.reconcileDaAttestedProgram({
          headerHash,
          watcherUrl: options.watcherUrl,
          deploymentFingerprint: options.deploymentFingerprint,
          repair: options.repair === true,
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`reconcile block-committed: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }
    const mainEffect = provideDatabaseTxServices(
      ReconcileCommand.reconcileBlockCommittedProgram({ headerHash }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

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
        console.error(`reconcile merge-complete: ${errorMessage(error)}`);
        process.exitCode = 1;
        return;
      }
      const mainEffect = provideNodeRuntimeServices(
        ReconcileCommand.reconcileMergeCompleteProgram({
          headerHash,
          repair: options.repair === true,
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("e2e-run-step")
  .description("Run one acceptance command through the structured e2e runner")
  .requiredOption("--id <id>", "Step id")
  .requiredOption("--cwd <path>", "Working directory")
  .requiredOption("--raw-log <path>", "Raw log path")
  .option("--summary-out <path>", "Write the step summary JSON to this path")
  .option("--timeout-ms <ms>", "Step timeout in milliseconds")
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
      process.stdout.write(`${formatJson(summary)}\n`);
      if (summary.status !== "success") {
        process.exitCode = 1;
      }
    } catch (error) {
      console.error(`e2e-run-step: ${errorMessage(error)}`);
      process.exitCode = 1;
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
  .argument("<command>", "Command to execute")
  .argument("[args...]", "Command arguments")
  .action(async (command, args, opts) => {
    try {
      const summary = await E2EServiceCommand.startManagedService({
        service: opts.service,
        command,
        args,
        cwd: opts.cwd,
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
      process.stdout.write(`${formatJson(summary)}\n`);
    } catch (error) {
      console.error(`e2e-start-service: ${errorMessage(error)}`);
      process.exitCode = 1;
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
      console.error(
        `prepare-hub-oracle-one-shot-nonce: ${errorMessage(error)}`,
      );
      process.exitCode = 1;
      return;
    }

    try {
      if (!opts.dryRun) {
        await DeploymentRunStateCommand.guardHubOracleNonceCreation({
          options: runOptions,
        });
      }
    } catch (error) {
      console.error(
        `prepare-hub-oracle-one-shot-nonce: ${errorMessage(error)}`,
      );
      process.exitCode = 1;
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
                process.stdout.write(`${formatJson(result)}\n`);
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
      NodeRuntime.runMain(mainEffect, { teardown: undefined });
      return;
    }

    const mainEffect = provideLucidOnlyServices(
      Effect.gen(function* () {
        const nodeConfig = yield* Services.NodeConfig;
        const result =
          yield* PrepareHubOracleNonce.prepareHubOracleOneShotNonceProgram(
            amountLovelace,
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
              process.stdout.write(`${formatJson(result)}\n`);
              return;
            }
            process.stdout.write(
              [
                `prepared hub-oracle one-shot nonce: ${result.outRef}`,
                `HUB_ORACLE_ONE_SHOT_TX_HASH=${result.txHash}`,
                `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX=${result.outputIndex.toString()}`,
                `address=${result.address}`,
                `lovelace=${result.lovelace}`,
              ].join("\n") + "\n",
            );
          }),
        ),
      ),
    );
    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
            process.stdout.write(`${formatJson(plan)}\n`);
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
          const liveReferenceScriptUtxos = yield* runProviderStepWithRetry(
            `deploy-reference-script-${commandName} final reference-script UTxO fetch`,
            Effect.tryPromise({
              try: () =>
                lucidService.referenceScriptsApi.utxosAt(
                  lucidService.referenceScriptsAddress,
                ),
              catch: (cause) =>
                new Error(
                  `Failed to fetch published reference-script UTxOs at ${lucidService.referenceScriptsAddress}: ${formatUnknownError(
                    cause,
                  )}`,
                ),
            }),
            REFERENCE_SCRIPT_MANIFEST_FETCH_RETRY,
          );
          const deploymentInfo =
            yield* ContractDeploymentInfo.buildContractDeploymentInfoProgram(
              contracts,
              [
                ...liveReferenceScriptUtxos,
                ...published.map(({ utxo }) => utxo),
              ],
              referenceScriptAuthPolicyDeploymentInfo(authPolicy),
            );
          const manifestPath =
            yield* ContractDeploymentInfo.writeContractDeploymentInfoFileProgram(
              manifestOutputPath,
              ContractDeploymentInfo.buildDeploymentManifestV2(deploymentInfo, {
                network: nodeConfig.NETWORK,
                referenceScriptDeployAddress:
                  nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
                hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
                hubOracleOneShotOutputIndex:
                  nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
              }),
            );
          yield* Effect.logInfo(
            `reference-script deployment info written: ${manifestPath}`,
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

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      endTimeMs: parseOptionalEndTimeMs(opts.endTimeMs),
      awaitConfirmation: opts.awaitConfirmation !== false,
    };
    const mainEffect = provideTxServices(
      commitExplicitBlockHeaderProgram(params).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`attest-state-queue-once: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideTxServices(
      DaAttestation.attestStateQueueOnceProgram({ headerHash }).pipe(
        Effect.tap((results) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(results)}\n`);
          }),
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
        console.error(`submit-deposit: ${errorMessage(error)}`);
        process.exitCode = 1;
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
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
          Effect.tap((result) =>
            Effect.logInfo(`submit-deposit completed: txHash=${result.txHash}`),
          ),
        ),
      );

      NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
        console.error(
          `reconcile-deposit-submission: invalid --tx-hash: ${errorMessage(error)}`,
        );
        process.exitCode = 1;
        return;
      }

      const mainEffect = provideDatabaseTxServices(
        SubmitDeposit.reconcileDepositSubmissionAttemptProgram(txHash).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
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
        console.error(`submit-l2-transfer: ${errorMessage(error)}`);
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
                referenceScriptsAddress:
                  lucidService.referenceScriptsWalletAddress,
              }),
          });
          return result;
        }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              process.stdout.write(`${formatJson(result)}\n`);
            }),
          ),
          Effect.tapError((error) =>
            Effect.logError(
              `submit-l2-transfer failed: ${errorMessage(error)}`,
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`utxos: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideDatabaseServices(
      UtxosCommand.utxosProgram(address).pipe(
        Effect.flatMap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
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
  .command("fetch-withdrawals-once")
  .description(
    "Fetch visible withdrawal order UTxOs from L1 once and reconcile them into withdrawal_utxos",
  )
  .action(async () => {
    const mainEffect = provideNodeRuntimeServices(
      FetchWithdrawalsOnceCommand.fetchWithdrawalsOnceProgram.pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`resolve-event-settlement-proof: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideDatabaseTxServices(
      EventSettlementProofCommand.resolveEventSettlementProofProgram(
        lookup,
      ).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(
              `${formatJson(EventSettlementProofCommand.serializeEventSettlementProofResolution(result))}\n`,
            );
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      }).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
      console.error(`withdrawal-status: ${errorMessage(error)}`);
      process.exitCode = 1;
      return;
    }

    const mainEffect = provideDatabaseTxServices(
      WithdrawalStatusCommand.withdrawalStatusProgram(lookup).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
  });

program
  .command("reserve-utxos")
  .description("Print typed reserve-address UTxOs and aggregate assets")
  .action(async () => {
    const mainEffect = provideTxServices(
      ReserveInspectionCommand.reserveUtxosProgram.pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
          }),
        ),
      ),
    );

    NodeRuntime.runMain(mainEffect, { teardown: undefined });
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
        Effect.tap((result) =>
          Effect.sync(() => {
            process.stdout.write(`${formatJson(result)}\n`);
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
