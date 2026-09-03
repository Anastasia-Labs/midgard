#!/usr/bin/env node

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import { assertReferenceScriptAuthMinimumRemaining } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Command } from "commander";
import { Effect, pipe } from "effect";

import packageJson from "../package.json" with { type: "json" };
import * as AddressFromSeed from "./commands/address-from-seed.js";
import { auditBlocksImmutableProgram } from "./commands/audit-blocks-immutable.js";
import {
  assertUserCliWalletIsOperationallyIsolated,
  collectStringOption,
  errorMessage,
  failCli,
  parseE2EEnvInheritanceOption,
  parseL1AddressOption,
  parseNonNegativeIntegerOption,
  parsePositiveBigIntOption,
  parsePositiveIntegerOption,
  parseStringListOption,
  provideDatabaseServices,
  provideDatabaseTxServices,
  provideLucidOnlyServices,
  provideNodeRuntimeServices,
  provideReferenceScriptDeploymentServices,
  provideTxServices,
  runCliEffect,
  tapJson,
  writeJson,
} from "./commands/cli-runtime.js";
import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  formatJson,
  parseAddressArgument,
  parseEventId,
  parseHexBytes,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "./commands/command-utils.js";
import * as ContractDeploymentInfo from "./commands/contract-deployment-info.js";
import * as DeploymentRunStateCommand from "./commands/deployment-run-state.js";
import * as EventSettlementProofCommand from "./commands/event-settlement-proof.js";
import * as FetchWithdrawalsOnceCommand from "./commands/fetch-withdrawals-once.js";
import * as L1ProviderPreflightCommand from "./commands/l1-provider-preflight.js";
import * as L1UtxosCommand from "./commands/l1-utxos.js";
import { runNode } from "./commands/listen.js";
import { runMpfAudit } from "./commands/mpf-audit.js";
import { mpfReplayProgram } from "./commands/mpf-replay.js";
import * as PrepareHubOracleNonce from "./commands/prepare-hub-oracle-nonce.js";
import * as ReconcileCommand from "./commands/reconcile.js";
import * as ReserveInspectionCommand from "./commands/reserve-inspection.js";
import * as ReservePayoutCommand from "./commands/reserve-payout.js";
import * as RetentionCheck from "./commands/retention-check.js";
import * as SubmitL2Transfer from "./commands/submit-l2-transfer.js";
import * as SubmitWithdrawalCommand from "./commands/submit-withdrawal.js";
import * as UtxosCommand from "./commands/utxos.js";
import * as WithdrawalStatusCommand from "./commands/withdrawal-status.js";
import {
  type DaLibp2pPreflightMode,
  runDaLibp2pPreflightFromEnv,
} from "./da/libp2p-producer.js";
import {
  DA_LIBP2P_RUNTIME_PROFILES,
  type DaLibp2pRuntimeManifestOptions,
  type DaLibp2pRuntimeManifestTarget,
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "./da/libp2p-runtime-manifest.js";
import * as MigrationRunner from "./database/migrations/runner.js";
import { buildE2EProcessEnv, parseEnvOverrides } from "./e2e/env.js";
import {
  fetchAndInsertDepositUTxOs,
  projectDepositsToMempoolLedger,
} from "./fibers/index.js";
import { loadRuntimeDotenv } from "./runtime-env.js";
import * as Services from "./services/index.js";
import * as DaAttestation from "./transactions/da-attestation.js";
import * as Initialization from "./transactions/initialization.js";
import * as PhasMembershipRegistration from "./transactions/phas-membership-registration.js";
import {
  fetchReferenceScriptUtxosProgram,
  planReferenceScriptCommandProgram,
  REFERENCE_SCRIPT_SWEEP_DEFAULT_MAX_ASSETS_PER_TOKEN_OUTPUT,
  REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
  referenceScriptWalletStatusProgram,
  sweepReferenceScriptWalletProgram,
} from "./transactions/reference-scripts.js";
import * as RegisterActiveOperator from "./transactions/register-active-operator.js";
import * as SubmitDeposit from "./transactions/submit-deposit.js";
import { chalk, ENV_VARS_GUIDE } from "./utils.js";
import { commitExplicitBlockHeaderProgram } from "./workers/commit-block-header.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "./workers/commit-block-header/da-payload-backfill.js";

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
  .command("retention-check")
  .description(
    "Check retained DA payload retention deadlines; exits nonzero when any still-challengeable record is inside its alert threshold",
  )
  .option(
    "--alert-threshold-ms <ms>",
    "Alert headroom in milliseconds (defaults to the derived canonical V1 retention margin)",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(async (options: { readonly alertThresholdMs?: string }) => {
    let alertThresholdMs: number | undefined;
    try {
      alertThresholdMs =
        options.alertThresholdMs === undefined
          ? undefined
          : parseNonNegativeIntegerOption(
              options.alertThresholdMs,
              "--alert-threshold-ms",
            );
    } catch (error) {
      failCli("reconcile retention-check", error);
      return;
    }
    const mainEffect = provideDatabaseServices(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // A DA payload whose header no longer sits in the local state-queue
        // mirror has reached a terminal L1 outcome; anything still queued is
        // attested-but-not-terminal. Unknown rows fail closed inside the
        // evaluator.
        const rows = yield* sql<{
          readonly header_hash: Buffer;
          readonly block_end_time: Date | null;
          readonly still_queued: boolean;
        }>`
          SELECT payload.header_hash,
                 payload.block_end_time,
                 EXISTS (
                   SELECT 1 FROM blocks
                   WHERE blocks.header_hash = payload.header_hash
                 ) AS still_queued
          FROM da_payloads payload`;
        const result = RetentionCheck.evaluateRetentionCheck({
          nowMillis: Date.now(),
          alertThresholdMs,
          records: rows.map((row) => ({
            headerHash: row.header_hash.toString("hex"),
            blockEndTimeMs: row.block_end_time?.getTime() ?? null,
            headerStatus: row.still_queued ? "attested" : "merged",
          })),
        });
        return result;
      }).pipe(tapJson()),
    ).pipe(
      Effect.tap((result) =>
        Effect.sync(() => {
          process.exitCode = RetentionCheck.retentionCheckExitCode(result);
        }),
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
                availabilityChallengeParameters:
                  Services.availabilityParametersFromExplicitEnvironment(),
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

    const mainEffect = provideDatabaseTxServices(
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
