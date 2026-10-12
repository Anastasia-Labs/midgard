import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import {
  failCli,
  provideDatabaseTxServices,
  provideNodeRuntimeServices,
  provideTxServices,
  runCliEffect,
  tapJson,
} from "./commands/cli-runtime.js";
import { formatJson, parseHexBytes } from "./commands/command-utils.js";
import * as ContractDeploymentInfo from "./commands/contract-deployment-info.js";
import * as ReconcileCommand from "./commands/reconcile.js";
import * as RetentionCheck from "./commands/retention-check.js";
import { program } from "./index.registration.js";
import { reconcile } from "./index.registration-3.js";
import * as Services from "./services/index.js";
import * as Initialization from "./transactions/initialization.js";

reconcile
  .command("retention-check")
  .description(
    "Check retained DA payload retention deadlines; with --alert-threshold-ms, lists still-challengeable records inside that threshold and exits nonzero when any is listed (every merged payload passes through it on its way to pruning, so this is information, not a health gate)",
  )
  .option(
    "--alert-threshold-ms <ms>",
    "Alert when a still-challengeable record has at most this many milliseconds left; must be below the merged-payload window, the challengeability horizon minus block maturity (no default: without it no deadline alert is raised)",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(async (options: { readonly alertThresholdMs?: string }) => {
    let alertThresholdMs: number | undefined;
    try {
      alertThresholdMs = RetentionCheck.parseRetentionAlertThresholdOption(
        options.alertThresholdMs,
      );
    } catch (error) {
      failCli("reconcile retention-check", error);
      return;
    }
    const mainEffect = provideDatabaseTxServices(
      RetentionCheck.retentionCheckProgram(alertThresholdMs).pipe(tapJson()),
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
  .option(
    "--repair",
    "Merge the header if it is the oldest queued block; refused outside the running node's follower write gate (use its admin GET /merge)",
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
