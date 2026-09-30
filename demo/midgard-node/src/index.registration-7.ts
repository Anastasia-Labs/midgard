import "./index.registration-6.js";

import { assertReferenceScriptAuthMinimumRemaining } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  provideLucidOnlyServices,
  provideReferenceScriptDeploymentServices,
  provideTxServices,
  runCliEffect,
  tapJson,
  writeJson,
} from "./commands/cli-runtime.js";
import { formatJson } from "./commands/command-utils.js";
import * as ContractDeploymentInfo from "./commands/contract-deployment-info.js";
import * as DeploymentRunStateCommand from "./commands/deployment-run-state.js";
import { program } from "./index.registration.js";
import * as Services from "./services/index.js";
import {
  planReferenceScriptCommandProgram,
  referenceScriptTargetsByCommand,
  referenceScriptWalletStatusProgram,
} from "./transactions/reference-scripts.js";
import * as RegisterActiveOperator from "./transactions/register-active-operator.js";

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
                eventHistoryProtectionDurationMs:
                  Services.eventHistoryProtectionDurationFromExplicitEnvironment(),
                eventHistoryBounds:
                  Services.eventHistoryBoundsFromExplicitEnvironment(),
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
