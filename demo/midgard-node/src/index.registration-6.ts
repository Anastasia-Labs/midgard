import "./index.registration-5.js";

import { Effect } from "effect";

import {
  failCli,
  provideLucidOnlyServices,
  provideTxServices,
  runCliEffect,
  writeJson,
} from "./commands/cli-runtime.js";
import { formatJson } from "./commands/command-utils.js";
import * as DeploymentRunStateCommand from "./commands/deployment-run-state.js";
import * as PrepareHubOracleNonce from "./commands/prepare-hub-oracle-nonce.js";
import { hubOracleNonceRunStateHooks } from "./commands/prepare-hub-oracle-nonce.run-state-hooks.js";
import { program } from "./index.registration.js";
import * as Services from "./services/index.js";
import * as PhasMembershipRegistration from "./transactions/phas-membership-registration.js";

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
                onTxHashConfirmed: hubOracleNonceRunStateHooks(
                  runOptions,
                  nodeConfig.NETWORK,
                ).onTxHashConfirmed,
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
            hubOracleNonceRunStateHooks(runOptions, nodeConfig.NETWORK),
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
