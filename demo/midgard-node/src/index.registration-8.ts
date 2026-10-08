import "./index.registration-7.js";

import { Effect, pipe } from "effect";

import {
  failCli,
  parseL1AddressOption,
  parsePositiveIntegerOption,
  provideTxServices,
  runCliEffect,
  tapJson,
} from "./commands/cli-runtime.js";
import * as ContractDeploymentInfo from "./commands/contract-deployment-info.js";
import { program } from "./index.registration.js";
import * as Services from "./services/index.js";
import {
  type IntentJournal,
  IntentJournalWithoutFollower,
} from "./services/intent-journal.js";
import * as OperatorCommands from "./transactions/operators/commands.js";
import {
  liveReferenceScriptDeployment,
  referenceScriptSweepLimitsFromProtocolParameters,
  sweepRetiredReferenceScriptsProgram,
} from "./transactions/reference-script-sweep.js";
import * as RegisterActiveOperator from "./transactions/register-active-operator.js";

program
  .command("sweep-reference-script-wallet")
  .description(
    "Reclaim the ADA in reference-script UTxOs of one retired auth policy, in confirmed batches; burns the retired tokens while the policy is satisfiable, otherwise quarantines them",
  )
  .requiredOption(
    "--retired-auth-policy <policyId>",
    "Reference-script auth policy of the retired deployment; only UTxOs carrying a reference script and a token of this policy are spent",
  )
  .option(
    "--retired-auth-policy-script <cborHex>",
    "Native script CBOR of the retired policy; lets the sweep burn its tokens while the policy is still satisfiable",
  )
  .option(
    "--quarantine-address <address>",
    "Receives the retired tokens with minimum ADA when they cannot be burned (default: the reference-script wallet address)",
  )
  .option(
    "--max-reference-script-bytes-per-batch <bytes>",
    "Lower the per-transaction reference-script byte budget (default: 90% of the protocol maximum)",
    (value) =>
      parsePositiveIntegerOption(
        value,
        "--max-reference-script-bytes-per-batch",
      ),
  )
  .option(
    "--execute",
    "Submit the batches. Without this flag the command only prints the plan.",
  )
  .option(
    "--i-am-retiring-reference-scripts",
    "Required with --execute; confirms the retired policy's reference scripts are no longer live.",
  )
  .action(async (_args, options) => {
    const opts = options.opts() as {
      readonly retiredAuthPolicy: string;
      readonly retiredAuthPolicyScript?: string;
      readonly quarantineAddress?: string;
      readonly maxReferenceScriptBytesPerBatch?: number;
      readonly execute?: boolean;
      readonly iAmRetiringReferenceScripts?: boolean;
    };
    const mainEffect = provideTxServices(
      Effect.gen(function* () {
        const nodeConfig = yield* Services.NodeConfig;
        const contracts = yield* Services.MidgardContracts;
        const lucidService = yield* Services.Lucid;
        yield* lucidService.switchToReferenceScriptWallet;
        const quarantineAddress = yield* Effect.try({
          try: () =>
            opts.quarantineAddress === undefined
              ? undefined
              : parseL1AddressOption(
                  opts.quarantineAddress,
                  "--quarantine-address",
                  nodeConfig.NETWORK,
                ),
          catch: (cause) =>
            cause instanceof Error
              ? cause
              : new Error(
                  `Failed to parse --quarantine-address: ${String(cause)}`,
                ),
        });
        const limits = yield* Effect.tryPromise({
          try: async () => {
            const { snapshot } =
              await ContractDeploymentInfo.cardanoProtocolParametersIdentityFromLedger(
                ContractDeploymentInfo.ledgerProtocolParametersReader(
                  lucidService,
                ),
              );
            return referenceScriptSweepLimitsFromProtocolParameters(snapshot);
          },
          catch: (cause) =>
            new Error(
              `Failed to read current protocol parameters: ${String(cause)}`,
            ),
        });
        return yield* sweepRetiredReferenceScriptsProgram({
          lucid: lucidService.referenceScriptsApi,
          referenceScriptsAddress: lucidService.referenceScriptsAddress,
          live: liveReferenceScriptDeployment(contracts),
          limits,
          options: {
            retiredAuthPolicyId: opts.retiredAuthPolicy,
            ...(opts.retiredAuthPolicyScript === undefined
              ? {}
              : {
                  retiredAuthPolicyScript: {
                    type: "Native" as const,
                    script: opts.retiredAuthPolicyScript.trim().toLowerCase(),
                  },
                }),
            ...(quarantineAddress === undefined ? {} : { quarantineAddress }),
            ...(opts.maxReferenceScriptBytesPerBatch === undefined
              ? {}
              : {
                  maxReferenceScriptBytesPerBatch:
                    opts.maxReferenceScriptBytesPerBatch,
                }),
            execute: opts.execute === true,
            acknowledgeRetirement: opts.iAmRetiringReferenceScripts === true,
          },
        });
      }).pipe(tapJson()),
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
      Effect.provide(IntentJournalWithoutFollower),
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

/**
 * Runs an operator lifecycle verb: prints the JSON result on success, and on a
 * local refusal or funding shortfall prints the one-line reason and exits
 * non-zero without a fiber trace.
 */
export const runOperatorCommand = <A, E>(
  label: string,
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.MidgardContracts
    | Services.Lucid
    | IntentJournal
  >,
): void => {
  runCliEffect(
    pipe(
      effect,
      tapJson((result) => ({ command: label, ...(result as object) })),
      Effect.asVoid,
      Effect.catchAll((error) =>
        OperatorCommands.isOperatorCommandRefusal(error)
          ? Effect.sync(() => failCli(label, error))
          : Effect.fail(error),
      ),
      provideTxServices,
    ),
  );
};

program
  .command("register-operator")
  .description(
    "Lock the operator bond in a new registered-operators node for the operator wallet (refused locally if the key is already registered, active, or retired)",
  )
  .action(async () => {
    runOperatorCommand(
      "register-operator",
      OperatorCommands.registerOperatorCommand,
    );
  });

program
  .command("activate-operator")
  .description(
    "Move a registered operator into the active set once its activation time has passed; defaults to the operator wallet's own key",
  )
  .option(
    "--operator-key-hash <hex>",
    "Payment key hash of the registered operator to activate",
    OperatorCommands.parseOperatorKeyHashOption,
  )
  .action(async (options: { operatorKeyHash?: string }) => {
    runOperatorCommand(
      "activate-operator",
      OperatorCommands.activateOperatorCommand({
        operatorKeyHash: options.operatorKeyHash,
      }),
    );
  });

program
  .command("operator-status")
  .description(
    "Print the operator directory status (state, bond, strikes, scheduler shift, inactivity threshold, local watchdog) as JSON; defaults to the operator wallet's own key",
  )
  .option(
    "--operator-key-hash <hex>",
    "Payment key hash of the operator to inspect",
    OperatorCommands.parseOperatorKeyHashOption,
  )
  .action(async (options: { operatorKeyHash?: string }) => {
    runCliEffect(
      pipe(
        OperatorCommands.operatorStatusCommand({
          operatorKeyHash: options.operatorKeyHash,
        }),
        tapJson(),
        Effect.asVoid,
        provideTxServices,
      ),
    );
  });

program
  .command("retire-operator")
  .alias("deactivate-operator")
  .description(
    "Voluntarily retire the operator wallet: move its active node to the retired list with the full bond, advancing or rewinding the scheduler if it holds the shift",
  )
  .action(async () => {
    runOperatorCommand(
      "retire-operator",
      OperatorCommands.retireOperatorCommand,
    );
  });

program
  .command("recover-operator-bond")
  .alias("unlock-operator")
  .description(
    "Burn the operator wallet's retired node and return the bond to the wallet; refused before the bond unlock time",
  )
  .action(async () => {
    runOperatorCommand(
      "recover-operator-bond",
      OperatorCommands.recoverOperatorBondCommand,
    );
  });

program
  .command("deregister-operator")
  .description(
    "Remove the operator wallet's registered node before activation and return the bond",
  )
  .action(async () => {
    runOperatorCommand(
      "deregister-operator",
      OperatorCommands.deregisterOperatorCommand,
    );
  });

program
  .command("strike-inactive-operator")
  .description(
    "Strike the scheduled operator for a missed shift and hand the shift to the next operator; refused before the inactivity threshold",
  )
  .action(async () => {
    runOperatorCommand(
      "strike-inactive-operator",
      OperatorCommands.strikeInactiveOperatorCommand,
    );
  });

program
  .command("force-retire-operator")
  .description(
    "Retire an operator that has reached the maximum inactivity strikes; the inactivity penalty is paid from its bond as the fee",
  )
  .requiredOption(
    "--operator-key-hash <hex>",
    "Payment key hash of the operator to force-retire",
    OperatorCommands.parseOperatorKeyHashOption,
  )
  .action(async (options: { operatorKeyHash: string }) => {
    runOperatorCommand(
      "force-retire-operator",
      OperatorCommands.forceRetireOperatorCommand({
        operatorKeyHash: options.operatorKeyHash,
      }),
    );
  });
