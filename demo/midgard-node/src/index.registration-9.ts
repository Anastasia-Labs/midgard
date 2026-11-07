import {
  failCli,
  provideDatabaseTxServices,
  runCliEffect,
  tapJson,
} from "./commands/cli-runtime.js";
import {
  parseMerkleRootOption,
  parseOptionalEndTimeMs,
  parseOptionalHeaderHashOption,
  program,
} from "./index.registration.js";
import { runOperatorCommand } from "./index.registration-8.js";
import * as DaAttestation from "./transactions/da-attestation.js";
import * as OperatorCommands from "./transactions/operators/commands.js";
import { commitExplicitBlockHeaderProgram } from "./workers/commit-block-header.js";

program
  .command("slash-duplicate-operator")
  .description(
    "Remove a duplicate registered node for an operator that is already registered, active, or retired; the slashing penalty is paid from its bond as the fee and the bounty goes to the enterprise address of this node's wallet key",
  )
  .requiredOption(
    "--operator-key-hash <hex>",
    "Payment key hash of the duplicated operator",
    OperatorCommands.parseOperatorKeyHashOption,
  )
  .action(async (options: { operatorKeyHash: string }) => {
    runOperatorCommand(
      "slash-duplicate-operator",
      OperatorCommands.slashDuplicateOperatorCommand({
        operatorKeyHash: options.operatorKeyHash,
      }),
    );
  });

program
  .command("commit-explicit-block-header")
  .description(
    "Commit a state_queue block header with caller-supplied roots using the live operator path; the block must carry at least one event",
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
    "--deposit-count <n>",
    "Deposit count to commit in the header (must be > 0 for a non-empty deposits root)",
  )
  .option(
    "--withdrawal-count <n>",
    "Withdrawal count to commit in the header (must be > 0 for a non-empty withdrawals root)",
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
      depositCount:
        opts.depositCount === undefined ? undefined : BigInt(opts.depositCount),
      withdrawalCount:
        opts.withdrawalCount === undefined
          ? undefined
          : BigInt(opts.withdrawalCount),
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
    const mainEffect = provideDatabaseTxServices(
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
