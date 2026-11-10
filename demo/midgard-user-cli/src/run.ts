/**
 * `runUserCli(argv, deps)`: the whole `midgard-user` command, returning its
 * exit code. 0: confirmed (or `status` printed). 75 (EX_TEMPFAIL): the
 * deadline passed; rerun with the same --submission-id. 1: refused or failed.
 * `forced-order` and `plutus-tx` are operator rehearsal commands over the
 * wallet's L2 outputs; they keep no submission record.
 */
import {
  type UserSubmissionOutcome,
  UserSubmissionPending,
} from "@al-ft/midgard-sdk";
import { Command, CommanderError, Option } from "commander";

import { type DepositOptions, runDeposit } from "./deposit.js";
import { type UserCliDeps } from "./deps.js";
import {
  type ForcedOrderOptions,
  formatForcedOrder,
  runForcedOrder,
} from "./forced-order.js";
import { FORCED_ORDER_SHAPE_NAMES, FORCED_ORDER_SHAPES } from "./l2-shapes.js";
import {
  formatPlutusTx,
  type PlutusTxOptions,
  runPlutusTx,
} from "./plutus-tx.js";
import { formatStatusReport, runStatus, type StatusOptions } from "./status.js";
import { runWithdraw, type WithdrawOptions } from "./withdraw.js";

export const EXIT_CONFIRMED = 0;
export const EXIT_REFUSED = 1;
export const EXIT_PENDING = 75;

const collect = (value: string, previous: readonly string[]) => [
  ...previous,
  value,
];

const l1AccessOptions = (command: Command): Command =>
  command
    .addOption(
      new Option("--l1 <access>", "the L1 indexer (env L1_ACCESS)").choices([
        "kupmios",
        "blockfrost",
      ]),
    )
    .option("--kupo-url <url>", "Kupo (env L1_KUPO_URL)")
    .option("--ogmios-url <url>", "Ogmios (env L1_OGMIOS_URL)")
    .option("--blockfrost-url <url>", "Blockfrost (env L1_BLOCKFROST_URL)")
    .option(
      "--blockfrost-project-id <id>",
      "Blockfrost project id (env L1_BLOCKFROST_PROJECT_ID)",
    );

const manifestOption = (command: Command): Command =>
  command.option(
    "--manifest <path>",
    "deployment manifest (env MIDGARD_DEPLOYMENT_MANIFEST)",
  );

const seedOptions = (command: Command): Command =>
  command
    .option(
      "--wallet-seed-phrase-env <name>",
      "environment variable holding the wallet seed phrase",
    )
    .option(
      "--wallet-seed-phrase-file <path>",
      "absolute path of a mode-0600 file holding the wallet seed phrase",
    );

const commonOptions = (command: Command): Command =>
  l1AccessOptions(
    manifestOption(
      command.requiredOption(
        "--submission-id <id>",
        "names this request; rerun with it",
      ),
    ),
  )
    .option(
      "--record-dir <path>",
      "submission records (env MIDGARD_USER_RECORD_DIR, default ~/.midgard/user-submissions)",
    )
    .option("--json", "print the result as JSON");

const submitOptions = (command: Command): Command =>
  seedOptions(commonOptions(command)).option(
    "--timeout-ms <ms>",
    "stop after this long; a transaction in flight stays recorded",
  );

const l2Options = (command: Command): Command =>
  seedOptions(manifestOption(command))
    .requiredOption(
      "--node-endpoint <url>",
      "a Midgard node's public HTTP API serving this deployment",
    )
    .option("--json", "print the result as JSON");

const shapeHelp = FORCED_ORDER_SHAPE_NAMES.map(
  (name) =>
    `  ${name}: ${FORCED_ORDER_SHAPES[name].summary}${FORCED_ORDER_SHAPES[name].refusedAt === null ? "" : ` (refused at ${FORCED_ORDER_SHAPES[name].refusedAt})`}`,
).join("\n");

const printOutcome = (
  deps: UserCliDeps,
  submissionId: string,
  outcome: UserSubmissionOutcome,
  json: boolean | undefined,
) => {
  const result = {
    kind: outcome.kind,
    submissionId,
    eventId: outcome.eventId,
    admissionTxHash: outcome.admissionTxHash,
    ...(outcome.publicationTxHash === undefined
      ? {}
      : { publicationTxHash: outcome.publicationTxHash }),
    slot: outcome.slot,
    attempts: outcome.attempts,
  };
  deps.stdout(
    json === true
      ? `${JSON.stringify(result)}\n`
      : `${outcome.kind} confirmed: admission ${outcome.admissionTxHash}${outcome.publicationTxHash === undefined ? "" : ` (publication ${outcome.publicationTxHash})`}, event id ${outcome.eventId}, attempt ${outcome.attempts.toString()}\n`,
  );
};

const program = (deps: UserCliDeps, setExit: (code: number) => void) => {
  const root = new Command("midgard-user")
    .description(
      "Submit your own Midgard deposits and withdrawals through your own L1 indexer.",
    )
    .exitOverride()
    .configureOutput({
      writeOut: (text) => deps.stdout(text),
      writeErr: (text) => deps.stderr(text),
    });
  submitOptions(
    root
      .command("deposit")
      .description("Deposit L1 funds to an L2 address.")
      .exitOverride(),
  )
    .requiredOption("--l2-address <address>", "the L2 recipient")
    .requiredOption("--lovelace <n>", "lovelace to deposit")
    .option(
      "--asset <policyId.assetName:amount>",
      "an additional asset (repeatable)",
      collect,
      [],
    )
    .option("--l2-datum <cbor>", "Plutus data CBOR attached on L2")
    .option(
      "--structural-lovelace <n>",
      "ADA kept apart from the deposited value (quoted when omitted)",
    )
    .action(async (options: DepositOptions) => {
      printOutcome(
        deps,
        options.submissionId,
        await runDeposit(options, deps),
        options.json,
      );
      setExit(EXIT_CONFIRMED);
    });
  submitOptions(
    root
      .command("withdraw")
      .description("Withdraw one committed L2 output to L1.")
      .exitOverride(),
  )
    .requiredOption("--l2-out-ref <txHash#index>", "the L2 output to withdraw")
    .requiredOption("--l1-address <address>", "the L1 payout address")
    .option("--l1-datum <cbor>", "Plutus data CBOR for the L1 payout")
    .option(
      "--refund-address <address>",
      "where a refused withdrawal's ADA returns (default --l1-address)",
    )
    .option("--refund-datum <cbor>", "Plutus data CBOR for the refund")
    .option("--lovelace <n>", "order lovelace (quoted when omitted)")
    .requiredOption(
      "--node-endpoint <url>",
      "a Midgard node's public HTTP API serving this deployment",
    )
    .action(async (options: WithdrawOptions) => {
      printOutcome(
        deps,
        options.submissionId,
        await runWithdraw(options, deps),
        options.json,
      );
      setExit(EXIT_CONFIRMED);
    });
  l1AccessOptions(
    l2Options(
      root
        .command("forced-order")
        .description(
          "Mint a forced order carrying an L2 transaction of one shape (operator rehearsal).",
        )
        .exitOverride(),
    ),
  )
    .addOption(
      new Option("--shape <shape>", "the L2 transaction's shape")
        .choices(FORCED_ORDER_SHAPE_NAMES)
        .makeOptionMandatory(),
    )
    .addHelpText("after", `\nShapes:\n${shapeHelp}\n`)
    .action(async (options: ForcedOrderOptions) => {
      const result = await runForcedOrder(options, deps);
      deps.stdout(
        options.json === true
          ? `${JSON.stringify(result)}\n`
          : formatForcedOrder(result),
      );
      setExit(EXIT_CONFIRMED);
    });
  l2Options(
    root
      .command("plutus-tx")
      .description(
        "Submit an L2 transaction that runs a PlutusV3 mint (operator rehearsal).",
      )
      .exitOverride(),
  ).action(async (options: PlutusTxOptions) => {
    const result = await runPlutusTx(options, deps);
    deps.stdout(
      options.json === true
        ? `${JSON.stringify(result)}\n`
        : formatPlutusTx(result),
    );
    setExit(EXIT_CONFIRMED);
  });
  commonOptions(
    root
      .command("status")
      .description("Show a submission's record and its transactions' status.")
      .exitOverride(),
  )
    .addOption(
      new Option("--kind <kind>", "the submission kind")
        .choices(["deposit", "withdrawal"])
        .makeOptionMandatory(),
    )
    .action(async (options: StatusOptions) => {
      const report = await runStatus(options, deps);
      deps.stdout(
        options.json === true
          ? `${JSON.stringify(report)}\n`
          : formatStatusReport(report),
      );
      setExit(EXIT_CONFIRMED);
    });
  return root;
};

const message = (cause: unknown) =>
  cause instanceof Error ? cause.message : String(cause);

export const runUserCli = async (
  argv: readonly string[],
  deps: UserCliDeps,
): Promise<number> => {
  let exitCode = EXIT_REFUSED;
  try {
    await program(deps, (code) => {
      exitCode = code;
    }).parseAsync([...argv], { from: "user" });
    return exitCode;
  } catch (cause) {
    if (cause instanceof CommanderError)
      // Help and version exit 0; usage errors were already printed.
      return cause.exitCode === 0 ? EXIT_CONFIRMED : EXIT_REFUSED;
    if (cause instanceof UserSubmissionPending) {
      deps.stderr(
        `${cause.message}\n${cause.attempt === undefined ? "" : `in flight: ${cause.attempt.phase.toLowerCase()} ${cause.attempt.txHash}\n`}pending; rerun with the same --submission-id\n`,
      );
      return EXIT_PENDING;
    }
    deps.stderr(`midgard-user: ${message(cause)}\n`);
    return EXIT_REFUSED;
  }
};
