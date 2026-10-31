import "./index.registration-10.js";

import { Effect, pipe } from "effect";

import { auditBlocksImmutableProgram } from "./commands/audit-blocks-immutable.js";
import {
  failCli,
  provideDatabaseTxServices,
  provideTxServices,
  runCliEffect,
  tapJson,
  writeJson,
} from "./commands/cli-runtime.js";
import { runMpfAudit } from "./commands/mpf-audit.js";
import { mpfReplayProgram } from "./commands/mpf-replay.js";
import * as ReserveInspectionCommand from "./commands/reserve-inspection.js";
import * as ReservePayoutCommand from "./commands/reserve-payout.js";
import * as StateReconciliation from "./commands/state-reconciliation.js";
import * as WithdrawalStatusCommand from "./commands/withdrawal-status.js";
import { program } from "./index.registration.js";
import * as Services from "./services/index.js";

program
  .command("add-reserve-funds-to-payout")
  .description("Move reserve funds into an initialized payout accumulator")
  .requiredOption(
    "--withdrawal-event-id <hex>",
    "Canonical OutputReference CBOR withdrawal event id",
  )
  .option(
    "--reserve-out-ref <txHash#outputIndex>",
    "Reserve UTxO to spend instead of the automatic selection",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mainEffect = provideDatabaseTxServices(
      ReservePayoutCommand.addReserveFundsToPayoutProgram({
        eventId: opts.withdrawalEventId,
        reserveOutRef: opts.reserveOutRef,
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
    "Recompute the ledger MPF root at the confirmed and committed-tip points and halt commits when the persisted root does not match",
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
                `MPF audit divergence: persisted=${result.persistedRoot},confirmed=${result.confirmedRoot},tip=${result.tipRoot ?? "unavailable"}${
                  result.tipIntegrityFailure === undefined
                    ? ""
                    : `,tip_integrity_failure=${result.tipIntegrityFailure}`
                }${
                  result.tipUnverifiable === undefined
                    ? ""
                    : `,tip_committed_root=${result.tipCommittedRoot ?? "unavailable"},tip_unverifiable=${result.tipUnverifiable}`
                }`,
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
  .command("reconcile-state")
  .description(
    "Read-only: compare L1, SQL, the native ledger root and the ledger cache; exit 1 on any failing check",
  )
  .option("--json", "print the report as JSON", false)
  .option(
    "--node-url <url>",
    "node HTTP base URL for the Architecture-G native root, read from /readyz with no LevelDB-copy fallback (default: this host on PORT, falling back to a copy of LEDGER_MPF_DB_PATH when the node gives no owner answer)",
  )
  .option(
    "--allow-in-flight",
    "accept transient states the reconciler cannot prove (listed in the report) instead of failing",
    false,
  )
  .action(
    async (opts: {
      json: boolean;
      nodeUrl?: string;
      allowInFlight: boolean;
    }) => {
      const mainEffect = pipe(
        StateReconciliation.stateReconciliationProgram({
          allowInFlight: opts.allowInFlight,
          ...(opts.nodeUrl === undefined ? {} : { nodeUrl: opts.nodeUrl }),
        }),
        Effect.tap((report) =>
          Effect.sync(() =>
            opts.json
              ? writeJson(report)
              : process.stdout.write(
                  `${StateReconciliation.formatStateReconciliationReport(report)}\n`,
                ),
          ),
        ),
        Effect.flatMap((report) =>
          report.ok
            ? Effect.succeed(report)
            : Effect.fail(
                new Error(
                  `state reconciliation: ${report.summary.fail.toString()} failing check(s)`,
                ),
              ),
        ),
        Effect.provide(Services.Database.layer),
        provideTxServices,
      );
      runCliEffect(mainEffect);
    },
  );

program
  .command("mpf-replay")
  .description(
    "Replay a recorded MPF NDJSON corpus through the TypeScript reference MPF and Architecture G using insert/fromlist fixtures",
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
