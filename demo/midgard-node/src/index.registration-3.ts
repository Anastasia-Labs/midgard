import "./index.registration-2.js";

import { Effect } from "effect";

import {
  failCli,
  parsePositiveIntegerOption,
  provideDatabaseServices,
  provideDatabaseTxServices,
  provideLucidOnlyServices,
  provideTxServices,
  runCliEffect,
  tapJson,
} from "./commands/cli-runtime.js";
import { parseEventId, parseHexBytes } from "./commands/command-utils.js";
import * as ContractDeploymentInfo from "./commands/contract-deployment-info.js";
import * as ReconcileCommand from "./commands/reconcile.js";
import * as MigrationRunner from "./database/migrations/runner.js";
import {
  parseOptionalHeaderHashOption,
  program,
} from "./index.registration.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "./workers/commit-block-header/da-payload-backfill.js";

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

export const reconcile = program
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
    "Inspect deposit visibility and projection into the L2 mempool ledger (read-only)",
  )
  .option("--event-id <hex>", "Canonical OutputReference CBOR deposit event id")
  .option("--cardano-tx-hash <hex>", "32-byte Cardano deposit transaction hash")
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: {
      readonly eventId?: string;
      readonly cardanoTxHash?: string;
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

      const mainEffect = provideDatabaseServices(
        ReconcileCommand.reconcileDepositProjectedProgram({
          eventId,
          cardanoTxHash,
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
  .description(
    "Reconcile DA payload and copied committee node attestation status",
  )
  .requiredOption("--header-hash <hex>", "28-byte block header hash")
  .option("--committee-url <url>", "Copied DA node base URL")
  .option(
    "--contract-deployment-info <path>",
    "Finalized V1 contract deployment info path used to derive the committee node deployment fingerprint",
  )
  .option(
    "--repair",
    "Backfill missing local DA payload rows from finalized journals",
  )
  .option("--json", "Print machine-readable JSON output", true)
  .action(
    async (options: {
      readonly headerHash: string;
      readonly committeeUrl?: string;
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
          committeeUrl: options.committeeUrl,
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
