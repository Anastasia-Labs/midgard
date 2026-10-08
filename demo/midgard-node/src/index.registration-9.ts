import { normalizeHex } from "@al-ft/midgard-core/hex";
import { Effect } from "effect";

import {
  assertUserCliWalletIsOperationallyIsolated,
  failCli,
  provideDatabaseTxServices,
  runCliEffect,
  tapJson,
} from "./commands/cli-runtime.js";
import {
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "./commands/command-utils.js";
import {
  parseMerkleRootOption,
  parseOptionalEndTimeMs,
  parseOptionalHeaderHashOption,
  program,
} from "./index.registration.js";
import { runOperatorCommand } from "./index.registration-8.js";
import * as Services from "./services/index.js";
import * as DaAttestation from "./transactions/da-attestation.js";
import * as OperatorCommands from "./transactions/operators/commands.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "./transactions/reference-scripts.js";
import * as SubmitDeposit from "./transactions/submit-deposit.js";
import { commitExplicitBlockHeaderProgram } from "./workers/commit-block-header.js";

program
  .command("slash-duplicate-operator")
  .description(
    "Remove a duplicate registered node for an operator that is already registered, active, or retired; the slashing penalty is paid from its bond and the rest goes to the submitter",
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

program
  .command("submit-deposit")
  .description(
    "Submit an L1 deposit to the Midgard deposit contract using the selected signer wallet",
  )
  .requiredOption(
    "--submission-id <id>",
    "Stable request ID; reuse this ID to resume an interrupted submission",
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
        readonly submissionId: string;
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
            options.submissionId,
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
