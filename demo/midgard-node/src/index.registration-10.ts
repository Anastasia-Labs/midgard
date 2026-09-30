import "./index.registration-9.js";

import { Effect, pipe } from "effect";

import {
  assertUserCliWalletIsOperationallyIsolated,
  errorMessage,
  failCli,
  provideDatabaseServices,
  provideDatabaseTxServices,
  runCliEffect,
  tapJson,
  writeJson,
} from "./commands/cli-runtime.js";
import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  parseAddressArgument,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "./commands/command-utils.js";
import * as EventSettlementProofCommand from "./commands/event-settlement-proof.js";
import * as ReservePayoutCommand from "./commands/reserve-payout.js";
import * as SubmitL2Transfer from "./commands/submit-l2-transfer.js";
import * as SubmitWithdrawalCommand from "./commands/submit-withdrawal.js";
import * as UtxosCommand from "./commands/utxos.js";
import { program } from "./index.registration.js";
import * as Services from "./services/index.js";

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
        Effect.provide(Services.Lucid.Default),
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
    "--submission-id <id>",
    "Stable request ID; reuse this ID to resume an interrupted submission",
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
            submissionId: opts.submissionId,
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
      provideDatabaseTxServices,
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
