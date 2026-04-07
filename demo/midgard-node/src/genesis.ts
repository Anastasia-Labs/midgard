import { Effect, Schedule } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  AlwaysSucceedsContract,
  Lucid,
  Database,
  NodeConfig,
} from "@/services/index.js";
import { Columns as LedgerColumns } from "@/database/utils/ledger.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import { CML, TxBuilder, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { DatabaseError } from "@/database/utils/common.js";
import { handleSignSubmit } from "@/transactions/utils.js";

const insertGenesisUtxos: Effect.Effect<
  void,
  DatabaseError,
  NodeConfig | Database
> = Effect.gen(function* () {
  const config = yield* NodeConfig;

  if (config.NETWORK === "Mainnet") {
    yield* Effect.logInfo(`🟣 On mainnet—No genesis UTxOs will be inserted.`);
    return;
  }

  const ledgerEntries = config.GENESIS_UTXOS.map((utxo: UTxO) => {
    const core = utxoToCore(utxo);
    return {
      [LedgerColumns.TX_ID]: Buffer.from(utxo.txHash, "hex"),
      [LedgerColumns.OUTREF]: Buffer.from(core.input().to_cbor_bytes()),
      [LedgerColumns.OUTPUT]: Buffer.from(core.output().to_cbor_bytes()),
      [LedgerColumns.ADDRESS]: utxo.address,
    };
  });

  yield* Effect.logInfo(
    `🟣 On testnet — Inserting ${ledgerEntries.length} genesis UTxOs...`,
  );

  yield* MempoolLedgerDB.insert(ledgerEntries);

  yield* Effect.logInfo(
    `🟣 Successfully inserted ${ledgerEntries.length} genesis UTxOs. Funded addresses are:
${Array.from(new Set(config.GENESIS_UTXOS.map((u) => u.address))).join("\n")}`,
  );
}).pipe(
  Effect.catchTag("DatabaseError", (_e) =>
    Effect.logInfo(`🟣 Genesis UTxOs already exist. Skipping insertion.`),
  ),
  Effect.andThen(Effect.succeed(Effect.void)),
);

const genesisTxOrderTx = (nonceUTxO: UTxO) =>
  Effect.gen(function* () {
    const { txOrder } = yield* AlwaysSucceedsContract;
    const config = yield* NodeConfig;
    const lucid = yield* Lucid;

    const l2UTxO = config.GENESIS_UTXOS[0];
    if (!l2UTxO) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to build genesis tx order transaction",
          cause: "No GENESIS_UTXOS configured",
        }),
      );
    }
    yield* lucid.switchToOperatorsMainWallet;

    const l2Address = l2UTxO.address;

    yield* lucid.switchToOperatorsMainWallet;
    const operatorWalletAddress = yield* Effect.tryPromise({
      try: lucid.api.wallet().address,
      catch: (e) =>
        new SDK.LucidError({
          message:
            "Failed to build genesis tx order transaction, no refund address was deducible",
          cause: e,
        }),
    });

    const l2CoreUtxO = utxoToCore(l2UTxO);
    const l2Fee = 100n;
    const l2InputLovelace = l2CoreUtxO.output().amount().coin();
    const l2OutputLovelace = l2InputLovelace - l2Fee;

    if (l2OutputLovelace < 0n) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to build genesis tx order transaction",
          cause:
            "The first GENESIS_UTXOS entry doesn't have enough ADA to cover the L2 fee",
        }),
      );
    }

    const inputs = CML.TransactionInputList.new();
    inputs.add(l2CoreUtxO.input());

    const outputs = CML.TransactionOutputList.new();
    if (l2OutputLovelace !== 0n) {
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(l2Address),
          CML.Value.from_coin(l2OutputLovelace),
        ),
      );
    }

    const txBody = CML.TransactionBody.new(inputs, outputs, l2Fee);
    const witnessSet = CML.TransactionWitnessSet.new();
    const l2Tx = CML.Transaction.new(txBody, witnessSet, true);

    const txOrderParams = {
      txOrderScriptAddress: txOrder.spendingScriptAddress,
      mintingPolicy: txOrder.mintingScript,
      policyId: txOrder.policyId,
      nonceUTxO,
      cardanoTx: l2Tx,
      refundAddress: yield* SDK.addressDataFromBech32(operatorWalletAddress),
    };

    const txBuilder = yield* SDK.incompleteTxOrderTxProgram(
      lucid.api,
      txOrderParams,
    );
    return txBuilder;
  });

const genesisDepositTx = (
  nonceUTxO: UTxO,
): Effect.Effect<
  TxBuilder,
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.UnspecifiedNetworkError,
  AlwaysSucceedsContract | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const { deposit: depositAuthValidator } = yield* AlwaysSucceedsContract;
    const config = yield* NodeConfig;
    const lucid = yield* Lucid;

    const genesisUtxo = config.GENESIS_UTXOS[0];
    if (!genesisUtxo) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to build genesis deposit transaction",
          cause: "No GENESIS_UTXOS configured",
        }),
      );
    }

    const l2Address = yield* SDK.midgardAddressFromBech32(genesisUtxo.address);

    // Hard-coded 10 ADA deposit.
    const depositParams: SDK.DepositParams = {
      depositScriptAddress: depositAuthValidator.spendingScriptAddress,
      mintingPolicy: depositAuthValidator.mintingScript,
      policyId: depositAuthValidator.policyId,
      nonceUTxO,
      depositAmount: 10_000_000n,
      depositInfo: {
        l2Address: l2Address,
        l2Datum: null,
      },
    };

    yield* lucid.switchToOperatorsMainWallet;

    const txBuilder = yield* SDK.incompleteDepositTxProgram(
      lucid.api,
      depositParams,
    );
    return txBuilder;
  });

const submitComposedGenesisUserEvents = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    yield* Effect.logInfo(
      `🟣 Skipping genesis deposits + tx order - no GENESIS_UTXOS configured`,
    );
    return;
  }

  yield* Effect.logInfo(`🟣 Building composed genesis deposits + tx order...`);
  yield* lucid.switchToOperatorsMainWallet;
  const nonceUTxOs = yield* Effect.tryPromise(lucid.api.wallet().getUtxos).pipe(
    Effect.andThen((utxos) => {
      if (utxos.length < 2) {
        return Effect.fail(
          "At least two UTxOs must be present in operator's wallet.",
        );
      } else {
        return Effect.succeed([utxos[0], utxos[1]]);
      }
    }),
    Effect.catchAllCause((_cause) =>
      Effect.logInfo(
        `🟣 Skipping genesis user events - failed to fethc operator UTxOs`,
      ).pipe(Effect.as(undefined)),
    ),
  );
  if (nonceUTxOs === undefined) {
    return;
  }
  const [depositNonceUTxO, txOrderNonceUTxO] = nonceUTxOs;
  const depositBuilder = yield* genesisDepositTx(depositNonceUTxO);
  const txOrderBuilder = yield* genesisTxOrderTx(txOrderNonceUTxO);
  const composedBuilder = depositBuilder.compose(txOrderBuilder);

  const signedTx = yield* composedBuilder.completeProgram({
    localUPLCEval: false,
  });
  yield* handleSignSubmit(lucid.api, signedTx);
  yield* Effect.logInfo(`🟣 Composed genesis tx submitted successfully!`);
}).pipe(Effect.tapError(Effect.logInfo));

export const program: Effect.Effect<
  void,
  never,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.gen(function* () {
  yield* insertGenesisUtxos;
  yield* submitComposedGenesisUserEvents.pipe(
    Effect.retry(Schedule.fixed("5000 millis")),
  );
}).pipe(Effect.catchAllCause(Effect.logInfo));
