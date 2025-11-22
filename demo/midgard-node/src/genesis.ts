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
import {
  getAddressDetails,
  TxSubmitError,
  UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { DatabaseError } from "@/database/utils/common.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
} from "./transactions/utils.js";

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

const submitGenesisTxOrders: Effect.Effect<
  void,
  | SDK.HashingError
  | SDK.LucidError
  | SDK.TxOrderError
  | SDK.ParsingError
  | TxSignError
  | TxSubmitError
  | TxConfirmError,
  AlwaysSucceedsContract | Lucid | NodeConfig
> = Effect.gen(function* () {
  yield* Effect.logInfo(`🟣 Building genesis tx order tx...`);

  const { txOrderAuthValidator } = yield* AlwaysSucceedsContract;
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    yield* Effect.logInfo(
      `🟣 Skipping genesis tx order - no GENESIS_UTXOS configured`,
    );
    return;
  }
  yield* lucid.switchToOperatorsMainWallet;

  const l2Address = config.GENESIS_UTXOS[0].address;
  const l2AddressData = yield* SDK.parseAddressDataCredentials(l2Address);
  const inclusionTime = Date.now();

  const txBuilder = lucid.api
    .newTx()
    .pay.ToAddress(l2Address, { lovelace: 1_000_00n });
  const txSignBuilder = yield* Effect.tryPromise({
    try: () => txBuilder.complete(),
    catch: (err) =>
      new SDK.LucidError({
        message: "Failed to build genesis tx order transaction",
        cause: err,
      }),
  });
  const tx = txSignBuilder.toTransaction();

  const txOrderParams: SDK.TxOrderParams = {
    txOrderAddress: txOrderAuthValidator.spendScriptAddress,
    mintingPolicy: txOrderAuthValidator.mintScript,
    policyId: txOrderAuthValidator.policyId,
    refundAddress: l2AddressData,
    refundDatum: "",
    inclusionTime: BigInt(inclusionTime),
    midgardTxBody: "",
    midgardTxWits: "",
    cardanoTx: tx,
  };

  const signedTx = yield* SDK.unsignedTxOrderTxProgram(
    lucid.api,
    txOrderParams,
  );
  yield* Effect.logInfo(
    `🟣 Submitting genesis tx order to L1 (inclusion time: ${inclusionTime})...`,
  );
  yield* handleSignSubmit(lucid.api, signedTx);
  yield* Effect.logInfo(
    `🟣 Genesis tx order submitted successfully! Waiting for L1 confirmation...`,
  );
});

const submitGenesisDeposits: Effect.Effect<
  void,
  | SDK.LucidError
  | SDK.HashingError
  | SDK.DepositError
  | TxSubmitError
  | TxSignError
  | TxConfirmError,
  AlwaysSucceedsContract | Lucid | NodeConfig
> = Effect.gen(function* () {
  yield* Effect.logInfo(`🟣 Building genesis deposit tx...`);

  const { depositAuthValidator } = yield* AlwaysSucceedsContract;
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    return;
  }

  const l2Address = config.GENESIS_UTXOS[0].address;

  // Hard-coded 10 ADA deposit.
  const depositParams: SDK.DepositParams = {
    depositScriptAddress: depositAuthValidator.spendScriptAddress,
    mintingPolicy: depositAuthValidator.mintScript,
    policyId: depositAuthValidator.policyId,
    depositAmount: 10_000_000n,
    depositInfo: {
      l2Address: l2Address,
      l2Datum: null,
    },
  };

  yield* lucid.switchToOperatorsMainWallet;

  const signedTx = yield* SDK.unsignedDepositTxProgram(
    lucid.api,
    depositParams,
  );
  yield* handleSignSubmit(lucid.api, signedTx);
});

export const program: Effect.Effect<
  void,
  never,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.gen(function* () {
  yield* insertGenesisUtxos;
  yield* submitGenesisDeposits.pipe(
    Effect.retry(Schedule.fixed("5000 millis")),
  );
  yield* submitGenesisTxOrders;
}).pipe(Effect.catchAllCause(Effect.logInfo));
