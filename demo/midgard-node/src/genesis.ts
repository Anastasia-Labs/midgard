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
import { TxSubmitError, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { DatabaseError } from "@/database/utils/common.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
} from "@/transactions/utils.js";

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

const submitGenesisDeposits: Effect.Effect<
  void,
  | SDK.Bech32DeserializationError
  | SDK.DepositError
  | SDK.HashingError
  | SDK.LucidError
  | TxSubmitError
  | TxSignError,
  AlwaysSucceedsContract | Lucid | NodeConfig
> = Effect.gen(function* () {
  yield* Effect.logInfo(`🟣 Building genesis deposit tx...`);

  const { deposit: depositAuthValidator } = yield* AlwaysSucceedsContract;
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    return;
  }

  const l2Address = yield* SDK.midgardAddressFromBech32(
    config.GENESIS_UTXOS[0].address,
  );

  // Hard-coded 10 ADA deposit.
  const depositParams: SDK.DepositParams = {
    depositScriptAddress: depositAuthValidator.spendingScriptAddress,
    mintingPolicy: depositAuthValidator.mintingScript,
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
  yield* handleSignSubmitNoConfirmation(lucid.api, signedTx);
}).pipe(Effect.tapError(Effect.logInfo));

export const program: Effect.Effect<
  void,
  never,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.all(
  [
    insertGenesisUtxos,
    submitGenesisDeposits.pipe(Effect.retry(Schedule.fixed("5000 millis"))),
  ],
  { concurrency: "unbounded" },
).pipe(Effect.catchAllCause(Effect.logInfo));
