import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  MidgardContracts,
  Lucid,
  Database,
  NodeConfig,
} from "@/services/index.js";
import { Columns as LedgerColumns } from "@/database/utils/ledger.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import { TxSubmitError, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { DatabaseError } from "@/database/utils/common.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
} from "@/transactions/utils.js";
import {
  buildUnsignedDepositTxProgram,
  type SubmitDepositError,
} from "@/transactions/submit-deposit.js";

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
  | SDK.LucidError
  | SDK.HashingError
  | SDK.HubOracleError
  | SDK.Bech32DeserializationError
  | SubmitDepositError
  | TxSubmitError
  | TxConfirmError
  | TxSignError,
  MidgardContracts | Lucid | NodeConfig
> = Effect.gen(function* () {
  yield* Effect.logInfo(`🟣 Building genesis deposit tx...`);

  const contracts = yield* MidgardContracts;
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    return;
  }

  const l2Address = config.GENESIS_UTXOS[0].address;

  // Hard-coded 10 ADA deposit.
  const depositConfig = {
    l2Address,
    l2Datum: null,
    lovelace: 10_000_000n,
    additionalAssets: {},
  };

  yield* lucid.switchToOperatorsMainWallet;

  const signedTx = yield* buildUnsignedDepositTxProgram(
    lucid.api,
    contracts,
    depositConfig,
  );
  yield* handleSignSubmit(lucid.api, signedTx);
}).pipe(Effect.tapError(Effect.logInfo));

export const program: Effect.Effect<
  void,
  never,
  MidgardContracts | Database | Lucid | NodeConfig
> = Effect.all(
  [insertGenesisUtxos, submitGenesisDeposits],
  { concurrency: "unbounded" },
).pipe(Effect.catchAllCause(Effect.logInfo));
