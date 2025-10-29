import { Effect } from "effect";
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

const submitGenesisDeposits: Effect.Effect<
  void,
  | SDK.Utils.LucidError
  | SDK.Utils.HashingError
  | SDK.Utils.DepositError
  | TxSubmitError
  | TxSignError,
  AlwaysSucceedsContract | Lucid | NodeConfig
> = Effect.gen(function* () {
  const { depositAuthValidator } = yield* AlwaysSucceedsContract;
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    return;
  }

  const l2Address = config.GENESIS_UTXOS[0].address;

  const depositParams: SDK.TxBuilder.UserEvents.Deposit.DepositParams = {
    depositScriptAddress: depositAuthValidator.spendScriptAddress,
    mintingPolicy: depositAuthValidator.mintScript,
    policyId: depositAuthValidator.policyId,
    depositInfo: {
      l2Address: l2Address,
      l2Datum: null,
    },
    inclusionTime: BigInt(
      Date.now() + config.PROTOCOL_PARAMETERS.event_wait_duration,
    ),
  };

  yield* lucid.switchToOperatorsMainWallet;

  const signedTx = yield* SDK.Endpoints.UserEvents.Deposit.depositTxProgram(
    lucid.api,
    depositParams,
  );

  yield* Effect.logInfo(`🟣 Submitting genesis deposit tx...`);
  yield* handleSignSubmitNoConfirmation(lucid.api, signedTx);
});

export const program: Effect.Effect<
  void,
  never,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.all([insertGenesisUtxos, submitGenesisDeposits], {
  concurrency: "unbounded",
}).pipe(Effect.catchAllCause(Effect.logInfo),);

