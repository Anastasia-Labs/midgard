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
  TxSubmitError,
  UTxO,
  utxoToCore,
  Script,
} from "@lucid-evolution/lucid";
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

export const getGenesisScriptInputs = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<{ keys: Buffer[]; values: Buffer[] }, SDK.HashingError> =>
  Effect.gen(function* () {
    const scripts = Object.values(contracts).flatMap((v) => {
      const s: string[] = [];
      if ("spendScript" in v) s.push(v.spendingCBOR);
      if ("mintScript" in v) s.push(v.mintingCBOR);
      return s;
    });

    const uniqueHashes = yield* Effect.all(
      [...new Set(scripts)].map((cbor) => SDK.hashHexWithBlake2b256(cbor)),
      { concurrency: "unbounded" },
    );

    const keys: Buffer[] = [];
    const values: Buffer[] = [];

    uniqueHashes.sort().forEach((hash, index) => {
      const key = Buffer.alloc(4);
      key.writeUInt32BE(index);
      keys.push(key);
      values.push(Buffer.from(hash, "hex"));
    });

    return { keys, values };
  });

const submitGenesisDeposits: Effect.Effect<
  void,
  | SDK.LucidError
  | SDK.HashingError
  | SDK.DepositError
  | TxSubmitError
  | TxSignError,
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
