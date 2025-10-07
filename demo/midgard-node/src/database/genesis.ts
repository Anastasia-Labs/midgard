import { Effect } from "effect";
import { Database, NodeConfig } from "@/services/index.js";
import { Columns as LedgerColumns } from "./utils/ledger.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { DatabaseError } from "./utils/common.js";

/**
 * Inserts genesis UTXOs from the imported TypeScript module into the MPT database
 * @returns Effect that succeeds if UTXOs were inserted, or fails on errors
 */
export const insertGenesisUtxos: Effect.Effect<
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
    `🟣 On testnet—Inserting ${ledgerEntries.length} genesis UTxOs...`,
  );

  yield* Effect.logInfo(
    `🟣 Debug: Inserting ${ledgerEntries.length} UTxOs into MempoolLedgerDB...`,
  );

  yield* MempoolLedgerDB.insert(ledgerEntries);

  yield* Effect.logInfo(
    `🟣 Successfully inserted ${ledgerEntries.length} genesis UTxOs. Funded addresses are:
${Array.from(new Set(config.GENESIS_UTXOS.map((u) => u.address))).join("\n")}`,
  );
});
