import { Database } from "@/services/database.js";
import {
  clearTable,
  insertKeyValue,
  delMultiple,
  retrieveValues,
  retrieveKeyValues,
  retrieveNumberOfEntries,
} from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { CML, fromHex } from "@lucid-evolution/lucid";

export type MempoolTx = {
  txHash: Uint8Array;
  txCbor: Uint8Array;
  inputs: Uint8Array[];
  outputs: Uint8Array[];
};

export const tableName = "mempool";

export const inputsTableName = "mempoolSpentInputs";

export const outputsTableName = MempoolLedgerDB.tableName;

export const init: Effect.Effect<void, Error, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      key BYTEA NOT NULL,
      value BYTEA NOT NULL,
      PRIMARY KEY (key)
    );`;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(inputsTableName)} (
      outref BYTEA NOT NULL,
      spendingTxHash BYTEA NOT NULL,
      PRIMARY KEY (outref),
      FOREIGN KEY (spendingTxHash) REFERENCES ${sql(tableName)}(key) ON DELETE CASCADE
    );`;
  },
);

export const insert = (
  txString: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const txCbor = fromHex(txString);
    const deserializedTx = CML.Transaction.from_cbor_bytes(txCbor);
    const txBody = deserializedTx.body();
    const txHash = CML.hash_transaction(txBody);
    const txHashBytes = Buffer.from(txHash.to_raw_bytes());
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    yield* insertKeyValue(tableName, txHashBytes, Buffer.from(txCbor));
    for (let i = 0; i < inputsCount; i++) {
      yield* insertKeyValue(
        inputsTableName,
        Buffer.from(inputs.get(i).to_cbor_bytes()),
        txHashBytes,
        "outref",
        "spendingTxHash",
      );
    }
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
      yield* MempoolLedgerDB.insert({
        txId: txHashBytes,
        outref: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        output: Buffer.from(output.to_cbor_bytes()),
        address: output.address().to_bech32(),
      });
    }
  });

/** Given a txHash, retrieves all associated values with a single SQL query.
 */
export const retrieveByHash = (
  txHash: Buffer,
): Effect.Effect<MempoolTx, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<MempoolTx>`
  SELECT 
    m.key, 
    ARRAY(SELECT outref FROM ${sql(inputsTableName)} WHERE spendingTxHash = m.key) AS inputs,
    ARRAY(SELECT output FROM ${sql(outputsTableName)} WHERE txId = m.key) AS outputs
  FROM ${sql(tableName)} m 
  WHERE m.key = ${txHash}
`;

    if (rows.length === 0) {
      yield* Effect.fail(new Error(`Transaction not found from ${tableName}`));
    }

    return rows[0];
  });

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = () => retrieveKeyValues(tableName);

export const retrieveTxCount = () => retrieveNumberOfEntries(tableName);

export const clearTxs = (txHashes: Buffer[]) =>
  delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
