import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {
  sqlErrorToDatabaseError,
  DatabaseError,
} from "@/database/utils/common.js";
import { CML, Data, PolicyId } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";

export enum Columns {
  ID = "event_id",
  INFO = "event_info",
  ASSET_NAME = "asset_name",
  L1_UTXO_CBOR = "l1_utxo_cbor",
  INCLUSION_TIME = "inclusion_time",
}

export type Entry = {
  [Columns.ID]: Buffer;
  [Columns.INFO]: Buffer;
  [Columns.ASSET_NAME]: string;
  [Columns.L1_UTXO_CBOR]: Buffer;
  [Columns.INCLUSION_TIME]: Date;
};

export const createTable = (
  tableName: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
        ${sql(Columns.ID)} BYTEA NOT NULL,
        ${sql(Columns.INFO)} BYTEA NOT NULL,
        ${sql(Columns.L1_UTXO_CBOR)} BYTEA NOT NULL,
        ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
        PRIMARY KEY (${sql(Columns.ID)})
      );`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const insertEntry = (
  tableName: string,
  entry: Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert UTxO`);
    const sql = yield* SqlClient.SqlClient;
    // No need to handle conflicts.
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}`;
  }).pipe(
    Effect.withLogSpan(`insertEntry ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntry: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert the given UTxO"),
  );

export const insertEntries = (
  tableName: string,
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert UTxOs`);
    const sql = yield* SqlClient.SqlClient;
    if (entries.length <= 0) {
      yield* Effect.logDebug("No entries provided, skipping insertion.");
      return;
    }
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given UTxOs"),
  );

export const retrieveTimeBoundEntries = (
  tableName: string,
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt to retrieveTimeBoundEntries`,
    );
    const sql = yield* SqlClient.SqlClient;
    const result = yield* sql<Entry>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${startTime} < ${sql(Columns.INCLUSION_TIME)} AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}`;
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveTimeBoundEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveTimeBoundEntries: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve all UTxOs"),
  );

export const retrieveAllEntries = (
  tableName: string,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieveEntries`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieveEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve all UTxOs"),
  );

export const delEntries = (
  tableName: string,
  ids: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.ID,
    )} IN ${sql.in(ids)}`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete given UTxOs"));

export const entryConverter = (
  entry: Entry,
  policyId: PolicyId,
) => Effect.gen(function* () {
      const l1_utxo = CML.TransactionUnspentOutput.from_cbor_bytes(Buffer.from(entry[Columns.L1_UTXO_CBOR]))
      const policyIdScriptHash = CML.ScriptHash.from_hex(policyId)
      const assets = CML.MapAssetNameToCoin.new()
      assets.insert(CML.AssetName.from_hex(entry[Columns.ASSET_NAME]), 1n)
      // We assume that it returns a number of succesful insertions
      if (assets === undefined) {
        throw new Error("TODO: change me")
      }

      const verificationNftMultiasset = CML.MultiAsset.new()
      verificationNftMultiasset.insert_assets(policyIdScriptHash, assets) // Same return as above
      if (verificationNftMultiasset === undefined) {
        throw new Error("TODO: change me")
      }
      const verificationNft = CML.Value.new(0n, verificationNftMultiasset)

      // We need to subtract the L2 midgard nft before inserting the values to L2 UTxO
      const l2Amount: CML.Value = l1_utxo.output().amount().checked_sub(verificationNft)

      const depositDatum = Data.from(SDK.bufferToHex(entry[Columns.INFO]), SDK.DepositInfo)
      const l2Address = CML.Address.from_bech32(depositDatum.l2Address)

      let l2Datum = undefined
      if (depositDatum.l2Datum !== null) {
        l2Datum = CML.DatumOption.from_cbor_hex(depositDatum.l2Datum)
      }

      const transactionOutput = CML.TransactionOutput.new(
        l2Address,
        l2Amount,
        l2Datum,
      )

      const transactionId = CML.TransactionHash.from_hex(entry[Columns.ASSET_NAME])
      const outRef = CML.TransactionInput.new(transactionId, 0n)
      return {outRef, transactionOutput}
})
