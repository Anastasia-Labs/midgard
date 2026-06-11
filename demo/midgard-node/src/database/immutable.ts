import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";

import {
  clearTable,
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Tx from "@/database/utils/tx.js";
import { Database } from "@/services/database.js";

export const tableName = "immutable";

const assertNoConflictingEntries = (
  txs: Tx.Entry[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txById = new Map<string, Buffer>();
    for (const tx of txs) {
      const key = tx[Tx.Columns.TX_ID].toString("hex");
      const existing = txById.get(key);
      if (existing !== undefined && !existing.equals(tx[Tx.Columns.TX])) {
        yield* Effect.fail(
          new SqlError.SqlError({
            cause: `immutable integrity violation: conflicting tx payloads in same batch for tx_id=${key}`,
          }),
        );
      }
      txById.set(key, tx[Tx.Columns.TX]);
    }

    const existingRows = yield* sql<
      Pick<Tx.Entry, Tx.Columns.TX_ID | Tx.Columns.TX>
    >`SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)} FROM ${sql(
      tableName,
    )} WHERE ${sql.in(
      Tx.Columns.TX_ID,
      txs.map((tx) => tx[Tx.Columns.TX_ID]),
    )}`;

    for (const row of existingRows) {
      const key = row[Tx.Columns.TX_ID].toString("hex");
      const expected = txById.get(key);
      if (expected !== undefined && !expected.equals(row[Tx.Columns.TX])) {
        yield* Effect.fail(
          new SqlError.SqlError({
            cause: `immutable integrity violation: existing tx payload differs for tx_id=${key}`,
          }),
        );
      }
    }
  });

const assertNativeTxEntryIdsMatchPayload = (
  txs: readonly Tx.Entry[],
): Effect.Effect<void, SqlError.SqlError> =>
  Effect.gen(function* () {
    for (let index = 0; index < txs.length; index += 1) {
      const tx = txs[index];
      const txId = tx[Tx.Columns.TX_ID];
      const txPayload = tx[Tx.Columns.TX];
      try {
        const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(txPayload);
        const computedTxId = computeMidgardNativeTxId(decoded);
        if (!computedTxId.equals(txId)) {
          yield* Effect.fail(
            new SqlError.SqlError({
              cause: `immutable integrity violation: tx_id mismatch at index=${index},stored=${txId.toString(
                "hex",
              )},computed=${computedTxId.toString("hex")}`,
            }),
          );
        }
      } catch (error) {
        yield* Effect.fail(
          new SqlError.SqlError({
            cause: `immutable integrity violation: invalid Midgard-native tx payload at index=${index},tx_id=${txId.toString(
              "hex",
            )},error=${formatUnknownError(error)}`,
          }),
        );
      }
    }
  });

export const insertTx = (
  tx: Tx.Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntry(tableName, tx);

export const insertTxs = (
  txs: Tx.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txs.length <= 0) {
      yield* Effect.logDebug("No txs provided, skipping immutable insertion.");
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* assertNoConflictingEntries(txs);
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(txs)}
      ON CONFLICT (${sql(Tx.Columns.TX_ID)})
      DO NOTHING`;
  }).pipe(
    Effect.withLogSpan(`insertTXs ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert the given transactions",
    ),
  );

/**
 * Strict insertion path for commit finalization.
 * Ensures each immutable row carries a decodable Midgard-native tx payload and
 * that its computed tx_id matches the primary key.
 */
export const insertTxsValidatedNative = (
  txs: Tx.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* assertNativeTxEntryIdsMatchPayload(txs).pipe(
      sqlErrorToDatabaseError(
        tableName,
        "Failed native tx payload validation for immutable insertion",
      ),
    );
    yield* insertTxs(txs);
  });

export const retrieve = Tx.retrieveAllEntries(tableName);

/**
 * Retrieves immutable transaction CBOR by transaction hash.
 */
export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => Tx.retrieveValues(tableName, txHashes);

export const retrieveTxEntriesByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
): Effect.Effect<
  readonly Pick<Tx.Entry, Tx.Columns.TX_ID | Tx.Columns.TX>[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (txHashes.length <= 0) {
      return [];
    }
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<
      Pick<Tx.Entry, Tx.Columns.TX_ID | Tx.Columns.TX>
    >`SELECT ${sql(
      Tx.Columns.TX_ID,
    )}, ${sql(Tx.Columns.TX)} FROM ${sql(tableName)} WHERE ${sql.in(
      Tx.Columns.TX_ID,
      txHashes,
    )}`;
  }).pipe(
    Effect.withLogSpan(`retrieveTxEntriesByHashes ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "retrieving tx entries error", e),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve tx entries for the given hashes",
    ),
  );

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.delMultiple(tableName, txHashes);

export const clear = clearTable(tableName);
