import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  Columns,
  type Entry,
  Status,
  tableName,
} from "./forcedTransactions.exact-forced-transaction-journal-member.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

export const markFinalizedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      [Columns.TX_ORDER_ID]: Buffer;
    }>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.TX_ORDER_ID)} IN ${sql.in(ids)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Finalized})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      RETURNING ${sql(Columns.TX_ORDER_ID)}`;
    if (rows.length !== ids.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to finalize forced transactions because at least one row is missing, unprojected, or assigned to a different header",
          cause: `requested=${ids.length},finalized=${rows.length},header_hash=${projectedHeaderHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markFinalizedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark forced transactions finalized",
    ),
  );

export const toRootKeyValue = (
  entry: Entry,
): { readonly key: Buffer; readonly value: Buffer } => ({
  key: Buffer.from(entry[Columns.TX_ORDER_ID]),
  value: Buffer.from(entry[Columns.FORCED_INCLUSION_VALUE]),
});

export const clear = clearTable(tableName);
