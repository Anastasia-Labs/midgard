/**
 * The commit end-time bound forced orders impose (plan §12.3, N10). A live
 * order the follower recorded but the node has no `forced_transaction_utxos`
 * row for (its carriage not yet resolved, or its row not yet written) may
 * become eligible at its inclusion time, so no block may end at or after
 * it: the bound is the earliest such inclusion time minus one. With no such
 * order there is no bound (`null`). An order that cannot be rebuilt bounds
 * the horizon as well; it also holds `/readyz` by name.
 *
 * A node row without a header whose order is gone (a rollback removed it)
 * bounds the horizon the same way until the forced-order hook deletes it or
 * the recovery disposes of its block (N10b). A row without a header is
 * still due, so no confirmed block ended at or after its inclusion time (a
 * block takes every due row): the bound never falls below a confirmed end.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { tableName as forcedTransactionsTable } from "../database/forcedTransactions.js";
import { canonicalForcedAdmission } from "../database/l1-admission-identity.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { FORCED_ORDERS_TABLE } from "./schema.js";

export const forcedOrderHorizon = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ earliest: string | null }>`SELECT LEAST(
      (SELECT MIN(f.inclusion_time)
        FROM ${sql(FORCED_ORDERS_TABLE)} f
        WHERE f.spent_slot IS NULL
          AND NOT EXISTS (
            SELECT 1 FROM ${sql(forcedTransactionsTable)} t
            WHERE t.tx_order_l1_tx_hash = f.order_tx_hash
              AND t.tx_order_l1_output_index = f.order_output_index
          )),
      (SELECT floor(extract(epoch FROM MIN(t.inclusion_time)) * 1000)::bigint
        FROM ${sql(forcedTransactionsTable)} t
        WHERE t.projected_header_hash IS NULL
          AND NOT ${canonicalForcedAdmission(sql, "t")})
    )::text AS earliest`;
  const earliest = rows[0]?.earliest ?? null;
  if (earliest === null) return null;
  const bound = Number(earliest) - 1;
  if (!Number.isSafeInteger(bound))
    return yield* Effect.fail(
      new DatabaseError({
        table: FORCED_ORDERS_TABLE,
        message: "A forced order's inclusion time cannot form a commit horizon",
        cause: earliest,
      }),
    );
  return bound;
}).pipe(
  sqlErrorToDatabaseError(
    FORCED_ORDERS_TABLE,
    "Failed to read the forced-order commit horizon",
  ),
);
