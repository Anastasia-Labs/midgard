import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { HTTP_PUBLIC_READ_DATA_MAX_BYTES } from "../services/http-resource-limits.js";
import { PublicHttpReadLimitError } from "./http-read-byte-budget.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const publicTxCborByHash = (txId: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ tx: Buffer | null }>`WITH candidate AS (
    SELECT tx_id, 0 AS priority, octet_length(tx) AS payload_bytes FROM mempool WHERE tx_id = ${txId} AND included_by IS NULL
    UNION ALL SELECT tx_id, 1 AS priority, octet_length(tx) AS payload_bytes FROM immutable WHERE tx_id = ${txId}
  ) SELECT CASE WHEN candidate.payload_bytes <= ${HTTP_PUBLIC_READ_DATA_MAX_BYTES}
      THEN CASE WHEN candidate.priority = 0 THEN pending.tx ELSE confirmed.tx END END AS tx
    FROM candidate LEFT JOIN mempool pending ON pending.tx_id = candidate.tx_id
      LEFT JOIN immutable confirmed ON confirmed.tx_id = candidate.tx_id ORDER BY candidate.priority ASC LIMIT 1`;
    if (row === undefined)
      return yield* Effect.fail(
        new DatabaseError({
          table: "mempool",
          message: "Transaction is unavailable",
          cause: "no current retained membership",
        }),
      );
    if (row.tx === null)
      return yield* Effect.fail(
        new PublicHttpReadLimitError({ message: "E_HTTP_READ_DATA_LIMIT" }),
      );
    return row.tx;
  }).pipe(
    sqlErrorToDatabaseError(
      "mempool",
      "Failed bounded public transaction lookup",
    ),
  );

/** Same spendable predicate and caller order as the existing lookup; excess bytes fail the whole batch. */
export const publicUtxosByOutRefs = (outrefs: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (outrefs.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      outref: Buffer;
      output: Buffer | null;
    }>`WITH candidates AS MATERIALIZED (
    SELECT mempool_ledger.outref, octet_length(mempool_ledger.output) AS payload_bytes FROM mempool_ledger
    LEFT JOIN deposits_utxos ON deposits_utxos.event_id = mempool_ledger.source_event_id
    WHERE mempool_ledger.outref IN ${sql.in(outrefs)} AND ${MempoolLedgerDB.spendablePredicate(sql)}
  ), budget AS (SELECT outref, SUM(payload_bytes) OVER (ORDER BY outref ROWS UNBOUNDED PRECEDING) AS used_bytes FROM candidates)
  SELECT budget.outref, CASE WHEN used_bytes <= ${HTTP_PUBLIC_READ_DATA_MAX_BYTES} THEN stored.output END AS output
    FROM budget JOIN mempool_ledger stored ON stored.outref = budget.outref ORDER BY budget.outref ASC`;
    const byKey = new Map<string, { outref: Buffer; output: Buffer }>();
    for (const row of rows) {
      if (row.output === null)
        return yield* Effect.fail(
          new PublicHttpReadLimitError({ message: "E_HTTP_READ_DATA_LIMIT" }),
        );
      byKey.set(row.outref.toString("hex"), {
        outref: row.outref,
        output: row.output,
      });
    }
    return outrefs.flatMap((outref) => {
      const row = byKey.get(outref.toString("hex"));
      return row === undefined ? [] : [row];
    });
  }).pipe(
    sqlErrorToDatabaseError(
      "mempool_ledger",
      "Failed bounded public outref lookup",
    ),
  );
