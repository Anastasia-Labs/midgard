import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { ReadPage } from "../commands/listen-read-page.js";
import { Database } from "../services/database.js";
import { HTTP_PUBLIC_READ_DATA_MAX_BYTES } from "../services/http-resource-limits.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import { publicWithdrawalReadAuthorityReady } from "./withdrawal-ledger-outref.backfill.js";

export const addressHistoryPage = (address: string, page: ReadPage) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{
      tx_id: Buffer;
      tx: Buffer | null;
    }>`WITH candidates AS MATERIALIZED (
      SELECT history.tx_id, octet_length(COALESCE(pending.tx, confirmed.tx)) AS payload_bytes
      FROM address_history AS history
      LEFT JOIN mempool AS pending ON pending.tx_id = history.tx_id
      LEFT JOIN immutable AS confirmed ON confirmed.tx_id = history.tx_id
      WHERE history.address = ${address} AND history.tx_id > ${page.cursor ?? Buffer.alloc(0)}
        AND (pending.tx IS NOT NULL OR confirmed.tx IS NOT NULL)
      ORDER BY history.tx_id ASC LIMIT ${page.limit + 1}
    ), budget AS (
      SELECT tx_id, SUM(payload_bytes) OVER (ORDER BY tx_id ROWS UNBOUNDED PRECEDING) AS used_bytes FROM candidates
    ) SELECT budget.tx_id, CASE WHEN used_bytes <= ${HTTP_PUBLIC_READ_DATA_MAX_BYTES}
      THEN COALESCE(pending.tx, confirmed.tx) END AS tx FROM budget
      LEFT JOIN mempool AS pending ON pending.tx_id = budget.tx_id
      LEFT JOIN immutable AS confirmed ON confirmed.tx_id = budget.tx_id ORDER BY budget.tx_id ASC`;
  }).pipe(
    sqlErrorToDatabaseError(
      "address_history",
      "Failed bounded address-history read",
    ),
  );

export type PublicUtxoPageRow = Omit<
  MempoolLedgerDB.EntryWithTimeStamp,
  "output"
> & { readonly output: Buffer | null };

/** OFFSET 0 keeps each exclusion correlated; the planner must not scan all withdrawals.
 * Candidate exclusion, byte barrier and unknown check share one read-only MVCC snapshot. */
export const spendableUtxoPage = (
  address: string,
  page: ReadPage,
): Effect.Effect<readonly PublicUtxoPageRow[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY`;
        yield* sql`SET LOCAL statement_timeout = '2s'`;
        if (!(yield* publicWithdrawalReadAuthorityReady))
          return yield* Effect.fail(
            new DatabaseError({
              table: "withdrawal_utxos",
              message: "E_PUBLIC_READ_MIGRATING",
              cause: "pending withdrawal read authority is not populated",
            }),
          );
        return yield* sql<PublicUtxoPageRow>`/* midgard-public-utxo-page */ WITH candidates AS MATERIALIZED (
      SELECT mempool_ledger.outref, octet_length(mempool_ledger.output) AS payload_bytes FROM mempool_ledger
      LEFT JOIN deposits_utxos ON deposits_utxos.event_id = mempool_ledger.source_event_id
      WHERE mempool_ledger.address = ${address} AND mempool_ledger.outref > ${page.cursor ?? Buffer.alloc(0)}
        AND ${MempoolLedgerDB.spendablePredicate(sql)}
        AND NOT EXISTS (SELECT 1 FROM withdrawal_utxos
          WHERE withdrawal_utxos.l2_ledger_outref = mempool_ledger.outref AND l2_ledger_outref_state = 'decoded'
            AND status IN ('awaiting', 'projected') AND (validity IS NULL OR validity = 'WithdrawalIsValid') OFFSET 0)
      ORDER BY mempool_ledger.outref ASC LIMIT ${page.limit + 1}
      ), budget AS (SELECT outref, SUM(payload_bytes) OVER (ORDER BY outref ROWS UNBOUNDED PRECEDING) AS used_bytes FROM candidates)
      SELECT stored.tx_id, stored.outref, stored.address, stored.source_event_id, stored.time_stamp_tz,
        CASE WHEN used_bytes <= ${HTTP_PUBLIC_READ_DATA_MAX_BYTES} THEN stored.output END AS output
      FROM budget JOIN mempool_ledger stored ON stored.outref = budget.outref ORDER BY stored.outref ASC`;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolLedgerDB.tableName,
      "Failed bounded spendable ledger read",
    ),
  );

export const blockTransactionPage = (headerHash: Buffer, page: ReadPage) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      tx_id: Buffer;
    }>`SELECT tx_id FROM blocks WHERE header_hash = ${headerHash}
    AND tx_id > ${page.cursor ?? Buffer.alloc(0)} ORDER BY tx_id ASC LIMIT ${page.limit + 1}`;
    return rows.map((row) => row.tx_id);
  }).pipe(sqlErrorToDatabaseError("blocks", "Failed bounded block read"));
