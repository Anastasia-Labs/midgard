import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { ReadPage } from "../commands/listen-read-page.js";
import { removedHeader } from "../l1-queue-terminals/reads.js";
import { Database } from "../services/database.js";
import { HTTP_PUBLIC_READ_DATA_MAX_BYTES } from "../services/http-resource-limits.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

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

/** OFFSET 0 keeps each exclusion correlated; the planner must not scan all withdrawals. */
export const spendableUtxoPage = (
  address: string,
  page: ReadPage,
): Effect.Effect<readonly PublicUtxoPageRow[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SET TRANSACTION READ ONLY`;
        yield* sql`SET LOCAL statement_timeout = '2s'`;
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

export type RecentBlockPageRow = {
  readonly header_hash: Buffer;
  readonly block_start_time: Date;
  readonly block_end_time: Date;
  readonly block_time: Date;
  readonly l2_transaction_count: bigint | number | string;
  readonly deposit_count: bigint | number | string;
  readonly withdrawal_count: bigint | number | string;
  readonly forced_transaction_count: bigint | number | string;
  readonly total_event_count: bigint | number | string;
};

/** 36-byte keyset cursor: 8-byte big-endian epoch-ms + 28-byte header_hash. */
export const RECENT_BLOCK_CURSOR_BYTES = 36;

export const encodeRecentBlockCursor = (row: {
  readonly block_end_time: Date;
  readonly header_hash: Buffer;
}): Buffer => {
  const cursor = Buffer.allocUnsafe(RECENT_BLOCK_CURSOR_BYTES);
  cursor.writeBigInt64BE(BigInt(row.block_end_time.getTime()), 0);
  row.header_hash.copy(cursor, 8, 0, 28);
  return cursor;
};

export const decodeRecentBlockCursor = (
  cursor: Buffer,
): { readonly blockEndTime: Date; readonly headerHash: Buffer } => {
  if (cursor.length !== RECENT_BLOCK_CURSOR_BYTES)
    throw new Error("Recent block cursor must be 36 bytes");
  const epochMsBigInt = cursor.readBigInt64BE(0);
  const epochMs = Number(epochMsBigInt);
  if (
    epochMsBigInt < 0n ||
    !Number.isSafeInteger(epochMs) ||
    Number.isNaN(new Date(epochMs).getTime())
  )
    throw new Error("Invalid recent block cursor timestamp");
  return {
    blockEndTime: new Date(epochMs),
    headerHash: cursor.subarray(8, RECENT_BLOCK_CURSOR_BYTES),
  };
};

/** Recent block metadata from `da_payloads`, excluding removed headers and payload bytes. */
export const recentBlocksPage = (
  page: ReadPage,
): Effect.Effect<readonly RecentBlockPageRow[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const cursor =
      page.cursor === undefined
        ? undefined
        : decodeRecentBlockCursor(page.cursor);
    return yield* sql<RecentBlockPageRow>`SELECT
        da_payloads.header_hash,
        da_payloads.block_start_time,
        da_payloads.block_end_time,
        da_payloads.block_time,
        da_payloads.l2_transaction_count,
        da_payloads.deposit_count,
        da_payloads.withdrawal_count,
        da_payloads.forced_transaction_count,
        da_payloads.total_event_count
      FROM da_payloads
      WHERE NOT ${removedHeader(sql, "da_payloads.header_hash")}
        ${
          cursor === undefined
            ? sql``
            : sql`AND (da_payloads.block_end_time, da_payloads.header_hash) < (${cursor.blockEndTime}, ${cursor.headerHash})`
        }
      ORDER BY da_payloads.block_end_time DESC, da_payloads.header_hash DESC
      LIMIT ${page.limit + 1}`;
  }).pipe(sqlErrorToDatabaseError("da_payloads", "Failed bounded blocks read"));
