/**
 * The rows that decide an acceptance-receipt member that is not pending
 * (`src/services/working-ledger-recompute.receipt-members.ts`): its
 * admission, its address history, and this node's block records of it.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

/** An accepted admission of `txId`. */
export const acceptedAdmission = (txId: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO tx_admissions
      (tx_id, arrival_seq, status, terminal_at, submit_source)
      VALUES (${txId}, nextval('tx_admissions_arrival_seq_seq'), 'accepted',
        now(), 'native')`,
  );

/** One address history row of `txId`. */
export const addressRow = (txId: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO address_history (tx_id, address)
      VALUES (${txId}, 'addr_test_receipt_member')`,
  );

/** A `blocks` row of `txId` under the block `headerHash` (hex). */
export const blockRow = (headerHash: string, txId: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO blocks (header_hash, tx_id)
      VALUES (${Buffer.from(headerHash, "hex")}, ${txId})`,
  );

/** An `immutable` row of `txId`. */
export const immutableRow = (txId: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO immutable (tx_id, tx)
      VALUES (${txId}, ${Buffer.from("a0", "hex")})`,
  );

/** `txId`'s admission status and reject code, if it has an admission. */
export const admissionOf = (txId: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql<{ status: string; reject_code: string | null }>`
      SELECT status, reject_code FROM tx_admissions WHERE tx_id = ${txId}`,
  ).pipe(Effect.map((rows) => rows[0]));

/** How many address history rows `txId` has. */
export const addressRows = (txId: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql<{ count: string }>`
      SELECT count(*)::text AS count FROM address_history WHERE tx_id = ${txId}`,
  ).pipe(Effect.map((rows) => Number(rows[0]?.count ?? 0)));

/** Takes `txId` out of both pending tables. */
export const leavePending = (txId: Buffer) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    Effect.zipRight(
      sql`DELETE FROM mempool WHERE tx_id = ${txId}`,
      sql`DELETE FROM processed_mempool WHERE tx_id = ${txId}`,
    ),
  );
