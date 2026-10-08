/**
 * The node's landed-block rows (plan §5.5 P3, N3): one per landed
 * state-queue block the node processed, with its net ledger delta against
 * its parent's ledger and the events it included, and the confirmed-ledger
 * frontier (the landed header whose post-state `confirmed_ledger` holds).
 * Migration 0009 holds the tables. What the working ledger is rebuilt on is
 * not stored: it is the rows' `applied` flags and the ledger-store root
 * stamp.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import type * as Ledger from "../database/utils/ledger.js";

export const tableName = "node_landed_blocks";
export const frontierTableName = "node_confirmed_ledger_frontier";

export type LandedBlockKind = "own" | "foreign";

/** A withdrawal a block included, with the classification its replay gave. */
export type WithdrawalMembership = Readonly<{
  /** Event id CBOR, hex. */
  id: string;
  validity: string;
  detail: unknown;
  /** Settlement event info CBOR, hex. */
  settlement: string;
}>;

export type LandedBlockRow = Readonly<{
  headerHash: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  utxosRoot: string;
  kind: LandedBlockKind;
  /** `removed`: it left the queue after the working ledger took it in. */
  state: "processed" | "removed";
  /** Whether the working ledger and the native MPF hold it. */
  applied: boolean;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  depositIds: readonly Buffer[];
  withdrawals: readonly WithdrawalMembership[];
  forcedIds: readonly Buffer[];
  txIds: readonly Buffer[];
}>;

/** A header and the root of its post-state. */
export type HeaderRoot = Readonly<{ headerHash: string; utxosRoot: string }>;

const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table: tableName, message, cause });

const bytea = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

const buffers = (value: unknown): Buffer[] =>
  ((value ?? []) as Uint8Array[]).map((item) => Buffer.from(item));

type RawRow = {
  header_hash: Buffer;
  parent_header_hash: Buffer;
  parent_utxos_root: string;
  utxos_root: string;
  kind: LandedBlockKind;
  state: "processed" | "removed";
  applied: boolean;
  spent: unknown;
  produced_outrefs: unknown;
  produced_outputs: unknown;
  deposit_ids: unknown;
  withdrawals: unknown;
  forced_ids: unknown;
  tx_ids: unknown;
};

const decodeRow = (row: RawRow): LandedBlockRow => {
  const outRefs = buffers(row.produced_outrefs);
  const outputs = buffers(row.produced_outputs);
  const withdrawals =
    typeof row.withdrawals === "string"
      ? (JSON.parse(row.withdrawals) as WithdrawalMembership[])
      : (row.withdrawals as WithdrawalMembership[]);
  return {
    headerHash: Buffer.from(row.header_hash).toString("hex"),
    parentHeaderHash: Buffer.from(row.parent_header_hash).toString("hex"),
    parentUtxosRoot: row.parent_utxos_root,
    utxosRoot: row.utxos_root,
    kind: row.kind,
    state: row.state,
    applied: row.applied,
    spent: buffers(row.spent),
    produced: outRefs.map((outref, index) => ({
      outref,
      output: outputs[index]!,
    })),
    depositIds: buffers(row.deposit_ids),
    withdrawals,
    forcedIds: buffers(row.forced_ids),
    txIds: buffers(row.tx_ids),
  };
};

/** Every row, processed and removed. */
export const retrieveRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<RawRow>`SELECT * FROM node_landed_blocks
    ORDER BY processed_at, header_hash`;
  return rows.map(decodeRow);
}).pipe(sqlErrorToDatabaseError(tableName, "Failed to read landed blocks"));

export const insertRow = (row: LandedBlockRow) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const inserted = yield* sql<{ header_hash: Buffer }>`
      INSERT INTO node_landed_blocks (header_hash, parent_header_hash,
        parent_utxos_root, utxos_root, kind, state, applied, spent,
        produced_outrefs, produced_outputs, deposit_ids, withdrawals,
        forced_ids, tx_ids)
      VALUES (${Buffer.from(row.headerHash, "hex")},
        ${Buffer.from(row.parentHeaderHash, "hex")}, ${row.parentUtxosRoot},
        ${row.utxosRoot}, ${row.kind}, ${row.state}, ${row.applied},
        ${pg.array(bytea(row.spent))}::bytea[],
        ${pg.array(bytea(row.produced.map((entry) => entry.outref)))}::bytea[],
        ${pg.array(bytea(row.produced.map((entry) => entry.output)))}::bytea[],
        ${pg.array(bytea(row.depositIds))}::bytea[],
        CAST(${JSON.stringify(row.withdrawals)} AS TEXT)::jsonb,
        ${pg.array(bytea(row.forcedIds))}::bytea[],
        ${pg.array(bytea(row.txIds))}::bytea[])
      ON CONFLICT (header_hash) DO NOTHING
      RETURNING header_hash`;
    if (inserted.length !== 1)
      return yield* Effect.fail(
        failure("A landed block was processed twice", row.headerHash),
      );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to record a landed block"),
  );

const byHashes = (hashes: readonly string[]) =>
  bytea(hashes.map((hash) => Buffer.from(hash, "hex")));

export const deleteRows = (hashes: readonly string[]) =>
  Effect.gen(function* () {
    if (hashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`DELETE FROM node_landed_blocks
      WHERE header_hash = ANY(${pg.array(byHashes(hashes))}::bytea[])`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete landed blocks"));

/** Moves rows between `processed` and `removed`. */
export const setState = (
  hashes: readonly string[],
  state: LandedBlockRow["state"],
) =>
  Effect.gen(function* () {
    if (hashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`UPDATE node_landed_blocks SET state = ${state}
      WHERE header_hash = ANY(${pg.array(byHashes(hashes))}::bytea[])`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to move landed blocks"));

export const markApplied = (hashes: readonly string[]) =>
  Effect.gen(function* () {
    if (hashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`UPDATE node_landed_blocks SET applied = true
      WHERE header_hash = ANY(${pg.array(byHashes(hashes))}::bytea[])`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to mark landed blocks"));

const singleRow = (table: string) => ({
  retrieve: Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ header_hash: Buffer; utxos_root: string }>`
      SELECT header_hash, utxos_root FROM ${sql(table)}`;
    return row === undefined
      ? undefined
      : ({
          headerHash: Buffer.from(row.header_hash).toString("hex"),
          utxosRoot: row.utxos_root,
        } satisfies HeaderRoot);
  }).pipe(sqlErrorToDatabaseError(table, `Failed to read ${table}`)),
  upsert: (value: HeaderRoot) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`INSERT INTO ${sql(table)} (id, header_hash, utxos_root)
        VALUES (true, ${Buffer.from(value.headerHash, "hex")}, ${value.utxosRoot})
        ON CONFLICT (id) DO UPDATE SET header_hash = EXCLUDED.header_hash,
          utxos_root = EXCLUDED.utxos_root, updated_at = NOW()`;
    }).pipe(sqlErrorToDatabaseError(table, `Failed to write ${table}`)),
});

/** The header whose post-state `confirmed_ledger` holds. */
export const Frontier = singleRow(frontierTableName);
