/**
 * The import checker's durable set (migration 0028): each Plutus
 * transaction of an imported foreign block, applied by its committed
 * verdict, with what its script runs are rebuilt from. An entry is written
 * in the transaction that processes its block, and leaves the set on an
 * accepting local verdict, its block's removal, or its deadline.
 *
 * Storage is linear in the block: each entry holds its transaction's bytes
 * and sidecar once, each output the block's entries spend or reference is
 * held once per block (`node_import_check_outputs`), and the block's
 * validation context once (`node_import_check_blocks`). No run's context
 * is stored; the checker rebuilds them one at a time.
 *
 * - `pending`: not yet run by the checker;
 * - `cap_skipped`: not run, the block's checker time was spent (backlog);
 * - `cache_skipped`: not run, a script of it is in the negative cache
 *   (backlog);
 * - `faulted`: a run faulted twice, the second time on a fresh instance;
 * - `rejected`: the local evaluator rejected a run. It stays, for a
 *   challenge, until its deadline or its block's removal.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import type {
  ImportBlockChecks,
  ImportCheckContext,
} from "../mpf/verified-block-import.js";
import { sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "node_import_checks";
export const blocksTableName = "node_import_check_blocks";
export const outputsTableName = "node_import_check_outputs";

export type ImportCheckState =
  | "pending"
  | "cap_skipped"
  | "cache_skipped"
  | "faulted"
  | "rejected";

/** The states that have no local verdict yet and wait for a run; the
 * backlog lane serves all of them. */
export const UNRUN_STATES = [
  "pending",
  "cap_skipped",
  "cache_skipped",
] as const satisfies readonly ImportCheckState[];

export type ImportCheckKey = Readonly<{
  headerHash: string;
  transactionId: Buffer;
}>;

/** An entry without its material: what a pass schedules by. */
export type ImportCheckEntry = ImportCheckKey &
  Readonly<{
    position: number;
    deadlineMs: number;
    state: ImportCheckState;
    /** The sha256 of each distinct script its runs execute. */
    scriptHashes: readonly Buffer[];
  }>;

/** What an entry's runs are rebuilt from. */
export type ImportCheckMaterial = Readonly<{
  transactionCbor: Buffer;
  programMaterialSidecarCbor: Buffer;
  context: ImportCheckContext;
  /** Each output the transaction spends or references. */
  outputs: readonly Readonly<{ outRef: Buffer; output: Buffer }>[];
}>;

/** The most rows, and bytes, one insert of a block's checks carries. */
export const RECORD_BATCH_ROWS = 256;
export const RECORD_BATCH_BYTES = 4 * 1024 * 1024;

/** Splits `rows` into batches of at most `RECORD_BATCH_ROWS` rows and
 * `RECORD_BATCH_BYTES` bytes (a larger row is a batch of its own). */
const batches = <A>(rows: readonly A[], bytesOf: (row: A) => number) => {
  const out: A[][] = [];
  let batch: A[] = [];
  let bytes = 0;
  for (const row of rows) {
    const size = bytesOf(row);
    if (
      batch.length > 0 &&
      (batch.length >= RECORD_BATCH_ROWS || bytes + size > RECORD_BATCH_BYTES)
    ) {
      out.push(batch);
      batch = [];
      bytes = 0;
    }
    batch.push(row);
    bytes += size;
  }
  if (batch.length > 0) out.push(batch);
  return out;
};

const hashBytes = (hashes: readonly string[]) =>
  hashes.map((hash) => `\\x${hash}`);

const hexOf = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

/**
 * Records a processed block's checks, in the caller's transaction, in
 * batches. A block processed again (it landed again after its row was
 * deleted) keeps the entries it still has.
 */
export const recordChecks = (
  headerHash: string,
  deadlineMs: number,
  block: ImportBlockChecks,
) =>
  Effect.gen(function* () {
    if (block.checks.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const header = Buffer.from(headerHash, "hex");
    const { context } = block;
    yield* sql`INSERT INTO node_import_check_blocks ${sql.insert({
      header_hash: header,
      block_time_ms: context.blockTimeMs,
      block_slot: context.blockSlot.toString(),
      network_id: context.expectedNetworkId.toString(),
      min_fee_a: context.minFeeA.toString(),
      min_fee_b: context.minFeeB.toString(),
      slot_zero_time: context.l1SlotConfig.zeroTime,
      slot_zero_slot: context.l1SlotConfig.zeroSlot,
      slot_length: context.l1SlotConfig.slotLength,
    })} ON CONFLICT (header_hash) DO NOTHING`;
    const rows = block.checks.map((check) => ({
      header_hash: header,
      tx_id: check.transactionId,
      block_position: check.position,
      deadline_ms: deadlineMs,
      state: "pending",
      tx_cbor: check.transactionCbor,
      program_material_sidecar: check.programMaterialSidecarCbor,
      runs: check.runs,
      budget_cpu: check.cpu.toString(),
      run_bytes: check.runBytes,
      script_hashes: Buffer.concat(check.scriptHashes),
    }));
    for (const batch of batches(
      rows,
      (row) => row.tx_cbor.byteLength + row.program_material_sidecar.byteLength,
    ))
      yield* sql`INSERT INTO node_import_checks ${sql.insert(batch)}
        ON CONFLICT (header_hash, tx_id) DO NOTHING`;
    const outputs = [...block.outputs].map(([outRef, output]) => ({
      header_hash: header,
      out_ref: Buffer.from(outRef, "hex"),
      output,
    }));
    for (const batch of batches(outputs, (row) => row.output.byteLength))
      yield* sql`INSERT INTO node_import_check_outputs ${sql.insert(batch)}
        ON CONFLICT (header_hash, out_ref) DO NOTHING`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to record import checks"));

/** Removes every entry of the blocks `headerHashes` (their removal). */
export const deleteChecksOfBlocks = (headerHashes: readonly string[]) =>
  Effect.gen(function* () {
    if (headerHashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`DELETE FROM node_import_check_blocks
      WHERE header_hash = ANY(${pg.array(hashBytes(headerHashes))}::bytea[])`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete import checks"));

/** Removes the rows of the blocks `headers` that have no entry left. */
const deleteEmptyBlocks = (headers: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (headers.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`DELETE FROM node_import_check_blocks b
      WHERE b.header_hash = ANY(${pg.array(hashBytes(headers.map(hexOf)))}::bytea[])
        AND NOT EXISTS (SELECT 1 FROM node_import_checks c
          WHERE c.header_hash = b.header_hash)`;
  });

/** Removes one entry (its local verdict). */
export const deleteCheck = (key: ImportCheckKey) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const header = Buffer.from(key.headerHash, "hex");
    yield* sql`DELETE FROM node_import_checks
      WHERE header_hash = ${header} AND tx_id = ${key.transactionId}`;
    yield* deleteEmptyBlocks([header]);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to delete an import check"),
  );

export const setCheckState = (key: ImportCheckKey, state: ImportCheckState) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE node_import_checks SET state = ${state}
      WHERE header_hash = ${Buffer.from(key.headerHash, "hex")}
        AND tx_id = ${key.transactionId}`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to update an import check"),
  );

/** The `(header_hash, tx_id)` pairs of `keys`, for an `unnest` join. */
const keyArrays = (pg: PgClient, keys: readonly ImportCheckKey[]) => ({
  headers: pg.array(hashBytes(keys.map((key) => key.headerHash))),
  txIds: pg.array(hashBytes(keys.map((key) => hexOf(key.transactionId)))),
});

/**
 * Removes and returns every entry whose deadline is at or before `nowMs`,
 * except those in `keep` (an entry a pass is running stays until its
 * verdict). `removed`: its block has left the landed chain (a `removed`
 * row).
 */
export const expireChecks = (
  nowMs: number,
  keep: readonly ImportCheckKey[] = [],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const kept = keyArrays(sql as PgClient, keep);
    const rows = yield* sql<{
      header_hash: Buffer;
      tx_id: Buffer;
      deadline_ms: string;
      state: ImportCheckState;
      removed: boolean;
    }>`WITH expired AS (
        DELETE FROM node_import_checks c WHERE c.deadline_ms <= ${nowMs}
          AND NOT EXISTS (SELECT 1 FROM unnest(${kept.headers}::bytea[],
              ${kept.txIds}::bytea[]) AS k(header_hash, tx_id)
            WHERE k.header_hash = c.header_hash AND k.tx_id = c.tx_id)
        RETURNING header_hash, tx_id, deadline_ms, state, block_position)
      SELECT e.header_hash, e.tx_id, e.deadline_ms, e.state,
        COALESCE(b.state = 'removed', FALSE) AS removed
      FROM expired e
      LEFT JOIN node_landed_blocks b ON b.header_hash = e.header_hash
      ORDER BY e.deadline_ms, e.header_hash, e.block_position`;
    yield* deleteEmptyBlocks([
      ...new Map(
        rows.map((row) => [hexOf(row.header_hash), row.header_hash]),
      ).values(),
    ]);
    return rows.map((row) => ({
      headerHash: hexOf(row.header_hash),
      transactionId: Buffer.from(row.tx_id),
      deadlineMs: Number(row.deadline_ms),
      state: row.state,
      removed: row.removed,
    }));
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to expire import checks"));

/** What the entries due at one deadline cost the checker: their count,
 * and their runs' count, declared cpu and bytes (script, context and the
 * sidecar once per run). */
export type ImportCheckLoad = Readonly<{
  deadlineMs: number;
  entries: number;
  runs: number;
  cpu: number;
  bytes: number;
}>;

/**
 * The load of the entries in `states` whose block is on the landed chain,
 * summed per deadline, earliest first (one row per deadline, whatever the
 * entries' number).
 */
export const retrieveCheckLoads = (states: readonly ImportCheckState[]) =>
  Effect.gen(function* () {
    if (states.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      deadline_ms: string;
      entries: string;
      runs: string;
      budget_cpu: string;
      run_bytes: string;
    }>`
      SELECT c.deadline_ms, COUNT(*) AS entries, SUM(c.runs) AS runs,
        SUM(c.budget_cpu) AS budget_cpu, SUM(c.run_bytes) AS run_bytes
      FROM node_import_checks c
      LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
      WHERE c.state IN ${sql.in(states)}
        AND (b.state IS NULL OR b.state <> 'removed')
      GROUP BY c.deadline_ms
      ORDER BY c.deadline_ms`;
    return rows.map(
      (row): ImportCheckLoad => ({
        deadlineMs: Number(row.deadline_ms),
        entries: Number(row.entries),
        runs: Number(row.runs),
        cpu: Number(row.budget_cpu),
        bytes: Number(row.run_bytes),
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to read import check loads"),
  );

/** The most entries, and stored bytes, one read of the set returns. */
export const READ_PAGE_ROWS = 64;
export const READ_PAGE_BYTES = 16 * 1024 * 1024;

export type RetrieveChecksOptions = Readonly<{
  /** Only this block's entries. */
  headerHash?: string;
  /** Only entries due after this time. */
  dueAfterMs?: number;
  /** Not these entries. */
  excluding?: readonly ImportCheckKey[];
  /** At most this many entries (default `READ_PAGE_ROWS`). */
  limit?: number;
}>;

/**
 * A page of the entries in `states`, earliest deadline first, then by
 * block and position, without their material: at most `limit` entries, and
 * stopped once the entries' stored bytes pass `READ_PAGE_BYTES` (at least
 * one entry). An entry whose block left the landed chain (a `removed` row
 * the rebase has not yet taken out) is not returned.
 */
export const retrieveChecks = (
  states: readonly ImportCheckState[],
  options: RetrieveChecksOptions = {},
) =>
  Effect.gen(function* () {
    if (states.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const excluded = keyArrays(sql as PgClient, options.excluding ?? []);
    const entries = yield* sql<{
      header_hash: Buffer;
      tx_id: Buffer;
      block_position: number;
      deadline_ms: string;
      state: ImportCheckState;
      script_hashes: Buffer;
      preceding_bytes: string;
    }>`
      SELECT * FROM (
        SELECT c.header_hash, c.tx_id, c.block_position, c.deadline_ms,
          c.state, c.script_hashes,
          SUM(octet_length(c.tx_cbor) + octet_length(c.program_material_sidecar))
            OVER (ORDER BY c.deadline_ms, c.header_hash, c.block_position
              ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
            AS preceding_bytes
        FROM node_import_checks c
        LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
        WHERE c.state IN ${sql.in(states)}
          AND (b.state IS NULL OR b.state <> 'removed')
          ${options.headerHash === undefined ? sql`` : sql`AND c.header_hash = ${Buffer.from(options.headerHash, "hex")}`}
          ${options.dueAfterMs === undefined ? sql`` : sql`AND c.deadline_ms > ${options.dueAfterMs}`}
          AND NOT EXISTS (SELECT 1 FROM unnest(${excluded.headers}::bytea[],
              ${excluded.txIds}::bytea[]) AS k(header_hash, tx_id)
            WHERE k.header_hash = c.header_hash AND k.tx_id = c.tx_id)
        ORDER BY c.deadline_ms, c.header_hash, c.block_position
        LIMIT ${options.limit ?? READ_PAGE_ROWS}) page
      WHERE COALESCE(preceding_bytes, 0) < ${READ_PAGE_BYTES}
      ORDER BY deadline_ms, header_hash, block_position`;
    return entries.map(
      (entry): ImportCheckEntry => ({
        headerHash: hexOf(entry.header_hash),
        transactionId: Buffer.from(entry.tx_id),
        position: entry.block_position,
        deadlineMs: Number(entry.deadline_ms),
        state: entry.state,
        scriptHashes: Array.from(
          { length: entry.script_hashes.byteLength / 32 },
          (_, index) =>
            Buffer.from(
              entry.script_hashes.subarray(32 * index, 32 * index + 32),
            ),
        ),
      }),
    );
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to read import checks"));

/**
 * The block of the earliest-due `pending` entry on the landed chain that is
 * due after `nowMs` and not in `excluding`.
 */
export const retrieveNextPendingBlock = (
  nowMs: number,
  excluding: readonly ImportCheckKey[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const excluded = keyArrays(sql as PgClient, excluding);
    const rows = yield* sql<{ header_hash: Buffer }>`
      SELECT c.header_hash FROM node_import_checks c
      LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
      WHERE c.state = 'pending' AND c.deadline_ms > ${nowMs}
        AND (b.state IS NULL OR b.state <> 'removed')
        AND NOT EXISTS (SELECT 1 FROM unnest(${excluded.headers}::bytea[],
            ${excluded.txIds}::bytea[]) AS k(header_hash, tx_id)
          WHERE k.header_hash = c.header_hash AND k.tx_id = c.tx_id)
      ORDER BY c.deadline_ms, c.header_hash
      LIMIT 1`;
    return rows[0] === undefined ? undefined : hexOf(rows[0].header_hash);
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to read import checks"));

/** Whether `key` is still in the set and its block on the landed chain. */
export const isCheckHeld = (key: ImportCheckKey) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ held: number }>`
      SELECT 1 AS held FROM node_import_checks c
      LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
      WHERE c.header_hash = ${Buffer.from(key.headerHash, "hex")}
        AND c.tx_id = ${key.transactionId}
        AND (b.state IS NULL OR b.state <> 'removed')`;
    return rows.length > 0;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to read import checks"));

/**
 * Marks every `pending` entry of `headerHash` except those in `excluding`
 * as `cap_skipped`; returns how many.
 */
export const skipPendingChecks = (
  headerHash: string,
  excluding: readonly ImportCheckKey[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const excluded = keyArrays(sql as PgClient, excluding);
    const rows = yield* sql<{ tx_id: Buffer }>`
      UPDATE node_import_checks c SET state = 'cap_skipped'
      WHERE c.header_hash = ${Buffer.from(headerHash, "hex")}
        AND c.state = 'pending'
        AND NOT EXISTS (SELECT 1 FROM unnest(${excluded.headers}::bytea[],
            ${excluded.txIds}::bytea[]) AS k(header_hash, tx_id)
          WHERE k.header_hash = c.header_hash AND k.tx_id = c.tx_id)
      RETURNING c.tx_id`;
    return rows.length;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to update import checks"));

/**
 * What `key`'s runs are rebuilt from, or undefined when the entry has left
 * the set. Outputs the entry's transaction does not name are not read.
 */
export const retrieveCheckMaterial = (
  key: ImportCheckKey,
  outRefs: (transactionCbor: Buffer) => readonly Buffer[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const header = Buffer.from(key.headerHash, "hex");
    const [row] = yield* sql<{
      tx_cbor: Buffer;
      program_material_sidecar: Buffer;
      block_time_ms: string;
      block_slot: string;
      network_id: string;
      min_fee_a: string;
      min_fee_b: string;
      slot_zero_time: string;
      slot_zero_slot: string;
      slot_length: string;
    }>`
      SELECT c.tx_cbor, c.program_material_sidecar, b.block_time_ms,
        b.block_slot, b.network_id, b.min_fee_a, b.min_fee_b,
        b.slot_zero_time, b.slot_zero_slot, b.slot_length
      FROM node_import_checks c
      JOIN node_import_check_blocks b ON b.header_hash = c.header_hash
      WHERE c.header_hash = ${header} AND c.tx_id = ${key.transactionId}`;
    if (row === undefined) return undefined;
    const transactionCbor = Buffer.from(row.tx_cbor);
    const wanted = outRefs(transactionCbor);
    const outputs =
      wanted.length === 0
        ? []
        : yield* sql<{ out_ref: Buffer; output: Buffer }>`
          SELECT out_ref, output FROM node_import_check_outputs
          WHERE header_hash = ${header}
            AND out_ref = ANY(${pg.array(hashBytes(wanted.map(hexOf)))}::bytea[])`;
    return {
      transactionCbor,
      programMaterialSidecarCbor: Buffer.from(row.program_material_sidecar),
      context: {
        blockTimeMs: Number(row.block_time_ms),
        blockSlot: BigInt(row.block_slot),
        expectedNetworkId: BigInt(row.network_id),
        minFeeA: BigInt(row.min_fee_a),
        minFeeB: BigInt(row.min_fee_b),
        l1SlotConfig: {
          zeroTime: Number(row.slot_zero_time),
          zeroSlot: Number(row.slot_zero_slot),
          slotLength: Number(row.slot_length),
        },
      },
      outputs: outputs.map((output) => ({
        outRef: Buffer.from(output.out_ref),
        output: Buffer.from(output.output),
      })),
    } satisfies ImportCheckMaterial;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to read an import check"));
