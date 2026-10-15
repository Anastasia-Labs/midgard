/**
 * The import checker's durable set (migration 0028): each Plutus
 * transaction of an imported foreign block, applied by its committed
 * verdict, with the script runs the import did not evaluate. An entry is
 * written in the transaction that processes its block, and leaves the set
 * on an accepting local verdict, its block's removal, or its deadline.
 *
 * - `pending`: not yet run by the checker;
 * - `cap_skipped`: not run, the block's checker time was spent (backlog);
 * - `cache_skipped`: not run, a script of it is in the negative cache
 *   (backlog);
 * - `faulted`: a run faulted twice, the second time on a fresh instance;
 * - `rejected`: the local evaluator rejected a run. It stays, for a
 *   challenge, until its deadline or its block's removal.
 */
import { type MidgardCekVerdictRequest } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import type { ImportScriptCheck } from "../mpf/verified-block-import.js";
import { sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "node_import_checks";
export const runsTableName = "node_import_check_runs";

export type ImportCheckState =
  | "pending"
  | "cap_skipped"
  | "cache_skipped"
  | "faulted"
  | "rejected";

/** The states the checker's backlog lane serves. */
export const BACKLOG_STATES = [
  "cap_skipped",
  "cache_skipped",
] as const satisfies readonly ImportCheckState[];

/** The states that have no local verdict yet and wait for a run. */
export const UNRUN_STATES = [
  "pending",
  ...BACKLOG_STATES,
] as const satisfies readonly ImportCheckState[];

export type ImportCheckKey = Readonly<{
  headerHash: string;
  transactionId: Buffer;
}>;

export type ImportCheckEntry = ImportCheckKey &
  Readonly<{
    position: number;
    deadlineMs: number;
    state: ImportCheckState;
    runs: readonly MidgardCekVerdictRequest[];
  }>;

const hashBytes = (hashes: readonly string[]) =>
  hashes.map((hash) => `\\x${hash}`);

/**
 * Records a processed block's checks, in the caller's transaction. A block
 * processed again (it landed again after its row was deleted) keeps the
 * entries it still has.
 */
export const recordChecks = (
  headerHash: string,
  deadlineMs: number,
  checks: readonly ImportScriptCheck[],
) =>
  Effect.gen(function* () {
    if (checks.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const header = Buffer.from(headerHash, "hex");
    for (const check of checks) {
      const inserted = yield* sql<{ tx_id: Buffer }>`
        INSERT INTO node_import_checks (header_hash, tx_id, block_position,
          deadline_ms, state, program_material_sidecar)
        VALUES (${header}, ${check.transactionId}, ${check.position},
          ${deadlineMs}, 'pending',
          ${sidecarOf(check.runs)})
        ON CONFLICT (header_hash, tx_id) DO NOTHING
        RETURNING tx_id`;
      if (inserted.length === 0) continue;
      for (const [runIndex, run] of check.runs.entries()) {
        yield* sql`INSERT INTO node_import_check_runs (header_hash, tx_id,
            run_index, execution_index, script_bytes, context_cbor,
            budget_cpu, budget_memory, max_steps)
          VALUES (${header}, ${check.transactionId}, ${runIndex},
            ${run.executionIndex.toString()},
            ${Buffer.from(run.scriptBytes)}, ${Buffer.from(run.contextCbor)},
            ${run.executionBudget?.cpu.toString() ?? null},
            ${run.executionBudget?.memory.toString() ?? null},
            ${run.maxSteps.toString()})`;
      }
    }
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to record import checks"));

/** One transaction's runs share its program-material sidecar. */
const sidecarOf = (runs: readonly MidgardCekVerdictRequest[]) => {
  const sidecar = runs[0]?.programMaterialSidecarCbor ?? null;
  return sidecar === null ? null : Buffer.from(sidecar);
};

/** Removes every entry of the blocks `headerHashes` (their removal). */
export const deleteChecksOfBlocks = (headerHashes: readonly string[]) =>
  Effect.gen(function* () {
    if (headerHashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`DELETE FROM node_import_checks
      WHERE header_hash = ANY(${pg.array(hashBytes(headerHashes))}::bytea[])`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete import checks"));

/** Removes one entry (its local verdict). */
export const deleteCheck = (key: ImportCheckKey) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM node_import_checks
      WHERE header_hash = ${Buffer.from(key.headerHash, "hex")}
        AND tx_id = ${key.transactionId}`;
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

/**
 * Removes and returns every entry whose deadline is at or before `nowMs`.
 * `removed`: its block has left the landed chain (a `removed` row).
 */
export const expireChecks = (nowMs: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      header_hash: Buffer;
      tx_id: Buffer;
      deadline_ms: string;
      state: ImportCheckState;
      removed: boolean;
    }>`WITH expired AS (
        DELETE FROM node_import_checks WHERE deadline_ms <= ${nowMs}
        RETURNING header_hash, tx_id, deadline_ms, state, block_position)
      SELECT e.header_hash, e.tx_id, e.deadline_ms, e.state,
        COALESCE(b.state = 'removed', FALSE) AS removed
      FROM expired e
      LEFT JOIN node_landed_blocks b ON b.header_hash = e.header_hash
      ORDER BY e.deadline_ms, e.header_hash, e.block_position`;
    return rows.map((row) => ({
      headerHash: Buffer.from(row.header_hash).toString("hex"),
      transactionId: Buffer.from(row.tx_id),
      deadlineMs: Number(row.deadline_ms),
      state: row.state,
      removed: row.removed,
    }));
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to expire import checks"));

/** What one entry's runs cost the checker: their count, declared cpu and
 * bytes (script, context and the sidecar once per run). */
export type ImportCheckLoad = Readonly<{
  deadlineMs: number;
  runs: number;
  cpu: number;
  bytes: number;
}>;

/**
 * The load of every entry in `states` whose block is on the landed chain,
 * earliest deadline first, without reading the runs' bytes.
 */
export const retrieveCheckLoads = (states: readonly ImportCheckState[]) =>
  Effect.gen(function* () {
    if (states.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      deadline_ms: string;
      runs: string;
      cpu: string | null;
      bytes: string | null;
    }>`
      SELECT c.deadline_ms, COUNT(r.run_index) AS runs,
        SUM(COALESCE(r.budget_cpu, 0)) AS cpu,
        SUM(octet_length(r.script_bytes) + octet_length(r.context_cbor)
          + COALESCE(octet_length(c.program_material_sidecar), 0)) AS bytes
      FROM node_import_checks c
      JOIN node_import_check_runs r
        ON r.header_hash = c.header_hash AND r.tx_id = c.tx_id
      LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
      WHERE c.state IN ${sql.in(states)}
        AND (b.state IS NULL OR b.state <> 'removed')
      GROUP BY c.header_hash, c.tx_id, c.deadline_ms
      ORDER BY c.deadline_ms`;
    return rows.map(
      (row): ImportCheckLoad => ({
        deadlineMs: Number(row.deadline_ms),
        runs: Number(row.runs),
        cpu: Number(row.cpu ?? 0),
        bytes: Number(row.bytes ?? 0),
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to read import check loads"),
  );

type RawEntry = {
  header_hash: Buffer;
  tx_id: Buffer;
  block_position: number;
  deadline_ms: string;
  state: ImportCheckState;
  program_material_sidecar: Buffer | null;
};

type RawRun = {
  header_hash: Buffer;
  tx_id: Buffer;
  execution_index: string;
  script_bytes: Buffer;
  context_cbor: Buffer;
  budget_cpu: string | null;
  budget_memory: string | null;
  max_steps: string;
};

const hexOf = (bytes: Buffer) => Buffer.from(bytes).toString("hex");

const keyOf = (headerHash: Buffer, txId: Buffer) =>
  `${hexOf(headerHash)}:${hexOf(txId)}`;

/**
 * The entries in `states`, earliest deadline first, then by block and
 * position, at most `limit` of them. An entry whose block left the landed
 * chain (a `removed` row the rebase has not yet taken out) is not returned.
 */
export const retrieveChecks = (
  states: readonly ImportCheckState[],
  limit?: number,
) =>
  Effect.gen(function* () {
    if (states.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const entries = yield* sql<RawEntry>`
      SELECT c.header_hash, c.tx_id, c.block_position, c.deadline_ms, c.state,
        c.program_material_sidecar
      FROM node_import_checks c
      LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
      WHERE c.state IN ${sql.in(states)}
        AND (b.state IS NULL OR b.state <> 'removed')
      ORDER BY c.deadline_ms, c.header_hash, c.block_position
      ${limit === undefined ? sql`` : sql`LIMIT ${limit}`}`;
    if (entries.length === 0) return [];
    const runs = yield* sql<RawRun>`
      SELECT r.header_hash, r.tx_id, r.execution_index, r.script_bytes,
        r.context_cbor, r.budget_cpu, r.budget_memory, r.max_steps
      FROM node_import_check_runs r
      JOIN unnest(
          ${pg.array(hashBytes(entries.map((entry) => hexOf(entry.header_hash))))}::bytea[],
          ${pg.array(hashBytes(entries.map((entry) => hexOf(entry.tx_id))))}::bytea[]
        ) AS k(header_hash, tx_id)
        ON k.header_hash = r.header_hash AND k.tx_id = r.tx_id
      ORDER BY r.header_hash, r.tx_id, r.run_index`;
    const runsByKey = new Map<string, RawRun[]>();
    for (const run of runs) {
      const key = keyOf(run.header_hash, run.tx_id);
      runsByKey.set(key, [...(runsByKey.get(key) ?? []), run]);
    }
    return entries.map((entry): ImportCheckEntry => {
      const sidecar =
        entry.program_material_sidecar === null
          ? null
          : Buffer.from(entry.program_material_sidecar);
      return {
        headerHash: Buffer.from(entry.header_hash).toString("hex"),
        transactionId: Buffer.from(entry.tx_id),
        position: entry.block_position,
        deadlineMs: Number(entry.deadline_ms),
        state: entry.state,
        runs: (runsByKey.get(keyOf(entry.header_hash, entry.tx_id)) ?? []).map(
          (run) => ({
            scriptBytes: Buffer.from(run.script_bytes),
            contextCbor: Buffer.from(run.context_cbor),
            ...(run.budget_cpu === null || run.budget_memory === null
              ? {}
              : {
                  executionBudget: {
                    cpu: BigInt(run.budget_cpu),
                    memory: BigInt(run.budget_memory),
                  },
                }),
            executionIndex: BigInt(run.execution_index),
            maxSteps: Number(run.max_steps),
            programMaterialSidecarCbor: sidecar,
          }),
        ),
      };
    });
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to read import checks"));
