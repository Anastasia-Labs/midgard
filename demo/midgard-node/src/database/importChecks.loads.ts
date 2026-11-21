/**
 * What the import checker's durable set (`importChecks.ts`) costs the
 * checker to run, per deadline: the sweep's projected completion
 * (`checker.ts`, `projectUnverified`) reads it.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { type ImportCheckState, tableName } from "./importChecks.js";
import { sqlErrorToDatabaseError } from "./utils/common.js";

/** What the entries due at one deadline cost the checker: their count,
 * and their runs' count, declared cpu and bytes (script and context per
 * run); `retryAtMs`, the latest retry time among them (`timed_out` ones). */
export type ImportCheckLoad = Readonly<{
  deadlineMs: number;
  entries: number;
  runs: number;
  cpu: number;
  bytes: number;
  retryAtMs?: number;
}>;

/**
 * The load of the entries in `states` whose block is on the landed chain
 * and that can still run (a `timed_out` entry whose retry is not past its
 * deadline), summed per deadline, earliest first (one row per deadline,
 * whatever the entries' number).
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
      retry_at_ms: string | null;
    }>`
      SELECT c.deadline_ms, COUNT(*) AS entries, SUM(c.runs) AS runs,
        SUM(c.budget_cpu) AS budget_cpu, SUM(c.run_bytes) AS run_bytes,
        MAX(c.retry_at_ms) FILTER (WHERE c.state = 'timed_out') AS retry_at_ms
      FROM node_import_checks c
      LEFT JOIN node_landed_blocks b ON b.header_hash = c.header_hash
      WHERE c.state IN ${sql.in(states)}
        AND (c.state <> 'timed_out' OR c.retry_at_ms < c.deadline_ms)
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
        ...(row.retry_at_ms === null
          ? {}
          : { retryAtMs: Number(row.retry_at_ms) }),
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to read import check loads"),
  );
