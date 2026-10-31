/**
 * The per-file database reset (`resetApplicationTables`) empties every table
 * but its named bookkeeping, a table no list names included. Each fork-pool
 * worker runs its files on one database and every emulator file restarts
 * its chain from the same genesis, so a table a hand list missed carried the
 * previous file's facts into the next one (a forced order left unspent
 * capped every later file's commit horizon in the past).
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  provideDatabaseLayers,
  RESET_KEPT_TABLES,
  resetApplicationTables,
} from "./utils.js";

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect) as Effect.Effect<A, E>);

/** The singletons a migration seeds, which the reset restores. */
const SEEDED_SINGLETONS = new Set([
  "commit_build_calibration",
  "node_follower_write_gate",
]);

/** A table no migration and no reset names: created here, dropped after. */
const PROBE = "reset_guard_unlisted_probe";

/** Every table in the database's schemas with its row count. */
const tableCounts = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const tables = yield* sql<{ schema: string; name: string }>`SELECT
      schemaname AS schema, tablename AS name FROM pg_tables
    WHERE schemaname <> 'information_schema' AND left(schemaname, 3) <> 'pg_'`;
  const counts = new Map<string, number>();
  for (const { schema, name } of tables) {
    const [row] = yield* sql.unsafe<{ count: string }>(
      `SELECT count(*)::text AS count FROM "${schema}"."${name}"`,
    );
    counts.set(name, Number(row!.count));
  }
  return counts;
});

describe("per-file database reset", () => {
  it("empties every table but the named bookkeeping, unlisted and follower tables included", async () => {
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.unsafe(
          `CREATE TABLE IF NOT EXISTS ${PROBE} (id integer PRIMARY KEY)`,
        );
        yield* sql.unsafe(`INSERT INTO ${PROBE} (id) VALUES (1)`);
        // A migrated table the old hand list never named: the commit
        // horizon reads it.
        yield* sql`INSERT INTO follower_event_ingestion
            (id, generation, slot, block_hash, height, ingested_through_ms, updated_at)
          VALUES (true, 7, 70, ${Buffer.alloc(32, 7)}, 7, 70000, NOW())
          ON CONFLICT (id) DO NOTHING`;
      }),
    );
    try {
      const before = await run(tableCounts);
      expect(before.get(PROBE)).toBe(1);
      expect(before.get("follower_event_ingestion")).toBe(1);

      await run(resetApplicationTables);

      const after = await run(tableCounts);
      const leftover = [...after].filter(
        ([name, count]) =>
          !RESET_KEPT_TABLES.has(name) &&
          // The migration seed rows the reset restores.
          count !== (SEEDED_SINGLETONS.has(name) ? 1 : 0),
      );
      expect(leftover).toEqual([]);
      expect(after.get(PROBE)).toBe(0);
      // The kept bookkeeping is untouched: the migration ledgers and the
      // follower's writer fence row.
      for (const kept of RESET_KEPT_TABLES.keys())
        expect([kept, after.get(kept)]).toEqual([kept, before.get(kept)]);
      expect(after.get("schema_migrations")).toBeGreaterThan(0);
      expect(after.get("l1_follower_writer")).toBe(1);
    } finally {
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql.unsafe(`DROP TABLE IF EXISTS ${PROBE}`);
        }),
      );
    }
  });
});
