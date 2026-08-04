import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  EXPECTED_SCHEMA_VERSION,
  MIGRATION_MANIFEST_HASH,
} from "@/database/migrations/index.js";
import * as MigrationRunner from "@/database/migrations/runner.js";
import { AdmissionSql, BatchSql } from "@/services/database.js";

import { provideDatabaseLayers } from "./utils.js";

describe("schema migration transaction lock", () => {
  it("serializes two independent pools, applies once, and leaks no session state", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const batchSql = yield* BatchSql;
          const admissionSql = yield* AdmissionSql;
          yield* batchSql.unsafe(
            "DROP SCHEMA public CASCADE; CREATE SCHEMA public",
          );

          const attempts = yield* Effect.all(
            [
              MigrationRunner.migrate({
                appVersion: "migration-locking-test",
                actor: "batch-pool",
              }).pipe(Effect.provideService(SqlClient.SqlClient, batchSql)),
              MigrationRunner.migrate({
                appVersion: "migration-locking-test",
                actor: "admission-pool",
              }).pipe(Effect.provideService(SqlClient.SqlClient, admissionSql)),
            ].map(Effect.either),
            { concurrency: "unbounded" },
          );
          const winner = attempts.find((attempt) => attempt._tag === "Right");
          const contender = attempts.find((attempt) => attempt._tag === "Left");
          expect(winner?._tag).toBe("Right");
          expect(contender?._tag).toBe("Left");
          if (winner?._tag !== "Right" || contender?._tag !== "Left") {
            throw new Error("Expected one migration winner and one contender");
          }
          expect(contender.left.code).toBe("schema_migration_in_progress");

          const retryStatus = yield* MigrationRunner.migrate({
            appVersion: "migration-locking-test",
            actor: "contender-retry",
          }).pipe(Effect.provideService(SqlClient.SqlClient, admissionSql));

          expect(
            [winner.right, retryStatus].map((status) => ({
              actualVersion: status.actualVersion,
              compatible: status.compatible,
              manifestHash: status.manifestHash,
            })),
          ).toEqual([
            {
              actualVersion: EXPECTED_SCHEMA_VERSION,
              compatible: true,
              manifestHash: MIGRATION_MANIFEST_HASH,
            },
            {
              actualVersion: EXPECTED_SCHEMA_VERSION,
              compatible: true,
              manifestHash: MIGRATION_MANIFEST_HASH,
            },
          ]);

          const appliers = yield* batchSql<{
            readonly applied_by: string;
            readonly applied_count: string;
            readonly manifest_count: string;
          }>`SELECT
                applied_by,
                COUNT(*)::bigint AS applied_count,
                COUNT(DISTINCT manifest_hash_sha256)::bigint AS manifest_count
              FROM schema_migrations
              GROUP BY applied_by`;
          expect(appliers).toHaveLength(1);
          expect(Number(appliers[0]?.applied_count)).toBe(
            EXPECTED_SCHEMA_VERSION,
          );
          expect(Number(appliers[0]?.manifest_count)).toBe(1);
          expect(["batch-pool", "admission-pool"]).toContain(
            appliers[0]?.applied_by,
          );

          const events = yield* batchSql<{
            readonly event_type: string;
            readonly event_count: string;
          }>`SELECT event_type, COUNT(*)::bigint AS event_count
              FROM schema_migration_events
              GROUP BY event_type
              ORDER BY event_type`;
          expect(events).toEqual([
            {
              event_type: "started",
              event_count: EXPECTED_SCHEMA_VERSION.toString(),
            },
            {
              event_type: "succeeded",
              event_count: EXPECTED_SCHEMA_VERSION.toString(),
            },
          ]);

          const remainingLocks = yield* batchSql<{
            readonly lock_count: string;
          }>`SELECT COUNT(*)::bigint AS lock_count
              FROM pg_locks
              WHERE locktype = 'advisory'
                AND pid IN (
                  SELECT pid
                  FROM pg_stat_activity
                  WHERE datname = current_database()
                )`;
          expect(remainingLocks[0]?.lock_count).toBe("0");

          const readSessionDefaults = (sql: SqlClient.SqlClient) =>
            sql<{
              readonly transaction_isolation: string;
              readonly lock_timeout: string;
              readonly statement_timeout: string;
              readonly default_tablespace: string;
              readonly default_table_access_method: string;
            }>`SELECT
                  current_setting('transaction_isolation') AS transaction_isolation,
                  current_setting('lock_timeout') AS lock_timeout,
                  current_setting('statement_timeout') AS statement_timeout,
                  current_setting('default_tablespace') AS default_tablespace,
                  current_setting('default_table_access_method') AS default_table_access_method`;
          const defaults = yield* Effect.all(
            [readSessionDefaults(batchSql), readSessionDefaults(admissionSql)],
            { concurrency: "unbounded" },
          );
          for (const rows of defaults) {
            expect(rows[0]).toEqual({
              transaction_isolation: "read committed",
              lock_timeout: "0",
              statement_timeout: "0",
              default_tablespace: "",
              default_table_access_method: "heap",
            });
          }
        }),
      ),
    );
  }, 30_000);
});
