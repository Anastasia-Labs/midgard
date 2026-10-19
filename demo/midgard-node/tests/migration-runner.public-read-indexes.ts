import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { WithdrawalsDB } from "../src/database/index.js";
import {
  MIGRATION_MANIFEST_HASH,
  MIGRATIONS,
} from "../src/database/migrations/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { BatchSql } from "../src/services/database.js";
import { makeHistoryWithdrawalEntry } from "./database.test/fixtures.make-history-withdrawal-entry.js";
import { insertWithdrawals } from "./helpers/event-rows.js";
import { testDatabaseName } from "./test-env.js";
import {
  deterministicFixtureOutputReferenceId,
  provideDatabaseLayers,
} from "./utils.js";

describe("populated public-read migration", () => {
  it("rolls back failed public-read DDL, then preserves its upgrade when Flat refuses retained state", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* BatchSql;
          const [database] = yield* sql<{
            name: string;
          }>`SELECT current_database() AS name`;
          expect(database!.name).toBe(testDatabaseName());
          expect(database!.name).toMatch(/^midgard_(?:contrib|test)_/u);
          yield* sql.unsafe("DROP SCHEMA public CASCADE; CREATE SCHEMA public");
          const withSql = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
            effect.pipe(Effect.provideService(SqlClient.SqlClient, sql));
          yield* withSql(MigrationRunner.getStatus);
          yield* sql.withTransaction(
            Effect.gen(function* () {
              for (const migration of MIGRATIONS.filter(
                (entry) => entry.version <= 26,
              )) {
                for (const statement of MigrationRunner.splitSqlStatements(
                  migration.sql,
                ))
                  yield* sql.unsafe(statement);
                yield* sql`INSERT INTO schema_migrations (version, name, checksum_sha256, manifest_hash_sha256, app_version, execution_ms, applied_by)
            VALUES (${migration.version}, ${migration.name}, ${migration.checksumSha256}, ${MIGRATION_MANIFEST_HASH}, 'r7-predecessor', 1, 'r7-test')`;
              }
            }),
          );
          const rows = [
            WithdrawalsDB.Status.Awaiting,
            WithdrawalsDB.Status.Projected,
            WithdrawalsDB.Status.Finalized,
          ].map((status) => ({
            ...makeHistoryWithdrawalEntry(),
            status,
            ...(status === WithdrawalsDB.Status.Awaiting
              ? {}
              : {
                  validity: WithdrawalsDB.Validity.WithdrawalIsValid,
                  settlement_event_info: Buffer.alloc(96, 0x41),
                  projected_header_hash: Buffer.alloc(28, 0x42),
                }),
            withdrawal_l1_output_index: [
              WithdrawalsDB.Status.Awaiting,
              WithdrawalsDB.Status.Projected,
              WithdrawalsDB.Status.Finalized,
            ].indexOf(status),
            event_id: deterministicFixtureOutputReferenceId(
              `r7-migration-${status}`,
            ),
          }));
          yield* withSql(insertWithdrawals(rows));
          const before =
            yield* sql`SELECT * FROM withdrawal_utxos ORDER BY event_id`;
          // Fail after the new columns, constraint and first pending index were created.
          // The runner must roll all of those back while retaining its failure audit.
          yield* sql`CREATE INDEX idx_withdrawal_ledger_outref_unknown ON withdrawal_utxos (event_id)`;
          const failed = yield* Effect.either(
            withSql(
              MigrationRunner.migrate({
                appVersion: "r7-test",
                actor: "populated-rollback",
              }),
            ),
          );
          expect(failed._tag).toBe("Left");
          if (failed._tag === "Left")
            expect(failed.left).toMatchObject({
              code: "schema_migration_sql_failed",
              cause: {
                cause: {
                  code: "42P07",
                  message:
                    'relation "idx_withdrawal_ledger_outref_unknown" already exists',
                },
              },
            });
          expect(
            yield* sql`SELECT event_type FROM schema_migration_events WHERE version = 27 AND actor = 'populated-rollback' AND event_type = 'failed'`,
          ).toEqual([{ event_type: "failed" }]);
          expect(
            yield* sql`SELECT * FROM withdrawal_utxos ORDER BY event_id`,
          ).toEqual(before);
          expect(
            yield* sql`SELECT column_name FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'withdrawal_utxos' AND column_name IN ('l2_ledger_outref', 'l2_ledger_outref_state')`,
          ).toEqual([]);
          expect(
            yield* sql`SELECT version FROM schema_migrations WHERE version = 27`,
          ).toEqual([]);
          yield* sql`DROP INDEX idx_withdrawal_ledger_outref_unknown`;
          const migrated = yield* Effect.either(
            withSql(
              MigrationRunner.migrate({
                appVersion: "r7-test",
                actor: "populated-forward",
              }),
            ),
          );
          expect(migrated).toMatchObject({
            _tag: "Left",
            left: {
              code: "schema_migration_requires_fresh_database",
              message:
                "Migration 29_drop_cek_program_material refused this database: Flat-script migration needs a fresh node database: withdrawal_utxos retains state. Flat scripts start from a fresh genesis. Point the node at a new, empty database and run db:migrate there.",
            },
          });
          expect(
            yield* sql`SELECT version FROM schema_migrations WHERE version = 29`,
          ).toEqual([]);
          expect(
            yield* sql`SELECT version, event_type FROM schema_migration_events
            WHERE actor = 'populated-forward' AND event_type IN ('succeeded', 'failed') ORDER BY id`,
          ).toEqual([
            { version: 27, event_type: "succeeded" },
            { version: 28, event_type: "succeeded" },
            { version: 29, event_type: "failed" },
          ]);
          const after = yield* sql<
            Record<string, unknown>
          >`SELECT * FROM withdrawal_utxos ORDER BY event_id`;
          expect(
            after.map(
              ({ l2_ledger_outref, l2_ledger_outref_state, ...original }) => {
                expect(l2_ledger_outref).toBeNull();
                expect(l2_ledger_outref_state).toBe("unknown");
                return original;
              },
            ),
          ).toEqual(before);
          expect(
            yield* sql`SELECT version FROM schema_migrations WHERE version = 27`,
          ).toEqual([{ version: 27 }]);
        }),
      ),
    );
  });
});
