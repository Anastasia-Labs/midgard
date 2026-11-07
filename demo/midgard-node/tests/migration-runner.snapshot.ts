import { SqlClient } from "@effect/sql";
import { Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import * as MigrationRunner from "../src/database/migrations/runner.js";
import {
  retiredStorageShape,
  withPredecessor,
} from "./migration-runner.flat-scripts.js";

// An in-flight transaction can outlive a lost runtime lease. The table guard
// must see its commit after waiting, even though migration metadata was read.
describe("stored Flat migration snapshot", () => {
  it("refuses state committed while waiting for its table lock", async () => {
    await Effect.runPromise(
      withPredecessor(
        Effect.scoped(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const ledger =
              yield* sql`SELECT * FROM schema_migrations ORDER BY version`;
            const storage = yield* retiredStorageShape;
            const writer = yield* sql.reserve;
            yield* writer.execute("BEGIN", [], undefined);
            yield* Effect.addFinalizer(() =>
              writer.execute("ROLLBACK", [], undefined).pipe(Effect.orDie),
            );
            yield* writer.execute(
              "LOCK TABLE public.node_deployment IN ROW EXCLUSIVE MODE",
              [],
              undefined,
            );
            const migration = yield* Effect.forkScoped(
              Effect.either(
                MigrationRunner.migrate({
                  appVersion: "flat-test",
                  actor: "flat-snapshot-test",
                }),
              ),
            );
            // Observe the actual blocked guard instead of guessing a sleep interval.
            yield* Effect.gen(function* () {
              for (;;) {
                const [row] = yield* sql<{ waiting: boolean }>`SELECT EXISTS (
            SELECT 1 FROM pg_locks WHERE database = (SELECT oid FROM pg_database WHERE datname = current_database())
              AND relation = 'public.node_deployment'::regclass AND mode = 'AccessExclusiveLock' AND NOT granted
          ) AS waiting`;
                if (row?.waiting) return;
                yield* Effect.sleep("10 millis");
              }
            }).pipe(
              Effect.timeoutFail({
                duration: "5 seconds",
                onTimeout: () =>
                  new Error(
                    "Migration never waited for the retained-state table lock",
                  ),
              }),
            );
            const deploymentId = "51".repeat(32);
            yield* writer.execute(
              "INSERT INTO node_deployment (deployment_id) VALUES ($1)",
              [deploymentId],
              undefined,
            );
            yield* writer.execute("COMMIT", [], undefined);
            const result = yield* Fiber.join(migration);
            expect(result).toMatchObject({
              _tag: "Left",
              left: {
                code: "schema_migration_requires_fresh_database",
                message:
                  "Migration 29_drop_cek_program_material refused this database: Flat-script migration needs a fresh node database: node_deployment retains state. Flat scripts start from a fresh genesis. Point the node at a new, empty database and run db:migrate there.",
              },
            });
            expect(
              yield* sql`SELECT deployment_id FROM node_deployment`,
            ).toEqual([{ deployment_id: deploymentId }]);
            expect(
              yield* sql`SELECT * FROM schema_migrations ORDER BY version`,
            ).toEqual(ledger);
            expect(yield* retiredStorageShape).toEqual(storage);
          }),
        ),
      ),
    );
  });
});
