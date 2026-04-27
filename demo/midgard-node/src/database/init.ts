import * as MigrationRunner from "@/database/migrations/runner.js";
import { Effect } from "effect";
import { Database } from "@/services/index.js";
import { DatabaseError } from "./utils/common.js";

/**
 * Startup schema gate for the long-running node.
 *
 * Production startup must not create, alter, or repair application tables. The
 * node only verifies that explicit migrations have already brought the database
 * to the exact schema version supported by this binary.
 */
export const program: Effect.Effect<void, DatabaseError, Database> =
  MigrationRunner.assertCompatible.pipe(
    Effect.mapError(
      (error) =>
        new DatabaseError({
          message: `Database schema is not compatible: ${error.message}`,
          cause: error,
          table: "<schema_migrations>",
        }),
    ),
  );
