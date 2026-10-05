import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Duration, Effect, Schedule } from "effect";

import { hasCauseCode, isConnectionClassError } from "../provider-retry.js";
// Imported from the module itself, not the services barrel: a value import
// of the barrel here closes an import cycle through the database tables.
import {
  type Database,
  DATABASE_STARTUP_RETRY,
  type DatabaseStartupRetryOptions,
} from "../services/database.js";
import * as MigrationRunner from "./migrations/runner.js";
import { DatabaseError } from "./utils/common.js";

/**
 * A compatibility check that could not run yet: the server dropped or
 * refused the connection, a `db:migrate` holds the schema lock, or a lock
 * wait timed out. A schema verdict (not migrated, unversioned, a ledger or
 * shape mismatch) is never one.
 */
export const isTransientSchemaCheckFailure = (
  error: MigrationRunner.MigrationError,
): boolean =>
  error.code === "schema_migration_in_progress" ||
  isConnectionClassError(error) ||
  hasCauseCode(error, "55P03"); // lock_not_available

/**
 * Runs `assertCompatible`, waiting out transient failures with backoff over
 * the startup budget and logging the unready reason; a schema verdict fails
 * at once.
 */
export const assertCompatibleWithStartupRetry = <R>(
  assertCompatible: Effect.Effect<void, MigrationRunner.MigrationError, R>,
  options: DatabaseStartupRetryOptions = DATABASE_STARTUP_RETRY,
): Effect.Effect<void, DatabaseError, R> =>
  assertCompatible.pipe(
    Effect.tapError((error) =>
      isTransientSchemaCheckFailure(error)
        ? Effect.logWarning(
            `Database unready: reason=${
              error.code === "schema_migration_in_progress"
                ? "schema_migration_in_progress"
                : "database_unreachable"
            }; the schema compatibility check waits and re-runs. cause=${formatUnknownError(error, { includeCause: true })}`,
          )
        : Effect.void,
    ),
    Effect.retry({
      schedule: Schedule.exponential(options.baseDelay).pipe(
        Schedule.union(Schedule.spaced(options.maxDelay)),
        Schedule.upTo(Duration.decode(options.budget)),
      ),
      while: isTransientSchemaCheckFailure,
    }),
    Effect.mapError(
      (error) =>
        new DatabaseError({
          message: `Database schema is not compatible: ${error.message}`,
          cause: error,
          table: "<schema_migrations>",
        }),
    ),
  );

/**
 * Startup schema gate for the long-running node.
 *
 * Production startup must not create, alter, or repair application tables. The
 * node only verifies that explicit migrations have already brought the database
 * to the exact schema version supported by this binary.
 */
export const program: Effect.Effect<void, DatabaseError, Database> =
  assertCompatibleWithStartupRetry(MigrationRunner.assertCompatible);
