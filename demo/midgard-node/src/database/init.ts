import { Duration, Effect } from "effect";

import { hasCauseCode, isConnectionClassError } from "../provider-retry.js";
// Imported from the module itself, not the services barrel: a value import
// of the barrel here closes an import cycle through the database tables.
import {
  type Database,
  DATABASE_STARTUP_RETRY,
  type DatabaseStartupRetryOptions,
} from "../services/database.js";
import {
  DATABASE_SCHEMA_INCOMPATIBLE,
  DATABASE_UNREACHABLE,
  retryStartupStep,
  SCHEMA_MIGRATION_IN_PROGRESS,
} from "../services/startup-waiting.js";
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

const SCHEMA_CHECK_KEY = "database_schema_check";

/**
 * Runs `assertCompatible`, waiting out transient failures
 * (`isTransientSchemaCheckFailure`) with backoff for at most the database
 * budget (15 min by default), the startup waiting under
 * `schema_migration_in_progress` or `database_unreachable`. A schema verdict
 * fails at once under `database_schema_incompatible`; a transient failure
 * past the budget fails under its waiting reason. Either way the
 * `DatabaseError` carries the step's `StartupStepFailedError`.
 */
export const assertCompatibleWithStartupRetry = <R>(
  assertCompatible: Effect.Effect<void, MigrationRunner.MigrationError, R>,
  options: DatabaseStartupRetryOptions = DATABASE_STARTUP_RETRY,
): Effect.Effect<void, DatabaseError, R> =>
  retryStartupStep(assertCompatible, {
    key: SCHEMA_CHECK_KEY,
    retryable: isTransientSchemaCheckFailure,
    reason: (error) =>
      error.code === SCHEMA_MIGRATION_IN_PROGRESS
        ? SCHEMA_MIGRATION_IN_PROGRESS
        : isTransientSchemaCheckFailure(error)
          ? DATABASE_UNREACHABLE
          : DATABASE_SCHEMA_INCOMPATIBLE,
    budget: { maxElapsed: options.budget },
    initialMs: Duration.toMillis(Duration.decode(options.baseDelay)),
    maxMs: Duration.toMillis(Duration.decode(options.maxDelay)),
  }).pipe(
    Effect.mapError(
      (failure) =>
        new DatabaseError({
          message: failure.message,
          cause: failure,
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
