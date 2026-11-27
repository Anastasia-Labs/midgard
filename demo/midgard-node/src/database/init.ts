import { Duration, Effect } from "effect";

import { isConnectionClassError } from "../provider-retry.js";
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
} from "../services/startup-waiting.js";
import * as NodeSchema from "./schema.js";
import { DatabaseError } from "./utils/common.js";

const SCHEMA_CHECK_KEY = "database_schema_check";

/**
 * Runs the schema step, waiting out a dropped or refused connection with
 * backoff for at most the database budget (15 min by default), the startup
 * waiting under `database_unreachable`. Any other failure (a schema refusal)
 * fails at once under `database_schema_incompatible`; a connection failure
 * past the budget fails under its waiting reason. Either way the
 * `DatabaseError` carries the step's `StartupStepFailedError`.
 */
export const ensureSchemaWithStartupRetry = <E, R>(
  ensure: Effect.Effect<void, E, R>,
  options: DatabaseStartupRetryOptions = DATABASE_STARTUP_RETRY,
): Effect.Effect<void, DatabaseError, R> =>
  retryStartupStep(ensure, {
    key: SCHEMA_CHECK_KEY,
    retryable: isConnectionClassError,
    reason: (error) =>
      isConnectionClassError(error)
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
          table: "<node_schema>",
        }),
    ),
  );

/**
 * Startup schema step for the long-running node: creates the schema on an
 * empty database, and refuses a database created by a different schema.
 */
export const program: Effect.Effect<void, DatabaseError, Database> =
  ensureSchemaWithStartupRetry(NodeSchema.ensure);
