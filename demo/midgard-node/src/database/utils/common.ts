import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { Data } from "effect";
import { SqlClient, SqlError } from "@effect/sql";

export const clearTable = (
  tableName: string,
): Effect.Effect<void, DeleteError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear table`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;

    yield* Effect.logInfo(`${tableName} db: Successfully cleared table`);
  }).pipe(
    Effect.withLogSpan(`clear ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: clearing error: ${JSON.stringify(e)}`),
    ),
    mapDeleteError(tableName),
  );

type DatabaseErrorFields = {
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}

export class SelectError extends Data.TaggedError("SelectError")<DatabaseErrorFields> {}
export class InsertError extends Data.TaggedError("InsertError")<DatabaseErrorFields> {}
export class UpdateError extends Data.TaggedError("UpdateError")<DatabaseErrorFields> {}
export class DeleteError extends Data.TaggedError("DeleteError")<DatabaseErrorFields> {}
export class CreateTableError extends Data.TaggedError("CreateTableError")<DatabaseErrorFields> {}
export class OtherDatabaseError extends Data.TaggedError("OtherDatabaseError")<DatabaseErrorFields> {}

type DatabaseErrorConstructor<T> = new (args: DatabaseErrorFields) => T

function createMapError(
  ErrorClass: DatabaseErrorConstructor<any>,
  action: string,
) {
  return (tableName: string) =>
    Effect.mapError((error: unknown) =>
      error instanceof SqlError.SqlError
        ? new ErrorClass({
            message: `Failed to ${action}`,
            table: tableName,
            cause: error,
          })
        : new ErrorClass({
            message: `Unknown error during ${action}`,
            table: tableName,
            cause: error,
          }),
    )
}

/**
 * Helper function to map SQL errors to DatabaseError for select operations
 * @param tableName - Name of the table being queried
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapSelectError = createMapError(SelectError, "select from table")

/**
 * Helper function to map SQL errors to DatabaseError for insert operations
 * @param tableName - Name of the table being inserted into
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapInsertError = createMapError(InsertError, "insert into table")

/**
 * Helper function to map SQL errors to DatabaseError for update operations
 * @param tableName - Name of the table being updated
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapUpdateError = createMapError(UpdateError, "update table")

/**
 * Helper function to map SQL errors to DatabaseError for delete operations
 * @param tableName - Name of the table being deleted from
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapDeleteError = createMapError(DeleteError, "delete from table")

/**
 * Helper function to map SQL errors to DatabaseError for createTable operations
 * @param tableName - Name of the table being created
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapCreateTableError = createMapError(CreateTableError, "create table")