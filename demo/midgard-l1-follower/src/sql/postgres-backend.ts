import pg from "pg";

import {
  type Dialect,
  RollbackWith,
  type SqlBackend,
  type SqlRow,
  type SqlTx,
  type SqlValue,
  toNumberedPlaceholders,
} from "./backend.js";

export const postgresDialect: Dialect = {
  name: "postgres",
  bool: (value) => value,
  readBool: (value) => {
    if (typeof value !== "boolean")
      throw new Error("expected a boolean column");
    return value;
  },
  json: (text) => text,
  outRefList: (encoded) => [...encoded],
  readOutRefList: (value) => {
    if (!Array.isArray(value)) throw new Error("expected a bytea[] column");
    return value.map((item: unknown) => {
      if (!Buffer.isBuffer(item)) throw new Error("expected bytea elements");
      return item;
    });
  },
  lockClause: (mode) => (mode === "update" ? " FOR UPDATE" : " FOR SHARE"),
  rowId: "ctid",
};

const executor = (client: pg.PoolClient): SqlTx => ({
  query: async (sql, params = []) => {
    const result = await client.query<SqlRow>(
      toNumberedPlaceholders(sql),
      params as SqlValue[],
    );
    return result.rows;
  },
  exec: async (sql) => {
    await client.query(sql);
  },
});

export type PostgresConnection =
  /** A pool the caller owns; `close()` leaves it open. */
  | { pool: pg.Pool }
  /** A connection string; the backend owns and closes its pool. */
  | { connectionString: string; maxConnections?: number };

/**
 * The Postgres backend. Write transactions are plain `BEGIN`: the follower
 * serialises writers on the cursor row (`SELECT … FOR UPDATE`, §7.1). Read
 * transactions are `REPEATABLE READ READ ONLY` snapshots.
 */
export const openPostgresBackend = (
  connection: PostgresConnection,
): SqlBackend => {
  const owned = !("pool" in connection);
  const pool =
    "pool" in connection
      ? connection.pool
      : new pg.Pool({
          connectionString: connection.connectionString,
          max: connection.maxConnections ?? 4,
        });
  return {
    dialect: postgresDialect,
    transaction: async <T>(
      mode: "write" | "read",
      run: (tx: SqlTx) => Promise<T>,
    ): Promise<T> => {
      const client = await pool.connect();
      let released = false;
      try {
        await client.query(
          mode === "read"
            ? "BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY"
            : "BEGIN",
        );
        const value = await run(executor(client));
        await client.query("COMMIT");
        return value;
      } catch (error) {
        try {
          await client.query("ROLLBACK");
        } catch (rollbackError) {
          // A connection that cannot roll back is discarded, not reused.
          client.release(rollbackError instanceof Error ? rollbackError : true);
          released = true;
        }
        if (error instanceof RollbackWith) return error.value as T;
        throw error;
      } finally {
        if (!released) client.release();
      }
    },
    close: async () => {
      if (owned) await pool.end();
    },
  };
};
