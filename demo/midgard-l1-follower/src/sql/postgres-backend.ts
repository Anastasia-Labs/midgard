import { DEFAULT_REQUEST_TIMEOUT_MS } from "@al-ft/l1-node-transport";
import pg from "pg";

import {
  type Dialect,
  RollbackWith,
  type SqlBackend,
  type SqlRow,
  type SqlTx,
  type SqlValue,
  toNumberedPlaceholders,
  type TransactionOptions,
  type WriterLease,
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

/**
 * A pooled connection, or the signal's reason once it fires: a wait for a
 * free connection ends at the abort, and a connection that arrives after
 * it goes straight back to the pool.
 */
const connectUnlessAborted = (
  pool: pg.Pool,
  signal: AbortSignal | undefined,
): Promise<pg.PoolClient> => {
  if (signal === undefined) return pool.connect();
  signal.throwIfAborted();
  return new Promise((resolve, reject) => {
    const aborted = () => reject(signal.reason as Error);
    signal.addEventListener("abort", aborted, { once: true });
    pool.connect().then(
      (client) => {
        signal.removeEventListener("abort", aborted);
        if (signal.aborted) client.release();
        else resolve(client);
      },
      (error: Error) => {
        signal.removeEventListener("abort", aborted);
        reject(error);
      },
    );
  });
};

export type PostgresConnection = (
  | {
      /**
       * A pool the caller owns; `close()` leaves it open. The caller
       * attaches the pool's `'error'` listener.
       */
      pool: pg.Pool;
    }
  /**
   * A connection string; the backend owns and closes its pool. Its
   * connections send TCP keepalives, so one whose peer vanished without a
   * word fails, on the operating system's keepalive timing.
   */
  | {
      connectionString: string;
      maxConnections?: number;
      /**
       * Told about a connection Postgres dropped (a restart, a failover, an
       * idle reaper). Nothing else is needed: the pool discards the client
       * and the next query opens a new connection or fails like any other.
       */
      onConnectionError?: (error: Error) => void;
    }
) & {
  /**
   * Bound on a read given no abort signal (ms), a write transaction that
   * only reads among them (`TransactionOptions.onlyReads`): past it the
   * read ends as an abort ends it (`TransactionOptions`), so a connection
   * that never answers holds no pooled connection longer. The local node
   * transport's request bound by default: a store read is one request to
   * the follower's other local dependency.
   */
  readDeadlineMs?: number;
};

/** A signal that fires `ms` from now, and a way to stop its timer. */
const readDeadline = (ms: number) => {
  const controller = new AbortController();
  const timer = setTimeout(() => {
    controller.abort(
      new Error(`the read had no answer within ${String(ms)} ms`),
    );
  }, ms);
  return { signal: controller.signal, clear: () => clearTimeout(timer) };
};

/**
 * The writer lease key, as a SQL expression: one per database and schema.
 * The committee's instance lock takes it on its own session beside its own
 * key (`da-committee-node/src/store/postgres.instance-lock.ts`), so that
 * process's follower and its store are held and lost together.
 */
export const POSTGRES_WRITER_LEASE_KEY_SQL = `('x' || left(md5(
    'midgard-l1-follower:writer:' || coalesce(current_schema(), '')
  ), 15))::bit(60)::bigint`;

const WRITER_LEASE_SQL = `SELECT pg_try_advisory_lock(${POSTGRES_WRITER_LEASE_KEY_SQL}) AS acquired`;

/**
 * Takes the writer lease on a dedicated connection outside the pool, so it
 * never costs the store a pooled connection and lives exactly as long as
 * that session: a dropped connection or a dead process releases it.
 */
const acquirePostgresLease = async (
  config: pg.ClientConfig,
): Promise<WriterLease | null> => {
  const client = new pg.Client({ ...config, keepAlive: true });
  let lost = false;
  // An unhandled 'error' on an idle client would crash the process.
  client.on("error", () => {
    lost = true;
  });
  client.on("end", () => {
    lost = true;
  });
  try {
    await client.connect();
    const rows = await client.query<{ acquired: boolean }>(WRITER_LEASE_SQL);
    if (rows.rows[0]?.acquired !== true) {
      await client.end();
      return null;
    }
  } catch (error) {
    await client.end().catch(() => undefined);
    throw error;
  }
  let released = false;
  return {
    lost: () => lost && !released,
    release: async () => {
      if (released) return;
      released = true;
      // Ending the session releases its advisory lock.
      await client.end().catch(() => undefined);
    },
  };
};

/**
 * The Postgres backend. Write transactions are plain `BEGIN`: the follower
 * serialises writers on the cursor row (`SELECT … FOR UPDATE`, §7.1). Read
 * transactions are `REPEATABLE READ READ ONLY` snapshots.
 */
export const openPostgresBackend = (
  connection: PostgresConnection,
): SqlBackend => {
  const owned = !("pool" in connection);
  const readDeadlineMs =
    connection.readDeadlineMs ?? DEFAULT_REQUEST_TIMEOUT_MS;
  if (!Number.isSafeInteger(readDeadlineMs) || readDeadlineMs < 1)
    throw new RangeError("readDeadlineMs must be a positive integer");
  const onConnectionError =
    "pool" in connection
      ? () => undefined
      : (connection.onConnectionError ?? (() => undefined));
  let pool: pg.Pool;
  if ("pool" in connection) pool = connection.pool;
  else {
    pool = new pg.Pool({
      connectionString: connection.connectionString,
      max: connection.maxConnections ?? 4,
      keepAlive: true,
    });
    // pg re-emits an idle client's error on its pool; with no listener it
    // is an uncaught exception that exits the process.
    pool.on("error", onConnectionError);
  }
  return {
    dialect: postgresDialect,
    transaction: async <T>(
      mode: "write" | "read",
      run: (tx: SqlTx) => Promise<T>,
      options: TransactionOptions = {},
    ): Promise<T> => {
      // A real write is not bounded so: one the server is still applying
      // is progress, and one cut short at its `COMMIT` has an unknown
      // outcome. TCP keepalive, on the operating system's timing, ends one
      // whose peer vanished. A write that only reads is a read here.
      const deadline =
        options.signal === undefined &&
        (mode === "read" || options.onlyReads === true)
          ? readDeadline(readDeadlineMs)
          : undefined;
      const signal = options.signal ?? deadline?.signal;
      let client: pg.PoolClient;
      try {
        client = await connectUnlessAborted(pool, signal);
      } catch (error) {
        deadline?.clear();
        throw error;
      }
      // While checked out the pool's idle listener is detached, so a dropped
      // connection's 'error' needs one here; the pending query rejects.
      client.on("error", onConnectionError);
      let released = false;
      /** Hands the connection back to be closed, not reused; once only. */
      const discard = (reason: Error | true) => {
        if (released) return;
        released = true;
        client.release(reason);
      };
      // An abort ends the session: the pool closes a connection released
      // with an error, and pg destroys the socket of a client with a
      // statement in flight, so that statement rejects now even when the
      // server or the network never answers it.
      const abandon = () => {
        discard(new Error("the transaction was aborted"));
      };
      signal?.addEventListener("abort", abandon, { once: true });
      // An abort between the connection's arrival and here fired with no
      // listener to hear it.
      if (signal?.aborted === true) abandon();
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
        if (!released)
          try {
            await client.query("ROLLBACK");
          } catch (rollbackError) {
            // A connection that cannot roll back is discarded, not reused.
            discard(rollbackError instanceof Error ? rollbackError : true);
          }
        if (signal?.aborted === true) throw signal.reason;
        if (error instanceof RollbackWith) return error.value as T;
        throw error;
      } finally {
        deadline?.clear();
        signal?.removeEventListener("abort", abandon);
        client.off("error", onConnectionError);
        if (!released) client.release();
      }
    },
    acquireWriterLease: () =>
      acquirePostgresLease(
        "pool" in connection
          ? connection.pool.options
          : { connectionString: connection.connectionString },
      ),
    close: async () => {
      if (owned) await pool.end();
    },
  };
};
