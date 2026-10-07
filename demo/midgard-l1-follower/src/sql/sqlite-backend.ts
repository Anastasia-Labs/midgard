import { DatabaseSync, type StatementSync } from "node:sqlite";

import {
  type Dialect,
  RollbackWith,
  type SqlBackend,
  type SqlRow,
  type SqlTx,
  type SqlValue,
} from "./backend.js";

export const sqliteDialect: Dialect = {
  name: "sqlite",
  bool: (value) => (value ? 1 : 0),
  readBool: (value) => {
    if (value === 0 || value === 1) return value === 1;
    throw new Error("expected a 0/1 boolean column");
  },
  json: (text) => text,
  outRefList: (encoded) =>
    JSON.stringify(encoded.map((outRef) => outRef.toString("hex"))),
  readOutRefList: (value) => {
    if (typeof value !== "string")
      throw new Error("expected a JSON outref list");
    const parsed = JSON.parse(value) as unknown;
    if (!Array.isArray(parsed)) throw new Error("expected a JSON array");
    return parsed.map((item: unknown) => {
      if (typeof item !== "string") throw new Error("expected hex outrefs");
      return Buffer.from(item, "hex");
    });
  },
  lockClause: () => "",
  rowId: "rowid",
};

type SqliteParam = null | number | bigint | string | Uint8Array;

const toSqliteParam = (value: SqlValue): SqliteParam => {
  if (typeof value === "boolean") return value ? 1 : 0;
  if (Array.isArray(value))
    throw new Error("SQLite has no array parameters; use the dialect encoder");
  return value;
};

/** Serialises transactions on the one connection (node:sqlite is synchronous). */
class Mutex {
  private tail: Promise<void> = Promise.resolve();
  run<T>(task: () => Promise<T>): Promise<T> {
    const result = this.tail.then(task);
    this.tail = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  }
}

/**
 * The SQLite backend (watcher only, §18.1 Q1). One connection in WAL mode
 * with `synchronous = FULL` and foreign keys on. Writers take
 * `BEGIN IMMEDIATE`, which is the SQLite form of the cursor row lock.
 * The file must sit on a local disk: network filesystems break its locking.
 */
export const openSqliteBackend = (path: string): SqlBackend => {
  const database = new DatabaseSync(path);
  database.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA foreign_keys = ON;
    PRAGMA busy_timeout = 5000;
  `);
  const statements = new Map<string, StatementSync>();
  const prepare = (sql: string): StatementSync => {
    let statement = statements.get(sql);
    if (statement === undefined) {
      statement = database.prepare(sql);
      statements.set(sql, statement);
    }
    return statement;
  };
  const tx: SqlTx = {
    query: async (sql, params = []) =>
      prepare(sql).all(...params.map(toSqliteParam)) as SqlRow[],
    exec: async (sql) => {
      database.exec(sql);
    },
  };
  const mutex = new Mutex();
  return {
    dialect: sqliteDialect,
    transaction: <T>(mode: "write" | "read", run: (tx: SqlTx) => Promise<T>) =>
      mutex.run(async () => {
        database.exec(mode === "write" ? "BEGIN IMMEDIATE" : "BEGIN");
        try {
          const value = await run(tx);
          database.exec("COMMIT");
          return value;
        } catch (error) {
          // SQLite may already have rolled back (for example on SQLITE_FULL).
          if (database.isTransaction) database.exec("ROLLBACK");
          if (error instanceof RollbackWith) return error.value as T;
          throw error;
        }
      }),
    close: async () => {
      await mutex.run(async () => {
        statements.clear();
        database.close();
      });
    },
  };
};
