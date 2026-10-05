import { DatabaseSync } from "node:sqlite";

/** SQLite owns kernel locks on a persistent local sidecar; death releases them.
 * Removing/replacing this file or using a filesystem without reliable SQLite
 * locking violates the JSON backend's supported storage boundary. */
export class JsonStoreProcessMutex {
  private closed = false;
  private constructor(private readonly database: DatabaseSync) {}

  static acquire(path: string): JsonStoreProcessMutex {
    const database = new DatabaseSync(path);
    try {
      database.exec(`
        PRAGMA busy_timeout = 0;
        PRAGMA synchronous = FULL;
        CREATE TABLE IF NOT EXISTS json_store_process_mutex (singleton INTEGER PRIMARY KEY);
        BEGIN IMMEDIATE;
      `);
      return new JsonStoreProcessMutex(database);
    } catch (error) {
      database.close();
      throw error;
    }
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    try {
      this.database.exec("ROLLBACK");
    } finally {
      this.database.close();
    }
  }
}

export const isSqliteMutexBusy = (error: unknown): boolean =>
  error instanceof Error &&
  "errcode" in error &&
  (error.errcode === 5 || error.errcode === 6);
