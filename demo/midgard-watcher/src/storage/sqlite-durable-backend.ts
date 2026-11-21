import { mkdir, realpath, stat } from "node:fs/promises";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

export const WATCHER_SQLITE_DURABLE_BACKEND_SCHEMA_VERSION =
  "midgard-watcher-sqlite-durable-backend-v1" as const;

const canonicalDatabasePath = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error("watcher SQLite backend requires a canonical durable path");
  }
  return value;
};

export type WatcherSqliteDurableBackend = Readonly<{
  schemaVersion: typeof WATCHER_SQLITE_DURABLE_BACKEND_SCHEMA_VERSION;
  close(): void;
}>;

/**
 * Opens the watcher's production SQLite file at startup: it refuses a
 * non-durable or symlinked path and sets the file's journal mode before the
 * stores that keep their tables in it open.
 */
export const openWatcherSqliteDurableBackend = async (input: {
  readonly path: string;
  readonly busyTimeoutMs?: number;
}): Promise<WatcherSqliteDurableBackend> => {
  const path = canonicalDatabasePath(input.path);
  const directory = dirname(path);
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if ((await realpath(directory)) !== directory) {
    throw new Error("watcher SQLite directory traverses a symlink");
  }
  try {
    if (
      (await stat(path)).isSymbolicLink() ||
      (await realpath(path)) !== path
    ) {
      throw new Error("watcher SQLite path traverses a symlink");
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const busyTimeoutMs = input.busyTimeoutMs ?? 5_000;
  if (
    !Number.isSafeInteger(busyTimeoutMs) ||
    busyTimeoutMs < 1 ||
    busyTimeoutMs > 120_000
  ) {
    throw new Error("watcher SQLite busy timeout is invalid");
  }

  const database = new DatabaseSync(path, {
    open: true,
    readOnly: false,
    enableForeignKeyConstraints: true,
  });
  database.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA trusted_schema = OFF;
    PRAGMA busy_timeout = ${busyTimeoutMs.toString()};
  `);

  return Object.freeze({
    schemaVersion: WATCHER_SQLITE_DURABLE_BACKEND_SCHEMA_VERSION,
    close: () => database.close(),
  });
};
