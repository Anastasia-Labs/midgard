import { openSqliteBackend } from "./sql/sqlite-backend.js";
import {
  createFactStore,
  type FactStore,
  type FactStoreOptions,
} from "./store/fact-store.js";

/**
 * The SQLite fact store, for the watcher only (§18.1 Q1). `path` is a file
 * on a local disk, or `":memory:"` for tests.
 */
export const openSqliteFactStore = (
  options: FactStoreOptions & Readonly<{ path: string }>,
): FactStore => createFactStore(openSqliteBackend(options.path), options);
