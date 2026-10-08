import type { DatabaseSync, StatementSync } from "node:sqlite";

import { sha256Hex } from "./watcher-journal-database.codec.js";
import {
  WATCHER_JOURNAL_LEDGER_SQL,
  WATCHER_JOURNAL_MIGRATIONS,
} from "./watcher-journal-schema.js";

/** Applies each journal migration once, refusing one whose SQL changed after
 * it was applied. */
export const migrateWatcherJournals = (
  database: DatabaseSync,
  prepare: (sql: string) => StatementSync,
  inTransaction: <T>(begin: string, run: () => T) => T,
): void =>
  inTransaction("BEGIN IMMEDIATE", () => {
    database.exec(WATCHER_JOURNAL_LEDGER_SQL);
    for (const migration of WATCHER_JOURNAL_MIGRATIONS.migrations) {
      const checksum = sha256Hex(migration.sql);
      const applied = prepare(
        "SELECT checksum FROM watcher_journal_migrations WHERE namespace = ? AND id = ?",
      ).get(WATCHER_JOURNAL_MIGRATIONS.namespace, migration.id) as
        | { checksum: string }
        | undefined;
      if (applied !== undefined) {
        if (applied.checksum !== checksum)
          throw new Error(
            `watcher journal migration ${migration.id} changed after it was applied`,
          );
        continue;
      }
      database.exec(migration.sql);
      prepare(
        "INSERT INTO watcher_journal_migrations (namespace, id, checksum) VALUES (?, ?, ?)",
      ).run(WATCHER_JOURNAL_MIGRATIONS.namespace, migration.id, checksum);
    }
  });
