import type { DatabaseSync, StatementSync } from "node:sqlite";

import { sha256Hex } from "./watcher-journal-database.codec.js";
import { WatcherJournalIntegrityError } from "./watcher-journal-database.types.js";
import { WATCHER_JOURNAL_SCHEMA_SQL } from "./watcher-journal-schema.js";

const SCHEMA_CHECKSUM = sha256Hex(WATCHER_JOURNAL_SCHEMA_SQL);

/** Creates the journals' schema in an empty file. A file created by another
 * schema text is an integrity failure: it holds the watcher unready by name
 * until an operator recreates it, rather than a transient open failure
 * retried forever. */
export const ensureWatcherJournalSchema = (
  database: DatabaseSync,
  prepare: (sql: string) => StatementSync,
  inTransaction: <T>(begin: string, run: () => T) => T,
): void =>
  inTransaction("BEGIN IMMEDIATE", () => {
    const tables = prepare(
      "SELECT name FROM sqlite_master WHERE type = 'table'",
    ).all() as { name: string }[];
    if (tables.length === 0) {
      database.exec(WATCHER_JOURNAL_SCHEMA_SQL);
      prepare("INSERT INTO watcher_journal_schema (checksum) VALUES (?)").run(
        SCHEMA_CHECKSUM,
      );
      return;
    }
    const recorded = tables.some(
      ({ name }) => name === "watcher_journal_schema",
    )
      ? (
          prepare("SELECT checksum FROM watcher_journal_schema").get() as
            | { checksum: string }
            | undefined
        )?.checksum
      : undefined;
    if (recorded !== SCHEMA_CHECKSUM)
      throw new WatcherJournalIntegrityError(
        "schema",
        "the file was created by a different schema; recreate it (delete the journals file and start again)",
      );
  });
