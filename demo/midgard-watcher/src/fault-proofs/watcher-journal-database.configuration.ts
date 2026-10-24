import { mkdirSync, realpathSync } from "node:fs";
import { isAbsolute, normalize } from "node:path";
import type { StatementSync } from "node:sqlite";

import {
  sameHex,
  type WatcherJournalCodec,
} from "./watcher-journal-database.codec.js";
import {
  type StoredRow,
  WatcherJournalConfigurationError,
} from "./watcher-journal-database.types.js";
import {
  WATCHER_JOURNAL_TABLES,
  WATCHER_JOURNALS,
} from "./watcher-journal-schema.js";

/**
 * Creates the journals' directory, refusing one that is not canonical, sits
 * under /tmp, cannot be created, or traverses a symlink: bad configuration,
 * which startup refuses before the operations server binds.
 */
export const prepareWatcherJournalDirectory = (journalRoot: string): string => {
  if (
    journalRoot.trim() !== journalRoot ||
    !isAbsolute(journalRoot) ||
    normalize(journalRoot) !== journalRoot ||
    journalRoot === "/" ||
    journalRoot === "/tmp" ||
    journalRoot.startsWith("/tmp/")
  )
    throw new WatcherJournalConfigurationError(
      "the journals require a canonical durable directory",
    );
  let resolved: string;
  try {
    mkdirSync(journalRoot, { recursive: true, mode: 0o700 });
    resolved = realpathSync(journalRoot);
  } catch (error) {
    throw new WatcherJournalConfigurationError(
      `the journal directory cannot be created: ${
        error instanceof Error ? error.message : String(error)
      }`,
    );
  }
  if (resolved !== journalRoot)
    throw new WatcherJournalConfigurationError(
      "the journal directory traverses a symlink",
    );
  return journalRoot;
};

/**
 * A wrong key, told from corruption: every head names one other key and no
 * stored row carries a MAC under this key.
 */
export const watcherJournalsWrittenUnderAnotherKey = (
  prepare: (sql: string) => StatementSync,
  codec: WatcherJournalCodec,
): boolean => {
  const keys = new Set(
    (
      prepare("SELECT key_id FROM watcher_journal_heads").all() as {
        key_id: string;
      }[]
    ).map(({ key_id }) => key_id),
  );
  if (keys.size !== 1 || keys.has(codec.keyId)) return false;
  return !WATCHER_JOURNALS.some((journal) => {
    const stored = prepare(
      `SELECT row_key, scope, state, revision, body, mac FROM ${WATCHER_JOURNAL_TABLES[journal]} LIMIT 1`,
    ).get() as StoredRow | undefined;
    return (
      stored !== undefined &&
      sameHex(
        stored.mac,
        codec.rowMac(journal, { ...stored, revision: Number(stored.revision) }),
      )
    );
  });
};
