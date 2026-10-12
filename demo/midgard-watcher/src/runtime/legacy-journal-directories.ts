import { readdir } from "node:fs/promises";
import { join } from "node:path";

import { WATCHER_JOURNAL_DATABASE_FILE } from "../fault-proofs/watcher-journal-database.types.js";
import { WATCHER_PACKAGE_NAME } from "./scaffold.js";

/**
 * The file journals the per-row SQLite journals replaced (ticket W2). No
 * importer exists: a deployment starts its journals fresh, and a leftover
 * directory never blocks readiness.
 */
export const WATCHER_LEGACY_JOURNAL_DIRECTORIES = Object.freeze([
  "fault-decisions",
  "fault-proof-queue-v1",
  "fault-proof-completions-v1",
] as const);

export type WatcherLegacyJournalIgnored = Readonly<{
  event: "legacy_journal_ignored";
  path: string;
  detail: string;
}>;

const writeWarning = (warning: WatcherLegacyJournalIgnored): void => {
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "warn", ...warning })}\n`,
  );
};

/** Warns once per non-empty legacy journal directory under `journalRoot`. */
export const warnWatcherLegacyJournalDirectories = async (
  journalRoot: string,
  warn: (warning: WatcherLegacyJournalIgnored) => void = writeWarning,
): Promise<void> => {
  for (const name of WATCHER_LEGACY_JOURNAL_DIRECTORIES) {
    const path = join(journalRoot, name);
    // Advisory only: a missing or unreadable entry holds nothing to import.
    const entries = await readdir(path).catch(() => []);
    if (entries.length > 0)
      warn(
        Object.freeze({
          event: "legacy_journal_ignored",
          path,
          detail: `legacy file journal is ignored, never imported; the watcher journals live in ${WATCHER_JOURNAL_DATABASE_FILE}`,
        }),
      );
  }
};
