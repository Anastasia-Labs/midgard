import {
  isWatcherJournalIntegrityError,
  WatcherJournalIntegrityError,
} from "./watcher-journal-database.types.js";

/** Each journal root's first integrity failure in this process. */
const refused = new Map<string, WatcherJournalIntegrityError>();

// SQLite's primary result codes for a damaged file.
const SQLITE_CORRUPT = 11;
const SQLITE_NOTADB = 26;

const sqliteCorruption = (error: unknown): string | null => {
  if (typeof error !== "object" || error === null) return null;
  const { code, errcode, errstr } = error as {
    code?: unknown;
    errcode?: unknown;
    errstr?: unknown;
  };
  if (code !== "ERR_SQLITE_ERROR" || typeof errcode !== "number") return null;
  const primary = errcode & 0xff;
  return primary === SQLITE_CORRUPT || primary === SQLITE_NOTADB
    ? String(errstr)
    : null;
};

/** Records an integrity failure under its root; other errors pass through. */
const latch = (journalRoot: string, error: unknown): unknown => {
  const corruption = sqliteCorruption(error);
  const failure = isWatcherJournalIntegrityError(error)
    ? error
    : corruption === null
      ? undefined
      : new WatcherJournalIntegrityError("database", corruption);
  if (failure === undefined) return error;
  if (!refused.has(journalRoot)) refused.set(journalRoot, failure);
  return failure;
};

export type WatcherJournalRefuse = (journal: string, detail: string) => never;

/**
 * The refusal latch of the journals under `journalRoot`. `refuse` latches a
 * failed check; `guard` wraps a database call so it throws the latched
 * failure without touching the file, and latches an integrity error or a
 * SQLite corruption code it raises.
 */
export const watcherJournalRefusal = (journalRoot: string) => {
  const refuse: WatcherJournalRefuse = (journal, detail) => {
    throw latch(journalRoot, new WatcherJournalIntegrityError(journal, detail));
  };
  const guard =
    <A extends unknown[], R>(run: (...args: A) => R) =>
    (...args: A): R => {
      const failure = refused.get(journalRoot);
      if (failure !== undefined) throw failure;
      try {
        return run(...args);
      } catch (error) {
        throw latch(journalRoot, error);
      }
    };
  return Object.freeze({ refuse, guard });
};

/** The first integrity failure of the journals under `journalRoot` in this
 * process, or null while they verify. */
export const watcherJournalIntegrityFailure = (
  journalRoot: string,
): string | null => refused.get(journalRoot)?.message ?? null;

/** Forgets the root's refusal, as a process exit would. */
export const forgetWatcherJournalRefusal = (journalRoot: string): void => {
  refused.delete(journalRoot);
};
