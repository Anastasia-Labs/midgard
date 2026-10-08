import { createHash } from "node:crypto";

import { createWatcherJournalDatabase } from "./watcher-journal-database.create-database.js";
import {
  forgetWatcherJournalRefusal,
  watcherJournalRefusal,
} from "./watcher-journal-database.refusal.js";
import {
  isWatcherJournalConfigurationError,
  isWatcherJournalIntegrityError,
  WatcherJournalConfigurationError,
  type WatcherJournalDatabase,
  WatcherJournalUnavailableError,
} from "./watcher-journal-database.types.js";

export { watcherJournalIntegrityFailure } from "./watcher-journal-database.refusal.js";
export {
  isWatcherJournalCapacityError,
  isWatcherJournalConfigurationError,
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
  WATCHER_JOURNAL_DATABASE_FILE,
  WatcherJournalCapacityError,
  WatcherJournalConfigurationError,
  type WatcherJournalDatabase,
  type WatcherJournalHead,
  WatcherJournalIntegrityError,
  type WatcherJournalRow,
  type WatcherJournalTransaction,
  WatcherJournalUnavailableError,
} from "./watcher-journal-database.types.js";

/**
 * The watcher's fault-proof journals as per-row SQLite tables (plan §3.4 WC1,
 * WC4; ticket W2). A commit writes only the rows it changes, so a persist is
 * O(delta), never a rewrite or rescan of the journal.
 *
 * Integrity, checked in full at startup:
 * - each row carries a MAC over its journal, key, scope, state, revision and
 *   body, so an edited row or a row moved to another revision is refused;
 * - each journal's head carries a MAC over its revision, its chained revision
 *   digest, its live row count and a keyed sum of its rows' MACs, so a
 *   deleted, added or replayed row is refused;
 * - the latest revisions keep their chained digest and delta, so a reordered
 *   or substituted revision is refused.
 * A whole-file rollback to an older consistent copy is not detectable without
 * an external anchor; the journals never claimed that.
 *
 * A failed check refuses the journals for the rest of the process: every
 * later call, and every later open of the same root, throws the first
 * failure without touching the file again. The watcher stays up and reports
 * the failure as the readiness reason `journal_integrity`; only a restart
 * after an operator repairs the journals clears it.
 *
 * An open that fails for any other reason (a busy, locked or unreadable
 * file) is not latched: it throws `WatcherJournalUnavailableError`, readiness
 * reports `journal_unavailable`, and the next use opens the file again. A
 * wrong directory, or a key the journals were not written under, throws
 * `WatcherJournalConfigurationError`, which startup refuses before binding.
 *
 * The file must sit on a local disk. Network filesystems break SQLite's
 * locking and its WAL.
 */

const opened = new Map<
  string,
  Readonly<{ keyId: string; database: WatcherJournalDatabase }>
>();

/** Each root's last failed open, while no open has succeeded since. */
const unavailable = new Map<string, string>();

/** Why the journals under `journalRoot` could not be opened at the last
 * attempt, or null once an open succeeds. */
export const watcherJournalUnavailable = (journalRoot: string): string | null =>
  unavailable.get(journalRoot) ?? null;

/**
 * The process's one connection to the journals under `journalRoot`. The
 * first open migrates and verifies every journal in full; later opens share
 * that connection and must present the same key. A failed open throws an
 * integrity error (latched), a configuration error, or
 * `WatcherJournalUnavailableError`, which the next open retries.
 */
export const openWatcherJournalDatabase = (input: {
  readonly journalRoot: string;
  readonly authenticationKey: Uint8Array;
}): WatcherJournalDatabase => {
  if (input.authenticationKey.byteLength !== 32)
    throw new WatcherJournalConfigurationError(
      "the journal authentication key is not 32 bytes",
    );
  const keyId = createHash("sha256")
    .update(input.authenticationKey)
    .digest("hex");
  const root = input.journalRoot;
  const { guard, refuse } = watcherJournalRefusal(root);
  const existing = opened.get(root);
  if (existing !== undefined) {
    if (existing.keyId !== keyId)
      throw new WatcherJournalConfigurationError(
        "the journals are already open under another key",
      );
    return guard(() => existing.database)(); // a refused root throws
  }
  let database: ReturnType<typeof createWatcherJournalDatabase>;
  try {
    database = guard(createWatcherJournalDatabase)(
      root,
      input.authenticationKey,
      refuse,
    );
  } catch (error) {
    if (
      isWatcherJournalIntegrityError(error) ||
      isWatcherJournalConfigurationError(error)
    )
      throw error;
    const failure = new WatcherJournalUnavailableError(error);
    unavailable.set(root, failure.message);
    throw failure;
  }
  unavailable.delete(root);
  const shared: WatcherJournalDatabase = Object.freeze({
    path: database.path,
    transaction: guard(database.transaction),
    row: guard(database.row),
    rows: guard(database.rows),
    count: guard(database.count),
    head: guard(database.head),
    verify: guard(database.verify),
    refuse,
    close: () => {
      opened.delete(root);
      database.close();
    },
  });
  opened.set(root, { keyId, database: shared });
  return shared;
};

/** Closes the process's connection and forgets its refusal, as a process
 * exit would. */
export const closeWatcherJournalDatabase = (journalRoot: string): void => {
  opened.get(journalRoot)?.database.close();
  unavailable.delete(journalRoot);
  forgetWatcherJournalRefusal(journalRoot);
};
