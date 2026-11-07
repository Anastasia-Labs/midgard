import { classifyFailure } from "@al-ft/midgard-l1-follower";

import {
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
} from "./watcher-journal-database.types.js";

// Backoff of a background reopen: 1 s doubling to 30 s.
const REOPEN_BASE_MS = 1_000;
const REOPEN_MAX_MS = 30_000;

/** The journals' retry backoff after `attempts` earlier retries: 1 s
 * doubling to 30 s. */
export const watcherJournalRetryDelayMs = (attempts: number): number =>
  Math.min(REOPEN_MAX_MS, REOPEN_BASE_MS * 2 ** Math.min(attempts, 5));

/**
 * Whether a journal failure, or one it wraps through `cause`, is one a later
 * attempt can clear on its own: SQLite busy or locked, or an errno the
 * follower classifies as transient (`classifyFailure`). Anything else (a
 * file SQLite cannot open, a full disk, a permission) waits for an operator,
 * so no timer retries it.
 */
export const isTransientJournalFailure = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (classifyFailure(current) === "transient") return true;
    current = current.cause;
  }
  return false;
};

export type WatcherJournalOpener<T> = Readonly<{
  /** The memoized open; a journal failure is retried by the next call. */
  open(): Promise<T>;
  /** Stops a pending background reopen. */
  close(): void;
}>;

/**
 * Memoizes an open of the journals. A journal failure is not cached, so the
 * next use opens again: an open that could not complete
 * (`WatcherJournalUnavailableError`) may succeed, and an integrity failure
 * stays refused through the journals' own latch, which every later open
 * throws before it reads the file. Any other failure is kept. With
 * `retryInBackground`, an open that could not complete for a transient
 * reason (`isTransientJournalFailure`) is also retried by a timer, backing
 * off from 1 s to 30 s, so readiness recovers without traffic; one that
 * failed for any other reason is opened again only by the next use.
 */
export const watcherJournalOpener = <T>(
  open: () => Promise<T>,
  options: Readonly<{ retryInBackground: boolean }>,
): WatcherJournalOpener<T> => {
  let opening: Promise<T> | undefined;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let attempts = 0;
  let closed = false;
  const reopenLater = (): void => {
    if (!options.retryInBackground || closed || timer !== undefined) return;
    const delay = watcherJournalRetryDelayMs(attempts);
    attempts += 1;
    timer = setTimeout(() => {
      timer = undefined;
      void memoized().catch(() => undefined); // readiness reports it
    }, delay);
    timer.unref();
  };
  const memoized = (): Promise<T> =>
    (opening ??= open().then(
      (value) => {
        attempts = 0;
        return value;
      },
      (error: unknown) => {
        if (isWatcherJournalUnavailableError(error)) {
          opening = undefined;
          if (isTransientJournalFailure(error)) reopenLater();
        } else if (isWatcherJournalIntegrityError(error)) opening = undefined;
        throw error;
      },
    ));
  return Object.freeze({
    open: memoized,
    close: () => {
      closed = true;
      if (timer !== undefined) clearTimeout(timer);
      timer = undefined;
    },
  });
};
