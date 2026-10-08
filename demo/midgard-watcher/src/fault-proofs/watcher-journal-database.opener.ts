import {
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
} from "./watcher-journal-database.types.js";

// Backoff of a background reopen: 1 s doubling to 30 s.
const REOPEN_BASE_MS = 1_000;
const REOPEN_MAX_MS = 30_000;

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
 * `retryInBackground`, an open that could not complete is also retried by a
 * timer, backing off from 1 s to 30 s, so readiness recovers without traffic.
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
    const delay = Math.min(
      REOPEN_MAX_MS,
      REOPEN_BASE_MS * 2 ** Math.min(attempts, 5),
    );
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
          reopenLater();
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
