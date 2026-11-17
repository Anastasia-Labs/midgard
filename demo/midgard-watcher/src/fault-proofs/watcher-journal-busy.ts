import {
  isTransientJournalFailure,
  watcherJournalRetryDelayMs,
} from "./watcher-journal-database.opener.js";

export type WatcherJournalBusyHold = Readonly<{
  /** The busy failure the supervisor holds on, or null. */
  reason(): string | null;
  /** Holds on `error` and schedules one requeue after the backoff. */
  hold(error: Error): void;
  /** A journal write committed: the next hold backs off from 1 s again. */
  committed(): void;
  close(): void;
}>;

/**
 * The supervisor's `journal_busy` hold. A held supervisor starts no work.
 * After the journals' retry backoff (1 s doubling to 30 s) it runs
 * `requeue`, which rebuilds the supervisor's work from durable state as a
 * fresh process would, then clears the hold and calls `resumed`. A requeue
 * that meets a busy database again (`isTransientJournalFailure`) throws it
 * and holds once more, on the backoff. A requeue that fails for any other
 * reason keeps the hold, named with that failure, and is not retried on a
 * timer: the next busy failure the supervisor meets holds it again.
 */
export const createWatcherJournalBusyHold = (input: {
  readonly requeue: () => Promise<void>;
  readonly resumed: () => void;
}): WatcherJournalBusyHold => {
  let reason: string | null = null;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let requeueing = false;
  let attempts = 0;
  let closed = false;
  const schedule = (): void => {
    if (closed || requeueing || timer !== undefined) return;
    timer = setTimeout(() => void fire(), watcherJournalRetryDelayMs(attempts));
    attempts += 1;
    timer.unref();
  };
  const fire = async (): Promise<void> => {
    timer = undefined;
    requeueing = true;
    let transient = false;
    try {
      await input.requeue();
      reason = null;
    } catch (error) {
      reason = error instanceof Error ? error.message : String(error);
      transient = isTransientJournalFailure(error);
    } finally {
      requeueing = false;
    }
    if (reason === null) {
      if (!closed) input.resumed();
    } else if (transient) schedule();
  };
  return Object.freeze({
    reason: () => reason,
    hold: (error) => {
      reason = error.message;
      schedule();
    },
    committed: () => {
      attempts = 0;
    },
    close: () => {
      closed = true;
      if (timer !== undefined) clearTimeout(timer);
      timer = undefined;
    },
  });
};
