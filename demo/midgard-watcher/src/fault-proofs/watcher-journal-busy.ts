import {
  isTransientJournalFailure,
  watcherJournalRetryDelayMs,
} from "./watcher-journal-database.opener.js";

/** node:sqlite primary result codes: SQLITE_BUSY, SQLITE_LOCKED. */
const BUSY_PRIMARY_CODES = new Set([5, 6]);
const MAX_CAUSE_DEPTH = 8;

/**
 * Whether `error`, or an error it wraps through `cause`, is a node:sqlite
 * SQLITE_BUSY or SQLITE_LOCKED (any extended code). Another connection held
 * the database past the busy timeout. The statement that met it committed
 * nothing, and running it again later can succeed. The supervisor holds its
 * work unready (`journal_busy`) and requeues it; this never fails the process.
 */
export const isWatcherSqliteBusyError = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < MAX_CAUSE_DEPTH; depth += 1) {
    if (!(current instanceof Error)) return false;
    const { code, errcode } = current as { code?: unknown; errcode?: unknown };
    if (
      code === "ERR_SQLITE_ERROR" &&
      typeof errcode === "number" &&
      BUSY_PRIMARY_CODES.has(errcode & 0xff)
    )
      return true;
    current = current.cause;
  }
  return false;
};

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
