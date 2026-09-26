/** Timer primitives of the retry backoff; production uses the monotonic clock
 * and unref'd timers. */
export type PendingBackoffTimers = Readonly<{
  now: () => number;
  setTimer: (fire: () => void, delayMs: number) => unknown;
  clearTimer: (timer: unknown) => void;
}>;

const monotonicTimers: PendingBackoffTimers = {
  now: () => performance.now(),
  setTimer: (fire, delayMs) => {
    const timer = setTimeout(fire, delayMs);
    timer.unref?.();
    return timer;
  },
  clearTimer: (timer) => clearTimeout(timer as ReturnType<typeof setTimeout>),
};

export type PendingReconciliationBlocked = Readonly<{
  reason: string;
  blockedMs: number;
  retryInMs: number;
}>;

export type PendingReconciliationBackoff = Readonly<{
  /** Schedules the retry of a reconciliation that stayed pending with
   * `reason`. The delay doubles while the reason is unchanged, up to the cap;
   * once the same reason has blocked for `warnIntervalMs`, `warn` names it,
   * and again at most once per interval while it stays blocked. */
  arm: (reason: string) => void;
  /** Whether a retry of a reconciliation pending with `reason` may run now.
   * Before the retry's deadline it re-arms the timer for the remaining time
   * and refuses: a timer can fire early against the monotonic clock (the
   * event loop's cached time), and a refused trigger must never leave the
   * retry unscheduled. */
  due: (reason: string) => boolean;
  /** Cancels the scheduled retry and forgets the blocked reason. */
  clear: () => void;
}>;

/**
 * The retry schedule of a pending history reconciliation: a doubling,
 * monotonic delay per unchanged reason, so a blocked reconciliation is retried
 * on its own timer rather than on every source tip, with a periodic warning
 * while it stays blocked.
 */
export const makePendingReconciliationBackoff = (input: {
  readonly initialMs: number;
  readonly maxMs: number;
  readonly warnIntervalMs: number;
  /** Runs when a scheduled retry is due. */
  readonly onDue: () => void;
  readonly warn: (blocked: PendingReconciliationBlocked) => void;
  readonly timers?: PendingBackoffTimers;
}): PendingReconciliationBackoff => {
  const timers = input.timers ?? monotonicTimers;
  let pending:
    | {
        reason: string;
        delayMs: number;
        notBefore: number;
        blockedSince: number;
        warnedAt: number;
        timer: unknown;
      }
    | undefined;
  const clear = () => {
    if (pending !== undefined) timers.clearTimer(pending.timer);
    pending = undefined;
  };
  return {
    arm: (reason) => {
      const now = timers.now();
      const previous = pending?.reason === reason ? pending : undefined;
      const delayMs =
        previous === undefined
          ? input.initialMs
          : Math.min(previous.delayMs * 2, input.maxMs);
      const blockedSince = previous?.blockedSince ?? now;
      const warnedAt = previous?.warnedAt ?? now;
      const warning = now - warnedAt >= input.warnIntervalMs;
      clear();
      pending = {
        reason,
        delayMs,
        notBefore: now + delayMs,
        blockedSince,
        warnedAt: warning ? now : warnedAt,
        timer: timers.setTimer(input.onDue, delayMs),
      };
      if (warning)
        input.warn({
          reason,
          blockedMs: now - blockedSince,
          retryInMs: delayMs,
        });
    },
    due: (reason) => {
      if (pending === undefined || pending.reason !== reason) return true;
      const remainingMs = pending.notBefore - timers.now();
      if (remainingMs <= 0) return true;
      timers.clearTimer(pending.timer);
      pending.timer = timers.setTimer(input.onDue, Math.ceil(remainingMs));
      return false;
    },
    clear,
  };
};
