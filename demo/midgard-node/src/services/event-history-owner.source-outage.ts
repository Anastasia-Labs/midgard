/** The history owner's reconnect schedule: the first retry after
 * initialMs, doubling to maxMs. A source that has not let the owner make
 * progress for outageLimitMs escalates diagnostics while bounded retries continue.
 * Progress is a reopened gate, a pending
 * reconciliation the owner prepared and still holds over an answering source
 * (it waits on evidence, not on the source), or a first-start replay step
 * past the furthest one reached: a closed-gate append is not, since a failure
 * that recurs after every new block would otherwise restart the clock
 * forever. */
export type HistorySourceReconnectBounds = Readonly<{
  initialMs: number;
  maxMs: number;
  /** Escalation threshold, never a deadline for recoverable source outages. */
  outageLimitMs: number;
}>;

export const HISTORY_SOURCE_RECONNECT_BOUNDS: HistorySourceReconnectBounds =
  Object.freeze({
    initialMs: 1_000,
    maxMs: 30_000,
    outageLimitMs: 10 * 60_000,
  });

/** Why the owner's source is not following, for readiness and operators. */
export type HistorySourceStatus = Readonly<{
  state: "following" | "reconnecting" | "waiting_for_index";
  reason:
    | "history_owner_reconnecting"
    | "history_owner_waiting_for_index"
    | null;
  /** When the current outage began; null once the owner progressed again. */
  since: string | null;
  /** The current outage exceeded its diagnostic escalation threshold. */
  escalated: boolean;
  attempts: number;
  lastError: string | null;
}>;

const describe = (cause: unknown) =>
  (cause instanceof Error ? cause.message : String(cause)).slice(0, 512);

export const makeHistorySourceOutage = (
  bounds: HistorySourceReconnectBounds,
) => {
  for (const value of [bounds.initialMs, bounds.maxMs, bounds.outageLimitMs])
    if (!Number.isSafeInteger(value) || value <= 0)
      throw new Error(
        "History reconnect bounds must be positive safe integers",
      );
  // Monotonic for escalation; wall clock only for the reported start.
  let since: { readonly at: number; readonly wall: string } | undefined;
  let attempts = 0;
  let lastError: string | null = null;
  let reconnecting = false;
  let indexLag: string | undefined;
  let replayedThrough = -1;
  // Progress ends the outage: a following source reports no earlier error.
  const reset = () => {
    since = undefined;
    attempts = 0;
    lastError = null;
  };
  return {
    /** The source stopped answering; set until it answers again. */
    get reconnecting() {
      return reconnecting;
    },
    lost: (cause: unknown) => {
      reconnecting = true;
      since ??= { at: performance.now(), wall: new Date().toISOString() };
      lastError = describe(cause);
    },
    answered: () => {
      reconnecting = false;
    },
    /** A reopened gate ends the outage. */
    reopened: () => {
      if (!reconnecting) reset();
    },
    /** A pending reconciliation prepared and still held, in a session the
     * source answered, ends the outage: the hold is the owner waiting on
     * evidence, and its own reason fails readiness meanwhile. */
    held: () => {
      if (!reconnecting) reset();
    },
    /** A first-start replay step at this height. Each session replays from
     * the activation again, so only a step past every earlier one counts. */
    replayed: (height: number) => {
      if (height <= replayedThrough) return;
      replayedThrough = height;
      if (!reconnecting) reset();
    },
    indexLag: (cause: unknown) => {
      indexLag = cause === undefined ? undefined : describe(cause);
    },
    nextDelayMs: () => {
      const wait = Math.min(bounds.maxMs, bounds.initialMs * 2 ** attempts);
      attempts += 1;
      return wait;
    },
    /** The outage's age once it exceeds the escalation threshold, else undefined. */
    exceeded: () => {
      if (since === undefined) return undefined;
      const elapsed = performance.now() - since.at;
      return elapsed > bounds.outageLimitMs ? elapsed : undefined;
    },
    status: (): HistorySourceStatus =>
      Object.freeze({
        state: reconnecting
          ? "reconnecting"
          : indexLag !== undefined
            ? "waiting_for_index"
            : "following",
        reason: reconnecting
          ? "history_owner_reconnecting"
          : indexLag !== undefined
            ? "history_owner_waiting_for_index"
            : null,
        since: since?.wall ?? null,
        escalated:
          since !== undefined &&
          performance.now() - since.at > bounds.outageLimitMs,
        attempts,
        lastError: indexLag ?? lastError,
      }),
  };
};
