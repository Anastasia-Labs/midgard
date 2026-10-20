/** Holds on the script re-execution of single events. A re-execution that
 * ends in a throw (the evaluator's own failure, such as running out of
 * memory, or a failed read of the event's pre-state) says nothing about the
 * script, so it never decides the block: that event alone is held and
 * retried after a backoff that doubles up to a bound, while every other
 * event of the block is still replayed. Only a deterministic outcome without
 * a verdict, such as a trace replay that stops where the verdict replay
 * failed, classifies as an unprovable gap. */

/** The readiness reason a held objective names. */
export const VALIDATION_TRACE_REPLAY_HELD = "validation_trace_replay_held";

/** The first retry waits `initialMs`; each failure after doubles it, to at
 * most `maxMs`. Retries never stop. */
export const VALIDATION_TRACE_REPLAY_BACKOFF = Object.freeze({
  initialMs: 1_000,
  maxMs: 60_000,
});

/** Events tracked at once; the oldest hold is forgotten past this, and its
 * event is simply retried at once. */
const CAPACITY = 4_096;

export type ValidationTraceReplayHold = Readonly<{
  headerHash: string;
  /** The held event's position among the block's L2 transactions. */
  position: number;
  txId: string;
  /** Consecutive failed re-executions. */
  attempts: number;
  /** No re-execution is tried before this time. */
  retryAtMs: number;
  detail: string;
}>;

/**
 * The block's decision waits on held events: no event of it was proven to
 * fail, and an event whose re-execution threw may be the one that does.
 * Not a verdict: the objective stays active and names `readiness` until a
 * replay at or after `retryAtMs` decides the held events.
 */
export class ValidationTraceReplayHeldError extends Error {
  readonly readiness = VALIDATION_TRACE_REPLAY_HELD;
  readonly category = "validationTraceDispute";
  readonly retryAtMs: number;

  constructor(
    readonly headerHash: string,
    readonly holds: readonly ValidationTraceReplayHold[],
  ) {
    super(
      `validationTraceDispute/${headerHash}: script re-execution held for ${holds
        .map(({ txId, attempts }) => `${txId} (attempt ${attempts.toString()})`)
        .join(", ")}`,
    );
    this.name = "ValidationTraceReplayHeldError";
    this.retryAtMs = Math.min(...holds.map(({ retryAtMs }) => retryAtMs));
  }
}

export const isValidationTraceReplayHeldError = (
  error: unknown,
): error is ValidationTraceReplayHeldError =>
  error instanceof ValidationTraceReplayHeldError;

export type ValidationTraceReplayHolds = Readonly<{
  /** The hold that keeps `key` from re-executing now, if any. */
  waiting: (key: string) => ValidationTraceReplayHold | undefined;
  /** Records one more failed re-execution of `key`. */
  failed: (
    key: string,
    event: Omit<ValidationTraceReplayHold, "attempts" | "retryAtMs">,
  ) => ValidationTraceReplayHold;
  /** `key` re-executed to an outcome. */
  decided: (key: string) => void;
}>;

/** One replayer's holds, keyed by block and event. */
export const createValidationTraceReplayHolds = ({
  now = Date.now,
  backoff = VALIDATION_TRACE_REPLAY_BACKOFF,
}: {
  readonly now?: () => number;
  readonly backoff?: Readonly<{ initialMs: number; maxMs: number }>;
} = {}): ValidationTraceReplayHolds => {
  const holds = new Map<string, ValidationTraceReplayHold>();
  return Object.freeze({
    waiting: (key: string) => {
      const hold = holds.get(key);
      return hold !== undefined && now() < hold.retryAtMs ? hold : undefined;
    },
    failed: (key, event) => {
      const attempts = (holds.get(key)?.attempts ?? 0) + 1;
      const delay = Math.min(
        backoff.initialMs * 2 ** Math.min(attempts - 1, 30),
        backoff.maxMs,
      );
      const hold = Object.freeze({
        ...event,
        attempts,
        retryAtMs: now() + delay,
      });
      holds.delete(key);
      holds.set(key, hold);
      if (holds.size > CAPACITY) holds.delete(holds.keys().next().value!);
      return hold;
    },
    decided: (key) => {
      holds.delete(key);
    },
  });
};
