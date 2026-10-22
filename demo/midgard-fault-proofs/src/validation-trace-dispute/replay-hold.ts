/** Holds on the script re-execution of single events. A re-execution that
 * ends in a throw (the evaluator's own failure, such as running out of
 * memory, or a failed read of the event's pre-state) says nothing about the
 * script, so it never decides the block: that event alone is held and
 * retried after a backoff that doubles up to a bound, while every other
 * event of the block is still replayed. Only a deterministic outcome without
 * a verdict, such as a trace replay that stops where the verdict replay
 * failed, classifies as an unprovable gap. The retry work is bounded: a
 * pass re-executes one held event, a forgotten hold keeps its backoff, and
 * an event that keeps failing the same way is named stuck rather than
 * held, still never a verdict. */

import type { CanonicalViolationDetection } from "../workflow/classification.js";

/** The readiness reason a held objective names. */
export const VALIDATION_TRACE_REPLAY_HELD = "validation_trace_replay_held";

/** The readiness reason of an objective held on a stuck event: one whose
 * re-execution failed the same way `VALIDATION_TRACE_REPLAY_STUCK_AFTER`
 * times in a row. Still not a verdict: the event keeps retrying. */
export const VALIDATION_TRACE_REPLAY_STUCK = "validation_trace_replay_stuck";

/** Consecutive identical failures that make an event stuck. */
export const VALIDATION_TRACE_REPLAY_STUCK_AFTER = 5;

/** The first retry waits `initialMs`; each failure after doubles it, to at
 * most `maxMs`. Retries never stop. */
export const VALIDATION_TRACE_REPLAY_BACKOFF = Object.freeze({
  initialMs: 1_000,
  maxMs: 60_000,
});

/** Held events one replay pass re-executes at most; any other held event
 * that is due is carried, still held, to the next pass. Events never held
 * are not counted: a block's first replay runs each of them once. */
export const VALIDATION_TRACE_REPLAY_RETRIES_PER_PASS = 1;

/** Events tracked at once. Past this, the least recently failed hold is
 * forgotten, but its retry time and attempt count are kept in `Backlog`. */
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
  /** Whether the last `VALIDATION_TRACE_REPLAY_STUCK_AFTER` or more
   * failures were identical. */
  stuck: boolean;
}>;

/**
 * The block's decision waits on held events: no event of it was proven to
 * fail, and an event whose re-execution threw may be the one that does.
 * Not a verdict: the objective stays active and names `readiness` until a
 * replay at or after `retryAtMs` decides the held events.
 */
export class ValidationTraceReplayHeldError extends Error {
  readonly readiness:
    | typeof VALIDATION_TRACE_REPLAY_HELD
    | typeof VALIDATION_TRACE_REPLAY_STUCK;
  readonly category = "validationTraceDispute";
  readonly _tag = "ValidationTraceReplayHeldError";
  readonly retryAtMs: number;

  /**
   * `heldFromOrder` is the earliest block event order of a held event, and
   * `detections` the findings the pass did decide. A replay union that has a
   * finding strictly before `heldFromOrder` may decide the block without the
   * held events, which can only add findings at or after it.
   */
  constructor(
    readonly headerHash: string,
    readonly holds: readonly ValidationTraceReplayHold[],
    readonly heldFromOrder: number,
    readonly detections: readonly CanonicalViolationDetection[],
  ) {
    super(
      `validationTraceDispute/${headerHash}: script re-execution held for ${holds
        .map(
          ({ txId, attempts, stuck }) =>
            `${txId} (attempt ${attempts.toString()}${stuck ? ", stuck" : ""})`,
        )
        .join(", ")}`,
    );
    this.name = "ValidationTraceReplayHeldError";
    this.readiness = holds.some(({ stuck }) => stuck)
      ? VALIDATION_TRACE_REPLAY_STUCK
      : VALIDATION_TRACE_REPLAY_HELD;
    this.retryAtMs = Math.min(...holds.map(({ retryAtMs }) => retryAtMs));
  }
}

/** Matched by tag: the package ships more than one entry bundle. */
export const isValidationTraceReplayHeldError = (
  error: unknown,
): error is ValidationTraceReplayHeldError =>
  typeof error === "object" &&
  error !== null &&
  (error as { readonly _tag?: unknown })._tag ===
    "ValidationTraceReplayHeldError";

/** The event a hold names. */
export type ValidationTraceReplayEvent = Pick<
  ValidationTraceReplayHold,
  "headerHash" | "position" | "txId"
>;

/** One replay pass over a block. */
export type ValidationTraceReplayPass = Readonly<{
  /**
   * The hold that keeps `key` from re-executing in this pass, if any: its
   * retry time has not come, or this pass's share of held events goes to its
   * block's due holds that were re-executed less recently (failed earlier).
   */
  waiting: (
    key: string,
    event: ValidationTraceReplayEvent,
  ) => ValidationTraceReplayHold | undefined;
}>;

export type ValidationTraceReplayHolds = Readonly<{
  /** Starts one replay pass. */
  pass: () => ValidationTraceReplayPass;
  /** Records one more failed re-execution of `key`. */
  failed: (
    key: string,
    event: ValidationTraceReplayEvent & Readonly<{ detail: string }>,
  ) => ValidationTraceReplayHold;
  /** `key` re-executed to an outcome. */
  decided: (key: string) => void;
}>;

/** Slots of the forgotten holds' backlog. */
const BACKLOG_SLOTS = 1 << 16;

/** FNV-1a over `key`'s UTF-16 code units, to a backlog slot. */
const slotOf = (key: string): number => {
  let hash = 0x811c9dc5;
  for (let index = 0; index < key.length; index += 1) {
    hash ^= key.charCodeAt(index);
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0) & (BACKLOG_SLOTS - 1);
};

/**
 * The retry time and attempt count of each forgotten hold, by slot. Keys
 * sharing a slot share the later retry time and the larger count, so a
 * forgotten event is never retried sooner, or backed off less, than its own
 * hold said; another event in its slot at worst waits as long.
 */
const createBacklog = () => {
  const attempts = new Uint8Array(BACKLOG_SLOTS);
  const retryAtMs = new Float64Array(BACKLOG_SLOTS);
  return Object.freeze({
    keep: (key: string, hold: ValidationTraceReplayHold): void => {
      const slot = slotOf(key);
      attempts[slot] = Math.max(attempts[slot]!, Math.min(hold.attempts, 255));
      retryAtMs[slot] = Math.max(retryAtMs[slot]!, hold.retryAtMs);
    },
    read: (key: string) => {
      const slot = slotOf(key);
      return { attempts: attempts[slot]!, retryAtMs: retryAtMs[slot]! };
    },
  });
};

/** One replayer's holds, keyed by block and event. `onStuck` is called
 * once each time an event becomes stuck. */
export const createValidationTraceReplayHolds = ({
  now = Date.now,
  backoff = VALIDATION_TRACE_REPLAY_BACKOFF,
  capacity = CAPACITY,
  onStuck,
}: {
  readonly now?: () => number;
  readonly backoff?: Readonly<{ initialMs: number; maxMs: number }>;
  readonly capacity?: number;
  readonly onStuck?: (hold: ValidationTraceReplayHold) => void;
} = {}): ValidationTraceReplayHolds => {
  /** By key, least recently failed first. */
  const holds = new Map<
    string,
    ValidationTraceReplayHold & Readonly<{ repeats: number }>
  >();
  const backlog = createBacklog();
  const visible = ({
    repeats: _repeats,
    ...hold
  }: ValidationTraceReplayHold & Readonly<{ repeats: number }>) =>
    Object.freeze(hold);
  return Object.freeze({
    pass: () => {
      let retries = 0;
      /** The keys this pass re-executes, by block: the block's due holds
       * that failed least recently. Holds that failed in the same pass are
       * in that pass's (step) order. */
      const granted = new Map<string, ReadonlySet<string>>();
      const grantedFor = (headerHash: string) => {
        let keys = granted.get(headerHash);
        if (keys === undefined) {
          const due = new Set<string>();
          for (const [key, hold] of holds) {
            if (due.size >= VALIDATION_TRACE_REPLAY_RETRIES_PER_PASS) break;
            if (hold.headerHash === headerHash && now() >= hold.retryAtMs)
              due.add(key);
          }
          keys = due;
          granted.set(headerHash, keys);
        }
        return keys;
      };
      return Object.freeze({
        waiting: (key: string, event: ValidationTraceReplayEvent) => {
          const hold = holds.get(key);
          if (hold === undefined) {
            const kept = backlog.read(key);
            return now() < kept.retryAtMs
              ? Object.freeze({
                  ...event,
                  attempts: kept.attempts,
                  retryAtMs: kept.retryAtMs,
                  detail: "held past the replayer's hold capacity",
                  stuck: false,
                })
              : undefined;
          }
          if (now() < hold.retryAtMs) return visible(hold);
          // A due hold this pass carries is retried in a later pass, so it
          // never reports a retry time already past: a scheduler honouring it
          // would replay the block back to back.
          if (
            retries >= VALIDATION_TRACE_REPLAY_RETRIES_PER_PASS ||
            !grantedFor(event.headerHash).has(key)
          )
            return Object.freeze({
              ...visible(hold),
              retryAtMs: now() + backoff.initialMs,
            });
          retries += 1;
          return undefined;
        },
      });
    },
    failed: (key, event) => {
      const previous = holds.get(key);
      const attempts = (previous?.attempts ?? backlog.read(key).attempts) + 1;
      const repeats =
        previous !== undefined && previous.detail === event.detail
          ? previous.repeats + 1
          : 1;
      const delay = Math.min(
        backoff.initialMs * 2 ** Math.min(attempts - 1, 30),
        backoff.maxMs,
      );
      const hold = Object.freeze({
        ...event,
        attempts,
        retryAtMs: now() + delay,
        stuck: repeats >= VALIDATION_TRACE_REPLAY_STUCK_AFTER,
        repeats,
      });
      holds.delete(key);
      holds.set(key, hold);
      if (holds.size > capacity) {
        const [forgotten, kept] = holds.entries().next().value!;
        holds.delete(forgotten);
        backlog.keep(forgotten, kept);
      }
      if (hold.stuck && previous?.stuck !== true) onStuck?.(visible(hold));
      return visible(hold);
    },
    decided: (key) => {
      holds.delete(key);
    },
  });
};
