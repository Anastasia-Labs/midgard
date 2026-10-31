/**
 * Head-change triggering of the node's planner fibers (plan §4.1, N2). The
 * follower driver bumps `Globals.L1_HEAD_SEQUENCE` each time it applies a new
 * view at the node's tip; a fiber on `onL1HeadChange` runs its next tick
 * when that happens, instead of polling L1 on a timer.
 *
 * - A tick reads only the node's projections (P1 and the rest), so waking on
 *   a head change is enough for every L1-driven decision.
 * - `maxIdle` bounds the wait: a tick still runs that long after the last one
 *   without a head change. It covers what moves without L1 (a maturity or
 *   timeout window passing, the readiness heartbeat a fiber writes each
 *   tick) and a follower that is not running. It is a wall-clock wake-up
 *   only; no L1 decision reads the clock here.
 * - `minSpacing` coalesces bursts: at most one tick per spacing.
 */
import {
  Duration,
  Effect,
  Option,
  Schedule,
  Stream,
  SubscriptionRef,
} from "effect";

import type { Globals } from "./globals.globals.js";

/** The least time between two head-triggered ticks. */
export const L1_HEAD_MIN_SPACING = Duration.seconds(1);

type HeadGlobals = Pick<Globals, "L1_HEAD_SEQUENCE">;

/** Records that the follower driver applied a new view. */
export const publishL1HeadChange = (
  globals: HeadGlobals,
): Effect.Effect<void> =>
  SubscriptionRef.update(globals.L1_HEAD_SEQUENCE, (sequence) => sequence + 1);

/**
 * Waits until the head sequence differs from `seen` or `maxIdle` passes, and
 * returns the sequence then current.
 */
export const awaitL1HeadChange = (
  globals: HeadGlobals,
  seen: number,
  maxIdle: Duration.DurationInput,
): Effect.Effect<number> =>
  globals.L1_HEAD_SEQUENCE.changes.pipe(
    Stream.filter((sequence) => sequence !== seen),
    Stream.runHead,
    Effect.timeoutTo({
      duration: maxIdle,
      onSuccess: (head) => Effect.succeed(head),
      onTimeout: () => Effect.succeed(Option.none<number>()),
    }),
    Effect.flatten,
    Effect.flatMap((head) =>
      Option.match(head, {
        onSome: Effect.succeed,
        onNone: () => SubscriptionRef.get(globals.L1_HEAD_SEQUENCE),
      }),
    ),
  );

/**
 * A schedule that runs the next tick on the next head change, at least
 * `minSpacing` and at most `maxIdle` after the previous one. A head change
 * during a tick runs the next one after `minSpacing`. Built per fiber: it
 * records the head each tick started from.
 */
export const onL1HeadChange = (
  globals: HeadGlobals,
  maxIdle: Duration.DurationInput,
  minSpacing: Duration.DurationInput = L1_HEAD_MIN_SPACING,
): Effect.Effect<Schedule.Schedule<number>> =>
  Effect.map(SubscriptionRef.get(globals.L1_HEAD_SEQUENCE), (initial) => {
    let seen = initial;
    const idle = Duration.max(
      Duration.zero,
      Duration.subtract(Duration.decode(maxIdle), Duration.decode(minSpacing)),
    );
    return Schedule.modifyDelayEffect(Schedule.forever, () =>
      Effect.sleep(minSpacing).pipe(
        Effect.zipRight(awaitL1HeadChange(globals, seen, idle)),
        Effect.map((head) => {
          seen = head;
          return Duration.zero;
        }),
      ),
    );
  });
