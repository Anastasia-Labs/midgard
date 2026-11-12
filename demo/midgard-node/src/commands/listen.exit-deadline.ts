/**
 * The node's exit deadline (`armExitDeadline`), armed once the node has
 * decided to exit non-zero, so a finalizer that hangs cannot keep a failed
 * node up. `runListen` gives the process's exit (`NodeProcessExit`); without
 * one, as in tests, nothing is armed.
 */
import {
  armExitDeadline,
  ROLE_EXIT_DEADLINE_MS,
} from "@al-ft/midgard-l1-follower";
import { type Cause, Deferred, Effect, FiberRef } from "effect";

import { ADMISSION_WRITE_SHUTDOWN_FLUSH_MS } from "../services/admission-writer.js";
import { findStartupStepFailure } from "../services/startup-waiting.js";
import {
  findTransientBudgetExhausted,
  type TransientExhaustion,
} from "../services/transient-exhaustion.js";

/**
 * The roles' shared deadline on top of the admission writer's bounded
 * shutdown flush, so a teardown that reaches the flush late still lets the
 * writer name every acknowledged tx it left unwritten before the exit.
 */
export const NODE_EXIT_DEADLINE_MS =
  ADMISSION_WRITE_SHUTDOWN_FLUSH_MS + ROLE_EXIT_DEADLINE_MS;

export type ProcessExit = Readonly<{
  exit: (code: number) => void;
  deadlineMs?: number;
}>;

/** The process exit the deadline calls; none outside `runListen`. */
export const NodeProcessExit = FiberRef.unsafeMake<ProcessExit | undefined>(
  undefined,
);

const armed = new WeakSet<ProcessExit>();

/** Arms the exit deadline, once per process exit. */
export const armNodeExitDeadline: Effect.Effect<void> = Effect.flatMap(
  FiberRef.get(NodeProcessExit),
  (processExit) =>
    Effect.sync(() => {
      if (processExit === undefined || armed.has(processExit)) return;
      armed.add(processExit);
      armExitDeadline({
        exit: processExit.exit,
        code: 1,
        deadlineMs: processExit.deadlineMs ?? NODE_EXIT_DEADLINE_MS,
      });
    }),
);

/**
 * Whether a failure ends the node (owner ruling 2026-10-09): a transient
 * that outlived its bound (`TransientBudgetExhaustedError`, or a startup
 * step `exhausted`), or a startup step marked `terminal`. Any other failure
 * holds the node up, unready.
 */
export const nodeFailureExits = (cause: Cause.Cause<unknown>): boolean => {
  const step = findStartupStepFailure(cause);
  return (
    findTransientBudgetExhausted(cause) !== undefined ||
    step?.exhausted === true ||
    step?.terminal === true
  );
};

/**
 * `Effect.scoped`, with the exit deadline armed before the scope's
 * finalizers run when `effect` fails in a way that ends the node.
 */
export const scopedWithExitDeadline = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
) =>
  effect.pipe(
    Effect.tapErrorCause((cause) =>
      nodeFailureExits(cause) ? armNodeExitDeadline : Effect.void,
    ),
    Effect.scoped,
  );

/**
 * Runs `effect` until a transient bound is spent (`exhaustion`), which
 * fails the node. The exit deadline is armed first, so it bounds `effect`'s
 * finalizers as the race interrupts it.
 */
export const untilTransientExhaustion = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  exhaustion: TransientExhaustion,
) =>
  Effect.raceFirst(
    effect,
    Deferred.await(exhaustion).pipe(
      Effect.tapErrorCause(() => armNodeExitDeadline),
    ),
  );
