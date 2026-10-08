import "./block-confirmation.record-confirmed-pending-block.js";

import { Effect, Schedule } from "effect";

import { runAtFollowerView } from "../services/follower-write-gate.js";
import {
  Database,
  Globals,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import { buildBlockConfirmationAction } from "./block-confirmation.build-block-confirmation-action.js";
import { type ConfirmationWorkerRunner } from "./block-confirmation.record-confirmed-pending-block.js";
import { runConfirmationWorkerInThread } from "./block-confirmation.run-confirmation-worker-in-thread.js";

export const blockConfirmationAction =
  buildBlockConfirmationAction().pipe(runAtFollowerView);

/** One confirmation tick. */
export const confirmationTick = (
  runWorker: ConfirmationWorkerRunner = runConfirmationWorkerInThread,
) => buildBlockConfirmationAction(runWorker).pipe(runAtFollowerView);

/**
 * One confirmation tick under the fiber: a failure is logged and retried on
 * the next tick. Never fails.
 */
export const blockConfirmationStep = <R>(
  action: Effect.Effect<unknown, unknown, R>,
): Effect.Effect<void, never, R> =>
  action.pipe(Effect.asVoid, Effect.catchAllCause(Effect.logWarning));

export const blockConfirmationFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Globals | Database | NodeConfig> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("🟫 Block confirmation fiber started.");
    const action = blockConfirmationStep(
      withL1ControlPlane(
        globals,
        { scope: "block_confirmation", maxHoldMs: 180_000 },
        confirmationTick(),
      ).pipe(Effect.withSpan("block-confirmation-fiber")),
    );
    yield* Effect.repeat(action, schedule);
  });
