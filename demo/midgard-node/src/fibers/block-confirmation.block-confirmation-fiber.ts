import "./block-confirmation.record-confirmed-pending-block.js";

import { Effect, Schedule } from "effect";

import {
  findSignedIntentReplacementIntegrityError,
  SignedIntentReplacementIntegrityError,
} from "../services/canonical-journal-recovery.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import {
  Database,
  Globals,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import { buildBlockConfirmationAction } from "./block-confirmation.build-block-confirmation-action.js";

export const blockConfirmationAction =
  buildBlockConfirmationAction().pipe(runHistoryProducer);

export const blockConfirmationFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SignedIntentReplacementIntegrityError,
  Globals | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("🟫 Block confirmation fiber started.");
    const action = withL1ControlPlane(
      globals,
      { scope: "block_confirmation", maxHoldMs: 180_000 },
      blockConfirmationAction,
    ).pipe(
      Effect.withSpan("block-confirmation-fiber"),
      // A transient failure is retried on the next tick; a replaced block
      // that won its slot after the node moved past its base is not
      // transient, so it stops the node.
      Effect.catchAllCause((cause) => {
        const integrity = findSignedIntentReplacementIntegrityError(cause);
        return integrity === undefined
          ? Effect.logWarning(cause)
          : Effect.logError(integrity.message).pipe(
              Effect.zipRight(Effect.fail(integrity)),
            );
      }),
    );
    yield* Effect.repeat(action, schedule);
  });
