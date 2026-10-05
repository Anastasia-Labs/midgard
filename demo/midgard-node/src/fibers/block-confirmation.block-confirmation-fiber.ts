import "./block-confirmation.record-confirmed-pending-block.js";

import { Effect, Schedule } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { findSignedIntentReplacementIntegrityError } from "../services/canonical-journal-recovery.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import {
  Database,
  Globals,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import {
  SIGNED_INTENT_UNDECIDED,
  SIGNED_INTENT_UNDECIDED_ESCALATION_MS,
} from "../services/signed-intent-undecided.js";
import type {
  WorkerInput as BlockConfirmationWorkerInput,
  WorkerOutput as BlockConfirmationWorkerOutput,
} from "../workers/utils/confirm-block-commitments.js";
import { buildBlockConfirmationAction } from "./block-confirmation.build-block-confirmation-action.js";
import {
  type ConfirmationWorkerRunner,
  toPendingWorkerInput,
} from "./block-confirmation.record-confirmed-pending-block.js";
import { runConfirmationWorkerInThread } from "./block-confirmation.run-confirmation-worker-in-thread.js";

export const blockConfirmationAction =
  buildBlockConfirmationAction().pipe(runHistoryProducer);

/**
 * Whether a confirmation tick reached the signed-intent decision: its worker
 * returned a queue snapshot that matched no pending block, the only output
 * the action revives replaced journals from (or refuses with
 * `SignedIntentReplacementIntegrityError`), and the active journal is still
 * the one the worker was given, so no stale-snapshot guard discarded it. A
 * tick that returned early (reset, idle backoff, a stale snapshot) or
 * confirmed a pending block decided nothing.
 */
export const reachedSignedIntentDecision = (
  given: BlockConfirmationWorkerInput | undefined,
  output: BlockConfirmationWorkerOutput | undefined,
  activeAfter: ReturnType<typeof toPendingWorkerInput>,
): boolean => {
  if (
    given === undefined ||
    output?.type !== "SuccessfulConfirmationOutput" ||
    output.matchedPendingBlocksUTxO !== null
  )
    return false;
  const before = given.data.pendingBlock;
  if (before === null || activeAfter === null)
    return before === null && activeAfter === null;
  return (
    before.expectedHeaderHash === activeAfter.expectedHeaderHash &&
    before.submittedTxHash === activeAfter.submittedTxHash &&
    (before.intendedTxHash ?? null) === activeAfter.intendedTxHash &&
    before.updatedAtMs === activeAfter.updatedAtMs
  );
};

/** One confirmation tick, reporting whether it reached the signed-intent
 * decision (see `reachedSignedIntentDecision`). */
export const confirmationTick = (
  runWorker: ConfirmationWorkerRunner = runConfirmationWorkerInThread,
) =>
  Effect.suspend(() => {
    let given: BlockConfirmationWorkerInput | undefined;
    let output: BlockConfirmationWorkerOutput | undefined;
    const recording: ConfirmationWorkerRunner = (input) =>
      runWorker(input).pipe(
        Effect.tap((result) =>
          Effect.sync(() => {
            given = input;
            output = result;
          }),
        ),
      );
    return buildBlockConfirmationAction(recording).pipe(
      runHistoryProducer,
      Effect.zipRight(
        Effect.suspend(() =>
          output === undefined
            ? Effect.succeed(false)
            : Effect.map(
                PendingBlockFinalizationsDB.retrieveActive(),
                (active) =>
                  reachedSignedIntentDecision(
                    given,
                    output,
                    toPendingWorkerInput(active),
                  ),
              ),
        ),
      ),
    );
  });

/**
 * One confirmation tick. A transient failure is logged and retried on the next
 * tick. A replaced block that won its slot after the node moved past its base
 * (`SignedIntentReplacementIntegrityError`, refused before anything is
 * written) cannot be decided yet: it raises `signed_intent_undecided`, which
 * holds block commitment, and every later tick re-derives it from the
 * confirmation output. Only a tick that completes after reaching that
 * decision again (`action` succeeds with true) clears it; a tick that
 * returned before it leaves the reason, and its escalation, as they are.
 * Never fails.
 */
export const blockConfirmationStep = <R>(
  action: Effect.Effect<boolean, unknown, R>,
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void, never, R> =>
  action.pipe(
    Effect.flatMap((decided) =>
      decided
        ? clearLivenessIncident(
            globals,
            HaltSource.blockConfirmationSignedIntent,
          )
        : Effect.void,
    ),
    Effect.catchAllCause((cause) => {
      const integrity = findSignedIntentReplacementIntegrityError(cause);
      return integrity === undefined
        ? Effect.logWarning(cause)
        : raiseLivenessIncident(
            globals,
            HaltSource.blockConfirmationSignedIntent,
            SIGNED_INTENT_UNDECIDED,
            `${integrity.message} Block commitment is held, the signed intent stays in place, and confirmation re-derives it on every tick.`,
            { escalateAfterMs: SIGNED_INTENT_UNDECIDED_ESCALATION_MS },
          );
    }),
  );

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
      globals,
    );
    yield* Effect.repeat(action, schedule);
  });
