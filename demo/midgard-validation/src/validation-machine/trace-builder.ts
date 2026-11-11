import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
} from "@al-ft/midgard-core";
import { Cause, Effect } from "effect";

import { completeValidationTrace } from "./trace-builder-complete.js";
import { prepareValidationTrace } from "./trace-builder-prepare.prepare-validation-trace.js";
import { ValidationTraceStopped } from "./trace-builder-stop.js";
import {
  DirectValidationTraceUnavailable,
  replayValidationTransaction,
} from "./trace-replay.js";
import {
  RetainingValidationTraceSink,
  type ValidationTraceRecording,
} from "./trace-sink.js";
import { openTraceWalk, ValidationTraceDisagreement } from "./trace-walk.js";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineReplayInput,
} from "./types.js";
export { ValidationTraceStopped } from "./trace-builder-stop.js";
export { DirectValidationTraceUnavailable } from "./trace-replay.js";

const ZERO_32 = Buffer.alloc(32);

/**
 * Replays `input` once, then walks its trace, recording the states with
 * `recording`. Returns the completed walk and the script evaluations replay
 * ran or reused, in run order: a replay given them as
 * `input.scriptEvaluations` runs no evaluator.
 */
export const replayValidationTrace = (
  input: ValidationMachineReplayInput,
  recording: ValidationTraceRecording,
) =>
  Effect.gen(function* () {
    const replay = yield* replayValidationTransaction(input, recording.mode);
    const { walk, finish } = yield* openTraceWalk(input, replay, recording);
    const context = yield* prepareValidationTrace(replay.transaction, walk);
    yield* completeValidationTrace(context, walk);
    const completed = yield* finish();
    return {
      ...completed,
      scriptEvaluations: replay.transaction.scriptEvaluations,
    };
  });

/**
 * Classifies every failure of a trace build, thrown or failed, the same way
 * for every recording: a process-local alarm that carries the transaction's
 * existing classification. A walk that disagrees with replay is a
 * disagreement; any other failure leaves the trace unavailable.
 */
export const classifyValidationTraceFailure =
  (input: ValidationMachineReplayInput) =>
  <A, E>(build: Effect.Effect<A, E>): Effect.Effect<A, Error> =>
    build.pipe(
      Effect.catchAllCause((failure) => {
        // An interruption carries no failure value.
        if (Cause.isInterrupted(failure)) {
          return Effect.failCause(failure as Cause.Cause<never>);
        }
        const cause = Cause.squash(failure);
        return Effect.fail(
          cause instanceof ValidationTraceStopped ||
            cause instanceof DirectValidationTraceUnavailable
            ? cause
            : cause instanceof ValidationTraceDisagreement
              ? new ValidationTraceStopped(
                  "disagreement",
                  input,
                  cause.message,
                  {
                    cause,
                  },
                )
              : new ValidationTraceStopped(
                  "unavailable",
                  input,
                  cause instanceof Error
                    ? cause.message
                    : "Validation trace builder cannot build the classified transaction",
                  { cause },
                ),
        );
      }),
    );

/** Replay validation with captured script executions, then assemble the authenticated trace. */
export const buildDeterministicValidationMachineTrace = (
  input: ValidationMachineReplayInput,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    let sink: RetainingValidationTraceSink | undefined;
    const completed = yield* replayValidationTrace(input, {
      mode: "proof",
      open: (frame) => (sink = new RetainingValidationTraceSink(frame)),
    });
    const states = sink!.states();
    const tree = buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      completed.verdict,
      completed.rejectionCodeHash,
    );
    if (
      tree.descriptor.initialStateHash.equals(ZERO_32) ||
      tree.descriptor.terminalStateHash.equals(ZERO_32)
    ) {
      return yield* Effect.fail(
        new Error("validation trace endpoint hash must not be zero"),
      );
    }
    return {
      validationContextCbor: completed.validationContextCbor,
      states,
      witnesses: sink!.witnesses,
      tree,
      verdict: completed.verdict,
      rejectionCode: completed.rejectionCode,
      rejectionReason: completed.rejectionReason,
      ledgerOps: completed.ledgerOps,
    };
  }).pipe(classifyValidationTraceFailure(input));
