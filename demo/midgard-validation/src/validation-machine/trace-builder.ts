import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
} from "@al-ft/midgard-core";
import { Cause, Effect } from "effect";

import { completeValidationTrace } from "./trace-builder-complete.js";
import { prepareValidationTrace } from "./trace-builder-prepare.js";
import { DirectValidationTraceUnavailable } from "./trace-builder-prepare.js";
import { ValidationTraceStopped } from "./trace-builder-stop.js";
import {
  RetainingValidationTraceSink,
  type ValidationTraceRecording,
} from "./trace-sink.js";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineReplayInput,
} from "./types.js";
export { DirectValidationTraceUnavailable } from "./trace-builder-prepare.js";
export { ValidationTraceStopped } from "./trace-builder-stop.js";

const ZERO_32 = Buffer.alloc(32);

/** Replays `input` once, recording its states with `recording`. */
export const replayValidationTrace = (
  input: ValidationMachineReplayInput,
  recording: ValidationTraceRecording,
) =>
  Effect.gen(function* () {
    const context = yield* prepareValidationTrace(input, recording);
    return yield* completeValidationTrace(context);
  });

/**
 * Classifies every failure of a trace build, thrown or failed, the same way
 * for every recording: a process-local alarm that carries the transaction's
 * existing classification.
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
      programMaterialSidecarCbor: completed.programMaterialSidecarCbor,
      states,
      witnesses: sink!.witnesses,
      tree,
      verdict: completed.verdict,
      rejectionCode: completed.rejectionCode,
      rejectionReason: completed.rejectionReason,
      ledgerOps: completed.ledgerOps,
    };
  }).pipe(classifyValidationTraceFailure(input));
