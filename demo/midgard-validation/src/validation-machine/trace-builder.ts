import { Cause, Effect } from "effect";

import { completeValidationTrace } from "./trace-builder-complete.js";
import { prepareValidationTrace } from "./trace-builder-prepare.js";
import { DirectValidationTraceUnavailable } from "./trace-builder-prepare.js";
import { ValidationTraceStopped } from "./trace-builder-stop.js";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineReplayInput,
} from "./types.js";
export { DirectValidationTraceUnavailable } from "./trace-builder-prepare.js";
export { ValidationTraceStopped } from "./trace-builder-stop.js";
/** Replay validation with captured script executions, then assemble the authenticated trace. */
export const buildDeterministicValidationMachineTrace = (
  input: ValidationMachineReplayInput,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    const context = yield* prepareValidationTrace(input);
    return yield* completeValidationTrace(context);
  }).pipe(
    Effect.catchAllCause((failure) => {
      if (Cause.isInterrupted(failure)) return Effect.failCause(failure);
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
