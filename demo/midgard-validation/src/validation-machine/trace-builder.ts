import { Effect } from "effect";

import { completeValidationTrace } from "./trace-builder-complete.js";
import { prepareValidationTrace } from "./trace-builder-prepare.js";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineReplayInput,
} from "./types.js";
export { DirectValidationTraceUnavailable } from "./trace-builder-prepare.js";
/** Replay validation, then execute the authenticated script and ledger phases. */
export const buildDeterministicValidationMachineTrace = (
  input: ValidationMachineReplayInput,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    const context = yield* prepareValidationTrace(input);
    return yield* completeValidationTrace(context);
  });
