/**
 * The field-preimage-length-mismatch builders the tests drive, each bound into
 * a function that builds its step and submits it with no journal, beside the
 * generic helpers of ./submit-unjournaled.ts, so a test takes both from one
 * module.
 */
import {
  buildFieldPreimageLengthAcceptedAuthentication,
  buildFieldPreimageLengthAcceptedDispatch,
  buildFieldPreimageLengthCancel,
  buildFieldPreimageLengthForcedAuthentication,
  buildFieldPreimageLengthForcedDispatch,
  buildFieldPreimageLengthInit,
  buildFieldPreimageLengthTerminal,
} from "../../src/field-preimage-length-mismatch/submit-lucid.js";
import { unjournaled } from "./submit-unjournaled.js";

export const submitFieldPreimageLengthAcceptedAuthentication = unjournaled(
  buildFieldPreimageLengthAcceptedAuthentication,
);
export const submitFieldPreimageLengthAcceptedDispatch = unjournaled(
  buildFieldPreimageLengthAcceptedDispatch,
);
export const submitFieldPreimageLengthCancel = unjournaled(
  buildFieldPreimageLengthCancel,
);
export const submitFieldPreimageLengthForcedAuthentication = unjournaled(
  buildFieldPreimageLengthForcedAuthentication,
);
export const submitFieldPreimageLengthForcedDispatch = unjournaled(
  buildFieldPreimageLengthForcedDispatch,
);
export const submitFieldPreimageLengthInit = unjournaled(
  buildFieldPreimageLengthInit,
);
export const submitFieldPreimageLengthTerminal = unjournaled(
  buildFieldPreimageLengthTerminal,
);

export * from "./submit-unjournaled.js";
