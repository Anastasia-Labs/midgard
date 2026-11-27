/**
 * The field-preimage-length-mismatch builders the tests drive, each bound into
 * a function that builds its step and submits it with no journal, beside the
 * generic helpers of ./submit-steps.ts, so a test takes both from one
 * module.
 */
import {
  buildFieldPreimageLengthAcceptedAuthentication,
  buildFieldPreimageLengthForcedDispatch,
} from "../../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-accepted-authentication.js";
import { buildFieldPreimageLengthAcceptedDispatch } from "../../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-accepted-dispatch.js";
import {
  buildFieldPreimageLengthCancel,
  buildFieldPreimageLengthInit,
} from "../../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-cancel.js";
import { buildFieldPreimageLengthForcedAuthentication } from "../../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-forced-authentication.js";
import { buildFieldPreimageLengthTerminal } from "../../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-terminal.js";
import { stepSubmitter } from "./submit-steps.js";

export const submitFieldPreimageLengthAcceptedAuthentication = stepSubmitter(
  buildFieldPreimageLengthAcceptedAuthentication,
);
export const submitFieldPreimageLengthAcceptedDispatch = stepSubmitter(
  buildFieldPreimageLengthAcceptedDispatch,
);
export const submitFieldPreimageLengthCancel = stepSubmitter(
  buildFieldPreimageLengthCancel,
);
export const submitFieldPreimageLengthForcedAuthentication = stepSubmitter(
  buildFieldPreimageLengthForcedAuthentication,
);
export const submitFieldPreimageLengthForcedDispatch = stepSubmitter(
  buildFieldPreimageLengthForcedDispatch,
);
export const submitFieldPreimageLengthInit = stepSubmitter(
  buildFieldPreimageLengthInit,
);
export const submitFieldPreimageLengthTerminal = stepSubmitter(
  buildFieldPreimageLengthTerminal,
);

export * from "./submit-steps.js";
