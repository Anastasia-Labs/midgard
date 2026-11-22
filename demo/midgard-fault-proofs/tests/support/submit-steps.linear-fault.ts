/**
 * The linear-fault builders the tests drive, each bound into a function that
 * builds its step and submits it with no journal (see ./submit-steps.ts).
 */
import { buildLinearFaultCancel } from "../../src/linear-fault-cancel.js";
import { buildLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { buildLinearFaultContinue } from "../../src/linear-fault-submit.js";
import {
  type StepSubmitter,
  stepSubmitter,
  transactionSubmitter,
} from "./submit-steps.js";

export const submitLinearFaultCancel = stepSubmitter(buildLinearFaultCancel);
export const submitLinearFaultContinue = transactionSubmitter(
  buildLinearFaultContinue,
);
export const submitLinearFaultFinalize: StepSubmitter<
  typeof buildLinearFaultFinalize
> = stepSubmitter(buildLinearFaultFinalize);
