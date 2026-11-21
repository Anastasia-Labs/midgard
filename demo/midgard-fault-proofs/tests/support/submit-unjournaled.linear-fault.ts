/**
 * The linear-fault builders the tests drive, each bound into a function that
 * builds its step and submits it with no journal (see ./submit-unjournaled.ts).
 */
import { buildLinearFaultCancel } from "../../src/linear-fault-cancel.js";
import { buildLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { buildLinearFaultContinue } from "../../src/linear-fault-submit.js";
import {
  unjournaled,
  type UnjournaledSubmitter,
  unjournaledTransaction,
} from "./submit-unjournaled.js";

export const submitLinearFaultCancel = unjournaled(buildLinearFaultCancel);
export const submitLinearFaultContinue = unjournaledTransaction(
  buildLinearFaultContinue,
);
export const submitLinearFaultFinalize: UnjournaledSubmitter<
  typeof buildLinearFaultFinalize
> = unjournaled(buildLinearFaultFinalize);
