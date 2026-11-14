/** Direct `executionSourceScriptDecoding` wire
 * (`execution_source_script_decoding/terminal.ak`). The shared step 01 and
 * event selection hand the terminal the selected event; the terminal decides
 * from its recorded reason alone. Constructor and field order is wire
 * format. */
import { rejectionReasonArmOf } from "../rejection-reason.js";
import type { DirectEventSelected } from "./direct-script-derivation-binding.js";
import { DirectBindPreStateDatum } from "./direct-script-derivation-binding.js";
import {
  terminalVerdictContradiction,
  type VerdictSubject,
} from "./proof-thread-substrate.js";

/** The terminal's input datum: the event selection's output. */
export const ExecutionSourceScriptDecodingFinalDatum = DirectBindPreStateDatum;
export type ExecutionSourceScriptDecodingFinalState = DirectEventSelected;

/** The recorded arms the terminal convicts. */
export const EXECUTION_SOURCE_SCRIPT_DECODING_ARMS: readonly string[] =
  Object.freeze([
    "ExecutionNativeScriptMalformed",
    "ExecutionNativeScriptNodeLimit",
    "ExecutionNativeScriptDepthLimit",
  ]);

/** Twin of `terminal.convicts_v1` over the selected leaf's subject: a leaf
 * convicts exactly when it rejects its transaction with one of the three
 * arms. */
export const executionSourceScriptDecodingSubjectConvicts = (
  subject: VerdictSubject,
): boolean => {
  const reason = subject.rejection_reason;
  return (
    reason !== null &&
    EXECUTION_SOURCE_SCRIPT_DECODING_ARMS.includes(
      rejectionReasonArmOf(reason),
    ) &&
    terminalVerdictContradiction(subject, false)
  );
};

export const executionSourceScriptDecodingConvicts = (
  state: ExecutionSourceScriptDecodingFinalState,
): boolean => executionSourceScriptDecodingSubjectConvicts(state.subject);
