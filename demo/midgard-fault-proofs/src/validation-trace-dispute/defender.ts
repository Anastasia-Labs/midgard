/** The defending operator's side of a validation-trace case, as the node
 * plays it: its defense material, its Defend, its reveals and its thread's
 * expiry, and the reads and clocks they are decided by. */
export {
  recordedValidationTraceDefenders,
  type ValidationTraceCaseTransaction,
  validationTraceThreadTokenName,
} from "./case-history.js";
export {
  buildValidationTraceDefend,
  completeValidationTraceDefend,
  VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE,
  VALIDATION_TRACE_THREAD_FLOOR_LOVELACE,
  type ValidationTraceDefendersProof,
  validationTraceDefendersProof,
  type ValidationTraceDefense,
} from "./defend.js";
export {
  buildValidationTraceDefenseMaterial,
  type ValidationTraceDefenseMaterial,
} from "./defense-material.js";
export {
  buildValidationTraceThreadExpiry,
  type ValidationTraceExpiryHolder,
  validationTraceThreadExpiresAfterMs,
  validationTraceThreadHeader,
} from "./expiry.js";
export {
  buildValidationTraceOperatorReveal,
  validationTraceMoveWindow,
} from "./move-builders.js";
export {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
} from "./move-fee.js";
export {
  classifyValidationTraceThread,
  readValidationTraceThread,
  validationTraceDefenderDeadlineMs,
  type ValidationTraceThread,
  type ValidationTraceThreadHolder,
  type ValidationTraceThreadStanding,
} from "./threads.js";
export { validationTraceDefenseDeadlineMs } from "./waiting.js";
