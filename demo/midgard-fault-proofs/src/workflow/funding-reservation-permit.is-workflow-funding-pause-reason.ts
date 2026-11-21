import { WorkflowFundingReservationUnavailableError } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import { WORKFLOW_FUNDING_SHORTFALL } from "./funding-reservation-permit.workflow-funding-shortfall.js";
import { SCRIPT_DECODE_RECORD_UNDERFUNDED } from "./script-decode-prerequisite.port.js";

const RESERVATION_UNAVAILABLE = new WorkflowFundingReservationUnavailableError()
  .message;

/**
 * Whether a workflow's pending reason says it paused because the prover
 * wallet cannot fund its next transaction, which a top-up resumes: a
 * funding shortfall of an action or of a script-decode record transaction,
 * or a reservation that could not be made. A wait that only mentions
 * funding, such as a signed transaction awaiting canonical reconciliation,
 * is not one.
 */
export const isWorkflowFundingPauseReason = (reason: string): boolean =>
  reason === RESERVATION_UNAVAILABLE ||
  reason.startsWith(`${WORKFLOW_FUNDING_SHORTFALL}:`) ||
  reason.startsWith(`${SCRIPT_DECODE_RECORD_UNDERFUNDED}:`);
