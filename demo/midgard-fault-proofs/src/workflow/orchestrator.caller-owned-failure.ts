import { isWorkflowActuationRevokedError } from "./actuation-permit.js";
import { isFraudProofL1WaitError } from "./l1-source.js";

/**
 * A failure a run hands to its caller instead of recording it as its own
 * stall: an L1 source the caller waits on, or actuation authority the caller
 * revoked. A revocation recorded as a stall would read as a failed proof and
 * hold the objective, though only its permit ended.
 */
export const isCallerOwnedWorkflowFailure = (cause: unknown): boolean =>
  isFraudProofL1WaitError(cause) || isWorkflowActuationRevokedError(cause);
