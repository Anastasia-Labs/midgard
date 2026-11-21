import { SCRIPT_DECODE_RECORD_UNDERFUNDED } from "./script-decode-prerequisite.port.js";
import {
  WORKFLOW_FUNDING_SHORTFALL,
  WorkflowFundingUnavailableError,
} from "./workflow-funding-shortfall.js";

const FUNDING_UNAVAILABLE = new WorkflowFundingUnavailableError().message;

/**
 * Whether a workflow's pending reason says it paused because the prover
 * wallet cannot fund its next transaction, which a top-up resumes: a
 * funding shortfall of an action or of a script-decode record transaction,
 * or funding unavailable for a fresh transaction. A wait that only mentions
 * funding is not one.
 */
export const isWorkflowFundingPauseReason = (reason: string): boolean =>
  reason === FUNDING_UNAVAILABLE ||
  reason.startsWith(`${WORKFLOW_FUNDING_SHORTFALL}:`) ||
  reason.startsWith(`${SCRIPT_DECODE_RECORD_UNDERFUNDED}:`);
