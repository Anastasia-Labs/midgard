// The leaf module, not the barrel: the barrel can still be evaluating when
// a family assembly loads this class, and a class must extend a defined value.
import { WorkflowFundingReservationUnavailableError } from "./funding-reservation-permit.workflow-funding-reservation-port.js";

/** The reason prefix of an action paused until the prover wallet is funded. */
export const WORKFLOW_FUNDING_SHORTFALL = "workflow-funding-shortfall" as const;

/**
 * What a transaction build throws when the wallet it was offered cannot pay
 * for it: Lucid's coin selection (inputs, collateral or the change output's
 * minimum ada), an empty input list handed to it, or a proof step finding no
 * plain-ADA coin large enough for its fee.
 */
const SHORTFALL =
  /EMPTY_UTXO|does not have enough funds|must contain a pure-ADA UTxO with at least \d+ lovelace for fees/u;

const describe = (value: unknown): string => {
  if (value instanceof Error) return value.message;
  if (typeof value === "string") return value;
  try {
    return JSON.stringify(value) ?? "";
  } catch {
    return "";
  }
};

/** True when a build failed for want of wallet funds, at any cause depth. */
export const isWorkflowFundingShortfall = (cause: unknown): boolean => {
  let at = cause;
  for (let depth = 0; at !== undefined && at !== null && depth < 8; depth++) {
    if (SHORTFALL.test(describe(at))) return true;
    at = at instanceof Error ? at.cause : undefined;
  }
  return false;
};

/**
 * The prover wallet cannot pay for an action's transaction. Like any
 * unavailable funding, it releases the idle reservation and pauses the
 * workflow until a later observation, which reserves the wallet as it is
 * then: a top-up resumes it. Funding is drawn from plain-ADA coins only,
 * and the reason says so: a coin holding a token is never spent.
 */
export class WorkflowFundingShortfallError extends WorkflowFundingReservationUnavailableError {
  constructor(
    readonly actionId: string,
    override readonly cause: unknown,
  ) {
    super();
    this.name = "WorkflowFundingShortfallError";
    this.message = `${WORKFLOW_FUNDING_SHORTFALL}: the prover wallet cannot fund ${actionId} from its plain-ADA coins (a coin holding a token is never spent): ${describe(cause)}`;
  }
}

/**
 * `cause` as a preflight failure of `actionId`: a funding shortfall becomes
 * the resumable funding-unavailable error; anything else, including a
 * failure already typed as unavailable funding, is returned unchanged.
 */
export const asWorkflowFundingShortfall = (
  actionId: string,
  cause: unknown,
): unknown =>
  cause instanceof WorkflowFundingReservationUnavailableError ||
  !isWorkflowFundingShortfall(cause)
    ? cause
    : new WorkflowFundingShortfallError(actionId, cause);
