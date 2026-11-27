/** The reason prefix of an action paused until the prover wallet is funded. */
export const WORKFLOW_FUNDING_SHORTFALL = "workflow-funding-shortfall" as const;

/**
 * What a transaction build throws when the wallet it was offered is short of
 * ada, which a top-up fixes: Lucid's coin selection missing lovelace only (for
 * the required assets, collateral or the change output's minimum ada), or a
 * proof step finding no plain-ADA coin large enough for its fee. A missing
 * native token, or an empty input list, is not a shortfall: no top-up of
 * plain ada supplies it.
 */
const SHORTFALL = new RegExp(
  [
    String.raw`does not have enough funds to cover the required assets: \{\s*"lovelace":\s*"\d+n"\s*\}`,
    String.raw`does not have enough funds to cover the required \d+ Lovelace collateral`,
    String.raw`does not have enough funds to cover required minimum ADA for change output`,
    String.raw`must contain a pure-ADA UTxO with at least \d+ lovelace for fees`,
  ].join("|"),
  "u",
);

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
 * The prover wallet cannot fund a fresh transaction now. The workflow pauses
 * and tries again at a later observation, against the wallet as it is then.
 */
export class WorkflowFundingUnavailableError extends Error {
  constructor() {
    super("prover funding is temporarily unavailable for a fresh transaction");
    this.name = "WorkflowFundingUnavailableError";
  }
}

/**
 * The prover wallet cannot pay for an action's transaction. Like any
 * unavailable funding, it pauses the workflow until a later observation: a
 * top-up resumes it. Funding is drawn from plain-ADA coins only, and the
 * reason says so: a coin holding a token is never spent.
 */
export class WorkflowFundingShortfallError extends WorkflowFundingUnavailableError {
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
  cause instanceof WorkflowFundingUnavailableError ||
  !isWorkflowFundingShortfall(cause)
    ? cause
    : new WorkflowFundingShortfallError(actionId, cause);
