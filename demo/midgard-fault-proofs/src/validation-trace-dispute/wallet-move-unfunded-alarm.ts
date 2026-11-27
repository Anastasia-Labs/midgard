import { WorkflowFundingShortfallError } from "../workflow/workflow-funding-shortfall.js";

/** The reason prefix of a validation dispute's thread move blocked on our
 * wallet's funds: an alarm, named for the operator. */
export const VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED =
  "validation-trace-wallet-move-unfunded" as const;

/**
 * A thread move of a validation dispute case that our wallet must pay, and
 * that does not build for want of our wallet's ada: the operator's alarm,
 * naming the case, the thread and the deadline by which the move must land.
 * Like any funding shortfall, the run tries again at its next observation,
 * so a top-up lands the move.
 */
export class ValidationDisputeWalletMoveUnfundedError extends WorkflowFundingShortfallError {
  constructor(
    input: Readonly<{
      actionId: string;
      caseUnit: string;
      threadOutRef: string;
      /** When the move must land by, if known. */
      deadlineMs: number | undefined;
      cause: unknown;
    }>,
  ) {
    super(input.actionId, input.cause);
    this.name = "ValidationDisputeWalletMoveUnfundedError";
    const deadline =
      input.deadlineMs === undefined
        ? "its deadline"
        : `its deadline ${input.deadlineMs.toString()}`;
    this.message = `${VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED}: the prover wallet cannot fund ${input.actionId} on thread ${input.threadOutRef} of case ${input.caseUnit}, which must land before ${deadline}; top up the wallet's plain-ADA coins: ${this.message}`;
  }
}
