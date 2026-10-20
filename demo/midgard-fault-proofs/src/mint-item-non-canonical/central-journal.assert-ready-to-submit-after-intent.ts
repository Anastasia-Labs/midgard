import {
  assertWorkflowFundingReservationReadyToSubmit,
  WorkflowFundingReservationUnavailableError,
} from "../workflow/funding-reservation-permit.js";
import { MintItemWorkflowRecoveryPendingError } from "./central-journal.reconcile-canonical-receipt.js";

/**
 * The funding check made after a submission intent is journaled. Once the
 * signed transaction is recorded, a reservation that is no longer available
 * is not a funding pause a top-up resolves: the recorded transaction must be
 * reconciled against the canonical chain first. The reason matches the
 * shared orchestrator's for the same check.
 */
export const assertMintItemReadyToSubmitAfterIntent = async ({
  journal,
  transactionHash,
}: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => {
  try {
    await assertWorkflowFundingReservationReadyToSubmit({
      journal,
      transactionHash,
    });
  } catch (cause) {
    if (cause instanceof WorkflowFundingReservationUnavailableError)
      throw new MintItemWorkflowRecoveryPendingError(
        `signed transaction ${transactionHash} requires canonical reconciliation: ${cause.message}`,
      );
    throw cause;
  }
};
