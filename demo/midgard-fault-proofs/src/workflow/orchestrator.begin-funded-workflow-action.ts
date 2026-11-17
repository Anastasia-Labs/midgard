import { WorkflowActionChangedError } from "./action-changed.js";
import {
  beginWorkflowFundingReservationAction,
  WorkflowFundingShortfallError,
} from "./funding-reservation-permit.js";
import { isFraudProofL1WaitError } from "./l1-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowAdapterContext,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";

/**
 * Begins `action` on the workflow's funding reservation. When the
 * reservation cannot fund it, the pause carries the action's own alarm
 * where the adapter names one; naming it never turns the pause into a
 * stall.
 */
export const beginFundedWorkflowAction = async ({
  journal,
  action,
  adapter,
  context,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
}): Promise<void> => {
  try {
    await beginWorkflowFundingReservationAction({ journal, action });
  } catch (failure) {
    if (
      !(failure instanceof WorkflowFundingShortfallError) ||
      adapter.unfunded === undefined
    )
      throw failure;
    let named: WorkflowFundingShortfallError = failure;
    try {
      named = await adapter.unfunded({
        ...context,
        action,
        shortfall: failure,
      });
    } catch (naming) {
      if (
        naming instanceof WorkflowActionChangedError ||
        isFraudProofL1WaitError(naming)
      )
        throw naming;
    }
    throw named;
  }
};
