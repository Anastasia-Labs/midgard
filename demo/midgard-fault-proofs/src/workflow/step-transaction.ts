import {
  type FaultProofTurn,
  isPrerequisiteTurn,
} from "../fault-proof-turn.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import type { LocallyEvaluatedTransaction } from "./transaction-boundary.js";

/**
 * Unwraps a builder's result into the step a workflow adapter submits, after
 * the adapter's own `check` of the built transaction.
 *
 * A workflow publishes and certifies a step's carriage as its own
 * prerequisite actions before the step is built, so a builder that still
 * returns a prerequisite turn met carriage the workflow did not settle. The
 * turn is refused as {@link WorkflowActionChangedError}, as a step that would
 * bypass its field carriage is, so the runner re-observes the workflow
 * instead of submitting a transaction outside its action graph.
 */
export const workflowStepTransaction = async <
  Step extends { readonly transaction: LocallyEvaluatedTransaction },
>(
  built: FaultProofTurn<Step>,
  label: string,
  check?: (transaction: LocallyEvaluatedTransaction) => void | Promise<void>,
): Promise<Step> => {
  if (isPrerequisiteTurn(built)) {
    throw new WorkflowActionChangedError(
      `${label} met unpublished carriage (${built.label}); the carriage is published by its own prerequisite action before the step is built`,
    );
  }
  await check?.(built.transaction);
  return built;
};
