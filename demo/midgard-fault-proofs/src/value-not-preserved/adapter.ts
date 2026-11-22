import type { StateQueueMutationLease } from "../remove-fraudulent-block.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { workflowJsonDigest } from "../workflow/identity.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
} from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "../workflow/transaction-boundary.js";

type Context = Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0];
type ActionContext = Parameters<
  FraudProofFamilyWorkflowAdapter["preflight"]
>[0];
export type ConservationCapturedAction = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly mutationLease?: StateQueueMutationLease;
};
/** Capture and submission around a fixed manifest-bound conservation
 * transaction port. */
export const createValueConservationAdapter = ({
  prepare,
  current,
  capture,
}: {
  readonly prepare: FraudProofFamilyWorkflowAdapter["prepare"];
  readonly current: (
    context: Context,
  ) => Promise<FraudProofWorkflowObservation>;
  readonly capture: (
    context: ActionContext,
  ) => Promise<ConservationCapturedAction>;
}): FraudProofFamilyWorkflowAdapter => {
  const captured = new Map<string, ConservationCapturedAction>();
  const key = (context: ActionContext) =>
    `${context.workflowId}\0${context.action.actionId}`;
  return {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: "valueNotPreserved",
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare,
    observe: current,
    preflight: async (context) => {
      const observed = await current(context);
      if (
        observed.kind !== "action_required" ||
        workflowJsonDigest(observed.action) !==
          workflowJsonDigest(context.action)
      )
        throw new WorkflowActionChangedError(
          "value conservation: requested action differs from authenticated current checkpoint",
        );
      const transactionKey = key(context);
      if (captured.has(transactionKey))
        throw new Error(
          "value conservation: outstanding preflight already exists",
        );
      const result = await capture(context);
      try {
        requireReferenceOnlyScriptWitnesses({
          transaction: result.transaction,
          label: "value conservation",
        });
        const preflight = bindWorkflowPreflightTransaction(
          {
            actionId: context.action.actionId,
            txHash: result.transaction.txHash,
            scriptExecution: "reference_scripts" as const,
            localUplcEvaluation: {
              status: "passed" as const,
              evaluator: LOCAL_UPLC_EVALUATOR,
            },
            referenceScripts: result.transaction.referenceScripts,
          },
          result.transaction.signed,
        );
        captured.set(transactionKey, result);
        return preflight;
      } catch (cause) {
        await result.mutationLease?.fail(
          `value conservation preflight failed: ${String(cause)}`,
        );
        throw cause;
      }
    },
    submit: async (context) => {
      const transactionKey = key(context);
      const result = captured.get(transactionKey);
      if (
        result === undefined ||
        result.transaction.txHash !== context.preflight.txHash
      )
        throw new Error(
          "value conservation: no matching locally evaluated body",
        );
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(result.transaction),
        };
      } finally {
        captured.delete(transactionKey);
        // The runner follows the submitted transaction on chain, so the
        // lease ends with its submission.
        await result.mutationLease?.release();
      }
    },
  };
};
