import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowReconcileResult,
} from "../workflow/orchestrator.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
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
const recoveryLease = (value: JournalJsonObject | undefined) => {
  if (value === undefined) return undefined;
  const lease = value.stateQueueMutationLease as JournalJsonObject;
  if (
    Object.keys(value).join() !== "stateQueueMutationLease" ||
    lease === null ||
    typeof lease !== "object" ||
    Array.isArray(lease) ||
    Object.keys(lease).sort().join() !== "source,token" ||
    typeof lease.token !== "string" ||
    typeof lease.source !== "string" ||
    lease.token.length === 0 ||
    lease.source.length === 0
  )
    throw new Error("value conservation: malformed durable mutation lease");
  return { token: lease.token, source: lease.source };
};

/** Journal mechanics around a fixed manifest-bound conservation transaction port. */
export const createValueConservationAdapter = ({
  prepare,
  current,
  capture,
  confirmed,
  signedRecovery,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly prepare: FraudProofFamilyWorkflowAdapter["prepare"];
  readonly current: (
    context: Context,
  ) => Promise<FraudProofWorkflowObservation>;
  readonly capture: (
    context: ActionContext,
  ) => Promise<ConservationCapturedAction>;
  /** Reconstructs the exact input/output checkpoint from release-final raw L1. */
  readonly confirmed: (
    context: Parameters<FraudProofFamilyWorkflowAdapter["reconcile"]>[0] & {
      readonly txHash: string;
    },
  ) => Promise<boolean>;
  /** The source's read of an exact signed attempt. An attempt whose
   * checkpoint is not on chain is read from its own bytes, as every family's
   * is: a landing that failed phase 2, an expiry or an invalidation is
   * impossible at once, and only the bytes tell it from one in flight. */
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}): FraudProofFamilyWorkflowAdapter => {
  const captured = new Map<string, ConservationCapturedAction>();
  const leases = new Map<string, StateQueueMutationLease>();
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
        journalJsonDigest(observed.action) !== journalJsonDigest(context.action)
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
            ...(result.mutationLease === undefined
              ? {}
              : {
                  durableRecovery: {
                    stateQueueMutationLease: {
                      token: result.mutationLease.token,
                      source: result.mutationLease.source,
                    },
                  },
                }),
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
      const lease = recoveryLease(context.preflight.durableRecovery);
      if (
        (lease === undefined) !== (result.mutationLease === undefined) ||
        (lease !== undefined &&
          (lease.token !== result.mutationLease?.token ||
            lease.source !== result.mutationLease?.source))
      )
        throw new Error(
          "value conservation: mutation lease differs from intent",
        );
      try {
        if (result.mutationLease !== undefined)
          leases.set(result.transaction.txHash, result.mutationLease);
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(result.transaction),
        };
      } finally {
        captured.delete(transactionKey);
      }
    },
    reconcile: async (context) => {
      const recovery = recoveryLease(context.durableRecovery);
      let lease =
        context.txHash === undefined ? undefined : leases.get(context.txHash);
      if (lease === undefined && recovery !== undefined) {
        if (stateQueueMutationLeaseCoordinator.resume === undefined)
          return {
            kind: "conflict",
            reason: "value conservation: mutation lease cannot resume",
          };
        lease = await stateQueueMutationLeaseCoordinator.resume(recovery);
        if (context.txHash !== undefined) leases.set(context.txHash, lease);
      }
      if (context.txHash === undefined) {
        await lease?.renew();
        return { kind: "not_found" };
      }
      const { txHash, signedTransactionCborHex, authorizeResubmission } =
        context;
      try {
        if (await confirmed({ ...context, txHash })) {
          await lease?.release();
          leases.delete(txHash);
          return { kind: "confirmed", txHash };
        }
      } catch (cause) {
        await lease?.fail(String(cause));
        leases.delete(txHash);
        return { kind: "conflict", reason: String(cause) };
      }
      const read: FraudProofWorkflowReconcileResult =
        signedTransactionCborHex === undefined
          ? { kind: "pending", txHash }
          : await reconcileSignedWorkflowTransaction({
              transactionHash: txHash,
              signedTransactionCborHex,
              observe: (input) =>
                signedRecovery.observeSignedTransaction(input),
              rebroadcast: (input) =>
                signedRecovery.rebroadcastSignedTransaction(input),
              authorizeResubmission:
                authorizeResubmission === undefined
                  ? undefined
                  : async (signed) => {
                      await lease?.renew();
                      await authorizeResubmission(signed);
                    },
            });
      if (read.kind === "not_found") {
        await lease?.release();
        leases.delete(txHash);
      } else if (read.kind === "conflict") {
        await lease?.fail(read.reason);
        leases.delete(txHash);
      } else {
        await lease?.renew();
      }
      return read;
    },
  };
};
