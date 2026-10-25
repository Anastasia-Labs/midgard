/** Actions of ours that run no script of the dispute: transactions our key
 * alone signs (a field copy spent back, an operand staged or spent back),
 * put in front of a family adapter. Each is taken in place of what the
 * adapter observes when `next` says so, or in place of a resolution by
 * another record when `resolved` says so, built from the live chain at its
 * preflight, and reconciled from its exact signed bytes. A build that fails
 * is the action's recorded, retried outcome for its workflow alone; a wait on
 * the L1 source goes to the orchestrator as a wait. */
import type { TxSigned } from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import type {
  JournalJsonObject,
  JournalJsonValue,
} from "../workflow/journal.js";
import {
  type FraudProofSignedTransactionRecovery,
  isFraudProofL1WaitError,
} from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { WorkflowActionUnbuiltError } from "../workflow/orchestrator.unbuilt-actions.js";
import { isFraudProofResolvedByAnotherRecordError } from "../workflow/resolved-by-another-record.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
} from "../workflow/transaction-boundary.js";

export type OwnKeyActions = Readonly<{
  /** Whether `action` is one of ours. */
  owns: (action: FraudProofWorkflowAction) => boolean;
  /** What to do instead of `observed`, the adapter's observation; none to
   * keep it. */
  next: (
    context: FraudProofWorkflowAdapterContext,
    observed: FraudProofWorkflowObservation,
  ) => Promise<FraudProofWorkflowObservation | undefined>;
  /** As `next`, for a workflow under reconciliation-only authority: the
   * spend back to take instead of `observed`, the one action of ours such
   * a workflow still takes; none to keep it. */
  reclaim?: (
    context: FraudProofWorkflowAdapterContext,
    observed: FraudProofWorkflowObservation,
  ) => Promise<FraudProofWorkflowObservation | undefined>;
  /** What to take before the adapter's resolution by another record goes
   * through: a spend back, or the wait on one; none to let it through. */
  resolved?: (
    context: FraudProofWorkflowAdapterContext,
  ) => Promise<FraudProofWorkflowObservation | undefined>;
  /** The signed transaction of `action`, and what its recovery keeps
   * besides the signed bytes. */
  build: (
    context: FraudProofWorkflowAdapterContext,
    action: FraudProofWorkflowAction,
  ) => Promise<
    Readonly<{ signed: TxSigned; recovery?: Record<string, JournalJsonValue> }>
  >;
}>;

/** The signed bytes the recovery of an action of ours keeps. */
export const ownKeySignedBytes = (
  durableRecovery: JournalJsonObject | undefined,
): string | undefined => {
  const recorded = durableRecovery?.signedTransactionCborHex;
  return typeof recorded === "string" ? recorded : undefined;
};

/** Whether a transaction, where `observed` says it stands, is final at
 * release depth. */
export const isFinalAtReleaseDepth = (
  observed: Awaited<
    ReturnType<FraudProofSignedTransactionRecovery["observeSignedTransaction"]>
  >,
): boolean =>
  observed.status === "included" &&
  observed.inclusionPoint !== undefined &&
  BigInt(observed.inclusionPoint.blockNo) <=
    BigInt(observed.releaseFinalPoint.blockNo);

/** Whether the transaction `txHash`, signed as `signed`, is final at
 * release depth. */
export const finalAtReleaseDepth = async (
  signedRecovery: FraudProofSignedTransactionRecovery,
  txHash: string,
  signed: string,
): Promise<boolean> =>
  isFinalAtReleaseDepth(
    await signedRecovery.observeSignedTransaction({
      transactionHash: txHash,
      signedTransactionCborHex: signed,
    }),
  );

/** `base` with the actions of `own` in front of it. */
export const withOwnKeyActions = ({
  base,
  own,
  signedRecovery,
  label,
}: {
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly own: OwnKeyActions;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  /** What the actions are, for their messages. */
  readonly label: string;
}): FraudProofFamilyWorkflowAdapter => {
  const prepared = new Map<string, LocallyEvaluatedTransaction>();
  const keyOf = (workflowId: string, actionId: string) =>
    `${workflowId}\u0000${actionId}`;
  const observe = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation> => {
    let observed: FraudProofWorkflowObservation;
    try {
      observed = await base.observe(context);
    } catch (cause) {
      if (
        own.resolved === undefined ||
        !isFraudProofResolvedByAnotherRecordError(cause)
      )
        throw cause;
      const instead = await own.resolved(context);
      if (instead === undefined) throw cause;
      return instead;
    }
    if (context.reconciliationOnly === true)
      return (await own.reclaim?.(context, observed)) ?? observed;
    return (await own.next(context, observed)) ?? observed;
  };
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe,
    preflight: async (context) => {
      if (!own.owns(context.action)) return await base.preflight(context);
      const { actionId } = context.action;
      const current = await observe(context);
      if (
        current.kind !== "action_required" ||
        current.action.actionId !== actionId
      )
        throw new WorkflowActionChangedError(
          `validation-trace ${actionId} is no longer the next action`,
        );
      const key = keyOf(context.workflowId, actionId);
      prepared.delete(key);
      let built: Awaited<ReturnType<OwnKeyActions["build"]>>;
      try {
        built = await own.build(context, context.action);
      } catch (cause) {
        if (
          cause instanceof WorkflowActionChangedError ||
          isWorkflowFundingShortfall(cause) ||
          isFraudProofL1WaitError(cause)
        )
          throw cause;
        throw new WorkflowActionUnbuiltError(
          actionId,
          `validation-trace ${actionId} does not build: ${String(cause)}`,
        );
      }
      const transaction: LocallyEvaluatedTransaction = {
        txHash: built.signed.toHash().toLowerCase(),
        signed: built.signed,
        referenceScripts: [],
      };
      prepared.set(key, transaction);
      return bindWorkflowPreflightTransaction(
        {
          actionId,
          txHash: transaction.txHash,
          scriptExecution: "none" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: [],
          durableRecovery: {
            ...built.recovery,
            signedTransactionCborHex: transaction.signed.toCBOR(),
          },
        },
        transaction.signed,
      );
    },
    submit: async (context) => {
      if (!own.owns(context.action)) return await base.submit(context);
      const key = keyOf(context.workflowId, context.action.actionId);
      const transaction = prepared.get(key);
      if (
        transaction === undefined ||
        transaction.txHash !== context.preflight.txHash
      )
        throw new Error(
          `validation-trace ${context.action.actionId} has no captured body to submit`,
        );
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async (context) => {
      if (!own.owns(context.action)) return await base.reconcile(context);
      const { txHash, durableRecovery } = context;
      if (txHash === undefined)
        return {
          kind: "conflict",
          reason: `validation-trace ${label} omitted its transaction hash`,
        };
      const signed =
        context.signedTransactionCborHex ?? ownKeySignedBytes(durableRecovery);
      const reconciled = await reconcileSignedWorkflowTransaction({
        transactionHash: txHash,
        ...(signed === undefined ? {} : { signedTransactionCborHex: signed }),
        observe: signedRecovery.observeSignedTransaction,
        rebroadcast: signedRecovery.rebroadcastSignedTransaction,
        ...(context.authorizeResubmission === undefined
          ? {}
          : { authorizeResubmission: context.authorizeResubmission }),
        reportInclusion: true,
      });
      return reconciled.kind === "pending" &&
        reconciled.retirement?.reason === "included"
        ? { kind: "confirmed", txHash }
        : reconciled;
    },
  };
  return Object.freeze(adapter);
};
