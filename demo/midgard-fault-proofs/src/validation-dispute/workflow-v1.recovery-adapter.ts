import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { ValidationTraceChallenge } from "../workflow/challenge-authority.js";
import {
  cacheKey,
  mutationLeaseRecovery,
  parseMutationLeaseRecovery,
  preflightOf,
  requiresMutationLease,
  sameJson,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import { withFieldCarriagePrerequisite } from "../workflow/field-carriage-prerequisite.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { latestSubmissionIntent } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
} from "../workflow/orchestrator.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary.js";
import {
  planValidationTraceDisputeMove,
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeCapturedAction,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import { VALIDATION_TRACE_DISPUTE_CATEGORY } from "./workflow-family.js";
import { validationTraceFieldCarriageAction } from "./workflow-field-carriage.js";
import type { ManifestBoundValidationTraceDisputeWorkflow } from "./workflow-v1.create-manifest-bound-validation-trace-dispute-workflow.js";

export const preparedValidationTraceChallengeArtifact = (
  challenge: ValidationTraceChallenge,
): JournalJsonObject => ({
  schemaVersion: "midgard-validation-trace-dispute-prepared-v1",
  challengeDigest: challenge.challengeDigest,
  claimCbor: challenge.claimCbor,
  challengerDescriptorCbor: challenge.challengerDescriptorCbor,
});

export const VALIDATION_TRACE_COUNTERPARTY_WAIT =
  "validationTraceDispute awaiting counterparty until ";

type DisputeMechanics = Pick<
  ManifestBoundValidationTraceDisputeWorkflow,
  | "binding"
  | "challenge"
  | "material"
  | "l1"
  | "actuator"
  | "fieldCarriage"
  | "deriveStage"
>;

/** The interactive grammar uses the same durable lifecycle as every other family. */
export const createValidationTraceDisputeRecoveryAdapter = ({
  workflow,
  stateQueueMutationLeaseCoordinator,
}: {
  workflow: DisputeMechanics;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}): FraudProofFamilyWorkflowAdapter => {
  const category = VALIDATION_TRACE_DISPUTE_CATEGORY;
  const headerHash = workflow.binding.definition.headerHash;
  const prepared = new Map<string, ValidationTraceDisputeCapturedAction>();
  const leases = new Map<string, StateQueueMutationLease>();
  const assertContext = (context: FraudProofWorkflowAdapterContext) => {
    if (
      context.identity.category !== category ||
      context.identity.deploymentFingerprint !==
        workflow.binding.deploymentFingerprint ||
      context.identity.target.kind !== "state_queue_header" ||
      context.identity.target.headerHash !== headerHash
    )
      throw new Error(
        "validationTraceDispute adapter changed workflow identity",
      );
  };
  const assertFreshArtifact = (artifact: JournalJsonObject) => {
    if (
      workflow.challenge === undefined ||
      workflow.material === undefined ||
      !sameJson(
        artifact,
        preparedValidationTraceChallengeArtifact(workflow.challenge),
      )
    )
      throw new Error(
        "validationTraceDispute requires its freshly admitted challenge",
      );
  };
  const assertRecovery = (
    recovery: JournalJsonObject | undefined,
    action: FraudProofWorkflowAction,
    artifact: JournalJsonObject,
  ): JournalJsonObject => {
    if (
      recovery === undefined ||
      recovery.challengeDigest !== artifact.challengeDigest ||
      recovery.actionDigest !== journalJsonDigest(action)
    )
      throw new Error(
        "validationTraceDispute recovery changed its challenge or action",
      );
    return recovery;
  };
  const retainedRoute = (context: FraudProofWorkflowAdapterContext) => {
    // Publication intents have their own recovery schema. Retain the last game
    // action's material across those prerequisites, and clear it on cancellation.
    const intent = [...context.entries]
      .reverse()
      .find(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.durableRecovery?.challengeDigest !== undefined,
      )?.event;
    if (intent?.kind !== "submission_intent") return undefined;
    const recovery = assertRecovery(
      intent.durableRecovery,
      {
        actionId: intent.actionId,
        input: intent.actionInput,
      },
      context.artifact,
    );
    return recovery.durableRouteInput as
      | ValidationTraceDisputeRetainedRouteInput
      | undefined;
  };
  const workflowAction = async (
    context: FraudProofWorkflowAdapterContext,
    action: ValidationTraceDisputeActuatorAction,
  ): Promise<FraudProofWorkflowAction> => {
    if (action.stage === "init") {
      // A cancelled route consumes the old init's effects without rolling back
      // its inclusion. Bind the next init to that exact completed cancellation.
      for (const { event } of [...context.entries].reverse()) {
        if (event.kind !== "confirmed") continue;
        const intent = latestSubmissionIntent(context.entries, event.actionId);
        if (
          intent?.actionInput.stage !== "cancel_semantic_route" ||
          intent.txHash !== event.txHash ||
          !(await workflow.l1.transactionConfirmed({
            headerHash,
            txHash: event.txHash,
          }))
        )
          continue;
        assertRecovery(
          intent.durableRecovery,
          { actionId: intent.actionId, input: intent.actionInput },
          context.artifact,
        );
        const threadOutRef = intent.actionInput.threadOutRef;
        if (typeof threadOutRef !== "string")
          throw new Error(
            "validationTraceDispute cancellation omitted its thread",
          );
        return validationTraceFieldCarriageAction(action, {
          txHash: event.txHash,
          threadOutRef,
        });
      }
    }
    return validationTraceFieldCarriageAction(action);
  };
  const current = async (context: FraudProofWorkflowAdapterContext) => {
    assertContext(context);
    const retained = retainedRoute(context);
    const stage = await workflow.deriveStage(Date.now());
    let move = planValidationTraceDisputeMove({
      stage,
      ...(retained === undefined ? {} : { retained }),
    });
    if (
      context.reconciliationOnly !== true &&
      move.kind === "act" &&
      move.action.stage === "semantic_resolution" &&
      retained?.fieldCarriageBinding !== undefined
    ) {
      assertFreshArtifact(context.artifact);
      const inspection = await workflow.fieldCarriage.prerequisite.inspect({
        headerHash,
        baseAction: await workflowAction(context, move.action),
        artifact: context.artifact,
        entries: context.entries,
      });
      if (inspection.kind === "required")
        move = {
          kind: "act",
          action: {
            stage: "cancel_semantic_route",
            threadOutRef: move.action.threadOutRef,
            group: "proof_item",
          },
        };
    }
    return { stage, move, retained };
  };
  const base: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async () => {
      if (workflow.challenge === undefined)
        throw new Error(
          "validationTraceDispute requires its freshly admitted challenge",
        );
      return preparedValidationTraceChallengeArtifact(workflow.challenge);
    },
    observe: async (context) => {
      const { move } = await current(context);
      if (move.kind === "completed") {
        // Lucid's cursor alone cannot close funding or authenticate economics.
        const observed = await workflow.l1.observe({ headerHash });
        return observed.stage.kind === "removed"
          ? { kind: "completed", terminal: observed.stage.terminal }
          : {
              kind: "pending",
              reason:
                "validationTraceDispute awaits authenticated removal effects",
            };
      }
      if (move.kind === "await_counterparty")
        return {
          kind: "pending",
          reason: `${VALIDATION_TRACE_COUNTERPARTY_WAIT}${move.responseDeadline}`,
        };
      if (context.reconciliationOnly !== true)
        assertFreshArtifact(context.artifact);
      return {
        kind: "action_required",
        action: await workflowAction(context, move.action),
      };
    },
    preflight: async (context) => {
      assertFreshArtifact(context.artifact);
      const { move, retained } = await current(context);
      if (
        move.kind !== "act" ||
        !sameJson(await workflowAction(context, move.action), context.action)
      )
        throw new WorkflowActionChangedError(
          "validationTraceDispute action changed before capture",
        );
      const key = cacheKey(context.workflowId, context.action.actionId);
      if (prepared.has(key))
        throw new Error("validationTraceDispute already captured this action");
      const captured = await workflow.actuator.capture({
        action: move.action,
        material: workflow.material!,
        ...(retained === undefined ? {} : { retained }),
      });
      try {
        if (
          requiresMutationLease(context.action) !==
          (captured.mutationLease !== undefined)
        )
          throw new Error(
            "validationTraceDispute removal topology disagrees with its lease",
          );
        const route =
          move.action.stage === "cancel_semantic_route"
            ? undefined
            : (captured.durableRouteInput ?? retained);
        const preflight = preflightOf({
          category,
          action: context.action,
          transaction: captured.transaction,
          durableRecovery: {
            challengeDigest: context.artifact.challengeDigest,
            actionDigest: journalJsonDigest(context.action),
            ...(route === undefined ? {} : { durableRouteInput: route }),
            ...(captured.mutationLease === undefined
              ? {}
              : mutationLeaseRecovery(captured.mutationLease)),
          },
        });
        prepared.set(key, captured);
        return preflight;
      } catch (cause) {
        await captured.mutationLease?.fail(
          `preflight admission failed before durable intent: ${String(cause)}`,
        );
        throw cause;
      }
    },
    submit: async (context) => {
      const key = cacheKey(context.workflowId, context.action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        captured.transaction.txHash !== context.preflight.txHash
      )
        throw new Error(
          "validationTraceDispute submit has no exact captured body",
        );
      assertRecovery(
        context.preflight.durableRecovery,
        context.action,
        context.artifact,
      );
      try {
        if (captured.mutationLease !== undefined)
          leases.set(context.preflight.txHash, captured.mutationLease);
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async (context) => {
      assertContext(context);
      const recovery = assertRecovery(
        context.durableRecovery,
        context.action,
        context.artifact,
      );
      const leaseRecovery =
        recovery.stateQueueMutationLease === undefined
          ? undefined
          : parseMutationLeaseRecovery({
              stateQueueMutationLease: recovery.stateQueueMutationLease,
            });
      if (
        requiresMutationLease(context.action) !==
        (leaseRecovery !== undefined)
      )
        return {
          kind: "conflict",
          reason:
            "validationTraceDispute durable lease disagrees with removal topology",
        };
      const txHash = context.txHash;
      let lease = txHash === undefined ? undefined : leases.get(txHash);
      const restoreLease = async () => {
        if (lease !== undefined || leaseRecovery === undefined) return;
        if (stateQueueMutationLeaseCoordinator.resume === undefined)
          throw new Error(
            "validationTraceDispute coordinator cannot resume durable lease",
          );
        lease = await stateQueueMutationLeaseCoordinator.resume(leaseRecovery);
        if (txHash !== undefined) leases.set(txHash, lease);
      };
      const { stage } = await current(context);
      const input = context.action.input;
      const included =
        txHash !== undefined &&
        (await workflow.l1.transactionConfirmed({
          headerHash,
          txHash,
          ...(input.stage !== "remove"
            ? {}
            : {
                removal: {
                  inputOutRef: input.nextRemovalOutRef as string,
                  targetOutRef: input.stateQueueBlockOutRef as string,
                  proofOutRef: input.fraudProofOutRef as string,
                  ...(stage.kind !== "proof_token"
                    ? {}
                    : {
                        continuation: {
                          targetOutRef: stage.stateQueueBlockOutRef,
                          nextRemovalOutRef: stage.nextRemovalOutRef,
                        },
                      }),
                },
              }),
        }));
      const result = included
        ? { kind: "confirmed" as const, txHash: txHash! }
        : txHash === undefined
          ? {
              kind: "unknown" as const,
              reason:
                "validationTraceDispute intent omitted its exact signed transaction hash",
            }
          : await reconcileSignedWorkflowTransaction({
              transactionHash: txHash,
              signedTransactionCborHex: context.signedTransactionCborHex,
              observe: workflow.l1.observeSignedTransaction,
              rebroadcast: workflow.l1.rebroadcastSignedTransaction,
              authorizeResubmission:
                context.authorizeResubmission === undefined
                  ? undefined
                  : async (signed) => {
                      const { move } = await current(context);
                      if (
                        move.kind !== "act" ||
                        !sameJson(
                          await workflowAction(context, move.action),
                          context.action,
                        )
                      )
                        throw new Error(
                          "validationTraceDispute cannot replay an action after its chain cursor changed",
                        );
                      await restoreLease();
                      await lease?.renew();
                      await context.authorizeResubmission!(signed);
                    },
            });
      try {
        await restoreLease();
      } catch (cause) {
        if (result.kind === "pending" || result.kind === "unknown")
          return {
            kind: "unknown",
            reason: `validationTraceDispute durable lease cannot resume: ${String(cause)}`,
          };
      }
      if (result.kind === "confirmed" || result.kind === "not_found")
        await lease?.release();
      else if (result.kind === "conflict") await lease?.fail(result.reason);
      else await lease?.renew();
      if (
        txHash !== undefined &&
        (result.kind === "confirmed" ||
          result.kind === "not_found" ||
          result.kind === "conflict")
      )
        leases.delete(txHash);
      return result;
    },
  };
  return withFieldCarriagePrerequisite({
    category,
    base,
    prerequisite: workflow.fieldCarriage.prerequisite,
  });
};
