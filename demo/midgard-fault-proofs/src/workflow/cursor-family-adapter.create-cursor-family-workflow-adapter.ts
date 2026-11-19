import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import type { CanonicalBlockClassification } from "./classification.js";
import {
  cacheKey,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyCapturedAction,
  type CursorFamilyTransactionPort,
  mutationLeaseRecovery,
  parseMutationLeaseRecovery,
  preflightOf,
  requiresMutationLease,
  sameJson,
} from "./cursor-family-adapter.admit-reference-scripts.js";
import {
  cursorFamilyObservation,
  type CursorFamilySpec,
  reconcileCursorFamilyAction,
} from "./cursor-family-state.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import { normalizeJournalJson } from "./journal.fraud-proof-workflow-terminal.js";
import type { JournalJsonObject } from "./journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowObservation,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { reconcileQueueLeaseFamily } from "./queue-lease-family-reconcile.js";
import { submitCapturedTransaction } from "./transaction-boundary.js";

/**
 * Crash-safe mechanics for explicitly specified bounded-step state
 * machines. This is not a production admission factory: each family must
 * still provide a fixed manifest-bound transaction port and admitted replay.
 */
export const createCursorFamilyWorkflowAdapter = <
  Category extends FraudProofCatalogueCategoryName,
>({
  spec,
  l1,
  transactions,
  stateQueueMutationLeaseCoordinator,
  refineAction,
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly l1: FraudProofFamilyL1ObservationPort<Category>;
  readonly transactions: CursorFamilyTransactionPort<Category>;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  /** Family grammar fields derived from this same authenticated observation.
   * Canonical cursor identity and spent references cannot be overridden. */
  readonly refineAction?: (input: {
    readonly observed: Awaited<
      ReturnType<FraudProofFamilyL1ObservationPort<Category>["observe"]>
    >;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }) => Promise<JournalJsonObject>;
}): FraudProofFamilyWorkflowAdapter => {
  const category = spec.category;
  if (
    l1.portVersion !== FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT ||
    l1.category !== category ||
    transactions.portVersion !== CURSOR_FAMILY_TRANSACTION_PORT ||
    transactions.category !== category
  ) {
    throw new Error(`${category} cursor-family ports changed identity`);
  }
  const prepared = new Map<string, CursorFamilyCapturedAction>();
  const leaseByTxHash = new Map<string, StateQueueMutationLease>();
  const current = async (
    headerHash: string,
    artifact: JournalJsonObject,
    reconciliationOnly = false,
  ) => {
    const observed = await l1.observe({ headerHash });
    let workflow: FraudProofWorkflowObservation = cursorFamilyObservation({
      spec,
      headerHash,
      provenance: observed.provenance,
      stage: observed.stage,
    });
    if (
      workflow.kind === "action_required" &&
      refineAction !== undefined &&
      !reconciliationOnly
    ) {
      const fields = normalizeJournalJson(
        await refineAction({ observed, action: workflow.action, artifact }),
      );
      if (
        typeof fields !== "object" ||
        fields === null ||
        Array.isArray(fields)
      )
        throw new Error(`${category} action refinement must return a record`);
      for (const key of Object.keys(fields)) {
        if (Object.prototype.hasOwnProperty.call(workflow.action.input, key))
          throw new Error(
            `${category} action refinement cannot override canonical input ${key}`,
          );
      }
      workflow = {
        kind: "action_required",
        action: {
          actionId: workflow.action.actionId,
          input: { ...workflow.action.input, ...(fields as JournalJsonObject) },
        },
      };
    }
    return { observed, workflow };
  };

  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({ evidence, classification, replayContext }) => {
      if (classification.category !== category) {
        throw new Error(`${category} port received another classification`);
      }
      return await transactions.prepare({
        evidence,
        ...(replayContext === undefined ? {} : { replayContext }),
        classification: classification as Extract<
          CanonicalBlockClassification,
          { readonly decision: "fault_detected" }
        > & { readonly category: Category },
      });
    },
    ...(transactions.prepareRaw === undefined
      ? {}
      : { prepareRaw: transactions.prepareRaw }),
    ...(transactions.validatePreparedRawArtifact === undefined
      ? {}
      : {
          validatePreparedRawArtifact: transactions.validatePreparedRawArtifact,
        }),
    ...(transactions.validatePreparedArtifact === undefined
      ? {}
      : {
          validatePreparedArtifact: async ({
            evidence,
            classification,
            replayContext,
            artifact,
          }: Parameters<
            NonNullable<
              FraudProofFamilyWorkflowAdapter["validatePreparedArtifact"]
            >
          >[0]) => {
            if (classification.category !== category)
              throw new Error(
                `${category} prepared validation received another classification`,
              );
            await transactions.validatePreparedArtifact!({
              evidence,
              artifact,
              ...(replayContext === undefined ? {} : { replayContext }),
              classification: classification as Extract<
                CanonicalBlockClassification,
                { readonly decision: "fault_detected" }
              > & { readonly category: Category },
            });
          },
        }),
    observe: async ({ identity, artifact, reconciliationOnly }) => {
      if (
        identity.category !== category ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(`${category} adapter received another identity`);
      }
      return (
        await current(identity.target.headerHash, artifact, reconciliationOnly)
      ).workflow;
    },
    preflight: async ({ identity, workflowId, artifact, action, entries }) => {
      if (
        identity.category !== category ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(`${category} preflight changed workflow identity`);
      }
      const snapshot = await current(identity.target.headerHash, artifact);
      if (
        snapshot.workflow.kind !== "action_required" ||
        !sameJson(snapshot.workflow.action, action)
      ) {
        throw new WorkflowActionChangedError(
          `${category} preflight differs from authenticated current L1 state`,
        );
      }
      const key = cacheKey(workflowId, action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} preflight already has an outstanding captured body`,
        );
      }
      const captured = await transactions.capture({
        action,
        artifact,
        entries,
      });
      if (
        requiresMutationLease(action) !==
        (captured.mutationLease !== undefined)
      ) {
        await captured.mutationLease?.fail(
          "authenticated removal topology disagreed with lease acquisition",
        );
        throw new Error(
          `${category} removal topology disagreed with its lease`,
        );
      }
      try {
        const preflight = preflightOf({
          category,
          action,
          transaction: captured.transaction,
          ...(captured.mutationLease === undefined
            ? {}
            : {
                durableRecovery: mutationLeaseRecovery(captured.mutationLease),
              }),
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
    submit: async ({ workflowId, action, preflight }) => {
      const key = cacheKey(workflowId, action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        captured.transaction.txHash !== preflight.txHash
      ) {
        throw new Error(`${category} submit has no exact captured body`);
      }
      const recovery = parseMutationLeaseRecovery(preflight.durableRecovery);
      if (
        (captured.mutationLease === undefined) !== (recovery === undefined) ||
        (captured.mutationLease !== undefined &&
          (captured.mutationLease.token !== recovery?.token ||
            captured.mutationLease.source !== recovery.source))
      ) {
        throw new Error(`${category} cached lease differs from durable intent`);
      }
      try {
        if (captured.mutationLease !== undefined) {
          leaseByTxHash.set(preflight.txHash, captured.mutationLease);
        }
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async ({
      identity,
      action,
      txHash,
      durableRecovery,
      signedTransactionCborHex,
      authorizeResubmission,
    }) => {
      if (
        identity.category !== category ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(`${category} reconcile changed workflow identity`);
      }
      const headerHash = identity.target.headerHash;
      const recovery = parseMutationLeaseRecovery(durableRecovery);
      if (requiresMutationLease(action) !== (recovery !== undefined)) {
        return {
          kind: "conflict",
          reason: `${category} durable lease disagrees with removal topology`,
        };
      }
      const observed = await l1.observe({ headerHash });
      return await reconcileQueueLeaseFamily({
        category,
        ...(txHash === undefined ? {} : { txHash }),
        ...(recovery === undefined ? {} : { recovery }),
        coordinator: stateQueueMutationLeaseCoordinator,
        leaseByTxHash,
        l1,
        ...(signedTransactionCborHex === undefined
          ? {}
          : { signedTransactionCborHex }),
        ...(authorizeResubmission === undefined
          ? {}
          : { authorizeResubmission }),
        read: async (recoverUnconfirmedTransaction) =>
          await reconcileCursorFamilyAction({
            spec,
            headerHash,
            action,
            ...(txHash === undefined ? {} : { txHash }),
            provenance: observed.provenance,
            stage: observed.stage,
            recoverUnconfirmedTransaction,
            transactionConfirmed: async (hash, removal) =>
              await l1.transactionConfirmed({
                headerHash,
                txHash: hash,
                ...(removal === undefined ? {} : { removal }),
              }),
          }),
      });
    },
  };
  return Object.freeze(adapter);
};
