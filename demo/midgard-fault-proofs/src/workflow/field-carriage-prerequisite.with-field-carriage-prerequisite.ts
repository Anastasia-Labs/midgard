import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { WorkflowActionChangedError } from "./action-changed.js";
import {
  FIELD_CARRIAGE_PREREQUISITE,
  type FieldCarriagePrerequisitePort,
  sameJson,
  TX_HASH,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { isCarriagePrerequisiteAction } from "./field-carriage-prerequisite.requirement-identity.js";
import type { JournalJsonObject } from "./journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowPreflight,
} from "./orchestrator.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "./transaction-boundary.js";

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

/** Adds durable field publication/certification in front of a family adapter. */
export const withFieldCarriagePrerequisite = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  base,
  prerequisite,
  rawDatum = false,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly prerequisite: FieldCarriagePrerequisitePort<Category>;
  readonly rawDatum?: boolean;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    base.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER ||
    base.category !== category ||
    !sameJson(base.safety, FRAUD_PROOF_WORKFLOW_SAFETY) ||
    prerequisite.portVersion !== FIELD_CARRIAGE_PREREQUISITE ||
    prerequisite.category !== category
  ) {
    throw new Error(`${category} field prerequisite ports changed identity`);
  }
  const isPrerequisiteAction = (action: FraudProofWorkflowAction) =>
    isCarriagePrerequisiteAction(action, rawDatum);
  const prepared = new Map<
    string,
    Readonly<{
      transaction: LocallyEvaluatedTransaction;
      durableRecovery: JournalJsonObject;
    }>
  >();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async (input) => await base.prepare(input),
    ...(base.validatePreparedArtifact === undefined
      ? {}
      : { validatePreparedArtifact: base.validatePreparedArtifact }),
    ...(base.prepareRaw === undefined ? {} : { prepareRaw: base.prepareRaw }),
    ...(base.validatePreparedRawArtifact === undefined
      ? {}
      : { validatePreparedRawArtifact: base.validatePreparedRawArtifact }),
    observe: async (context) => {
      const observed = await base.observe(context);
      if (
        context.reconciliationOnly === true ||
        observed.kind !== "action_required" ||
        context.identity.target.kind !== "state_queue_header"
      ) {
        return observed;
      }
      const inspection = await prerequisite.inspect({
        headerHash: context.identity.target.headerHash,
        baseAction: observed.action,
        artifact: context.artifact,
        entries: context.entries,
      });
      if (inspection.kind === "required") {
        return { kind: "action_required", action: inspection.action };
      }
      return inspection.kind === "pending"
        ? { kind: "pending", reason: inspection.reason }
        : observed;
    },
    preflight: async (context) => {
      if (!isPrerequisiteAction(context.action)) {
        if (context.identity.target.kind !== "state_queue_header") {
          throw new Error(`${category} field prerequisite changed target`);
        }
        const inspection = await prerequisite.inspect({
          headerHash: context.identity.target.headerHash,
          baseAction: context.action,
          artifact: context.artifact,
          entries: context.entries,
        });
        if (inspection.kind === "required" || inspection.kind === "pending") {
          throw new WorkflowActionChangedError(
            `${category} proof step cannot bypass authenticated field carriage`,
          );
        }
        return await base.preflight(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        throw new Error(`${category} field prerequisite changed target`);
      }
      const observed = await base.observe(context);
      if (observed.kind !== "action_required") {
        throw new WorkflowActionChangedError(
          `${category} field prerequisite has no base action`,
        );
      }
      const inspection = await prerequisite.inspect({
        headerHash: context.identity.target.headerHash,
        baseAction: observed.action,
        artifact: context.artifact,
        entries: context.entries,
      });
      if (
        inspection.kind !== "required" ||
        !sameJson(inspection.action, context.action)
      ) {
        throw new WorkflowActionChangedError(
          `${category} field prerequisite differs from current requirement`,
        );
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} field prerequisite already captured this action`,
        );
      }
      const captured = await prerequisite.capture({
        headerHash: context.identity.target.headerHash,
        action: context.action,
        artifact: context.artifact,
      });
      if (
        !TX_HASH.test(captured.transaction.txHash) ||
        captured.transaction.signed.toHash().toLowerCase() !==
          captured.transaction.txHash
      ) {
        throw new Error(`${category} field prerequisite body hash is invalid`);
      }
      requireReferenceOnlyScriptWitnesses({
        transaction: captured.transaction,
        label: `${category} field prerequisite`,
      });
      prepared.set(key, captured);
      return bindWorkflowPreflightTransaction(
        {
          actionId: context.action.actionId,
          txHash: captured.transaction.txHash,
          scriptExecution:
            captured.transaction.referenceScripts.length === 0
              ? "none"
              : "reference_scripts",
          localUplcEvaluation: {
            status: "passed",
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: captured.transaction.referenceScripts,
          durableRecovery: captured.durableRecovery,
        } satisfies FraudProofWorkflowPreflight,
        captured.transaction.signed,
      );
    },
    submit: async (context) => {
      if (!isPrerequisiteAction(context.action)) {
        return await base.submit(context);
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        captured.transaction.txHash !== context.preflight.txHash ||
        !sameJson(captured.durableRecovery, context.preflight.durableRecovery)
      ) {
        throw new Error(
          `${category} field prerequisite has no exact captured body`,
        );
      }
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async (context) => {
      if (!isPrerequisiteAction(context.action)) {
        return await base.reconcile(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        return {
          kind: "conflict",
          reason: `${category} field prerequisite changed target`,
        };
      }
      return await prerequisite.reconcile({
        headerHash: context.identity.target.headerHash,
        ...(context.signedTransactionCborHex === undefined
          ? {}
          : { signedTransactionCborHex: context.signedTransactionCborHex }),
        ...(context.authorizeResubmission === undefined
          ? {}
          : { authorizeResubmission: context.authorizeResubmission }),
        action: context.action,
        artifact: context.artifact,
        ...(context.txHash === undefined ? {} : { txHash: context.txHash }),
        ...(context.durableRecovery === undefined
          ? {}
          : { durableRecovery: context.durableRecovery }),
      });
    },
  };
  return Object.freeze(adapter);
};
