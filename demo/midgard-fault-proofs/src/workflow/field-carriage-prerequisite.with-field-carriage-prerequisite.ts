import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { WorkflowActionChangedError } from "./action-changed.js";
import {
  FIELD_CARRIAGE_PREREQUISITE,
  type FieldCarriagePrerequisitePort,
  sameJson,
  TX_HASH,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { isCarriagePrerequisiteAction } from "./field-carriage-prerequisite.requirement-identity.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowPreflight,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { ProofMaterialsHolder } from "./proof-materials-view.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "./transaction-boundary.js";

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

// The workflow, and the objective it serves, that the step's needs belong to.
const holderOf = (
  workflowId: string,
  category: string,
  headerHash: string,
): ProofMaterialsHolder => ({
  workflowId,
  objective: { category, headerHash },
});

/** Adds field publication/certification in front of a family adapter. */
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
    Readonly<{ transaction: LocallyEvaluatedTransaction }>
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
    // The base names its own actions' alarm; this wrapper's own pause on
    // the shortfall itself.
    ...(base.unfunded === undefined
      ? {}
      : {
          unfunded: async (context) =>
            isPrerequisiteAction(context.action)
              ? context.shortfall
              : ((await base.unfunded?.(context)) ?? context.shortfall),
        }),
    observe: async (context) => {
      const observed = await base.observe(context);
      if (
        observed.kind !== "action_required" ||
        context.identity.target.kind !== "state_queue_header"
      ) {
        return observed;
      }
      const step = {
        headerHash: context.identity.target.headerHash,
        holder: holderOf(
          context.workflowId,
          category,
          context.identity.target.headerHash,
        ),
        baseAction: observed.action,
        artifact: context.artifact,
      };
      // A workflow under reconciliation-only authority builds nothing, but
      // what its step needs stays out of every sweep.
      if (context.reconciliationOnly === true) {
        await prerequisite.need(step);
        return observed;
      }
      const inspection = await prerequisite.inspect(step);
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
          holder: holderOf(
            context.workflowId,
            category,
            context.identity.target.headerHash,
          ),
          baseAction: context.action,
          artifact: context.artifact,
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
        holder: holderOf(
          context.workflowId,
          category,
          context.identity.target.headerHash,
        ),
        baseAction: observed.action,
        artifact: context.artifact,
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
        holder: holderOf(
          context.workflowId,
          category,
          context.identity.target.headerHash,
        ),
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
        captured.transaction.txHash !== context.preflight.txHash
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
  };
  return Object.freeze(adapter);
};
