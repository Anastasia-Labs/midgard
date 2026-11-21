import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { WorkflowActionChangedError } from "./action-changed.js";
import {
  sameJson,
  TX_HASH,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowPreflight,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { workflowProofMaterialsHolder } from "./proof-materials-view.holder.js";
import {
  isScriptDecodePrerequisiteAction,
  SCRIPT_DECODE_PREREQUISITE,
} from "./script-decode-prerequisite.actions.js";
import type { ScriptDecodePrerequisitePort } from "./script-decode-prerequisite.port.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "./transaction-boundary.js";

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

/**
 * Records the script decode a family's proof needs before its first proof
 * action: each Stage, Bind, Walk and Finish is an action of the workflow,
 * funded, submitted and followed like any other. No base action is
 * preflighted while the record is incomplete.
 */
export const withScriptDecodePrerequisite = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  base,
  prerequisite,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly prerequisite: ScriptDecodePrerequisitePort<Category>;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    base.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER ||
    base.category !== category ||
    !sameJson(base.safety, FRAUD_PROOF_WORKFLOW_SAFETY) ||
    prerequisite.portVersion !== SCRIPT_DECODE_PREREQUISITE ||
    prerequisite.category !== category
  ) {
    throw new Error(`${category} script decode prerequisite changed identity`);
  }
  const prepared = new Map<
    string,
    Readonly<{ transaction: LocallyEvaluatedTransaction }>
  >();
  const headerOf = (
    target: Parameters<
      FraudProofFamilyWorkflowAdapter["preflight"]
    >[0]["identity"]["target"],
  ): string => {
    if (target.kind !== "state_queue_header") {
      throw new Error(`${category} script decode prerequisite changed target`);
    }
    return target.headerHash;
  };
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
            isScriptDecodePrerequisiteAction(context.action)
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
      const holder = workflowProofMaterialsHolder(context);
      // A workflow under reconciliation-only authority builds nothing, but
      // what its record stages stays out of every sweep.
      if (context.reconciliationOnly === true) {
        await prerequisite.need({
          holder,
          baseAction: observed.action,
          artifact: context.artifact,
        });
        return observed;
      }
      const inspection = await prerequisite.inspect({
        headerHash: context.identity.target.headerHash,
        holder,
        baseAction: observed.action,
        artifact: context.artifact,
      });
      if (inspection.kind === "required")
        return { kind: "action_required", action: inspection.action };
      return inspection.kind === "pending"
        ? { kind: "pending", reason: inspection.reason }
        : observed;
    },
    preflight: async (context) => {
      const headerHash = headerOf(context.identity.target);
      if (!isScriptDecodePrerequisiteAction(context.action)) {
        const inspection = await prerequisite.inspect({
          headerHash,
          holder: workflowProofMaterialsHolder(context),
          baseAction: context.action,
          artifact: context.artifact,
        });
        if (inspection.kind === "required" || inspection.kind === "pending") {
          throw new WorkflowActionChangedError(
            `${category} proof step cannot bypass its script decode record`,
          );
        }
        return await base.preflight(context);
      }
      const observed = await base.observe(context);
      if (observed.kind !== "action_required") {
        throw new WorkflowActionChangedError(
          `${category} script decode prerequisite has no base action`,
        );
      }
      const inspection = await prerequisite.inspect({
        headerHash,
        holder: workflowProofMaterialsHolder(context),
        baseAction: observed.action,
        artifact: context.artifact,
      });
      if (
        inspection.kind !== "required" ||
        !sameJson(inspection.action, context.action)
      ) {
        throw new WorkflowActionChangedError(
          `${category} script decode prerequisite differs from current requirement`,
        );
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      // A capture whose submission never ran (its funding was unavailable)
      // is replaced by this fresh one.
      prepared.delete(key);
      const captured = await prerequisite.capture({
        headerHash,
        holder: workflowProofMaterialsHolder(context),
        action: context.action,
        artifact: context.artifact,
      });
      if (
        !TX_HASH.test(captured.transaction.txHash) ||
        captured.transaction.signed.toHash().toLowerCase() !==
          captured.transaction.txHash
      ) {
        throw new Error(
          `${category} script decode prerequisite body hash is invalid`,
        );
      }
      requireReferenceOnlyScriptWitnesses({
        transaction: captured.transaction,
        label: `${category} script decode prerequisite`,
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
      if (!isScriptDecodePrerequisiteAction(context.action)) {
        return await base.submit(context);
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        captured.transaction.txHash !== context.preflight.txHash
      ) {
        throw new Error(
          `${category} script decode prerequisite has no exact captured body`,
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
