import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { WorkflowActionChangedError } from "./action-changed.js";
import {
  sameJson,
  TX_HASH,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type { JournalJsonObject } from "./journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowPreflight,
} from "./orchestrator.js";
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
 * action: each Stage, Bind, Walk and Finish is a journaled action of the
 * workflow, funded, submitted, reconciled and resumed like any other. No
 * base action is preflighted while the record is incomplete.
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
    Readonly<{
      transaction: LocallyEvaluatedTransaction;
      durableRecovery: JournalJsonObject;
    }>
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
      // A held action is still the one required: its preflight pauses the
      // workflow until a fresh observation instead of polling the hold.
      if (inspection.kind === "required" || inspection.kind === "held") {
        return { kind: "action_required", action: inspection.action };
      }
      return inspection.kind === "pending"
        ? { kind: "pending", reason: inspection.reason }
        : observed;
    },
    preflight: async (context) => {
      const headerHash = headerOf(context.identity.target);
      if (!isScriptDecodePrerequisiteAction(context.action)) {
        const inspection = await prerequisite.inspect({
          headerHash,
          baseAction: context.action,
          artifact: context.artifact,
          entries: context.entries,
        });
        if (
          inspection.kind === "required" ||
          inspection.kind === "held" ||
          inspection.kind === "pending"
        ) {
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
        baseAction: observed.action,
        artifact: context.artifact,
        entries: context.entries,
      });
      if (
        inspection.kind === "held" &&
        sameJson(inspection.action, context.action)
      ) {
        throw new WorkflowActionChangedError(inspection.reason);
      }
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
        action: context.action,
        artifact: context.artifact,
        entries: context.entries,
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
          durableRecovery: captured.durableRecovery,
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
        captured.transaction.txHash !== context.preflight.txHash ||
        !sameJson(captured.durableRecovery, context.preflight.durableRecovery)
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
    reconcile: async (context) => {
      if (!isScriptDecodePrerequisiteAction(context.action)) {
        return await base.reconcile(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        return {
          kind: "conflict",
          reason: `${category} script decode prerequisite changed target`,
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
