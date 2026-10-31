import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { WorkflowActionChangedError } from "./action-changed.js";
import { type JournalJsonObject } from "./journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
} from "./orchestrator.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
} from "./orchestrator.js";
import { cacheKey } from "./proof-chunk-prerequisite.cache-key.js";
import {
  parseProofCarriageRecovery,
  proofCarriageRecovery,
} from "./proof-chunk-prerequisite.requirement-from-journaled-action.js";
import {
  isPublicationAction,
  PROOF_CHUNK_PREREQUISITE,
  type ProofCarriageRecovery,
  type ProofChunkPrerequisitePort,
  routeActionIdentity,
  sameJson,
  TX_HASH,
  withDirectFirstProofCarriageRoute,
} from "./proof-chunk-prerequisite.route-action-identity.js";
import {
  bindWorkflowPreflightTransaction,
  copyWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "./transaction-boundary.js";

/**
 * Adds a direct-first, journal-visible proof-carriage decision in front of an
 * exact family step. The wrapper first captures and locally evaluates the
 * complete direct transaction. It may publish proof chunks only when that
 * exact attempt fails with the release-bound CML max-transaction-size error.
 */
export const withProofChunkPrerequisite = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  base,
  prerequisite,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly prerequisite: ProofChunkPrerequisitePort<Category>;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    base.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER ||
    base.category !== category ||
    base.safety.evidenceSource !== FRAUD_PROOF_WORKFLOW_SAFETY.evidenceSource ||
    base.safety.scriptCarriage !== FRAUD_PROOF_WORKFLOW_SAFETY.scriptCarriage ||
    base.safety.localEvaluation !==
      FRAUD_PROOF_WORKFLOW_SAFETY.localEvaluation ||
    prerequisite.portVersion !== PROOF_CHUNK_PREREQUISITE ||
    prerequisite.category !== category
  ) {
    throw new Error(
      `${category} proof-chunk prerequisite ports changed identity`,
    );
  }
  type PreparedRoute =
    | Readonly<{
        kind: "direct";
        baseAction: FraudProofWorkflowAction;
        basePreflight: FraudProofWorkflowPreflight;
        durableRecovery: JournalJsonObject;
      }>
    | Readonly<{
        kind: "publication";
        transaction: LocallyEvaluatedTransaction;
        durableRecovery: JournalJsonObject;
      }>;
  const prepared = new Map<string, PreparedRoute>();
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
      if (inspection.kind === "pending") {
        return { kind: "pending", reason: inspection.reason };
      }
      return observed;
    },
    preflight: async (context) => {
      if (!isPublicationAction(context.action)) {
        if (context.identity.target.kind !== "state_queue_header") {
          throw new Error(
            `${category} proof prerequisite changed workflow target`,
          );
        }
        const inspection = await prerequisite.inspect({
          headerHash: context.identity.target.headerHash,
          baseAction: context.action,
          artifact: context.artifact,
          entries: context.entries,
        });
        if (inspection.kind === "required" || inspection.kind === "pending") {
          throw new WorkflowActionChangedError(
            `${category} proof step cannot bypass its direct-first carriage decision`,
          );
        }
        return await withDirectFirstProofCarriageRoute({
          action: context.action,
          route: inspection.kind === "satisfied" ? "publication" : "direct",
          run: async () => await base.preflight(context),
        });
      }
      if (context.identity.target.kind !== "state_queue_header") {
        throw new Error(`${category} proof carriage changed workflow target`);
      }
      const observed = await base.observe(context);
      if (observed.kind !== "action_required") {
        throw new WorkflowActionChangedError(
          `${category} proof carriage has no current base step`,
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
          `${category} proof carriage differs from the current requirement`,
        );
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} proof carriage already has an outstanding captured body`,
        );
      }
      const route = routeActionIdentity(context.action);
      if (context.action.input.category !== category) {
        throw new Error(`${category} proof carriage changed category`);
      }
      if (!sameJson(route.baseAction, observed.action)) {
        throw new Error(`${category} proof carriage changed its base action`);
      }
      let direct: FraudProofWorkflowPreflight;
      try {
        direct = await withDirectFirstProofCarriageRoute({
          action: observed.action,
          route: "direct",
          run: async () =>
            await base.preflight({
              ...context,
              action: observed.action,
            }),
        });
      } catch (cause) {
        const directCapacityFailure =
          prerequisite.classifyDirectCapacityFailure(cause);
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
          throw new Error(`${category} proof publication body hash is invalid`);
        }
        requireReferenceOnlyScriptWitnesses({
          transaction: captured.transaction,
          label: `${category} proof publication`,
        });
        const durableRecovery = proofCarriageRecovery({
          route: "publication",
          baseAction: observed.action,
          requirement: route.requirement,
          directCapacityFailure,
          publicationDurableRecovery: captured.durableRecovery,
        });
        prepared.set(
          key,
          Object.freeze({
            kind: "publication",
            transaction: captured.transaction,
            durableRecovery,
          }),
        );
        return bindWorkflowPreflightTransaction(
          {
            actionId: context.action.actionId,
            txHash: captured.transaction.txHash,
            scriptExecution: "none",
            localUplcEvaluation: {
              status: "passed",
              evaluator: LOCAL_UPLC_EVALUATOR,
            },
            referenceScripts: [],
            durableRecovery,
          } satisfies FraudProofWorkflowPreflight,
          captured.transaction.signed,
        );
      }
      const durableRecovery = proofCarriageRecovery({
        route: "direct",
        baseAction: observed.action,
        requirement: route.requirement,
        ...(direct.durableRecovery === undefined
          ? {}
          : { baseDurableRecovery: direct.durableRecovery }),
      });
      prepared.set(
        key,
        Object.freeze({
          kind: "direct",
          baseAction: observed.action,
          basePreflight: direct,
          durableRecovery,
        }),
      );
      return copyWorkflowPreflightTransaction({
        from: direct,
        to: {
          ...direct,
          actionId: context.action.actionId,
          durableRecovery,
        } satisfies FraudProofWorkflowPreflight,
      });
    },
    submit: async (context) => {
      if (!isPublicationAction(context.action)) {
        return await base.submit(context);
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        !sameJson(captured.durableRecovery, context.preflight.durableRecovery)
      ) {
        throw new Error(
          `${category} proof carriage has no exact locally evaluated body`,
        );
      }
      try {
        if (captured.kind === "direct") {
          if (captured.basePreflight.txHash !== context.preflight.txHash) {
            throw new Error(
              `${category} direct proof carriage changed transaction body`,
            );
          }
          return await base.submit({
            ...context,
            action: captured.baseAction,
            preflight: captured.basePreflight,
          });
        }
        if (captured.transaction.txHash !== context.preflight.txHash) {
          throw new Error(
            `${category} proof publication changed transaction body`,
          );
        }
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async (context) => {
      if (!isPublicationAction(context.action)) {
        return await base.reconcile(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        return {
          kind: "conflict",
          reason: `${category} proof publication changed workflow target`,
        };
      }
      let route: ReturnType<typeof routeActionIdentity>;
      let recovery: ProofCarriageRecovery;
      try {
        route = routeActionIdentity(context.action);
        if (context.action.input.category !== category) {
          throw new Error(`${category} proof carriage changed category`);
        }
        recovery = parseProofCarriageRecovery({
          value: context.durableRecovery,
          requirement: route.requirement,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      if (!sameJson(route.baseAction, recovery.baseAction)) {
        return {
          kind: "conflict",
          reason: `${category} proof carriage recovery changed its base action`,
        };
      }
      if (recovery.route === "direct") {
        return await base.reconcile({
          identity: context.identity,
          workflowId: context.workflowId,
          artifact: context.artifact,
          entries: context.entries,
          action: recovery.baseAction,
          ...(context.signedTransactionCborHex === undefined
            ? {}
            : { signedTransactionCborHex: context.signedTransactionCborHex }),
          ...(context.authorizeResubmission === undefined
            ? {}
            : { authorizeResubmission: context.authorizeResubmission }),
          ...(context.txHash === undefined ? {} : { txHash: context.txHash }),
          ...(recovery.baseDurableRecovery === undefined
            ? {}
            : { durableRecovery: recovery.baseDurableRecovery }),
        });
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
        durableRecovery: recovery.publicationDurableRecovery,
      });
    },
  };
  return Object.freeze(adapter);
};
