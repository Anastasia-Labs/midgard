import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLease } from "../remove-fraudulent-block.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import type { WorkflowJsonObject } from "./identity.js";
import { normalizeWorkflowJson, workflowJsonDigest } from "./identity.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
  FraudProofWorkflowReferenceScript,
} from "./orchestrator.js";
import type { ProofMaterialsHolder } from "./proof-materials-view.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const CURSOR_FAMILY_TRANSACTION_PORT =
  "midgard-production-cursor-family-transaction-port-v1" as const;

export type CursorFamilyCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: StateQueueMutationLease;
}>;

export interface CursorFamilyTransactionPort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof CURSOR_FAMILY_TRANSACTION_PORT;
  readonly category: Category;
  prepare(input: {
    readonly evidence: CanonicalBlockEvidence;
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    > & { readonly category: Category };
  }): Promise<WorkflowJsonObject>;
  prepareRaw?: FraudProofFamilyWorkflowAdapter["prepareRaw"];
  validatePreparedRawArtifact?: FraudProofFamilyWorkflowAdapter["validatePreparedRawArtifact"];
  validatePreparedArtifact?(
    input: Parameters<CursorFamilyTransactionPort<Category>["prepare"]>[0] & {
      readonly artifact: WorkflowJsonObject;
    },
  ): Promise<void>;
  capture(input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: WorkflowJsonObject;
    /** The workflow and objective that find proof materials for the step. */
    readonly holder: ProofMaterialsHolder;
  }): Promise<CursorFamilyCapturedAction>;
}

const TX_HASH = /^[0-9a-f]{64}$/u;

const SCRIPT_HASH = /^[0-9a-f]{56}$/u;

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const sameJson = (left: unknown, right: unknown): boolean =>
  left === undefined || right === undefined
    ? left === right
    : workflowJsonDigest(normalizeWorkflowJson(left)) ===
      workflowJsonDigest(normalizeWorkflowJson(right));

const admitReferenceScripts = ({
  category,
  transaction,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly transaction: LocallyEvaluatedTransaction;
}): readonly FraudProofWorkflowReferenceScript[] => {
  if (!TX_HASH.test(transaction.txHash)) {
    throw new Error(`${category} preflight returned a malformed body hash`);
  }
  if (transaction.signed.toHash().toLowerCase() !== transaction.txHash) {
    throw new Error(`${category} preflight body hash changed after capture`);
  }
  if (transaction.referenceScripts.length === 0) {
    throw new Error(
      `${category} production transaction did not use published reference scripts`,
    );
  }
  requireReferenceOnlyScriptWitnesses({
    transaction,
    label: `${category} production transaction`,
  });
  const referenceInputs = new Set(
    workflowTransactionReferenceInputOutRefs(transaction.signed),
  );
  const roles = new Set<string>();
  const outRefs = new Set<string>();
  for (const reference of transaction.referenceScripts) {
    if (
      reference.role.trim() !== reference.role ||
      reference.role.length === 0 ||
      !OUT_REF.test(reference.outRef) ||
      !SCRIPT_HASH.test(reference.scriptHash)
    ) {
      throw new Error(`${category} captured a malformed reference identity`);
    }
    if (roles.has(reference.role) || outRefs.has(reference.outRef)) {
      throw new Error(
        `${category} captured duplicate reference-script role or outRef`,
      );
    }
    if (!referenceInputs.has(reference.outRef)) {
      throw new Error(
        `${category} claimed a reference script absent from the signed body`,
      );
    }
    roles.add(reference.role);
    outRefs.add(reference.outRef);
  }
  return Object.freeze(
    transaction.referenceScripts.map((reference) =>
      Object.freeze({ ...reference }),
    ),
  );
};

export const preflightOf = ({
  category,
  action,
  transaction,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly action: FraudProofWorkflowAction;
  readonly transaction: LocallyEvaluatedTransaction;
}): FraudProofWorkflowPreflight =>
  bindWorkflowPreflightTransaction(
    {
      actionId: action.actionId,
      txHash: transaction.txHash,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: {
        status: "passed",
        evaluator: LOCAL_UPLC_EVALUATOR,
      },
      referenceScripts: admitReferenceScripts({ category, transaction }),
    },
    transaction.signed,
  );

export const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

export const requiresMutationLease = (
  action: FraudProofWorkflowAction,
): boolean =>
  action.input.requiresMutationLease === true &&
  (action.input.stage === "remove" ||
    (action.input.category === "transitionTrace" &&
      (action.input.stage === "step_07" || action.input.stage === "step_08")));
