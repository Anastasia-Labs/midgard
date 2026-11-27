import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLease } from "../remove-fraudulent-block.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import {
  normalizeWorkflowJson,
  workflowJsonDigest,
  type WorkflowJsonObject,
} from "./identity.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import { linearFamilyObservation } from "./linear-family-state.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
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
  submitCapturedTransaction,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const LINEAR_FAMILY_TRANSACTION_PORT =
  "midgard-production-linear-family-transaction-port-v1" as const;

export type LinearFamilyCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  /** Present only when descendant removal acquired the shared queue fence. */
  mutationLease?: StateQueueMutationLease;
}>;

export interface LinearFamilyTransactionPort<
  Category extends LinearFamilyCategory,
> extends Pick<
    FraudProofFamilyWorkflowAdapter,
    "prepareRaw" | "validatePreparedRawArtifact"
  > {
  readonly portVersion: typeof LINEAR_FAMILY_TRANSACTION_PORT;
  readonly category: Category;
  /** Pure public-evidence preparation; no operator file/API input. */
  prepare(input: {
    readonly evidence: CanonicalBlockEvidence;
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    > & { readonly category: Category };
  }): Promise<WorkflowJsonObject>;
  /**
   * Invokes the exact family builder, which returns the signed body without
   * submitting it: the body has passed local UPLC and has not reached
   * provider I/O.
   */
  capture(input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: WorkflowJsonObject;
    /** The workflow and objective that find proof materials for the step. */
    readonly holder: ProofMaterialsHolder;
  }): Promise<LinearFamilyCapturedAction>;
}

const TX_HASH = /^[0-9a-f]{64}$/u;
const SCRIPT_HASH = /^[0-9a-f]{56}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

// The orchestrator hands back the journal-normalized (key-sorted) action, so
// equality must be structural, not byte order.
const sameJson = (left: unknown, right: unknown): boolean =>
  workflowJsonDigest(normalizeWorkflowJson(left)) ===
  workflowJsonDigest(normalizeWorkflowJson(right));

const assertExactCurrentAction = ({
  expected,
  requested,
  category,
}: {
  readonly expected: ReturnType<typeof linearFamilyObservation>;
  readonly requested: FraudProofWorkflowAction;
  readonly category: FraudProofCatalogueCategoryName;
}): void => {
  if (
    expected.kind !== "action_required" ||
    !sameJson(expected.action, requested)
  ) {
    throw new WorkflowActionChangedError(
      `${category} preflight action differs from authenticated current L1 state`,
    );
  }
};

const admitReferenceScripts = ({
  transaction,
  category,
}: {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly category: FraudProofCatalogueCategoryName;
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

const preflightOf = ({
  action,
  transaction,
  category,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly category: FraudProofCatalogueCategoryName;
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
      referenceScripts: admitReferenceScripts({ transaction, category }),
    },
    transaction.signed,
  );

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

const requiresMutationLease = (
  category: LinearFamilyCategory,
  action: FraudProofWorkflowAction,
): boolean =>
  ((category === "fabricatedDeposit" || category === "fabricatedWithdrawal") &&
    action.input.stage === "step_04") ||
  (action.input.stage === "remove" &&
    action.input.requiresMutationLease === true);

/**
 * Shared crash-safe adapter mechanics for exact 1–4 step families. This
 * factory is intentionally not a production-runner admission boundary: a
 * fixed-category manifest-bound factory must supply the actual builder port.
 */
export const createLinearFamilyWorkflowAdapter = <
  Category extends LinearFamilyCategory,
>({
  category,
  l1,
  transactions,
}: {
  readonly category: Category;
  readonly l1: FraudProofFamilyL1ObservationPort<Category>;
  readonly transactions: LinearFamilyTransactionPort<Category>;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    l1.portVersion !== FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT ||
    l1.category !== category ||
    transactions.portVersion !== LINEAR_FAMILY_TRANSACTION_PORT ||
    transactions.category !== category
  ) {
    throw new Error(`${category} linear workflow ports changed identity`);
  }
  const prepared = new Map<string, LinearFamilyCapturedAction>();
  const current = async (headerHash: string) => {
    const observed = await l1.observe({ headerHash });
    return {
      observed,
      workflow: linearFamilyObservation({
        category,
        headerHash,
        provenance: observed.provenance,
        stage: observed.stage,
      }),
    };
  };

  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({ evidence, replayContext, classification }) => {
      if (classification.category !== category) {
        throw new Error(
          `${category} transaction port received another classification`,
        );
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
    prepareRaw: transactions.prepareRaw,
    validatePreparedRawArtifact: transactions.validatePreparedRawArtifact,
    observe: async ({ identity }) => {
      if (
        identity.category !== category ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(
          `${category} adapter received another workflow identity`,
        );
      }
      return (await current(identity.target.headerHash)).workflow;
    },
    preflight: async ({ identity, workflowId, artifact, action }) => {
      if (
        identity.category !== category ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(`${category} preflight changed workflow identity`);
      }
      const snapshot = await current(identity.target.headerHash);
      assertExactCurrentAction({
        expected: snapshot.workflow,
        requested: action,
        category,
      });
      const key = cacheKey(workflowId, action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} preflight already has an outstanding captured body for this workflow action`,
        );
      }
      const request = {
        action,
        artifact,
        holder: {
          workflowId,
          objective: { category, headerHash: identity.target.headerHash },
        },
      };
      const captured = await transactions.capture(request);
      if (
        requiresMutationLease(category, action) !==
        (captured.mutationLease !== undefined)
      ) {
        await captured.mutationLease?.fail(
          "authenticated removal topology disagreed with mutation-lease acquisition",
        );
        throw new Error(
          `${category} removal topology disagreed with its mutation lease`,
        );
      }
      try {
        const preflight = preflightOf({
          action,
          transaction: captured.transaction,
          category,
        });
        prepared.set(key, captured);
        return preflight;
      } catch (cause) {
        await captured.mutationLease?.fail(
          `preflight admission failed before submission: ${String(cause)}`,
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
        throw new Error(
          `${category} submit has no exact locally evaluated captured body`,
        );
      }
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(captured.transaction),
        };
      } finally {
        prepared.delete(key);
        // The runner follows the submitted transaction on chain, so the
        // lease ends with its submission.
        await captured.mutationLease?.release();
      }
    },
  };
  return Object.freeze(adapter);
};
