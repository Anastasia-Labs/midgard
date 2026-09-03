import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import type { CanonicalBlockClassification } from "./classification-v1.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation-v1.js";
import type { JournalJsonObject } from "./journal-v1.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowReferenceScript,
} from "./orchestrator-v1.js";
import {
  cursorFamilyObservation,
  type CursorFamilySpec,
  reconcileCursorFamilyAction,
} from "./production-cursor-family-state-v1.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

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
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    > & { readonly category: Category };
  }): Promise<JournalJsonObject>;
  capture(input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<CursorFamilyCapturedAction>;
}

const TX_HASH = /^[0-9a-f]{64}$/u;
const SCRIPT_HASH = /^[0-9a-f]{56}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

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

const preflightOf = ({
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

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

const mutationLeaseRecovery = (
  lease: StateQueueMutationLease,
): JournalJsonObject => ({
  stateQueueMutationLease: { token: lease.token, source: lease.source },
});

const parseMutationLeaseRecovery = (
  recovery: JournalJsonObject | undefined,
): { readonly token: string; readonly source: string } | undefined => {
  if (recovery === undefined) return undefined;
  const value = recovery.stateQueueMutationLease;
  if (
    Object.keys(recovery).length !== 1 ||
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value)
  ) {
    throw new Error("cursor-family durable mutation lease is malformed");
  }
  const record = value as Readonly<Record<string, unknown>>;
  if (
    Object.keys(record).sort().join(",") !== "source,token" ||
    typeof record.token !== "string" ||
    record.token.trim() !== record.token ||
    record.token.length === 0 ||
    typeof record.source !== "string" ||
    record.source.trim() !== record.source ||
    record.source.length === 0
  ) {
    throw new Error("cursor-family durable mutation lease is malformed");
  }
  return { token: record.token, source: record.source };
};

const requiresMutationLease = (action: FraudProofWorkflowAction): boolean =>
  action.input.stage === "remove" &&
  action.input.requiresMutationLease === true;

/**
 * Crash-safe mechanics for explicitly specified one-to-eight-step state
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
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly l1: FraudProofFamilyL1ObservationPort<Category>;
  readonly transactions: CursorFamilyTransactionPort<Category>;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
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
  const current = async (headerHash: string) => {
    const observed = await l1.observe({ headerHash });
    return {
      observed,
      workflow: cursorFamilyObservation({
        spec,
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
    prepare: async ({ evidence, classification }) => {
      if (classification.category !== category) {
        throw new Error(`${category} port received another classification`);
      }
      return await transactions.prepare({
        evidence,
        classification: classification as Extract<
          CanonicalBlockClassification,
          { readonly decision: "fault_detected" }
        > & { readonly category: Category },
      });
    },
    observe: async ({ identity }) => {
      if (
        identity.category !== category ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(`${category} adapter received another identity`);
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
      if (
        snapshot.workflow.kind !== "action_required" ||
        !sameJson(snapshot.workflow.action, action)
      ) {
        throw new Error(
          `${category} preflight differs from authenticated current L1 state`,
        );
      }
      const key = cacheKey(workflowId, action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} preflight already has an outstanding captured body`,
        );
      }
      const captured = await transactions.capture({ action, artifact });
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
        });
        prepared.set(key, captured);
        return {
          ...preflight,
          ...(captured.mutationLease === undefined
            ? {}
            : {
                durableRecovery: mutationLeaseRecovery(captured.mutationLease),
              }),
        };
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
    reconcile: async ({ identity, action, txHash, durableRecovery }) => {
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
      let lease = txHash === undefined ? undefined : leaseByTxHash.get(txHash);
      if (lease === undefined && recovery !== undefined) {
        if (stateQueueMutationLeaseCoordinator.resume === undefined) {
          return {
            kind: "conflict",
            reason: `${category} lease coordinator cannot resume durable intent`,
          };
        }
        try {
          lease = await stateQueueMutationLeaseCoordinator.resume(recovery);
          if (txHash !== undefined) leaseByTxHash.set(txHash, lease);
        } catch (cause) {
          return {
            kind: "conflict",
            reason: `${category} durable lease cannot resume: ${String(cause)}`,
          };
        }
      }
      const observed = await l1.observe({ headerHash });
      const result = await reconcileCursorFamilyAction({
        spec,
        headerHash,
        action,
        ...(txHash === undefined ? {} : { txHash }),
        provenance: observed.provenance,
        stage: observed.stage,
        transactionConfirmed: async (hash) =>
          await l1.transactionConfirmed({
            headerHash,
            txHash: hash,
          }),
      });
      if (result.kind === "confirmed") {
        await lease?.release();
        leaseByTxHash.delete(result.txHash);
      } else if (result.kind === "conflict") {
        await lease?.fail(result.reason);
        if (txHash !== undefined) leaseByTxHash.delete(txHash);
      } else {
        await lease?.renew();
      }
      return result;
    },
  };
  return Object.freeze(adapter);
};
