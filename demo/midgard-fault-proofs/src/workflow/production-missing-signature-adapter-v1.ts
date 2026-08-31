import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import type { JournalJsonObjectV1 } from "./journal-v1.js";
import type {
  FraudProofWorkflowActionV1,
  FraudProofWorkflowPreflightV1,
  FraudProofWorkflowReferenceScriptV1,
} from "./orchestrator-v1.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  type FraudProofFamilyWorkflowAdapterV1,
} from "./orchestrator-v1.js";
import {
  productionMissingSignatureObservationV1,
  reconcileProductionMissingSignatureActionV1,
} from "./production-missing-signature-state-v1.js";
import {
  bindProductionWorkflowPreflightTransactionV1,
  LOCAL_UPLC_EVALUATOR_V1,
  type LocallyEvaluatedTransactionV1,
  requireReferenceOnlyScriptWitnessesV1,
  submitCapturedTransactionV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1 =
  "midgard-production-missing-signature-transaction-port-v1" as const;

export type ProductionMissingSignatureCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  /** Present only when descendant removal acquired the shared queue fence. */
  mutationLease?: StateQueueMutationLease;
}>;

export interface ProductionMissingSignatureTransactionPortV1 {
  readonly portVersion: typeof PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1;
  readonly category: "missingSignature";
  /** Pure public-evidence preparation; no operator file/API input. */
  prepare(input: {
    readonly evidence: CanonicalBlockEvidenceV1;
    readonly classification: Extract<
      CanonicalBlockClassificationV1,
      { readonly decision: "fault_detected" }
    > & { readonly category: "missingSignature" };
  }): Promise<JournalJsonObjectV1>;
  /**
   * Builds exactly one authenticated cursor action to the common pre-submit
   * boundary. A step-04 scan batch and step-04 finalization use the same port;
   * the current on-chain thread datum decides which transaction is valid.
   */
  capture(input: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<ProductionMissingSignatureCapturedActionV1>;
}

const CATEGORY = "missingSignature" as const;
const TX_HASH = /^[0-9a-f]{64}$/u;
const SCRIPT_HASH = /^[0-9a-f]{56}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

const admitReferenceScripts = (
  transaction: LocallyEvaluatedTransactionV1,
): readonly FraudProofWorkflowReferenceScriptV1[] => {
  if (!TX_HASH.test(transaction.txHash)) {
    throw new Error(
      "missingSignature preflight returned a malformed body hash",
    );
  }
  if (transaction.signed.toHash().toLowerCase() !== transaction.txHash) {
    throw new Error(
      "missingSignature preflight body hash changed after capture",
    );
  }
  if (transaction.referenceScripts.length === 0) {
    throw new Error(
      "missingSignature production transaction did not use published reference scripts",
    );
  }
  requireReferenceOnlyScriptWitnessesV1({
    transaction,
    label: "missingSignature production transaction",
  });
  const referenceInputs = new Set(
    workflowTransactionReferenceInputOutRefsV1(transaction.signed),
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
      throw new Error(
        "missingSignature captured a malformed reference identity",
      );
    }
    if (roles.has(reference.role) || outRefs.has(reference.outRef)) {
      throw new Error(
        "missingSignature captured duplicate reference-script role or outRef",
      );
    }
    if (!referenceInputs.has(reference.outRef)) {
      throw new Error(
        "missingSignature claimed a reference script absent from the signed body",
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
}: {
  readonly action: FraudProofWorkflowActionV1;
  readonly transaction: LocallyEvaluatedTransactionV1;
}): FraudProofWorkflowPreflightV1 =>
  bindProductionWorkflowPreflightTransactionV1(
    {
      actionId: action.actionId,
      txHash: transaction.txHash,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: {
        status: "passed",
        evaluator: LOCAL_UPLC_EVALUATOR_V1,
      },
      referenceScripts: admitReferenceScripts(transaction),
    },
    transaction.signed,
  );

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

const mutationLeaseRecovery = (
  lease: StateQueueMutationLease,
): JournalJsonObjectV1 => ({
  stateQueueMutationLease: { token: lease.token, source: lease.source },
});

const parseMutationLeaseRecovery = (
  recovery: JournalJsonObjectV1 | undefined,
): { readonly token: string; readonly source: string } | undefined => {
  if (recovery === undefined) return undefined;
  const value = recovery.stateQueueMutationLease;
  if (
    Object.keys(recovery).length !== 1 ||
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value)
  ) {
    throw new Error("missingSignature durable mutation lease is malformed");
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
    throw new Error("missingSignature durable mutation lease is malformed");
  }
  return { token: record.token, source: record.source };
};

const requiresMutationLease = (action: FraudProofWorkflowActionV1): boolean =>
  action.input.stage === "remove" &&
  action.input.requiresMutationLease === true;

/**
 * Crash-safe workflow adapter for missing-signature's cursor-driven step 04.
 * Chain state is authoritative on every action and reconciliation boundary.
 */
export const createProductionMissingSignatureWorkflowAdapterV1 = ({
  l1,
  transactions,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly l1: FraudProofFamilyL1ObservationPortV1<"missingSignature">;
  readonly transactions: ProductionMissingSignatureTransactionPortV1;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}): FraudProofFamilyWorkflowAdapterV1 => {
  if (
    l1.portVersion !== FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1 ||
    l1.category !== CATEGORY ||
    transactions.portVersion !==
      PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1 ||
    transactions.category !== CATEGORY
  ) {
    throw new Error("missingSignature workflow ports changed identity");
  }
  const prepared = new Map<
    string,
    ProductionMissingSignatureCapturedActionV1
  >();
  const leaseByTxHash = new Map<string, StateQueueMutationLease>();
  const current = async (headerHash: string) => {
    const observed = await l1.observe({ headerHash });
    return {
      observed,
      workflow: productionMissingSignatureObservationV1({
        headerHash,
        provenance: observed.provenance,
        stage: observed.stage,
      }),
    };
  };

  const adapter: FraudProofFamilyWorkflowAdapterV1 = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
    category: CATEGORY,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
    prepare: async ({ evidence, classification }) => {
      if (classification.category !== CATEGORY) {
        throw new Error(
          "missingSignature transaction port received another classification",
        );
      }
      return await transactions.prepare({
        evidence,
        classification: classification as Extract<
          CanonicalBlockClassificationV1,
          { readonly decision: "fault_detected" }
        > & { readonly category: "missingSignature" },
      });
    },
    observe: async ({ identity }) => {
      if (
        identity.category !== CATEGORY ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error(
          "missingSignature adapter received another workflow identity",
        );
      }
      return (await current(identity.target.headerHash)).workflow;
    },
    preflight: async ({ identity, workflowId, artifact, action }) => {
      if (
        identity.category !== CATEGORY ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error("missingSignature preflight changed workflow identity");
      }
      const snapshot = await current(identity.target.headerHash);
      if (
        snapshot.workflow.kind !== "action_required" ||
        !sameJson(snapshot.workflow.action, action)
      ) {
        throw new Error(
          "missingSignature preflight action differs from authenticated current L1 state",
        );
      }
      const key = cacheKey(workflowId, action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          "missingSignature preflight already has an outstanding captured body for this workflow action",
        );
      }
      const captured = await transactions.capture({ action, artifact });
      if (
        requiresMutationLease(action) !==
        (captured.mutationLease !== undefined)
      ) {
        await captured.mutationLease?.fail(
          "authenticated removal topology disagreed with mutation-lease acquisition",
        );
        throw new Error(
          "missingSignature removal topology disagreed with its mutation lease",
        );
      }
      try {
        const preflight = preflightOf({
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
        throw new Error(
          "missingSignature submit has no exact locally evaluated captured body",
        );
      }
      const recovery = parseMutationLeaseRecovery(preflight.durableRecovery);
      if (
        (captured.mutationLease === undefined) !== (recovery === undefined) ||
        (captured.mutationLease !== undefined &&
          (captured.mutationLease.token !== recovery?.token ||
            captured.mutationLease.source !== recovery.source))
      ) {
        throw new Error(
          "missingSignature cached mutation lease differs from durable intent",
        );
      }
      try {
        if (captured.mutationLease !== undefined) {
          leaseByTxHash.set(preflight.txHash, captured.mutationLease);
        }
        return {
          kind: "submitted",
          txHash: await submitCapturedTransactionV1(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async ({ identity, action, txHash, durableRecovery }) => {
      if (
        identity.category !== CATEGORY ||
        identity.target.kind !== "state_queue_header"
      ) {
        throw new Error("missingSignature reconcile changed workflow identity");
      }
      const headerHash = identity.target.headerHash;
      const recovery = parseMutationLeaseRecovery(durableRecovery);
      if (requiresMutationLease(action) !== (recovery !== undefined)) {
        return {
          kind: "conflict",
          reason:
            "missingSignature durable mutation-lease identity disagrees with removal topology",
        };
      }
      let lease = txHash === undefined ? undefined : leaseByTxHash.get(txHash);
      if (lease === undefined && recovery !== undefined) {
        if (stateQueueMutationLeaseCoordinator.resume === undefined) {
          return {
            kind: "conflict",
            reason:
              "missingSignature mutation-lease coordinator cannot resume durable intent",
          };
        }
        try {
          lease = await stateQueueMutationLeaseCoordinator.resume(recovery);
          if (txHash !== undefined) leaseByTxHash.set(txHash, lease);
        } catch (cause) {
          return {
            kind: "conflict",
            reason: `missingSignature durable mutation lease cannot resume: ${String(cause)}`,
          };
        }
      }
      const observed = await l1.observe({
        headerHash,
      });
      const result = await reconcileProductionMissingSignatureActionV1({
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
