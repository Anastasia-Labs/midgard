import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import type { CanonicalBlockClassification } from "./classification.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.fraud-proof-workflow-journal-event.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.fraud-proof-workflow-terminal.js";
import {
  missingSignatureObservation,
  reconcileMissingSignatureAction,
} from "./missing-signature-state.js";
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
import { reconcileQueueLeaseFamily } from "./queue-lease-family-reconcile.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const MISSING_SIGNATURE_TRANSACTION_PORT =
  "midgard-production-missing-signature-transaction-port-v1" as const;

export type MissingSignatureCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  /** Present only when descendant removal acquired the shared queue fence. */
  mutationLease?: StateQueueMutationLease;
}>;

export interface MissingSignatureTransactionPort {
  readonly portVersion: typeof MISSING_SIGNATURE_TRANSACTION_PORT;
  readonly category: "missingSignature";
  /** Pure public-evidence preparation; no operator file/API input. */
  prepare(input: {
    readonly evidence: CanonicalBlockEvidence;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    > & { readonly category: "missingSignature" };
  }): Promise<JournalJsonObject>;
  /**
   * Builds exactly one authenticated cursor action and returns its signed,
   * unsubmitted transaction. A step-04 scan batch and step-04 finalization use the same port;
   * the current on-chain thread datum decides which transaction is valid.
   */
  capture(input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    /** The workflow journal: its own carriage claims among it. */
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  }): Promise<MissingSignatureCapturedAction>;
}

const CATEGORY = "missingSignature" as const;
const TX_HASH = /^[0-9a-f]{64}$/u;
const SCRIPT_HASH = /^[0-9a-f]{56}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

// The orchestrator hands back the journal-normalized (key-sorted) action, so
// equality must be structural, not byte order.
const sameJson = (left: unknown, right: unknown): boolean =>
  journalJsonDigest(normalizeJournalJson(left)) ===
  journalJsonDigest(normalizeJournalJson(right));

const admitReferenceScripts = (
  transaction: LocallyEvaluatedTransaction,
): readonly FraudProofWorkflowReferenceScript[] => {
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
  requireReferenceOnlyScriptWitnesses({
    transaction,
    label: "missingSignature production transaction",
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
  durableRecovery,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly durableRecovery?: JournalJsonObject;
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
      referenceScripts: admitReferenceScripts(transaction),
      ...(durableRecovery === undefined ? {} : { durableRecovery }),
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

const requiresMutationLease = (action: FraudProofWorkflowAction): boolean =>
  action.input.stage === "remove" &&
  action.input.requiresMutationLease === true;

/**
 * Crash-safe workflow adapter for missing-signature's cursor-driven step 04.
 * Chain state is authoritative on every action and reconciliation boundary.
 */
export const createMissingSignatureWorkflowAdapter = ({
  l1,
  transactions,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly l1: FraudProofFamilyL1ObservationPort<"missingSignature">;
  readonly transactions: MissingSignatureTransactionPort;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    l1.portVersion !== FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT ||
    l1.category !== CATEGORY ||
    transactions.portVersion !== MISSING_SIGNATURE_TRANSACTION_PORT ||
    transactions.category !== CATEGORY
  ) {
    throw new Error("missingSignature workflow ports changed identity");
  }
  const prepared = new Map<string, MissingSignatureCapturedAction>();
  const leaseByTxHash = new Map<string, StateQueueMutationLease>();
  const current = async (headerHash: string) => {
    const observed = await l1.observe({ headerHash });
    return {
      observed,
      workflow: missingSignatureObservation({
        headerHash,
        provenance: observed.provenance,
        stage: observed.stage,
      }),
    };
  };

  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: CATEGORY,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({ evidence, classification }) => {
      if (classification.category !== CATEGORY) {
        throw new Error(
          "missingSignature transaction port received another classification",
        );
      }
      return await transactions.prepare({
        evidence,
        classification: classification as Extract<
          CanonicalBlockClassification,
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
    preflight: async ({ identity, workflowId, artifact, action, entries }) => {
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
      const observed = await l1.observe({
        headerHash,
      });
      return await reconcileQueueLeaseFamily({
        category: CATEGORY,
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
          await reconcileMissingSignatureAction({
            headerHash,
            action,
            ...(txHash === undefined ? {} : { txHash }),
            provenance: observed.provenance,
            stage: observed.stage,
            recoverUnconfirmedTransaction,
            transactionConfirmed: async (hash) =>
              await l1.transactionConfirmed({
                headerHash,
                txHash: hash,
              }),
          }),
      });
    },
  };
  return Object.freeze(adapter);
};
