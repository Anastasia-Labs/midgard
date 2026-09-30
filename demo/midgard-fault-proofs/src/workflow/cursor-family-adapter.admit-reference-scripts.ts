import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLease } from "../remove-fraudulent-block.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import type { JournalJsonObject } from "./journal.js";
import { journalJsonDigest, normalizeJournalJson } from "./journal.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowReferenceScript,
} from "./orchestrator.js";
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
  }): Promise<JournalJsonObject>;
  prepareRaw?: FraudProofFamilyWorkflowAdapter["prepareRaw"];
  validatePreparedRawArtifact?: FraudProofFamilyWorkflowAdapter["validatePreparedRawArtifact"];
  validatePreparedArtifact?(
    input: Parameters<CursorFamilyTransactionPort<Category>["prepare"]>[0] & {
      readonly artifact: JournalJsonObject;
    },
  ): Promise<void>;
  capture(input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<CursorFamilyCapturedAction>;
}

const TX_HASH = /^[0-9a-f]{64}$/u;

const SCRIPT_HASH = /^[0-9a-f]{56}$/u;

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const sameJson = (left: unknown, right: unknown): boolean =>
  left === undefined || right === undefined
    ? left === right
    : journalJsonDigest(normalizeJournalJson(left)) ===
      journalJsonDigest(normalizeJournalJson(right));

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
  durableRecovery,
}: {
  readonly category: FraudProofCatalogueCategoryName;
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
      referenceScripts: admitReferenceScripts({ category, transaction }),
      ...(durableRecovery === undefined ? {} : { durableRecovery }),
    },
    transaction.signed,
  );

export const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

export const mutationLeaseRecovery = (
  lease: StateQueueMutationLease,
): JournalJsonObject => ({
  stateQueueMutationLease: { token: lease.token, source: lease.source },
});

export const parseMutationLeaseRecovery = (
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

export const requiresMutationLease = (
  action: FraudProofWorkflowAction,
): boolean =>
  action.input.requiresMutationLease === true &&
  (action.input.stage === "remove" ||
    (action.input.category === "transitionTrace" &&
      (action.input.stage === "step_07" || action.input.stage === "step_08")));
