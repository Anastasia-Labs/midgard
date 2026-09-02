import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  computeFraudProofWorkflowIdV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalEntryV1,
  type FraudProofWorkflowJournalStoreV1,
  journalJsonDigestV1,
} from "../workflow/journal-v1.js";
import type { FraudProofWorkflowActionV1 } from "../workflow/orchestrator-v1.js";
import { assertProductionWorkflowJournalActuationV1 } from "../workflow/production-actuation-permit-v1.js";
import {
  abandonProductionWorkflowFundingReservationTransactionV1,
  assertProductionWorkflowFundingReservationReadyToSubmitV1,
  beginProductionWorkflowFundingReservationActionV1,
  confirmProductionWorkflowFundingReservationTransactionV1,
  conflictProductionWorkflowFundingReservationTransactionV1,
  prepareProductionWorkflowFundingReservationTransactionV1,
} from "../workflow/production-funding-reservation-permit-v1.js";
import {
  bindProductionWorkflowPreflightTransactionV1,
  type FraudProofPreSubmitBoundaryV1,
  LOCAL_UPLC_EVALUATOR_V1,
} from "../workflow/transaction-boundary-v1.js";
import type {
  ProtectedOutputSignerActionV1,
  ProtectedOutputSignerJournalEntryV1,
  ProtectedOutputSignerJournalV1,
  ProtectedOutputSignerStageV1,
} from "./workflow-v1.js";

type SubmitAction = Exclude<ProtectedOutputSignerActionV1, "done">;
type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: ProtectedOutputSignerStageV1;
  targetStage: ProtectedOutputSignerStageV1;
  auxiliary?: boolean;
}>;

const TX_HASH = /^[0-9a-f]{64}$/u;
const stages: readonly ProtectedOutputSignerStageV1[] = [
  "none",
  "step01",
  "step02",
  "step03",
  "scanning",
  "step05",
  "proven",
  "removed",
  "cancelled",
];
const actionId = (action: SubmitAction): string =>
  `protectedOutputSignerMissing:${action}`;
const now = (): string => new Date().toISOString();

const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntryV1,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error(
      "protectedOutputSignerMissing journal entry is not an intent",
    );
  }
  const value = entry.event.durableRecovery;
  const familyIdentity = value?.familyIdentity;
  const sourceStage = value?.sourceStage;
  const targetStage = value?.targetStage;
  if (
    typeof familyIdentity !== "string" ||
    typeof sourceStage !== "string" ||
    typeof targetStage !== "string" ||
    !stages.includes(sourceStage as ProtectedOutputSignerStageV1) ||
    !stages.includes(targetStage as ProtectedOutputSignerStageV1)
  ) {
    throw new Error(
      "protectedOutputSignerMissing durable intent is incomplete",
    );
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as ProtectedOutputSignerStageV1,
    targetStage: targetStage as ProtectedOutputSignerStageV1,
    ...(value?.auxiliary === true ? { auxiliary: true } : {}),
  };
};

const actionFinishedAfter = (
  entries: readonly FraudProofWorkflowJournalEntryV1[],
  sequence: number,
  wantedActionId: string,
): boolean =>
  entries.slice(sequence + 1).some(({ event }) => {
    if (!("actionId" in event) || event.actionId !== wantedActionId) {
      return false;
    }
    return (
      event.kind === "confirmed" ||
      (event.kind === "reconciled" && event.outcome === "not_found")
    );
  });

const unresolvedIntent = (
  entries: readonly FraudProofWorkflowJournalEntryV1[],
): FraudProofWorkflowJournalEntryV1 | undefined =>
  [...entries].reverse().find((entry) => {
    const event = entry.event;
    return (
      event.kind === "submission_intent" &&
      !actionFinishedAfter(entries, entry.sequence, event.actionId)
    );
  });

/** Central-journal bridge with exact post-restart raw-L1 reconciliation. */
export const createProtectedOutputSignerMissingCentralJournalAdapterV1 = ({
  store,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  transactionConfirmed,
  testOnlyJournalCategoryAlias,
}: {
  readonly store: FraudProofWorkflowJournalStoreV1;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly decisionDigest: string;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
  /** Temporary family-test bridge until frozen category 0000002b is centrally sealed. */
  readonly testOnlyJournalCategoryAlias?: FraudProofCatalogueCategoryName;
}) => {
  if (
    testOnlyJournalCategoryAlias !== undefined &&
    process.env.NODE_ENV !== "test"
  ) {
    throw new Error(
      "protectedOutputSignerMissing test journal alias is unavailable in production",
    );
  }
  const journalCategory =
    testOnlyJournalCategoryAlias ??
    ("protectedOutputSignerMissing" as FraudProofCatalogueCategoryName);
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint,
    category: journalCategory,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowIdV1(identity);
  const entries = async () => await store.load(workflowId);
  const appendEvent = async (
    event: FraudProofWorkflowJournalEntryV1["event"],
  ): Promise<void> => {
    let current = await entries();
    if (current.length === 0) {
      await store.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence: 0,
          recordedAt: now(),
          event: { kind: "started" },
        },
        0,
      );
      current = await entries();
    }
    await store.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: current.length,
        recordedAt: now(),
        event,
      },
      current.length,
    );
  };
  const workflowAction = (
    kind: SubmitAction,
    recovery: DurableRecovery,
  ): FraudProofWorkflowActionV1 => ({
    actionId: actionId(kind),
    input: { actionKind: actionId(kind), ...recovery },
  });
  const assertActuation = (
    checkpoint: "before_preflight" | "before_submit" | "before_reconcile",
  ): void =>
    assertProductionWorkflowJournalActuationV1({
      journal: store,
      deploymentFingerprint,
      category: journalCategory,
      headerHash,
      checkpoint,
    });

  const ensurePrepared = async (familyIdentity: string): Promise<void> => {
    const current = await entries();
    const prepared = current.find(({ event }) => event.kind === "prepared");
    const artifact = {
      category: "protectedOutputSignerMissing",
      familyIdentity,
    };
    const artifactDigest = journalJsonDigestV1(artifact);
    if (prepared?.event.kind === "prepared") {
      if (prepared.event.artifactDigest !== artifactDigest) {
        throw new Error(
          "protectedOutputSignerMissing prepared family evidence identity changed",
        );
      }
      return;
    }
    await appendEvent({ kind: "prepared", artifact, artifactDigest });
  };

  const begin = async (
    kind: SubmitAction,
    familyIdentity: string,
    sourceStage: ProtectedOutputSignerStageV1,
    targetStage: ProtectedOutputSignerStageV1,
  ): Promise<void> => {
    await ensurePrepared(familyIdentity);
    if (unresolvedIntent(await entries()) !== undefined) {
      throw new Error(
        "protectedOutputSignerMissing unresolved submission must reconcile before another build",
      );
    }
    assertActuation("before_preflight");
    await beginProductionWorkflowFundingReservationActionV1({
      journal: store,
      action: workflowAction(kind, {
        familyIdentity,
        sourceStage,
        targetStage,
      }),
    });
  };

  const boundary =
    (
      kind: SubmitAction,
      familyIdentity: string,
      sourceStage: ProtectedOutputSignerStageV1,
      targetStage: ProtectedOutputSignerStageV1,
    ): FraudProofPreSubmitBoundaryV1 =>
    async (transaction) => {
      if (!TX_HASH.test(transaction.txHash)) {
        throw new Error(
          "protectedOutputSignerMissing pre-submit hash is not canonical",
        );
      }
      const recovery = { familyIdentity, sourceStage, targetStage } as const;
      const action = workflowAction(kind, recovery);
      const current = await entries();
      const prior = unresolvedIntent(current);
      if (prior?.event.kind === "submission_intent") {
        if (
          prior.event.actionId !== action.actionId ||
          prior.event.txHash !== transaction.txHash
        ) {
          throw new Error(
            "protectedOutputSignerMissing transaction identity changed across restart",
          );
        }
        return;
      }
      const attempt =
        current.filter(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionId === action.actionId,
        ).length + 1;
      const preflight = bindProductionWorkflowPreflightTransactionV1(
        {
          actionId: action.actionId,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR_V1,
          },
          referenceScripts: transaction.referenceScripts,
          durableRecovery: recovery,
        },
        transaction.signed,
      );
      await beginProductionWorkflowFundingReservationActionV1({
        journal: store,
        action,
      });
      await prepareProductionWorkflowFundingReservationTransactionV1({
        journal: store,
        action,
        preflight,
      });
      await appendEvent({
        kind: "preflight_passed",
        actionId: action.actionId,
        txHash: transaction.txHash,
        localEvaluator: LOCAL_UPLC_EVALUATOR_V1,
        referenceScripts: transaction.referenceScripts,
      });
      await appendEvent({
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        durableRecovery: recovery,
        attempt,
        txHash: transaction.txHash,
      });
      assertActuation("before_submit");
      await assertProductionWorkflowFundingReservationReadyToSubmitV1({
        journal: store,
        transactionHash: transaction.txHash,
      });
    };

  const reconcile = async (
    observedStage: ProtectedOutputSignerStageV1,
  ): Promise<void> => {
    const intent = unresolvedIntent(await entries());
    if (intent?.event.kind !== "submission_intent") return;
    assertActuation("before_reconcile");
    const recovery = recoveryFrom(intent);
    const confirmed = await transactionConfirmed(intent.event.txHash);
    if (confirmed && observedStage === recovery.targetStage) {
      await confirmProductionWorkflowFundingReservationTransactionV1({
        journal: store,
        transactionHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "reconciled",
        actionId: intent.event.actionId,
        outcome: "confirmed",
        txHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "confirmed",
        actionId: intent.event.actionId,
        txHash: intent.event.txHash,
      });
      return;
    }
    if (!confirmed && observedStage === recovery.sourceStage) {
      await abandonProductionWorkflowFundingReservationTransactionV1({
        journal: store,
        transactionHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "reconciled",
        actionId: intent.event.actionId,
        outcome: "not_found",
      });
      return;
    }
    await conflictProductionWorkflowFundingReservationTransactionV1({
      journal: store,
      transactionHash: intent.event.txHash,
    });
    throw new Error(
      "protectedOutputSignerMissing authenticated stage/transaction identity substitution",
    );
  };

  const auxiliaryBoundary =
    (
      kind: "publication" | "certificate",
      familyIdentity: string,
      stage: ProtectedOutputSignerStageV1,
      captured: string[],
    ): FraudProofPreSubmitBoundaryV1 =>
    async (transaction) => {
      await ensurePrepared(familyIdentity);
      const pending = unresolvedIntent(await entries());
      if (pending?.event.kind === "submission_intent") {
        if (recoveryFrom(pending).auxiliary !== true) {
          throw new Error(
            "protectedOutputSignerMissing auxiliary actuation found an unresolved proof transaction",
          );
        }
        await confirmAuxiliary(pending.event.txHash);
      }
      const id = `protectedOutputSignerMissing:${kind}:${transaction.txHash}`;
      const recovery = {
        familyIdentity,
        sourceStage: stage,
        targetStage: stage,
        auxiliary: true,
      } as const;
      const action: FraudProofWorkflowActionV1 = {
        actionId: id,
        input: {
          actionKind:
            kind === "publication"
              ? "publish_field_carriage"
              : "certify_field_carriage",
          kind,
        },
      };
      const preflight = bindProductionWorkflowPreflightTransactionV1(
        {
          actionId: id,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR_V1,
          },
          referenceScripts: transaction.referenceScripts,
          durableRecovery: recovery,
        },
        transaction.signed,
      );
      await beginProductionWorkflowFundingReservationActionV1({
        journal: store,
        action,
      });
      await prepareProductionWorkflowFundingReservationTransactionV1({
        journal: store,
        action,
        preflight,
      });
      await appendEvent({
        kind: "preflight_passed",
        actionId: id,
        txHash: transaction.txHash,
        localEvaluator: LOCAL_UPLC_EVALUATOR_V1,
        referenceScripts: transaction.referenceScripts,
      });
      await appendEvent({
        kind: "submission_intent",
        actionId: id,
        actionInput: action.input,
        durableRecovery: recovery,
        attempt: 1,
        txHash: transaction.txHash,
      });
      captured.push(transaction.txHash);
      assertActuation("before_submit");
      await assertProductionWorkflowFundingReservationReadyToSubmitV1({
        journal: store,
        transactionHash: transaction.txHash,
      });
    };

  const confirmAuxiliary = async (txHash: string): Promise<void> => {
    const current = await entries();
    if (
      current.some(
        ({ event }) => event.kind === "confirmed" && event.txHash === txHash,
      )
    ) {
      return;
    }
    const intent = unresolvedIntent(current);
    if (
      intent?.event.kind !== "submission_intent" ||
      intent.event.txHash !== txHash ||
      recoveryFrom(intent).auxiliary !== true
    ) {
      throw new Error(
        "protectedOutputSignerMissing auxiliary completion changed transaction identity",
      );
    }
    if (!(await transactionConfirmed(txHash))) {
      throw new Error(
        "protectedOutputSignerMissing auxiliary transaction is not authenticated on L1",
      );
    }
    await confirmProductionWorkflowFundingReservationTransactionV1({
      journal: store,
      transactionHash: txHash,
    });
    await appendEvent({
      kind: "submitted",
      actionId: intent.event.actionId,
      attempt: intent.event.attempt,
      txHash,
    });
    await appendEvent({
      kind: "reconciled",
      actionId: intent.event.actionId,
      outcome: "confirmed",
      txHash,
    });
    await appendEvent({
      kind: "confirmed",
      actionId: intent.event.actionId,
      txHash,
    });
  };

  const familyJournal: ProtectedOutputSignerJournalV1 = {
    load: async (familyIdentity) => {
      const current = await entries();
      const result: ProtectedOutputSignerJournalEntryV1[] = [];
      for (const entry of current) {
        const confirmedEvent = entry.event;
        if (confirmedEvent.kind !== "confirmed") continue;
        const intent = [...current]
          .slice(0, entry.sequence)
          .reverse()
          .find(
            ({ event }) =>
              event.kind === "submission_intent" &&
              event.actionId === confirmedEvent.actionId,
          );
        if (intent?.event.kind !== "submission_intent") {
          throw new Error(
            "protectedOutputSignerMissing confirmed transaction has no durable intent",
          );
        }
        const recovery = recoveryFrom(intent);
        if (recovery.auxiliary === true) continue;
        if (recovery.familyIdentity !== familyIdentity) {
          throw new Error(
            "protectedOutputSignerMissing family evidence identity changed",
          );
        }
        result.push({
          sequence: result.length,
          identity: familyIdentity,
          sourceStage: recovery.sourceStage,
          targetStage: recovery.targetStage,
          action: intent.event.actionId.replace(
            "protectedOutputSignerMissing:",
            "",
          ) as SubmitAction,
          phase: "confirmed",
          txHash: confirmedEvent.txHash,
        });
      }
      return result;
    },
    append: async (entry) => {
      const intent = unresolvedIntent(await entries());
      if (
        intent?.event.kind !== "submission_intent" ||
        intent.event.txHash !== entry.txHash
      ) {
        throw new Error(
          "protectedOutputSignerMissing submission completed without its exact pre-submit intent",
        );
      }
      const recovery = recoveryFrom(intent);
      if (
        recovery.familyIdentity !== entry.identity ||
        recovery.sourceStage !== entry.sourceStage ||
        recovery.targetStage !== entry.targetStage
      ) {
        throw new Error(
          "protectedOutputSignerMissing submission result changed durable recovery identity",
        );
      }
      await appendEvent({
        kind: "submitted",
        actionId: intent.event.actionId,
        attempt: intent.event.attempt,
        txHash: entry.txHash,
      });
    },
  };
  return Object.freeze({
    workflowId,
    identity,
    begin,
    boundary,
    reconcile,
    auxiliaryBoundary,
    confirmAuxiliary,
    familyJournal,
  });
};
