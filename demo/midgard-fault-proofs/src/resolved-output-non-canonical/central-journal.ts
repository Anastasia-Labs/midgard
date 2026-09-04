import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { assertWorkflowJournalActuation } from "../workflow/actuation-permit.js";
import {
  abandonWorkflowFundingReservationTransaction,
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  prepareWorkflowFundingReservationTransaction,
} from "../workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import type { FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import {
  bindWorkflowPreflightTransaction,
  type FraudProofPreSubmitBoundary,
  LOCAL_UPLC_EVALUATOR,
} from "../workflow/transaction-boundary.js";
import type {
  ResolvedOutputAction,
  ResolvedOutputJournal,
  ResolvedOutputJournalEntry,
  ResolvedOutputStage,
} from "./workflow.js";

type SubmitAction = Exclude<ResolvedOutputAction, "done">;
type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: ResolvedOutputStage;
  targetStage: ResolvedOutputStage;
  auxiliary?: boolean;
}>;

const TX_HASH = /^[0-9a-f]{64}$/u;
const stages: readonly ResolvedOutputStage[] = [
  "none",
  "step01",
  "step02",
  "step03",
  "reconstructing",
  "step05",
  "proven",
  "removed",
  "cancelled",
];
const actionId = (action: SubmitAction): string =>
  `resolvedOutputNonCanonical:${action}`;
const now = (): string => new Date().toISOString();

const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error(
      "resolvedOutputNonCanonical journal entry is not an intent",
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
    !stages.includes(sourceStage as ResolvedOutputStage) ||
    !stages.includes(targetStage as ResolvedOutputStage)
  ) {
    throw new Error("resolvedOutputNonCanonical durable intent is incomplete");
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as ResolvedOutputStage,
    targetStage: targetStage as ResolvedOutputStage,
    ...(value?.auxiliary === true ? { auxiliary: true } : {}),
  };
};

const actionFinishedAfter = (
  entries: readonly FraudProofWorkflowJournalEntry[],
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
  entries: readonly FraudProofWorkflowJournalEntry[],
): FraudProofWorkflowJournalEntry | undefined =>
  [...entries].reverse().find((entry) => {
    const event = entry.event;
    return (
      event.kind === "submission_intent" &&
      !actionFinishedAfter(entries, entry.sequence, event.actionId)
    );
  });

/** Central-journal bridge with exact post-restart raw-L1 reconciliation. */
export const createResolvedOutputNonCanonicalCentralJournalAdapter = ({
  store,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  transactionConfirmed,
  testOnlyJournalCategoryAlias,
}: {
  readonly store: FraudProofWorkflowJournalStore;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly decisionDigest: string;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
  /** Temporary family-test bridge until frozen category 00000026 is centrally sealed. */
  readonly testOnlyJournalCategoryAlias?: FraudProofCatalogueCategoryName;
}) => {
  if (
    testOnlyJournalCategoryAlias !== undefined &&
    process.env.NODE_ENV !== "test"
  ) {
    throw new Error(
      "resolvedOutputNonCanonical test journal alias is unavailable in production",
    );
  }
  const journalCategory =
    testOnlyJournalCategoryAlias ??
    ("resolvedOutputNonCanonical" as FraudProofCatalogueCategoryName);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category: journalCategory,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const entries = async () => await store.load(workflowId);
  const appendEvent = async (
    event: FraudProofWorkflowJournalEntry["event"],
  ): Promise<void> => {
    let current = await entries();
    if (current.length === 0) {
      await store.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
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
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
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
  ): FraudProofWorkflowAction => ({
    actionId: actionId(kind),
    input: { actionKind: actionId(kind), ...recovery },
  });
  const assertActuation = (
    checkpoint: "before_preflight" | "before_submit" | "before_reconcile",
  ): void =>
    assertWorkflowJournalActuation({
      journal: store,
      deploymentFingerprint,
      category: journalCategory,
      headerHash,
      checkpoint,
    });

  const ensurePrepared = async (familyIdentity: string): Promise<void> => {
    const current = await entries();
    const prepared = current.find(({ event }) => event.kind === "prepared");
    const artifact = { category: "resolvedOutputNonCanonical", familyIdentity };
    const artifactDigest = journalJsonDigest(artifact);
    if (prepared?.event.kind === "prepared") {
      if (prepared.event.artifactDigest !== artifactDigest) {
        throw new Error(
          "resolvedOutputNonCanonical prepared family evidence identity changed",
        );
      }
      return;
    }
    await appendEvent({ kind: "prepared", artifact, artifactDigest });
  };

  const begin = async (
    kind: SubmitAction,
    familyIdentity: string,
    sourceStage: ResolvedOutputStage,
    targetStage: ResolvedOutputStage,
  ): Promise<void> => {
    await ensurePrepared(familyIdentity);
    if (unresolvedIntent(await entries()) !== undefined) {
      throw new Error(
        "resolvedOutputNonCanonical unresolved submission must reconcile before another build",
      );
    }
    assertActuation("before_preflight");
    await beginWorkflowFundingReservationAction({
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
      sourceStage: ResolvedOutputStage,
      targetStage: ResolvedOutputStage,
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      if (!TX_HASH.test(transaction.txHash)) {
        throw new Error(
          "resolvedOutputNonCanonical pre-submit hash is not canonical",
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
            "resolvedOutputNonCanonical transaction identity changed across restart",
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
      const preflight = bindWorkflowPreflightTransaction(
        {
          actionId: action.actionId,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: transaction.referenceScripts,
          durableRecovery: recovery,
        },
        transaction.signed,
      );
      await beginWorkflowFundingReservationAction({
        journal: store,
        action,
      });
      await prepareWorkflowFundingReservationTransaction({
        journal: store,
        action,
        preflight,
      });
      await appendEvent({
        kind: "preflight_passed",
        actionId: action.actionId,
        txHash: transaction.txHash,
        localEvaluator: LOCAL_UPLC_EVALUATOR,
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
      await assertWorkflowFundingReservationReadyToSubmit({
        journal: store,
        transactionHash: transaction.txHash,
      });
    };

  const reconcile = async (
    observedStage: ResolvedOutputStage,
  ): Promise<void> => {
    const intent = unresolvedIntent(await entries());
    if (intent?.event.kind !== "submission_intent") return;
    assertActuation("before_reconcile");
    const recovery = recoveryFrom(intent);
    const confirmed = await transactionConfirmed(intent.event.txHash);
    if (confirmed && observedStage === recovery.targetStage) {
      await confirmWorkflowFundingReservationTransaction({
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
      await abandonWorkflowFundingReservationTransaction({
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
    await conflictWorkflowFundingReservationTransaction({
      journal: store,
      transactionHash: intent.event.txHash,
    });
    throw new Error(
      "resolvedOutputNonCanonical authenticated stage/transaction identity substitution",
    );
  };

  const auxiliaryBoundary =
    (
      kind: "publication" | "certificate",
      familyIdentity: string,
      stage: ResolvedOutputStage,
      captured: string[],
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      await ensurePrepared(familyIdentity);
      const pending = unresolvedIntent(await entries());
      if (pending?.event.kind === "submission_intent") {
        if (recoveryFrom(pending).auxiliary !== true) {
          throw new Error(
            "resolvedOutputNonCanonical auxiliary actuation found an unresolved proof transaction",
          );
        }
        await confirmAuxiliary(pending.event.txHash);
      }
      const id = `resolvedOutputNonCanonical:${kind}:${transaction.txHash}`;
      const recovery = {
        familyIdentity,
        sourceStage: stage,
        targetStage: stage,
        auxiliary: true,
      } as const;
      const action: FraudProofWorkflowAction = {
        actionId: id,
        input: {
          actionKind:
            kind === "publication"
              ? "publish_field_carriage"
              : "certify_field_carriage",
          kind,
        },
      };
      const preflight = bindWorkflowPreflightTransaction(
        {
          actionId: id,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: transaction.referenceScripts,
          durableRecovery: recovery,
        },
        transaction.signed,
      );
      await beginWorkflowFundingReservationAction({
        journal: store,
        action,
      });
      await prepareWorkflowFundingReservationTransaction({
        journal: store,
        action,
        preflight,
      });
      await appendEvent({
        kind: "preflight_passed",
        actionId: id,
        txHash: transaction.txHash,
        localEvaluator: LOCAL_UPLC_EVALUATOR,
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
      await assertWorkflowFundingReservationReadyToSubmit({
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
        "resolvedOutputNonCanonical auxiliary completion changed transaction identity",
      );
    }
    if (!(await transactionConfirmed(txHash))) {
      throw new Error(
        "resolvedOutputNonCanonical auxiliary transaction is not authenticated on L1",
      );
    }
    await confirmWorkflowFundingReservationTransaction({
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

  const familyJournal: ResolvedOutputJournal = {
    load: async (familyIdentity) => {
      const current = await entries();
      const result: ResolvedOutputJournalEntry[] = [];
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
            "resolvedOutputNonCanonical confirmed transaction has no durable intent",
          );
        }
        const recovery = recoveryFrom(intent);
        if (recovery.auxiliary === true) continue;
        if (recovery.familyIdentity !== familyIdentity) {
          throw new Error(
            "resolvedOutputNonCanonical family evidence identity changed",
          );
        }
        result.push({
          sequence: result.length,
          identity: familyIdentity,
          stage: recovery.targetStage,
          action: intent.event.actionId.replace(
            "resolvedOutputNonCanonical:",
            "",
          ) as SubmitAction,
          phase: "confirmed",
          txHash: confirmedEvent.txHash,
          outputReference: null,
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
          "resolvedOutputNonCanonical submission completed without its exact pre-submit intent",
        );
      }
      const recovery = recoveryFrom(intent);
      if (
        recovery.familyIdentity !== entry.identity ||
        recovery.targetStage !== entry.stage
      ) {
        throw new Error(
          "resolvedOutputNonCanonical submission result changed durable recovery identity",
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
