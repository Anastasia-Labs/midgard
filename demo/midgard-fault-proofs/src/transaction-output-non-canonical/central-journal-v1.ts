import { createHash } from "node:crypto";

import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
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
  TransactionOutputActionV1,
  TransactionOutputJournalEntryV1,
  TransactionOutputJournalV1,
  TransactionOutputStageV1,
} from "./transaction-output-non-canonical-v1.js";

const familyWorkflowIdV1 = (identity: FraudProofWorkflowIdentityV1): string => {
  if (!/^[0-9a-f]{64}$/u.test(identity.deploymentFingerprint))
    throw new Error(
      "transactionOutputNonCanonical deployment fingerprint is invalid",
    );
  if (
    identity.target.kind !== "state_queue_header" ||
    !/^[0-9a-f]{56}$/u.test(identity.target.headerHash)
  )
    throw new Error(
      "transactionOutputNonCanonical target header hash is invalid",
    );
  return createHash("sha256")
    .update(
      [
        FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
        identity.deploymentFingerprint,
        "transactionOutputNonCanonical",
        `header:${identity.target.headerHash}`,
        ...(identity.decisionDigest === undefined
          ? []
          : [`decision:${identity.decisionDigest}`]),
      ].join("\u0000"),
    )
    .digest("hex");
};

type SubmitAction = Exclude<TransactionOutputActionV1, "done">;
type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: TransactionOutputStageV1;
  targetStage: TransactionOutputStageV1;
  auxiliary?: boolean;
}>;

const TX_HASH = /^[0-9a-f]{64}$/u;
const stages: readonly TransactionOutputStageV1[] = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04",
  "proven",
  "removed",
  "cancelled",
];
const actionId = (action: SubmitAction): string =>
  `transactionOutputNonCanonical:${action}`;
const now = (): string => new Date().toISOString();

const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntryV1,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error(
      "transactionOutputNonCanonical journal entry is not an intent",
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
    !stages.includes(sourceStage as TransactionOutputStageV1) ||
    !stages.includes(targetStage as TransactionOutputStageV1)
  ) {
    throw new Error(
      "transactionOutputNonCanonical durable intent is incomplete",
    );
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as TransactionOutputStageV1,
    targetStage: targetStage as TransactionOutputStageV1,
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
export const createTransactionOutputNonCanonicalCentralJournalAdapterV1 = ({
  store,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  transactionConfirmed,
}: {
  readonly store: FraudProofWorkflowJournalStoreV1;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly decisionDigest: string;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
}) => {
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint,
    category:
      "transactionOutputNonCanonical" as FraudProofCatalogueCategoryName,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest,
  };
  // Byte-for-byte the central workflow-id preimage, kept family-local until
  // the frozen category is admitted to the SDK union.
  const workflowId = familyWorkflowIdV1(identity);
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
      category:
        "transactionOutputNonCanonical" as FraudProofCatalogueCategoryName,
      headerHash,
      checkpoint,
    });

  const ensurePrepared = async (familyIdentity: string): Promise<void> => {
    const current = await entries();
    const prepared = current.find(({ event }) => event.kind === "prepared");
    const artifact = {
      category: "transactionOutputNonCanonical",
      familyIdentity,
    };
    const artifactDigest = journalJsonDigestV1(artifact);
    if (prepared?.event.kind === "prepared") {
      if (prepared.event.artifactDigest !== artifactDigest) {
        throw new Error(
          "transactionOutputNonCanonical prepared family evidence identity changed",
        );
      }
      return;
    }
    await appendEvent({ kind: "prepared", artifact, artifactDigest });
  };

  const begin = async (
    kind: SubmitAction,
    familyIdentity: string,
    sourceStage: TransactionOutputStageV1,
    targetStage: TransactionOutputStageV1,
  ): Promise<void> => {
    await ensurePrepared(familyIdentity);
    if (unresolvedIntent(await entries()) !== undefined) {
      throw new Error(
        "transactionOutputNonCanonical unresolved submission must reconcile before another build",
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
      sourceStage: TransactionOutputStageV1,
      targetStage: TransactionOutputStageV1,
    ): FraudProofPreSubmitBoundaryV1 =>
    async (transaction) => {
      if (!TX_HASH.test(transaction.txHash)) {
        throw new Error(
          "transactionOutputNonCanonical pre-submit hash is not canonical",
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
            "transactionOutputNonCanonical transaction identity changed across restart",
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
    observedStage: TransactionOutputStageV1,
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
      "transactionOutputNonCanonical authenticated stage/transaction identity substitution",
    );
  };

  const auxiliaryBoundary =
    (
      kind: "publication" | "certificate",
      familyIdentity: string,
      stage: TransactionOutputStageV1,
      captured: string[],
    ): FraudProofPreSubmitBoundaryV1 =>
    async (transaction) => {
      await ensurePrepared(familyIdentity);
      const pending = unresolvedIntent(await entries());
      if (pending?.event.kind === "submission_intent") {
        if (recoveryFrom(pending).auxiliary !== true) {
          throw new Error(
            "transactionOutputNonCanonical auxiliary actuation found an unresolved proof transaction",
          );
        }
        await confirmAuxiliary(pending.event.txHash);
      }
      const id = `transactionOutputNonCanonical:${kind}:${transaction.txHash}`;
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
        "transactionOutputNonCanonical auxiliary completion changed transaction identity",
      );
    }
    if (!(await transactionConfirmed(txHash))) {
      throw new Error(
        "transactionOutputNonCanonical auxiliary transaction is not authenticated on L1",
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

  const familyJournal: TransactionOutputJournalV1 = {
    load: async (familyIdentity) => {
      const current = await entries();
      const result: TransactionOutputJournalEntryV1[] = [];
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
            "transactionOutputNonCanonical confirmed transaction has no durable intent",
          );
        }
        const recovery = recoveryFrom(intent);
        if (recovery.auxiliary === true) continue;
        if (recovery.familyIdentity !== familyIdentity) {
          throw new Error(
            "transactionOutputNonCanonical family evidence identity changed",
          );
        }
        result.push({
          sequence: result.length,
          identity: familyIdentity,
          stage: recovery.targetStage,
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
          "transactionOutputNonCanonical submission completed without its exact pre-submit intent",
        );
      }
      const recovery = recoveryFrom(intent);
      if (
        recovery.familyIdentity !== entry.identity ||
        recovery.targetStage !== entry.stage
      ) {
        throw new Error(
          "transactionOutputNonCanonical submission result changed durable recovery identity",
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
