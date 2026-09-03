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
  WitnessScriptDecodingAction,
  WitnessScriptDecodingJournalEntry,
  WitnessScriptDecodingStage,
} from "./witness-script-decoding.js";

type WitnessScriptDecodingJournal = Readonly<{
  load: (
    identity: string,
  ) => Promise<readonly WitnessScriptDecodingJournalEntry[]>;
  append: (entry: WitnessScriptDecodingJournalEntry) => Promise<void>;
}>;

type SubmitAction = Exclude<WitnessScriptDecodingAction, "done">;
type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: WitnessScriptDecodingStage;
  targetStage: WitnessScriptDecodingStage;
  auxiliary?: boolean;
}>;

const TX_HASH = /^[0-9a-f]{64}$/u;
const stages: readonly WitnessScriptDecodingStage[] = [
  "none",
  "step01",
  "step02",
  "scan",
  "step04",
  "proven",
  "removed",
  "cancelled",
];
const actionId = (action: SubmitAction): string =>
  `witnessScriptDecoding:${action}`;
const now = (): string => new Date().toISOString();

const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error("witnessScriptDecoding journal entry is not an intent");
  }
  const value = entry.event.durableRecovery;
  const familyIdentity = value?.familyIdentity;
  const sourceStage = value?.sourceStage;
  const targetStage = value?.targetStage;
  if (
    typeof familyIdentity !== "string" ||
    typeof sourceStage !== "string" ||
    typeof targetStage !== "string" ||
    !stages.includes(sourceStage as WitnessScriptDecodingStage) ||
    !stages.includes(targetStage as WitnessScriptDecodingStage)
  ) {
    throw new Error("witnessScriptDecoding durable intent is incomplete");
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as WitnessScriptDecodingStage,
    targetStage: targetStage as WitnessScriptDecodingStage,
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
export const createWitnessScriptDecodingCentralJournalAdapter = ({
  store,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  transactionConfirmed,
}: {
  readonly store: FraudProofWorkflowJournalStore;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly decisionDigest: string;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
}) => {
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category: "witnessScriptDecoding",
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
      category: "witnessScriptDecoding",
      headerHash,
      checkpoint,
    });

  const ensurePrepared = async (familyIdentity: string): Promise<void> => {
    const current = await entries();
    const prepared = current.find(({ event }) => event.kind === "prepared");
    const artifact = { category: "witnessScriptDecoding", familyIdentity };
    const artifactDigest = journalJsonDigest(artifact);
    if (prepared?.event.kind === "prepared") {
      if (prepared.event.artifactDigest !== artifactDigest) {
        throw new Error(
          "witnessScriptDecoding prepared family evidence identity changed",
        );
      }
      return;
    }
    await appendEvent({ kind: "prepared", artifact, artifactDigest });
  };

  const begin = async (
    kind: SubmitAction,
    familyIdentity: string,
    sourceStage: WitnessScriptDecodingStage,
    targetStage: WitnessScriptDecodingStage,
  ): Promise<void> => {
    await ensurePrepared(familyIdentity);
    if (unresolvedIntent(await entries()) !== undefined) {
      throw new Error(
        "witnessScriptDecoding unresolved submission must reconcile before another build",
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
      sourceStage: WitnessScriptDecodingStage,
      targetStage: WitnessScriptDecodingStage,
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      if (!TX_HASH.test(transaction.txHash)) {
        throw new Error(
          "witnessScriptDecoding pre-submit hash is not canonical",
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
            "witnessScriptDecoding transaction identity changed across restart",
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
    observedStage: WitnessScriptDecodingStage,
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
      "witnessScriptDecoding authenticated stage/transaction identity substitution",
    );
  };

  const auxiliaryBoundary =
    (
      kind: "publication" | "certificate",
      familyIdentity: string,
      stage: WitnessScriptDecodingStage,
      captured: string[],
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      await ensurePrepared(familyIdentity);
      const pending = unresolvedIntent(await entries());
      if (pending?.event.kind === "submission_intent") {
        if (recoveryFrom(pending).auxiliary !== true) {
          throw new Error(
            "witnessScriptDecoding auxiliary actuation found an unresolved proof transaction",
          );
        }
        await confirmAuxiliary(pending.event.txHash);
      }
      const id = `witnessScriptDecoding:${kind}:${transaction.txHash}`;
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
        "witnessScriptDecoding auxiliary completion changed transaction identity",
      );
    }
    if (!(await transactionConfirmed(txHash))) {
      throw new Error(
        "witnessScriptDecoding auxiliary transaction is not authenticated on L1",
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

  const familyJournal: WitnessScriptDecodingJournal = {
    load: async (familyIdentity) => {
      const current = await entries();
      const result: WitnessScriptDecodingJournalEntry[] = [];
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
            "witnessScriptDecoding confirmed transaction has no durable intent",
          );
        }
        const recovery = recoveryFrom(intent);
        if (recovery.auxiliary === true) continue;
        if (recovery.familyIdentity !== familyIdentity) {
          throw new Error(
            "witnessScriptDecoding family evidence identity changed",
          );
        }
        result.push({
          sequence: result.length,
          identity: familyIdentity,
          stage: recovery.targetStage,
          transactionId: confirmedEvent.txHash,
          outputReference: null,
          checkpointHash: null,
        });
      }
      return result;
    },
    append: async (entry) => {
      const intent = unresolvedIntent(await entries());
      if (
        intent?.event.kind !== "submission_intent" ||
        intent.event.txHash !== entry.transactionId
      ) {
        throw new Error(
          "witnessScriptDecoding submission completed without its exact pre-submit intent",
        );
      }
      const recovery = recoveryFrom(intent);
      if (
        recovery.familyIdentity !== entry.identity ||
        recovery.targetStage !== entry.stage
      ) {
        throw new Error(
          "witnessScriptDecoding submission result changed durable recovery identity",
        );
      }
      await appendEvent({
        kind: "submitted",
        actionId: intent.event.actionId,
        attempt: intent.event.attempt,
        txHash: entry.transactionId,
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
