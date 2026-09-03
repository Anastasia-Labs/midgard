import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type {
  FieldPreimageLengthAction,
  FieldPreimageLengthJournal,
  PreparedFieldPreimageLengthWorkflow,
} from "./workflow.js";

type Action = Exclude<FieldPreimageLengthAction, "complete">;
const actions: readonly Action[] = [
  "init",
  "dispatch",
  "authenticate",
  "finalize",
  "remove",
];
const actionId = (action: Action): string =>
  `fieldPreimageLengthMismatch:${action}`;
const now = (): string => new Date().toISOString();

const artifact = (
  prepared: PreparedFieldPreimageLengthWorkflow,
): JournalJsonObject => ({
  schemaVersion: prepared.schemaVersion,
  headerHash: prepared.headerHash,
  transactionId: prepared.transactionId,
  direction: prepared.direction,
  fieldIndex: prepared.fieldIndex,
  declaredLength: prepared.declaredLength,
  actualLength: prepared.actualLength,
  preimageHex: prepared.preimageHex,
  carriage: prepared.carriage,
  evidenceDigest: prepared.evidenceDigest,
});

/** Durable bridge from the family runner to the shared production journal. */
export const createFieldPreimageLengthCentralJournalAdapter = ({
  store,
  deploymentFingerprint,
  decisionDigest,
  prepared,
  observeConfirmed,
}: {
  readonly store: FraudProofWorkflowJournalStore;
  readonly deploymentFingerprint: string;
  readonly decisionDigest: string;
  readonly prepared: PreparedFieldPreimageLengthWorkflow;
  readonly observeConfirmed: (
    action: Action,
    txHash: string,
  ) => Promise<boolean>;
}) => {
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category: "fieldPreimageLengthMismatch",
    target: { kind: "state_queue_header", headerHash: prepared.headerHash },
    decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const entries = async () => await store.load(workflowId);
  const append = async (event: FraudProofWorkflowJournalEntry["event"]) => {
    const current = await entries();
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
  const requirePrepared = async () => {
    const current = await entries();
    const existing = current.find((entry) => entry.event.kind === "prepared");
    const value = artifact(prepared);
    const digest = journalJsonDigest(value);
    if (existing?.event.kind === "prepared") {
      if (
        existing.event.artifactDigest !== digest ||
        existing.event.artifact.evidenceDigest !== prepared.evidenceDigest
      ) {
        throw new Error(
          "persisted field-preimage-length evidence digest differs from authenticated retained DA",
        );
      }
      return;
    }
    await append({ kind: "prepared", artifact: value, artifactDigest: digest });
  };
  const boundary =
    (
      action: Action,
      candidate: PreparedFieldPreimageLengthWorkflow,
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      if (candidate.evidenceDigest !== prepared.evidenceDigest) {
        throw new Error(
          "fieldPreimageLengthMismatch pre-submit evidence changed",
        );
      }
      await requirePrepared();
      const current = await entries();
      const id = actionId(action);
      const prior = [...current]
        .reverse()
        .find(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId === id,
        );
      if (prior?.event.kind === "submission_intent") {
        if (prior.event.txHash !== transaction.txHash) {
          throw new Error(
            "fieldPreimageLengthMismatch transaction identity changed across restart",
          );
        }
        return;
      }
      await append({
        kind: "submission_intent",
        actionId: id,
        actionInput: {
          action,
          evidenceDigest: prepared.evidenceDigest,
        },
        durableRecovery: { action },
        attempt: 1,
        txHash: transaction.txHash,
      });
    };
  const auxiliaryBoundary =
    (kind: "publication" | "certificate"): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      await requirePrepared();
      const id = `fieldPreimageLengthMismatch:carriage:${kind}:${transaction.txHash}`;
      const current = await entries();
      if (
        kind === "certificate" &&
        current.some(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId.startsWith(
              "fieldPreimageLengthMismatch:carriage:certificate:",
            ) &&
            entry.event.txHash !== transaction.txHash,
        )
      ) {
        throw new Error(
          "fieldPreimageLengthMismatch certificate transaction identity changed across restart",
        );
      }
      if (
        current.some(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId === id,
        )
      ) {
        return;
      }
      await append({
        kind: "submission_intent",
        actionId: id,
        actionInput: { kind, evidenceDigest: prepared.evidenceDigest },
        durableRecovery: { kind },
        attempt: 1,
        txHash: transaction.txHash,
      });
    };
  const auxiliaryConfirmed = async (
    kind: "publication" | "certificate",
    txHashes: readonly string[],
  ) => {
    for (const txHash of new Set(txHashes)) {
      const id = `fieldPreimageLengthMismatch:carriage:${kind}:${txHash}`;
      let current = await entries();
      if (
        !current.some(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId === id &&
            entry.event.txHash === txHash,
        )
      ) {
        continue;
      }
      if (
        !current.some(
          (entry) =>
            entry.event.kind === "submitted" && entry.event.actionId === id,
        )
      ) {
        await append({ kind: "submitted", actionId: id, attempt: 1, txHash });
        current = await entries();
      }
      if (
        !current.some(
          (entry) =>
            entry.event.kind === "confirmed" && entry.event.actionId === id,
        )
      ) {
        await append({
          kind: "reconciled",
          actionId: id,
          outcome: "confirmed",
          txHash,
        });
        await append({ kind: "confirmed", actionId: id, txHash });
      }
    }
  };
  const load = async (): Promise<FieldPreimageLengthJournal | null> => {
    await requirePrepared();
    const current = await entries();
    const transactionIds: Partial<Record<Action, string>> = {};
    const confirmed: Action[] = [];
    for (const action of actions) {
      const id = actionId(action);
      const intent = [...current]
        .reverse()
        .find(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId === id,
        );
      if (intent?.event.kind !== "submission_intent") continue;
      const intentTxHash = intent.event.txHash;
      transactionIds[action] = intentTxHash;
      const confirmation = current.find(
        (entry) =>
          entry.event.kind === "confirmed" &&
          entry.event.actionId === id &&
          entry.event.txHash === intentTxHash,
      );
      if (confirmation !== undefined) confirmed.push(action);
    }
    return Object.freeze({
      prepared,
      confirmed: Object.freeze(confirmed),
      transactionIds: Object.freeze(transactionIds),
    });
  };
  const save = async (journal: FieldPreimageLengthJournal) => {
    if (journal.prepared.evidenceDigest !== prepared.evidenceDigest) {
      throw new Error("fieldPreimageLengthMismatch journal evidence changed");
    }
    for (const action of actions) {
      const txHash = journal.transactionIds[action];
      if (txHash === undefined) continue;
      const id = actionId(action);
      let current = await entries();
      const intent = current.find(
        (entry) =>
          entry.event.kind === "submission_intent" &&
          entry.event.actionId === id &&
          entry.event.txHash === txHash,
      );
      if (intent === undefined) {
        throw new Error(
          "fieldPreimageLengthMismatch submission completed without pre-submit intent",
        );
      }
      if (
        !current.some(
          (entry) =>
            entry.event.kind === "submitted" &&
            entry.event.actionId === id &&
            entry.event.txHash === txHash,
        )
      ) {
        await append({ kind: "submitted", actionId: id, attempt: 1, txHash });
        current = await entries();
      }
      if (journal.confirmed.includes(action)) {
        if (
          !current.some(
            (entry) =>
              entry.event.kind === "confirmed" &&
              entry.event.actionId === id &&
              entry.event.txHash === txHash,
          )
        ) {
          await append({
            kind: "reconciled",
            actionId: id,
            outcome: "confirmed",
            txHash,
          });
          await append({ kind: "confirmed", actionId: id, txHash });
        }
      }
    }
  };
  return Object.freeze({
    workflowId,
    identity,
    boundary,
    auxiliaryBoundary,
    auxiliaryConfirmed,
    journal: Object.freeze({ load, save, observeConfirmed }),
  });
};
