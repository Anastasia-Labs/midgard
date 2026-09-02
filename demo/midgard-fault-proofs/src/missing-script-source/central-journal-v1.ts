import {
  computeFraudProofWorkflowIdV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalStoreV1,
} from "../workflow/journal-v1.js";
import type {
  MissingScriptSourceJournalEntryV1,
  MissingScriptSourceJournalV1,
} from "./workflow-v1.js";

const CATEGORY = "missingScriptSource";
const now = () => new Date().toISOString();

/**
 * Family journal bridge. The serial catalogue pass makes CATEGORY a central
 * identity; until then tests may supply a structurally equivalent store.
 */
export const createMissingScriptSourceCentralJournalV1 = ({
  store,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
}: {
  store: FraudProofWorkflowJournalStoreV1;
  deploymentFingerprint: string;
  headerHash: string;
  decisionDigest: string;
}): MissingScriptSourceJournalV1 => {
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint,
    category: CATEGORY as never,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowIdV1(identity);
  const load = async (): Promise<
    readonly MissingScriptSourceJournalEntryV1[]
  > => {
    const central = await store.load(workflowId);
    return central
      .flatMap(({ event }) => {
        if (
          event.kind !== "submission_intent" &&
          event.kind !== "submitted" &&
          event.kind !== "confirmed"
        )
          return [];
        const intent =
          event.kind === "submission_intent"
            ? event
            : central.find(
                ({ event: candidate }) =>
                  candidate.kind === "submission_intent" &&
                  candidate.txHash === event.txHash,
              )?.event;
        const recovery =
          intent?.kind === "submission_intent"
            ? intent.durableRecovery
            : undefined;
        const encoded = recovery?.familyEntry;
        if (typeof encoded !== "string") return [];
        const entry = JSON.parse(encoded) as MissingScriptSourceJournalEntryV1;
        const phase: MissingScriptSourceJournalEntryV1["phase"] =
          event.kind === "submission_intent" ? "intent" : event.kind;
        return [
          {
            ...entry,
            phase,
          },
        ] satisfies readonly MissingScriptSourceJournalEntryV1[];
      })
      .map((entry, sequence) => ({ ...entry, sequence }));
  };
  return Object.freeze({
    load: async () => await load(),
    append: async (entry: MissingScriptSourceJournalEntryV1) => {
      let current = await store.load(workflowId);
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
        current = await store.load(workflowId);
      }
      const actionId = `${CATEGORY}:${entry.action}`;
      const event =
        entry.phase === "intent"
          ? {
              kind: "submission_intent" as const,
              actionId,
              actionInput: { stage: entry.source.stage },
              durableRecovery: { familyEntry: JSON.stringify(entry) },
              attempt: 1,
              txHash: entry.txHash,
            }
          : entry.phase === "submitted"
            ? {
                kind: "submitted" as const,
                actionId,
                attempt: 1,
                txHash: entry.txHash,
              }
            : { kind: "confirmed" as const, actionId, txHash: entry.txHash };
      // Every event retains the exact family entry; submitted/confirmed are
      // paired to the preceding durable intent by txHash on load.
      if (entry.phase !== "intent") {
        const prior = [...current]
          .reverse()
          .find(
            ({ event: candidate }) =>
              candidate.kind === "submission_intent" &&
              candidate.txHash === entry.txHash,
          );
        if (prior?.event.kind !== "submission_intent")
          throw new Error(`${CATEGORY} journal phase has no durable intent`);
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
    },
  });
};

export const createMissingScriptSourceDirectoryJournalV1 = (input: {
  directory: string;
  deploymentFingerprint: string;
  headerHash: string;
  decisionDigest: string;
}) =>
  createMissingScriptSourceCentralJournalV1({
    store: new DirectoryFraudProofWorkflowJournalStoreV1(input.directory),
    deploymentFingerprint: input.deploymentFingerprint,
    headerHash: input.headerHash,
    decisionDigest: input.decisionDigest,
  });
