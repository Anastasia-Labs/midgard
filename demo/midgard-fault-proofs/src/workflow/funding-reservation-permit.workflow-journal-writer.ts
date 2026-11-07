import type { WorkflowFundingReservationPort } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
} from "./journal.js";

/** How a workflow records its journal events: `entriesFor` numbers them
 * after the entries already recorded, and `recorded` learns of entries once
 * they are durable. */
export type WorkflowJournalWriter = Readonly<{
  journal: FraudProofWorkflowJournalStore;
  entriesFor: (
    events: readonly FraudProofWorkflowJournalEvent[],
  ) => Promise<readonly FraudProofWorkflowJournalEntry[]>;
  recorded: (entries: readonly FraudProofWorkflowJournalEntry[]) => void;
}>;

const appendEntries = async (
  writer: WorkflowJournalWriter,
  entries: readonly FraudProofWorkflowJournalEntry[],
): Promise<void> => {
  for (const entry of entries)
    await writer.journal.append(entry, entry.sequence);
  writer.recorded(entries);
};

/** Appends journal-only events, one after another. */
export const appendWorkflowJournalEvents = async (
  writer: WorkflowJournalWriter,
  events: readonly FraudProofWorkflowJournalEvent[],
): Promise<void> => {
  if (events.length === 0) return;
  await appendEntries(writer, await writer.entriesFor(events));
};

/**
 * Applies a funding change together with the journal events it implies. A
 * durable port commits them in the change's own transaction, and a durable
 * port that does not keep the writer's journal is refused before any change;
 * for an in-memory port they are appended once the change returns. A null
 * result means the change committed nothing, and nothing is recorded.
 */
export const commitWithJournal = async <T>({
  port,
  writer,
  events,
  change,
}: {
  readonly port: WorkflowFundingReservationPort;
  readonly writer: WorkflowJournalWriter;
  readonly events: readonly FraudProofWorkflowJournalEvent[];
  /** `journal` is what the port commits with the change: `entries` when it
   * keeps the writer's journal, otherwise none. */
  readonly change: (
    recorded: Readonly<{
      journal: readonly FraudProofWorkflowJournalEntry[];
      entries: readonly FraudProofWorkflowJournalEntry[];
    }>,
  ) => Promise<T>;
}): Promise<T> => {
  const atomic = port.commitsJournal?.(writer.journal);
  if (atomic === false)
    throw new Error("funding port does not keep this workflow journal");
  const entries = events.length === 0 ? [] : await writer.entriesFor(events);
  const result = await change({
    journal: atomic === true ? entries : [],
    entries,
  });
  if (result === null) return result;
  if (atomic === true) writer.recorded(entries);
  else await appendEntries(writer, entries);
  return result;
};

/** A writer over a journal store read afresh for each record: the family
 * workflows that keep no entries of their own. The first record starts the
 * workflow. */
export const createStoredWorkflowJournalWriter = ({
  store,
  workflowId,
  identity,
  now,
}: {
  readonly store: FraudProofWorkflowJournalStore;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly now: () => string;
}): WorkflowJournalWriter =>
  Object.freeze({
    journal: store,
    entriesFor: async (events) => {
      const current = await store.load(workflowId);
      const all: readonly FraudProofWorkflowJournalEvent[] =
        current.length === 0 ? [{ kind: "started" }, ...events] : events;
      return all.map((event, index) =>
        Object.freeze({
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence: current.length + index,
          recordedAt: now(),
          event,
        }),
      );
    },
    recorded: () => undefined,
  });
