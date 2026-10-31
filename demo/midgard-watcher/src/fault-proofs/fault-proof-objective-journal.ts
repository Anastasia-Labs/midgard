import {
  type FraudProofWorkflowJournalEntry,
  validateFraudProofWorkflowJournal,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import type { WatcherWorkflowJournal } from "./watcher-workflow-journal.js";

export type WatcherProofObjective = Readonly<{
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
}>;

export type WatcherProofExecution = Readonly<{
  workflowId: string;
  entries: readonly FraudProofWorkflowJournalEntry[];
}>;

/**
 * Whether a failed journal read refused what it found (a sequence gap, a
 * foreign or invalid execution) rather than met a storage error (one
 * carrying an error code). A refusal reads the same on every retry.
 */
export const isWatcherProofJournalRefusal = (error: unknown): boolean =>
  !(
    error instanceof Error &&
    "code" in error &&
    typeof error.code === "string"
  );

/** One selected durable execution per objective; scheduling records are not
 * completion evidence. Only affected objectives are read after startup. */
export const readWatcherProofExecution = (input: {
  readonly journal: WatcherWorkflowJournal;
  readonly deploymentFingerprint: string;
  readonly objective: WatcherProofObjective;
  readonly selectedWorkflowId?: string;
}): WatcherProofExecution | undefined => {
  const selected = input.journal.readExecution(input.objective);
  if (selected === undefined) {
    if (input.selectedWorkflowId !== undefined)
      throw new Error("selected proof execution disappeared");
    return undefined;
  }
  const first = selected.entries[0]!;
  validateFraudProofWorkflowJournal({
    workflowId: selected.workflowId,
    entries: selected.entries,
    expectedIdentity: first.identity,
  });
  if (first.identity.deploymentFingerprint !== input.deploymentFingerprint)
    throw new Error("proof objective contains a foreign execution");
  if (
    input.selectedWorkflowId !== undefined &&
    input.selectedWorkflowId !== selected.workflowId
  )
    throw new Error("proof objective changed its selected execution");
  return selected;
};
