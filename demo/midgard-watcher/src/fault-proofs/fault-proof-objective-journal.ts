import { readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
  validateFraudProofWorkflowJournal,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

const EXECUTION_ID = /^[0-9a-f]{64}$/u;

export type WatcherProofObjective = Readonly<{
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
}>;

export type WatcherProofExecution = Readonly<{
  workflowId: string;
  entries: readonly FraudProofWorkflowJournalEntry[];
}>;

/** One selected durable execution per objective; scheduling records are not
 * completion evidence. Only affected objectives are read after startup. */
export const readWatcherProofExecution = async (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly objective: WatcherProofObjective;
  readonly selectedWorkflowId?: string;
}): Promise<WatcherProofExecution | undefined> => {
  const directory = join(
    input.journalRoot,
    "fault-proofs",
    input.objective.category,
    input.objective.headerHash,
  );
  let names;
  try {
    names = await readdir(directory, { withFileTypes: true });
  } catch (error) {
    if (error instanceof Error && "code" in error && error.code === "ENOENT") {
      if (input.selectedWorkflowId !== undefined)
        throw new Error("selected proof execution disappeared");
      return undefined;
    }
    throw error;
  }
  if ((await realpath(directory)) !== directory)
    throw new Error("proof objective journal traverses a symlink");
  if (names.length > 2_048)
    throw new Error("proof objective exceeds the execution recovery bound");
  const journal = new DirectoryFraudProofWorkflowJournalStore(directory);
  let selected: WatcherProofExecution | undefined;
  for (const name of names) {
    if (!name.isDirectory() || !EXECUTION_ID.test(name.name))
      throw new Error("proof objective contains an invalid execution identity");
    const executionDirectory = join(directory, name.name);
    if ((await realpath(executionDirectory)) !== executionDirectory)
      throw new Error("proof execution journal traverses a symlink");
    const entries = await journal.load(name.name);
    const first = entries[0];
    if (first === undefined) continue;
    validateFraudProofWorkflowJournal({
      workflowId: name.name,
      entries,
      expectedIdentity: first.identity,
    });
    if (
      first.identity.deploymentFingerprint !== input.deploymentFingerprint ||
      first.identity.category !== input.objective.category ||
      first.identity.target.kind !== "state_queue_header" ||
      first.identity.target.headerHash !== input.objective.headerHash
    )
      throw new Error("proof objective contains a foreign execution");
    if (selected !== undefined)
      throw new Error("proof objective has multiple durable executions");
    if (
      input.selectedWorkflowId !== undefined &&
      input.selectedWorkflowId !== name.name
    )
      throw new Error("proof objective changed its selected execution");
    selected = { workflowId: name.name, entries };
  }
  if (selected === undefined && input.selectedWorkflowId !== undefined)
    throw new Error("selected proof execution became empty");
  return selected;
};
