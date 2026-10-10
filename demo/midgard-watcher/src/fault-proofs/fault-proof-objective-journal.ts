import { randomUUID } from "node:crypto";
import { mkdir, readdir, realpath, rename, rm } from "node:fs/promises";
import { dirname, join } from "node:path";

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

/** The workflow directory that holds an objective's executions. */
export const watcherProofObjectiveDirectory = (
  journalRoot: string,
  objective: WatcherProofObjective,
): string =>
  join(journalRoot, "fault-proofs", objective.category, objective.headerHash);

const isMissing = (error: unknown): boolean =>
  error instanceof Error && "code" in error && error.code === "ENOENT";

/**
 * Whether a failed journal read refused what it found (a symlink, a sequence
 * gap, a foreign or invalid execution) rather than met a filesystem error
 * (one carrying an errno code). A refusal reads the same on every retry.
 */
export const isWatcherProofJournalRefusal = (error: unknown): boolean =>
  !(
    error instanceof Error &&
    "code" in error &&
    typeof error.code === "string"
  );

// Removal renames an objective's directory here first, on the same
// filesystem, so the rename is atomic and a crash never leaves a partly
// removed directory at the objective's path. The leading dot keeps it apart
// from every category name.
const TOMBSTONES = ".removing";

/** Where removed objective directories wait for their recursive delete. */
export const watcherProofTombstoneDirectory = (journalRoot: string): string =>
  join(journalRoot, "fault-proofs", TOMBSTONES);

/**
 * Removes an objective's workflow directory: renames it to a tombstone, runs
 * `forget` (which drops the objective's rows), then deletes the tombstone. A
 * crash before the rename leaves the directory whole; after it, the
 * objective's path is absent; during the delete, only the tombstone remains,
 * which `sweepWatcherProofTombstones` removes. A missing directory is already
 * removed and still runs `forget`. A path whose parent resolves through a
 * symlink is refused: nothing moves and `forget` does not run. A symlinked
 * directory itself is renamed as a link, so its target is never followed or
 * deleted.
 */
export const removeWatcherProofObjectiveDirectory = async (
  journalRoot: string,
  objective: WatcherProofObjective,
  forget: () => void = () => undefined,
): Promise<"removed" | "refused"> => {
  const directory = watcherProofObjectiveDirectory(journalRoot, objective);
  const parent = dirname(directory);
  let resolved;
  try {
    resolved = await realpath(parent);
  } catch (error) {
    if (!isMissing(error)) throw error;
    forget();
    return "removed";
  }
  if (resolved !== parent) return "refused";
  const tombstones = watcherProofTombstoneDirectory(journalRoot);
  await mkdir(tombstones, { recursive: true });
  if ((await realpath(tombstones)) !== tombstones) return "refused";
  const tombstone = join(
    tombstones,
    `${objective.category}.${objective.headerHash}.${randomUUID()}`,
  );
  try {
    await rename(directory, tombstone);
  } catch (error) {
    if (!isMissing(error)) throw error;
    forget();
    return "removed";
  }
  forget();
  await rm(tombstone, { recursive: true, force: true });
  return "removed";
};

/**
 * Deletes every tombstone a removal left behind. Returns the ones it could
 * not delete, each with the failure; the next sweep retries them.
 */
export const sweepWatcherProofTombstones = async (
  journalRoot: string,
): Promise<readonly Readonly<{ name: string; detail: string }>[]> => {
  const tombstones = watcherProofTombstoneDirectory(journalRoot);
  let names;
  try {
    names = await readdir(tombstones);
  } catch (error) {
    if (isMissing(error)) return [];
    return [{ name: TOMBSTONES, detail: messageOf(error) }];
  }
  try {
    if ((await realpath(tombstones)) !== tombstones)
      throw new Error("proof objective tombstones traverse a symlink");
  } catch (error) {
    return [{ name: TOMBSTONES, detail: messageOf(error) }];
  }
  const failed: Readonly<{ name: string; detail: string }>[] = [];
  for (const name of names)
    try {
      await rm(join(tombstones, name), { recursive: true, force: true });
    } catch (error) {
      failed.push({ name, detail: messageOf(error) });
    }
  return failed;
};

const messageOf = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/** One selected durable execution per objective; scheduling records are not
 * completion evidence. Only affected objectives are read after startup. */
export const readWatcherProofExecution = async (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly objective: WatcherProofObjective;
  readonly selectedWorkflowId?: string;
}): Promise<WatcherProofExecution | undefined> => {
  const directory = watcherProofObjectiveDirectory(
    input.journalRoot,
    input.objective,
  );
  let names;
  try {
    names = await readdir(directory, { withFileTypes: true });
  } catch (error) {
    if (isMissing(error)) {
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
