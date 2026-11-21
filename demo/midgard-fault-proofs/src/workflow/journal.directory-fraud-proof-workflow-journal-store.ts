import { randomUUID } from "node:crypto";
import { link, mkdir, open, readdir, readFile, unlink } from "node:fs/promises";
import { join } from "node:path";

import { createFraudProofWorkflowJournalFold } from "./journal.create-fraud-proof-workflow-journal-fold.js";
import {
  ConcurrentFraudProofWorkflowWriteError,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalFold,
  type FraudProofWorkflowJournalStore,
  validateWorkflowId,
} from "./journal.fraud-proof-workflow-journal-event.js";
import { type FraudProofWorkflowIdentity } from "./journal.fraud-proof-workflow-terminal.js";

export const validateFraudProofWorkflowJournal = ({
  workflowId,
  entries,
  expectedIdentity,
}: {
  readonly workflowId: string;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly expectedIdentity?: FraudProofWorkflowIdentity;
}): readonly FraudProofWorkflowJournalEntry[] => {
  const fold = createFraudProofWorkflowJournalFold({
    workflowId,
    expectedIdentity,
  });
  for (const entry of entries) fold.push(entry);
  return entries;
};

/**
 * A validated journal prefix together with the fold that accepted it, so the
 * next append validates one entry against retained state.
 */
type ValidatedJournal = {
  readonly entries: FraudProofWorkflowJournalEntry[];
  readonly fold: FraudProofWorkflowJournalFold;
};

const foldJournal = (
  workflowId: string,
  entries: readonly FraudProofWorkflowJournalEntry[],
): ValidatedJournal => {
  const fold = createFraudProofWorkflowJournalFold({ workflowId });
  for (const entry of entries) fold.push(entry);
  return { entries: [...entries], fold };
};

/**
 * Validates `entry` as the next entry of `journal`. Appending an entry whose
 * identity does not derive the workflow id fails inside the fold, which is
 * the same rejection the whole-journal validator reaches through its
 * requested-identity check. The fold is poisoned when `push` throws, so the
 * caller must drop the cached journal on failure.
 */
const pushValidated = (
  journal: ValidatedJournal,
  entry: FraudProofWorkflowJournalEntry,
): void => {
  journal.fold.push(entry);
  journal.entries.push(entry);
};

/** In-memory store with optimistic sequence checks, useful for embedded use. */
export class MemoryFraudProofWorkflowJournalStore
  implements FraudProofWorkflowJournalStore
{
  private readonly journalsByWorkflow = new Map<string, ValidatedJournal>();

  async load(
    workflowId: string,
  ): Promise<readonly FraudProofWorkflowJournalEntry[]> {
    validateWorkflowId(workflowId);
    return [...(this.journalsByWorkflow.get(workflowId)?.entries ?? [])];
  }

  async append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void> {
    const current = this.journalsByWorkflow.get(entry.workflowId);
    const currentLength = current?.entries.length ?? 0;
    if (
      currentLength !== expectedSequence ||
      entry.sequence !== expectedSequence
    ) {
      throw new ConcurrentFraudProofWorkflowWriteError(
        `journal sequence changed: expected=${expectedSequence.toString()} actual=${currentLength.toString()}`,
      );
    }
    const journal = current ?? foldJournal(entry.workflowId, []);
    try {
      pushValidated(journal, entry);
    } catch (error) {
      // The fold is poisoned; rebuild from the entries that were accepted.
      if (current !== undefined) {
        this.journalsByWorkflow.set(
          entry.workflowId,
          foldJournal(entry.workflowId, current.entries),
        );
      }
      throw error;
    }
    this.journalsByWorkflow.set(entry.workflowId, journal);
  }
}

/**
 * Crash-safe filesystem journal: one immutable, fsynced file per sequence.
 * The final hard-link is an atomic compare-and-append; concurrent processes
 * racing for the same sequence cannot both win. Temporary files are ignored on
 * recovery and never treated as submitted/confirmed evidence.
 */
export class DirectoryFraudProofWorkflowJournalStore
  implements FraudProofWorkflowJournalStore
{
  constructor(private readonly rootDirectory: string) {}

  /**
   * The validated journal per workflow as this store last read it from disk,
   * with the entry file names it covers. Entry files are immutable once
   * linked into place (the append protocol below never rewrites one), so a
   * later listing that still begins with exactly these names extends the
   * cached prefix: only the newer files are read and folded. Any other
   * listing, such as a directory that was removed and rebuilt, is refolded
   * from scratch. Nothing here is trusted across the cache's own name check.
   */
  private readonly journalsByWorkflow = new Map<
    string,
    ValidatedJournal & { readonly entryNames: readonly string[] }
  >();

  private workflowDirectory(workflowId: string): string {
    validateWorkflowId(workflowId);
    return join(this.rootDirectory, workflowId);
  }

  private async loadValidated(
    workflowId: string,
  ): Promise<
    (ValidatedJournal & { readonly entryNames: readonly string[] }) | undefined
  > {
    const directory = this.workflowDirectory(workflowId);
    let names: string[];
    try {
      names = await readdir(directory);
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        this.journalsByWorkflow.delete(workflowId);
        return undefined;
      }
      throw error;
    }
    const entryNames = names
      .filter((name) => /^\d{8}\.json$/u.test(name))
      .sort();
    const cached = this.journalsByWorkflow.get(workflowId);
    const extendsCached =
      cached !== undefined &&
      cached.entryNames.length <= entryNames.length &&
      cached.entryNames.every((name, index) => name === entryNames[index]);
    const journal =
      extendsCached && cached !== undefined
        ? cached
        : { ...foldJournal(workflowId, []), entryNames: [] as string[] };
    const newNames = entryNames.slice(journal.entryNames.length);
    const newEntries = await Promise.all(
      newNames.map(
        async (name) =>
          JSON.parse(
            await readFile(join(directory, name), "utf8"),
            (_key, value) => value,
          ) as FraudProofWorkflowJournalEntry,
      ),
    );
    try {
      for (const entry of newEntries) pushValidated(journal, entry);
    } catch (error) {
      this.journalsByWorkflow.delete(workflowId);
      throw error;
    }
    const validated = { ...journal, entryNames };
    this.journalsByWorkflow.set(workflowId, validated);
    return validated;
  }

  async load(
    workflowId: string,
  ): Promise<readonly FraudProofWorkflowJournalEntry[]> {
    const journal = await this.loadValidated(workflowId);
    return journal === undefined ? [] : [...journal.entries];
  }

  async append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void> {
    if (entry.sequence !== expectedSequence) {
      throw new ConcurrentFraudProofWorkflowWriteError(
        `entry sequence ${entry.sequence.toString()} does not equal expected ${expectedSequence.toString()}`,
      );
    }
    const directory = this.workflowDirectory(entry.workflowId);
    await mkdir(directory, { recursive: true });
    const current = await this.loadValidated(entry.workflowId);
    if (current === undefined) {
      throw new Error("journal directory vanished while appending");
    }
    if (current.entries.length !== expectedSequence) {
      throw new ConcurrentFraudProofWorkflowWriteError(
        `journal sequence changed: expected=${expectedSequence.toString()} actual=${current.entries.length.toString()}`,
      );
    }
    // Validate against the fold's retained state. The fold now carries the
    // entry, so until the hard-link below has made it part of the on-disk
    // history any failure must drop the cached journal rather than leave a
    // fold ahead of the directory.
    const finalName = `${expectedSequence.toString().padStart(8, "0")}.json`;
    const commitCache = (): void => {
      this.journalsByWorkflow.set(entry.workflowId, {
        entries: [...current.entries, entry],
        fold: current.fold,
        entryNames: [...current.entryNames, finalName],
      });
    };
    this.journalsByWorkflow.delete(entry.workflowId);
    current.fold.push(entry);
    const finalPath = join(directory, finalName);
    const temporaryPath = join(
      directory,
      `.${expectedSequence.toString().padStart(8, "0")}.${randomUUID()}.tmp`,
    );
    const handle = await open(temporaryPath, "wx", 0o600);
    try {
      await handle.writeFile(`${JSON.stringify(entry)}\n`, "utf8");
      await handle.sync();
    } finally {
      await handle.close();
    }
    try {
      await link(temporaryPath, finalPath);
    } catch (error) {
      await unlink(temporaryPath).catch(() => undefined);
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "EEXIST"
      ) {
        throw new ConcurrentFraudProofWorkflowWriteError(
          `journal sequence ${expectedSequence.toString()} was written concurrently`,
        );
      }
      throw error;
    }
    commitCache();
    await unlink(temporaryPath);
    const directoryHandle = await open(directory, "r");
    try {
      await directoryHandle.sync();
    } finally {
      await directoryHandle.close();
    }
  }
}
