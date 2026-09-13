import { open, readFile, rename } from "node:fs/promises";
import path from "node:path";

import type {
  ScriptIntegrityHashMismatchJournal,
  ScriptIntegrityHashMismatchJournalEntry,
} from "./workflow.js";

const canonicalIdentity = (identity: string): string => {
  if (!/^[0-9a-f]{64}$/u.test(identity))
    throw new Error("scriptIntegrityHashMismatch journal identity changed");
  return identity;
};

/** Crash-durable, fsynced, atomic per-thread production journal. */
export const createScriptIntegrityHashMismatchDirectoryJournal = (
  directory: string,
): ScriptIntegrityHashMismatchJournal => {
  const file = (identity: string) =>
    path.join(directory, `${canonicalIdentity(identity)}.json`);
  return Object.freeze({
    load: async (identity: string) => {
      try {
        return JSON.parse(
          await readFile(file(identity), "utf8"),
        ) as readonly ScriptIntegrityHashMismatchJournalEntry[];
      } catch (error) {
        if ((error as NodeJS.ErrnoException).code === "ENOENT") return [];
        throw error;
      }
    },
    append: async (entry: ScriptIntegrityHashMismatchJournalEntry) => {
      const target = file(entry.identity);
      const current = await (async () => {
        try {
          return JSON.parse(
            await readFile(target, "utf8"),
          ) as ScriptIntegrityHashMismatchJournalEntry[];
        } catch (error) {
          if ((error as NodeJS.ErrnoException).code === "ENOENT") return [];
          throw error;
        }
      })();
      if (entry.sequence !== current.length)
        throw new Error("scriptIntegrityHashMismatch journal sequence changed");
      const temporary = `${target}.${process.pid.toString()}.tmp`;
      const handle = await open(temporary, "wx", 0o600);
      try {
        await handle.writeFile(
          `${JSON.stringify([...current, entry])}\n`,
          "utf8",
        );
        await handle.sync();
      } finally {
        await handle.close();
      }
      await rename(temporary, target);
      const directoryHandle = await open(directory, "r");
      try {
        await directoryHandle.sync();
      } finally {
        await directoryHandle.close();
      }
    },
  });
};
