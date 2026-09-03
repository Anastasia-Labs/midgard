import { createHash } from "node:crypto";
import { mkdir, open, readFile, unlink } from "node:fs/promises";
import { join, resolve } from "node:path";

import type {
  UnusedRedeemerJournal,
  UnusedRedeemerJournalEntry,
} from "./workflow-v1.js";

const fileName = (identity: string): string =>
  `${createHash("sha256").update(identity).digest("hex")}.jsonl`;

const parse = (contents: string): readonly UnusedRedeemerJournalEntry[] =>
  contents
    .split("\n")
    .filter((line) => line !== "")
    .map((line) => JSON.parse(line) as UnusedRedeemerJournalEntry);

/** Append-only, fsynced, compare-and-append workflow journal. */
export const createUnusedRedeemerDirectoryJournal = async (
  directory: string,
): Promise<UnusedRedeemerJournal> => {
  const root = resolve(directory);
  await mkdir(root, { recursive: true });
  const load = async (
    identity: string,
  ): Promise<readonly UnusedRedeemerJournalEntry[]> => {
    try {
      return parse(await readFile(join(root, fileName(identity)), "utf8"));
    } catch (cause) {
      if (
        typeof cause === "object" &&
        cause !== null &&
        "code" in cause &&
        cause.code === "ENOENT"
      )
        return [];
      throw cause;
    }
  };
  return Object.freeze({
    load,
    append: async (entry: UnusedRedeemerJournalEntry) => {
      const path = join(root, fileName(entry.identity));
      const lockPath = `${path}.lock`;
      let lock;
      try {
        lock = await open(lockPath, "wx", 0o600);
        const current = await load(entry.identity);
        if (current.length !== entry.sequence)
          throw new Error(
            "unusedRedeemer: durable journal compare-and-append conflict",
          );
        const journal = await open(path, "a", 0o600);
        try {
          await journal.write(`${JSON.stringify(entry)}\n`);
          await journal.sync();
        } finally {
          await journal.close();
        }
      } finally {
        if (lock !== undefined) {
          await lock.close();
          await unlink(lockPath).catch((cause: unknown) => {
            if (
              !(
                typeof cause === "object" &&
                cause !== null &&
                "code" in cause &&
                cause.code === "ENOENT"
              )
            )
              throw cause;
          });
        }
      }
    },
  });
};
