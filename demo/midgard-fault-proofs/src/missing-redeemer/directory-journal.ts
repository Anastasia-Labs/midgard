import { createHash } from "node:crypto";
import { mkdir, open, readFile, unlink } from "node:fs/promises";
import { join, resolve } from "node:path";

import type {
  MissingRedeemerDurableState,
  MissingRedeemerJournal,
} from "./workflow.js";

const fileName = (identity: string): string =>
  `${createHash("sha256").update(identity).digest("hex")}.jsonl`;

const parse = (contents: string): readonly MissingRedeemerDurableState[] =>
  contents
    .split("\n")
    .filter((line) => line !== "")
    .map((line) => JSON.parse(line) as MissingRedeemerDurableState);

/**
 * Family-owned append-only journal. The exclusive sidecar lock makes the
 * expected-length compare-and-append atomic across process restarts and
 * concurrent workers; every append is fsynced before the lock is released.
 */
export const createMissingRedeemerDirectoryJournal = async (
  directory: string,
): Promise<MissingRedeemerJournal> => {
  const root = resolve(directory);
  await mkdir(root, { recursive: true });
  const load = async (
    identity: string,
  ): Promise<readonly MissingRedeemerDurableState[]> => {
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
    append: async (identity, expectedLength, state) => {
      const path = join(root, fileName(identity));
      const lockPath = `${path}.lock`;
      let lock;
      try {
        lock = await open(lockPath, "wx", 0o600);
        const current = await load(identity);
        if (current.length !== expectedLength)
          throw new Error(
            "missingRedeemer: durable journal compare-and-append conflict",
          );
        const journal = await open(path, "a", 0o600);
        try {
          await journal.write(`${JSON.stringify(state)}\n`);
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
