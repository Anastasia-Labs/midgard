/**
 * A replayed-ledger checkpoint kept in one JSON file:
 * `{ "headerHash": <28-byte hex>, "entries": LedgerJson }`. Writes replace the
 * file atomically. The file is untrusted: `ReplayedLedger.restore` admits it
 * only against the ConfirmedState root.
 */
import { open, readFile, rename } from "node:fs/promises";
import { dirname } from "node:path";

import { parseHex, parseJsonText } from "./json-file.js";
import { parseLedgerJson } from "./ledger-at.js";
import {
  type LedgerCheckpoint,
  type LedgerCheckpointStore,
} from "./replayed-ledger.js";

export const fileLedgerCheckpointStore = (
  path: string,
): LedgerCheckpointStore => ({
  read: async () => {
    let text: string;
    try {
      text = await readFile(path, "utf8");
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "ENOENT") return undefined;
      throw error;
    }
    const label = `ledger checkpoint ${path}`;
    const json = parseJsonText(text, () => `${label} is not JSON`);
    if (
      typeof json !== "object" ||
      json === null ||
      Object.keys(json).sort().join(",") !== "entries,headerHash"
    )
      throw new Error(`${label} must hold exactly headerHash and entries`);
    const { headerHash, entries } = json as Record<string, unknown>;
    return {
      headerHash: parseHex(headerHash, `${label} headerHash`, 28),
      entries: parseLedgerJson(entries, label),
    } satisfies LedgerCheckpoint;
  },
  write: async ({ headerHash, entries }) => {
    const temporary = `${path}.tmp`;
    const file = await open(temporary, "w", 0o600);
    try {
      await file.writeFile(
        JSON.stringify({
          headerHash,
          entries: entries.map(({ outRef, output, descriptor }) =>
            descriptor === undefined
              ? [outRef.toString("hex"), output.toString("hex")]
              : [
                  outRef.toString("hex"),
                  output.toString("hex"),
                  descriptor.toString("hex"),
                ],
          ),
        }),
      );
      await file.sync();
    } finally {
      await file.close();
    }
    await rename(temporary, path);
    // A retention floor may move only after the rename survives a crash.
    const directory = await open(dirname(path), "r");
    try {
      await directory.sync();
    } finally {
      await directory.close();
    }
  },
});
