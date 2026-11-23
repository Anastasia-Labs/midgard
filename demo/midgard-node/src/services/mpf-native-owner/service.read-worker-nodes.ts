/**
 * The owner's side of a worker's `readNodes` request: trie nodes by hash
 * from the ledger LevelDB, which only the owner's thread has open. A worker
 * reads the commit base's ledger through it (`worker-ledger.ts`).
 */
import type { Level } from "level";

import { assertNativeMpfHashHex } from "./protocol.js";
import type {
  StoredNode,
  StoredValue,
} from "./service.normalize-owner-options.js";

/**
 * The nodes with `hashes`, in order; `undefined` for a hash the LevelDB does
 * not hold. Refuses anything but node hashes: the LevelDB also holds the
 * `__root__` marker and prefixed bookkeeping keys, which are not nodes.
 */
export const readWorkerNodes = async (
  db: Level<string, StoredValue>,
  hashes: unknown,
): Promise<(StoredNode | undefined)[]> => {
  const asked = hashes as string[];
  // Anything but an array of node hashes throws here: a number is not
  // iterable, and a string yields characters, which are not hashes.
  for (const hash of asked)
    assertNativeMpfHashHex(String(hash), "readNodes hash");
  if (asked.length === 0) return [];
  return (await db.getMany(asked)) as (StoredNode | undefined)[];
};
