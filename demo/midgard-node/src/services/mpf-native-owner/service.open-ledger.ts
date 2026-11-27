/**
 * The node's MPF LevelDB, opened once, with the one ledger store over it.
 * The native owner reads the database through `level` and writes nodes and
 * the `__root__` marker only through `store`, so every node it writes is
 * counted and its root retained in the same atomic write.
 */
import {
  createLedgerStore,
  type LedgerStore,
  type OutRefHex,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { Level } from "level";

import { createLevelLedgerStorage } from "../../mpf/ledger-store-level.js";
import type { MpfStoredValue } from "../../mpf/types.js";
import type { StoredValue } from "./service.normalize-owner-options.js";

export type NativeOwnerLedger = {
  readonly level: Level<string, StoredValue>;
  readonly store: LedgerStore;
};

/** Output bytes by out-ref, in order; `undefined` for an out-ref it lacks. */
export type LedgerOutputReader = (
  outRefs: readonly OutRefHex[],
) => Promise<(Buffer | undefined)[]>;

/**
 * Opens the LevelDB at `path` and builds the ledger store over it. LevelDB
 * holds trie nodes only; the store reads outputs through `readOutputs`.
 */
export const openNativeOwnerLedger = async (
  path: string,
  readOutputs: LedgerOutputReader,
): Promise<NativeOwnerLedger> => {
  const level = new Level<string, StoredValue>(path, { valueEncoding: "json" });
  await level.open();
  const storage = createLevelLedgerStorage({
    // The same handle: the adapter types values as the MPF store does.
    level: level as unknown as Level<string, MpfStoredValue>,
    readOutputs,
  });
  return { level, store: createLedgerStore(storage) };
};
