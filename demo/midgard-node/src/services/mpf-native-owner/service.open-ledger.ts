/**
 * The node's MPF LevelDB, opened once, with the one ledger store over it.
 * The native owner reads the database through `level` and writes nodes and
 * the `__root__` marker only through `store`, so every node it writes is
 * counted and its root retained in the same atomic write.
 */
import {
  createLedgerStore,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { Level } from "level";

import { createLevelLedgerStorage } from "../../mpf/ledger-store-level.js";
import type { MpfStoredValue } from "../../mpf/types.js";
import type { StoredValue } from "./service.normalize-owner-options.js";

export type NativeOwnerLedger = {
  readonly level: Level<string, StoredValue>;
  readonly store: LedgerStore;
};

/**
 * Opens the LevelDB at `path` and builds the ledger store over it. Nothing
 * on the node reads ledger outputs through this store, and LevelDB holds
 * none, so an output read is refused.
 */
export const openNativeOwnerLedger = async (
  path: string,
): Promise<NativeOwnerLedger> => {
  const level = new Level<string, StoredValue>(path, { valueEncoding: "json" });
  await level.open();
  const storage = createLevelLedgerStorage({
    // The same handle: the adapter types values as the MPF store does.
    level: level as unknown as Level<string, MpfStoredValue>,
    readOutputs: () =>
      Promise.reject(
        new Error("The native owner's ledger store reads no outputs"),
      ),
  });
  return { level, store: createLedgerStore(storage) };
};

/**
 * Writes genesis: the ledger of `entries`, which must have `expectedRoot`
 * (`admit` refuses any other), with its counts, its retained root and the
 * `__root__` marker in one write, so a crash leaves the store empty or
 * holding all of genesis.
 */
export const writeGenesisLedger = async (
  store: LedgerStore,
  entries: Parameters<LedgerStore["admit"]>[0],
  expectedRoot: string,
): Promise<void> =>
  await store.persist(await store.admit(entries, expectedRoot), {
    current: true,
  });
