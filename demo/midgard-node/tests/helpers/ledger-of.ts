/**
 * A replay parent for tests: the ledger of a set of entries, built in a
 * fresh memory ledger store, or retained by a store that has lost their
 * outputs.
 */
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
} from "@al-ft/midgard-fault-proofs/ledger-store";

import type * as LedgerRows from "../../src/database/utils/ledger.js";

const inserts = (entries: readonly LedgerRows.MinimalEntry[]) =>
  entries.map(({ outref, output }) => ({
    type: "insert" as const,
    outRef: outref,
    output,
  }));

/** The ledger holding exactly `entries`. */
export const ledgerOf = async (
  entries: readonly LedgerRows.MinimalEntry[],
): Promise<Ledger> =>
  (
    await createLedgerStore(createMemoryLedgerStorage()).at(EMPTY_LEDGER_ROOT)
  ).apply(inserts(entries));

/**
 * The ledger of `entries`, retained by a store whose output reader has none
 * of their outputs: its trie holds every out-ref, and reading a held one
 * fails with `LedgerOutputMissing`.
 */
export const ledgerWithoutOutputs = async (
  entries: readonly LedgerRows.MinimalEntry[],
): Promise<Ledger> => {
  const storage = createMemoryLedgerStorage();
  const store = createLedgerStore({
    ...storage,
    readOutputs: async (outRefs) => outRefs.map(() => undefined),
  });
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(inserts(entries));
  await store.persist(ledger);
  // Read back from the store, not from `ledger`'s in-memory outputs.
  return store.at(ledger.root);
};
