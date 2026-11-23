/**
 * A checked ledger as a ledger store `Ledger`, for a family whose replay
 * reads a `Ledger` while its caller still hands it a `CheckedLedger`.
 */
import type { CheckedLedger } from "./ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
} from "./ledger-store/index.js";

const converted = new WeakMap<CheckedLedger, Promise<Ledger>>();

/**
 * `checked` admitted to a memory store of its own: the same root and
 * entries. The store is never retained against, so the ledger stays readable
 * while anything holds it. One `checked` converts once: every family reading
 * a block from it gets the same `Ledger`, so they share the block's one walk
 * (`event-ledger.ledger.ts`).
 */
export const checkedLedgerAsLedger = (
  checked: CheckedLedger,
): Promise<Ledger> => {
  const known = converted.get(checked);
  if (known !== undefined) return known;
  const ledger = createLedgerStore(createMemoryLedgerStorage()).admit(
    checked.entries,
    checked.utxosRoot,
  );
  converted.set(checked, ledger);
  return ledger;
};
