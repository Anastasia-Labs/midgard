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

/**
 * `checked` admitted to a memory store of its own: the same root and
 * entries. The store is never retained against, so the ledger stays readable
 * while anything holds it. Every reader of a block from the one `Ledger`
 * shares the block's one walk (`event-ledger.ledger.ts`), so a caller
 * converts once per block and hands the same `Ledger` to every read.
 */
export const checkedLedgerAsLedger = async (
  checked: CheckedLedger,
): Promise<Ledger> =>
  await createLedgerStore(createMemoryLedgerStorage()).admit(
    checked.entries,
    checked.utxosRoot,
  );
