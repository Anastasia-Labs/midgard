/**
 * A whole ledger as a map by hex outref, for tests that model or compare a
 * ledger in memory. The node keeps no such map: it reads and writes its
 * ledger by key.
 */
import * as Ledger from "../../src/database/utils/ledger.js";
import type { LandedBlockRow } from "../../src/landed-blocks/store.js";

/** A ledger by hex outref. */
export type LedgerMap = Map<string, Buffer>;

export const ledgerMap = (entries: readonly Ledger.MinimalEntry[]): LedgerMap =>
  new Map(
    entries.map((entry) => [
      Buffer.from(entry[Ledger.Columns.OUTREF]).toString("hex"),
      Buffer.from(entry[Ledger.Columns.OUTPUT]),
    ]),
  );

/** `ledger` with `row`'s delta applied, in place. */
export const applyDelta = (
  ledger: LedgerMap,
  row: Pick<LandedBlockRow, "spent" | "produced">,
): LedgerMap => {
  for (const outRef of row.spent) ledger.delete(outRef.toString("hex"));
  for (const entry of row.produced)
    ledger.set(entry.outref.toString("hex"), Buffer.from(entry.output));
  return ledger;
};
