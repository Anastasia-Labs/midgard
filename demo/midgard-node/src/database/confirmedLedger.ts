import { Effect, Metric } from "effect";

import { clearTable } from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";

export const tableName = "confirmed_ledger";

/** Counts unbounded confirmed-ledger reads so the MPF growth gate can prove
 * the normal root+aggregate commit path remains independent of state size. */
export const fullScanCounter = Metric.counter(
  "confirmed_ledger_full_scan_total",
  {
    description: "Unbounded SELECTs over the full confirmed ledger",
    incremental: true,
  },
);

/**
 * Inserts multiple confirmed-ledger entries.
 */
export const insertMultiple = (entries: Ledger.Entry[]) =>
  Ledger.insertEntries(tableName, entries);

export const retrieve = Metric.increment(fullScanCounter).pipe(
  Effect.zipRight(Ledger.retrieveAllEntries(tableName)),
);

/**
 * Deletes confirmed-ledger entries for the supplied UTxO references.
 */
export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
