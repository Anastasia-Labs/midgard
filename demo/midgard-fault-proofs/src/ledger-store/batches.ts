/** The batches `admit` reads a snapshot in. */
import type { LedgerEntry } from "../ledger-at.js";

/** Snapshot entries one `admit` batch inserts and writes. */
const ADMIT_BATCH = 1024;

/** `entries` in batches of at most `ADMIT_BATCH`, the last one marked; a
 * stream with no entries is one empty last batch. */
export async function* snapshotBatches(
  entries: Iterable<LedgerEntry> | AsyncIterable<LedgerEntry>,
): AsyncIterable<{ readonly entries: LedgerEntry[]; readonly last: boolean }> {
  let batch: LedgerEntry[] = [];
  for await (const entry of entries) {
    if (batch.length === ADMIT_BATCH) {
      yield { entries: batch, last: false };
      batch = [];
    }
    batch.push(entry);
  }
  yield { entries: batch, last: true };
}
