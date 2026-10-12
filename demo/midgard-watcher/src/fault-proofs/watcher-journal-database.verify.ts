import type { WatcherJournalCodec } from "./watcher-journal-database.codec.js";
import { MODULUS, sameHex } from "./watcher-journal-database.codec.js";
import {
  type StoredHead,
  type StoredRevision,
  type StoredRow,
  WatcherJournalIntegrityError,
} from "./watcher-journal-database.types.js";
import {
  WATCHER_JOURNAL_RETAINED_REVISIONS,
  type WatcherJournalName,
} from "./watcher-journal-schema.js";

/**
 * The full check of one journal against its authenticated head: every row's
 * MAC, the live row count and the keyed accumulator, and the retained
 * revisions' order, MACs and chain. `admit` checks one row's MAC and throws
 * on a mismatch.
 */
export const verifyWatcherJournal = (input: {
  readonly journal: WatcherJournalName;
  readonly head: StoredHead;
  readonly rows: readonly StoredRow[];
  readonly revisions: readonly StoredRevision[];
  readonly codec: WatcherJournalCodec;
  readonly admit: (stored: StoredRow) => Readonly<{ revision: number }>;
}): void => {
  const { journal, head, codec } = input;
  const fail = (detail: string): never => {
    throw new WatcherJournalIntegrityError(journal, detail);
  };
  let accumulator = 0n;
  const rowsByKey = new Map<string, StoredRow>();
  for (const stored of input.rows) {
    if (input.admit(stored).revision > head.revision)
      fail(`row ${stored.row_key} is newer than the head`);
    accumulator = (accumulator + codec.element(stored.mac)) % MODULUS;
    rowsByKey.set(stored.row_key, stored);
  }
  if (input.rows.length !== head.liveRows || accumulator !== head.accumulator)
    fail("rows differ from the authenticated head");
  const expectedCount = Math.min(
    head.revision,
    WATCHER_JOURNAL_RETAINED_REVISIONS,
  );
  if (input.revisions.length !== expectedCount)
    fail("retained revision history is incomplete");
  let prior: string | undefined;
  input.revisions.forEach((stored, index) => {
    const revision = Number(stored.revision);
    if (
      revision !== head.revision - expectedCount + 1 + index ||
      !sameHex(
        stored.mac,
        codec.revisionMac(journal, revision, stored.chain, stored.delta),
      ) ||
      (prior !== undefined &&
        stored.chain !== codec.chainOf(journal, revision, prior, stored.delta))
    )
      fail(`revision ${revision} is out of order or altered`);
    prior = stored.chain;
    let delta: unknown;
    try {
      delta = JSON.parse(stored.delta);
    } catch {
      fail(`revision ${revision} delta is malformed`);
    }
    if (!Array.isArray(delta)) fail(`revision ${revision} delta is malformed`);
    for (const entry of delta as unknown[]) {
      const [key, writtenMac] = entry as [string, string | null];
      const current = rowsByKey.get(key);
      if (
        current !== undefined &&
        Number(current.revision) === revision &&
        current.mac !== writtenMac
      )
        fail(`row ${key} differs from revision ${revision}`);
    }
  });
  if (prior !== undefined && prior !== head.chain)
    fail("latest revision differs from the head");
};
