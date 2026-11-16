import { HEADER_HASH } from "./fault-decision-journal.exact-record.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";
import type { WatcherJournalName } from "./watcher-journal-schema.js";

/**
 * Detected faults whose header merged into confirmed state unproven: the
 * invalid block is now part of confirmed state. No watcher action changes
 * that, and nothing acknowledges it, so each record stays (the oldest goes
 * once the table holds its cap) and raises the `unproven_fault_merged`
 * alert again after every restart.
 */
const JOURNAL = "unproven_fault_merges" satisfies WatcherJournalName;

export const MAX_UNPROVEN_FAULT_MERGES = 1_024;

export type WatcherUnprovenFaultMerge = Readonly<{
  headerHash: string;
  category: string;
  decisionDigest: string;
  mergeTransactionHash: string;
  /** When this watcher saw the merge at release finality. */
  observedAtMs: string;
}>;

export type WatcherUnprovenFaultMerges = Readonly<{
  /** Records the merge; false when the header was already recorded. */
  record(merge: WatcherUnprovenFaultMerge): boolean;
  list(): readonly WatcherUnprovenFaultMerge[];
  /** How many merges were ever recorded, pruned ones included. */
  recorded(): number;
}>;

const parse = (
  database: WatcherJournalDatabase,
  key: string,
  body: unknown,
): WatcherUnprovenFaultMerge => {
  const value = body as Partial<
    Record<keyof WatcherUnprovenFaultMerge, unknown>
  >;
  if (
    typeof body !== "object" ||
    body === null ||
    value.headerHash !== key ||
    !HEADER_HASH.test(key) ||
    typeof value.category !== "string" ||
    typeof value.decisionDigest !== "string" ||
    typeof value.mergeTransactionHash !== "string" ||
    typeof value.observedAtMs !== "string"
  )
    return database.refuse(JOURNAL, `row ${key} is not an unproven merge`);
  return Object.freeze({
    headerHash: value.headerHash,
    category: value.category,
    decisionDigest: value.decisionDigest,
    mergeTransactionHash: value.mergeTransactionHash,
    observedAtMs: value.observedAtMs,
  });
};

/** The unproven merges recorded in the watcher's journal database. */
export const watcherUnprovenFaultMerges = (
  database: () => WatcherJournalDatabase,
): WatcherUnprovenFaultMerges =>
  Object.freeze({
    record: (merge) => {
      if (!HEADER_HASH.test(merge.headerHash))
        throw new Error("unproven fault merge names an invalid header hash");
      return database().transaction((tx) => {
        if (tx.row(JOURNAL, merge.headerHash) !== undefined) return false;
        const rows = [...tx.rows(JOURNAL)].sort(
          (left, right) => left.revision - right.revision,
        );
        for (const oldest of rows.slice(
          0,
          Math.max(0, rows.length - MAX_UNPROVEN_FAULT_MERGES + 1),
        ))
          tx.delete(JOURNAL, oldest.key);
        tx.put(JOURNAL, {
          key: merge.headerHash,
          scope: merge.headerHash,
          state: "merged",
          body: merge,
        });
        return true;
      });
    },
    // Each record is the journal's one write, so its revision counts them.
    recorded: () => database().head(JOURNAL).revision,
    list: () => {
      const opened = database();
      return Object.freeze(
        [...opened.rows(JOURNAL)]
          .sort((left, right) => left.revision - right.revision)
          .map(({ key, body }) => parse(opened, key, body)),
      );
    },
  });
