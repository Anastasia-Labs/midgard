/**
 * The schema of the watcher's fault-proof journals: one SQLite table per
 * journal, one row per record. Every table declares its retention class
 * on the line above its `CREATE TABLE`, so the follower's
 * schema lint checks it like any role schema.
 */

/** The journals, each a table of its own. */
export const WATCHER_JOURNALS = [
  "fault_decisions",
  "fault_proof_queue",
  "fault_proof_objectives",
  "unproven_fault_merges",
] as const;

export type WatcherJournalName = (typeof WATCHER_JOURNALS)[number];

export const WATCHER_JOURNAL_TABLES: Readonly<
  Record<WatcherJournalName, string>
> = Object.freeze({
  fault_decisions: "watcher_fault_decisions",
  fault_proof_queue: "watcher_fault_proof_queue",
  fault_proof_objectives: "watcher_fault_proof_objectives",
  unproven_fault_merges: "watcher_unproven_fault_merges",
});

/** The row scope that ties a decision, a queued job and an objective to one
 * proof objective, so pruning the objective prunes all three. */
export const watcherObjectiveScope = (
  category: string,
  headerHash: string,
): string => `${category}:${headerHash}`;

const rowTable = (table: string, retention: string): string => `
-- class: B; retention: ${retention}
CREATE TABLE ${table} (
  row_key TEXT PRIMARY KEY,
  scope TEXT NOT NULL,
  state TEXT NOT NULL,
  revision INTEGER NOT NULL,
  body TEXT NOT NULL,
  mac TEXT NOT NULL
) STRICT;
CREATE INDEX ${table}_scope ON ${table} (scope);
CREATE INDEX ${table}_state ON ${table} (state);
CREATE INDEX ${table}_revision ON ${table} (revision);
`;

/**
 * The journals' tables, created whole on an empty file. Each journal's head
 * keeps only its revision counter and the key its rows are written under:
 * the per-row MAC catches a row the disk corrupted, and whole-row
 * deletions, replays and reorders come from hand edits or restores, which
 * the watcher does not defend against. `watcher_unproven_fault_merges` holds
 * the detected faults whose header merged unproven: the durable record
 * behind the `unproven_fault_merged` alert (watcher-unproven-fault-merges.ts).
 */
export const WATCHER_JOURNAL_SCHEMA_SQL = `
-- class: B; retention: one row per journal, kept while the journal directory exists
CREATE TABLE watcher_journal_heads (
  journal TEXT PRIMARY KEY,
  revision INTEGER NOT NULL,
  key_id TEXT NOT NULL
) STRICT;
${rowTable(
  WATCHER_JOURNAL_TABLES.fault_decisions,
  "one row per fault_detected decision, pruned with its objective once that objective's completion is verified at least k deep",
)}${rowTable(
  WATCHER_JOURNAL_TABLES.fault_proof_queue,
  "one row per scheduled job identity; a newer identity of the same objective replaces it, and the row is pruned with its objective",
)}${rowTable(
  WATCHER_JOURNAL_TABLES.fault_proof_objectives,
  "one row per proof objective; a completion verified at least k deep marks it, and the next start prunes it",
)}${rowTable(
  WATCHER_JOURNAL_TABLES.unproven_fault_merges,
  "one row per detected fault whose header merged unproven, at most 1024; the oldest goes first",
)}
-- class: A; retention: one row, forever: the checksum of the schema text that created the file
CREATE TABLE watcher_journal_schema (
  checksum TEXT NOT NULL
) STRICT;
`;
