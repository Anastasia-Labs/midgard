/**
 * The schema of the watcher's fault-proof journals: one SQLite table per
 * journal, one row per record. Every table declares its retention class
 * (plan §5.1, §11) on the line above its `CREATE TABLE`, so the follower's
 * schema lint checks it like any role schema.
 */

/** The journals, each a table of its own. */
export const WATCHER_JOURNALS = [
  "fault_decisions",
  "fault_proof_queue",
  "fault_proof_objectives",
] as const;

export type WatcherJournalName = (typeof WATCHER_JOURNALS)[number];

export const WATCHER_JOURNAL_TABLES: Readonly<
  Record<WatcherJournalName, string>
> = Object.freeze({
  fault_decisions: "watcher_fault_decisions",
  fault_proof_queue: "watcher_fault_proof_queue",
  fault_proof_objectives: "watcher_fault_proof_objectives",
});

/** The row scope that ties a decision, a queued job and an objective to one
 * proof objective, so pruning the objective prunes all three. */
export const watcherObjectiveScope = (
  category: string,
  headerHash: string,
): string => `${category}:${headerHash}`;

/** How many revisions of each journal keep their chained digest row. */
export const WATCHER_JOURNAL_RETAINED_REVISIONS = 64;

export const WATCHER_JOURNAL_MIGRATION_NAMESPACE = "midgard-watcher-journals";

const rowTable = (table: string, retention: string): string => `
-- class: B; retention: ${retention}
CREATE TABLE IF NOT EXISTS ${table} (
  row_key TEXT PRIMARY KEY,
  scope TEXT NOT NULL,
  state TEXT NOT NULL,
  revision INTEGER NOT NULL,
  body TEXT NOT NULL,
  mac TEXT NOT NULL
) STRICT;
CREATE INDEX IF NOT EXISTS ${table}_scope ON ${table} (scope);
CREATE INDEX IF NOT EXISTS ${table}_state ON ${table} (state);
CREATE INDEX IF NOT EXISTS ${table}_revision ON ${table} (revision);
`;

const JOURNAL_TABLES_SQL = `
-- class: B; retention: one row per journal, kept while the journal directory exists
CREATE TABLE IF NOT EXISTS watcher_journal_heads (
  journal TEXT PRIMARY KEY,
  revision INTEGER NOT NULL,
  chain TEXT NOT NULL,
  live_rows INTEGER NOT NULL,
  accumulator TEXT NOT NULL,
  key_id TEXT NOT NULL,
  mac TEXT NOT NULL
) STRICT;

-- class: B; retention: the latest ${WATCHER_JOURNAL_RETAINED_REVISIONS} revisions of each journal; each commit prunes older ones
CREATE TABLE IF NOT EXISTS watcher_journal_revisions (
  journal TEXT NOT NULL,
  revision INTEGER NOT NULL,
  chain TEXT NOT NULL,
  delta TEXT NOT NULL,
  mac TEXT NOT NULL,
  PRIMARY KEY (journal, revision)
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
)}`;

/** The migration ledger, created before any migration. */
export const WATCHER_JOURNAL_LEDGER_SQL = `
-- class: A; retention: one row per applied migration, forever
CREATE TABLE IF NOT EXISTS watcher_journal_migrations (
  namespace TEXT NOT NULL,
  id TEXT NOT NULL,
  checksum TEXT NOT NULL,
  PRIMARY KEY (namespace, id)
) STRICT;
`;

/** The journals' migrations, in the follower's `MigrationSet` shape. */
export const WATCHER_JOURNAL_MIGRATIONS = Object.freeze({
  namespace: WATCHER_JOURNAL_MIGRATION_NAMESPACE,
  migrations: Object.freeze([
    Object.freeze({ id: "0000_ledger", sql: WATCHER_JOURNAL_LEDGER_SQL }),
    Object.freeze({ id: "0001_journal_tables", sql: JOURNAL_TABLES_SQL }),
  ]),
});
