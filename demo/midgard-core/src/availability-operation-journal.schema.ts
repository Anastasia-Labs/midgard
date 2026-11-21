import { mkdirSync } from "node:fs";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { journalTransaction } from "#availability-operation-journal-storage";

export type AvailabilityJournalOpenOptions = Readonly<{
  /** Told once when a halt row left by an older release is cleared. */
  onLegacyHaltCleared?: (reason: string) => void;
}>;

/**
 * One challenge workflow per (actor, deployment, header), live while
 * `retired_by` is null. `release` keeps a foreign terminal's evidence until it
 * is final. Schema 3; schema-1 (one workflow per actor) and schema-2 (no
 * retirement) journals are migrated in place on open.
 */
const WORKFLOW_COLUMNS = `actor TEXT NOT NULL, deployment TEXT NOT NULL,
  header_hash TEXT NOT NULL, retired_by TEXT, release TEXT,
  PRIMARY KEY(actor, deployment, header_hash)`;

/**
 * Opens the journal database at `path`, creating or migrating its schema and
 * clearing a halt row an older release left.
 */
export const openAvailabilityJournalDatabase = (
  path: string,
  options: AvailabilityJournalOpenOptions,
): DatabaseSync => {
  if (!isAbsolute(path) || normalize(path) !== path) {
    throw new Error(
      "Availability operation journal requires a canonical absolute path",
    );
  }
  mkdirSync(dirname(path), { recursive: true, mode: 0o700 });
  const db = new DatabaseSync(path);
  db.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA busy_timeout = 5000;
    CREATE TABLE IF NOT EXISTS availability_journal_metadata (
      key TEXT PRIMARY KEY, value TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS availability_operation_leases (
      scope TEXT PRIMARY KEY, owner TEXT NOT NULL, generation INTEGER NOT NULL,
      expires_at INTEGER NOT NULL
    );
    CREATE TABLE IF NOT EXISTS availability_operation_intents (
      id TEXT PRIMARY KEY, deployment TEXT NOT NULL, actor TEXT NOT NULL,
      record TEXT NOT NULL, state TEXT NOT NULL, tx_hash TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS availability_operation_intents_tx_hash
      ON availability_operation_intents(tx_hash);
    CREATE TABLE IF NOT EXISTS availability_operation_resources (
      resource TEXT NOT NULL, intent_id TEXT NOT NULL, kind TEXT NOT NULL,
      actor TEXT NOT NULL, PRIMARY KEY(resource, intent_id)
    );
    CREATE TABLE IF NOT EXISTS availability_operation_dependencies (
      parent_tx_hash TEXT NOT NULL, child_id TEXT NOT NULL,
      PRIMARY KEY(parent_tx_hash, child_id)
    );
  `);
  const transaction = <T>(run: () => T): T => journalTransaction(db, run);
  let legacyHalt: string | undefined;
  try {
    legacyHalt = transaction(() => {
      const meta = (key: string) =>
        db
          .prepare(
            "SELECT value FROM availability_journal_metadata WHERE key = ?",
          )
          .get(key)?.value;
      const schema = meta("schema");
      if (schema !== undefined && !["1", "2", "3"].includes(String(schema)))
        throw new Error("Unsupported availability operation journal schema");
      if (schema === "1") {
        // Schema 1 keyed workflows on the actor alone. Its rows map 1:1 onto
        // the per-header key, so the migration keeps every live workflow.
        db.exec(`
          CREATE TABLE availability_operation_workflows_v2 (${WORKFLOW_COLUMNS});
          INSERT INTO availability_operation_workflows_v2 (actor, deployment, header_hash)
            SELECT actor, deployment, header_hash FROM availability_operation_workflows;
          DROP TABLE availability_operation_workflows;
          ALTER TABLE availability_operation_workflows_v2 RENAME TO availability_operation_workflows;
        `);
      }
      db.exec(
        `CREATE TABLE IF NOT EXISTS availability_operation_workflows (${WORKFLOW_COLUMNS})`,
      );
      // Schema 2 deleted a terminal step's workflow row at confirmation.
      for (const column of ["retired_by", "release"])
        if (
          !db
            .prepare(
              "SELECT 1 FROM pragma_table_info('availability_operation_workflows') WHERE name = ?",
            )
            .get(column)
        )
          db.exec(
            `ALTER TABLE availability_operation_workflows ADD COLUMN ${column} TEXT`,
          );
      db.exec(
        "INSERT INTO availability_journal_metadata VALUES ('schema', '3') ON CONFLICT(key) DO UPDATE SET value = '3'",
      );
      // Older releases latched all work behind this row on a lost finalized
      // observation; reconciliation now rewinds and rebroadcasts instead.
      const halt = meta("halt");
      db.exec("DELETE FROM availability_journal_metadata WHERE key = 'halt'");
      return halt === undefined ? undefined : String(halt);
    });
  } catch (error) {
    db.close();
    throw error;
  }
  if (legacyHalt !== undefined)
    (
      options.onLegacyHaltCleared ??
      ((reason) =>
        process.stderr.write(
          `${JSON.stringify({ event: "availability_journal_legacy_halt_cleared", reason })}\n`,
        ))
    )(legacyHalt);
  return db;
};
