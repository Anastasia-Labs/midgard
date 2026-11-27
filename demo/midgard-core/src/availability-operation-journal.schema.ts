import { mkdirSync } from "node:fs";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

/**
 * Opens the journal database at `path`, creating its schema (schema 3) if
 * absent and refusing a journal of any other schema. One challenge workflow
 * per (actor, deployment, header) is live while `retired_by` is null;
 * `release` keeps a foreign terminal's evidence until it is final.
 */
export const openAvailabilityJournalDatabase = (path: string): DatabaseSync => {
  if (!isAbsolute(path) || normalize(path) !== path) {
    throw new Error(
      "Availability operation journal requires a canonical absolute path",
    );
  }
  mkdirSync(dirname(path), { recursive: true, mode: 0o700 });
  const db = new DatabaseSync(path);
  try {
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
      CREATE TABLE IF NOT EXISTS availability_operation_workflows (
        actor TEXT NOT NULL, deployment TEXT NOT NULL,
        header_hash TEXT NOT NULL, retired_by TEXT, release TEXT,
        PRIMARY KEY(actor, deployment, header_hash)
      );
      INSERT OR IGNORE INTO availability_journal_metadata VALUES ('schema', '3');
    `);
    const schema = db
      .prepare("SELECT value FROM availability_journal_metadata WHERE key = ?")
      .get("schema")?.value;
    if (schema !== "3")
      throw new Error("Unsupported availability operation journal schema");
  } catch (error) {
    db.close();
    throw error;
  }
  return db;
};
