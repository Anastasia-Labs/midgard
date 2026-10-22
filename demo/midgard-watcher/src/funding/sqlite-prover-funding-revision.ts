import type { DatabaseSync } from "node:sqlite";

/**
 * The funding revision: a counter every funding transaction bumps, on any
 * connection. While it stands, an audit of the reservation tables stands.
 * Workflow journal appends leave it alone, so they never force a fresh audit.
 */
export const openProverFundingRevision = (
  database: DatabaseSync,
): Readonly<{ read: () => string; bump: () => void }> => {
  database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_revision_v1 (
      singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
      revision INTEGER NOT NULL CHECK (revision >= 0)
    ) STRICT;
    INSERT OR IGNORE INTO watcher_prover_funding_revision_v1(singleton, revision)
      VALUES (1, 0);
  `);
  const select = database.prepare(
    "SELECT revision FROM watcher_prover_funding_revision_v1 WHERE singleton = 1",
  );
  const bump = database.prepare(
    "UPDATE watcher_prover_funding_revision_v1 SET revision = revision + 1 WHERE singleton = 1",
  );
  return Object.freeze({
    read: () =>
      String((select.get() as { revision: unknown } | undefined)?.revision),
    bump: () => {
      bump.run();
    },
  });
};
