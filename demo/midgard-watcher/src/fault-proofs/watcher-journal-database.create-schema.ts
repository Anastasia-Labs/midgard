import type { DatabaseSync, StatementSync } from "node:sqlite";

import { sha256Hex } from "./watcher-journal-database.codec.js";
import {
  WatcherJournalConfigurationError,
  WatcherJournalIntegrityError,
} from "./watcher-journal-database.types.js";
import { WATCHER_JOURNAL_SCHEMA_SQL } from "./watcher-journal-schema.js";

const SCHEMA_CHECKSUM = sha256Hex(WATCHER_JOURNAL_SCHEMA_SQL);

/** Creates the journals' schema in an empty file and binds it to
 * `hubOraclePolicyId`. A file created by another schema text is an integrity
 * failure; a file bound to another deployment is a configuration failure so
 * startup refuses before binding without latching corruption. */
export const ensureWatcherJournalSchema = (
  database: DatabaseSync,
  prepare: (sql: string) => StatementSync,
  inTransaction: <T>(begin: string, run: () => T) => T,
  hubOraclePolicyId: string,
): void =>
  inTransaction("BEGIN IMMEDIATE", () => {
    const tables = prepare(
      "SELECT name FROM sqlite_master WHERE type = 'table'",
    ).all() as { name: string }[];
    if (tables.length === 0) {
      database.exec(WATCHER_JOURNAL_SCHEMA_SQL);
      prepare(
        "INSERT INTO watcher_journal_schema (checksum, hub_oracle_policy_id) VALUES (?, ?)",
      ).run(SCHEMA_CHECKSUM, hubOraclePolicyId);
      return;
    }
    const recorded = tables.some(
      ({ name }) => name === "watcher_journal_schema",
    )
      ? (prepare(
          "SELECT checksum, hub_oracle_policy_id FROM watcher_journal_schema",
        ).get() as
          | { checksum: string; hub_oracle_policy_id: string }
          | undefined)
      : undefined;
    if (recorded?.checksum !== SCHEMA_CHECKSUM)
      throw new WatcherJournalIntegrityError(
        "schema",
        "the file was created by a different schema; recreate it (delete the journals file and start again)",
      );
    if (recorded.hub_oracle_policy_id !== hubOraclePolicyId)
      throw new WatcherJournalConfigurationError(
        `the journals belong to deployment ${recorded.hub_oracle_policy_id}, not ${hubOraclePolicyId}`,
      );
  });
