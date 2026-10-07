import type {
  DialectName,
  MigrationSet,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

/**
 * The committee's D-t table on the L1 follower (plan §7.2, §7.3): one row per
 * state-queue output the follower has seen on the current chain, versioned
 * by the slot that created it and the slot that spent it. The landed queue at
 * any point P is the rows live at P, so the follower's generated rewind (drop
 * rows created after the intersection, reopen rows spent after it) is the
 * whole of the committee's rollback handling.
 */
export const COMMITTEE_QUEUE_TABLE = "committee_queue_outputs";

export const COMMITTEE_MIGRATION_NAMESPACE = "committee-l1";

export const COMMITTEE_QUEUE_TABLE_SPEC: TemporalTableSpec = {
  name: COMMITTEE_QUEUE_TABLE,
  shape: "versioned",
  startColumn: "created_slot",
  endColumn: "spent_slot",
  retention: { kind: "closed_k_deep" },
};

const queueTableSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: current rows forever; closed rows once spent_slot is k deep
CREATE TABLE ${COMMITTEE_QUEUE_TABLE} (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  kind text NOT NULL,
  asset_name text,
  node_key text,
  next_key text,
  header_hash text,
  da_status text,
  end_time_ms ${int8},
  problems text NOT NULL,
  datum ${bytes},
  created_slot ${int8} NOT NULL,
  created_height ${int8} NOT NULL,
  created_tx_index integer NOT NULL,
  spent_slot ${int8},
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX ${COMMITTEE_QUEUE_TABLE}_created ON ${COMMITTEE_QUEUE_TABLE} (created_slot);
CREATE INDEX ${COMMITTEE_QUEUE_TABLE}_spent ON ${COMMITTEE_QUEUE_TABLE} (spent_slot);
CREATE INDEX ${COMMITTEE_QUEUE_TABLE}_header ON ${COMMITTEE_QUEUE_TABLE} (header_hash);
CREATE INDEX ${COMMITTEE_QUEUE_TABLE}_live ON ${COMMITTEE_QUEUE_TABLE} (created_slot) WHERE spent_slot IS NULL;
`;
};

/**
 * The committee's follower migrations. Production runs them on Postgres only
 * (the committee's JSON store gets no follower tables, plan C2); the SQLite
 * dialect exists for the fork simulator.
 */
export const committeeMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: COMMITTEE_MIGRATION_NAMESPACE,
  migrations: [
    { id: "0001_committee_queue_outputs", sql: queueTableSql(dialect) },
  ],
});
