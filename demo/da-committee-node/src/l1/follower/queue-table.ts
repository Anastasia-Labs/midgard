import type {
  DialectName,
  MigrationSet,
  RetentionPins,
  TemporalRowPin,
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

/**
 * The committee's retention pin tables (plan §11; written only through
 * `retention-pins.ts`): the L1 history a committee record will read again
 * stays stored past the follower's k-deep pruning while the record exists.
 * They are the committee's own records (class B: a rewind or a reset never
 * touches them), in the follower's database, so the prune statement sees
 * them.
 */
export const COMMITTEE_PINNED_BLOCKS_TABLE = "committee_l1_pinned_blocks";
export const COMMITTEE_PINNED_TXS_TABLE = "committee_l1_pinned_txs";
export const COMMITTEE_PINNED_HEADERS_TABLE = "committee_l1_pinned_headers";

export const retentionPinsMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: B; retention: the committee deletes a holder's row once no record it holds names the slot
CREATE TABLE ${COMMITTEE_PINNED_BLOCKS_TABLE} (
  slot ${int8} NOT NULL,
  holder text NOT NULL,
  PRIMARY KEY (slot, holder)
);
-- class: B; retention: the committee deletes a holder's row once no record it holds names the transaction
CREATE TABLE ${COMMITTEE_PINNED_TXS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  holder text NOT NULL,
  PRIMARY KEY (tx_hash, holder)
);
-- class: B; retention: the committee deletes a holder's row once the header is retired or deleted
CREATE TABLE ${COMMITTEE_PINNED_HEADERS_TABLE} (
  header_hash text NOT NULL,
  holder text NOT NULL,
  PRIMARY KEY (header_hash, holder)
);
`;
};

/** The follower's block and tx pins the committee registers. */
export const COMMITTEE_RETENTION_PINS: RetentionPins = {
  blocks: [
    { table: COMMITTEE_PINNED_BLOCKS_TABLE, column: "slot" },
    // A kept queue row keeps the blocks that created and spent it.
    { table: COMMITTEE_QUEUE_TABLE, column: "created_slot" },
    { table: COMMITTEE_QUEUE_TABLE, column: "spent_slot" },
  ],
  txs: [{ table: COMMITTEE_PINNED_TXS_TABLE, column: "tx_hash" }],
};

/** The queue table's row pin: a pinned header keeps its rows. */
export const COMMITTEE_QUEUE_ROW_PINS: readonly TemporalRowPin[] = [
  {
    column: "header_hash",
    table: COMMITTEE_PINNED_HEADERS_TABLE,
    tableColumn: "header_hash",
  },
];

export const COMMITTEE_QUEUE_TABLE_SPEC: TemporalTableSpec = {
  name: COMMITTEE_QUEUE_TABLE,
  shape: "versioned",
  startColumn: "created_slot",
  endColumn: "spent_slot",
  retention: { kind: "closed_k_deep" },
  // A pinned header keeps its rows, and through their slots their blocks.
  pinnedBy: COMMITTEE_QUEUE_ROW_PINS,
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
 * The committee's follower migrations. Production runs them on Postgres, in
 * the committee store's database; the SQLite dialect exists for the fork
 * simulator.
 */
export const committeeMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: COMMITTEE_MIGRATION_NAMESPACE,
  migrations: [
    { id: "0001_committee_queue_outputs", sql: queueTableSql(dialect) },
    {
      id: "0002_committee_retention_pins",
      sql: retentionPinsMigrationSql(dialect),
    },
  ],
});
