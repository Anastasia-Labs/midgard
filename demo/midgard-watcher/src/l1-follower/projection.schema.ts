import type {
  DialectName,
  MigrationSet,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

import {
  CHECKPOINT_TEMPORAL_TABLES,
  checkpointMigrationSql,
} from "./checkpoints.js";
import {
  DA_ATTESTATIONS_TEMPORAL_TABLE,
  daAttestationsMigrationSql,
} from "./projection.da-attestations.js";
import {
  DEPARTED_HEADERS_TEMPORAL_TABLE,
  departedHeadersMigrationSql,
} from "./projection.departed-headers.js";
import {
  FOLLOWED_UNITS_TEMPORAL_TABLES,
  followedUnitsMigrationSql,
} from "./projection.followed-units.js";
import {
  PROTOCOL_INIT_FAULTS_TEMPORAL_TABLE,
  protocolInitFaultsMigrationSql,
} from "./projection.protocol-init.js";
import {
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
} from "./tables.js";

/** The watcher projection's D-t tables and migrations (see projection.ts). */

export const WATCHER_TEMPORAL_TABLES: readonly TemporalTableSpec[] = [
  {
    name: WATCHER_QUEUE_OUTPUTS_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  {
    name: WATCHER_QUEUE_UNIT_HISTORY_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  DA_ATTESTATIONS_TEMPORAL_TABLE,
  PROTOCOL_INIT_FAULTS_TEMPORAL_TABLE,
  DEPARTED_HEADERS_TEMPORAL_TABLE,
  ...FOLLOWED_UNITS_TEMPORAL_TABLES,
  ...CHECKPOINT_TEMPORAL_TABLES,
];

const migrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: current rows forever; closed rows once to_slot is k deep
CREATE TABLE ${WATCHER_QUEUE_OUTPUTS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  kind text NOT NULL CHECK (kind IN ('root', 'node', 'lock', 'malformed')),
  header_hash ${bytes},
  next_header_hash ${bytes},
  header_cbor ${bytes},
  state_queue_node_cbor ${bytes},
  datum_cbor ${bytes},
  malformed text,
  anchor_tx_hash ${bytes} NOT NULL,
  anchor_slot ${int8} NOT NULL,
  anchor_block_hash ${bytes} NOT NULL,
  anchor_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_from ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_to ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (to_slot);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_header ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (header_hash);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_anchor ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (anchor_tx_hash);
`;
};

/**
 * Per state-queue node header, every canonical tx that created or spent one
 * of its node outputs: the unit history the old Kupmios reads return for
 * the node unit. A header's rows close when its node leaves the queue.
 */
const unitHistoryMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: rows of a header in the queue forever; closed rows once to_slot (the header's removal) is k deep
CREATE TABLE ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (
  header_hash ${bytes} NOT NULL,
  tx_hash ${bytes} NOT NULL,
  block_hash ${bytes} NOT NULL,
  block_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (header_hash, tx_hash)
);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_from ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (from_slot);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_to ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (to_slot);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_tx ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (tx_hash);
`;
};

export const watcherMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: "watcher",
  migrations: [
    { id: "0001_watcher_queue_outputs", sql: migrationSql(dialect) },
    {
      id: "0002_watcher_queue_unit_history",
      sql: unitHistoryMigrationSql(dialect),
    },
    {
      id: "0003_watcher_queue_checkpoints",
      sql: checkpointMigrationSql(dialect),
    },
    {
      id: "0004_watcher_da_attestations",
      sql: daAttestationsMigrationSql(dialect),
    },
    {
      id: "0005_watcher_protocol_init_faults",
      sql: protocolInitFaultsMigrationSql(dialect),
    },
    {
      id: "0006_watcher_departed_headers",
      sql: departedHeadersMigrationSql(dialect),
    },
    {
      id: "0007_watcher_followed_units",
      sql: followedUnitsMigrationSql(dialect),
    },
  ],
});
