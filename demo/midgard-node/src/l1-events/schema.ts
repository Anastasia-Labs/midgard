/**
 * The node event projection's D-t tables (plan §5.5 P2, §7.2, §11). The
 * follower's registry generates their rewind and pruning; the projection
 * has no rollback code of its own. The never-reuse key set is the
 * follower's class A `l1_event_keys`, which the derivation also writes.
 */
import type {
  DialectName,
  MigrationSet,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

export const EVENTS_TABLE = "node_l1_events";
export const RETIREMENTS_TABLE = "node_l1_event_retirements";
export const REFUSALS_TABLE = "node_l1_event_refusals";

export const EVENT_TABLES: readonly TemporalTableSpec[] = [
  {
    name: EVENTS_TABLE,
    shape: "versioned",
    startColumn: "admitted_slot",
    endColumn: "retired_slot",
    retention: { kind: "closed_k_deep" },
  },
  {
    name: RETIREMENTS_TABLE,
    shape: "append_only",
    slotColumn: "retired_slot",
    retention: { kind: "created_k_deep" },
  },
  {
    name: REFUSALS_TABLE,
    shape: "append_only",
    slotColumn: "slot",
    retention: { kind: "created_k_deep" },
  },
];

const eventSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: live events forever; a retired event once retired_slot is k deep
CREATE TABLE ${EVENTS_TABLE} (
  kind text NOT NULL,
  event_key ${bytes} NOT NULL,
  event_id ${bytes} NOT NULL,
  inclusion_time ${int8} NOT NULL,
  facts_cbor ${bytes} NOT NULL,
  payload_cbor ${bytes} NOT NULL,
  original_assets_cbor ${bytes} NOT NULL,
  admission_tx_hash ${bytes} NOT NULL,
  admission_output_index integer NOT NULL,
  admission_tx_index integer NOT NULL,
  admitted_block_hash ${bytes} NOT NULL,
  admitted_height ${int8} NOT NULL,
  admitted_slot ${int8} NOT NULL,
  retired_slot ${int8},
  PRIMARY KEY (kind, event_key)
);
CREATE INDEX ${EVENTS_TABLE}_admitted ON ${EVENTS_TABLE} (admitted_slot);
CREATE INDEX ${EVENTS_TABLE}_retired ON ${EVENTS_TABLE} (retired_slot);
CREATE INDEX ${EVENTS_TABLE}_due ON ${EVENTS_TABLE} (kind, inclusion_time);

-- class: D-t; retention: once retired_slot is k deep
CREATE TABLE ${RETIREMENTS_TABLE} (
  kind text NOT NULL,
  event_key ${bytes} NOT NULL,
  retired_slot ${int8} NOT NULL,
  retirement_tx_hash ${bytes} NOT NULL,
  retirement_tx_index integer NOT NULL,
  retired_block_hash ${bytes} NOT NULL,
  retired_height ${int8} NOT NULL,
  order_tx_hash ${bytes} NOT NULL,
  order_output_index integer NOT NULL,
  reason text,
  observer_redeemer_index integer,
  witness_cbor ${bytes},
  PRIMARY KEY (kind, event_key)
);
CREATE INDEX ${RETIREMENTS_TABLE}_slot ON ${RETIREMENTS_TABLE} (retired_slot);

-- class: D-t; retention: once slot is k deep
CREATE TABLE ${REFUSALS_TABLE} (
  slot ${int8} NOT NULL,
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  kind text NOT NULL,
  event_key ${bytes} NOT NULL,
  reason text NOT NULL,
  detail text NOT NULL,
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX ${REFUSALS_TABLE}_slot ON ${REFUSALS_TABLE} (slot);
`;
};

export const eventMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: "node_l1_events",
  migrations: [{ id: "0001_node_l1_events", sql: eventSql(dialect) }],
});
