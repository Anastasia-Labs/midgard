/**
 * The queue-terminal projection's D-t table (plan §5.5 P8, §7.4, N4): one
 * row per state-queue header a landed tx took out of the queue, merged into
 * the confirmed state or removed. The follower's registry generates its
 * rewind: a rollback past the tx's block deletes the row, and the header is
 * live in the landed queue again. Final rows are the retention sweep's to
 * delete (`pruneFinalQueueTerminals`).
 */
import type {
  DialectName,
  MigrationSet,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

export const QUEUE_TERMINALS_TABLE = "node_l1_queue_terminals";

export const QUEUE_TERMINAL_TABLES: readonly TemporalTableSpec[] = [
  {
    name: QUEUE_TERMINALS_TABLE,
    shape: "append_only",
    slotColumn: "slot",
    retention: {
      kind: "owner",
      description:
        "the node's retention sweep deletes a final row once no DA payload or block journal names its header",
    },
  },
];

/**
 * `terminal_outcome`: `merged` when the tx's new root confirms the header,
 * `removed` otherwise (a correction or a fraud removal). One header can have
 * more than one row only if it is taken out, put back and taken out again
 * on chain; readers take the newest.
 */
const queueTerminalSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: the node's retention sweep deletes a final row once no DA payload or block journal names its header
CREATE TABLE ${QUEUE_TERMINALS_TABLE} (
  header_hash ${bytes} NOT NULL,
  transaction_hash ${bytes} NOT NULL,
  terminal_outcome text NOT NULL,
  tx_index integer NOT NULL,
  block_hash ${bytes} NOT NULL,
  height ${int8} NOT NULL,
  slot ${int8} NOT NULL,
  PRIMARY KEY (header_hash, transaction_hash)
);
CREATE INDEX ${QUEUE_TERMINALS_TABLE}_slot ON ${QUEUE_TERMINALS_TABLE} (slot);
CREATE INDEX ${QUEUE_TERMINALS_TABLE}_height ON ${QUEUE_TERMINALS_TABLE} (height);
`;
};

export const queueTerminalMigrations = (
  dialect: DialectName,
): MigrationSet => ({
  namespace: QUEUE_TERMINALS_TABLE,
  migrations: [
    { id: "0001_node_l1_queue_terminals", sql: queueTerminalSql(dialect) },
  ],
});
