/**
 * The forced-order projection's D-t table (plan §5.5, §11, §12): one row
 * per authenticated tx order, with the carriage the order's own block
 * resolved. The follower's registry generates its rewind and pruning; a
 * rewind past the order's block deletes the row.
 */
import type {
  DialectName,
  MigrationSet,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

export const FORCED_ORDERS_TABLE = "node_l1_forced_order_fields";

export const FORCED_ORDER_TABLES: readonly TemporalTableSpec[] = [
  {
    name: FORCED_ORDERS_TABLE,
    shape: "versioned",
    startColumn: "order_slot",
    endColumn: "spent_slot",
    retention: { kind: "closed_k_deep" },
  },
];

/**
 * `status`:
 * - `resolved`: every carried field was read in the order's block (§12.3
 *   step 1, or tier 1 inline), and `field_preimages` holds all nine;
 * - `carriage_pending`: some carriage outref was created before the block;
 *   the driver hook resolves it (steps 2 to 4);
 * - `malformed`: the order authenticates but its mint redeemer or in-block
 *   carriage does not open (`detail` says why). The real tx-order mint
 *   admits no such order.
 */
const forcedOrderSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: live orders forever; a spent order once spent_slot is k deep
CREATE TABLE ${FORCED_ORDERS_TABLE} (
  order_tx_hash ${bytes} NOT NULL,
  order_output_index integer NOT NULL,
  order_tx_index integer NOT NULL,
  block_hash ${bytes} NOT NULL,
  height ${int8} NOT NULL,
  order_slot ${int8} NOT NULL,
  spent_slot ${int8},
  parent_slot ${int8} NOT NULL,
  parent_hash ${bytes} NOT NULL,
  inclusion_time ${int8} NOT NULL,
  status text NOT NULL,
  reference_inputs ${bytes} NOT NULL,
  mint_redeemer ${bytes},
  block_datums text NOT NULL,
  field_preimages ${bytes},
  detail text,
  PRIMARY KEY (order_tx_hash, order_output_index)
);
CREATE INDEX ${FORCED_ORDERS_TABLE}_slot ON ${FORCED_ORDERS_TABLE} (order_slot);
CREATE INDEX ${FORCED_ORDERS_TABLE}_spent ON ${FORCED_ORDERS_TABLE} (spent_slot);
`;
};

export const forcedOrderMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: "node_l1_forced_orders",
  migrations: [
    { id: "0001_node_l1_forced_orders", sql: forcedOrderSql(dialect) },
  ],
});
