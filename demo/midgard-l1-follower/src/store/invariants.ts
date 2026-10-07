import { slotColumnsOf, type TemporalRegistry } from "../registry.js";
import { asNumber, type Dialect, type SqlTx } from "../sql/backend.js";

export type InvariantName =
  | "INV1"
  | "INV2"
  | "INV3"
  | "INV4"
  | "INV5"
  | "INV6"
  | "REGISTRY";

export type InvariantViolation = Readonly<{
  invariant: InvariantName;
  check: string;
  /** The first offending row, as returned by the query. */
  sample: Record<string, unknown>;
}>;

export type InvariantReport =
  | Readonly<{ ok: true }>
  | Readonly<{ ok: false; violations: readonly InvariantViolation[] }>;

type Check = Readonly<{ invariant: InvariantName; check: string; sql: string }>;

/**
 * The cursor's slot and pruned-through slot are bound as parameters, read
 * once per run: a scalar subquery would leave the planner guessing its value
 * and, at 10^6 outputs, choosing a sequential scan for `created_slot > x`.
 */
const CURSOR_SLOT = ":cursor_slot";
const PRUNED_THROUGH = ":pruned_through";
const MARKER = /:(cursor_slot|pruned_through)\b/gu;

/** INV3 needs the outref-in-array test, the one place the dialects differ. */
const inv3 = (dialect: Dialect): string =>
  dialect.name === "postgres"
    ? `SELECT o.tx_hash, o.output_index FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE NOT ((t.is_valid AND (o.tx_hash || int2send(o.output_index::int2)) = ANY (t.inputs))
         OR (NOT t.is_valid AND (o.tx_hash || int2send(o.output_index::int2)) = ANY (t.collaterals)))`
    : `SELECT o.tx_hash, o.output_index FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE NOT EXISTS (
   SELECT 1 FROM json_each(CASE WHEN t.is_valid = 1 THEN t.inputs ELSE t.collaterals END) j
    WHERE j.value = lower(hex(o.tx_hash)) || printf('%04x', o.output_index))`;

/** Nothing above the cursor (INV5, first half), each probe index-backed. */
const aboveCursorChecks = (registry: TemporalRegistry): Check[] => [
  {
    invariant: "INV5",
    check: "block above cursor",
    sql: `SELECT slot FROM l1_blocks WHERE slot > ${CURSOR_SLOT}`,
  },
  {
    invariant: "INV5",
    check: "tx above cursor",
    sql: `SELECT tx_hash FROM l1_txs WHERE block_slot > ${CURSOR_SLOT}`,
  },
  {
    invariant: "INV5",
    check: "output created above cursor",
    sql: `SELECT tx_hash, output_index FROM l1_outputs WHERE created_slot > ${CURSOR_SLOT}`,
  },
  {
    invariant: "INV5",
    check: "output spent above cursor",
    sql: `SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot > ${CURSOR_SLOT}`,
  },
  {
    invariant: "INV5",
    check: "event key above cursor",
    sql: `SELECT kind, key FROM l1_event_keys WHERE first_canonical_slot > ${CURSOR_SLOT}`,
  },
  {
    invariant: "INV5",
    check: "cursor is not a stored block",
    sql: `SELECT c.slot FROM l1_follower_cursor c WHERE NOT EXISTS (
      SELECT 1 FROM l1_blocks b WHERE b.slot = c.slot AND b.hash = c.hash AND b.height = c.height)`,
  },
  ...registry.tables.flatMap((table) =>
    slotColumnsOf(table).map(
      (column): Check => ({
        invariant: "REGISTRY",
        check: `${table.name}.${column} above cursor`,
        sql: `SELECT ${column} FROM ${table.name} WHERE ${column} > ${CURSOR_SLOT}`,
      }),
    ),
  ),
];

const linkage = (where: string): string =>
  `SELECT b.slot, b.height FROM l1_blocks b LEFT JOIN l1_blocks pb ON pb.hash = b.parent_hash
 WHERE b.slot > ${PRUNED_THROUGH}${where}
   AND (pb.hash IS NULL OR pb.height + 1 <> b.height)`;

const fullChecks = (dialect: Dialect, registry: TemporalRegistry): Check[] => [
  {
    invariant: "INV1",
    check: "spent before created",
    sql: "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot < created_slot",
  },
  {
    invariant: "INV1",
    check: "spent before created within a block",
    sql: `SELECT o.tx_hash, o.output_index FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE o.spent_slot = o.created_slot AND t.block_tx_index <= o.created_tx_index`,
  },
  {
    invariant: "INV2",
    check: "spender missing or elsewhere",
    sql: `SELECT o.tx_hash, o.output_index FROM l1_outputs o LEFT JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE o.spent_tx IS NOT NULL AND (t.tx_hash IS NULL OR t.block_slot <> o.spent_slot)`,
  },
  {
    invariant: "INV3",
    check: "spender does not consume the outref",
    sql: inv3(dialect),
  },
  {
    invariant: "INV4",
    check: "creator missing or does not create the index",
    sql: `SELECT o.tx_hash, o.output_index FROM l1_outputs o LEFT JOIN l1_txs t ON t.tx_hash = o.tx_hash
 WHERE o.created_slot IS NOT NULL AND (t.tx_hash IS NULL OR t.block_slot <> o.created_slot
   OR (t.is_valid AND o.output_index >= t.output_count)
   OR (NOT t.is_valid AND NOT (t.has_collateral_return AND o.output_index = t.output_count)))`,
  },
  ...aboveCursorChecks(registry),
  { invariant: "INV5", check: "chain linkage", sql: linkage("") },
  {
    invariant: "INV6",
    check: "seed row whose creator was seen",
    sql: "SELECT o.tx_hash, o.output_index FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.tx_hash WHERE o.created_slot IS NULL",
  },
  ...registry.tables.flatMap((table): Check[] =>
    table.shape === "versioned"
      ? [
          {
            invariant: "REGISTRY",
            check: `${table.name} closed before opened`,
            sql: `SELECT ${table.startColumn} FROM ${table.name} WHERE ${table.endColumn} < ${table.startColumn}`,
          },
        ]
      : [],
  ),
];

/**
 * The scoped check after a rewind (§7.1, §7.5 R5). A rewind only deletes
 * rows above its target and clears spends above it, so the rows it can
 * have left wrong are the ones above the new cursor and the cursor's own
 * linkage. Every probe is index-backed, so the check costs O(1) in the
 * stored state; the full INV1–INV6 scan runs at start (`checkInvariants`).
 */
const postRewindChecks = (registry: TemporalRegistry): Check[] => [
  ...aboveCursorChecks(registry),
  {
    invariant: "INV5",
    check: "cursor block linkage",
    sql: linkage(` AND b.slot = ${CURSOR_SLOT}`),
  },
];

export type InvariantScope = "full" | "post_rewind";

export const runInvariantChecks = async (
  tx: SqlTx,
  dialect: Dialect,
  registry: TemporalRegistry,
  scope: InvariantScope,
): Promise<InvariantReport> => {
  const checks =
    scope === "full"
      ? fullChecks(dialect, registry)
      : postRewindChecks(registry);
  const cursor = (
    await tx.query("SELECT slot, pruned_through_slot FROM l1_follower_cursor")
  )[0];
  const values = {
    cursor_slot: cursor === undefined ? null : asNumber(cursor.slot),
    pruned_through:
      cursor === undefined ? null : asNumber(cursor.pruned_through_slot),
  };
  const violations: InvariantViolation[] = [];
  for (const check of checks) {
    const params: (number | null)[] = [];
    const sql = check.sql.replace(
      MARKER,
      (_, name: "cursor_slot" | "pruned_through") => {
        params.push(values[name]);
        return "?";
      },
    );
    const rows = await tx.query(`${sql} LIMIT 1`, params);
    const sample = rows[0];
    if (sample !== undefined)
      violations.push({
        invariant: check.invariant,
        check: check.check,
        sample,
      });
  }
  return violations.length === 0 ? { ok: true } : { ok: false, violations };
};

export const describeViolations = (report: InvariantReport): string =>
  report.ok
    ? "ok"
    : report.violations.map((v) => `${v.invariant} (${v.check})`).join("; ");
