import type { TemporalTableSpec } from "../registry.js";
import type { RetentionPin, RetentionPins } from "../store/context.js";
import type { FactStore } from "../store/fact-store.js";
import { dumpStore, FACT_QUERIES, type StoreDump } from "./replay.js";

/**
 * Whether a registered temporal row (alias `alias`) is retained once the
 * store pruned through slot `s`, from the plan §11 rules: a `closed_k_deep`
 * versioned row while it is open or closed after `s`, a `created_k_deep`
 * append-only row while it was created after `s`, and every other row
 * (`owner`, or a rule the shape never meets) forever.
 */
const rowRetained = (
  table: TemporalTableSpec,
  alias: string,
  s: number,
): string => {
  if (table.retention.kind === "closed_k_deep" && table.shape === "versioned")
    return `(${alias}.${table.endColumn} IS NULL OR ${alias}.${table.endColumn} > ${s})`;
  if (
    table.retention.kind === "created_k_deep" &&
    table.shape === "append_only"
  )
    return `${alias}.${table.slotColumn} > ${s}`;
  return "1 = 1";
};

const OUTPUT_RETAINED = (alias: string, s: number): string =>
  `(${alias}.spent_slot IS NULL OR ${alias}.spent_slot > ${s})`;

/**
 * The rows of every fact table and registered temporal table that a store
 * pruned through slot `s` must still hold (plan §11), selected from an
 * unpruned store with the same facts:
 *
 * - outputs: live, or spent after `s`; their assets with them;
 * - txs: in a block after `s`, or that created or spent a retained output,
 *   or pinned by a retained row of a `retentionPins` table; their mint
 *   policies with them;
 * - blocks: at or after `s`, every 1,000th height, the origin, and any block
 *   a retained tx or output (created or spent there) or pin references;
 * - scripts: referenced by a retained output; event keys and the cursor:
 *   always;
 * - registered temporal rows: by their retention rule (`rowRetained`).
 *
 * At `s` = the origin slot (nothing pruned) every row is retained.
 */
export const retainedQueries = (
  tables: readonly TemporalTableSpec[],
  pins: RetentionPins,
  s: number,
): Record<string, string> => {
  const pinned = (list: readonly RetentionPin[] | undefined, value: string) =>
    (list ?? [])
      .map((pin) => {
        const table = tables.find((t) => t.name === pin.table);
        const retained =
          table === undefined ? "1 = 1" : rowRetained(table, "p", s);
        return ` OR EXISTS (SELECT 1 FROM ${pin.table} p WHERE p.${pin.column} = ${value} AND ${retained})`;
      })
      .join("");
  const txRetained = (alias: string): string =>
    `(${alias}.block_slot > ${s}
      OR EXISTS (SELECT 1 FROM l1_outputs o WHERE (o.tx_hash = ${alias}.tx_hash OR o.spent_tx = ${alias}.tx_hash) AND ${OUTPUT_RETAINED("o", s)})${pinned(pins.txs, `${alias}.tx_hash`)})`;
  const queries: Record<string, string> = {
    ...FACT_QUERIES,
    l1_outputs: `SELECT * FROM l1_outputs o WHERE ${OUTPUT_RETAINED("o", s)}`,
    l1_output_assets: `SELECT * FROM l1_output_assets a WHERE EXISTS (SELECT 1 FROM l1_outputs o WHERE o.tx_hash = a.tx_hash AND o.output_index = a.output_index AND ${OUTPUT_RETAINED("o", s)})`,
    l1_txs: `SELECT * FROM l1_txs t WHERE ${txRetained("t")}`,
    l1_tx_mint_policies: `SELECT * FROM l1_tx_mint_policies m WHERE EXISTS (SELECT 1 FROM l1_txs t WHERE t.tx_hash = m.tx_hash AND ${txRetained("t")})`,
    l1_blocks: `SELECT * FROM l1_blocks b WHERE b.slot >= ${s}
      OR b.height % 1000 = 0
      OR b.slot = (SELECT origin_slot FROM l1_follower_cursor)
      OR EXISTS (SELECT 1 FROM l1_txs t WHERE t.block_slot = b.slot AND ${txRetained("t")})
      OR EXISTS (SELECT 1 FROM l1_outputs o WHERE (o.created_slot = b.slot OR o.spent_slot = b.slot) AND ${OUTPUT_RETAINED("o", s)})${pinned(pins.blocks, "b.slot")}`,
    l1_scripts: `SELECT * FROM l1_scripts c WHERE EXISTS (SELECT 1 FROM l1_outputs o WHERE o.script_ref_hash = c.script_hash AND ${OUTPUT_RETAINED("o", s)})`,
  };
  for (const table of tables)
    queries[table.name] =
      `SELECT * FROM ${table.name} r WHERE ${rowRetained(table, "r", s)}`;
  return queries;
};

const counts = (rows: readonly string[]): Map<string, number> => {
  const map = new Map<string, number>();
  for (const row of rows) map.set(row, (map.get(row) ?? 0) + 1);
  return map;
};

/** Rows of `rows` beyond what `within` holds (multiset difference), up to two. */
const beyond = (rows: readonly string[], within: readonly string[]) => {
  const left = counts(within);
  const out: string[] = [];
  for (const row of rows) {
    const n = left.get(row) ?? 0;
    if (n > 0) left.set(row, n - 1);
    else out.push(row);
  }
  return out;
};

/**
 * Compares a pruned store with a fresh, unpruned replay of the same chain:
 * every row the store holds is a row of the replay (nothing invented or
 * changed, pruned rows only missing), and every row the replay holds that
 * retention keeps at the store's `prunedThroughSlot` is in the store. The
 * first table that fails, with a short sample, or null.
 */
export const diffPruned = (
  actual: StoreDump,
  full: StoreDump,
  retained: StoreDump,
): string | null => {
  for (const table of Object.keys(full).sort()) {
    const extra = beyond(actual[table] ?? [], full[table] ?? []);
    if (extra.length > 0)
      return `${table}: ${extra.length} rows not in a fresh replay; ${JSON.stringify(extra.slice(0, 2))}`;
    const missing = beyond(retained[table] ?? [], actual[table] ?? []);
    if (missing.length > 0)
      return `${table}: ${missing.length} retained rows pruned; ${JSON.stringify(missing.slice(0, 2))}`;
  }
  return null;
};

/** The rows `reference` (unpruned) holds that a store pruned through `s` must keep. */
export const dumpRetained = (
  reference: FactStore,
  pins: RetentionPins,
  s: number,
): Promise<StoreDump> =>
  dumpStore(reference, retainedQueries(reference.registry.tables, pins, s));
