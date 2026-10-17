import type { FactStore } from "../store/fact-store.js";

/** Normalises a column value so a Postgres row and a SQLite row compare equal. */
const normalise = (value: unknown): unknown => {
  if (value === null || value === undefined) return null;
  if (Buffer.isBuffer(value) || value instanceof Uint8Array)
    return Buffer.from(value).toString("hex");
  if (typeof value === "boolean") return value ? 1 : 0;
  if (typeof value === "bigint") return Number(value);
  if (typeof value === "string") {
    if (/^-?\d+$/u.test(value) && value.length < 16) return Number(value);
    if (value.startsWith("{") || value.startsWith("["))
      return canonical(JSON.parse(value) as unknown);
    return value;
  }
  if (Array.isArray(value) || typeof value === "object")
    return canonical(value);
  return value;
};

const canonical = (value: unknown): unknown => {
  if (Array.isArray(value))
    return value.map((item) =>
      Buffer.isBuffer(item) ? item.toString("hex") : canonical(item),
    );
  if (value !== null && typeof value === "object" && !Buffer.isBuffer(value))
    return Object.fromEntries(
      Object.entries(value as Record<string, unknown>)
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
        .map(([key, item]) => [key, canonical(item)]),
    );
  return value;
};

const rowKey = (row: Record<string, unknown>): string =>
  JSON.stringify(
    Object.fromEntries(
      Object.keys(row)
        .sort()
        .map((key) => [key, normalise(row[key])]),
    ),
  );

export type StoreDump = Readonly<Record<string, readonly string[]>>;

const FACT_QUERIES: Readonly<Record<string, string>> = {
  l1_blocks: "SELECT * FROM l1_blocks",
  l1_txs: "SELECT * FROM l1_txs",
  l1_tx_mint_policies: "SELECT * FROM l1_tx_mint_policies",
  l1_outputs: "SELECT * FROM l1_outputs",
  l1_output_assets: "SELECT * FROM l1_output_assets",
  // Class C is not rewound (§5.1): compare only what retained rows reference.
  l1_scripts:
    "SELECT * FROM l1_scripts s WHERE EXISTS (SELECT 1 FROM l1_outputs o WHERE o.script_ref_hash = s.script_hash)",
  l1_event_keys: "SELECT * FROM l1_event_keys",
  l1_protocol_init: "SELECT * FROM l1_protocol_init",
  // The generation and the rollback log are history, not chain state.
  l1_follower_cursor:
    "SELECT slot, hash, height, origin_slot, origin_hash FROM l1_follower_cursor",
};

/**
 * Every fact table and every registered temporal table, as sorted canonical
 * row strings. `where` optionally restricts tables to a window (prune runs).
 */
export const dumpStore = async (
  store: FactStore,
  queries: Readonly<Record<string, string>> = FACT_QUERIES,
): Promise<StoreDump> =>
  store.transaction("read", async (tx) => {
    const dump: Record<string, string[]> = {};
    const all: Record<string, string> = { ...queries };
    for (const table of store.registry.tables)
      if (all[table.name] === undefined)
        all[table.name] = `SELECT * FROM ${table.name}`;
    for (const [table, sql] of Object.entries(all))
      dump[table] = (await tx.query(sql)).map(rowKey).sort();
    return dump;
  });

/** The first table that differs, with a short sample, or null when equal. */
export const diffDumps = (
  actual: StoreDump,
  expected: StoreDump,
): string | null => {
  for (const table of Object.keys(expected).sort()) {
    const left = actual[table] ?? [];
    const right = expected[table] ?? [];
    if (
      left.length === right.length &&
      left.every((row, i) => row === right[i])
    )
      continue;
    const rightSet = new Set(right);
    const leftSet = new Set(left);
    const extra = left.filter((row) => !rightSet.has(row)).slice(0, 2);
    const missing = right.filter((row) => !leftSet.has(row)).slice(0, 2);
    return `${table}: ${left.length} rows vs ${right.length} expected; extra ${JSON.stringify(extra)}; missing ${JSON.stringify(missing)}`;
  }
  return null;
};

export { FACT_QUERIES };
