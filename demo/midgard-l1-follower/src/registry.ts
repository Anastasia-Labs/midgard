/**
 * The temporal-table registry (§7.2). Every D-t projection registers its
 * shape, slot columns, parents and retention; the rewind and the pruning SQL
 * are generated from it, so a new projection is rewound, pruned and covered
 * by the property test without new code.
 */

export type RetentionRule =
  /** Versioned state: closed rows go once their end slot is final (> k deep). */
  | Readonly<{ kind: "closed_k_deep" }>
  /** Append-only log: rows go once their slot is final (> k deep). */
  | Readonly<{ kind: "created_k_deep" }>
  /** The follower never prunes it; the owning stage does, as `description` says. */
  | Readonly<{ kind: "owner"; description: string }>;

/**
 * Keeps a row past its retention rule while `table` holds a row whose
 * `tableColumn` equals this row's `column`: a decision that reads the row
 * stays answerable while the pinning row lives. `table` is a registered
 * temporal table (itself possibly pinned: pins are transitive, never
 * cyclic), or an owner-managed table a migration declares (the schema lint
 * checks it), whose rows its owner deletes when the hold ends.
 */
export type TemporalRowPin = Readonly<{
  column: string;
  table: string;
  tableColumn: string;
}>;

export type TemporalTableSpec =
  | Readonly<{
      name: string;
      /** `(key…, value…, start, end NULL)`; an update closes and inserts. */
      shape: "versioned";
      startColumn: string;
      endColumn: string;
      /** Tables this one references; rewound after it, pruned after it. */
      parents?: readonly string[];
      retention: RetentionRule;
      pinnedBy?: readonly TemporalRowPin[];
    }>
  | Readonly<{
      name: string;
      /** `(…, slot)`; rows are only ever inserted. */
      shape: "append_only";
      slotColumn: string;
      parents?: readonly string[];
      retention: RetentionRule;
      pinnedBy?: readonly TemporalRowPin[];
    }>;

export class RegistryError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "RegistryError";
  }
}

const IDENTIFIER = /^[a-z_][a-z0-9_]*$/u;

const assertIdentifier = (value: string, what: string): void => {
  if (!IDENTIFIER.test(value))
    throw new RegistryError(`${what} "${value}" is not a plain identifier`);
};

/** A fact table (class A) may be named as a parent; facts rewind last anyway. */
const isFactTable = (name: string): boolean => name.startsWith("l1_");

export type Statement = Readonly<{ sql: string; params: readonly number[] }>;

export type TemporalRegistry = Readonly<{
  /** Parents before children. */
  tables: readonly TemporalTableSpec[];
  has(name: string): boolean;
  /** Children first (reverse dependency order), each cut at `slot`. */
  rewindStatements(slot: number): Statement[];
  /** The slot columns of a table, for checks and reads. */
  slotColumns(table: TemporalTableSpec): readonly string[];
}>;

export const slotColumnsOf = (table: TemporalTableSpec): readonly string[] =>
  table.shape === "versioned"
    ? [table.startColumn, table.endColumn]
    : [table.slotColumn];

export const createTemporalRegistry = (
  specs: readonly TemporalTableSpec[],
): TemporalRegistry => {
  const byName = new Map<string, TemporalTableSpec>();
  for (const spec of specs) {
    assertIdentifier(spec.name, "table");
    for (const column of slotColumnsOf(spec))
      assertIdentifier(column, "column");
    if (isFactTable(spec.name))
      throw new RegistryError(
        `${spec.name}: the l1_ prefix is reserved for facts`,
      );
    if (byName.has(spec.name))
      throw new RegistryError(`${spec.name} is registered twice`);
    if (spec.retention.kind === "closed_k_deep" && spec.shape !== "versioned")
      throw new RegistryError(
        `${spec.name}: closed_k_deep needs a versioned table`,
      );
    if (
      spec.retention.kind === "created_k_deep" &&
      spec.shape !== "append_only"
    )
      throw new RegistryError(
        `${spec.name}: created_k_deep needs an append-only table`,
      );
    byName.set(spec.name, spec);
  }
  // A registered pinning table prunes before the tables it pins (it comes
  // after them in `tables`), so one prune step releases a whole pin chain.
  const pinnedByTable = new Map<string, TemporalTableSpec[]>();
  for (const spec of specs)
    for (const pin of spec.pinnedBy ?? []) {
      assertIdentifier(pin.table, "table");
      assertIdentifier(pin.column, "column");
      assertIdentifier(pin.tableColumn, "column");
      if (byName.has(pin.table))
        pinnedByTable.set(pin.table, [
          ...(pinnedByTable.get(pin.table) ?? []),
          spec,
        ]);
    }
  for (const spec of specs)
    for (const parent of spec.parents ?? []) {
      if (parent === spec.name)
        throw new RegistryError(`${spec.name} lists itself as a parent`);
      if (!byName.has(parent) && !isFactTable(parent))
        throw new RegistryError(
          `${spec.name}: parent ${parent} is not registered`,
        );
    }
  // Topological order, parents (and the tables a table pins) first; ties
  // keep registration order.
  const ordered: TemporalTableSpec[] = [];
  const state = new Map<string, "visiting" | "done">();
  const visit = (spec: TemporalTableSpec, path: string[]): void => {
    const mark = state.get(spec.name);
    if (mark === "done") return;
    if (mark === "visiting")
      throw new RegistryError(
        `parent or pin cycle: ${[...path, spec.name].join(" -> ")}`,
      );
    state.set(spec.name, "visiting");
    for (const parent of spec.parents ?? []) {
      const parentSpec = byName.get(parent);
      if (parentSpec !== undefined) visit(parentSpec, [...path, spec.name]);
    }
    for (const pinned of pinnedByTable.get(spec.name) ?? [])
      visit(pinned, [...path, spec.name]);
    state.set(spec.name, "done");
    ordered.push(spec);
  };
  for (const spec of specs) visit(spec, []);
  const rewindOrder = [...ordered].reverse();
  return {
    tables: ordered,
    has: (name) => byName.has(name),
    slotColumns: slotColumnsOf,
    rewindStatements: (slot) =>
      rewindOrder.flatMap((table): Statement[] =>
        table.shape === "versioned"
          ? [
              {
                sql: `DELETE FROM ${table.name} WHERE ${table.startColumn} > ?`,
                params: [slot],
              },
              {
                sql: `UPDATE ${table.name} SET ${table.endColumn} = NULL WHERE ${table.endColumn} > ?`,
                params: [slot],
              },
            ]
          : [
              {
                sql: `DELETE FROM ${table.name} WHERE ${table.slotColumn} > ?`,
                params: [slot],
              },
            ],
      ),
  };
};

/** The `NOT EXISTS` clauses of a table's row pins, for a row aliased `alias`. */
export const rowPinClauses = (
  table: TemporalTableSpec,
  alias: string,
): string =>
  (table.pinnedBy ?? [])
    .map(
      (pin) =>
        ` AND NOT EXISTS (SELECT 1 FROM ${pin.table} p WHERE p.${pin.tableColumn} = ${alias}.${pin.column})`,
    )
    .join("");

/**
 * The pruning predicate of a table (aliased `t`) at the final boundary
 * slot, or null.
 */
export const prunePredicate = (table: TemporalTableSpec): string | null => {
  const rule = ((): string | null => {
    switch (table.retention.kind) {
      case "closed_k_deep":
        return table.shape === "versioned"
          ? `t.${table.endColumn} IS NOT NULL AND t.${table.endColumn} <= ?`
          : null;
      case "created_k_deep":
        return table.shape === "append_only"
          ? `t.${table.slotColumn} <= ?`
          : null;
      case "owner":
        return null;
    }
  })();
  return rule === null ? null : `${rule}${rowPinClauses(table, "t")}`;
};
