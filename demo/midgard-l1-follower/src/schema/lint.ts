import type { TemporalRegistry } from "../registry.js";
import type { MigrationSet } from "./migrate.js";

/** Persistent table classes (§5.1); D-v and D-c are never tables. */
export const TABLE_CLASSES = ["A", "B", "C", "D-t", "D-x"] as const;
export type TableClass = (typeof TABLE_CLASSES)[number];

export type DeclaredTable = Readonly<{
  namespace: string;
  migration: string;
  table: string;
  tableClass: TableClass;
  retention: string;
}>;

export type SchemaLintProblem = Readonly<{
  namespace: string;
  migration: string;
  table: string | null;
  message: string;
}>;

const CREATE_TABLE =
  /^\s*CREATE\s+(?:TEMP(?:ORARY)?\s+|UNLOGGED\s+)?TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?("?)([A-Za-z_][A-Za-z0-9_.]*)\1/iu;
const HEADER = /^\s*--\s*class:\s*([A-Za-z-]+)\s*;\s*retention:\s*(.*?)\s*$/u;

const isTableClass = (value: string): value is TableClass =>
  (TABLE_CLASSES as readonly string[]).includes(value);

/** A foreign key a migration declares: `table` references `target`. */
type ForeignKey = Readonly<{
  namespace: string;
  migration: string;
  table: string;
  target: string;
}>;

type Scan = {
  declared: DeclaredTable[];
  problems: SchemaLintProblem[];
  foreignKeys: ForeignKey[];
};

const emptyScan = (): Scan => ({ declared: [], problems: [], foreignKeys: [] });

const STATEMENT_OWNER =
  /^\s*(?:CREATE\s+(?:TEMP(?:ORARY)?\s+|UNLOGGED\s+)?TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?|ALTER\s+TABLE\s+(?:IF\s+EXISTS\s+)?(?:ONLY\s+)?)("?)([A-Za-z_][A-Za-z0-9_.]*)\1/iu;
const REFERENCES = /\bREFERENCES\s+("?)([A-Za-z_][A-Za-z0-9_.]*)\1/giu;

/** The foreign keys of every `CREATE TABLE` and `ALTER TABLE` statement. */
const scanForeignKeys = (
  namespace: string,
  id: string,
  sql: string,
  scan: Scan,
): void => {
  const code = sql.replace(/\/\*[\s\S]*?\*\//gu, " ").replace(/--[^\n]*/gu, "");
  for (const statement of code.split(";")) {
    const owner = STATEMENT_OWNER.exec(statement);
    if (owner === null) continue;
    for (const reference of statement.matchAll(REFERENCES))
      scan.foreignKeys.push({
        namespace,
        migration: id,
        table: (owner[2] ?? "").toLowerCase(),
        target: (reference[2] ?? "").toLowerCase(),
      });
  }
};

const scanMigration = (
  namespace: string,
  id: string,
  sql: string,
  scan: Scan,
): void => {
  const lines = sql.split(/\r?\n/u);
  lines.forEach((line, index) => {
    const match = CREATE_TABLE.exec(line);
    if (match === null) return;
    const table = (match[2] ?? "").toLowerCase();
    // The header is the nearest non-blank line above, and must be a comment.
    let cursor = index - 1;
    while (cursor >= 0 && (lines[cursor] ?? "").trim() === "") cursor -= 1;
    const header = HEADER.exec(cursor >= 0 ? (lines[cursor] ?? "") : "");
    const problem = (message: string): void => {
      scan.problems.push({ namespace, migration: id, table, message });
    };
    if (header === null) {
      problem("missing `-- class: <A|B|C|D-t|D-x>; retention: <rule>` header");
      return;
    }
    const tableClass = header[1] ?? "";
    const retention = header[2] ?? "";
    if (!isTableClass(tableClass)) {
      problem(
        `class "${tableClass}" is not one of ${TABLE_CLASSES.join(", ")}`,
      );
      return;
    }
    if (retention.length === 0) {
      problem("retention rule is empty");
      return;
    }
    scan.declared.push({
      namespace,
      migration: id,
      table,
      tableClass,
      retention,
    });
  });
  scanForeignKeys(namespace, id, sql, scan);
  if (
    /\bCREATE\s+TABLE\b/iu.test(sql) &&
    !lines.some((line) => CREATE_TABLE.test(line))
  )
    scan.problems.push({
      namespace,
      migration: id,
      table: null,
      message:
        "CREATE TABLE must start its own line so the header can be checked",
    });
};

/**
 * The tables one migration declares, and its header problems. The migration
 * runner refuses a migration with problems, so every table it creates is
 * in the catalog with its class (`l1_follower_tables`).
 */
export const migrationTables = (
  namespace: string,
  id: string,
  sql: string,
): Readonly<{ declared: DeclaredTable[]; problems: SchemaLintProblem[] }> => {
  const scan = emptyScan();
  scanMigration(namespace, id, sql, scan);
  return scan;
};

/**
 * The schema lint (§5.1, §7.2, §11): every table in every migration declares
 * a class and a retention rule on the line above its `CREATE TABLE`, and
 * every D-t table is in the temporal registry (and every registered table is
 * a declared D-t or D-x table). A class B table declares no foreign key into
 * a table that is not class B: `follower reset --to-origin` and a rewind
 * delete non-B rows, and must never be blocked by, or cascade into, B rows.
 * Returns the problems; empty means clean.
 */
export const lintSchema = (
  sets: readonly MigrationSet[],
  registry?: TemporalRegistry,
): SchemaLintProblem[] => {
  const scan = emptyScan();
  for (const set of sets)
    for (const migration of set.migrations)
      scanMigration(set.namespace, migration.id, migration.sql, scan);
  const declaredByName = new Map(
    scan.declared.map((table) => [table.table, table]),
  );
  for (const key of scan.foreignKeys) {
    if (declaredByName.get(key.table)?.tableClass !== "B") continue;
    const target = declaredByName.get(key.target)?.tableClass;
    if (target === "B") continue;
    scan.problems.push({
      namespace: key.namespace,
      migration: key.migration,
      table: key.table,
      message: `class B table references ${key.target} (${
        target === undefined ? "declared by no migration" : `class ${target}`
      }); a class B table may reference only class B tables`,
    });
  }
  for (const table of scan.declared)
    if (table.tableClass === "D-t" && registry?.has(table.table) !== true)
      scan.problems.push({
        namespace: table.namespace,
        migration: table.migration,
        table: table.table,
        message: "D-t table is not registered in the temporal registry",
      });
  for (const spec of registry?.tables ?? []) {
    const declared = declaredByName.get(spec.name);
    if (
      declared === undefined ||
      (declared.tableClass !== "D-t" && declared.tableClass !== "D-x")
    )
      scan.problems.push({
        namespace: declared?.namespace ?? "(registry)",
        migration: declared?.migration ?? "(registry)",
        table: spec.name,
        message:
          "registered temporal table is not declared D-t or D-x by any migration",
      });
  }
  return scan.problems;
};

/** The class and retention of every table the migrations declare. */
export const declaredTables = (
  sets: readonly MigrationSet[],
): DeclaredTable[] => {
  const scan = emptyScan();
  for (const set of sets)
    for (const migration of set.migrations)
      scanMigration(set.namespace, migration.id, migration.sql, scan);
  return scan.declared;
};
