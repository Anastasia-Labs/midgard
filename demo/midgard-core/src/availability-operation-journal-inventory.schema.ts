import type { DatabaseSync } from "node:sqlite";

export class AvailabilityJournalInventoryError extends Error {
  constructor(readonly code: string) {
    super(code);
    this.name = "AvailabilityJournalInventoryError";
  }
}

export function inventoryRefuse(code: string): never {
  throw new AvailabilityJournalInventoryError(code);
}

/** Fixed schema 3 tables and ordered primary keys; never create missing families. */
export const inventoryFamilies = [
  {
    name: "metadata",
    table: "availability_journal_metadata",
    keys: ["key"],
    columns: ["key:TEXT:0:1", "value:TEXT:1:0"],
  },
  {
    name: "leases",
    table: "availability_operation_leases",
    keys: ["scope"],
    columns: [
      "scope:TEXT:0:1",
      "owner:TEXT:1:0",
      "generation:INTEGER:1:0",
      "expires_at:INTEGER:1:0",
    ],
  },
  {
    name: "intents",
    table: "availability_operation_intents",
    keys: ["id"],
    columns: [
      "id:TEXT:0:1",
      "deployment:TEXT:1:0",
      "actor:TEXT:1:0",
      "record:TEXT:1:0",
      "state:TEXT:1:0",
      "tx_hash:TEXT:1:0",
    ],
  },
  {
    name: "resources",
    table: "availability_operation_resources",
    keys: ["resource", "intent_id"],
    columns: [
      "resource:TEXT:1:1",
      "intent_id:TEXT:1:2",
      "kind:TEXT:1:0",
      "actor:TEXT:1:0",
    ],
  },
  {
    name: "dependencies",
    table: "availability_operation_dependencies",
    keys: ["parent_tx_hash", "child_id"],
    columns: ["parent_tx_hash:TEXT:1:1", "child_id:TEXT:1:2"],
  },
  {
    name: "workflows",
    table: "availability_operation_workflows",
    keys: ["actor", "deployment", "header_hash"],
    columns: [
      "actor:TEXT:1:1",
      "deployment:TEXT:1:2",
      "header_hash:TEXT:1:3",
      "retired_by:TEXT:0:0",
      "release:TEXT:0:0",
    ],
  },
] as const;

export type InventoryFamily = (typeof inventoryFamilies)[number]["name"];

export const validateInventorySchema = (db: DatabaseSync): void => {
  // The first read pins the ordinary BEGIN transaction, including its WAL state.
  const metadata = db
    .prepare(
      "SELECT type FROM sqlite_schema WHERE name = 'availability_journal_metadata'",
    )
    .get();
  if (metadata?.type !== "table")
    inventoryRefuse("inventory_schema_incomplete");
  const schema = db
    .prepare(
      `SELECT CASE WHEN typeof(value) = 'text'
    AND length(CAST(value AS BLOB)) = 1 THEN value END AS value
    FROM availability_journal_metadata WHERE key = 'schema'`,
    )
    .get()?.value;
  if (schema === "1" || schema === "2")
    inventoryRefuse("historical_schema_inventory_unsupported");
  if (schema !== "3") inventoryRefuse("inventory_schema_unsupported");
  for (const family of inventoryFamilies) {
    if (
      db
        .prepare("SELECT type FROM sqlite_schema WHERE name = ?")
        .get(family.table)?.type !== "table"
    )
      inventoryRefuse("inventory_schema_incomplete");
    const columns = db
      .prepare(
        `SELECT CASE WHEN length(CAST(name AS BLOB)) <= 128 THEN name END AS name,
        CASE WHEN length(CAST(type AS BLOB)) <= 128 THEN type END AS type, "notnull", pk FROM pragma_table_info(?)
      ORDER BY cid LIMIT ?`,
      )
      .all(family.table, family.columns.length + 1);
    if (
      columns.length !== family.columns.length ||
      columns.some(
        (column, index) =>
          `${String(column.name)}:${String(column.type)}:${String(column.notnull)}:${String(column.pk)}` !==
          family.columns[index],
      )
    )
      inventoryRefuse("inventory_schema_inconsistent");
  }
};
