import { realpathSync, statSync } from "node:fs";
import { isAbsolute, normalize } from "node:path";
import { performance } from "node:perf_hooks";
import { DatabaseSync } from "node:sqlite";

import {
  type InventoryRow,
  inventorySelection,
  projectInventoryRow,
} from "./availability-operation-journal-inventory.projection.js";
import {
  AvailabilityJournalInventoryError,
  inventoryFamilies,
  type InventoryFamily,
  inventoryRefuse,
  validateInventorySchema,
} from "./availability-operation-journal-inventory.schema.js";

export type { InventoryRow } from "./availability-operation-journal-inventory.projection.js";
export type { InventoryFamily } from "./availability-operation-journal-inventory.schema.js";
export { AvailabilityJournalInventoryError } from "./availability-operation-journal-inventory.schema.js";

export type AvailabilityJournalInventoryLimits = Readonly<{
  pageRows: number;
  totalRows: number;
  outputBytes: number;
  fieldBytes: number;
  /** Operator allocation ceiling, not a protocol transaction maximum. */
  recordBytes: number;
  lifetimeMs: number;
}>;
/** Fixed ceilings keep inspection memory, output and read-snapshot retention bounded. */
export const AVAILABILITY_JOURNAL_INVENTORY_LIMITS: AvailabilityJournalInventoryLimits =
  Object.freeze({
    pageRows: 64,
    totalRows: 100_000,
    outputBytes: 16 * 1024 * 1024,
    fieldBytes: 1024,
    recordBytes: 256 * 1024,
    lifetimeMs: 60_000,
  });
export type AvailabilityJournalInventoryPage = Readonly<{
  event: "availability_journal_inventory_page";
  schema: 3;
  sequence: number;
  family: InventoryFamily;
  rows: readonly InventoryRow[];
}>;
export type AvailabilityJournalInventoryFooter = Readonly<{
  event: "availability_journal_inventory_complete";
  schema: 3;
  scope: "stored_journal_rows";
  authority: "stored_unreobserved";
  complete: boolean;
  code: string;
  rows: number;
  pages: number;
  familyRows: Readonly<Record<InventoryFamily, number>>;
}>;
export interface AvailabilityJournalInventory {
  /** All pages share one private SQLite read transaction; cursors cannot be resumed. */
  nextPage():
    | AvailabilityJournalInventoryPage
    | AvailabilityJournalInventoryFooter;
  /** Closing before the footer makes inspection incomplete. Does not mutate the journal. */
  close(): void;
}

/** Existing canonical file only; ordinary read-only SQLite observes live WAL. */
export const openAvailabilityJournalInventory = (
  path: string,
  requested: Partial<AvailabilityJournalInventoryLimits> = {},
): AvailabilityJournalInventory => {
  if (
    Object.keys(requested).some(
      (key) =>
        !Object.keys(AVAILABILITY_JOURNAL_INVENTORY_LIMITS).includes(key),
    )
  )
    inventoryRefuse("inventory_limit_invalid");
  const limits = { ...AVAILABILITY_JOURNAL_INVENTORY_LIMITS, ...requested };
  for (const key of Object.keys(
    limits,
  ) as (keyof AvailabilityJournalInventoryLimits)[])
    if (
      !Number.isSafeInteger(limits[key]) ||
      limits[key] <= 0 ||
      limits[key] > AVAILABILITY_JOURNAL_INVENTORY_LIMITS[key]
    )
      inventoryRefuse("inventory_limit_invalid");
  // Reserve a fixed bounded footer even after row/output exhaustion.
  if (limits.outputBytes < 1024) inventoryRefuse("inventory_limit_invalid");
  if (!isAbsolute(path) || normalize(path) !== path)
    inventoryRefuse("inventory_path_invalid");
  try {
    if (realpathSync(path) !== path || !statSync(path).isFile())
      inventoryRefuse("inventory_path_invalid");
  } catch (error) {
    if (error instanceof AvailabilityJournalInventoryError) throw error;
    inventoryRefuse("inventory_existing_file_unavailable");
  }
  const deadline = performance.now() + limits.lifetimeMs;
  let db: DatabaseSync;
  try {
    db = new DatabaseSync(path, { readOnly: true, allowExtension: false });
  } catch {
    return inventoryRefuse("inventory_readonly_access_failed");
  }
  try {
    db.exec("PRAGMA busy_timeout = 1000; BEGIN;");
    validateInventorySchema(db);
  } catch (error) {
    db.close();
    if (error instanceof AvailabilityJournalInventoryError) throw error;
    return inventoryRefuse("inventory_schema_read_failed");
  }
  let familyIndex = 0;
  let cursor: readonly string[] | undefined;
  let rows = 0;
  let pages = 0;
  let bytes = 0;
  const familyRows: Record<InventoryFamily, number> = {
    metadata: 0,
    leases: 0,
    intents: 0,
    resources: 0,
    dependencies: 0,
    workflows: 0,
  };
  let footer: AvailabilityJournalInventoryFooter | undefined;
  const finish = (
    complete: boolean,
    code: string,
  ): AvailabilityJournalInventoryFooter => {
    if (footer !== undefined) return footer;
    clearTimeout(lifetimeTimer);
    // Closing the connection ends the read transaction even if ROLLBACK fails.
    try {
      db.exec("ROLLBACK");
    } catch {
      complete = false;
      code = "inventory_cleanup_failed";
    }
    try {
      db.close();
    } catch {
      complete = false;
      code = "inventory_cleanup_failed";
    }
    footer = {
      event: "availability_journal_inventory_complete",
      schema: 3,
      scope: "stored_journal_rows",
      authority: "stored_unreobserved",
      complete,
      code,
      rows,
      pages,
      familyRows: { ...familyRows },
    };
    return footer;
  };
  const nextPage = ():
    | AvailabilityJournalInventoryPage
    | AvailabilityJournalInventoryFooter => {
    if (footer !== undefined) return footer;
    if (performance.now() >= deadline)
      return finish(false, "inventory_lifetime_exceeded");
    try {
      while (familyIndex < inventoryFamilies.length) {
        if (performance.now() >= deadline)
          return finish(false, "inventory_lifetime_exceeded");
        const family = inventoryFamilies[familyIndex]!;
        const count = Math.min(limits.pageRows, limits.totalRows - rows);
        const keys = family.keys.map((key) => `r.${key}`).join(", ");
        const where =
          cursor === undefined
            ? ""
            : `WHERE (${keys}) > (${family.keys.map(() => "?").join(", ")})`;
        const statement =
          db.prepare(`SELECT ${inventorySelection(family.name, limits.fieldBytes, limits.recordBytes)}
          FROM ${family.table} r ${where} ORDER BY ${keys} LIMIT ?`);
        statement.setReadBigInts(true);
        const stored = statement.all(...(cursor ?? []), count + 1);
        if (performance.now() >= deadline)
          return finish(false, "inventory_lifetime_exceeded");
        if (stored.length === 0) {
          familyIndex++;
          cursor = undefined;
          continue;
        }
        if (count === 0) return finish(false, "inventory_row_limit_exceeded");
        const selected = stored.slice(0, count);
        const projected = selected.map((row) =>
          projectInventoryRow(family.name, row, limits.fieldBytes),
        );
        const page: AvailabilityJournalInventoryPage = {
          event: "availability_journal_inventory_page",
          schema: 3,
          sequence: pages + 1,
          family: family.name,
          rows: projected,
        };
        const pageBytes = Buffer.byteLength(JSON.stringify(page)) + 1;
        if (bytes + pageBytes > limits.outputBytes - 1024)
          return finish(false, "inventory_output_limit_exceeded");
        if (performance.now() >= deadline)
          return finish(false, "inventory_lifetime_exceeded");
        rows += selected.length;
        familyRows[family.name] += selected.length;
        pages++;
        bytes += pageBytes;
        if (stored.length > count) {
          const last = selected[selected.length - 1]!;
          cursor = family.keys.map((key) => {
            const value = last[key];
            if (typeof value !== "string")
              return inventoryRefuse("inventory_key_malformed");
            return value;
          });
        } else {
          familyIndex++;
          cursor = undefined;
        }
        return page;
      }
      return finish(true, "stored_rows_complete");
    } catch (error) {
      return finish(
        false,
        error instanceof AvailabilityJournalInventoryError
          ? error.code
          : "inventory_read_failed",
      );
    }
  };
  const lifetimeTimer = setTimeout(
    () => {
      finish(false, "inventory_lifetime_exceeded");
    },
    Math.max(1, deadline - performance.now()),
  );
  lifetimeTimer.unref();
  return {
    nextPage,
    close: () => {
      finish(false, "inventory_closed");
    },
  };
};
