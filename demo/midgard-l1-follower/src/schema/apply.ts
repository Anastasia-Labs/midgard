import { createHash } from "node:crypto";

import { asString, type SqlBackend, type SqlTx } from "../sql/backend.js";
import { schemaTables } from "./lint.js";

/**
 * One namespace's schema: the text that creates its tables on an empty
 * database. The follower's own is `l1-follower`; a role adds its D-t tables
 * under its own namespace. The ledger table is separate from every role's
 * own, so the follower's tables can live in a role's existing database
 * without colliding.
 */
export type SchemaSet = Readonly<{ namespace: string; sql: string }>;

export class FollowerSchemaError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "FollowerSchemaError";
  }
}

/**
 * The follower's bookkeeping, created before any schema and outside the
 * catalog, so `follower reset --to-origin` never deletes it: the schema
 * ledger (each created namespace's checksum), the table catalog (each
 * table's declared class, which reset reads), the writer row (the lease's
 * fencing epoch and the generation the next `initialize` starts at), and
 * the tracked-set record (the protocol tracked set the facts were built
 * under, whether a store reset is still replaying, and the cursor height it
 * replays to; `store/tracked-set-record.ts`).
 */
export const FOLLOWER_BOOKKEEPING_DDL = `
-- class: A; retention: one row per created schema, forever; reset keeps it
CREATE TABLE IF NOT EXISTS l1_follower_schemas (
  namespace text PRIMARY KEY,
  checksum text NOT NULL
);

-- class: A; retention: one row per created table, forever; reset keeps it
CREATE TABLE IF NOT EXISTS l1_follower_tables (
  table_name text PRIMARY KEY,
  table_class text NOT NULL,
  namespace text NOT NULL
);

-- class: A; retention: one row forever; reset keeps it
CREATE TABLE IF NOT EXISTS l1_follower_writer (
  id integer PRIMARY KEY CHECK (id = 1),
  writer_epoch bigint NOT NULL,
  next_generation bigint NOT NULL
);

INSERT INTO l1_follower_writer (id, writer_epoch, next_generation)
  SELECT 1, 0, 0 WHERE NOT EXISTS (SELECT 1 FROM l1_follower_writer);

-- class: A; retention: one row forever; reset keeps it (and rewrites it on a tracked-set reset)
CREATE TABLE IF NOT EXISTS l1_follower_tracked_set (
  id integer PRIMARY KEY CHECK (id = 1),
  addresses text NOT NULL,
  payment_credentials text NOT NULL,
  policies text NOT NULL,
  replaying integer NOT NULL,
  replay_height bigint
);
`;

const checksum = (sql: string): string =>
  createHash("sha256").update(sql).digest("hex");

/** Records each table a schema declares, with its class, in the catalog. */
const catalogTables = async (tx: SqlTx, set: SchemaSet): Promise<void> => {
  const { declared, problems } = schemaTables(set.namespace, set.sql);
  const problem = problems[0];
  if (problem !== undefined)
    throw new FollowerSchemaError(
      `schema ${set.namespace}${
        problem.table === null ? "" : ` table ${problem.table}`
      }: ${problem.message}`,
    );
  for (const table of declared)
    await tx.query(
      "INSERT INTO l1_follower_tables (table_name, table_class, namespace) VALUES (?, ?, ?)",
      [table.table, table.tableClass, set.namespace],
    );
};

/**
 * Creates each set's schema the database does not hold yet, in one write
 * transaction (on Postgres under a transaction-scoped advisory lock, so two
 * processes never create one concurrently), and records its checksum. A
 * schema the database holds under a different checksum was created by
 * another text, and is refused: such a database is recreated, never
 * altered. A schema with a table that lacks its class header is refused, and
 * every declared table is recorded in the catalog.
 */
export const applySchemas = async (
  backend: SqlBackend,
  sets: readonly SchemaSet[],
): Promise<void> =>
  backend.transaction("write", async (tx) => {
    if (backend.dialect.name === "postgres")
      await tx.query(
        "SELECT pg_advisory_xact_lock(hashtext('midgard-l1-follower:schemas'))",
      );
    await tx.exec(FOLLOWER_BOOKKEEPING_DDL);
    for (const set of sets) {
      const digest = checksum(set.sql);
      const existing = (
        await tx.query(
          "SELECT checksum FROM l1_follower_schemas WHERE namespace = ?",
          [set.namespace],
        )
      )[0];
      if (existing !== undefined) {
        if (asString(existing.checksum) !== digest)
          throw new FollowerSchemaError(
            `schema ${set.namespace}: this database was created by a different schema; recreate it (drop it and create it empty)`,
          );
        continue;
      }
      await catalogTables(tx, set);
      await tx.exec(set.sql);
      await tx.query(
        "INSERT INTO l1_follower_schemas (namespace, checksum) VALUES (?, ?)",
        [set.namespace, digest],
      );
    }
  });
