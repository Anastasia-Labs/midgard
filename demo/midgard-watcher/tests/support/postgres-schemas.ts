/**
 * Watcher stores on the workspace Postgres test cluster (127.0.0.1:5433,
 * `scripts/start-test-postgres.sh`): each in a fresh schema of the cluster's
 * default database, named under the per-worktree
 * `MIDGARD_TEST_DATABASE_PREFIX`. The follower's writer lease is keyed by
 * schema, so stores in sibling schemas never contend. Fails closed when the
 * cluster is down.
 */
import { randomBytes } from "node:crypto";

import {
  type FactStore,
  type FactStoreOptions,
  openPostgresBackend,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";

const host = process.env.POSTGRES_HOST ?? "127.0.0.1";
const port = process.env.POSTGRES_PORT ?? "5433";
const user = process.env.POSTGRES_USER ?? "postgres";
const password = process.env.POSTGRES_PASSWORD ?? "postgres";
const database = process.env.POSTGRES_DB ?? "postgres";
const base = `postgresql://${user}:${password}@${host}:${port}/${database}`;

const prefix = (
  process.env.MIDGARD_TEST_DATABASE_PREFIX ?? "midgard_test_watcher"
)
  .toLowerCase()
  .replace(/[^a-z0-9_]/gu, "_");

const onAdmin = async (sql: string): Promise<void> => {
  const admin = openPostgresBackend({ connectionString: base });
  try {
    await admin.transaction("write", (tx) => tx.query(sql));
  } finally {
    await admin.close();
  }
};

export type StoreOpener = Readonly<{
  dialect: "postgres";
  store: (options: FactStoreOptions) => FactStore;
}>;

export const postgresSchemas = () => {
  const created: string[] = [];
  return {
    /** A fresh schema, and an opener of stores whose search path is it. */
    open: async (): Promise<StoreOpener> => {
      const name = `${prefix}_${randomBytes(6).toString("hex")}`;
      await onAdmin(`CREATE SCHEMA ${name}`);
      created.push(name);
      const connectionString = `${base}?options=${encodeURIComponent(`-c search_path=${name}`)}`;
      return {
        dialect: "postgres",
        store: (options) =>
          openPostgresFactStore({
            ...options,
            connection: { connectionString },
          }),
      };
    },
    dropAll: async (): Promise<void> => {
      for (const name of created.splice(0))
        await onAdmin(`DROP SCHEMA IF EXISTS ${name} CASCADE`);
    },
  };
};
