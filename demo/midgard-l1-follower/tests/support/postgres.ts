import { randomBytes } from "node:crypto";

import pg from "pg";

const admin = {
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  user: process.env.POSTGRES_USER ?? "postgres",
  password: process.env.POSTGRES_PASSWORD ?? "postgres",
};

const withAdmin = async <T>(
  run: (client: pg.Client) => Promise<T>,
): Promise<T> => {
  const client = new pg.Client({
    ...admin,
    database: process.env.POSTGRES_DB ?? "postgres",
  });
  await client.connect();
  try {
    return await run(client);
  } finally {
    await client.end();
  }
};

export type TestDatabase = Readonly<{ name: string; url: string }>;

/**
 * Fresh databases on the workspace test cluster (127.0.0.1:5433,
 * `scripts/start-test-postgres.sh`), named under the per-worktree
 * `MIDGARD_TEST_DATABASE_PREFIX`. Fails closed when the cluster is down.
 */
export const testDatabases = () => {
  const created: string[] = [];
  const prefix = (
    process.env.MIDGARD_TEST_DATABASE_PREFIX ?? "midgard_test_l1_follower"
  )
    .toLowerCase()
    .replace(/[^a-z0-9_]/gu, "_");
  return {
    create: async (): Promise<TestDatabase> => {
      const name = `${prefix}_${randomBytes(6).toString("hex")}`;
      await withAdmin((client) => client.query(`CREATE DATABASE ${name}`));
      created.push(name);
      return {
        name,
        url: `postgresql://${admin.user}:${admin.password}@${admin.host}:${String(admin.port)}/${name}`,
      };
    },
    dropAll: async (): Promise<void> => {
      if (created.length === 0) return;
      await withAdmin(async (client) => {
        for (const name of created.splice(0))
          await client.query(`DROP DATABASE IF EXISTS ${name} WITH (FORCE)`);
      });
    },
  };
};
