import { randomBytes } from "node:crypto";

import { Client } from "pg";

const postgresAdmin = {
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  user: process.env.POSTGRES_USER ?? "postgres",
  password: process.env.POSTGRES_PASSWORD ?? "postgres",
};

const withAdmin = async <T>(run: (admin: Client) => Promise<T>): Promise<T> => {
  const admin = new Client({
    ...postgresAdmin,
    database: process.env.POSTGRES_DB ?? "postgres",
  });
  await admin.connect();
  try {
    return await run(admin);
  } finally {
    await admin.end();
  }
};

export type PostgresTestDatabase = {
  readonly name: string;
  readonly url: string;
};

/**
 * Fresh databases on the workspace test cluster
 * (`scripts/start-test-postgres.sh`), named under
 * `MIDGARD_TEST_DATABASE_PREFIX`; fails closed when none is reachable. Call
 * `dropAll` once the suite is done.
 */
export const postgresTestDatabases = (fallbackPrefix: string) => {
  const created: string[] = [];
  const prefix = (process.env.MIDGARD_TEST_DATABASE_PREFIX ?? fallbackPrefix)
    .toLowerCase()
    .replace(/[^a-z0-9_]/gu, "_");
  return {
    create: async (): Promise<PostgresTestDatabase> => {
      const name = `${prefix}_${randomBytes(6).toString("hex")}`;
      await withAdmin(async (admin) => {
        await admin.query(`CREATE DATABASE ${name}`);
      });
      created.push(name);
      return {
        name,
        url: `postgresql://${postgresAdmin.user}:${postgresAdmin.password}@${postgresAdmin.host}:${postgresAdmin.port.toString()}/${name}`,
      };
    },
    dropAll: async (): Promise<void> => {
      if (created.length === 0) return;
      await withAdmin(async (admin) => {
        for (const name of created.splice(0)) {
          await admin.query(`DROP DATABASE IF EXISTS ${name} WITH (FORCE)`);
        }
      });
    },
  };
};

/**
 * Ends, at the server, the session holding a committee store's instance lock
 * on `database`, as the server does when the process holding it dies.
 * Returns how many sessions it ended.
 */
export const terminateInstanceLockSessions = async (
  database: PostgresTestDatabase,
): Promise<number> => {
  const result = await withAdmin(async (admin) =>
    admin.query<{ readonly terminated: boolean }>(
      `SELECT pg_terminate_backend(l.pid) AS terminated
       FROM pg_locks l
       JOIN pg_database d ON d.oid = l.database
       WHERE l.locktype = 'advisory' AND l.granted AND d.datname = $1`,
      [database.name],
    ),
  );
  return result.rows.filter(({ terminated }) => terminated).length;
};
