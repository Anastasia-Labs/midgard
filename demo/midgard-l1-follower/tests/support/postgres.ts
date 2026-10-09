import { randomBytes } from "node:crypto";
import { once } from "node:events";
import { type AddressInfo, connect, createServer, type Socket } from "node:net";

import pg from "pg";

const admin = {
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  user: process.env.POSTGRES_USER ?? "postgres",
  password: process.env.POSTGRES_PASSWORD ?? "postgres",
};

export const withAdmin = async <T>(
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

/**
 * A TCP proxy in front of the test cluster for one database. `freeze()`
 * leaves every connection open now half-open: from then on nothing is
 * forwarded either way and a close from the server is not passed on, so a
 * client sees no end while the server has ended its session.
 */
export const freezableProxy = async (
  database: TestDatabase,
): Promise<{
  url: string;
  freeze: () => void;
  /** Sockets of either side not yet closed. */
  openSockets: () => number;
  close: () => Promise<void>;
}> => {
  const frozen = new WeakSet<Socket>();
  const open = new Set<Socket>();
  const server = createServer((downstream) => {
    const upstream = connect(admin.port, admin.host);
    open.add(downstream);
    open.add(upstream);
    const forward = (from: Socket, to: Socket): void => {
      from.on("data", (chunk: Buffer) => {
        if (!frozen.has(from)) to.write(chunk);
      });
      from.on("close", () => {
        open.delete(from);
        if (!frozen.has(from)) to.destroy();
      });
      from.on("error", () => undefined);
    };
    forward(downstream, upstream);
    forward(upstream, downstream);
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const { port } = server.address() as AddressInfo;
  return {
    url: `postgresql://${admin.user}:${admin.password}@127.0.0.1:${String(port)}/${database.name}`,
    freeze: () => {
      for (const socket of open) frozen.add(socket);
    },
    openSockets: () => open.size,
    close: async () => {
      for (const socket of open) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};

/** The pids holding a granted advisory lock in `database`. */
export const advisoryLockHolders = (
  database: TestDatabase,
): Promise<number[]> =>
  withAdmin(async (client) =>
    (
      await client.query<{ pid: number }>(
        `SELECT DISTINCT l.pid FROM pg_locks l
           JOIN pg_database d ON d.oid = l.database
          WHERE l.locktype = 'advisory' AND l.granted AND d.datname = $1`,
        [database.name],
      )
    ).rows.map((row) => row.pid),
  );

/** Terminates `pid`'s session and waits until its locks are gone. */
export const terminateLockHolder = async (
  database: TestDatabase,
  pid: number,
): Promise<void> => {
  await withAdmin((client) =>
    client.query("SELECT pg_terminate_backend($1)", [pid]),
  );
  const deadline = Date.now() + 10_000;
  while ((await advisoryLockHolders(database)).includes(pid)) {
    if (Date.now() > deadline)
      throw new Error(`session ${String(pid)} still holds its locks`);
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
};
