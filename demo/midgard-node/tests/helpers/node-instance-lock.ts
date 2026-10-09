/**
 * The node's instance lock (`node-instance-lock.ts`) over this worker's
 * database, for its tests: taking it in a scope of its own, ending its
 * sessions from another session, a frozen connection, and a stepped retry
 * sleep.
 */
import { once } from "node:events";
import { createServer } from "node:http";
import {
  type AddressInfo,
  connect,
  createServer as createTcpServer,
  type Socket,
} from "node:net";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Exit, Fiber, Redacted, Ref, Scope } from "effect";

import { nodeDatabaseConnectionString } from "../../src/services/l1-provider.js";
import { HaltSource } from "../../src/services/liveness-halt.js";
import {
  acquireNodeInstanceLock,
  type AcquireNodeInstanceLockOptions,
  type NodeInstanceLock,
} from "../../src/services/node-instance-lock.js";

const env = (name: string): string => {
  const value = process.env[name];
  if (value === undefined || value === "")
    throw new Error(`${name} is not set`);
  return value;
};

export const database = {
  POSTGRES_HOST: env("POSTGRES_HOST"),
  POSTGRES_PORT: Number(env("POSTGRES_PORT")),
  POSTGRES_USER: env("POSTGRES_USER"),
  POSTGRES_PASSWORD: env("POSTGRES_PASSWORD"),
  POSTGRES_DB: env("POSTGRES_DB"),
};
export const connectionString = nodeDatabaseConnectionString(database);

export const livenessGlobals = () => ({
  LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
});

export const reasonOf = (globals: ReturnType<typeof livenessGlobals>) =>
  Effect.runSync(Ref.get(globals.LIVENESS_REASONS)).get(
    HaltSource.instanceLock,
  );

export const scopes: Scope.CloseableScope[] = [];
export const fibers: Fiber.RuntimeFiber<unknown, unknown>[] = [];

/** Interrupts the fibers and closes the scopes a test left in `fibers` and
 * `scopes`. */
export const cleanUpLocks = async () => {
  for (const fiber of fibers.splice(0))
    await Effect.runPromise(Fiber.interrupt(fiber));
  for (const scope of scopes.splice(0))
    await Effect.runPromise(Scope.close(scope, Exit.void));
};

export type LockOptions = Partial<AcquireNodeInstanceLockOptions> & {
  readonly waited?: string[][];
};

/** Takes the lock in a scope of its own; resolves once it is held. */
export const acquire = async (
  options: LockOptions = {},
): Promise<{ lock: NodeInstanceLock; scope: Scope.CloseableScope }> => {
  const scope = Effect.runSync(Scope.make());
  scopes.push(scope);
  const lock = await Effect.runPromise(
    acquireNodeInstanceLock({
      connectionString,
      globals: livenessGlobals(),
      waiting: (reasons) =>
        Effect.sync(() => options.waited?.push([...reasons])),
      retryInitialMs: 20,
      retryMaxMs: 50,
      ...options,
    }).pipe(Scope.extend(scope)),
  );
  return { lock, scope };
};

export const release = (scope: Scope.CloseableScope) =>
  Effect.runPromise(Scope.close(scope, Exit.void));

/** Ends, from another session, every session holding an advisory lock here. */
export const endLockSessions = () =>
  Effect.runPromise(
    Effect.provide(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql<{ ended: boolean }>`
          SELECT pg_terminate_backend(pid) AS ended FROM pg_locks
          WHERE locktype = 'advisory' AND granted
            AND database = (
              SELECT oid FROM pg_database WHERE datname = current_database()
            )`,
      ),
      PgClient.layer({ url: Redacted.make(connectionString) }),
    ),
  );

/** A local port nothing listens on. */
export const closedPort = async (): Promise<number> => {
  const server = createServer();
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  await new Promise<void>((resolve, reject) =>
    server.close((error) => (error ? reject(error) : resolve())),
  );
  return port;
};

/**
 * A TCP proxy to the test Postgres. `freezeFirst` stops forwarding on the
 * first connection it took without closing either side, as a connection
 * broken without a close looks to its client; the others are forwarded.
 * `close` destroys every connection.
 */
export const freezingProxy = async () => {
  const pairs: { client: Socket; upstream: Socket }[] = [];
  const server = createTcpServer((client) => {
    const upstream = connect(database.POSTGRES_PORT, database.POSTGRES_HOST);
    client.on("error", () => undefined);
    upstream.on("error", () => undefined);
    client.pipe(upstream);
    upstream.pipe(client);
    pairs.push({ client, upstream });
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  return {
    port: (server.address() as AddressInfo).port,
    connections: () => pairs.length,
    freezeFirst: () => {
      const pair = pairs[0];
      if (pair === undefined) throw new Error("the proxy took no connection");
      pair.client.unpipe(pair.upstream);
      pair.upstream.unpipe(pair.client);
      pair.client.pause();
      pair.upstream.pause();
    },
    close: async () => {
      for (const { client, upstream } of pairs) {
        client.destroy();
        upstream.destroy();
      }
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};

/** A sleep the test lets go of, one wake-up at a time. */
export const steppedSleep = () => {
  const sleepers: (() => void)[] = [];
  return {
    timers: {
      sleep: () =>
        new Promise<void>((resolve) => {
          sleepers.push(resolve);
        }),
    },
    sleeping: () => sleepers.length,
    wake: () => sleepers.shift()?.(),
  };
};
