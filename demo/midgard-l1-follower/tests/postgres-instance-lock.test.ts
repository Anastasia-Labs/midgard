/**
 * The instance lock's bounds on one attempt, against servers that never
 * answer: an attempt ends at its connect or statement bound with an error
 * that is not `isInstanceLockHeldElsewhere`, so its holder reports the lock
 * as unavailable and tries again, instead of waiting for the operating
 * system's TCP timeout. Then, on the test cluster, a lock whose session the
 * server ended: suspended under a named refusal, and taken again; and a lock
 * still held: never reported lost.
 */
import { once } from "node:events";
import {
  type AddressInfo,
  createServer,
  type Server,
  type Socket,
} from "node:net";

import pg from "pg";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  isInstanceLockHeldElsewhere,
  PostgresInstanceLock,
  type PostgresInstanceLockBounds,
  type PostgresInstanceLockIdentity,
  type PostgresInstanceLockTimers,
} from "../src/index.js";
import {
  advisoryLockHolders,
  freezableProxy,
  terminateLockHolder,
  type TestDatabase,
  testDatabases,
} from "./support/postgres.js";

const IDENTITY: PostgresInstanceLockIdentity = {
  keyName: "instance-lock-bounds-test:",
  messages: {
    heldElsewhere: "held elsewhere",
    heldByOwnStaleSession: "held by own stale session",
    suspended: "suspended",
    passive: "passive",
    lostAtServer: "lost at server",
  },
};

/** Generous against a loaded machine, far below any TCP timeout. */
const WITHIN_MS = 3_000;

const servers: Server[] = [];
const sockets: Socket[] = [];

afterEach(async () => {
  for (const socket of sockets.splice(0)) socket.destroy();
  for (const server of servers.splice(0))
    await new Promise<void>((resolve) => server.close(() => resolve()));
});

/** A local server that accepts TCP and then runs `onConnection`. */
const listen = async (
  onConnection: (socket: Socket) => void,
): Promise<string> => {
  const server = createServer((socket) => {
    sockets.push(socket);
    onConnection(socket);
  });
  servers.push(server);
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const { port } = server.address() as AddressInfo;
  return `postgres://u:p@127.0.0.1:${port.toString()}/db`;
};

/** Answers the startup message as a trusting server would, then no more. */
const answerStartupOnly = (socket: Socket): void => {
  socket.once("data", () => {
    const authenticationOk = Buffer.from([0x52, 0, 0, 0, 8, 0, 0, 0, 0]);
    const readyForQuery = Buffer.from([0x5a, 0, 0, 0, 5, 0x49]);
    socket.write(Buffer.concat([authenticationOk, readyForQuery]));
  });
};

/** The attempt's error and how long it took to fail. */
const attempt = async (
  databaseUrl: string,
  bounds: PostgresInstanceLockBounds,
): Promise<{ error: unknown; elapsedMs: number }> => {
  const startedAt = Date.now();
  const outcome = await Promise.race([
    PostgresInstanceLock.acquire(IDENTITY, databaseUrl, {}, undefined, bounds)
      .then(async (lock) => {
        await lock.release();
        return { taken: true as const };
      })
      .catch((error: unknown) => ({ error })),
    new Promise<{ pending: true }>((resolve) =>
      setTimeout(() => resolve({ pending: true }), WITHIN_MS).unref(),
    ),
  ]);
  if (!("error" in outcome))
    throw new Error(
      `expected the attempt to fail within ${WITHIN_MS.toString()} ms, got ${JSON.stringify(outcome)}`,
    );
  return { error: outcome.error, elapsedMs: Date.now() - startedAt };
};

describe("the instance lock's attempt bounds", () => {
  it("ends an attempt on an unroutable address at its connect bound", async () => {
    // TEST-NET-1 (RFC 5737) is never routed: the connect is neither
    // answered nor refused.
    const { error, elapsedMs } = await attempt(
      "postgres://u:p@192.0.2.1:5432/db",
      { connectTimeoutMs: 200 },
    );
    expect(isInstanceLockHeldElsewhere(error)).toBe(false);
    expect(elapsedMs).toBeLessThan(WITHIN_MS);
  });

  it("ends an attempt whose server accepts TCP and never answers at its connect bound", async () => {
    const url = await listen(() => undefined);
    const { error, elapsedMs } = await attempt(url, { connectTimeoutMs: 200 });
    expect(isInstanceLockHeldElsewhere(error)).toBe(false);
    expect(elapsedMs).toBeLessThan(WITHIN_MS);
  });

  it("ends an attempt whose server stops answering after the startup at its statement bound", async () => {
    const url = await listen(answerStartupOnly);
    const { error, elapsedMs } = await attempt(url, {
      connectTimeoutMs: 60_000,
      statementTimeoutMs: 200,
    });
    expect(isInstanceLockHeldElsewhere(error)).toBe(false);
    expect(String(error)).toMatch(/timeout/iu);
    expect(elapsedMs).toBeLessThan(WITHIN_MS);
  });
});

const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

/** Every lock event, in order, by name and message. */
const recorder = () => {
  const seen: string[] = [];
  return {
    seen,
    events: {
      onInstanceLockSuspended: (error: Error) =>
        seen.push(`suspended: ${error.message}`),
      onInstanceLockHeldElsewhere: (error: Error) =>
        seen.push(`held elsewhere: ${error.message}`),
      onInstanceLockRestored: () => seen.push("restored"),
    },
  };
};

/** Reacquire waits on the gate until the test opens it. */
const gatedTimers = (): {
  timers: PostgresInstanceLockTimers;
  open: () => void;
} => {
  let open = (): void => undefined;
  const gate = new Promise<void>((resolve) => {
    open = resolve;
  });
  return { timers: { sleep: () => gate }, open };
};

const until = async (what: string, holds: () => boolean): Promise<void> => {
  const deadline = Date.now() + 10_000;
  while (!holds()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};

const withClient = async <T>(
  database: TestDatabase,
  run: (client: pg.Client) => Promise<T>,
): Promise<T> => {
  const client = new pg.Client({ connectionString: database.url });
  await client.connect();
  try {
    return await run(client);
  } finally {
    await client.end();
  }
};

const soleHolder = async (database: TestDatabase): Promise<number> => {
  const holders = await advisoryLockHolders(database);
  expect(holders).toHaveLength(1);
  return holders[0]!;
};

describe("the instance lock on Postgres", () => {
  it("suspends a lock the server no longer holds for a session still open on this side, and takes it again", async () => {
    const database = await databases.create();
    const proxy = await freezableProxy(database);
    const { seen, events } = recorder();
    const gate = gatedTimers();
    const lock = await PostgresInstanceLock.acquire(
      IDENTITY,
      proxy.url,
      events,
      gate.timers,
    );
    try {
      const lease = lock.followerWriterLease()!;
      expect(lease.lost()).toBe(false);
      const pid = await soleHolder(database);
      // The server ends the session; this side's connection stays open and
      // hears nothing.
      proxy.freeze();
      await terminateLockHolder(database, pid);
      expect(seen).toEqual([]);
      await withClient(database, async (client) => {
        await expect(lock.assertHeldAtServer(client)).rejects.toThrow(
          "lost at server",
        );
      });
      // Named, refused, and the stale connection let go of.
      expect(seen).toEqual(["suspended: lost at server"]);
      expect(() => lock.assertHeld()).toThrow("lost at server");
      expect(lock.followerWriterLease()).toBeNull();
      expect(lease.lost()).toBe(true);
      expect(lease.refusal?.()).toBe("lost at server");
      await until(
        "the stale connection closed",
        () => proxy.openSockets() === 0,
      );
      gate.open();
      await until("the lock taken again", () => seen.includes("restored"));
      expect(seen).toEqual(["suspended: lost at server", "restored"]);
      expect(() => lock.assertHeld()).not.toThrow();
      expect(lock.followerWriterLease()?.lost()).toBe(false);
      expect(await soleHolder(database)).not.toBe(pid);
      await withClient(database, (client) => lock.assertHeldAtServer(client));
    } finally {
      await lock.release();
      await proxy.close();
    }
  });

  it("suspends a lock whose session ended with a close, and takes it again", async () => {
    const database = await databases.create();
    const { seen, events } = recorder();
    const gate = gatedTimers();
    const lock = await PostgresInstanceLock.acquire(
      IDENTITY,
      database.url,
      events,
      gate.timers,
    );
    try {
      const pid = await soleHolder(database);
      await terminateLockHolder(database, pid);
      await until("the suspension", () => seen.length > 0);
      expect(seen).toEqual(["suspended: suspended"]);
      expect(() => lock.assertHeld()).toThrow("suspended");
      expect(lock.followerWriterLease()).toBeNull();
      gate.open();
      await until("the lock taken again", () => seen.includes("restored"));
      expect(seen).toEqual(["suspended: suspended", "restored"]);
      expect(() => lock.assertHeld()).not.toThrow();
    } finally {
      await lock.release();
    }
  });

  it("never reports a held lock lost, while other sessions end around it", async () => {
    const database = await databases.create();
    const proxy = await freezableProxy(database);
    const { seen, events } = recorder();
    const lock = await PostgresInstanceLock.acquire(
      IDENTITY,
      proxy.url,
      events,
    );
    try {
      const pid = await soleHolder(database);
      const lease = lock.followerWriterLease()!;
      for (let round = 0; round < 5; round += 1) {
        // A session of another kind, ended by the server under the lock.
        const other = new pg.Client({ connectionString: database.url });
        other.on("error", () => undefined);
        await other.connect();
        await withClient(database, (client) => lock.assertHeldAtServer(client));
        const otherPid = (
          await other.query<{ pid: number }>("SELECT pg_backend_pid() AS pid")
        ).rows[0]!.pid;
        await withClient(database, (client) =>
          client.query("SELECT pg_terminate_backend($1)", [otherPid]),
        );
        await other.end().catch(() => undefined);
        await withClient(database, (client) => lock.assertHeldAtServer(client));
      }
      expect(seen).toEqual([]);
      expect(() => lock.assertHeld()).not.toThrow();
      expect(lease.lost()).toBe(false);
      expect(lease.refusal?.()).toBeUndefined();
      expect(await soleHolder(database)).toBe(pid);
    } finally {
      await lock.release();
      await proxy.close();
    }
  });
});
