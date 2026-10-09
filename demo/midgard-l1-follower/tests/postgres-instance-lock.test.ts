/**
 * The instance lock's bounds on one attempt, against servers that never
 * answer: an attempt ends at its connect or statement bound with an error
 * that is not `isInstanceLockHeldElsewhere`, so its holder reports the lock
 * as unavailable and tries again, instead of waiting for the operating
 * system's TCP timeout.
 */
import { once } from "node:events";
import {
  type AddressInfo,
  createServer,
  type Server,
  type Socket,
} from "node:net";

import { afterEach, describe, expect, it } from "vitest";

import {
  isInstanceLockHeldElsewhere,
  PostgresInstanceLock,
  type PostgresInstanceLockBounds,
  type PostgresInstanceLockIdentity,
} from "../src/index.js";

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
