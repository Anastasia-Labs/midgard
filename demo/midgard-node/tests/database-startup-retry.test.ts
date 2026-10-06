import { createServer, type Server, Socket } from "node:net";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Data, Duration, Effect, Fiber, Layer, Logger, Redacted } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { assertCompatibleWithStartupRetry } from "../src/database/init.js";
import { MigrationError } from "../src/database/migrations/runner.js";
import { isConnectionClassError } from "../src/provider-retry.js";
import {
  type DatabaseStartupRetryOptions,
  retryDatabaseConnectionAtStartup,
} from "../src/services/database.js";
import { databaseUpstreamSocket } from "../src/services/database-upstream-socket.js";

// The same variables global-setup and the node read: CI serves Postgres on
// 5432 through POSTGRES_PORT, a local checkout on 5433.
const PG_HOST = process.env.POSTGRES_HOST ?? "127.0.0.1";
const PG_PORT = Number(process.env.POSTGRES_PORT ?? "5433");
const PG_USER = process.env.POSTGRES_USER ?? "postgres";
const PG_PASSWORD = process.env.POSTGRES_PASSWORD ?? "postgres";

const FAST = {
  baseDelay: Duration.millis(5),
  maxDelay: Duration.millis(20),
  budget: Duration.seconds(20),
} as const;

class PoolInitError extends Data.TaggedError("PoolInitError")<{
  readonly cause: unknown;
}> {}

const servers: Server[] = [];
// server.close waits for every connection, and nothing ends a stalled one.
const silentSockets: Socket[] = [];
afterEach(async () => {
  silentSockets.splice(0).forEach((socket) => socket.destroy());
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
});

/** The ErrorResponse a Postgres still replaying its WAL sends at startup. */
const startingUpResponse = () => {
  const fields = Buffer.from(
    "SFATAL\0VFATAL\0C57P03\0Mthe database system is starting up\0\0",
  );
  const header = Buffer.alloc(5);
  header.write("E", 0);
  header.writeInt32BE(fields.length + 4, 1);
  return Buffer.concat([header, fields]);
};

const listen = async (server: Server) => {
  await new Promise<void>((resolve) =>
    server.listen(0, "127.0.0.1", () => resolve()),
  );
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("proxy has no port");
  }
  return address.port;
};

/**
 * How the proxy answers a connection: forward it to the test Postgres,
 * refuse it the way a restarting Postgres does (57P03), accept it and never
 * answer (a stalled host), or accept it and close it before answering (a
 * docker userland proxy in front of a dead Postgres).
 */
type ProxyAnswer = "forward" | "starting_up" | "stall" | "close";

/** A TCP proxy to the test Postgres answering connection `n` (from 1). */
const startProxy = async (answer: (n: number) => ProxyAnswer) => {
  let accepted = 0;
  const clients = new Set<Socket>();
  const server = createServer((client) => {
    accepted += 1;
    client.on("error", () => undefined);
    switch (answer(accepted)) {
      case "starting_up":
        client.once("data", () => client.end(startingUpResponse()));
        return;
      case "stall":
        silentSockets.push(client);
        return;
      case "close":
        // Read (and drop) what the client sends, or the socket never sees
        // its end and never closes.
        client.resume();
        client.end();
        return;
      case "forward": {
        clients.add(client);
        const upstream = new Socket();
        upstream.connect(PG_PORT, PG_HOST, () => {
          client.pipe(upstream).pipe(client);
        });
        upstream.on("error", () => client.destroy());
        client.on("close", () => {
          clients.delete(client);
          upstream.destroy();
        });
      }
    }
  });
  servers.push(server);
  const port = await listen(server);
  return {
    port,
    accepted: () => accepted,
    /** How many forwarded connections are still open. */
    forwarding: () => clients.size,
    /** Closes every forwarded connection, as a backend that went away. */
    dropForwarded: () => clients.forEach((client) => client.end()),
  };
};

/** Refuses the first `refusals` connections (57P03), then forwards. */
const startRestartingPostgres = (refusals: number) =>
  startProxy((n) => (n <= refusals ? "starting_up" : "forward"));

/** Stalls the first `stalls` connections, then forwards. */
const startStalledPostgres = (stalls: number) =>
  startProxy((n) => (n <= stalls ? "stall" : "forward"));

/** Closes the first `closes` connections before answering, then forwards. */
const startDroppingPostgres = (closes: number) =>
  startProxy((n) => (n <= closes ? "close" : "forward"));

/** A port nothing listens on: every connect is refused. */
const closedPort = async () => {
  const server = createServer();
  const port = await listen(server);
  await new Promise((resolve) => server.close(resolve));
  return port;
};

const capture = <A, E>(effect: Effect.Effect<A, E>) => {
  const logs: string[] = [];
  const logger = Logger.make(({ message }) => {
    logs.push(Array.isArray(message) ? message.join(" ") : String(message));
  });
  return Effect.runPromise(
    Effect.either(effect).pipe(
      Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
    ),
  ).then((result) => ({ result, logs }));
};

const unready = (logs: readonly string[]) =>
  logs.filter((line) => line.includes("reason=database_unreachable")).length;
const drops = (logs: readonly string[]) =>
  logs.filter((line) => line.includes("Database upstream drops connections"))
    .length;
const answersAgain = (logs: readonly string[]) =>
  logs.filter((line) => line.includes("Database upstream answers again"))
    .length;

/** A pool built the way the node builds one, through its upstream socket. */
const pool = (
  port: number,
  {
    database = "postgres",
    connectTimeout = Duration.seconds(5),
    retry = FAST,
  }: {
    readonly database?: string;
    readonly connectTimeout?: Duration.DurationInput;
    readonly retry?: DatabaseStartupRetryOptions;
  } = {},
) =>
  Layer.unwrapEffect(
    Effect.map(databaseUpstreamSocket("batch", "127.0.0.1", port), (socket) =>
      retryDatabaseConnectionAtStartup(
        Layer.mapError(
          PgClient.layer({
            host: "127.0.0.1",
            port,
            username: PG_USER,
            password: Redacted.make(PG_PASSWORD),
            database,
            maxConnections: 1,
            connectTimeout,
            socket,
          }),
          (cause) => new PoolInitError({ cause }),
        ),
        "batch",
        retry,
      ),
    ),
  );

const selectOne = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly one: number }>`select 1 as one`;
  return rows[0]?.one;
});

describe("the database pool at startup", () => {
  it("waits out a restarting Postgres, then opens once", async () => {
    const proxy = await startRestartingPostgres(3);
    const { result, logs } = await capture(
      selectOne.pipe(Effect.provide(pool(proxy.port)), Effect.scoped),
    );

    expect(result).toMatchObject({ _tag: "Right", right: 1 });
    expect(unready(logs)).toBe(3);
    // Three refused startups, then the one the pool keeps.
    expect(proxy.accepted()).toBe(4);
  });

  it("waits out a Postgres that accepts and never answers, then opens once", async () => {
    const proxy = await startStalledPostgres(2);
    const { result, logs } = await capture(
      selectOne.pipe(
        // The Effect pool-open timer (1.6 s) fires before postgres.js's own
        // connect_timeout (rounded to 2 s), the order a stalled host gives.
        Effect.provide(
          pool(proxy.port, { connectTimeout: Duration.millis(1_600) }),
        ),
        Effect.scoped,
      ),
    );

    expect(result).toMatchObject({ _tag: "Right", right: 1 });
    const unreadyLines = logs.filter((line) =>
      line.includes("reason=database_unreachable"),
    );
    expect(unreadyLines).toHaveLength(2);
    expect(
      unreadyLines.every((line) =>
        line.includes("PgClient: Connection timed out"),
      ),
    ).toBe(true);
    // Two stalled opens, then the one the pool keeps.
    expect(proxy.accepted()).toBe(3);
  }, 30_000);

  it("refuses a database that does not exist at once", async () => {
    const proxy = await startRestartingPostgres(0);
    const { result, logs } = await capture(
      selectOne.pipe(
        Effect.provide(
          pool(proxy.port, { database: "lv_pv1_absent_database" }),
        ),
        Effect.scoped,
      ),
    );

    expect(result._tag).toBe("Left");
    expect(unready(logs)).toBe(0);
    expect(proxy.accepted()).toBe(1);
  });

  it("gives up once the startup budget is spent", async () => {
    const { result, logs } = await capture(
      selectOne.pipe(
        Effect.provide(
          pool(await closedPort(), {
            retry: { ...FAST, budget: Duration.millis(60) },
          }),
        ),
        Effect.scoped,
      ),
    );
    expect(result._tag).toBe("Left");
    expect(unready(logs)).toBeGreaterThan(1);
  });

  it("waits out a Postgres that closes connections before answering, then opens once", async () => {
    const proxy = await startDroppingPostgres(3);
    const { result, logs } = await capture(
      selectOne.pipe(Effect.provide(pool(proxy.port)), Effect.scoped),
    );

    expect(result).toMatchObject({ _tag: "Right", right: 1 });
    expect(unready(logs)).toBe(3);
    // Three dropped startups, then the one the pool keeps.
    expect(proxy.accepted()).toBe(4);
    // Once when the drops start and once when they stop, not per attempt.
    expect(drops(logs)).toBe(1);
    expect(answersAgain(logs)).toBe(1);
  });

  it("gives up on a Postgres that closes every connection once the budget is spent, at the retry pace", async () => {
    const proxy = await startProxy(() => "close");
    const started = performance.now();
    const { result, logs } = await capture(
      selectOne.pipe(
        Effect.provide(
          pool(proxy.port, {
            retry: {
              baseDelay: Duration.millis(50),
              maxDelay: Duration.millis(200),
              budget: Duration.seconds(1),
            },
          }),
        ),
        Effect.scoped,
      ),
    );
    const elapsedMs = performance.now() - started;

    expect(result._tag).toBe("Left");
    expect(elapsedMs).toBeLessThan(3_000);
    // One connection per startup attempt, each logged as unready: a 1 s
    // budget at 50-200 ms spacing is under a dozen, not hundreds a second.
    expect(proxy.accepted()).toBe(unready(logs));
    expect(proxy.accepted()).toBeLessThan(12);
    expect(drops(logs)).toBe(1);
  }, 10_000);
});

describe("a running database pool", () => {
  it("fails queries at a backed-off pace while its upstream closes connections, then recovers", async () => {
    let closing = false;
    const proxy = await startProxy(() => (closing ? "close" : "forward"));
    const attempt = selectOne.pipe(
      // A query a hot reconnect loop holds never settles; this bounds it.
      Effect.timeout(Duration.seconds(10)),
      Effect.either,
    );

    const { result, logs } = await capture(
      Effect.gen(function* () {
        expect(yield* attempt).toMatchObject({ _tag: "Right", right: 1 });

        closing = true;
        proxy.dropForwarded();
        yield* Effect.sleep(Duration.millis(100));
        const acceptedBefore = proxy.accepted();
        const started = performance.now();
        const failures: unknown[] = [];
        for (let query = 0; query < 5; query += 1) {
          const outcome = yield* attempt;
          expect(outcome._tag).toBe("Left");
          if (outcome._tag === "Left") failures.push(outcome.left);
        }
        const elapsedMs = performance.now() - started;
        const dropped = proxy.accepted() - acceptedBefore;

        closing = false;
        const recovered = yield* attempt;
        return { failures, elapsedMs, dropped, recovered };
      }).pipe(Effect.provide(pool(proxy.port)), Effect.scoped),
    );

    expect(result._tag).toBe("Right");
    if (result._tag !== "Right") return;
    const { failures, elapsedMs, dropped, recovered } = result.right;
    // Every query fails as an unreachable database (readiness reports
    // db_unhealthy); none is held until the timeout.
    expect(failures.every(isConnectionClassError)).toBe(true);
    // One connection per query, spaced by postgres.js's backoff: its
    // shortest delays between five attempts add up to 0.6 s.
    expect(dropped).toBeLessThanOrEqual(5);
    expect(elapsedMs).toBeGreaterThan(500);
    expect(recovered).toMatchObject({ _tag: "Right", right: 1 });
    expect(drops(logs)).toBe(1);
    expect(answersAgain(logs)).toBe(1);
  }, 30_000);

  it("cancels an interrupted query through Postgres without a drop warning or an unhandled rejection", async () => {
    // Postgres answers a CancelRequest by closing the connection without a
    // byte: the close is the cancel's success, not a dropped upstream. The
    // cancel promise postgres.js creates is never observed (Query.cancel
    // drops it), so a socket error on that connection is an unhandled
    // rejection, which terminates the node.
    const proxy = await startProxy(() => "forward");
    const rejections: unknown[] = [];
    const onRejection = (reason: unknown) => rejections.push(reason);
    process.on("unhandledRejection", onRejection);
    try {
      const { result, logs } = await capture(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const sleeping = yield* Effect.fork(sql`select pg_sleep(30)`);
          // Long enough for the query to reach the server and run there.
          yield* Effect.sleep(Duration.millis(300));
          yield* Fiber.interrupt(sleeping);
          // The pool's one connection runs this only once Postgres has
          // cancelled the sleep, well before its 30 s.
          const after = yield* selectOne.pipe(
            Effect.timeout(Duration.seconds(10)),
          );
          // The cancel connection is closed by Postgres; wait until the
          // proxy saw it go, then give its socket events a turn to land.
          while (proxy.forwarding() > 1) {
            yield* Effect.sleep(Duration.millis(10));
          }
          yield* Effect.sleep(Duration.millis(200));
          return after;
        }).pipe(Effect.provide(pool(proxy.port)), Effect.scoped),
      );

      expect(result).toMatchObject({ _tag: "Right", right: 1 });
      // The pool's connection and the cancel request's own connection.
      expect(proxy.accepted()).toBe(2);
      expect(rejections).toEqual([]);
      expect(drops(logs)).toBe(0);
    } finally {
      process.off("unhandledRejection", onRejection);
    }
  }, 30_000);
});

describe("the startup schema compatibility check", () => {
  const scripted = (failures: readonly MigrationError[]) => {
    let calls = 0;
    const effect = Effect.suspend(() => {
      const failure = failures[calls];
      calls += 1;
      return failure === undefined ? Effect.void : Effect.fail(failure);
    });
    return { effect, calls: () => calls };
  };
  const refused = (code: string) =>
    new MigrationError({
      code,
      message: "Failed to reserve schema migration connection",
      cause: Object.assign(new Error("connect ECONNREFUSED"), {
        code: "ECONNREFUSED",
      }),
    });

  it("waits out a dropped connection and a running migration, then passes once", async () => {
    const check = scripted([
      refused("schema_lock_failed"),
      new MigrationError({
        code: "schema_migration_in_progress",
        message: "Could not acquire Midgard schema migration advisory lock",
      }),
    ]);
    const { result, logs } = await capture(
      assertCompatibleWithStartupRetry(check.effect, FAST),
    );
    expect(result._tag).toBe("Right");
    expect(check.calls()).toBe(3);
    expect(
      logs.filter((line) => line.includes("Database unready")),
    ).toHaveLength(2);
  });

  it.each([
    "schema_not_migrated",
    "schema_unversioned_database",
    "schema_checksum_mismatch",
  ])("fails a %s verdict at once", async (code) => {
    const check = scripted([
      new MigrationError({ code, message: "incompatible" }),
    ]);
    const { result } = await capture(
      assertCompatibleWithStartupRetry(check.effect, FAST),
    );
    expect(result._tag).toBe("Left");
    expect(check.calls()).toBe(1);
  });
});
