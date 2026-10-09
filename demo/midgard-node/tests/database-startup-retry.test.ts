import { randomUUID } from "node:crypto";
import type { Socket } from "node:net";
import { setImmediate } from "node:timers/promises";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import {
  Data,
  Duration,
  Effect,
  Fiber,
  Layer,
  Logger,
  Redacted,
  Schedule,
} from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { isConnectionClassError } from "../src/provider-retry.js";
import {
  type DatabaseStartupRetryOptions,
  retryDatabaseConnectionAtStartup,
} from "../src/services/database.js";
import { databaseUpstreamSocket } from "../src/services/database-upstream-socket.js";
import {
  DATABASE_UNREACHABLE,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";
import {
  closedPort,
  PG_HOST,
  PG_PASSWORD,
  PG_PORT,
  PG_USER,
  servers,
  silentSockets,
  startDroppingPostgres,
  startProxy,
  startRestartingPostgres,
  startStalledPostgres,
} from "./database-startup-retry.start-proxy.js";

const FAST = {
  baseDelay: Duration.millis(5),
  maxDelay: Duration.millis(20),
} as const;

class PoolInitError extends Data.TaggedError("PoolInitError")<{
  readonly cause: unknown;
}> {}

afterEach(async () => {
  silentSockets.splice(0).forEach((socket) => socket.destroy());
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
});

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

/**
 * Runs `effect` for `ms` under a startup reporter (`StartupWaitingReporter`)
 * that records every report, then interrupts it: `result` is `None` when it
 * was still waiting.
 */
const captureFor = <A, E>(effect: Effect.Effect<A, E>, ms: number) => {
  const reported: [string, readonly string[]][] = [];
  return capture(
    effect.pipe(
      Effect.timeoutOption(Duration.millis(ms)),
      Effect.locally(StartupWaitingReporter, (key, reasons) =>
        Effect.sync(() => {
          reported.push([key, reasons]);
        }),
      ),
    ),
  ).then(({ result, logs }) => ({ result, logs, reported }));
};

const unready = (logs: readonly string[]) =>
  logs.filter((line) => line.includes("reason=database_unreachable")).length;
const drops = (logs: readonly string[]) =>
  logs.filter((line) => line.includes("Database upstream drops connections"))
    .length;
const answersAgain = (logs: readonly string[]) =>
  logs.filter((line) => line.includes("Database upstream answers again"))
    .length;

/** A pool built the way the node builds one, through its upstream socket.
 * Every socket it opens is appended to `sockets`. */
const pool = (
  port: number,
  {
    database = "postgres",
    connectTimeout = Duration.seconds(5),
    retry = FAST,
    sockets = [],
  }: {
    readonly database?: string;
    readonly connectTimeout?: Duration.DurationInput;
    readonly retry?: DatabaseStartupRetryOptions;
    readonly sockets?: Socket[];
  } = {},
) =>
  Layer.unwrapEffect(
    Effect.map(databaseUpstreamSocket("batch", "127.0.0.1", port), (open) => {
      const socket = () => {
        const opened = open();
        sockets.push(opened);
        return opened;
      };
      return retryDatabaseConnectionAtStartup(
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
      );
    }),
  );

/** Waits until a query whose text holds `marker` is executing in Postgres,
 * read on a connection of its own straight to the test Postgres, so the
 * proxy's connection counts stay the pool's own. */
const untilExecuting = (marker: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql<{ readonly count: number }>`
      SELECT count(*)::int AS count FROM pg_stat_activity
      WHERE state = 'active' AND pid <> pg_backend_pid()
        AND query LIKE ${`%${marker}%`}`.pipe(
      Effect.repeat({
        until: (rows) => (rows[0]?.count ?? 0) > 0,
        schedule: Schedule.spaced(Duration.millis(10)),
      }),
      Effect.timeoutFail({
        duration: Duration.seconds(10),
        onTimeout: () =>
          new Error(`No query marked ${marker} executed within 10 s`),
      }),
    );
  }).pipe(
    Effect.provide(
      PgClient.layer({
        host: PG_HOST,
        port: PG_PORT,
        username: PG_USER,
        password: Redacted.make(PG_PASSWORD),
        database: "postgres",
        maxConnections: 1,
      }),
    ),
  );

/** Waits until every socket in `sockets` but the first (the pool's own
 * connection) has closed, then for one more event-loop turn, so whatever a
 * close set off (a drop warning, a rejection reaching `unhandledRejection`)
 * has landed. */
const untilOthersClosed = (sockets: readonly Socket[]) =>
  Effect.promise(async () => {
    await Promise.all(
      sockets
        .slice(1)
        .map((socket) =>
          socket.closed
            ? undefined
            : new Promise((resolve) => socket.once("close", resolve)),
        ),
    );
    await setImmediate();
  }).pipe(
    Effect.timeoutFail({
      duration: Duration.seconds(10),
      onTimeout: () => new Error("A cancel connection stayed open for 10 s"),
    }),
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

  it("keeps waiting on an unreachable Postgres with no deadline, reporting the reason to the startup", async () => {
    const { result, logs, reported } = await captureFor(
      selectOne.pipe(Effect.provide(pool(await closedPort())), Effect.scoped),
      1_000,
    );
    // Still waiting when the test stops it, after many attempts.
    expect(result).toMatchObject({ _tag: "Right", right: { _tag: "None" } });
    expect(unready(logs)).toBeGreaterThan(10);
    expect(
      new Set(reported.map(([key, reasons]) => `${key}=${reasons.join()}`)),
    ).toEqual(new Set([`database_pool:batch=${DATABASE_UNREACHABLE}`]));
  });

  it("reports the reason while it waits and none once the pool opens", async () => {
    const proxy = await startRestartingPostgres(2);
    const { result, reported } = await captureFor(
      selectOne.pipe(Effect.provide(pool(proxy.port)), Effect.scoped),
      20_000,
    );
    expect(result).toMatchObject({
      _tag: "Right",
      right: { _tag: "Some", value: 1 },
    });
    expect(reported.at(-1)).toEqual(["database_pool:batch", []]);
    expect(reported.slice(0, -1)).toEqual([
      ["database_pool:batch", [DATABASE_UNREACHABLE]],
      ["database_pool:batch", [DATABASE_UNREACHABLE]],
    ]);
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

  it("keeps waiting on a Postgres that closes every connection, at the retry pace", async () => {
    const proxy = await startProxy(() => "close");
    const { result, logs } = await captureFor(
      selectOne.pipe(
        Effect.provide(
          pool(proxy.port, {
            retry: {
              baseDelay: Duration.millis(50),
              maxDelay: Duration.millis(200),
            },
          }),
        ),
        Effect.scoped,
      ),
      1_000,
    );

    expect(result).toMatchObject({ _tag: "Right", right: { _tag: "None" } });
    // One connection per startup attempt, each logged as unready (the one
    // the test stopped may not be): 1 s at 50-200 ms spacing is under a
    // dozen, not hundreds a second.
    expect(proxy.accepted() - unready(logs)).toBeGreaterThanOrEqual(0);
    expect(proxy.accepted() - unready(logs)).toBeLessThanOrEqual(1);
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
    // byte: the close is the cancel's success, not a dropped upstream, so it
    // logs no drop and rejects nothing.
    const proxy = await startProxy(() => "forward");
    const sockets: Socket[] = [];
    const marker = `cancel-${randomUUID()}`;
    const rejections: unknown[] = [];
    const onRejection = (reason: unknown) => rejections.push(reason);
    process.on("unhandledRejection", onRejection);
    try {
      const { result, logs } = await capture(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const sleeping = yield* Effect.fork(
            sql.unsafe(`select pg_sleep(30) /* ${marker} */`),
          );
          // Postgres cancels only a query it is running.
          yield* untilExecuting(marker);
          yield* Fiber.interrupt(sleeping);
          // The pool's one connection runs this only once Postgres has
          // cancelled the sleep, well before its 30 s.
          const after = yield* selectOne.pipe(
            Effect.timeout(Duration.seconds(10)),
          );
          // Postgres closes the cancel connection.
          yield* untilOthersClosed(sockets);
          return after;
        }).pipe(Effect.provide(pool(proxy.port, { sockets })), Effect.scoped),
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

  it("survives a cancel request whose own connection is reset, then serves the next query", async () => {
    // postgres.js sends a cancel on a connection of its own and settles a
    // promise @effect/sql-pg never holds with the outcome: a reset there
    // must not become an unhandled rejection, which terminates the node.
    const proxy = await startProxy((n) => (n === 1 ? "forward" : "reset"));
    const sockets: Socket[] = [];
    const marker = `reset-cancel-${randomUUID()}`;
    const rejections: unknown[] = [];
    const onRejection = (reason: unknown) => rejections.push(reason);
    process.on("unhandledRejection", onRejection);
    try {
      const { result } = await capture(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          // The cancel never arrives, so the sleep runs to its end and only
          // then frees the pool's one connection for the next query.
          const sleeping = yield* Effect.fork(
            sql.unsafe(`select pg_sleep(1) /* ${marker} */`),
          );
          // postgres.js sends a cancel only for a query on the wire.
          yield* untilExecuting(marker);
          yield* Fiber.interrupt(sleeping);
          const after = yield* selectOne.pipe(
            Effect.timeout(Duration.seconds(10)),
          );
          yield* untilOthersClosed(sockets);
          return after;
        }).pipe(Effect.provide(pool(proxy.port, { sockets })), Effect.scoped),
      );

      expect(result).toMatchObject({ _tag: "Right", right: 1 });
      // The pool's connection and the reset cancel connection.
      expect(proxy.accepted()).toBe(2);
      expect(rejections).toEqual([]);
    } finally {
      process.off("unhandledRejection", onRejection);
    }
  }, 30_000);
});
