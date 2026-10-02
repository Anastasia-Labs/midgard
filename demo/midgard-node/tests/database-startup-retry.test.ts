import { createServer, type Server, Socket } from "node:net";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Data, Duration, Effect, Layer, Logger, Redacted } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { assertCompatibleWithStartupRetry } from "../src/database/init.js";
import { MigrationError } from "../src/database/migrations/runner.js";
import { retryDatabaseConnectionAtStartup } from "../src/services/database.js";

const PG_HOST = process.env.PGHOST ?? "127.0.0.1";
const PG_PORT = Number(process.env.PGPORT ?? "5433");
const PG_USER = process.env.PGUSER ?? "postgres";
const PG_PASSWORD = process.env.PGPASSWORD ?? "postgres";

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
 * A TCP proxy to the test Postgres that answers the first `refusals`
 * connections the way a restarting Postgres does (57P03), then forwards.
 */
const startRestartingPostgres = async (refusals: number) => {
  let accepted = 0;
  const server = createServer((client) => {
    accepted += 1;
    client.on("error", () => undefined);
    if (accepted <= refusals) {
      client.once("data", () => client.end(startingUpResponse()));
      return;
    }
    const upstream = new Socket();
    upstream.connect(PG_PORT, PG_HOST, () => {
      client.pipe(upstream).pipe(client);
    });
    upstream.on("error", () => client.destroy());
    client.on("close", () => upstream.destroy());
  });
  servers.push(server);
  const port = await listen(server);
  return { port, accepted: () => accepted };
};

/**
 * A TCP proxy to the test Postgres that accepts the first `stalls`
 * connections and never answers them (a stalled host), then forwards.
 */
const startStalledPostgres = async (stalls: number) => {
  let accepted = 0;
  const server = createServer((client) => {
    accepted += 1;
    client.on("error", () => undefined);
    if (accepted <= stalls) {
      silentSockets.push(client);
      return;
    }
    const upstream = new Socket();
    upstream.connect(PG_PORT, PG_HOST, () => {
      client.pipe(upstream).pipe(client);
    });
    upstream.on("error", () => client.destroy());
    client.on("close", () => upstream.destroy());
  });
  servers.push(server);
  const port = await listen(server);
  return { port, accepted: () => accepted };
};

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

const pool = (
  port: number,
  database = "postgres",
  connectTimeout: Duration.DurationInput = Duration.seconds(5),
) =>
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
      }),
      (cause) => new PoolInitError({ cause }),
    ),
    "batch",
    FAST,
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
        Effect.provide(pool(proxy.port, "postgres", Duration.millis(1_600))),
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
        Effect.provide(pool(proxy.port, "lv_pv1_absent_database")),
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
          retryDatabaseConnectionAtStartup(
            Layer.mapError(
              PgClient.layer({
                host: "127.0.0.1",
                port: await closedPort(),
                username: PG_USER,
                password: Redacted.make(PG_PASSWORD),
                database: "postgres",
                maxConnections: 1,
              }),
              (cause) => new PoolInitError({ cause }),
            ),
            "batch",
            { ...FAST, budget: Duration.millis(60) },
          ),
        ),
        Effect.scoped,
      ),
    );
    expect(result._tag).toBe("Left");
    expect(unready(logs)).toBeGreaterThan(1);
  });
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
