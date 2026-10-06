import { connect, type Socket } from "node:net";

import { Effect, Runtime } from "effect";

import type { DatabasePoolRole } from "./database.js";

/**
 * The socket factory every node database pool hands postgres.js.
 *
 * postgres.js (3.4.9, and upstream master) reconnects at once, with no
 * backoff, when a socket closes cleanly while a connection is still being
 * established; each attempt also restarts its connect_timeout. An upstream
 * that accepts TCP and closes before answering (a docker userland proxy in
 * front of a dead Postgres) therefore makes it reconnect about 700 times a
 * second, forever: the pending query never settles, so neither the startup
 * retry budget nor the pool-open timeout ever fires, and nothing is logged.
 *
 * This factory turns that close into the ECONNRESET a dropped connection
 * gives. postgres.js then fails the pending query (the startup retry and
 * readiness see `database_unreachable`, as for a refused connection) and
 * spaces the next attempt with its own backoff. Only a close before the
 * server's first byte is converted; any answer, including an ErrorResponse
 * from a restarting Postgres, keeps postgres.js's own handling.
 */
export const databaseUpstreamSocket = (
  role: DatabasePoolRole,
  host: string,
  port: number,
): Effect.Effect<() => Socket> =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<never>();
    const log = (effect: Effect.Effect<void>) =>
      Runtime.runSync(runtime)(effect);
    const endpoint = `${host}:${port.toString()}`;
    // One state per pool, kept across startup retries, so the condition is
    // logged when it starts and when it clears, not on every attempt.
    let dropping = false;
    return () => {
      // A host naming a directory is a Unix socket, as postgres.js reads it.
      const socket = host.includes("/")
        ? connect({ path: `${host}/.s.PGSQL.${port.toString()}` })
        : connect({ host, port });
      // postgres.js names the endpoint in its connection errors from these.
      Object.assign(socket, { host, port });
      let answered = false;
      socket.once("data", () => {
        answered = true;
        if (dropping) {
          dropping = false;
          log(
            Effect.logInfo(
              `Database upstream answers again: the ${role} pool's upstream ${endpoint} is answering connections.`,
            ),
          );
        }
      });
      socket.once("end", () => {
        if (answered) {
          return;
        }
        if (!dropping) {
          dropping = true;
          log(
            Effect.logWarning(
              `Database upstream drops connections: the ${role} pool's upstream ${endpoint} accepts each connection and closes it before answering. Queries fail as database_unreachable and reconnects back off until it answers.`,
            ),
          );
        }
        socket.destroy(
          Object.assign(
            new Error(
              `database upstream ${endpoint} closed the connection before answering`,
            ),
            { code: "ECONNRESET" },
          ),
        );
      });
      return socket;
    };
  });
