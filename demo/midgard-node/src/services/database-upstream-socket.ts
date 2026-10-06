import { connect, type Socket } from "node:net";

import { Effect, Runtime } from "effect";

import type { DatabasePoolRole } from "./database.js";

/** The protocol code of a CancelRequest, after its length (16). */
const CANCEL_REQUEST_CODE = 80877102;

const isCancelRequest = (chunk: unknown): boolean =>
  Buffer.isBuffer(chunk) &&
  chunk.length === 16 &&
  chunk.readInt32BE(0) === 16 &&
  chunk.readInt32BE(4) === CANCEL_REQUEST_CODE;

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
 *
 * A CancelRequest connection is left alone. postgres.js opens one through
 * this factory whenever an in-flight query is cancelled (Effect interrupts
 * a running query by cancelling it), and Postgres answers it by closing the
 * connection without a byte: that close is the cancel's success, not a
 * dropped upstream. postgres.js never observes the promise of that cancel,
 * so turning its close into an error would be an unhandled rejection, which
 * terminates the node.
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
      // postgres.js writes one message first and waits for the server: a
      // StartupMessage, an SSLRequest or a CancelRequest. Only that first
      // write is inspected.
      let cancelRequest = false;
      const write = socket.write;
      socket.write = ((chunk: unknown, ...rest: unknown[]): boolean => {
        socket.write = write;
        cancelRequest = isCancelRequest(chunk);
        return Reflect.apply(write, socket, [chunk, ...rest]) as boolean;
      }) as Socket["write"];
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
        if (answered || cancelRequest) {
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
