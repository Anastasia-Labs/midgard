/**
 * The node's single-process exclusion on its database (E-N1H-1): a Postgres
 * session advisory lock (`PostgresInstanceLock`), taken at startup before
 * the database is initialized, before any lease release, mutation-job
 * classification or L1 follower driver start, and held for the process's
 * life. Postgres releases it when the holding session ends, so a node that
 * dies frees it and the next one takes it; nothing needs a manual release.
 *
 * - A second node process on the same database waits at startup, process up
 *   and unready under `node_instance_lock_held_elsewhere`, and retries on a
 *   capped backoff until the holder's session ends.
 * - The lock's session also holds the L1 follower's writer lease for the
 *   node's schema and lends it to the node's follower store, so the node and
 *   its follower are held, lost and taken again together.
 * - When the session ends under a live node, the lock is suspended: the node
 *   raises `node_instance_lock_suspended` (or, refused by a reachable
 *   Postgres, `node_instance_lock_held_elsewhere`) under
 *   `HaltSource.instanceLock`. While it stands, the block-commitment, merge
 *   and watchdog fibers start no new tick (a tick already running finishes,
 *   `pausedWhileHalted`), the settlement worker is stopped
 *   (`restartedAcrossHalts`), and the follower store waits on
 *   `store_locked`. Taken again, the reason clears and they resume. Nothing
 *   here exits.
 * - The session can end at the server while this side's connection stays
 *   open (a connection broken without a close), which the lock's connection
 *   would notice only when its TCP keepalive gives up. So every
 *   `NODE_INSTANCE_LOCK_CHECK_INTERVAL_MS` the node asks the server, on a
 *   short session of its own, whether it still holds the lock for the lock's
 *   session (`assertHeldAtServer`). When it does not, the node raises
 *   `node_instance_lock_suspended` under `HaltSource.instanceLock` at once;
 *   it clears when the lock is taken again on a new session. A check that
 *   cannot reach Postgres changes nothing and runs again at the next
 *   interval; a refused lock (suspended or passive) is not checked.
 */
import {
  isInstanceLockHeldElsewhere,
  PostgresInstanceLock,
  type PostgresInstanceLockBounds,
  type PostgresInstanceLockIdentity,
  type PostgresInstanceLockTimers,
  type WriterLease,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import {
  Cause,
  Context,
  Duration,
  Effect,
  Layer,
  Redacted,
  Runtime,
} from "effect";

import { DATABASE_CONNECT_TIMEOUT } from "./database.js";
import type { Globals } from "./globals.globals.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "./liveness-halt.js";

/** Another live process holds the node's instance lock on this database. */
export const NODE_INSTANCE_LOCK_HELD_ELSEWHERE =
  "node_instance_lock_held_elsewhere";

/** The instance lock could not be tried: no session to Postgres. */
export const NODE_INSTANCE_LOCK_UNAVAILABLE = "node_instance_lock_unavailable";

/** The session holding the lock ended; it is being taken again. */
export const NODE_INSTANCE_LOCK_SUSPENDED = "node_instance_lock_suspended";

/** How often the node confirms that the server still holds its lock. */
export const NODE_INSTANCE_LOCK_CHECK_INTERVAL_MS = 10_000;

/** First wait before the startup tries the lock again. */
export const NODE_INSTANCE_LOCK_RETRY_INITIAL_MS = 1_000;
/** Ceiling of that wait as it doubles. */
export const NODE_INSTANCE_LOCK_RETRY_MAX_MS = 30_000;

const NODE_INSTANCE_LOCK: PostgresInstanceLockIdentity = {
  keyName: "midgard-node:",
  messages: {
    heldElsewhere:
      "node instance lock is held by another live process (another node on this database, or an L1 follower command on its follower tables); this process waits, unready, and takes the lock when that process's session ends",
    heldByOwnStaleSession:
      "node instance lock is still held by this process's own ended session; that session was terminated and the lock is tried again",
    suspended:
      "node instance lock suspended: the session holding it ended; operator duties are held until it is taken again",
    passive:
      "node instance lock is held by another live process (another node on this database, or an L1 follower command on its follower tables); operator duties are held until that process's session ends and this one takes over",
    lostAtServer:
      "node instance lock lost: the server no longer holds it for this process",
  },
};

export type NodeInstanceLock = Readonly<{
  /** The follower's writer lease on the lock's session (`openPostgresFactStore`'s `writerLease`). */
  followerWriterLease: () => Promise<WriterLease | null>;
}>;

export type AcquireNodeInstanceLockOptions = Readonly<{
  connectionString: string;
  globals: Pick<Globals, "LIVENESS_REASONS">;
  /** Reports the named reasons the startup waits on (`/readyz`). */
  waiting: (reasons: readonly string[]) => Effect.Effect<void>;
  timers?: PostgresInstanceLockTimers;
  /** Bounds one attempt; the connect bound defaults to the node pools'. */
  bounds?: PostgresInstanceLockBounds;
  retryInitialMs?: number;
  retryMaxMs?: number;
  /** Interval of the server-side check; default `NODE_INSTANCE_LOCK_CHECK_INTERVAL_MS`. */
  checkIntervalMs?: number;
}>;

type LockCheckClient = Parameters<
  PostgresInstanceLock["assertHeldAtServer"]
>[0];

/**
 * Asks the server, on a session of its own opened for the check, whether it
 * still holds `held` for the lock's session. `held` when it does,
 * `lost` when it does not, `unknown` when the check could not run.
 */
const checkHeldAtServer = (
  held: PostgresInstanceLock,
  connectionString: string,
) =>
  Effect.scoped(
    Effect.gen(function* () {
      const context = yield* Layer.build(
        PgClient.layer({
          url: Redacted.make(connectionString),
          maxConnections: 1,
          connectTimeout: DATABASE_CONNECT_TIMEOUT,
        }),
      );
      const sql = Context.get(context, SqlClient.SqlClient);
      const runtime = yield* Effect.runtime<never>();
      // `assertHeldAtServer` runs one parameterized query on the client.
      const client = {
        query: (text: string, values: readonly (string | number)[]) =>
          Runtime.runPromise(runtime)(sql.unsafe(text, values)).then(
            (rows) => ({ rows }),
          ),
      } as unknown as LockCheckClient;
      return yield* Effect.tryPromise(() => held.assertHeldAtServer(client));
    }),
  ).pipe(
    Effect.timeout(Duration.times(DATABASE_CONNECT_TIMEOUT, 2)),
    Effect.as("held" as const),
    Effect.catchAllCause((cause) => {
      const failure = Cause.squash(cause);
      const error =
        failure instanceof Error && "error" in failure
          ? (failure as { error: unknown }).error
          : failure;
      return Effect.succeed(
        error instanceof Error &&
          error.message === NODE_INSTANCE_LOCK.messages.lostAtServer
          ? ({ kind: "lost", message: error.message } as const)
          : ({
              kind: "unknown",
              message: error instanceof Error ? error.message : String(error),
            } as const),
      );
    }),
  );

/**
 * Takes the node's instance lock, waiting under a named reason while it
 * cannot, and releases it when the scope closes. Never fails.
 */
export const acquireNodeInstanceLock = (
  options: AcquireNodeInstanceLockOptions,
) =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<never>();
    const run = (effect: Effect.Effect<void>) => {
      Runtime.runFork(runtime)(effect);
    };
    const { globals } = options;
    const events = {
      onInstanceLockSuspended: (error: Error) =>
        run(
          raiseLivenessIncident(
            globals,
            HaltSource.instanceLock,
            NODE_INSTANCE_LOCK_SUSPENDED,
            error.message,
          ),
        ),
      onInstanceLockHeldElsewhere: (error: Error) =>
        run(
          raiseLivenessIncident(
            globals,
            HaltSource.instanceLock,
            NODE_INSTANCE_LOCK_HELD_ELSEWHERE,
            error.message,
          ),
        ),
      onInstanceLockRestored: () =>
        run(clearLivenessIncident(globals, HaltSource.instanceLock)),
    };
    const maxMs = options.retryMaxMs ?? NODE_INSTANCE_LOCK_RETRY_MAX_MS;
    let delayMs = options.retryInitialMs ?? NODE_INSTANCE_LOCK_RETRY_INITIAL_MS;
    let lastReason: string | undefined;
    // Each attempt alone is uninterruptible (a lock it takes is released with
    // the scope), and bounded by its connect and statement timeouts; the wait
    // between attempts is not, so a startup interrupted while it waits stops
    // at once.
    const bounds: PostgresInstanceLockBounds = {
      connectTimeoutMs: Duration.toMillis(DATABASE_CONNECT_TIMEOUT),
      ...options.bounds,
    };
    const attempt = Effect.either(
      Effect.acquireRelease(
        Effect.tryPromise(() =>
          PostgresInstanceLock.acquire(
            NODE_INSTANCE_LOCK,
            options.connectionString,
            events,
            options.timers,
            bounds,
          ),
        ),
        (held) => Effect.promise(() => held.release().catch(() => undefined)),
      ),
    );
    for (;;) {
      const result = yield* attempt;
      if (result._tag === "Right") {
        if (lastReason !== undefined)
          yield* Effect.logInfo("node instance lock taken");
        const held = result.right;
        const checkMs =
          options.checkIntervalMs ?? NODE_INSTANCE_LOCK_CHECK_INTERVAL_MS;
        // Ends with the scope, before the lock is released.
        yield* Effect.forkScoped(
          Effect.forever(
            Effect.gen(function* () {
              yield* Effect.sleep(Duration.millis(checkMs));
              // A refused lock (suspended or passive) names its reason through
              // its events already, and clears it once taken again.
              const lease = held.followerWriterLease();
              if (lease === null) return;
              const checked = yield* checkHeldAtServer(
                held,
                options.connectionString,
              );
              if (checked === "held") return;
              // Raised only while the session the check ran for is still the
              // lock's own: once the lock left it, the lock's events name the
              // reason and clear it.
              if (checked.kind === "lost" && !lease.lost())
                yield* raiseLivenessIncident(
                  globals,
                  HaltSource.instanceLock,
                  NODE_INSTANCE_LOCK_SUSPENDED,
                  checked.message,
                );
              else
                yield* Effect.logDebug(
                  `node instance lock check did not run: ${checked.message}`,
                );
            }),
          ),
        );
        return {
          followerWriterLease: () =>
            Promise.resolve(held.followerWriterLease()),
        } satisfies NodeInstanceLock;
      }
      const cause = result.left.error;
      const reason = isInstanceLockHeldElsewhere(cause)
        ? NODE_INSTANCE_LOCK_HELD_ELSEWHERE
        : NODE_INSTANCE_LOCK_UNAVAILABLE;
      yield* options.waiting([reason]);
      const detail = `${reason}: ${cause instanceof Error ? cause.message : String(cause)}; retrying in ${delayMs.toString()} ms`;
      yield* reason === lastReason
        ? Effect.logDebug(detail)
        : Effect.logWarning(detail);
      lastReason = reason;
      yield* Effect.sleep(Duration.millis(delayMs));
      delayMs = Math.min(delayMs * 2, maxMs);
    }
  });
