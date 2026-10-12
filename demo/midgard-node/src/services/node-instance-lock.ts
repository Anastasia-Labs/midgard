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
 *   capped backoff until the holder's session ends: it waits on another
 *   process, so the wait has no deadline.
 * - A startup attempt that cannot reach Postgres waits under
 *   `node_instance_lock_unavailable` while the failure is a transient
 *   connection failure, for at most the database budget (15 min) of
 *   consecutive such failures; past it, or on any other failure (bad
 *   credentials, a missing database), the startup fails
 *   (`StartupStepFailedError`, step `instance_lock`).
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
 *   `store_locked`. Taken again, the reason clears and they resume. The wait
 *   on another holder has no deadline; a reconnect that fails transiently is
 *   retried for at most the reacquire budget (15 min) in a row, and any
 *   other failure, or the budget running out, stops the retries under
 *   `node_instance_lock_failed`. On a budget that ran out the node exits
 *   non-zero (`transient-exhaustion.ts`), its supervisor's restart being the
 *   backoff; on any other failure the reason stands, the process up and
 *   unready, until the node is restarted.
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
  classifyFailure,
  type InstanceLockFailedError,
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
  Clock,
  Context,
  Duration,
  Effect,
  Layer,
  Redacted,
  Runtime,
} from "effect";

import { isConnectionClassError } from "../provider-retry.js";
import { DATABASE_CONNECT_TIMEOUT } from "./database.js";
import type { Globals } from "./globals.globals.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import {
  STARTUP_DATABASE_BUDGET,
  startupStepFailed,
} from "./startup-waiting.js";
import { signalTransientExhausted } from "./transient-exhaustion.js";

/** Another live process holds the node's instance lock on this database. */
export const NODE_INSTANCE_LOCK_HELD_ELSEWHERE =
  "node_instance_lock_held_elsewhere";

/** The instance lock could not be tried: no session to Postgres. */
export const NODE_INSTANCE_LOCK_UNAVAILABLE = "node_instance_lock_unavailable";

/** The session holding the lock ended; it is being taken again. */
export const NODE_INSTANCE_LOCK_SUSPENDED = "node_instance_lock_suspended";

/**
 * A suspended lock stopped trying: a failure that is not transient (the
 * node stays up, unready, until it is restarted), or Postgres unreachable
 * past the reacquire budget (the node exits non-zero).
 */
export const NODE_INSTANCE_LOCK_FAILED = "node_instance_lock_failed";

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
    failed:
      "node instance lock was not taken again; operator duties stay held until the node is restarted",
  },
};

export type NodeInstanceLock = Readonly<{
  /** The follower's writer lease on the lock's session (`openPostgresFactStore`'s `writerLease`). */
  followerWriterLease: () => Promise<WriterLease | null>;
}>;

export type AcquireNodeInstanceLockOptions = Readonly<{
  connectionString: string;
  globals: Pick<Globals, "LIVENESS_REASONS" | "TRANSIENT_EXHAUSTION">;
  /** Reports the named reasons the startup waits on (`/readyz`). */
  waiting: (reasons: readonly string[]) => Effect.Effect<void>;
  timers?: PostgresInstanceLockTimers;
  /** Bounds one attempt; the connect bound defaults to the node pools'. */
  bounds?: PostgresInstanceLockBounds;
  retryInitialMs?: number;
  retryMaxMs?: number;
  /** How long consecutive transient connection failures are waited out;
   * default `STARTUP_DATABASE_BUDGET`. */
  unavailableBudget?: Duration.DurationInput;
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
 * Whether a failed attempt to take the lock is a transient connection
 * failure: a connection-class error (`isConnectionClassError`) or a failure
 * the follower's store classifies as transient (`classifyFailure`: pg's own
 * connection failures carry no SQLSTATE).
 */
export const isInstanceLockAttemptTransient = (error: unknown): boolean =>
  isConnectionClassError(error) || classifyFailure(error) === "transient";

/**
 * Takes the node's instance lock and releases it when the scope closes.
 * While another process holds it the startup waits, with no deadline,
 * under `node_instance_lock_held_elsewhere`. While Postgres cannot be
 * reached it waits under `node_instance_lock_unavailable` for at most
 * `unavailableBudget` (the database budget) of consecutive transient
 * failures; past it, or on a failure that is not transient, it fails with a
 * `StartupStepFailedError` naming the step `instance_lock`.
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
      // Transient failures past the reacquire budget end the node (it exits
      // non-zero); any other failure holds it up under the reason.
      onInstanceLockFailed: (error: InstanceLockFailedError) => {
        run(
          raiseLivenessIncident(
            globals,
            HaltSource.instanceLock,
            NODE_INSTANCE_LOCK_FAILED,
            error.message,
          ),
        );
        if (error.exhausted)
          signalTransientExhausted(globals.TRANSIENT_EXHAUSTION, {
            source: "instance_lock",
            reason: NODE_INSTANCE_LOCK_FAILED,
            detail: error.message,
          });
      },
      onInstanceLockRestored: () =>
        run(clearLivenessIncident(globals, HaltSource.instanceLock)),
    };
    const maxMs = options.retryMaxMs ?? NODE_INSTANCE_LOCK_RETRY_MAX_MS;
    let delayMs = options.retryInitialMs ?? NODE_INSTANCE_LOCK_RETRY_INITIAL_MS;
    let lastReason: string | undefined;
    let attempts = 0;
    let unavailableSince: number | undefined;
    const unavailableMs = Duration.toMillis(
      Duration.decode(options.unavailableBudget ?? STARTUP_DATABASE_BUDGET),
    );
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
      const heldElsewhere = isInstanceLockHeldElsewhere(cause);
      const reason = heldElsewhere
        ? NODE_INSTANCE_LOCK_HELD_ELSEWHERE
        : NODE_INSTANCE_LOCK_UNAVAILABLE;
      attempts += 1;
      if (heldElsewhere) {
        // Waiting on the holder: no deadline, and a later outage's budget
        // starts afresh.
        unavailableSince = undefined;
      } else {
        const now = yield* Clock.currentTimeMillis;
        unavailableSince ??= now;
        const transient = isInstanceLockAttemptTransient(cause);
        if (!transient || now - unavailableSince + delayMs > unavailableMs) {
          yield* options.waiting([]);
          return yield* Effect.fail(
            startupStepFailed({
              step: "instance_lock",
              reason,
              cause,
              exhausted: transient,
              attempts,
              waitedMs: now - unavailableSince,
            }),
          );
        }
      }
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
