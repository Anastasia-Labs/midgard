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
 *   `HaltSource.instanceLock`, which holds the commit, settlement, merge and
 *   watchdog fibers, and its follower store waits on `store_locked`. Taken
 *   again, the reason clears and they resume. Nothing here exits.
 */
import {
  isInstanceLockHeldElsewhere,
  PostgresInstanceLock,
  type PostgresInstanceLockIdentity,
  type PostgresInstanceLockTimers,
  type WriterLease,
} from "@al-ft/midgard-l1-follower";
import { Duration, Effect, Runtime } from "effect";

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
  retryInitialMs?: number;
  retryMaxMs?: number;
}>;

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
    // the scope); the wait between attempts is not, so a startup interrupted
    // while it waits stops at once.
    const attempt = Effect.either(
      Effect.acquireRelease(
        Effect.tryPromise(() =>
          PostgresInstanceLock.acquire(
            NODE_INSTANCE_LOCK,
            options.connectionString,
            events,
            options.timers,
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
