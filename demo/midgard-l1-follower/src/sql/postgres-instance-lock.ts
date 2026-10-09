import pg from "pg";

import type { WriterLease } from "./backend.js";
import { POSTGRES_WRITER_LEASE_KEY_SQL } from "./postgres-backend.js";

/** Reconnect backoff while the lock's Postgres session is gone. */
export const INSTANCE_LOCK_RECONNECT_INITIAL_MS = 1_000;
export const INSTANCE_LOCK_RECONNECT_MAX_MS = 30_000;

/**
 * How long one attempt waits for its session to connect, and for each of
 * its statements to answer. Without them, an attempt against a host that
 * drops packets without answering lasts until the operating system's TCP
 * timeout, and so does a shutdown waiting on that attempt.
 */
const INSTANCE_LOCK_CONNECT_TIMEOUT_MS = 10_000;
const INSTANCE_LOCK_STATEMENT_TIMEOUT_MS = 10_000;
/**
 * Idle time before the lock's session sends its first TCP keepalive probe.
 * The session runs no statement while it holds the lock, so a connection
 * broken without a close is otherwise noticed only after the operating
 * system's default idle time (two hours on Linux).
 */
export const INSTANCE_LOCK_KEEPALIVE_INITIAL_DELAY_MS = 10_000;

/**
 * Which process kind a lock excludes, and how its refusals read. The lock's
 * key is derived from `keyName` and the schema the holder's tables resolve
 * to, so two holders in different schemas of one database do not exclude
 * each other.
 */
export type PostgresInstanceLockIdentity = {
  /** The key's name; the key is derived from it and `current_schema()`. */
  readonly keyName: string;
  readonly messages: {
    /** Postgres refused the lock: another live process holds it. */
    readonly heldElsewhere: string;
    /** The holder was this process's own ended session, now terminated. */
    readonly heldByOwnStaleSession: string;
    /** The session holding the lock ended under this live process. */
    readonly suspended: string;
    /** A suspended lock was refused again: this process is passive. */
    readonly passive: string;
    /** The server no longer holds the lock for this process's session. */
    readonly lostAtServer: string;
  };
};

export type PostgresInstanceLockEvents = {
  /**
   * Called when an attempt to take the lock again reached Postgres and was
   * refused: another process holds it. This process is now the passive
   * member. It keeps refusing the work the lock guards and keeps trying, and
   * takes over once the holder's session ends.
   */
  readonly onInstanceLockHeldElsewhere?: (error: Error) => void;
  /**
   * Called when the session holding the lock ends. The work the lock guards
   * is refused while the lock reconnects and is taken again.
   */
  readonly onInstanceLockSuspended?: (error: Error) => void;
  /** Called when a suspended or passive lock is held again. */
  readonly onInstanceLockRestored?: () => void;
};

export type PostgresInstanceLockTimers = {
  readonly sleep: (ms: number) => Promise<void>;
};

/** The bounds on one attempt; each defaults to its exported constant. */
export type PostgresInstanceLockBounds = {
  readonly connectTimeoutMs?: number;
  readonly statementTimeoutMs?: number;
};

const defaultTimers: PostgresInstanceLockTimers = {
  sleep: (ms) =>
    new Promise((resolve) => {
      setTimeout(resolve, ms).unref?.();
    }),
};

type LockSession = {
  readonly client: pg.Client;
  readonly key: string;
  readonly backendPid: number;
  /** The backend's start time: with its pid, names the session uniquely. */
  readonly backendStart: string;
};

/** Another live process holds the lock, as Postgres itself reported. */
class InstanceLockHeldElsewhereError extends Error {}

/**
 * The lock is still held by this process's own earlier session, which ended
 * on this side while the server kept it (a half-open connection). That
 * session was asked to end; the lock is tried again.
 */
class InstanceLockHeldByOwnStaleSessionError extends Error {}

const holderOfLockSql = `SELECT l.pid, a.backend_start::text AS backend_start
   FROM pg_locks l
   LEFT JOIN pg_stat_activity a ON a.pid = l.pid
   WHERE l.locktype = 'advisory'
     AND l.granted
     AND l.database = (
       SELECT oid FROM pg_database WHERE datname = current_database()
     )
     AND l.objsubid = 1
     AND ((l.classid::bigint << 32) | l.objid::bigint) = $1::bigint`;

type LockRow = {
  readonly key: string;
  readonly acquired: boolean;
  readonly follower_acquired: boolean;
  readonly pid: number;
  readonly backend_start: string;
};

/**
 * Opens a dedicated session and tries both keys on it: the lock's own and
 * the L1 follower's writer lease key for the same schema. The session holds
 * both or neither; ending it frees whichever it took. Throws
 * `InstanceLockHeldElsewhereError` only when Postgres answered that another
 * process's session holds either key; any other error is a session that
 * could not be had. When the holder is `ownStale`, this process's own
 * earlier session, that session is terminated and
 * `InstanceLockHeldByOwnStaleSessionError` thrown, so the caller tries again.
 */
const trySession = async (
  identity: PostgresInstanceLockIdentity,
  databaseUrl: string,
  bounds: PostgresInstanceLockBounds,
  ownStale?: Pick<LockSession, "backendPid" | "backendStart">,
): Promise<LockSession> => {
  const statementTimeoutMs =
    bounds.statementTimeoutMs ?? INSTANCE_LOCK_STATEMENT_TIMEOUT_MS;
  const client = new pg.Client({
    connectionString: databaseUrl,
    keepAlive: true,
    keepAliveInitialDelayMillis: INSTANCE_LOCK_KEEPALIVE_INITIAL_DELAY_MS,
    connectionTimeoutMillis:
      bounds.connectTimeoutMs ?? INSTANCE_LOCK_CONNECT_TIMEOUT_MS,
    // The server cancels a statement past the bound; the client stops
    // waiting for one whose answer never arrives.
    statement_timeout: statementTimeoutMs,
    query_timeout: statementTimeoutMs,
  });
  // Errors before the session holds the lock reject the attempt; once it
  // holds it, an error suspends the lock (`watch`).
  client.on("error", () => undefined);
  let row: LockRow | undefined;
  let heldByOwnStaleSession = false;
  try {
    await client.connect();
    const result = await client.query<LockRow>(
      `WITH lock_key AS (
         SELECT ('x' || left(md5(
                  $1::text || coalesce(current_schema(), '')
                ), 15))::bit(60)::bigint AS key,
                ${POSTGRES_WRITER_LEASE_KEY_SQL} AS follower_key
       )
       SELECT key::text AS key,
              pg_try_advisory_lock(key) AS acquired,
              pg_try_advisory_lock(follower_key) AS follower_acquired,
              pg_backend_pid() AS pid,
              (SELECT backend_start::text FROM pg_stat_activity
               WHERE pid = pg_backend_pid()) AS backend_start
       FROM lock_key`,
      [identity.keyName],
    );
    row = result.rows[0];
    // This process's own ended session held both keys, so it can only be the
    // holder when the lock's own key was refused.
    if (row !== undefined && !row.acquired && ownStale !== undefined) {
      // Matched by pid and start time together, so a pid the server reused
      // for another process's session is never mistaken for this one's.
      const holders = await client.query<{
        readonly pid: number;
        readonly backend_start: string | null;
      }>(holderOfLockSql, [row.key]);
      heldByOwnStaleSession = holders.rows.some(
        (holder) =>
          holder.pid === ownStale.backendPid &&
          holder.backend_start === ownStale.backendStart,
      );
      if (heldByOwnStaleSession) {
        await client
          .query("SELECT pg_terminate_backend($1)", [ownStale.backendPid])
          .catch(() => undefined);
      }
    }
  } catch (error) {
    await client.end().catch(() => undefined);
    throw error;
  }
  if (row?.acquired !== true || !row.follower_acquired) {
    // Ending the session frees the key it did take.
    await client.end().catch(() => undefined);
    throw heldByOwnStaleSession
      ? new InstanceLockHeldByOwnStaleSessionError(
          identity.messages.heldByOwnStaleSession,
        )
      : new InstanceLockHeldElsewhereError(identity.messages.heldElsewhere);
  }
  return {
    client,
    key: row.key,
    backendPid: row.pid,
    backendStart: row.backend_start,
  };
};

/**
 * A single-instance guarantee for one process kind on one Postgres schema: a
 * session-level advisory lock, taken on a dedicated connection and held
 * until it is released. Postgres releases it when that session ends, so a
 * process that dies frees it and the next process takes it, while a second
 * process started beside a live one is refused it.
 *
 * When the session ends under a live process (Postgres restarted, the
 * connection dropped), or a check finds the server no longer holds the lock
 * for it (`assertHeldAtServer`: the session ended at the server while this
 * side's connection stayed open), the lock is suspended: the work it guards
 * is refused, that session's connection is destroyed, and a session is
 * reopened with bounded backoff and the lock tried again. Taken again, the work resumes. Refused by a reachable
 * Postgres, another process holds it: this process becomes the passive
 * member, refuses the work, and keeps trying at the backoff ceiling until
 * the holder's session ends, then takes over. The holder can also be this
 * process's own ended session, which the server can keep after the
 * connection broke on this side only: that session is terminated and the
 * lock tried again. Nothing here ends the process.
 *
 * The same session also holds the L1 follower's writer lease key for that
 * schema, and lends it to this process's follower (`followerWriterLease`):
 * the lock and the follower are held, lost and taken again together, and
 * no other follower process, `reset` included, can write those tables while
 * this process holds the session.
 */
export class PostgresInstanceLock {
  private session: LockSession;
  /** Set while suspended or passive; the guarded work is refused. */
  private refusal: Error | undefined;
  /** Whether the last refused attempt found another process holding it. */
  private heldElsewhere = false;
  /** The session the lock was last suspended from; it is suspended once. */
  private suspendedFrom: LockSession | undefined;
  private releasing = false;

  private constructor(
    session: LockSession,
    private readonly identity: PostgresInstanceLockIdentity,
    private readonly databaseUrl: string,
    private readonly events: PostgresInstanceLockEvents,
    private readonly timers: PostgresInstanceLockTimers,
    private readonly bounds: PostgresInstanceLockBounds,
  ) {
    this.session = session;
    this.watch(session);
  }

  /**
   * Takes the lock, or throws: `isInstanceLockHeldElsewhere` names a refusal
   * because another live process holds it; any other error is a session
   * that could not be had.
   */
  static async acquire(
    identity: PostgresInstanceLockIdentity,
    databaseUrl: string,
    events: PostgresInstanceLockEvents = {},
    timers: PostgresInstanceLockTimers = defaultTimers,
    bounds: PostgresInstanceLockBounds = {},
  ): Promise<PostgresInstanceLock> {
    return new PostgresInstanceLock(
      await trySession(identity, databaseUrl, bounds),
      identity,
      databaseUrl,
      events,
      timers,
      bounds,
    );
  }

  private watch(session: LockSession): void {
    const ended = (): void => {
      this.suspend(session, this.identity.messages.suspended);
    };
    session.client.on("error", ended);
    session.client.once("end", ended);
  }

  /**
   * Suspends the lock held on `session`, once and only while it is the
   * current session: the guarded work is refused under `message`, the
   * holder is told, the session's connection is destroyed (it may be open
   * on this side only), and the lock is taken again with backoff.
   */
  private suspend(session: LockSession, message: string): void {
    if (
      this.releasing ||
      session !== this.session ||
      this.suspendedFrom === session
    )
      return;
    this.suspendedFrom = session;
    const error = new Error(message);
    this.refusal = error;
    this.events.onInstanceLockSuspended?.(error);
    session.client.connection.stream.destroy();
    void this.reacquire();
  }

  private async reacquire(): Promise<void> {
    let delayMs = INSTANCE_LOCK_RECONNECT_INITIAL_MS;
    while (!this.releasing) {
      await this.timers.sleep(delayMs);
      if (this.releasing) return;
      try {
        const session = await trySession(
          this.identity,
          this.databaseUrl,
          this.bounds,
          this.session,
        );
        if (this.releasing) {
          await session.client.end().catch(() => undefined);
          return;
        }
        this.session = session;
        this.watch(session);
        this.refusal = undefined;
        this.heldElsewhere = false;
        this.events.onInstanceLockRestored?.();
        return;
      } catch (error) {
        if (error instanceof InstanceLockHeldElsewhereError) {
          if (!this.heldElsewhere) {
            this.heldElsewhere = true;
            this.refusal = new Error(this.identity.messages.passive);
            this.events.onInstanceLockHeldElsewhere?.(this.refusal);
          }
        }
        delayMs = Math.min(INSTANCE_LOCK_RECONNECT_MAX_MS, delayMs * 2);
      }
    }
  }

  /**
   * The L1 follower's writer lease on this lock's current session, or null
   * while the lock is suspended, passive or released. It is lost once that
   * session ends or the lock is released, and a lease taken again comes from
   * the next session. Releasing it does nothing: the session is the lock's.
   * While it is lost, `refusal` names the lock's own refusal (suspended,
   * lost at the server, or passive), which the fact store reports.
   */
  followerWriterLease(): WriterLease | null {
    if (this.releasing || this.refusal !== undefined) return null;
    const session = this.session;
    return {
      lost: () =>
        this.releasing ||
        this.refusal !== undefined ||
        this.session !== session,
      refusal: () => this.refusal?.message,
      release: async () => undefined,
    };
  }

  assertHeld(): void {
    if (this.refusal !== undefined) {
      throw this.refusal;
    }
  }

  /**
   * Confirms, from inside `client`'s transaction, that the server still holds
   * the lock for this instance's session. The session can end at the server
   * before this process sees it end: when the server no longer holds it, the
   * lock is suspended (refused under `lostAtServer`, then taken again) and
   * this throws.
   */
  async assertHeldAtServer(client: pg.ClientBase): Promise<void> {
    this.assertHeld();
    const session = this.session;
    const { backendPid, key } = session;
    const result = await client.query<{ readonly held: boolean }>(
      `SELECT EXISTS (
         SELECT 1 FROM pg_locks
         WHERE locktype = 'advisory'
           AND granted
           AND pid = $1
           AND database = (
             SELECT oid FROM pg_database WHERE datname = current_database()
           )
           AND objsubid = 1
           AND ((classid::bigint << 32) | objid::bigint) = $2::bigint
       ) AS held`,
      [backendPid, key],
    );
    if (result.rows[0]?.held !== true) {
      // The session ended at the server; this side may not see it end until
      // its keepalive gives up. Suspend now and take the lock again.
      this.suspend(session, this.identity.messages.lostAtServer);
      throw new Error(this.identity.messages.lostAtServer);
    }
  }

  async release(): Promise<void> {
    if (this.releasing) {
      return;
    }
    this.releasing = true;
    if (this.refusal === undefined) {
      await this.session.client.end();
    }
  }
}

export const isInstanceLockHeldElsewhere = (error: unknown): boolean =>
  error instanceof InstanceLockHeldElsewhereError;
