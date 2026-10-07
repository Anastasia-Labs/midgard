import {
  POSTGRES_WRITER_LEASE_KEY_SQL,
  type WriterLease,
} from "@al-ft/midgard-l1-follower";
import { Client, type PoolClient } from "pg";

/** Reconnect backoff while the lock's Postgres session is gone. */
export const INSTANCE_LOCK_RECONNECT_INITIAL_MS = 1_000;
export const INSTANCE_LOCK_RECONNECT_MAX_MS = 30_000;

export type PostgresStoreInstanceLockEvents = {
  /**
   * Called when an attempt to take the lock again reached Postgres and was
   * refused: another process holds it. This process is now the passive
   * member. It keeps refusing every decision effect and keeps trying, and
   * takes over once the holder's session ends.
   */
  readonly onInstanceLockHeldElsewhere?: (error: Error) => void;
  /**
   * Called when the session holding the lock ends. Every decision effect is
   * refused while the lock reconnects and is taken again.
   */
  readonly onInstanceLockSuspended?: (error: Error) => void;
  /** Called when a suspended or passive lock is held again. */
  readonly onInstanceLockRestored?: () => void;
};

export type PostgresStoreInstanceLockTimers = {
  readonly sleep: (ms: number) => Promise<void>;
};

const defaultTimers: PostgresStoreInstanceLockTimers = {
  sleep: (ms) =>
    new Promise((resolve) => {
      setTimeout(resolve, ms).unref?.();
    }),
};

type LockSession = {
  readonly client: Client;
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

const heldElsewhereMessage =
  "committee node Postgres store instance lock is held by another live process (another committee node, or an L1 follower command on this store's follower tables); this process stays passive and takes over when that process's session ends";

/**
 * Opens a dedicated session and tries both keys on it: the store's own and
 * the L1 follower's writer lease key for the same schema. The session holds
 * both or neither; ending it frees whichever it took. Throws
 * `InstanceLockHeldElsewhereError` only when Postgres answered that another
 * process's session holds either key; any other error is a session that
 * could not be had. When the holder is `ownStale`, this process's own earlier session,
 * that session is terminated and `InstanceLockHeldByOwnStaleSessionError`
 * thrown, so the caller tries again.
 */
const trySession = async (
  databaseUrl: string,
  ownStale?: Pick<LockSession, "backendPid" | "backendStart">,
): Promise<LockSession> => {
  const client = new Client({
    connectionString: databaseUrl,
    keepAlive: true,
  });
  // An unexpected end of the session is handled on "end".
  client.on("error", () => undefined);
  let row:
    | {
        readonly key: string;
        readonly acquired: boolean;
        readonly follower_acquired: boolean;
        readonly pid: number;
        readonly backend_start: string;
      }
    | undefined;
  let heldByOwnStaleSession = false;
  try {
    await client.connect();
    const result = await client.query<{
      readonly key: string;
      readonly acquired: boolean;
      readonly follower_acquired: boolean;
      readonly pid: number;
      readonly backend_start: string;
    }>(
      `WITH lock_key AS (
         SELECT ('x' || left(md5(
                  'midgard-da-committee-store:' ||
                  coalesce(current_schema(), '')
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
    );
    row = result.rows[0];
    // This process's own ended session held both keys, so it can only be the
    // holder when the store's own key was refused.
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
          "committee node Postgres store instance lock is still held by this process's own ended session; that session was terminated and the lock is tried again",
        )
      : new InstanceLockHeldElsewhereError(heldElsewhereMessage);
  }
  return {
    client,
    key: row.key,
    backendPid: row.pid,
    backendStart: row.backend_start,
  };
};

/**
 * The Postgres store's single-instance guarantee: a session-level advisory
 * lock, taken on a dedicated connection when the store opens and held until it
 * closes. Postgres releases it when that session ends, so a process that dies
 * frees it and the next process takes it, while a second process started
 * beside a live one cannot open the store at all.
 *
 * When the session ends under a live process (Postgres restarted, the
 * connection dropped), the lock is suspended: every decision effect is
 * refused, and the session is reopened with bounded backoff and the lock
 * tried again. Taken again, the store resumes. Refused by a reachable
 * Postgres, another process holds it: this process becomes the passive
 * member, refuses every decision effect, and keeps trying at the backoff
 * ceiling until the holder's session ends, then takes over. The holder can
 * also be this process's own ended session, which the server can keep after
 * the connection broke on this side only: that session is terminated and
 * the lock tried again. Effects this process completes after a gap are
 * still checked against their attempt count, so work another holder did in
 * between is never overwritten. Nothing here ends the process.
 *
 * The key is derived from the schema the store's tables resolve to, so two
 * stores in different schemas of one database do not exclude each other.
 *
 * The same session also holds the L1 follower's writer lease key for that
 * schema, and lends it to this process's follower (`followerWriterLease`):
 * the store and the follower are held, lost and taken again together, and
 * no other follower process, `reset` included, can write those tables while
 * this process holds the session.
 */
export class PostgresStoreInstanceLock {
  private session: LockSession;
  /** Set while suspended or passive; decision effects are refused. */
  private refusal: Error | undefined;
  /** Whether the last refused attempt found another process holding it. */
  private heldElsewhere = false;
  private releasing = false;

  private constructor(
    session: LockSession,
    private readonly databaseUrl: string,
    private readonly events: PostgresStoreInstanceLockEvents,
    private readonly timers: PostgresStoreInstanceLockTimers,
  ) {
    this.session = session;
    this.watch(session);
  }

  static async acquire(
    databaseUrl: string,
    events: PostgresStoreInstanceLockEvents = {},
    timers: PostgresStoreInstanceLockTimers = defaultTimers,
  ): Promise<PostgresStoreInstanceLock> {
    return new PostgresStoreInstanceLock(
      await trySession(databaseUrl),
      databaseUrl,
      events,
      timers,
    );
  }

  private watch(session: LockSession): void {
    session.client.once("end", () => {
      if (this.releasing || session !== this.session) return;
      const error = new Error(
        "committee node Postgres store suspended its instance lock: the session holding it ended; decision effects are refused until it is taken again",
      );
      this.refusal = error;
      this.events.onInstanceLockSuspended?.(error);
      void this.reacquire();
    });
  }

  private async reacquire(): Promise<void> {
    let delayMs = INSTANCE_LOCK_RECONNECT_INITIAL_MS;
    while (!this.releasing) {
      await this.timers.sleep(delayMs);
      if (this.releasing) return;
      try {
        const session = await trySession(this.databaseUrl, this.session);
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
            this.refusal = new Error(
              "committee node Postgres store is passive: another live process (another committee node, or an L1 follower command on this store's follower tables) holds its instance lock; decision effects are refused until that process's session ends and this one takes over",
            );
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
   */
  followerWriterLease(): WriterLease | null {
    if (this.releasing || this.refusal !== undefined) return null;
    const session = this.session;
    return {
      lost: () =>
        this.releasing ||
        this.refusal !== undefined ||
        this.session !== session,
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
   * before this process sees it end.
   */
  async assertHeldAtServer(client: PoolClient): Promise<void> {
    this.assertHeld();
    const { backendPid, key } = this.session;
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
      throw new Error(
        "committee node Postgres store lost its instance lock: the server no longer holds it for this process",
      );
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
