import {
  INSTANCE_LOCK_RECONNECT_INITIAL_MS,
  INSTANCE_LOCK_RECONNECT_MAX_MS,
  isInstanceLockHeldElsewhere,
  PostgresInstanceLock,
  type PostgresInstanceLockEvents,
  type PostgresInstanceLockIdentity,
  type PostgresInstanceLockTimers,
} from "@al-ft/midgard-l1-follower";

export {
  INSTANCE_LOCK_RECONNECT_INITIAL_MS,
  INSTANCE_LOCK_RECONNECT_MAX_MS,
  isInstanceLockHeldElsewhere,
};

/**
 * The instance lock's events. The work it guards is every decision effect:
 * each is refused while the lock is suspended or passive.
 */
export type PostgresStoreInstanceLockEvents = PostgresInstanceLockEvents;
export type PostgresStoreInstanceLockTimers = PostgresInstanceLockTimers;

/**
 * The committee store's instance lock (`PostgresInstanceLock`), taken when
 * the store opens and held until it closes: a second process started beside
 * a live one cannot open the store at all. While it is suspended or passive,
 * every decision effect is refused. Effects this process completes after a
 * gap are still checked against their attempt count, so work another holder
 * did in between is never overwritten. Its session also holds the L1
 * follower's writer lease for the store's schema, lent to this process's
 * follower.
 */
const COMMITTEE_STORE_INSTANCE_LOCK: PostgresInstanceLockIdentity = {
  keyName: "midgard-da-committee-store:",
  messages: {
    heldElsewhere:
      "committee node Postgres store instance lock is held by another live process (another committee node, or an L1 follower command on this store's follower tables); this process stays passive and takes over when that process's session ends",
    heldByOwnStaleSession:
      "committee node Postgres store instance lock is still held by this process's own ended session; that session was terminated and the lock is tried again",
    suspended:
      "committee node Postgres store suspended its instance lock: the session holding it ended; decision effects are refused until it is taken again",
    passive:
      "committee node Postgres store is passive: another live process (another committee node, or an L1 follower command on this store's follower tables) holds its instance lock; decision effects are refused until that process's session ends and this one takes over",
    lostAtServer:
      "committee node Postgres store lost its instance lock: the server no longer holds it for this process",
    failed:
      "committee node Postgres store stopped taking its instance lock again; decision effects stay refused until the process is restarted",
  },
};

export type PostgresStoreInstanceLock = PostgresInstanceLock;

export const PostgresStoreInstanceLock = {
  acquire: (
    databaseUrl: string,
    events: PostgresStoreInstanceLockEvents = {},
    timers?: PostgresStoreInstanceLockTimers,
  ): Promise<PostgresStoreInstanceLock> =>
    PostgresInstanceLock.acquire(
      COMMITTEE_STORE_INSTANCE_LOCK,
      databaseUrl,
      events,
      timers,
    ),
};
