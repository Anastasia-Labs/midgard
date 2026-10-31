import {
  type FactStore,
  openPostgresBackend,
  openPostgresFactStore,
  POSTGRES_WRITER_LEASE_KEY_SQL,
  resetToOrigin,
} from "@al-ft/midgard-l1-follower";
import { Client } from "pg";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { isInstanceLockHeldElsewhere } from "../src/store/postgres.instance-lock.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "../src/store/postgres.js";
import {
  type PostgresTestDatabase,
  postgresTestDatabases,
  terminateInstanceLockSessions,
} from "./helpers/postgres-database.js";

const databases = postgresTestDatabases("committee_follower_lease");
const cleanups: (() => Promise<unknown>)[] = [];

afterEach(async () => {
  for (const cleanup of cleanups.splice(0).reverse())
    await cleanup().catch(() => undefined);
});

afterAll(async () => {
  await databases.dropAll();
});

const openCommittee = async (
  database: PostgresTestDatabase,
  options: PostgresCommitteeStoreOptions = {},
): Promise<PostgresCommitteeStore> => {
  const store = await PostgresCommitteeStore.open(database.url, options);
  cleanups.push(() => store.close());
  return store;
};

/**
 * A follower store on the committee's database: the committee's own, on the
 * lease its instance lock holds, or an independent process's, on a lease of
 * its own (`lease` omitted).
 */
const openFollower = (
  database: PostgresTestDatabase,
  lease?: PostgresCommitteeStore,
): FactStore => {
  const store = openPostgresFactStore({
    securityParameter: 10,
    trackedSet: {
      addresses: new Set(),
      paymentCredentials: new Set(),
      policies: new Set(),
    },
    connection: { connectionString: database.url, maxConnections: 2 },
    ...(lease === undefined
      ? {}
      : { writerLease: async () => lease.instanceLock.followerWriterLease() }),
  });
  cleanups.push(() => store.close());
  return store;
};

const ORIGIN = { point: { slot: 7, hash: Buffer.alloc(32, 0x07) }, height: 1 };

/** Sessions holding the committee store's key and the follower's, by pid. */
const lockHolders = async (database: PostgresTestDatabase) => {
  const client = new Client({ connectionString: database.url });
  await client.connect();
  try {
    const { rows } = await client.query<{
      readonly store: number[] | null;
      readonly follower: number[] | null;
    }>(
      `WITH keys AS (
         SELECT ('x' || left(md5(
                  'midgard-da-committee-store:' ||
                  coalesce(current_schema(), '')
                ), 15))::bit(60)::bigint AS store,
                ${POSTGRES_WRITER_LEASE_KEY_SQL} AS follower
       ),
       held AS (
         SELECT pid, (classid::bigint << 32) | objid::bigint AS key
         FROM pg_locks
         WHERE locktype = 'advisory' AND granted AND objsubid = 1
           AND database = (
             SELECT oid FROM pg_database WHERE datname = current_database()
           )
       )
       SELECT
         (SELECT array_agg(pid) FROM held, keys WHERE key = keys.store) AS store,
         (SELECT array_agg(pid) FROM held, keys WHERE key = keys.follower) AS follower`,
    );
    return { store: rows[0]?.store ?? [], follower: rows[0]?.follower ?? [] };
  } finally {
    await client.end();
  }
};

describe("the committee instance lock holds the L1 follower's writer lease on one session", () => {
  it("excludes a second committee process and an independent follower from both keys, and the committee from a follower's key", async () => {
    const database = await databases.create();
    const committee = await openCommittee(database);
    const holders = await lockHolders(database);
    expect(holders.store).toHaveLength(1);
    expect(holders.follower).toEqual(holders.store);

    const second = await PostgresCommitteeStore.open(database.url).catch(
      (error: unknown) => error,
    );
    expect(isInstanceLockHeldElsewhere(second)).toBe(true);
    const independent = openFollower(database);
    await expect(independent.start()).resolves.toMatchObject({
      kind: "store_locked",
    });

    // The committee's own follower writes on the lease the lock holds.
    const own = openFollower(database, committee);
    await expect(own.start()).resolves.toMatchObject({ kind: "ready" });
    await expect(own.initialize(ORIGIN)).resolves.toMatchObject({
      kind: "initialized",
    });
    await own.close();

    // Closed, the committee frees both: an independent follower takes the
    // follower key, and a committee process starting now is refused until
    // that follower lets it go.
    await committee.close();
    await expect(independent.start()).resolves.toMatchObject({
      kind: "ready",
    });
    const blocked = await PostgresCommitteeStore.open(database.url).catch(
      (error: unknown) => error,
    );
    expect(isInstanceLockHeldElsewhere(blocked)).toBe(true);
    expect((await lockHolders(database)).store).toEqual([]);
    await independent.close();
    await openCommittee(database);
  });

  it("loses both when the session ends: the committee refuses work and its follower is locked out, then retakes both on one new session", async () => {
    const database = await databases.create();
    const events: string[] = [];
    const committee = await openCommittee(database, {
      onInstanceLockSuspended: () => events.push("suspended"),
      onInstanceLockHeldElsewhere: () => events.push("held_elsewhere"),
      onInstanceLockRestored: () => events.push("restored"),
    });
    const follower = openFollower(database, committee);
    await expect(follower.start()).resolves.toMatchObject({ kind: "ready" });
    const before = await lockHolders(database);

    // One session holds both keys, so one termination ends both.
    expect(await terminateInstanceLockSessions(database)).toBe(1);
    await vi.waitFor(() => expect(events).toEqual(["suspended"]));
    expect(committee.instanceLock.followerWriterLease()).toBeNull();
    await expect(follower.initialize(ORIGIN)).resolves.toMatchObject({
      kind: "store_locked",
    });
    await expect(follower.start()).resolves.toMatchObject({
      kind: "store_locked",
    });

    await vi.waitFor(() => expect(events).toEqual(["suspended", "restored"]), {
      timeout: 5_000,
    });
    const after = await lockHolders(database);
    expect(after.store).toHaveLength(1);
    expect(after.follower).toEqual(after.store);
    expect(after.store).not.toEqual(before.store);
    await expect(follower.start()).resolves.toMatchObject({ kind: "ready" });
    await expect(follower.initialize(ORIGIN)).resolves.toMatchObject({
      kind: "initialized",
    });
  });

  it("refuses `follower reset` while the committee holds the session, and lets it run once the committee is gone", async () => {
    const database = await databases.create();
    const committee = await openCommittee(database);
    // What `midgard-l1-follower reset --to-origin --postgres <url>` runs.
    const reset = async () => {
      const backend = openPostgresBackend({
        connectionString: database.url,
        maxConnections: 1,
      });
      try {
        return await resetToOrigin(backend);
      } finally {
        await backend.close();
      }
    };
    await expect(reset()).resolves.toMatchObject({ kind: "store_locked" });
    await committee.close();
    await expect(reset()).resolves.toMatchObject({ kind: "reset" });
  });
});
