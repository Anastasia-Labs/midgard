import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import pg from "pg";
import { afterAll, describe, expect, it } from "vitest";

import {
  type FactStore,
  followChain,
  FOLLOWER_WAITING,
  type FollowStatus,
  openPostgresFactStore,
  PostgresInstanceLock,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  simStoreOptions,
} from "../src/testing/index.js";
import { script, scriptedTransport, simOrigin } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";
import {
  advisoryLockHolders,
  freezableProxy,
  terminateLockHolder,
  testDatabases,
  withAdmin,
} from "./support/postgres.js";

const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

const events: readonly ChainSyncEvent[] = buildForkSteps(
  forkCorpus(SIM_K).find((entry) => entry.name === "every shape in sequence")!
    .scenario,
  [FIXTURE_PROJECTION],
).steps.map((step) => step.event);

const pause = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

const waitFor = async (
  what: string,
  holds: () => boolean,
  latest: () => unknown,
): Promise<void> => {
  const deadline = Date.now() + 15_000;
  while (!holds()) {
    if (Date.now() > deadline)
      throw new Error(
        `timed out waiting for ${what}; ${JSON.stringify(latest())}`,
      );
    await pause(5);
  }
};

/** Terminates the database's sessions (every one, or those matching `where`). */
const terminate = (database: string, where = "true"): Promise<number> =>
  withAdmin(async (client) => {
    const result = await client.query(
      `SELECT pg_terminate_backend(pid) AS done FROM pg_stat_activity
        WHERE datname = $1 AND pid <> pg_backend_pid() AND ${where}`,
      [database],
    );
    return result.rows.length;
  });

describe("followChain on Postgres: dropped connections", () => {
  it("survives Postgres terminating idle and busy connections, and applies the next event", async () => {
    const database = await databases.create();
    const dropped: string[] = [];
    const uncaught: unknown[] = [];
    const monitor = (error: unknown): void => {
      uncaught.push(error);
    };
    process.on("uncaughtExceptionMonitor", monitor);
    const base = openPostgresFactStore({
      ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "postgres"),
      connection: {
        connectionString: database.url,
        maxConnections: 4,
        onConnectionError: (error) => dropped.push(error.message),
      },
    });
    // The block at `stall` holds its connection in a query long enough for
    // the test to terminate that backend mid-transaction.
    const stallAt = events.findIndex(
      (event, index) => index >= 8 && event.kind === "roll_forward",
    );
    const stallPoint = events[stallAt]!.point;
    const stall = stallPoint.kind === "point" ? Number(stallPoint.slot) : -1;
    let stalled = false;
    const store: FactStore = {
      ...base,
      applyBlock: async (block) => {
        if (block.point.slot === stall && !stalled) {
          stalled = true;
          try {
            await base.transaction("write", (tx) =>
              tx.query("SELECT pg_sleep(5) AS slept"),
            );
          } catch (error) {
            return {
              kind: "error",
              error: error instanceof Error ? error : new Error(String(error)),
            };
          }
        }
        return base.applyBlock(block);
      },
    };
    const s = script(events, { limit: 5 });
    const statuses: FollowStatus[] = [];
    const latest = () => statuses[statuses.length - 1];
    const abort = new AbortController();
    const running = followChain({
      store,
      transport: scriptedTransport(s),
      origin: simOrigin(),
      signal: abort.signal,
      backoffMs: { initial: 1, max: 20 },
      stuckAfter: 1,
      onStatus: (status) => {
        statuses.push(status);
      },
    });
    try {
      await waitFor(
        "the first 5 events",
        () => (latest()?.events ?? 0) >= 5,
        latest,
      );
      // Idle pooled connections, the writer lease and any listener: gone.
      expect(await terminate(database.name)).toBeGreaterThan(0);
      await waitFor(
        "the pool to see the drop",
        () => dropped.length > 0,
        () => dropped,
      );
      s.limit = undefined;
      // A busy connection: terminated inside its transaction.
      await waitFor("the stalled apply", () => stalled, latest);
      await pause(200);
      expect(await terminate(database.name, "query LIKE '%pg_sleep%'")).toBe(1);
      await waitFor(
        "the cursor at the last event",
        () => s.acked === events.length,
        () => ({ acked: s.acked, status: latest() }),
      );
      expect(uncaught).toEqual([]);
      // The drops were transient: waited out, never escalated.
      const causes = new Set(statuses.map((status) => status.waiting?.cause));
      expect(causes.has("apply")).toBe(true);
      expect(statuses.some((status) => status.stuck !== null)).toBe(false);
      const cursor = await base.cursor();
      const tip = events[events.length - 1]!.tip.point;
      expect(cursor?.point.slot).toBe(
        tip.kind === "point" ? Number(tip.slot) : -1,
      );
    } finally {
      abort.abort();
      await running;
      process.off("uncaughtExceptionMonitor", monitor);
      await base.close();
    }
  });
});

describe("followChain on Postgres: an instance lock lent the writer lease", () => {
  it("names the lock's refusal in readiness while the server has lost it, writes nothing, and resumes once it is taken again", async () => {
    const database = await databases.create();
    const proxy = await freezableProxy(database);
    const suspended: string[] = [];
    let restored = 0;
    let openGate = (): void => undefined;
    const gate = new Promise<void>((resolve) => {
      openGate = resolve;
    });
    const lock = await PostgresInstanceLock.acquire(
      {
        keyName: "follow-loop-instance-lock-test:",
        messages: {
          heldElsewhere: "held elsewhere",
          heldByOwnStaleSession: "held by own stale session",
          suspended: "instance lock suspended",
          passive: "instance lock passive",
          lostAtServer: "instance lock lost at server",
        },
      },
      proxy.url,
      {
        onInstanceLockSuspended: (error) => suspended.push(error.message),
        onInstanceLockRestored: () => {
          restored += 1;
        },
      },
      { sleep: () => gate },
    );
    const store = openPostgresFactStore({
      ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "postgres"),
      connection: { connectionString: database.url, maxConnections: 4 },
      writerLease: () => Promise.resolve(lock.followerWriterLease()),
    });
    const s = script(events, { limit: 5 });
    const statuses: FollowStatus[] = [];
    const latest = () => statuses[statuses.length - 1];
    const abort = new AbortController();
    const running = followChain({
      store,
      transport: scriptedTransport(s),
      origin: simOrigin(),
      signal: abort.signal,
      backoffMs: { initial: 1, max: 20 },
      onStatus: (status) => {
        statuses.push(status);
      },
    });
    const lockWaiting = (status: FollowStatus | undefined): boolean =>
      status?.readiness.some(
        (entry) =>
          entry.reason === FOLLOWER_WAITING &&
          entry.detail.includes("instance lock lost at server"),
      ) ?? false;
    try {
      await waitFor("the first 5 events", () => s.acked >= 5, latest);
      const [pid] = await advisoryLockHolders(database);
      // The server ends the lock's session; this side's connection hears
      // nothing, until a fenced write checks the lock at the server.
      proxy.freeze();
      await terminateLockHolder(database, pid!);
      const checker = new pg.Client({ connectionString: database.url });
      await checker.connect();
      try {
        await expect(lock.assertHeldAtServer(checker)).rejects.toThrow(
          "instance lock lost at server",
        );
      } finally {
        await checker.end();
      }
      expect(suspended).toEqual(["instance lock lost at server"]);
      const ackedWhenLost = s.acked;
      const cursorWhenLost = await store.cursor();
      s.limit = undefined;
      await waitFor(
        "readiness to name the lock's refusal",
        () => lockWaiting(latest()),
        latest,
      );
      expect(latest()?.waiting?.cause).toBe("store_locked");
      // Refused: nothing applied while the lock is lost.
      await pause(100);
      expect(s.acked).toBe(ackedWhenLost);
      expect(await store.cursor()).toEqual(cursorWhenLost);
      openGate();
      await waitFor("the lock taken again", () => restored === 1, latest);
      await waitFor(
        "the cursor at the last event",
        () => s.acked === events.length,
        () => ({ acked: s.acked, status: latest() }),
      );
      expect(lockWaiting(latest())).toBe(false);
      expect(latest()?.waiting).toBeNull();
      expect(suspended).toHaveLength(1);
    } finally {
      abort.abort();
      await running;
      await store.close();
      await lock.release();
      await proxy.close();
    }
  });
});
