/**
 * The node's instance lock (`node-instance-lock.ts`) when its session is
 * lost under a live holder.
 *
 * - A held fiber's tick running when another process takes the lock
 *   finishes; the fiber starts no new tick until this process takes the lock
 *   again.
 * - A session ended at the server while the lock's connection stays open on
 *   this side (a frozen connection) raises `node_instance_lock_suspended` at
 *   the next server-side check; while the server holds the lock, the checks
 *   raise nothing.
 */
import { Duration, Effect, Schedule } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { nodeDatabaseConnectionString } from "../src/services/l1-provider.js";
import {
  COMMIT_HALT_SOURCES,
  HALT_POLL_MS,
  pausedWhileHalted,
} from "../src/services/liveness-halt.js";
import { NODE_INSTANCE_LOCK_SUSPENDED } from "../src/services/node-instance-lock.js";
import {
  acquire,
  cleanUpLocks,
  database,
  endLockSessions,
  fibers,
  freezingProxy,
  livenessGlobals,
  reasonOf,
  release,
  scopes,
  steppedSleep,
} from "./helpers/node-instance-lock.js";

afterEach(cleanUpLocks);

describe(
  "the node instance lock taken over mid-tick",
  { concurrent: false },
  () => {
    it("lets a held fiber's running tick finish when another process takes the lock mid-tick, and starts no new tick until this process takes it again", async () => {
      const globals = livenessGlobals();
      const sleep = steppedSleep();
      await acquire({ globals, timers: sleep.timers });
      let started = 0;
      let finished = 0;
      let finishFirst = () => undefined as void;
      const firstGate = new Promise<void>((resolve) => {
        finishFirst = resolve;
      });
      // A fiber held as the commit fibers are (`heldSchedule`): its first tick
      // runs until the test lets it finish.
      const tick = Effect.gen(function* () {
        started += 1;
        if (started === 1) yield* Effect.promise(() => firstGate);
        finished += 1;
      });
      fibers.push(
        Effect.runFork(
          Effect.repeat(
            tick,
            pausedWhileHalted(
              Schedule.spaced(Duration.millis(10)),
              globals,
              COMMIT_HALT_SOURCES,
            ),
          ),
        ),
      );
      await vi.waitFor(() => expect(started).toBe(1));

      // Mid-tick, this process's session ends and another process takes the
      // lock.
      await endLockSessions();
      await vi.waitFor(() =>
        expect(reasonOf(globals)).toBe(NODE_INSTANCE_LOCK_SUSPENDED),
      );
      const other = await acquire();
      expect(await other.lock.followerWriterLease()).not.toBeNull();

      // The running tick finishes; no new one starts while the reason stands.
      finishFirst();
      await vi.waitFor(() => expect(finished).toBe(1));
      await new Promise((resolve) => setTimeout(resolve, HALT_POLL_MS * 2));
      expect(started).toBe(1);

      // The other process's session ends and this one takes the lock again:
      // the fiber ticks again.
      await release(other.scope);
      await vi.waitFor(() => expect(sleep.sleeping()).toBe(1));
      sleep.wake();
      await vi.waitFor(() => expect(reasonOf(globals)).toBeUndefined());
      await vi.waitFor(() => expect(started).toBeGreaterThan(1), {
        timeout: HALT_POLL_MS * 4,
        interval: 50,
      });
    }, 30_000);
  },
);

describe(
  "the node instance lock's server-side check",
  { concurrent: false },
  () => {
    it("raises nothing while the server holds the lock, and holds operator duties once the server ended the session behind a connection that stays open", async () => {
      const proxy = await freezingProxy();
      const globals = livenessGlobals();
      // The reacquire after a suspension waits on this sleep, never woken.
      const sleep = steppedSleep();
      try {
        await acquire({
          connectionString: nodeDatabaseConnectionString({
            ...database,
            POSTGRES_HOST: "127.0.0.1",
            POSTGRES_PORT: proxy.port,
          }),
          globals,
          timers: sleep.timers,
          checkIntervalMs: 50,
        });
        // Checks run (each opens a session of its own) and find the lock held.
        await vi.waitFor(() => expect(proxy.connections()).toBeGreaterThan(3), {
          timeout: 5_000,
          interval: 20,
        });
        expect(reasonOf(globals)).toBeUndefined();

        // The lock's connection (the proxy's first) freezes, then the server
        // ends its session: the lock's client sees no end, the next check
        // finds the lock gone.
        proxy.freezeFirst();
        await endLockSessions();
        await vi.waitFor(
          () => expect(reasonOf(globals)).toBe(NODE_INSTANCE_LOCK_SUSPENDED),
          { timeout: 5_000, interval: 20 },
        );
      } finally {
        await proxy.close();
        for (const scope of scopes.splice(0)) await release(scope);
      }
    }, 30_000);
  },
);
