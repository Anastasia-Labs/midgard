/**
 * The node's instance lock over this worker's database (E-N1H-1).
 *
 * - A second acquirer waits, under `node_instance_lock_held_elsewhere`, until
 *   the holder's session ends, then takes the lock; an unreachable Postgres
 *   is waited on under `node_instance_lock_unavailable`, and an attempt
 *   against a host that never answers ends at its connect bound.
 * - The lock's session lends the follower its writer lease: none while the
 *   session is gone.
 * - A session ended under a live holder raises `node_instance_lock_suspended`
 *   under `HaltSource.instanceLock`; refused on the next try because another
 *   process took the lock meanwhile, it raises
 *   `node_instance_lock_held_elsewhere`; taken again, the reason clears.
 * - Its loss mid-tick and at the server alone:
 *   `node-instance-lock.session-loss.test.ts`.
 * - `runNode` waits at its `instance_lock` stage, unready under the named
 *   reason, while another process holds the lock, and goes on to the startup
 *   steps after it only once the lock is taken.
 */
import { Effect, Exit, Fiber, Scope } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { nodeDatabaseConnectionString } from "../src/services/l1-provider.js";
import {
  acquireNodeInstanceLock,
  NODE_INSTANCE_LOCK_HELD_ELSEWHERE,
  NODE_INSTANCE_LOCK_SUSPENDED,
  NODE_INSTANCE_LOCK_UNAVAILABLE,
} from "../src/services/node-instance-lock.js";
import {
  acquire,
  cleanUpLocks,
  closedPort,
  database,
  endLockSessions,
  fibers,
  livenessGlobals,
  reasonOf,
  release,
  scopes,
  steppedSleep,
} from "./helpers/node-instance-lock.js";

const after = vi.hoisted(() => ({
  entered: undefined as (() => void) | undefined,
}));

vi.mock("../src/services/index.js", async (importOriginal) => {
  const { Layer } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/services/index.js")>()),
    validationPoolLayer: Layer.empty,
    mempoolLedgerCacheLayer: Layer.empty,
  };
});
vi.mock("../src/services/settlement.js", () => ({
  settlementWalletAddress: () => "unused-before-the-instance-lock",
}));
vi.mock("../src/e2e/phase1-accept-crash-checkpoint.js", async () => {
  const { Effect: E } = await import("effect");
  return { assertPhase1AcceptCrashCheckpointConfiguration: E.void };
});
vi.mock("../src/services/native-mpf-startup.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/native-mpf-startup.js")
    >()),
    requirePinnedNativeOwnerBinary: () => E.void,
  };
});
// The first startup step after the lock: entered, it holds there.
vi.mock("../src/da/startup.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/da/startup.js")>()),
    runDaIdentityGatedStartupSequence: () =>
      E.sync(() => after.entered?.()).pipe(E.zipRight(E.never)),
  };
});

afterEach(async () => {
  await cleanUpLocks();
  after.entered = undefined;
});

describe("the node instance lock", { concurrent: false }, () => {
  it("makes a second acquirer wait, held elsewhere, until the holder's session ends", async () => {
    const first = await acquire();
    expect(await first.lock.followerWriterLease()).not.toBeNull();
    const waited: string[][] = [];
    let taken = false;
    const second = acquire({ waited }).then((held) => {
      taken = true;
      return held;
    });
    await vi.waitFor(() => expect(waited.length).toBeGreaterThanOrEqual(3));
    expect(new Set(waited.flat())).toEqual(
      new Set([NODE_INSTANCE_LOCK_HELD_ELSEWHERE]),
    );
    expect(taken).toBe(false);

    await release(first.scope);
    const { lock } = await second;
    expect(await lock.followerWriterLease()).not.toBeNull();
  });

  it("waits on an unreachable Postgres under its own reason, and stops at once when interrupted", async () => {
    const waited: string[][] = [];
    const scope = Effect.runSync(Scope.make());
    scopes.push(scope);
    const fiber = Effect.runFork(
      acquireNodeInstanceLock({
        connectionString: nodeDatabaseConnectionString({
          ...database,
          POSTGRES_PORT: await closedPort(),
        }),
        globals: livenessGlobals(),
        waiting: (reasons) => Effect.sync(() => waited.push([...reasons])),
        retryInitialMs: 20,
        retryMaxMs: 50,
      }).pipe(Scope.extend(scope)),
    );
    await vi.waitFor(() => expect(waited.length).toBeGreaterThanOrEqual(2));
    expect(new Set(waited.flat())).toEqual(
      new Set([NODE_INSTANCE_LOCK_UNAVAILABLE]),
    );
    expect(fiber.unsafePoll()).toBeNull();
    // A shutdown during the wait is not held up by it.
    const exit = await Promise.race([
      Effect.runPromise(Fiber.interrupt(fiber)),
      new Promise<"still waiting">((resolve) =>
        setTimeout(() => resolve("still waiting"), 2_000),
      ),
    ]);
    expect(exit).not.toBe("still waiting");
    expect(Exit.isInterrupted(exit as Exit.Exit<unknown, unknown>)).toBe(true);
  });

  it("ends an attempt against a host that never answers at its connect bound, under the unavailable reason", async () => {
    const waited: string[][] = [];
    const scope = Effect.runSync(Scope.make());
    scopes.push(scope);
    const startedAt = Date.now();
    const fiber = Effect.runFork(
      acquireNodeInstanceLock({
        // TEST-NET-1 (RFC 5737) is never routed: the connect is neither
        // answered nor refused.
        connectionString: nodeDatabaseConnectionString({
          ...database,
          POSTGRES_HOST: "192.0.2.1",
          POSTGRES_PORT: 5432,
        }),
        globals: livenessGlobals(),
        waiting: (reasons) => Effect.sync(() => waited.push([...reasons])),
        bounds: { connectTimeoutMs: 200 },
        retryInitialMs: 20,
        retryMaxMs: 50,
      }).pipe(Scope.extend(scope)),
    );
    fibers.push(fiber);
    await vi.waitFor(() => expect(waited.length).toBeGreaterThanOrEqual(1), {
      timeout: 3_000,
      interval: 20,
    });
    expect(Date.now() - startedAt).toBeLessThan(3_000);
    expect(new Set(waited.flat())).toEqual(
      new Set([NODE_INSTANCE_LOCK_UNAVAILABLE]),
    );
    expect(fiber.unsafePoll()).toBeNull();
  });

  it("holds operator duties while its session is gone and clears once it is taken again", async () => {
    const globals = livenessGlobals();
    const sleep = steppedSleep();
    const { lock } = await acquire({ globals, timers: sleep.timers });
    const lease = (await lock.followerWriterLease())!;
    expect(reasonOf(globals)).toBeUndefined();

    await endLockSessions();
    await vi.waitFor(() =>
      expect(reasonOf(globals)).toBe(NODE_INSTANCE_LOCK_SUSPENDED),
    );
    expect(lease.lost()).toBe(true);
    expect(await lock.followerWriterLease()).toBeNull();

    // Another process takes the lock while this one's session is gone: the
    // next try is refused, and the reason names that.
    const other = await acquire();
    await vi.waitFor(() => expect(sleep.sleeping()).toBe(1));
    sleep.wake();
    await vi.waitFor(() =>
      expect(reasonOf(globals)).toBe(NODE_INSTANCE_LOCK_HELD_ELSEWHERE),
    );
    expect(await lock.followerWriterLease()).toBeNull();

    // That process's session ends: the next try takes the lock again.
    await release(other.scope);
    await vi.waitFor(() => expect(sleep.sleeping()).toBe(1));
    sleep.wake();
    await vi.waitFor(() => expect(reasonOf(globals)).toBeUndefined());
    const again = (await lock.followerWriterLease())!;
    expect(again.lost()).toBe(false);
  });
});

describe("runNode's instance-lock stage", { concurrent: false }, () => {
  it("waits unready while another process holds the lock, and goes on once it is taken", async () => {
    const holder = await acquire();
    const port = await closedPort();
    let entered = false;
    after.entered = () => {
      entered = true;
    };
    // Startup is held at the first step after the lock; the cast describes
    // that partial test boundary.
    const fiber = Effect.runFork(
      withStartupHttpServer(port, (startup) =>
        runNode(startup).pipe(
          Effect.provideService(NodeConfig, {
            PORT: port,
            ...database,
          } as unknown as NodeConfig["Type"]),
          Effect.provide(Globals.Default),
        ),
      ) as Effect.Effect<void, unknown>,
    );
    fibers.push(fiber);
    const base = `http://127.0.0.1:${port.toString()}`;
    await vi.waitFor(
      async () => {
        const ready = await fetch(`${base}/readyz`);
        expect(ready.status).toBe(503);
        expect(await ready.json()).toMatchObject({
          reasons: ["startup_incomplete", NODE_INSTANCE_LOCK_HELD_ELSEWHERE],
          stage: "instance_lock",
        });
      },
      { timeout: 10_000, interval: 100 },
    );
    expect(entered).toBe(false);

    await release(holder.scope);
    await vi.waitFor(() => expect(entered).toBe(true), {
      timeout: 10_000,
      interval: 100,
    });
    const ready = await fetch(`${base}/readyz`);
    expect(await ready.json()).toMatchObject({
      reasons: ["startup_incomplete"],
      stage: "local_preflight",
    });
  }, 30_000);
});
