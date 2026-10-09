/**
 * The node's instance lock over this worker's database (E-N1H-1).
 *
 * - A second acquirer waits, under `node_instance_lock_held_elsewhere`, until
 *   the holder's session ends, then takes the lock; an unreachable Postgres
 *   is waited on under `node_instance_lock_unavailable`.
 * - The lock's session lends the follower its writer lease: none while the
 *   session is gone.
 * - A session ended under a live holder raises `node_instance_lock_suspended`
 *   under `HaltSource.instanceLock`; refused on the next try because another
 *   process took the lock meanwhile, it raises
 *   `node_instance_lock_held_elsewhere`; taken again, the reason clears.
 * - `runNode` waits at its `instance_lock` stage, unready under the named
 *   reason, while another process holds the lock, and goes on to the startup
 *   steps after it only once the lock is taken.
 */
import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Exit, Fiber, Redacted, Ref, Scope } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { nodeDatabaseConnectionString } from "../src/services/l1-provider.js";
import { HaltSource } from "../src/services/liveness-halt.js";
import {
  acquireNodeInstanceLock,
  type AcquireNodeInstanceLockOptions,
  NODE_INSTANCE_LOCK_HELD_ELSEWHERE,
  NODE_INSTANCE_LOCK_SUSPENDED,
  NODE_INSTANCE_LOCK_UNAVAILABLE,
  type NodeInstanceLock,
} from "../src/services/node-instance-lock.js";

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

const env = (name: string): string => {
  const value = process.env[name];
  if (value === undefined || value === "")
    throw new Error(`${name} is not set`);
  return value;
};

const database = {
  POSTGRES_HOST: env("POSTGRES_HOST"),
  POSTGRES_PORT: Number(env("POSTGRES_PORT")),
  POSTGRES_USER: env("POSTGRES_USER"),
  POSTGRES_PASSWORD: env("POSTGRES_PASSWORD"),
  POSTGRES_DB: env("POSTGRES_DB"),
};
const connectionString = nodeDatabaseConnectionString(database);

const livenessGlobals = () => ({
  LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
});

const reasonOf = (globals: ReturnType<typeof livenessGlobals>) =>
  Effect.runSync(Ref.get(globals.LIVENESS_REASONS)).get(
    HaltSource.instanceLock,
  );

const scopes: Scope.CloseableScope[] = [];
const fibers: Fiber.RuntimeFiber<unknown, unknown>[] = [];

afterEach(async () => {
  for (const fiber of fibers.splice(0))
    await Effect.runPromise(Fiber.interrupt(fiber));
  for (const scope of scopes.splice(0))
    await Effect.runPromise(Scope.close(scope, Exit.void));
  after.entered = undefined;
});

type Options = Partial<AcquireNodeInstanceLockOptions> & {
  readonly waited?: string[][];
};

/** Takes the lock in a scope of its own; resolves once it is held. */
const acquire = async (
  options: Options = {},
): Promise<{ lock: NodeInstanceLock; scope: Scope.CloseableScope }> => {
  const scope = Effect.runSync(Scope.make());
  scopes.push(scope);
  const lock = await Effect.runPromise(
    acquireNodeInstanceLock({
      connectionString,
      globals: livenessGlobals(),
      waiting: (reasons) =>
        Effect.sync(() => options.waited?.push([...reasons])),
      retryInitialMs: 20,
      retryMaxMs: 50,
      ...options,
    }).pipe(Scope.extend(scope)),
  );
  return { lock, scope };
};

const release = (scope: Scope.CloseableScope) =>
  Effect.runPromise(Scope.close(scope, Exit.void));

/** Ends, from another session, every session holding an advisory lock here. */
const endLockSessions = () =>
  Effect.runPromise(
    Effect.provide(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql<{ ended: boolean }>`
          SELECT pg_terminate_backend(pid) AS ended FROM pg_locks
          WHERE locktype = 'advisory' AND granted
            AND database = (
              SELECT oid FROM pg_database WHERE datname = current_database()
            )`,
      ),
      PgClient.layer({ url: Redacted.make(connectionString) }),
    ),
  );

/** A local port nothing listens on. */
const closedPort = async (): Promise<number> => {
  const server = createServer();
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  await new Promise<void>((resolve, reject) =>
    server.close((error) => (error ? reject(error) : resolve())),
  );
  return port;
};

/** A sleep the test lets go of, one wake-up at a time. */
const steppedSleep = () => {
  const sleepers: (() => void)[] = [];
  return {
    timers: {
      sleep: () =>
        new Promise<void>((resolve) => {
          sleepers.push(resolve);
        }),
    },
    sleeping: () => sleepers.length,
    wake: () => sleepers.shift()?.(),
  };
};

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
