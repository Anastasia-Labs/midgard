import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Duration, Effect, Either, Exit, Fiber, Redacted, Ref } from "effect";
import { afterAll, beforeAll, expect, it } from "vitest";

import * as MutationJobs from "../src/database/mutationJobs.js";
import { settleLeaseDurably } from "../src/database/stateQueueMutationLeases.settle.js";
import type { Globals } from "../src/services/globals.globals.js";
import { initialL1ControlPlaneActivity } from "../src/services/globals.l1-control-plane.activity.js";
import { withL1ControlPlane } from "../src/services/globals.l1-control-plane.js";
import { L1ControlPlaneTimeoutError } from "../src/services/globals.next-l1-provider-health-evidence.js";
import { TEST_DATABASE_PREFIX } from "./test-env.js";

const database = `${TEST_DATABASE_PREFIX}_hold_${Date.now()}`;
if (!/^[a-z_][a-z0-9_]{0,62}$/u.test(database))
  throw new Error("Protected SQL fixture requires a valid owned database name");
const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";
let createdDatabase = false;
const sqlLayer = (name: string) =>
  PgClient.layer({
    host: process.env.POSTGRES_HOST ?? "127.0.0.1",
    port: Number(process.env.POSTGRES_PORT ?? "5433"),
    username: process.env.POSTGRES_USER ?? "postgres",
    password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
    database: name,
    maxConnections: 1,
    applicationName: "codex_rel_protected_finalization",
    connectTimeout: Duration.seconds(5),
  });
const run = <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(Effect.provide(work, sqlLayer(database)));
const awaitEntry = <A, E>(
  entered: Promise<void>,
  owner: Fiber.RuntimeFiber<A, E>,
): Promise<void> =>
  Promise.race([
    entered,
    Effect.runPromise(Fiber.await(owner)).then((exit) => {
      if (Exit.isFailure(exit))
        return Effect.runPromise(Effect.failCause(exit.cause));
      throw new Error("Control-plane owner exited before work entry");
    }),
  ]);
const makeGlobals = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return {
        L1_CONTROL_PLANE: yield* Effect.makeSemaphore(1),
        L1_CONTROL_PLANE_ACTIVITY: yield* Ref.make(
          initialL1ControlPlaneActivity(),
        ),
      } as Globals;
    }),
  );
beforeAll(async () => {
  if (!dbEnabled) return;
  await Effect.runPromise(
    Effect.provide(
      Effect.flatMap(SqlClient.SqlClient, (sql) =>
        sql.unsafe(`CREATE DATABASE ${database}`).pipe(
          Effect.tap(() =>
            Effect.sync(() => {
              createdDatabase = true;
            }),
          ),
        ),
      ),
      sqlLayer("postgres"),
    ),
  );
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`CREATE TABLE local_mutation_jobs (job_id TEXT PRIMARY KEY,status TEXT NOT NULL,last_error TEXT,completed_at TIMESTAMPTZ,updated_at TIMESTAMPTZ)`;
      yield* sql`CREATE TABLE state_queue_mutation_leases (token TEXT PRIMARY KEY,status TEXT NOT NULL,released_at TIMESTAMPTZ,last_error TEXT)`;
    }),
  );
});

afterAll(async () => {
  if (!createdDatabase) return;
  await Effect.runPromise(
    Effect.provide(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const [sessions] = yield* sql<{
          count: number;
        }>`SELECT count(*)::integer AS count FROM pg_stat_activity WHERE datname=${database}`;
        expect(sessions?.count).toBe(0);
        yield* sql.unsafe(`DROP DATABASE ${database}`);
        createdDatabase = false;
        const [remaining] = yield* sql<{
          present: boolean;
        }>`SELECT EXISTS(SELECT 1 FROM pg_database WHERE datname=${database}) AS present`;
        expect(remaining?.present).toBe(false);
      }),
      sqlLayer("postgres"),
    ),
  );
});

it.skipIf(!dbEnabled).each(["interruption", "hold-timeout"] as const)(
  "keeps owned SQL open through confirmed finalization and terminal lease writes on %s",
  async (trigger) => {
    const token = randomUUID();
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO local_mutation_jobs (job_id,status) VALUES (${token},'running')`;
        yield* sql`INSERT INTO state_queue_mutation_leases (token,status) VALUES (${token},'active')`;
      }),
    );
    const globals = await makeGlobals();
    let enter!: () => void;
    const entered = new Promise<void>((resolve) => {
      enter = resolve;
    });
    let release!: () => void;
    const paused = new Promise<void>((resolve) => {
      release = resolve;
    });
    let writeSucceeded = false;
    const protectedWrites = Effect.uninterruptible(
      Effect.gen(function* () {
        yield* Effect.sync(enter);
        yield* Effect.promise(() => paused);
        yield* MutationJobs.markCompleted(token);
        writeSucceeded = true;
      }),
    ).pipe(
      Effect.ensuring(
        settleLeaseDurably(token, "state_queue_merge", { kind: "released" }),
      ),
    );
    const fiber = Effect.runFork(
      Effect.provide(
        withL1ControlPlane(
          globals,
          {
            scope: "state_queue_merge",
            maxHoldMs: trigger === "hold-timeout" ? 20 : 10000,
          },
          protectedWrites,
        ),
        sqlLayer(database),
      ),
    );
    try {
      await awaitEntry(entered, fiber);
      let joined = false;
      const settled =
        trigger === "interruption"
          ? Effect.runPromise(Fiber.interrupt(fiber))
          : Effect.runPromise(Fiber.await(fiber));
      const completion = settled.then((exit) => {
        joined = true;
        return exit;
      });
      // One actual scheduling turn after cancellation/deadline; SQL must remain usable.
      await new Promise((resolve) => setTimeout(resolve, 40));
      expect(joined).toBe(false);
      release();
      const outcome = await completion;
      const state = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const [job] = yield* sql<{
            status: string;
          }>`SELECT status FROM local_mutation_jobs WHERE job_id=${token}`;
          const [lease] = yield* sql<{
            status: string;
          }>`SELECT status FROM state_queue_mutation_leases WHERE token=${token}`;
          return { job: job!.status, lease: lease!.status };
        }),
      );
      expect({ ...state, writeSucceeded }).toEqual({
        job: "completed",
        lease: "released",
        writeSucceeded: true,
      });
      expect(
        (await Effect.runPromise(Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)))
          .holder,
      ).toBeNull();
      expect(Exit.isFailure(outcome)).toBe(true);
      if (trigger === "interruption")
        expect(Exit.isInterrupted(outcome)).toBe(true);
    } finally {
      release();
      await Effect.runPromise(Fiber.interrupt(fiber));
    }
  },
);

it("preserves successful work, its own failure, and an interruptible hold timeout", async () => {
  const globals = await makeGlobals();
  const held = <A, E>(work: Effect.Effect<A, E>) =>
    withL1ControlPlane(globals, { scope: "controls", maxHoldMs: 20 }, work);
  expect(await Effect.runPromise(held(Effect.succeed(7)))).toBe(7);
  const failure = new Error("original work failure");
  const failed = await Effect.runPromise(
    Effect.either(held(Effect.fail(failure))),
  );
  expect(Either.isLeft(failed) && failed.left).toBe(failure);
  const timedOut = await Effect.runPromise(Effect.either(held(Effect.never)));
  expect(Either.isLeft(timedOut) && timedOut.left).toBeInstanceOf(
    L1ControlPlaneTimeoutError,
  );
  expect(
    (await Effect.runPromise(Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)))
      .holder,
  ).toBeNull();
});

it("still interrupts ordinary work and releases its control-plane permit", async () => {
  const globals = await makeGlobals();
  let enter!: () => void;
  const entered = new Promise<void>((resolve) => {
    enter = resolve;
  });
  const fiber = Effect.runFork(
    withL1ControlPlane(
      globals,
      { scope: "ordinary", maxHoldMs: 10000 },
      Effect.sync(enter).pipe(Effect.zipRight(Effect.never)),
    ),
  );
  try {
    await awaitEntry(entered, fiber);
    expect(
      Exit.isInterrupted(await Effect.runPromise(Fiber.interrupt(fiber))),
    ).toBe(true);
    expect(
      (await Effect.runPromise(Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)))
        .holder,
    ).toBeNull();
    expect(
      await Effect.runPromise(
        withL1ControlPlane(
          globals,
          { scope: "next", maxHoldMs: 1000 },
          Effect.succeed("next"),
        ),
      ),
    ).toBe("next");
  } finally {
    await Effect.runPromise(Fiber.interrupt(fiber));
  }
});
