/**
 * Liveness of the node's landed state queue (plan §5.5 P1, §15 N2), read
 * from the node database: an unhealthy queue stops every proposal with its
 * named reason while reads keep serving its walk, and startup waits on it
 * without failing or exiting, naming what it waits on, until it is healthy.
 * The wait for P1 to show a confirmed transaction re-reads it only while it
 * waits on the follower or the database failed transiently, at most its
 * retries; an unhealthy queue or any other read failure ends it at once.
 * (`/readyz` naming `state_queue_unhealthy` while the process stays live is
 * `readiness-honest-degradation-route.test.ts`; the driver's hold is
 * `l1-state-queue-hook.test.ts`.)
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { credentialToAddress, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either, Fiber, Option, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import {
  awaitLandedStateQueueOnStartup,
  landedStateQueueStartupReasons,
} from "../src/commands/listen-startup.await-landed-state-queue.js";
import {
  landedElements,
  STATE_QUEUE_UNHEALTHY,
} from "../src/l1-state-queue/index.js";
import { Globals } from "../src/services/globals.js";
import {
  awaitLandedStateQueueSnapshot,
  readLandedStateQueue,
  requireLandedStateQueue,
} from "../src/services/landed-state-queue.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import { seedLandedStateQueue } from "./helpers/landed-state-queue.js";
import {
  nodeDatum,
  rootDatum,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";
import {
  followingAtTip,
  runningFollower,
} from "./readiness-l1-follower.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

const GENESIS = "00".repeat(28);
const POLICY = "71".repeat(28);
const stateQueue = {
  spendingScriptAddress: credentialToAddress("Preprod", {
    type: "Script",
    hash: "5a".repeat(28),
  }),
  policyId: POLICY,
};

const utxo = (nonce: number, assetName: string, datum: Buffer): UTxO => ({
  txHash: nonce.toString(16).padStart(64, "0"),
  outputIndex: 0,
  address: stateQueue.spendingScriptAddress,
  assets: { lovelace: 5_000_000n, [toUnit(POLICY, assetName)]: 1n },
  datum: datum.toString("hex"),
});

const first = simHeader(1, GENESIS);
const firstHash = SDK.stateQueueHeaderHash(first);
const orphan = simHeader(2, GENESIS);
const healthyQueue = [
  utxo(1, SDK.STATE_QUEUE_ROOT_ASSET_NAME, rootDatum(GENESIS, firstHash)),
  utxo(
    2,
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + firstHash,
    nodeDatum(first, "Unattested", null),
  ),
];
const withOrphan = [
  ...healthyQueue,
  utxo(
    3,
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + SDK.stateQueueHeaderHash(orphan),
    nodeDatum(orphan, "Unattested", null),
  ),
];

const clearFollowerView = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql`DELETE FROM l1_follower_cursor`,
);

const run = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Globals | MidgardContracts | SqlClient.SqlClient | never
  >,
  globals: Globals,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      effect.pipe(
        Effect.provideService(Globals, globals),
        Effect.provideService(MidgardContracts, { stateQueue } as never),
      ),
    ),
  );

describe("the landed state queue's liveness", () => {
  it("stops proposals on an unhealthy queue while reads keep serving its walk", async () => {
    const globals = await Effect.runPromise(
      Globals.pipe(Effect.provide(Globals.Default)),
    );
    await run(
      Effect.gen(function* () {
        yield* seedLandedStateQueue(stateQueue, withOrphan, 10);
        const read = yield* readLandedStateQueue(stateQueue);
        expect(read.kind).toBe("ok");
        if (read.kind !== "ok") return;
        expect(read.queue).toMatchObject({
          healthy: false,
          reason: "orphan_node",
        });
        expect(landedElements(read.queue).map((e) => e.headerHash)).toEqual([
          GENESIS,
          firstHash,
        ]);
        const refused = yield* Effect.either(
          requireLandedStateQueue(stateQueue, "the commit"),
        );
        expect(Either.isLeft(refused)).toBe(true);
        if (Either.isLeft(refused))
          expect(refused.left.message).toBe(
            "The landed state queue is unhealthy (orphan_node); the commit stops",
          );
        // Healthy again, the same read proposes.
        yield* seedLandedStateQueue(stateQueue, healthyQueue, 11);
        const healthy = yield* requireLandedStateQueue(
          stateQueue,
          "the commit",
        );
        expect(healthy.nodes.map((node) => node.headerHash)).toEqual([
          firstHash,
        ]);
      }),
      globals,
    );
  });

  it("holds startup on each named reason, never failing, until the queue is healthy", async () => {
    const globals = await Effect.runPromise(
      Globals.pipe(Effect.provide(Globals.Default)),
    );
    await run(
      Effect.gen(function* () {
        // The follower has not started (the default state).
        expect(yield* landedStateQueueStartupReasons).toEqual([
          "l1_follower_unconfigured",
        ]);
        // Running but behind the tip.
        yield* Ref.set(
          globals.L1_FOLLOWER,
          runningFollower(followingAtTip({ atTip: false })),
        );
        expect(yield* landedStateQueueStartupReasons).toEqual([
          "l1_follower_catching_up",
        ]);
        // At the tip without a view: P1 cannot be read.
        yield* Ref.set(globals.L1_FOLLOWER, runningFollower());
        yield* clearFollowerView;
        expect(yield* landedStateQueueStartupReasons).toEqual([
          "state_queue_unavailable",
        ]);

        // Unhealthy: the wait reports it and keeps waiting.
        yield* seedLandedStateQueue(stateQueue, withOrphan, 20);
        const reported: (readonly string[])[] = [];
        const wait = yield* Effect.fork(
          awaitLandedStateQueueOnStartup(
            (reasons) => Effect.sync(() => reported.push(reasons)),
            "5 millis",
          ),
        );
        // Wait for the first report rather than a fixed delay: a loaded
        // runner can take longer than any fixed sleep to reach it. Then
        // keep waiting a while so a repeated report would show.
        yield* Effect.sync(() => reported.length).pipe(
          Effect.repeat({
            schedule: Schedule.spaced("5 millis"),
            until: (length) => length > 0,
          }),
        );
        yield* Effect.sleep("100 millis");
        expect(reported).toEqual([[STATE_QUEUE_UNHEALTHY]]);
        expect(Option.isNone(yield* Fiber.poll(wait))).toBe(true);

        // Healthy: the wait reports nothing left and returns.
        yield* seedLandedStateQueue(stateQueue, healthyQueue, 21);
        yield* Fiber.join(wait);
        expect(reported).toEqual([[STATE_QUEUE_UNHEALTHY], []]);
      }),
      globals,
    );
  });
});

/** The real database, counting P1's read transactions. */
const countingReads = <A, E, R>(
  effect: Effect.Effect<A, E, R | SqlClient.SqlClient>,
  reads: { count: number },
  fail?: () => Error,
) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) => {
    const counted = new Proxy(sql, {
      get: (target, property, receiver) => {
        if (property === "withTransaction")
          return (work: Effect.Effect<unknown, unknown, unknown>) =>
            Effect.suspend(() => {
              reads.count += 1;
              if (reads.count > 200) throw new Error("retried without bound");
              return fail === undefined
                ? target.withTransaction(work)
                : Effect.fail(fail());
            });
        const value: unknown = Reflect.get(target, property, receiver);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    return Effect.provideService(effect, SqlClient.SqlClient, counted);
  });

const FAST = { delay: "5 millis", retries: 4 } as const;
const snapshotWhen = (landed: () => boolean) =>
  Effect.either(
    awaitLandedStateQueueSnapshot(
      stateQueue,
      "post_merge",
      landed,
      "the probe",
      FAST,
    ),
  );

describe("the wait for P1 to show a transaction, by failure class", () => {
  it("re-reads while the queue does not show it yet, and fails once its retries run out", async () => {
    const globals = await Effect.runPromise(
      Globals.pipe(Effect.provide(Globals.Default)),
    );
    await run(
      Effect.gen(function* () {
        yield* seedLandedStateQueue(stateQueue, healthyQueue, 30);
        let polls = 0;
        const shown = yield* snapshotWhen(() => (polls += 1) >= 3);
        expect(Either.isRight(shown)).toBe(true);
        expect(polls).toBe(3);
        const reads = { count: 0 };
        const never = yield* countingReads(
          snapshotWhen(() => false),
          reads,
        );
        expect(Either.isLeft(never)).toBe(true);
        if (Either.isLeft(never))
          expect(never.left.message).toContain("does not show the probe yet");
        expect(reads.count).toBe(FAST.retries + 1);
      }),
      globals,
    );
  });

  it("ends at once on an unhealthy queue or a read failure no wait fixes, and retries a transient one", async () => {
    const globals = await Effect.runPromise(
      Globals.pipe(Effect.provide(Globals.Default)),
    );
    await run(
      Effect.gen(function* () {
        yield* seedLandedStateQueue(stateQueue, withOrphan, 31);
        const unhealthy = { count: 0 };
        const refused = yield* countingReads(
          snapshotWhen(() => true),
          unhealthy,
        );
        expect(Either.isLeft(refused)).toBe(true);
        expect(unhealthy.count).toBe(1);

        const broken = { count: 0 };
        yield* countingReads(
          snapshotWhen(() => true),
          broken,
          () =>
            Object.assign(new Error("relation does not exist"), {
              code: "42P01",
            }),
        );
        expect(broken.count).toBe(1);

        const down = { count: 0 };
        yield* countingReads(
          snapshotWhen(() => true),
          down,
          () =>
            Object.assign(new Error("connect ECONNREFUSED"), {
              code: "ECONNREFUSED",
            }),
        );
        expect(down.count).toBe(FAST.retries + 1);
      }),
      globals,
    );
  });
});
