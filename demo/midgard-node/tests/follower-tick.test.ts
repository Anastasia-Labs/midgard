/**
 * The node follower's composition (`followerTick`, `l1-follower.tick.ts`)
 * as the follower runs it: the production follower-change driver over a
 * follower store in the node database, its sink and recompute, under the
 * coalesced runner's backoff (`coalescedRunner`).
 *
 * A deposit a processed landed block includes is admitted, then the
 * follower rewinds past its admission: the run is held by name
 * (`l1_events_orphan_recovery`), the runner retries it on its backoff with
 * no trigger, and once the follower admits the deposit again the next retry
 * clears the hold, opens the write gate and keeps the deposit.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  createFollowerDriver,
  EVENTS_ORPHAN_RECOVERY,
} from "../src/l1-events/driver.js";
import { readFollowerWriteGate } from "../src/services/follower-write-gate.js";
import { coalescedRunner } from "../src/services/l1-follower.coalesced-runner.js";
import { driverSink } from "../src/services/l1-follower.driver-sink.js";
import { planCurrentView } from "../src/services/l1-follower.readiness.js";
import {
  type FollowerTick,
  followerTick,
} from "../src/services/l1-follower.tick.js";
import { Lucid } from "../src/services/lucid.js";
import {
  modelSlotLucid,
  testDriverRecompute,
} from "./helpers/driver-recompute.js";
import { openNodeFollowerStore } from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventIdOf,
  eventOrder,
  EVENTS_CONFIG,
} from "./helpers/l1-events-chain.js";
import { ChainDriver } from "./helpers/l1-events-store.js";
import {
  BLOCK,
  freshNative,
  processOf,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";

const K = 4;
const NONCE = { txHash: Buffer.alloc(32, 0xd5), index: 1 };
const EVENT_ID = Buffer.from(
  Data.to(eventIdOf(NONCE), SDK.OutputReference),
  "hex",
);
const admission = () =>
  admissionTx(eventOrder("deposit", NONCE, { inclusionTime: 1_000n }), 1);

const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** Polls `done` every 25 ms until it holds, for up to 20 s. */
const until = async (done: () => boolean, what: string) => {
  const deadline = Date.now() + 20_000;
  while (!done()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};

describe("the follower's composition", () => {
  it("holds a run by name while a landed block holds an orphaned deposit, retries it on the backoff, and clears once the admission returns", async () => {
    const globals = await processOf(freshNative());
    // The processed landed block BLOCK includes the deposit; its rebase is due.
    await seed(globals, { depositIds: [EVENT_ID] });
    const store = await openNodeFollowerStore(
      [eventProjection(EVENTS_CONFIG)],
      K,
    );
    opened.push(store);
    const chain = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
    await chain.init();
    await chain.forward([admission()]);

    const deposits = Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{
        id: Buffer;
        status: string;
        projected: Buffer | null;
      }>`SELECT event_id AS id, status::text AS status,
        projected_header_hash AS projected FROM deposits_utxos`,
    );
    const abort = new AbortController();
    try {
      await run(
        globals,
        Effect.gen(function* () {
          const recompute = yield* testDriverRecompute({
            planCurrent: () => planCurrentView(store, EVENTS_CONFIG),
          });
          const sink = yield* driverSink(recompute).pipe(
            Effect.provideService(Lucid, modelSlotLucid),
          );
          const driver = createFollowerDriver({
            store,
            config: EVENTS_CONFIG,
            sink,
          });
          const tick = followerTick({
            driver,
            nodeBehind: () => false,
            recompute,
            refreshJournal: () => Effect.void,
          });
          const ticks: FollowerTick[] = [];
          const trigger = coalescedRunner(
            () =>
              Effect.runPromise(tick).then((ran) => {
                ticks.push(ran);
                return ran.holds;
              }),
            abort.signal,
          );
          const reasons = (ran: FollowerTick | undefined) =>
            ran?.holds.map((hold) => hold.reason);

          // The first view: the deposit is ingested and the rebase onto
          // BLOCK carries it.
          trigger();
          yield* Effect.promise(() =>
            until(() => ticks.length === 1, "the first run"),
          );
          expect(reasons(ticks[0])).toEqual([]);
          const admitted = yield* deposits;
          expect(admitted).toHaveLength(1);
          expect(admitted[0]!.id.equals(EVENT_ID)).toBe(true);
          expect(admitted[0]!.projected).toEqual(Buffer.from(BLOCK, "hex"));
          expect((yield* readFollowerWriteGate).pending).toBeUndefined();

          // The follower rewinds past the admission: the deposit is an
          // orphan BLOCK holds, and the run is held by name.
          yield* Effect.promise(() => chain.backward(1));
          trigger();
          yield* Effect.promise(() =>
            until(() => ticks.length >= 3, "a held run and its retry"),
          );
          // Retried on the backoff with no trigger, still held, nothing fails.
          for (const ran of ticks.slice(1))
            expect(reasons(ran)).toEqual([EVENTS_ORPHAN_RECOVERY]);
          expect(ticks[1]!.driverRun).toMatchObject({
            kind: "ran",
            change: { kind: "rewind" },
            result: { kind: "held" },
          });
          expect((yield* readFollowerWriteGate).pending?.reason).toBe(
            EVENTS_ORPHAN_RECOVERY,
          );
          expect(yield* deposits).toHaveLength(1);

          // The follower admits the deposit again: the next retry clears.
          yield* Effect.promise(() => chain.forward([admission()]));
          const held = ticks.length;
          yield* Effect.promise(() =>
            until(
              () => ticks.length > held && reasons(ticks.at(-1))?.length === 0,
              "the cleared run",
            ),
          );
          expect((yield* readFollowerWriteGate).pending).toBeUndefined();
          const kept = yield* deposits;
          expect(kept).toHaveLength(1);
          expect(kept[0]!.projected).toEqual(Buffer.from(BLOCK, "hex"));
        }),
      );
    } finally {
      abort.abort();
    }
  }, 60_000);
});
