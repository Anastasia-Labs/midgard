/**
 * Deciding that no rebase is due reads no `confirmed_ledger` row: a
 * follower tick with nothing new, after the rebase onto a processed block
 * ran, and a `rebaseIfDue` with the native MPF at the target's root, scan
 * the confirmed ledger zero times (`confirmed_ledger_full_scan_total`).
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import { Effect, Metric } from "effect";
import { afterEach, expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { createFollowerDriver } from "../src/l1-events/driver.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { driverSink } from "../src/services/l1-follower.driver-sink.js";
import { failureHold } from "../src/services/l1-follower.failure-hold.js";
import { planCurrentView } from "../src/services/l1-follower.readiness.js";
import { followerTick } from "../src/services/l1-follower.tick.js";
import { Lucid } from "../src/services/lucid.js";
import {
  modelSlotLucid,
  testDriverRecompute,
} from "./helpers/driver-recompute.js";
import { openNodeFollowerStore } from "./helpers/forced-orders-node-store.js";
import { EVENTS_CONFIG } from "./helpers/l1-events-chain.js";
import { ChainDriver } from "./helpers/l1-events-store.js";
import {
  freshNative,
  processOf,
  R1,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";

const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const scansOf = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const before = (yield* Metric.value(fullScanCounter)).count;
    const result = yield* effect;
    const after = (yield* Metric.value(fullScanCounter)).count;
    return { result, scans: after - before };
  });

it("scans confirmed_ledger zero times on a follower tick with nothing new and on a rebaseIfDue that is not due", async () => {
  const native = freshNative();
  const globals = await processOf(native);
  // A processed foreign block the working ledger does not hold yet.
  await seed(globals);
  const store = await openNodeFollowerStore(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const chain = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await chain.init();
  await run(
    globals,
    Effect.gen(function* () {
      const recompute = yield* testDriverRecompute({
        planCurrent: () => planCurrentView(store, EVENTS_CONFIG),
      });
      const sink = yield* driverSink(recompute).pipe(
        Effect.provideService(Lucid, modelSlotLucid),
      );
      const tick = followerTick({
        driver: createFollowerDriver({
          failureHold,
          store,
          config: EVENTS_CONFIG,
          sink,
        }),
        nodeBehind: () => false,
        recompute,
        refreshJournal: () => Effect.void,
      });

      // The rebase onto the block runs.
      const first = yield* tick;
      expect(first.holds).toEqual([]);
      expect(first.dispositionHold).toBeUndefined();
      expect((yield* retrieveRows).map((row) => row.applied)).toEqual([true]);
      expect(native.durableRoot).toBe(R1);

      const again = yield* scansOf(tick);
      expect(again.result.holds).toEqual([]);
      expect(again.result.dispositionHold).toBeUndefined();
      expect(again.scans).toBe(0);

      const notDue = yield* scansOf(recompute.rebaseIfDue("not due"));
      expect(notDue.result).toBeUndefined();
      expect(notDue.scans).toBe(0);
    }),
  );
}, 60_000);
