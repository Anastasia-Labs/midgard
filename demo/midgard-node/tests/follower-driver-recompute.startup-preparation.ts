/**
 * The follower-change driver's startup preparation
 * (`follower-driver-recompute.test.ts`): a failed preparation is the driver
 * sink's named hold, the gate stays pending, and the next run prepares and
 * opens it; nothing fails. The hold is retried on the driver's backoff only
 * when the failure is transient (`isRetriedHold`); startup fails on one that
 * is not.
 */
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { classifyChange, isRetriedHold } from "../src/l1-events/driver.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  readFollowerWriteGate,
  runAtFollowerView,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import { followerWriteGateReasons } from "../src/services/follower-write-gate.local.js";
import { Globals } from "../src/services/globals.globals.js";
import { driverSink } from "../src/services/l1-follower.driver-sink.js";
import { STARTUP_PREPARATION_FAILED } from "../src/services/l1-follower.recompute.js";
import { Lucid } from "../src/services/lucid.js";
import {
  DRIVER_TEST_SLOT,
  holdOf,
  modelSlotLucid,
  testDriverRecompute,
} from "./helpers/driver-recompute.js";
import { writeFollowerView } from "./helpers/follower-view.js";
import { freshNative, processOf, run } from "./landed-blocks-rebase.fixture.js";
import { resetApplicationTables } from "./utils.js";

describe("the driver's startup preparation", () => {
  it("is a named hold of the driver's first view while it fails, retried until it runs", async () => {
    const globals = await processOf(freshNative());
    let failing = true;
    let prepared = 0;
    await run(
      globals,
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const recompute = yield* testDriverRecompute({
          startupPreparation: Effect.suspend(() =>
            failing
              ? Effect.fail(new Error("the startup preparation failed"))
              : // It runs under the driver's write capability.
                withFollowerWrite(
                  Effect.sync(() => {
                    prepared += 1;
                  }),
                ),
          ),
        });
        const sink = yield* driverSink(recompute).pipe(
          Effect.provideService(Lucid, modelSlotLucid),
        );
        const plan = yield* writeFollowerView(DRIVER_TEST_SLOT, []);
        const change = classifyChange(null, plan.view);
        const first = yield* Effect.promise(() => sink.apply(change, plan));
        expect(first).toEqual({
          kind: "held",
          hold: {
            reason: STARTUP_PREPARATION_FAILED,
            detail: expect.stringContaining("the startup preparation failed"),
          },
        });
        // An unclassified failure: no timer retry, startup fails on it.
        if (first.kind === "held")
          expect(isRetriedHold(first.hold)).toBe(false);
        const gate = yield* readFollowerWriteGate;
        expect(gate.pending?.reason).toBe(STARTUP_PREPARATION_FAILED);
        expect(gate.applied).toBeUndefined();
        expect(
          followerWriteGateReasons(yield* Ref.get(globals.FOLLOWER_WRITE_GATE)),
        ).toEqual([DRIVER_RECOMPUTE_PENDING]);
        expect(
          holdOf(yield* Effect.either(runAtFollowerView(Effect.void))),
        ).toBe(DRIVER_RECOMPUTE_PENDING);

        // The next driver run retries it.
        const again = yield* Effect.promise(() => sink.apply(change, plan));
        expect(again).toMatchObject({
          kind: "held",
          hold: { reason: STARTUP_PREPARATION_FAILED },
        });
        failing = false;
        const ran = yield* Effect.promise(() => sink.apply(change, plan));
        expect(ran).toMatchObject({ kind: "applied" });
        expect(prepared).toBe(1);
        expect((yield* readFollowerWriteGate).pending).toBeUndefined();
        // Once per process: the next recompute does not prepare again.
        expect((yield* recompute.run("a later recompute")).published).toBe(
          true,
        );
        expect(prepared).toBe(1);
        yield* runAtFollowerView(Effect.void);
      }).pipe(Effect.provideService(Globals, globals)),
    );
  });

  it("is a hold the driver retries on its backoff when the failure is transient", async () => {
    const globals = await processOf(freshNative());
    await run(
      globals,
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const recompute = yield* testDriverRecompute({
          startupPreparation: Effect.fail(
            new Error("mutation jobs recovery check", {
              cause: Object.assign(
                new Error("connect ECONNREFUSED 127.0.0.1:5433"),
                { code: "ECONNREFUSED" },
              ),
            }),
          ),
        });
        const sink = yield* driverSink(recompute).pipe(
          Effect.provideService(Lucid, modelSlotLucid),
        );
        const plan = yield* writeFollowerView(DRIVER_TEST_SLOT, []);
        const held = yield* Effect.promise(() =>
          sink.apply(classifyChange(null, plan.view), plan),
        );
        expect(held).toMatchObject({
          kind: "held",
          hold: { reason: STARTUP_PREPARATION_FAILED },
        });
        if (held.kind === "held") expect(isRetriedHold(held.hold)).toBe(true);
      }).pipe(Effect.provideService(Globals, globals)),
    );
  });
});
