/**
 * Startup's two follower waits, by failure class (owner ruling 2026-10-09:
 * retry only what is transient, under a budget). The landed-state-queue
 * wait rides out a transient database failure within
 * `STARTUP_DATABASE_BUDGET`, fails startup once one outlives it, and fails
 * at once on any other read failure. The follower-view wait fails startup at
 * once on a driver failure hold that is not retried, fails a retried one
 * that outlives its budget, and keeps waiting on a retried one within it.
 * Both fail with `StartupStepFailedError` naming the step and reason.
 */
import "./utils.js";

import { SqlClient } from "@effect/sql";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { type Duration, Effect, Either, Fiber, Option, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { awaitFollowerViewOnStartup } from "../src/commands/listen-startup.await-follower-view.js";
import {
  awaitLandedStateQueueOnStartup,
  STATE_QUEUE_UNAVAILABLE,
} from "../src/commands/listen-startup.await-landed-state-queue.js";
import {
  type DriverHold,
  failureHold,
  notRetried,
} from "../src/l1-events/driver.js";
import { Globals } from "../src/services/globals.js";
import type { L1FollowerHandle } from "../src/services/l1-follower.readiness.js";
import { STARTUP_PREPARATION_FAILED } from "../src/services/l1-follower.recompute.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import type { StartupStepFailedError } from "../src/services/startup-waiting.js";
import { followingAtTip } from "./readiness-l1-follower.fixture.js";

const stateQueue = {
  spendingScriptAddress: credentialToAddress("Preprod", {
    type: "Script",
    hash: "5a".repeat(28),
  }),
  policyId: "71".repeat(28),
};

/** A connection refused: the class a database outage fails with. */
const refused = () =>
  Object.assign(new Error("connect ECONNREFUSED 127.0.0.1:5433"), {
    code: "ECONNREFUSED",
  });
/** A failure no wait fixes (an undefined table). */
const broken = () =>
  Object.assign(new Error('relation "l1_follower_cursor" does not exist'), {
    code: "42P01",
  });

/**
 * A database whose every transaction fails with `fail()`, counting the
 * reads; the guard ends a wait that would otherwise be retried without bound.
 */
const failingDatabase = (fail: () => Error) => {
  let reads = 0;
  const sql = {
    withTransaction: () =>
      Effect.suspend(() => {
        reads += 1;
        if (reads > 500) throw new Error("retried without bound");
        return Effect.fail(fail());
      }),
  } as unknown as SqlClient.SqlClient;
  return { sql, reads: () => reads };
};

const caughtUp = (holds: () => readonly DriverHold[] = () => []) =>
  ({
    kind: "running",
    status: () => followingAtTip(),
    holds,
    planCurrent: () => Promise.resolve({ kind: "none", detail: "fixture" }),
  }) satisfies L1FollowerHandle;

const makeGlobals = () =>
  Effect.runPromise(Globals.pipe(Effect.provide(Globals.Default)));

const stateQueueWait = async (
  fail: () => Error,
  budget: Duration.DurationInput,
): Promise<{
  result: Either.Either<void, StartupStepFailedError>;
  reads: number;
  reported: (readonly string[])[];
}> => {
  const globals = await makeGlobals();
  await Effect.runPromise(Ref.set(globals.L1_FOLLOWER, caughtUp()));
  const database = failingDatabase(fail);
  const reported: (readonly string[])[] = [];
  const result = await Effect.runPromise(
    Effect.either(
      awaitLandedStateQueueOnStartup(
        (reasons) => Effect.sync(() => reported.push(reasons)),
        "5 millis",
        budget,
      ),
    ).pipe(
      Effect.provideService(Globals, globals),
      Effect.provideService(SqlClient.SqlClient, database.sql),
      Effect.provideService(MidgardContracts, { stateQueue } as never),
      Effect.timeoutFail({
        duration: "5 seconds",
        onTimeout: () => new Error("the wait did not end"),
      }),
    ),
  );
  return { result, reads: database.reads(), reported };
};

describe("startup's landed-state-queue wait, by failure class", () => {
  it("names a transient database failure and fails, exhausted, once it outlives the budget", async () => {
    const { result, reads, reported } = await stateQueueWait(
      refused,
      "60 millis",
    );
    expect(reported).toEqual([[STATE_QUEUE_UNAVAILABLE]]);
    // Retried more than once within the budget.
    expect(reads).toBeGreaterThan(2);
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result))
      expect(result.left).toMatchObject({
        _tag: "StartupStepFailedError",
        step: "l1_follower_catch_up",
        reason: STATE_QUEUE_UNAVAILABLE,
        exhausted: true,
      });
  });

  it("fails at once, not exhausted, on a read failure no wait fixes", async () => {
    const { result, reads } = await stateQueueWait(broken, "10 seconds");
    expect(reads).toBe(1);
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left).toMatchObject({
        step: "l1_follower_catch_up",
        reason: STATE_QUEUE_UNAVAILABLE,
        exhausted: false,
      });
      expect(result.left.message).toContain("does not exist");
    }
  });

  it("rides out a transient database failure within the budget", async () => {
    const globals = await makeGlobals();
    await Effect.runPromise(Ref.set(globals.L1_FOLLOWER, caughtUp()));
    const database = failingDatabase(refused);
    const outcome = await Effect.runPromise(
      Effect.gen(function* () {
        const wait = yield* Effect.fork(
          awaitLandedStateQueueOnStartup(
            () => Effect.void,
            "5 millis",
            "10 seconds",
          ),
        );
        yield* Effect.sleep("150 millis");
        return yield* Fiber.poll(wait);
      }).pipe(
        Effect.provideService(Globals, globals),
        Effect.provideService(SqlClient.SqlClient, database.sql),
        Effect.provideService(MidgardContracts, { stateQueue } as never),
      ),
    );
    expect(Option.isNone(outcome)).toBe(true);
    expect(database.reads()).toBeGreaterThan(2);
  });
});

const followerViewWait = async (
  holds: () => readonly DriverHold[],
  budget: Duration.DurationInput,
  observeMs: number,
) => {
  const globals = await makeGlobals();
  await Effect.runPromise(Ref.set(globals.L1_FOLLOWER, caughtUp(holds)));
  return Effect.runPromise(
    Effect.gen(function* () {
      const wait = yield* Effect.fork(
        awaitFollowerViewOnStartup(() => Effect.void, "5 millis", budget),
      );
      yield* Effect.sleep(observeMs);
      const polled = yield* Fiber.poll(wait);
      yield* Fiber.interrupt(wait);
      return polled;
    }).pipe(Effect.provideService(Globals, globals)),
  );
};

const transientHold = () =>
  failureHold(STARTUP_PREPARATION_FAILED, "store refused", refused());

describe("startup's follower-view wait, by failure class", () => {
  it("fails at once on a startup-preparation failure the driver does not retry", async () => {
    const hold = failureHold(
      STARTUP_PREPARATION_FAILED,
      "mutation jobs recovery check: broken",
      broken(),
    );
    const polled = await followerViewWait(() => [hold], "10 seconds", 100);
    expect(Option.isSome(polled)).toBe(true);
    if (Option.isSome(polled) && polled.value._tag === "Failure") {
      const failure = polled.value.cause;
      expect(String(failure)).toContain(
        "startup step follower_view_apply failed: reason=startup_preparation_failed",
      );
    } else expect.fail("the wait did not fail");
  });

  it("keeps waiting on a transient one within the budget", async () => {
    const hold = transientHold();
    const polled = await followerViewWait(() => [hold], "10 seconds", 150);
    expect(Option.isNone(polled)).toBe(true);
  });

  it("fails, exhausted, on a transient one that outlives the budget", async () => {
    const hold = transientHold();
    const polled = await followerViewWait(() => [hold], "60 millis", 400);
    expect(Option.isSome(polled)).toBe(true);
    if (Option.isSome(polled) && polled.value._tag === "Failure")
      expect(String(polled.value.cause)).toContain(
        "a transient failure outlived the step's budget",
      );
    else expect.fail("the wait did not fail");
  });

  it("does not fail on a not-retried hold that is a wait, not a failure", async () => {
    const hold = notRetried({
      reason: "intent_resubmit_rejected",
      detail: "refused at every tip",
    });
    const polled = await followerViewWait(() => [hold], "60 millis", 200);
    expect(Option.isNone(polled)).toBe(true);
  });
});
