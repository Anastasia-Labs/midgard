import { Effect, Fiber, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import {
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
  USER_EVENT_INGESTION_HOLD_DURATION_FACTOR,
  userEventIngestionHoldMs,
} from "../src/fibers/user-event-ingestion.js";
import { extendL1ControlPlaneHold } from "../src/services/globals.l1-control-plane.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import { Globals } from "../src/services/index.js";

type VisibleDeposit = { readonly outRef: string; readonly amount: number };

const VISIBLE_SET: readonly VisibleDeposit[] = [
  { outRef: "aa#0", amount: 5 },
  { outRef: "bb#1", amount: 7 },
  { outRef: "cc#2", amount: 11 },
];

/**
 * A provider whose full-set read takes `delaysMs[attempt]` (the last entry
 * repeats), and a store that upserts by out-ref like DepositsDB.insertEntries.
 */
const harness = (delaysMs: readonly number[]) => {
  const store = new Map<string, VisibleDeposit>();
  const writes: string[] = [];
  let attempt = 0;
  const reconcile = Effect.gen(function* () {
    const delay = delaysMs[Math.min(attempt, delaysMs.length - 1)] ?? 0;
    attempt += 1;
    yield* Effect.sleep(delay);
    // The in-memory store needs no database.
    yield* persistVisibleUserEventUTxOs({
      visibleUtxos: VISIBLE_SET,
      toEntry: (utxo: VisibleDeposit) => Effect.succeed(utxo),
      insertEntries: (entries) =>
        Effect.sync(() => {
          for (const entry of entries) {
            writes.push(entry.outRef);
            store.set(entry.outRef, entry);
          }
        }),
      emptyLogMessage: "none",
      foundLogMessage: (count) => `${count} found`,
    }) as Effect.Effect<unknown>;
  });
  return { store, writes, reconcile, attempts: () => attempt };
};

const awaitCondition = (condition: () => Effect.Effect<boolean>) =>
  Effect.gen(function* () {
    while (!(yield* condition())) yield* Effect.sleep(5);
  }).pipe(Effect.timeoutFail({ duration: 10_000, onTimeout: () => "timeout" }));

describe("visible user-event ingestion hold", () => {
  it("derives the hold from the observed duration and failures, within floor and ceiling", () => {
    const bounds = { floorMs: 30_000, ceilingMs: 600_000 };
    expect(
      userEventIngestionHoldMs(
        {
          lastSuccessDurationMs: 0,
          consecutiveFailures: 0,
          consecutiveHoldTimeouts: 0,
        },
        bounds,
      ),
    ).toBe(30_000);
    expect(
      userEventIngestionHoldMs(
        {
          lastSuccessDurationMs: 40_000,
          consecutiveFailures: 0,
          consecutiveHoldTimeouts: 0,
        },
        bounds,
      ),
    ).toBe(USER_EVENT_INGESTION_HOLD_DURATION_FACTOR * 40_000);
    expect(
      userEventIngestionHoldMs(
        {
          lastSuccessDurationMs: 0,
          consecutiveFailures: 2,
          consecutiveHoldTimeouts: 2,
        },
        bounds,
      ),
    ).toBe(120_000);
    expect(
      userEventIngestionHoldMs(
        {
          lastSuccessDurationMs: 0,
          consecutiveFailures: 40,
          consecutiveHoldTimeouts: 40,
        },
        bounds,
      ),
    ).toBe(600_000);
    // Failures that were not hold timeouts leave the hold where it was.
    expect(
      userEventIngestionHoldMs(
        {
          lastSuccessDurationMs: 0,
          consecutiveFailures: 9,
          consecutiveHoldTimeouts: 0,
        },
        bounds,
      ),
    ).toBe(30_000);
  });

  it("outgrows a provider slower than the floor, reconciles the full set once it recovers, and raises then clears the stall reason", async () => {
    // Holds go 50, 100, 200, 400 ms: three reads of 300 ms time out, the
    // fourth fits.
    const h = harness([300, 300, 300, 300]);
    const outcome = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const fiber = yield* Effect.fork(
          repeatVisibleUserEventIngestionFiber({
            schedule: Schedule.spaced(1),
            startLogMessage: "start",
            spanName: "test_ingestion",
            action: h.reconcile,
            holdFloorMs: 50,
            holdCeilingMs: 10_000,
          }),
        );
        yield* awaitCondition(() =>
          currentLivenessReasons(globals).pipe(
            Effect.map((reasons) => reasons.length > 0),
          ),
        );
        const stalled = yield* currentLivenessReasons(globals);
        const storedWhileStalled = h.store.size;
        yield* awaitCondition(() => Effect.sync(() => h.store.size > 0));
        yield* awaitCondition(() =>
          currentLivenessReasons(globals).pipe(
            Effect.map((reasons) => reasons.length === 0),
          ),
        );
        // A few more reconciles of the same set must not duplicate anything.
        const attemptsAtRecovery = h.attempts();
        yield* awaitCondition(() =>
          Effect.sync(() => h.attempts() >= attemptsAtRecovery + 2),
        );
        yield* Fiber.interrupt(fiber);
        return { stalled, storedWhileStalled };
      }).pipe(Effect.provide(Globals.Default)),
    );
    // The L1 control plane reports the same streak of hold timeouts.
    expect(outcome.stalled).toEqual([
      "l1_control_plane_hold_timeouts:test_ingestion:3",
      "user_event_ingestion_stalled:test_ingestion:3",
    ]);
    expect(outcome.storedWhileStalled).toBe(0);
    expect([...h.store.keys()].sort()).toEqual(["aa#0", "bb#1", "cc#2"]);
    expect([...h.store.values()]).toEqual(
      expect.arrayContaining([...VISIBLE_SET]),
    );
    // Every reconcile writes the full set; each deposit is one row.
    expect(h.writes.length % VISIBLE_SET.length).toBe(0);
    expect(h.store.size).toBe(VISIBLE_SET.length);
  });

  it("raises nothing for a provider that stays within the hold", async () => {
    const h = harness([5]);
    const reasons = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const fiber = yield* Effect.fork(
          repeatVisibleUserEventIngestionFiber({
            schedule: Schedule.spaced(1),
            startLogMessage: "start",
            spanName: "fast_ingestion",
            action: h.reconcile,
            holdFloorMs: 200,
          }),
        );
        yield* awaitCondition(() => Effect.sync(() => h.attempts() >= 5));
        yield* Fiber.interrupt(fiber);
        return yield* currentLivenessReasons(globals);
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(reasons).toEqual([]);
    expect(h.store.size).toBe(VISIBLE_SET.length);
  });

  it("keeps the hold at the floor through fast provider failures, which still raise the stall reason", async () => {
    // `extendL1ControlPlaneHold(0)` never shortens a hold: it reports the
    // budget this attempt was granted.
    const holds: (number | undefined)[] = [];
    const failFast = extendL1ControlPlaneHold(0).pipe(
      Effect.tap((held) => Effect.sync(() => holds.push(held))),
      Effect.zipRight(Effect.fail(new Error("ECONNREFUSED"))),
    );
    const reasons = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const fiber = yield* Effect.fork(
          repeatVisibleUserEventIngestionFiber({
            schedule: Schedule.spaced(1),
            startLogMessage: "start",
            spanName: "refused_ingestion",
            action: failFast,
            holdFloorMs: 50,
            holdCeilingMs: 10_000,
          }),
        );
        yield* awaitCondition(() => Effect.sync(() => holds.length >= 6));
        yield* Fiber.interrupt(fiber);
        return yield* currentLivenessReasons(globals);
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(holds.length).toBeGreaterThanOrEqual(6);
    expect(new Set(holds)).toEqual(new Set([50]));
    expect(
      reasons.some((r) =>
        r.startsWith("user_event_ingestion_stalled:refused_ingestion:"),
      ),
    ).toBe(true);
  });
});
