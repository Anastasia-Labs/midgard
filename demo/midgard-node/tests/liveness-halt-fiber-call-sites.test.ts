/**
 * Each held fiber in the node's roster is held under its own name's entry of
 * `FIBER_HALT_SOURCES`. The table itself is pinned in `liveness-halt.test.ts`;
 * this pins the call sites: a roster entry held under another fiber's name
 * (the commit fiber under "merge", say) would stop for the wrong halts.
 */
import { Effect, Fiber, Ref, type Schedule } from "effect";
import { describe, expect, it, vi } from "vitest";

import { nodeFibers } from "../src/commands/listen.node-fibers.js";
import { Globals } from "../src/services/globals.js";
import type { NodeConfigDep } from "../src/services/index.js";
import {
  FIBER_HALT_SOURCES,
  type HeldFiber,
} from "../src/services/liveness-halt.js";

// The sources each held roster entry, once run, asked to be held under.
const held = vi.hoisted(() => ({
  sources: [] as (readonly string[])[],
  factories: 0,
  ticks: 0,
  cleanups: 0,
  repeat: false,
}));

// Every name gets a source list of its own, so a call site that names another
// fiber shows up even where two fibers share their real sources.
vi.mock("../src/services/liveness-halt.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/services/liveness-halt.js")>();
  const { Effect: E } = await import("effect");
  return {
    ...actual,
    FIBER_HALT_SOURCES: Object.fromEntries(
      Object.keys(actual.FIBER_HALT_SOURCES).map((name) => [
        name,
        [`held_as:${name}`],
      ]),
    ),
    pausedWhileHalted: (
      schedule: Schedule.Schedule<number>,
      _globals: Globals,
      sources: readonly string[],
    ) => {
      held.sources.push(sources);
      return actual.pausedWhileHalted(schedule, _globals, sources);
    },
    restartedAcrossHalts: (
      _globals: unknown,
      sources: readonly string[],
      _fiber: unknown,
    ) => {
      held.sources.push(sources);
      return E.void;
    },
  };
});

// The scheduled held fibers run their schedule-built effect; stand them in.
vi.mock("../src/fibers/index.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  const scheduled = (schedule: Schedule.Schedule<number>) => {
    held.factories += 1;
    if (!held.repeat) return E.void;
    return E.repeat(
      E.sync(() => {
        held.ticks += 1;
      }),
      schedule,
    ).pipe(
      E.ensuring(
        E.sync(() => {
          held.cleanups += 1;
        }),
      ),
    );
  };
  return {
    ...(await importOriginal<typeof import("../src/fibers/index.js")>()),
    blockCommitmentFiber: (schedule: Schedule.Schedule<number>) =>
      scheduled(schedule),
    mergeFiber: (schedule: Schedule.Schedule<number>) => scheduled(schedule),
    operatorWatchdogFiber: (schedule: Schedule.Schedule<number>) =>
      scheduled(schedule),
  };
});

const nodeConfig = {
  ADMISSION_BACKLOG_REFRESH_MS: 1_000,
  MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS: 1_000,
  WAIT_BETWEEN_BLOCK_COMMITMENT: 1_000,
  WAIT_BETWEEN_BLOCK_CONFIRMATION: 1_000,
  WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: 1_000,
  WAIT_BETWEEN_RETENTION_SWEEPS: 1_000,
  WAIT_BETWEEN_MERGE_TXS: 1_000,
  TX_QUEUE_POLL_INTERVAL_MS: 1_000,
} as unknown as NodeConfigDep;

// The roster's scheduled bodies are mocked above; their other service
// requirements are unreachable in these tests. Keep the existing test boundary
// assertion shared instead of adding one at each mocked roster call.
const runWithGlobals = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    effect.pipe(Effect.provide(Globals.Default)) as unknown as Effect.Effect<
      A,
      E,
      never
    >,
  );

const waitForTicks = (count: number) =>
  Effect.repeat(Effect.sleep("10 millis"), {
    until: () => held.ticks >= count,
  }).pipe(Effect.timeout("1500 millis"));

describe("held fiber call sites", () => {
  it("holds every held roster fiber under its own name", async () => {
    const roster = nodeFibers({ nodeConfig, withMonitoring: false });
    const heldAs: Record<string, (readonly string[])[]> = {};
    for (const name of Object.keys(FIBER_HALT_SOURCES) as HeldFiber[]) {
      held.sources.length = 0;
      // A held entry returns at once here; an entry no longer held runs its
      // real fiber, which the timeout stops, and records nothing.
      await runWithGlobals(
        roster[name].pipe(Effect.timeout("2 seconds"), Effect.exit),
      );
      heldAs[name] = [...held.sources];
    }
    expect(heldAs).toEqual(
      Object.fromEntries(
        Object.keys(FIBER_HALT_SOURCES).map((name) => [
          name,
          [[`held_as:${name}`]],
        ]),
      ),
    );
  });
});

describe.each(["blockCommitment", "merge"] as const)(
  "initial %s halt",
  (name) => {
    const startRoster = () =>
      nodeFibers({
        nodeConfig: {
          ...nodeConfig,
          WAIT_BETWEEN_BLOCK_COMMITMENT: 50,
          WAIT_BETWEEN_MERGE_TXS: 50,
        },
        withMonitoring: false,
      })[name];
    const reset = () => {
      held.sources.length = 0;
      held.factories = held.ticks = held.cleanups = 0;
      held.repeat = true;
    };

    it("holds construction and the first action, resumes after clearance, and preserves between-tick halts", async () => {
      reset();
      try {
        await runWithGlobals(
          Effect.gen(function* () {
            const globals = yield* Globals;
            const source = `held_as:${name}`;
            yield* Ref.set(
              globals.LIVENESS_REASONS,
              new Map([[source, "already_halted"]]),
            );
            const worker = yield* Effect.fork(startRoster());
            yield* Effect.sleep("35 millis");
            const initial = { factories: held.factories, ticks: held.ticks };
            yield* Ref.set(
              globals.LIVENESS_REASONS,
              new Map([["unrelated", "still_raised"]]),
            );
            yield* waitForTicks(1);
            yield* Ref.set(
              globals.LIVENESS_REASONS,
              new Map([[source, "raised_again"]]),
            );
            const before = held.ticks;
            yield* Effect.sleep("100 millis");
            const after = held.ticks;
            yield* Ref.set(globals.LIVENESS_REASONS, new Map());
            yield* waitForTicks(before + 1);
            yield* Fiber.interrupt(worker);
            expect(initial).toEqual({ factories: 0, ticks: 0 });
            expect(after).toBe(before);
            expect(held.factories).toBe(1);
            expect(held.cleanups).toBe(1);
            expect(held.sources).toEqual([[source]]);
          }),
        );
      } finally {
        held.repeat = false;
      }
    });

    it("cancels an initial wait without constructing or later starting work", async () => {
      reset();
      try {
        await runWithGlobals(
          Effect.gen(function* () {
            const globals = yield* Globals;
            yield* Ref.set(
              globals.LIVENESS_REASONS,
              new Map([[`held_as:${name}`, "already_halted"]]),
            );
            const worker = yield* Effect.fork(startRoster());
            yield* Effect.sleep("35 millis");
            yield* Fiber.interrupt(worker);
            yield* Ref.set(globals.LIVENESS_REASONS, new Map());
            yield* Effect.sleep("50 millis");
            expect(held.factories).toBe(0);
            expect(held.ticks).toBe(0);
            expect(held.cleanups).toBe(0);
          }),
        );
      } finally {
        held.repeat = false;
      }
    });
  },
);
