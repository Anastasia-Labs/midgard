/**
 * The follower-change driver over the emulator follower host
 * (`emulator-l1-follower.ts`), run by the production follower's composition
 * (`followerTick`, `l1-follower.tick.ts`).
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { Effect, Option } from "effect";

import {
  createFollowerDriver,
  type DriverHold,
  type DriverHook,
  type FollowerDriver,
  type IngestionPlan,
  type SinkResult,
} from "../../src/l1-events/driver.js";
import {
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCKS_WAITING,
} from "../../src/landed-blocks/holds.js";
import { Globals } from "../../src/services/index.js";
import { IntentJournal } from "../../src/services/intent-journal.js";
import { driverSink } from "../../src/services/l1-follower.driver-sink.js";
import { failureHold } from "../../src/services/l1-follower.failure-hold.js";
import { planCurrentView } from "../../src/services/l1-follower.readiness.js";
import { makeDriverRecompute } from "../../src/services/l1-follower.recompute.js";
import { followerTick } from "../../src/services/l1-follower.tick.js";
import {
  type EmulatorFollowerFixture,
  emulatorOf,
  syncEmulatorFollower,
} from "./emulator-l1-follower.js";
import {
  boundNodeFollower,
  syncFollowerHost,
} from "./follower-emulator.host.js";

/**
 * The landed-block hook (`landedBlockHook`): the driver's
 * `foreignBlockInclusion` hook, run after its sink at the change's view
 * under the driver's write capability, as the production driver runs it;
 * its hold is what the node would report.
 */
export type EmulatorLandedBlocks = DriverHook;

/** Landed-block holds a later run of the same view clears without help:
 * the rebase the hook ran and that has not finished, or a view that moved. */
const RERUN_HOLDS: ReadonlySet<string> = new Set([
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCKS_WAITING,
]);

/** Runs after the first that a landed-block hold may ask for. */
const LANDED_RERUNS = 3;

/**
 * Consecutive runs that do not apply before `untilApplied` gives up. The
 * production runner retries them on its backoff without a bound; a test
 * whose driver never applies fails here instead of hanging.
 */
const UNAPPLIED_RUNS = 8;

/** One run's outcome: the driver's sink result (or why it had none), its
 * view, the landed-block hook's hold, the hold of the rebase the own-commit
 * disposition made due, and the run's holds. */
export type EmulatorDriverRun = Readonly<{
  result:
    | SinkResult
    | Readonly<{ kind: "unreadable"; detail: string }>
    | Readonly<{ kind: "no_view" }>;
  view: IngestionPlan["view"];
  landedHold: DriverHold | undefined;
  dispositionHold: DriverHold | undefined;
  holds: readonly DriverHold[];
}>;

/**
 * The production follower's composition over the emulator follower: each
 * run brings the follower store to the emulator's tip, then runs
 * `followerTick` with the production driver (`createFollowerDriver`) over
 * that store, its sink (`driverSink`: the recompute on a first view, a
 * rewind or orphans, with the startup preparation once), the landed-block
 * hook as its `foreignBlockInclusion` hook, the rebase the own-commit
 * disposition makes due, and the intent journal's re-read when the runtime
 * has a journal. S6 is not run (its intent statuses are what a test writes
 * or reconciles), and the driver's other hooks are not wired.
 */
export const makeEmulatorDriver = <R = never>(
  fixture: EmulatorFollowerFixture,
  options: Readonly<{
    startupPreparation?: Effect.Effect<void, unknown, R>;
    landed?: EmulatorLandedBlocks;
  }> = {},
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const journal = yield* Effect.serviceOption(IntentJournal);
    const emulator = emulatorOf(fixture.operatorLucid);
    // The host replaces its store object on a reset: the driver reads the
    // one the latest sync returned.
    let host: FactStore | undefined;
    const store = new Proxy({} as FactStore, {
      get: (_, key) => {
        if (host === undefined)
          throw new Error("The emulator follower has not synced");
        const value: unknown = Reflect.get(host, key);
        return typeof value === "function" ? value.bind(host) : value;
      },
    });
    const projection = () => {
      const plan = boundNodeFollower(emulator);
      if (plan === undefined)
        throw new Error("The emulator's follower is not bound");
      return plan.projection;
    };
    const recompute = yield* makeDriverRecompute({
      planCurrent: () =>
        host === undefined
          ? Promise.resolve({
              kind: "none",
              detail: "the emulator follower has not synced",
            } as const)
          : planCurrentView(store, projection()),
      ...(options.startupPreparation === undefined
        ? {}
        : { startupPreparation: options.startupPreparation }),
    });
    const sink = yield* driverSink(recompute);
    const landed = options.landed;
    let landedHold: DriverHold | undefined;
    let driver: FollowerDriver | undefined;
    const driverOf = () =>
      (driver ??= createFollowerDriver({
        failureHold,
        store,
        config: projection(),
        sink,
        hooks:
          landed === undefined
            ? {}
            : {
                foreignBlockInclusion: async (change) => {
                  landedHold = await landed(change);
                  return landedHold;
                },
              },
      }));
    const runOnce = Effect.gen(function* () {
      const plan = yield* syncEmulatorFollower(fixture, globals);
      host = yield* Effect.promise(() => syncFollowerHost(emulator));
      landedHold = undefined;
      const tick = yield* followerTick({
        driver: driverOf(),
        nodeBehind: () => false,
        recompute,
        refreshJournal: () =>
          Option.match(journal, {
            onNone: () => Effect.void,
            onSome: (service) => service.refresh(),
          }),
      });
      const ran = tick.driverRun;
      return {
        result: ran.kind === "ran" ? ran.result : ran,
        view: ran.kind === "ran" ? ran.change.view : plan.view,
        // Set by the hook during the run (a closure the compiler does not
        // follow).
        landedHold: landedHold as DriverHold | undefined,
        dispositionHold: tick.dispositionHold,
        holds: tick.holds,
      } satisfies EmulatorDriverRun;
    });
    /**
     * Runs until one applies, as the production runner retries a held run
     * on its backoff: a run that does not apply (held, stale, unreadable)
     * runs again, up to `UNAPPLIED_RUNS` in a row, and an applied run whose
     * landed-block hold a later run clears runs again, up to
     * `LANDED_RERUNS` more. A run that still does not apply fails with its
     * result and holds.
     */
    const untilApplied = Effect.gen(function* () {
      let landedReruns = 0;
      let unapplied = 0;
      for (;;) {
        const run = yield* runOnce;
        if (run.result.kind === "applied") {
          unapplied = 0;
          const rerun =
            run.landedHold !== undefined &&
            RERUN_HOLDS.has(run.landedHold.reason) &&
            landedReruns++ < LANDED_RERUNS;
          if (!rerun) return run;
          continue;
        }
        if (++unapplied >= UNAPPLIED_RUNS)
          return yield* Effect.die(
            new Error(
              `The emulator driver run did not apply: ${JSON.stringify(run.result)}; holds ${JSON.stringify(run.holds)}`,
            ),
          );
      }
    });
    return {
      recompute,
      /** One run of the composition. */
      runOnce,
      untilApplied,
      /** The view the driver last applied. */
      applied: () => driver?.applied() ?? null,
    };
  });
