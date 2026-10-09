/**
 * The follower-change driver over the emulator follower host
 * (`emulator-l1-follower.ts`), as the production follower runs it.
 */
import { Effect } from "effect";

import {
  classifyChange,
  type DriverHold,
  type IngestionPlan,
  type SinkResult,
} from "../../src/l1-events/driver.js";
import {
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCKS_WAITING,
} from "../../src/landed-blocks/holds.js";
import { Globals } from "../../src/services/index.js";
import { driverSink } from "../../src/services/l1-follower.driver-sink.js";
import { makeDriverRecompute } from "../../src/services/l1-follower.recompute.js";
import {
  type EmulatorFollowerFixture,
  syncEmulatorFollower,
} from "./emulator-l1-follower.js";

/**
 * The landed-block hook (`landedBlockHook`) a driver run runs after its sink,
 * at the run's view, under the driver's write capability, as the production
 * driver runs it; its hold is what the node would report.
 */
export type EmulatorLandedBlocks = (
  view: IngestionPlan["view"],
) => Promise<DriverHold | undefined>;

/** Holds a later driver run of the same view clears without help: the
 * rebase the hook ran and that has not finished, or a view that moved. */
const RERUN_HOLDS: ReadonlySet<string> = new Set([
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCKS_WAITING,
]);

/** Driver runs after the first that a landed-block hook may ask for. */
const LANDED_RERUNS = 3;

/** One driver run's outcome: the sink's result, the landed-block hook's
 * hold, and the hold of the rebase the own-commit disposition made due. */
export type EmulatorDriverRun = Readonly<{
  result: SinkResult;
  view: IngestionPlan["view"];
  landedHold: DriverHold | undefined;
  dispositionHold: DriverHold | undefined;
}>;

/**
 * The follower-change driver over the emulator follower, as the production
 * follower runs it (`l1-follower.ts`): each run brings the follower store
 * to the emulator's tip and plans at its current view, the driver's sink (`driverSink`) applies the plan
 * (its recompute on a first view, a rewind or orphans, with the startup
 * preparation once), the landed-block hook runs at the applied view, and
 * then the rebase the own-commit disposition makes due (`rebaseIfDue`), as
 * after S6. S6 itself is not run: its intent statuses are what a test
 * writes or reconciles.
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
    let current: IngestionPlan | undefined;
    let applied: IngestionPlan["view"] | null = null;
    const recompute = yield* makeDriverRecompute({
      planCurrent: () =>
        Promise.resolve(
          current === undefined
            ? ({
                kind: "none",
                detail: "the emulator follower has not synced",
              } as const)
            : ({ kind: "ok", plan: current } as const),
        ),
      ...(options.startupPreparation === undefined
        ? {}
        : { startupPreparation: options.startupPreparation }),
    });
    const sink = yield* driverSink(recompute);
    const runOnce = Effect.gen(function* () {
      const plan = yield* syncEmulatorFollower(fixture, globals);
      current = plan;
      const result = yield* Effect.promise(() =>
        sink.apply(classifyChange(applied, plan.view), plan),
      );
      if (result.kind === "applied") applied = plan.view;
      const landedHold =
        result.kind === "applied" && options.landed !== undefined
          ? yield* Effect.promise(() => options.landed!(plan.view))
          : undefined;
      const dispositionHold = yield* recompute.rebaseIfDue(
        "S6 derived the status of this node's own commits",
      );
      return {
        result,
        view: plan.view,
        landedHold,
        dispositionHold,
      } satisfies EmulatorDriverRun;
    });
    /**
     * Driver runs until one applies: a run held for its recompute (orphans,
     * a cache reload, the startup preparation) runs again once, as the
     * driver's backoff would, and a landed-block hold that a later run
     * clears runs again, up to `LANDED_RERUNS` more. A run that still does
     * not apply fails with its result.
     */
    const untilApplied = Effect.gen(function* () {
      let reruns = 0;
      let heldLast = false;
      for (;;) {
        const run = yield* runOnce;
        const rerun =
          run.landedHold !== undefined &&
          RERUN_HOLDS.has(run.landedHold.reason) &&
          reruns++ < LANDED_RERUNS;
        if (run.result.kind === "applied" && !rerun) return run;
        if (
          run.result.kind !== "applied" &&
          (run.result.kind !== "held" || heldLast)
        )
          return yield* Effect.die(
            new Error(
              `The emulator driver run did not apply: ${JSON.stringify(run.result)}`,
            ),
          );
        heldLast = run.result.kind === "held";
      }
    });
    return {
      recompute,
      /** One driver run. */
      runOnce,
      untilApplied,
      /** The view the driver last applied. */
      applied: () => applied,
    };
  });
