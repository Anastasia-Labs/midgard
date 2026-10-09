/**
 * One run of the node follower's composition (`l1-follower.ts`), the work
 * every coalesced trigger does after a cursor advance or a rewind:
 *
 * 1. the follower-change driver applies the follower's current view (its
 *    sink, then its hooks in `DRIVER_HOOK_ORDER`);
 * 2. S6 reconciles the node's signed intents, unless the node is behind
 *    wall-clock time (`l1_node_behind`): it sends nothing then, and the next
 *    head change after it catches up runs it;
 * 3. the rebase the own commits S6 derived dead make due runs through the
 *    driver's recompute (`rebaseIfDue`); a rebase that does not finish is
 *    logged and returned, and the next run retries it;
 * 4. the intent journal re-reads its refusal holds (the commit and
 *    settlement workers raise theirs in the node database).
 *
 * The holds it returns are the driver's and S6's: a run that ends with any
 * runs again on the coalesced runner's backoff
 * (`l1-follower.coalesced-runner.ts`). The node's follower and the emulator
 * driver the node's tests run (`tests/helpers/emulator-l1-follower.driver.ts`)
 * both call this one function.
 */
import { Effect } from "effect";

import type {
  DriverHold,
  DriverRun,
  FollowerDriver,
} from "../l1-events/driver.js";
import type { DriverRecompute } from "./l1-follower.recompute.js";

/** The reason the own-commit disposition's rebase runs under. */
export const OWN_COMMIT_DISPOSITION_REASON =
  "S6 derived the status of this node's own commits";

export type FollowerTickParts = Readonly<{
  driver: FollowerDriver;
  /** S6 (`createNodeIntentStage`, whose pass never rejects); absent where
   * no S6 runs. */
  intents?: Readonly<{
    run: () => Promise<unknown>;
    holds: () => readonly DriverHold[];
  }>;
  /** Whether the node is behind wall-clock time (S6 waits then). */
  nodeBehind: () => boolean;
  recompute: Pick<DriverRecompute, "rebaseIfDue">;
  /** The intent journal's re-read of its refusal holds. */
  refreshJournal: () => Effect.Effect<void>;
}>;

export type FollowerTick = Readonly<{
  /** The driver's run. */
  driverRun: DriverRun;
  /** Why the own-commit disposition's rebase did not finish, if it did not. */
  dispositionHold: DriverHold | undefined;
  /** The driver's and S6's holds after the run. */
  holds: readonly DriverHold[];
}>;

/** One run of the follower's composition; never fails. */
export const followerTick = (
  parts: FollowerTickParts,
): Effect.Effect<FollowerTick> =>
  Effect.gen(function* () {
    const driverRun = yield* Effect.promise(() => parts.driver.run());
    const intents = parts.intents;
    if (intents !== undefined && !parts.nodeBehind())
      yield* Effect.promise(() => intents.run());
    const dispositionHold = yield* parts.recompute.rebaseIfDue(
      OWN_COMMIT_DISPOSITION_REASON,
    );
    if (dispositionHold !== undefined)
      yield* Effect.logWarning(
        `L1 follower: own-commit disposition held (${dispositionHold.reason}): ${dispositionHold.detail}`,
      );
    yield* parts.refreshJournal();
    return {
      driverRun,
      dispositionHold,
      holds: [...parts.driver.holds(), ...(intents?.holds() ?? [])],
    };
  });
