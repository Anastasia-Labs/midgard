import { type FollowStatus, readinessOf } from "@al-ft/midgard-l1-follower";
import { Ref } from "effect";

import type { DriverHold } from "../src/l1-events/driver.js";
import type { Globals } from "../src/services/globals.js";
import type {
  L1FollowerHandle,
  L1FollowerState,
} from "../src/services/l1-follower.readiness.js";

/** A follow loop status at the node's tip with nothing held. */
export const followingAtTip = (
  change: Partial<Omit<FollowStatus, "readiness">> = {},
): FollowStatus => {
  const status: Omit<FollowStatus, "readiness"> = {
    state: "following",
    interventions: [],
    waiting: null,
    stuck: null,
    protocolInit: "seen",
    cursor: { slot: 100, height: 10, generation: 1 },
    node: null,
    tip: { slot: 100, height: 10 },
    atTip: true,
    replaying: false,
    events: 1,
    lastError: null,
    prune: {
      steps: 0,
      prunedThroughSlot: null,
      lastError: null,
      failures: 0,
    },
    ...change,
  };
  return { ...status, readiness: readinessOf(status) };
};

/** A running follower handle reporting `status` and the driver `holds`. */
export const runningFollower = (
  status: FollowStatus = followingAtTip(),
  holds: readonly DriverHold[] = [],
): L1FollowerHandle => ({
  kind: "running",
  status: () => status,
  holds: () => holds,
  planCurrent: () =>
    Promise.resolve({ kind: "none", detail: "readiness fixture" }),
});

/**
 * What a serving node holds once its L1 follower is at the tip with no
 * driver hold. `/readyz` names every other follower state (N1), so a route
 * fixture that models a healthy node seeds this.
 */
export const seedCaughtUpL1Follower = (
  globals: Globals,
  state: L1FollowerState = runningFollower(),
) => Ref.set(globals.L1_FOLLOWER, state);
