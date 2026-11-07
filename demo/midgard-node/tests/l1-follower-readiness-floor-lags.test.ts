/**
 * A role prune floor that holds the follower's prune boundary back is a
 * readiness detail named by the floor that declared it, so the detail names
 * the real cause: `<floor>_prune_floor:<lag slots>`, one per lagging floor.
 * It leaves the node ready.
 */
import { describe, expect, it } from "vitest";

import { l1FollowerReadiness } from "../src/services/l1-follower.readiness.js";
import {
  followingAtTip,
  runningFollower,
} from "./readiness-l1-follower.fixture.js";

const withFloorLags = (
  floorLags: readonly { floor: string; lagSlots: number }[],
) =>
  runningFollower(
    followingAtTip({
      prune: {
        steps: 3,
        prunedThroughSlot: 40,
        lastError: null,
        failures: 0,
        floorLags,
      },
    }),
  );

describe("the follower's prune-floor lag details", () => {
  it("names each lagging floor by its declared name, with its own lag", () => {
    const readiness = l1FollowerReadiness(
      withFloorLags([
        { floor: "landed_frontier", lagSlots: 12 },
        { floor: "intent_journal_replay", lagSlots: 30 },
      ]),
    );
    expect(readiness.details).toEqual([
      "landed_frontier_prune_floor:12",
      "intent_journal_replay_prune_floor:30",
    ]);
    expect(readiness.reasons).toEqual([]);
  });

  it("reports no detail while no floor lags", () => {
    expect(l1FollowerReadiness(withFloorLags([])).details).toEqual([]);
  });
});
