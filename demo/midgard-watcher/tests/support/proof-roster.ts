import type { FollowStatus } from "@al-ft/midgard-l1-follower";

import type { Removed } from "./proof-retention-removed-header.js";

/** A follow status with its cursor at `height` (null: none yet). */
export const followStatus = (
  height: number | null,
  atTip: boolean,
): FollowStatus => ({
  state: "following",
  readiness: [],
  interventions: [],
  waiting: null,
  stuck: null,
  protocolInit: "seen",
  cursor: height === null ? null : { slot: 0, height, generation: 0 },
  node: null,
  tip: null,
  nodeBehind: null,
  atTip,
  replaying: false,
  events: 0,
  lastError: null,
  prune: {
    steps: 0,
    prunedThroughSlot: null,
    lastError: null,
    failures: 0,
    floorLags: [],
  },
});

/** Reports the follower at the fixture's tip, as the follow loop would. */
export const reportAtTip = (r: Removed): void =>
  r.roster.observe(followStatus(r.h.chain.tip.height, true));

/** Reports the follower at tip and runs a pass at tip over the tip's block. */
export const passAtTip = async (r: Removed): Promise<boolean> => {
  reportAtTip(r);
  return await r.retention.passAtTip(r.h.tipPoint());
};

/** Moves the tip `blocks` forward with empty blocks. */
export const advance = async (r: Removed, blocks: number): Promise<void> => {
  for (let i = 0; i < blocks; i += 1) await r.h.forward([]);
};

/** The prune floors one prune step reports holding the boundary back. */
export const floorLagsOf = async (r: Removed) => {
  const step = await r.h.store.prune(1_000);
  if ("kind" in step) throw new Error(`prune: ${step.kind}`);
  return step.floorLags;
};
