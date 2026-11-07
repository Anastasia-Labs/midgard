import { Effect } from "effect";

import { readCommitAnchorBlock } from "../database/commit-anchor.js";
import {
  followerViewOf,
  readFollowerWriteGate,
} from "../services/follower-write-gate.js";
import { Globals, NodeConfig } from "../services/index.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";

/** The source the commit anchor readiness raises under. It holds no fiber:
 * while it is raised every commit already holds (`commitEventHorizon` is
 * `null`), so the reason only makes that hold visible on /readyz. */
export const COMMIT_ANCHOR_SOURCE = "commit_anchor";

/** The L1 follower has no block d (the profile's commit-event depth) below
 * the view the driver applied: the chain above its origin is not yet d
 * blocks long, or that block is pruned while the applied view trails the
 * follower's tip. No end time is safe, so no block is committed. It clears on
 * the first commitment tick that finds the block. */
export const COMMIT_ANCHOR_UNAVAILABLE = "commit_anchor_unavailable";

/**
 * Raises or clears `commit_anchor_unavailable` from the anchor read the
 * commit worker plans its end time with (`readCommitAnchorBlock` at the
 * write gate's applied view). With no applied view, or one a rewind removed,
 * the gate's own reasons hold and this one clears. A failed read leaves the
 * reason as it is and is logged; the next tick reads again. Never fails.
 */
export const publishCommitAnchorReadiness = Effect.gen(function* () {
  const globals = yield* Globals;
  const depth = (yield* NodeConfig).COMMIT_EVENT_DEPTH;
  const read = yield* Effect.either(
    Effect.gen(function* () {
      const gate = yield* readFollowerWriteGate;
      if (gate.applied === undefined) return null;
      return yield* readCommitAnchorBlock({
        view: followerViewOf(gate.applied),
        depth,
      });
    }),
  );
  if (read._tag === "Left")
    return yield* Effect.logWarning(
      `Could not read the commit anchor at commit-event depth ${depth.toString()}; readiness is unchanged.`,
      read.left,
    );
  const block = read.right;
  if (
    block === null ||
    block.kind === "anchored" ||
    block.reason === "view_gone"
  )
    return yield* clearLivenessIncident(globals, COMMIT_ANCHOR_SOURCE);
  yield* raiseLivenessIncident(
    globals,
    COMMIT_ANCHOR_SOURCE,
    COMMIT_ANCHOR_UNAVAILABLE,
    `every commit holds until the L1 follower has the block ${depth.toString()} below its applied view (${block.reason}: ${block.detail})`,
  );
});
