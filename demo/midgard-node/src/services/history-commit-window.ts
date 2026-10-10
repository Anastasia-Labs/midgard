import { COMMIT_TTL_FUTURE_BUFFER_MS } from "@al-ft/midgard-core/deployment-profile";
import { Effect } from "effect";

import { readCommitAnchor } from "../database/commit-anchor.js";
import { followerIngestedView } from "../database/follower-events.js";
import { forcedOrderHorizon } from "../forced-orders/horizon.js";
import type {
  CommitTimingBudget,
  CommitTimingCheckpoint,
} from "../workers/utils/commit-end-time.js";
import { NodeConfig } from "./config.js";
import { followerViewOf, type GateView } from "./follower-write-gate.js";
import { Lucid } from "./lucid.js";

/** The node's commit-event depth d and the L1 slot clock that dates its anchors. */
export const configuredCommitAnchorClock = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;
  return {
    depth: config.COMMIT_EVENT_DEPTH,
    slotToUnixTime: lucid.api.slotToUnixTime,
  };
});

/**
 * The commit end-time horizon (plan §8.1, N10): min(the commit anchor's cap,
 * the forced-order bound), with the anchor the journal stores. The anchor
 * is the follower block d below the planning view (`readCommitAnchor`): the
 * write permit's view, or, for a model fixture without one, the view the
 * driver last ingested through. A block never claims an end time whose due
 * events reach the last d + 1 blocks of that view, nor reaches a forced
 * order the node has not rebuilt yet. With no view (before the first
 * ingestion, or after a rewind removed it) or no anchor block, nothing is
 * eligible and the commit holds (`null`).
 */
export const commitEventHorizon = <E, R>(input: {
  readonly view: GateView | undefined;
  readonly depth: number;
  readonly slotToUnixTime: Effect.Effect<(slot: number) => number, E, R>;
}) =>
  Effect.gen(function* () {
    const view =
      input.view === undefined
        ? yield* followerIngestedView
        : followerViewOf(input.view);
    if (view === null) return null;
    const slotToUnixTime = yield* input.slotToUnixTime;
    const read = yield* readCommitAnchor({
      view,
      depth: input.depth,
      slotToUnixTime,
    });
    if (read.kind === "unavailable") {
      yield* Effect.logInfo(
        `🔹 Commit anchor unavailable (${read.reason}: ${read.detail}).`,
      );
      return null;
    }
    const forced = yield* forcedOrderHorizon;
    return {
      horizonMs: Math.min(read.capMs, ...(forced === null ? [] : [forced])),
      anchor: read.anchor,
    };
  });

/** The fixed header interval cannot be extended to rescue a slow build. These
 * stage reserves apply only to a source-owned short-window attempt; expiration
 * causes a new selection/build against fresh coverage. The ordinary long-window
 * fixture policy and on-chain range/size/execution limits remain independent.
 * The value is the deployment profiles' commit TTL floor, which the profile
 * build also bounds the commit-event depth with: the anchor cap must reach
 * it, or no commit can be planned. */
export const HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS =
  COMMIT_TTL_FUTURE_BUFFER_MS;
/** A source-owned commit capped to the current scheduler shift ends at that
 * shift's end, and the header end is its inclusive TTL. Several L1 block
 * intervals (~20 s each) must remain for it to land, so neither the pre-lease
 * scheduler target nor the worker's current-window cap admits a shift with
 * less left than this. A shorter remainder waits for the next shift. */
export const HISTORY_COMMIT_LANDING_MARGIN_MS = 120_000;
const reserves: Readonly<Record<CommitTimingCheckpoint, number>> = {
  pre_witness: 30_000,
  pre_build: 20_000,
  pre_submit: 10_000,
};
export const historyCommitTimingBudget = (input: {
  checkpoint: CommitTimingCheckpoint;
  resolvedEndTimeMs: number;
  nowMs: number;
}): CommitTimingBudget => {
  const minimumBudgetMs = reserves[input.checkpoint];
  const remainingBudgetMs = input.resolvedEndTimeMs - input.nowMs;
  return {
    ...input,
    remainingBudgetMs,
    minimumBudgetMs,
    satisfied:
      Number.isSafeInteger(input.nowMs) &&
      Number.isSafeInteger(input.resolvedEndTimeMs) &&
      remainingBudgetMs >= minimumBudgetMs,
  };
};
