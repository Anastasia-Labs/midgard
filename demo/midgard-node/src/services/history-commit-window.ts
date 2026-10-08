import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { followerBlockBelowCoveredTip } from "../database/follower-events.block-below-covered-tip.js";
import { followerEligibilityHorizon } from "../database/follower-events.js";
import { DatabaseError } from "../database/utils/common.js";
import type {
  CommitTimingBudget,
  CommitTimingCheckpoint,
} from "../workers/utils/commit-end-time.js";
import { NodeConfig } from "./config.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.js";
import { Lucid } from "./lucid.js";

/** Both admitted validator environments enforce inclusion = inclusive validTo
 * + this delay. An admission after the whole canonical point cannot be eligible
 * at or before this conservative horizon. This does not move the source point,
 * and known admissions in the lookahead interval still have to be selected. */
export const historyEligibilityHorizon = (
  coverage: HistoryOwnerCoverage,
): number => {
  const end = coverage.includedThroughMs + EVENT_WAIT_DURATION_MS - 1;
  if (
    !Number.isSafeInteger(coverage.includedThroughMs) ||
    !Number.isSafeInteger(end)
  )
    throw new Error(
      "Authenticated history time cannot form a safe commit horizon",
    );
  return end;
};

/**
 * The horizon lag d (`HISTORY_COMMIT_HORIZON_LAG_BLOCKS`) and the L1 slot
 * clock that dates the lagged block. The clock runs only when d > 0, so the
 * unlagged horizon acquires and reads nothing more than before.
 */
export type CommitHorizonLag<E = never, R = never> = Readonly<{
  lagBlocks: number;
  slotToUnixTime: Effect.Effect<(slot: number) => number, E, R>;
}>;

/** The lag the node is configured with, dated by its Lucid slot clock. */
export const configuredCommitHorizonLag = Effect.map(
  NodeConfig,
  (config): CommitHorizonLag<never, Lucid> => ({
    lagBlocks: config.HISTORY_COMMIT_HORIZON_LAG_BLOCKS,
    slotToUnixTime: Effect.map(Lucid, (lucid) => lucid.api.slotToUnixTime),
  }),
);

/**
 * The cap the horizon lag d sets on a block's end time (U3): an event
 * admitted at or after the follower block d below its covered tip has
 * inclusion = inclusive validTo + W > that block's time + W - 1, so no due
 * event comes from the last d + 1 blocks. `undefined` at d = 0 (no cap, and
 * nothing is read). `null` while that block is unavailable (no follower
 * cursor yet, or the chain above the origin is not yet d blocks long): the
 * caller holds, as before a first ingestion. The point is the follower's
 * (`followerBlockBelowCoveredTip`), never the journal's.
 */
export const laggedEligibilityCap = <E, R>(lag: CommitHorizonLag<E, R>) =>
  lag.lagBlocks === 0
    ? Effect.succeed(undefined)
    : Effect.gen(function* () {
        const below = yield* followerBlockBelowCoveredTip(lag.lagBlocks);
        if (below.kind === "unavailable") {
          yield* Effect.logInfo(
            `🔹 Commit horizon lag ${lag.lagBlocks.toString()} has no follower block yet (${below.reason}: ${below.detail}).`,
          );
          return null;
        }
        const slotToUnixTime = yield* lag.slotToUnixTime;
        const time = slotToUnixTime(below.point.slot);
        const cap = time + EVENT_WAIT_DURATION_MS - 1;
        if (!Number.isSafeInteger(time) || !Number.isSafeInteger(cap))
          return yield* Effect.fail(
            new DatabaseError({
              table: "l1_blocks",
              message: "Lagged follower block time cannot form a safe cap",
              cause: `slot=${below.point.slot.toString()},time=${String(time)}`,
            }),
          );
        return cap;
      });

/**
 * The commit end-time horizon (E-N1-2 item 3): min(journal coverage, the
 * follower's ingestion horizon), both capped by the horizon lag d
 * (`laggedEligibilityCap`). A block never claims an end time past the
 * events the follower-change driver has ingested; before its first
 * ingestion (or after a rewind removed it), or while the lagged block is
 * unavailable, nothing is eligible and the commit holds (`null`).
 * `coverage` is the owner's journal coverage when the commit runs under a
 * history producer; N1-close drops that term.
 */
export const commitEventHorizon = <E, R>(
  coverage: HistoryOwnerCoverage | undefined,
  lag: CommitHorizonLag<E, R>,
) =>
  Effect.gen(function* () {
    const follower = yield* followerEligibilityHorizon;
    if (follower === null) return null;
    const unlagged =
      coverage === undefined
        ? follower
        : Math.min(follower, historyEligibilityHorizon(coverage));
    const cap = yield* laggedEligibilityCap(lag);
    return cap === undefined
      ? unlagged
      : cap === null
        ? null
        : Math.min(unlagged, cap);
  });

/** The fixed header interval cannot be extended to rescue a slow build. These
 * stage reserves apply only to a source-owned short-window attempt; expiration
 * causes a new selection/build against fresh coverage. The ordinary long-window
 * fixture policy and on-chain range/size/execution limits remain independent. */
export const HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS = 30_000;
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
