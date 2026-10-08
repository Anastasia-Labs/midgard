import { Effect } from "effect";

import { HISTORY_READY_MAXIMUM_LAG_BLOCKS } from "./event-history-owner.history-owner-change.js";

/** The owner's visible, not silent, state transitions: each is logged once
 * when it starts and once when it ends, never on every block. */
export const makeHistoryOwnerNotices = (input: {
  readonly run: (effect: Effect.Effect<void>) => Promise<void>;
}) => {
  let lagging = false;
  return {
    // One warning when an open gate falls too far behind the tip, one notice
    // when it catches up. The owner reports lag only from its first
    // readiness on, so a notice always follows its warning.
    lag: (lag: number) => {
      const next = lag > HISTORY_READY_MAXIMUM_LAG_BLOCKS;
      if (next === lagging) return;
      lagging = next;
      void input
        .run(
          (next
            ? Effect.logWarning(
                "History follower is lagging the source tip; new producers are refused until it catches up",
              )
            : Effect.logInfo("History follower caught up with the source tip")
          ).pipe(
            Effect.annotateLogs({
              event: "history_follower_lag",
              state: next ? "lagging" : "caught_up",
              lagBlocks: lag,
              maximumLagBlocks: HISTORY_READY_MAXIMUM_LAG_BLOCKS,
            }),
          ),
        )
        .catch(() => undefined);
    },
  };
};
