import { Effect } from "effect";

import type * as Journal from "../database/eventHistoryJournal.js";
import {
  HISTORY_READY_MAXIMUM_LAG_BLOCKS,
  type HistoryRetentionHold,
} from "./event-history-owner.history-owner-change.js";

/** The owner's visible, not silent, state transitions: each is logged once
 * when it starts and once when it ends, never on every block. */
export const makeHistoryOwnerNotices = (input: {
  readonly run: (effect: Effect.Effect<void>) => Promise<void>;
  readonly rollbackHorizon: number;
}) => {
  let lagging = false;
  let retentionHold: HistoryRetentionHold | undefined;
  return {
    retentionHold: () => retentionHold,
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
    // Warn once when retained evidence starts holding the anchor more than k
    // blocks back, and once when it lets go.
    retention: async (hold: Journal.RetentionHold | undefined) => {
      const heldBlocks =
        hold === undefined ? 0 : hold.unheldAnchorHeight - hold.anchorHeight;
      const next =
        hold !== undefined && heldBlocks > input.rollbackHorizon
          ? Object.freeze({
              ...hold,
              heldBlocks,
              rollbackHorizon: input.rollbackHorizon,
            })
          : undefined;
      const previous = retentionHold;
      retentionHold = next;
      if ((previous === undefined) === (next === undefined)) return;
      const annotations = next ?? previous!;
      await input.run(
        (next === undefined
          ? Effect.logInfo(
              "History retention hold released; the journal anchor advances again",
            )
          : Effect.logWarning(
              "History retention held by settlement or foreign-adoption evidence: the journal anchor is more than the rollback horizon behind",
            )
        ).pipe(
          Effect.annotateLogs({
            event: "history_retention_hold",
            state: next === undefined ? "released" : "holding",
            holdSlot: annotations.holdSlot,
            anchorHeight: annotations.anchorHeight,
            unheldAnchorHeight: annotations.unheldAnchorHeight,
            heldBlocks: annotations.heldBlocks,
            rollbackHorizon: input.rollbackHorizon,
          }),
        ),
      );
    },
  };
};
