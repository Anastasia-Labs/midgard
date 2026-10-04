import { Effect } from "effect";

import * as ForeignCensus from "../database/eventHistoryForeignCensus.js";
import * as Journal from "../database/eventHistoryJournal.js";
import { foreignNativeAdoptionHoldSlot } from "../database/foreignNativeAdoptions.js";
import { settlementRetentionHoldSlot } from "../database/settlement.js";
import type { EventHistorySourceBinding } from "../l1-event-history-source.js";
import type { HistoryRetentionHold } from "./event-history-owner.history-owner-change.js";
import { HistoryOwnerUnavailable } from "./event-history-owner.history-owner-change.js";
import { signedHeaderRecoveryHoldSlot } from "./history-signed-header-recovery.js";

export type Retained = Readonly<{
  result: Journal.Checkpoint;
  hold: Journal.RetentionHold | undefined;
}>;

/** Called only inside the source owner's existing append transaction. */
export const makeRetainedHistoryAppender = <E, R>(input: {
  binding: EventHistorySourceBinding;
  rollbackHorizon: number;
  requireCheckpoint: Effect.Effect<Journal.Checkpoint, E, R>;
}) => {
  return <E2, R2>(
    prepared: ReturnType<typeof Journal.prepareAppend>,
    tipHeight: number,
    reconcile: (
      appended: Journal.Appended,
    ) => Effect.Effect<Journal.Checkpoint, E2, R2>,
  ) =>
    Effect.all([
      signedHeaderRecoveryHoldSlot(input.binding.digest),
      settlementRetentionHoldSlot(input.binding.manifestId),
      foreignNativeAdoptionHoldSlot(
        input.binding.digest,
        input.rollbackHorizon,
      ),
    ]).pipe(
      Effect.map((slots) => {
        const held = slots.filter((slot): slot is number => slot !== undefined);
        return held.length === 0 ? undefined : Math.min(...held);
      }),
      Effect.flatMap((holdSlot) =>
        ForeignCensus.append({
          binding: input.binding,
          block: prepared.block,
          receipt: prepared.receipt,
        }).pipe(
          Effect.zipRight(
            Journal.append(input.binding, prepared, reconcile, {
              tipHeight,
              horizon: input.rollbackHorizon,
              holdSlot,
            }),
          ),
        ),
      ),
      Effect.flatMap((appended) =>
        appended.applied
          ? Effect.succeed<Retained>({
              result: appended.result,
              hold: appended.hold,
            })
          : input.requireCheckpoint.pipe(
              Effect.map(
                (result): Retained => ({
                  result,
                  hold: undefined,
                }),
              ),
            ),
      ),
    );
};

// Warn once when pending recovery holds the anchor more than k blocks back,
// and once when it releases that hold.
export const noteHistoryRetention = (input: {
  hold: Journal.RetentionHold | undefined;
  previous: HistoryRetentionHold | undefined;
  rollbackHorizon: number;
  setHold: (hold: HistoryRetentionHold | undefined) => void;
}) =>
  Effect.gen(function* () {
    const hold = input.hold;
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
    const previous = input.previous;
    input.setHold(next);
    if ((previous === undefined) === (next === undefined)) return;
    const annotations = next ?? previous!;
    yield* (
      next === undefined
        ? Effect.logInfo(
            "History retention hold released; the journal anchor advances again",
          )
        : Effect.logWarning(
            "History retention held by pending signed-header recovery: the journal anchor is more than the rollback horizon behind",
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
    );
  });

// The chain was verified in full by startup; later steps extend or reverse it
// under the cursor lock, so the working checkpoint verifies cursor and head.
export const requireRetainedHistoryCheckpoint = (
  binding: EventHistorySourceBinding,
) =>
  Effect.gen(function* () {
    const value = yield* Journal.loadCurrent(binding);
    if (value === null)
      return yield* Effect.fail(
        new HistoryOwnerUnavailable({ cause: "History checkpoint is missing" }),
      );
    return value;
  });
