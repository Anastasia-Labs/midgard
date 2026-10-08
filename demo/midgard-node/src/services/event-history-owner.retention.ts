import { Effect } from "effect";

import * as Journal from "../database/eventHistoryJournal.js";
import type { EventHistorySourceBinding } from "../l1-event-history-source.js";
import { HistoryOwnerUnavailable } from "./event-history-owner.history-owner-change.js";

export type Retained = Readonly<{
  result: Journal.Checkpoint;
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
    Journal.append(input.binding, prepared, reconcile, {
      tipHeight,
      horizon: input.rollbackHorizon,
    }).pipe(
      Effect.flatMap((appended) =>
        appended.applied
          ? Effect.succeed<Retained>({ result: appended.result })
          : input.requireCheckpoint.pipe(
              Effect.map((result): Retained => ({ result })),
            ),
      ),
    );
};

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
