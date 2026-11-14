import { Effect, Ref } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import type { Globals } from "../services/globals.globals.js";

type UnresolvedSubmissionGlobals = Pick<
  Globals,
  "UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH" | "UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS"
>;

/**
 * Points the confirmation at the commit tx the unlanded journal `row` may
 * have put on L1 (its journaled signed intent's), unresolved since the row
 * last changed, or at none when the row was never signed. Landing, or the
 * journal's death, resolves it (`followJournals`).
 */
export const setUnresolvedBlockSubmission = (
  globals: UnresolvedSubmissionGlobals,
  row: PendingBlockFinalizationsDB.Row,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const txHash = row[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH];
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      txHash?.toString("hex") ?? "",
    );
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      txHash == null
        ? 0
        : row[PendingBlockFinalizationsDB.Columns.UPDATED_AT].getTime(),
    );
  });

/**
 * After a failed commit attempt, points the confirmation at the commit tx its
 * lease's journals retain, as startup hydration does for the live journal: a
 * signed journal that has neither landed nor died may be on L1. Without one
 * it leaves the confirmation's submission as it is.
 */
export const watchRetainedBlockSubmission = (
  globals: UnresolvedSubmissionGlobals,
  journals: readonly PendingBlockFinalizationsDB.LeaseJournal[],
): Effect.Effect<void> => {
  const retained = journals.find(
    ({ row, status, dead }) =>
      status === PendingBlockFinalizationsDB.OwnBlockStatus.Unlanded &&
      !dead &&
      row[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] != null,
  );
  return retained === undefined
    ? Effect.void
    : setUnresolvedBlockSubmission(globals, retained.row);
};
