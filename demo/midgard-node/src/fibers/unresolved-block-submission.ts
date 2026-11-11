import { Effect, Ref } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { ACTIVE_STATUSES } from "../database/pendingBlockFinalizations.columns.js";
import type { Globals } from "../services/globals.globals.js";

type UnresolvedSubmissionGlobals = Pick<
  Globals,
  "UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH" | "UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS"
>;

/**
 * The commit tx a journal row may have put on L1 without the confirmation
 * having seen it: the submitted hash, else the journaled signed intent's. An
 * observed block's submission is resolved, so it has none.
 */
const unresolvedBlockSubmissionTxHash = (
  row: PendingBlockFinalizationsDB.Row,
): Buffer | null =>
  row[PendingBlockFinalizationsDB.Columns.STATUS] ===
  PendingBlockFinalizationsDB.Status.ObservedWaitingStability
    ? null
    : (row[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH] ??
      row[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] ??
      null);

/**
 * Points the confirmation at `row`'s unresolved commit tx, unresolved since the
 * row last changed, or at none when the row has no unresolved tx.
 */
export const setUnresolvedBlockSubmission = (
  globals: UnresolvedSubmissionGlobals,
  row: PendingBlockFinalizationsDB.Row,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const txHash = unresolvedBlockSubmissionTxHash(row);
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      txHash?.toString("hex") ?? "",
    );
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      txHash === null
        ? 0
        : row[PendingBlockFinalizationsDB.Columns.UPDATED_AT].getTime(),
    );
  });

/**
 * After a failed commit attempt, points the confirmation at the commit tx its
 * lease's journal `rows` retain, as startup hydration does for the active
 * journal, since a retained signed intent may be on L1. Without one it leaves
 * the confirmation's submission as it is.
 */
export const watchRetainedBlockSubmission = (
  globals: UnresolvedSubmissionGlobals,
  rows: readonly PendingBlockFinalizationsDB.Row[],
): Effect.Effect<void> => {
  const retained = rows.find(
    (row) =>
      ACTIVE_STATUSES.includes(
        row[PendingBlockFinalizationsDB.Columns.STATUS],
      ) && unresolvedBlockSubmissionTxHash(row) !== null,
  );
  return retained === undefined
    ? Effect.void
    : setUnresolvedBlockSubmission(globals, retained);
};
