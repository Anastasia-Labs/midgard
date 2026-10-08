/**
 * What a fold leaves until its merge is final (plan §9 `final`, §11, N5).
 *
 * A fold into `confirmed_ledger` happens once the merged queue root covers
 * the block, before its merge is final, so it is reversible: the unfold
 * restores the ledger rows, the landed row and the event marks the fold
 * stored (`confirmed-merges.ts`). A delete that nothing could restore must
 * not run at the fold. It runs here instead, when the follower's prune
 * boundary reaches the fold's merge slot: no rollback reaches that slot (the
 * follower refuses one below its prune boundary as `rollback_beyond_k`),
 * the fold can never unfold, and its stored delta is deleted in the same
 * transaction (`pruneMerges`).
 *
 * The headers are every block whose fold just became final, own and foreign,
 * whichever path folded it (landed-block processing or the merge fiber; the
 * merge fiber's folds are released once processing has recorded their merge
 * point). A block whose fold a rollback unfolds never reaches here.
 *
 * Nothing is released yet. The pending-table rows a block marked included
 * (`included_by`) are deleted here, never at the fold or at the own merge
 * finalization.
 */
import { Effect } from "effect";

import type { Database } from "../services/database.js";

export const releaseFinalFolds = (
  _headerHashes: readonly Buffer[],
): Effect.Effect<void, never, Database> => Effect.void;
