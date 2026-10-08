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
 * Released here: the pending-table rows each block marked included
 * (`included_by`) and their deltas (`MempoolInclusionsDB.deleteIncluded`).
 * They stay marked from the fold until here, never deleted at the fold or at
 * the own merge finalization, so an unfold restores nothing for them: the
 * rows are still marked by the block it reopens, and a rollback that takes
 * that block off the landed chain clears the marks.
 */
import { Effect } from "effect";

import * as MempoolInclusionsDB from "../database/mempoolInclusions.js";
import type { DatabaseError } from "../database/utils/common.js";
import type { Database } from "../services/database.js";

export const releaseFinalFolds = (
  headerHashes: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.forEach(headerHashes, MempoolInclusionsDB.deleteIncluded, {
    discard: true,
  });
