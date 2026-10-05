import { type Effect } from "effect";

import { type DatabaseError } from "../../database/utils/common.js";
import { type Database } from "../../services/index.js";

export type CommitSubmissionHooks = {
  readonly beforePendingJournalInsert?: (
    blockEndTimeMs: number,
  ) => Effect.Effect<void, DatabaseError, Database>;
  /** Runs once the pending journal transaction has committed. */
  readonly afterPendingJournalPrepared?: Effect.Effect<void>;
  /** Reports exact DA admission before journal preparation or submission. */
  readonly afterDaFrameAccepted?: Effect.Effect<void>;
};
