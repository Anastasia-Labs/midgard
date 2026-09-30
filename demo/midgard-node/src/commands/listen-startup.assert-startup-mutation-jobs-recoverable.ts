import { Effect, Option } from "effect";

import {
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { DatabaseInitializationError } from "../services/index.js";
import {
  classifyUnfinishedMutationJobOnStartup,
  failedLocalFinalizationHeader,
} from "./listen-startup.seed-latest-local-block-boundary-on-startup.js";

/**
 * Startup gate over unfinished local mutation jobs. It runs after pending
 * history reconciliation (a correction rewind that abandons a removed block's
 * journal also removes its job) and refuses to serve while any job needs
 * operator recovery; see classifyUnfinishedMutationJobOnStartup.
 */
export const assertStartupMutationJobsRecoverable = Effect.gen(function* () {
  const unfinished = yield* MutationJobsDB.retrieveUnfinished;
  const refused: MutationJobsDB.Entry[] = [];
  for (const job of unfinished) {
    const header = failedLocalFinalizationHeader(job);
    const journal =
      header === undefined
        ? Option.none()
        : yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(header);
    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : undefined;
    if (
      classifyUnfinishedMutationJobOnStartup(job, journalStatus) === "refuse"
    ) {
      refused.push(job);
      continue;
    }
    yield* Effect.logWarning(
      job[MutationJobsDB.Columns.KIND] ===
        MutationJobsDB.Kind.ConfirmedMergeFinalization
        ? `Startup left ${job[MutationJobsDB.Columns.STATUS]} confirmed-merge finalization job ${job[MutationJobsDB.Columns.JOB_ID]} (attempts=${job[MutationJobsDB.Columns.ATTEMPTS].toString()}) to the runtime: the merge fiber finalizes every merge L1 confirmed before it merges again, once the history owner is Ready. last_error=${job[MutationJobsDB.Columns.LAST_ERROR] ?? "none"}`
        : `Startup left failed local mutation job ${job[MutationJobsDB.Columns.JOB_ID]} (attempts=${job[MutationJobsDB.Columns.ATTEMPTS].toString()},journal_status=${journalStatus ?? "none"}) to the runtime: finalization is retried while its block is live, and a correction removing the block also removes the job. last_error=${job[MutationJobsDB.Columns.LAST_ERROR] ?? "none"}`,
    );
  }
  if (refused.length > 0)
    return yield* Effect.fail(
      new DatabaseInitializationError({
        message:
          "Startup found unfinished local mutation jobs; refusing to serve until recovery is performed",
        cause: refused.map((job) => ({
          jobId: job[MutationJobsDB.Columns.JOB_ID],
          kind: job[MutationJobsDB.Columns.KIND],
          status: job[MutationJobsDB.Columns.STATUS],
          updatedAt: job[MutationJobsDB.Columns.UPDATED_AT].toISOString(),
          lastError: job[MutationJobsDB.Columns.LAST_ERROR],
        })),
      }),
    );
});
