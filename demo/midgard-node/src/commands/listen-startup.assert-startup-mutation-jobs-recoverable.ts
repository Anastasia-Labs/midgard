import { Effect, Option } from "effect";

import {
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { DatabaseInitializationError } from "../services/index.js";
import {
  classifyUnfinishedMutationJobOnStartup,
  unfinishedLocalFinalizationHeader,
} from "./listen-startup.seed-latest-local-block-boundary-on-startup.js";

/**
 * Startup gate over the mutation jobs a killed node process left unfinished.
 * It runs while this process holds the node instance lock, so no other node
 * process is live on this database; after pending history reconciliation (a
 * correction rewind that abandons a removed block's journal also removes its
 * job) and before any fiber starts. It closes a local finalization that
 * finished all but its markCompleted, and refuses to serve while any job
 * needs operator recovery; see classifyUnfinishedMutationJobOnStartup.
 * Otherwise it only reads; the previous process's state-queue leases are a
 * separate startup step (releaseStateQueueLeasesOfPreviousNodeProcess).
 */
export const assertStartupMutationJobsRecoverable = Effect.gen(function* () {
  const unfinished = yield* MutationJobsDB.retrieveUnfinished;
  const refused: MutationJobsDB.Entry[] = [];
  for (const job of unfinished) {
    const jobId = job[MutationJobsDB.Columns.JOB_ID];
    const header = unfinishedLocalFinalizationHeader(job);
    const journal =
      header === undefined
        ? Option.none()
        : yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(header);
    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : undefined;
    const disposition = classifyUnfinishedMutationJobOnStartup(
      job,
      journalStatus,
    );
    if (disposition === "refuse") {
      refused.push(job);
      continue;
    }
    if (disposition === "complete") {
      yield* MutationJobsDB.markCompleted(jobId).pipe(withFollowerWrite);
      yield* Effect.logWarning(
        `Startup completed local mutation job ${jobId} (attempts=${job[MutationJobsDB.Columns.ATTEMPTS].toString()}): its journal is finalized, so the process that ran it ended after its last durable step and before recording completion.`,
      );
      continue;
    }
    yield* Effect.logWarning(
      job[MutationJobsDB.Columns.KIND] ===
        MutationJobsDB.Kind.ConfirmedMergeFinalization
        ? `Startup left ${job[MutationJobsDB.Columns.STATUS]} confirmed-merge finalization job ${jobId} (attempts=${job[MutationJobsDB.Columns.ATTEMPTS].toString()}) to the runtime: the merge fiber finalizes every merge L1 confirmed before it merges again, once the follower-change driver has published its view. last_error=${job[MutationJobsDB.Columns.LAST_ERROR] ?? "none"}`
        : `Startup left ${job[MutationJobsDB.Columns.STATUS]} local mutation job ${jobId} (attempts=${job[MutationJobsDB.Columns.ATTEMPTS].toString()},journal_status=${journalStatus ?? "none"}) to the runtime: finalization is retried while its block is live, and a correction removing the block also removes the job. last_error=${job[MutationJobsDB.Columns.LAST_ERROR] ?? "none"}`,
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
