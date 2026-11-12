import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import type { Globals } from "../services/globals.globals.js";
import type { WorkerOutput } from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import { watchRetainedBlockSubmission } from "./unresolved-block-submission.js";

export type CommitWorkerFailureJournalEvidence = {
  readonly headerHash: Buffer;
  readonly intendedTxHash: Buffer | null;
  /** The journal's derived status (`ownBlockStatusSql`). */
  readonly status: string;
  readonly dead: boolean;
};

export type RetrieveCommitWorkerFailureJournalEvidence<E, R> = (
  stateQueueLeaseToken: string,
) => Effect.Effect<readonly CommitWorkerFailureJournalEvidence[], E, R>;

const encodeJournalEvidence = (
  row: CommitWorkerFailureJournalEvidence,
): string =>
  `header_hash=${row.headerHash.toString("hex")},status=${row.status},dead=${row.dead.toString()},intended_tx_hash=${row.intendedTxHash?.toString("hex") ?? "null"}`;

/**
 * A typed worker failure can release its state-queue mutation lease only when
 * the durable journal proves that no submission under the lease can be on L1:
 * either preparation never started (or its unsigned journal was discarded),
 * or no journal row under the lease records a signed intent. A commit is
 * broadcast only once its signed intent is journaled
 * (`recordSignedIntent`), so a row without one was never broadcast; a row
 * with one may be on L1 whatever its status.
 */
export const classifyCommitWorkerOutputForMutationLease = <E, R>({
  output,
  stateQueueLeaseToken,
  retrieveJournalEvidence,
}: {
  readonly output: WorkerOutput;
  readonly stateQueueLeaseToken?: string;
  readonly retrieveJournalEvidence: RetrieveCommitWorkerFailureJournalEvidence<
    E,
    R
  >;
}): Effect.Effect<WorkerOutput, WorkerError, R> => {
  if (output.type !== "FailureOutput") return Effect.succeed(output);
  if (stateQueueLeaseToken === undefined) {
    return Effect.fail(
      new WorkerError({
        worker: "commit-block-header",
        message:
          "Commitment worker failed without a state-queue lease token for durable journal safety classification",
        cause: output.error,
      }),
    );
  }

  return retrieveJournalEvidence(stateQueueLeaseToken).pipe(
    Effect.mapError(
      (cause) =>
        new WorkerError({
          worker: "commit-block-header",
          message:
            "Commitment worker failed and durable journal safety classification failed",
          cause: { workerFailure: output.error, journalLookupFailure: cause },
        }),
    ),
    Effect.flatMap((rows) => {
      if (!rows.every((row) => row.intendedTxHash === null)) {
        return Effect.fail(
          new WorkerError({
            worker: "commit-block-header",
            message:
              "Commitment worker failed after durable mutation preparation may have started",
            cause: {
              workerFailure: output.error,
              journalEvidence: rows.map(encodeJournalEvidence),
            },
          }),
        );
      }

      // The failure itself is `commit_worker_failed`; this benign
      // disposition repeats on every failing tick.
      return Effect.logDebug(
        `commit_worker_failure_lease_disposition=${rows.length === 0 ? "provably_pre_mutation" : "unsigned_pre_broadcast"} state_queue_lease_token=${stateQueueLeaseToken} error=${output.error}`,
      ).pipe(Effect.as(output));
    }),
  );
};

/**
 * Classifies `output` against its lease's journal rows, read once; a commit tx
 * those rows retain also becomes the confirmation's unresolved submission.
 */
export const classifyCommitWorkerOutputAgainstLeaseJournal = (
  globals: Globals,
  output: WorkerOutput,
  stateQueueLeaseToken?: string,
) =>
  classifyCommitWorkerOutputForMutationLease({
    output,
    stateQueueLeaseToken,
    retrieveJournalEvidence: (token) =>
      PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(token).pipe(
        Effect.tap((rows) => watchRetainedBlockSubmission(globals, rows)),
        Effect.map((rows) =>
          rows.map(({ row, status, dead }) => ({
            headerHash: row[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
            intendedTxHash:
              row[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] ?? null,
            status,
            dead,
          })),
        ),
      ),
  });
