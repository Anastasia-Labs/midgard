import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { Status } from "../database/pendingBlockFinalizations.columns.js";
import type { Globals } from "../services/globals.globals.js";
import type { WorkerOutput } from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import { watchRetainedBlockSubmission } from "./unresolved-block-submission.js";

export type CommitWorkerFailureJournalEvidence = {
  readonly headerHash: Buffer;
  readonly intendedTxHash: Buffer | null;
  readonly submittedTxHash: Buffer | null;
  readonly status: string;
};

export type RetrieveCommitWorkerFailureJournalEvidence<E, R> = (
  stateQueueLeaseToken: string,
) => Effect.Effect<readonly CommitWorkerFailureJournalEvidence[], E, R>;

const encodeJournalEvidence = (
  row: CommitWorkerFailureJournalEvidence,
): string =>
  `header_hash=${row.headerHash.toString("hex")},status=${row.status},intended_tx_hash=${row.intendedTxHash?.toString("hex") ?? "null"},submitted_tx_hash=${row.submittedTxHash?.toString("hex") ?? "null"}`;

/**
 * An abandoned row whose commit was never signed: the submit path abandons
 * only such rows, while a state-queue correction or journal disposal abandons
 * signed or submitted rows too, whose commit may still be on L1.
 */
const isAbandonedUnsigned = (row: CommitWorkerFailureJournalEvidence) =>
  row.status === Status.Abandoned &&
  row.intendedTxHash === null &&
  row.submittedTxHash === null;

/**
 * A typed worker failure can release its state-queue mutation lease only when
 * the durable journal proves that no submission under the lease can be on L1:
 * either preparation never started, or every journal row is abandoned without
 * a journaled signed intent or submitted hash. The lookup includes every
 * journal status: a pending row with a null submitted hash cannot rule out a
 * crash or uncertain submit.
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
      if (!rows.every(isAbandonedUnsigned)) {
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
        `commit_worker_failure_lease_disposition=${rows.length === 0 ? "provably_pre_mutation" : "abandoned_pre_broadcast"} state_queue_lease_token=${stateQueueLeaseToken} error=${output.error}`,
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
          rows.map((row) => ({
            headerHash: row[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
            intendedTxHash:
              row[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] ?? null,
            submittedTxHash:
              row[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH],
            status: row[PendingBlockFinalizationsDB.Columns.STATUS],
          })),
        ),
      ),
  });
