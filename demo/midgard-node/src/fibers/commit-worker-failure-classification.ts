import { Effect } from "effect";

import type { WorkerOutput } from "@/workers/utils/commit-block-header.js";
import { WorkerError } from "@/workers/utils/common.js";

export type CommitWorkerFailureJournalEvidence = {
  readonly headerHash: Buffer;
  readonly submittedTxHash: Buffer | null;
  readonly status: string;
};

export type RetrieveCommitWorkerFailureJournalEvidence<E, R> = (
  stateQueueLeaseToken: string,
) => Effect.Effect<readonly CommitWorkerFailureJournalEvidence[], E, R>;

const encodeJournalEvidence = (
  row: CommitWorkerFailureJournalEvidence,
): string =>
  `header_hash=${row.headerHash.toString("hex")},status=${row.status},submitted_tx_hash=${row.submittedTxHash?.toString("hex") ?? "null"}`;

/**
 * A typed worker failure can release its state-queue mutation lease only when
 * the durable journal proves that submission preparation never started. The
 * lookup deliberately includes every journal status: pending or abandoned
 * rows with a null submitted hash cannot rule out a crash or uncertain submit.
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
      if (rows.length > 0) {
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

      return Effect.logWarning(
        `commit_worker_failure_lease_disposition=provably_pre_mutation state_queue_lease_token=${stateQueueLeaseToken} error=${output.error}`,
      ).pipe(Effect.as(output));
    }),
  );
};
