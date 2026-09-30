import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  failedLocalJob,
  findJob,
  header,
  isolatedDb,
  J,
  journalFixture,
  localJobId,
  observedJournal,
  readJob,
  runningLocalJob,
  withLogs,
} from "./local-mutation-job-abandonment.journal-fixture.js";

describe("local-finalization job abandonment", () => {
  it.effect(
    "abandoning a removed block's journal removes only that header's local-finalization job, logging its diagnosis",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const removed = header("removed");
          const other = header("other");
          yield* observedJournal(removed);
          yield* failedLocalJob(
            removed,
            "DatabaseError: ledger delta invalid; cause=Error: absent outref",
          );
          yield* failedLocalJob(other);
          const mergeJobId = MutationJobsDB.confirmedMergeFinalizationJobId(
            removed.toString("hex"),
          );
          yield* MutationJobsDB.start({
            jobId: mergeJobId,
            kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
          });
          yield* MutationJobsDB.markFailed(mergeJobId, "merge failed");
          expect(yield* MutationJobsDB.countUnfinished).toBe(3n);

          const { logs } = yield* withLogs(
            PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
              removed,
              "cd".repeat(32),
            ),
          );

          expect(yield* findJob(localJobId(removed))).toBeUndefined();
          // The diagnosis survives the row: one line with the header,
          // attempts, reason and cause-chained last error.
          const removal = logs.filter((line) =>
            line.includes("Removing moot local block finalization job"),
          );
          expect(removal).toHaveLength(1);
          expect(removal[0]).toContain(`header=${removed.toString("hex")}`);
          expect(removal[0]).toContain("status=failed");
          expect(removal[0]).toContain("attempts=1");
          expect(removal[0]).toContain(
            `reason=block removed on L1 by admitted state-queue correction ${"cd".repeat(32)}`,
          );
          expect(removal[0]).toContain(
            "last_error=DatabaseError: ledger delta invalid; cause=Error: absent outref",
          );
          // Another header's job, and another kind of job for this header,
          // are untouched.
          expect((yield* readJob(localJobId(other)))[J.STATUS]).toBe(
            MutationJobsDB.Status.Failed,
          );
          expect((yield* readJob(mergeJobId))[J.STATUS]).toBe(
            MutationJobsDB.Status.Failed,
          );
          expect(yield* MutationJobsDB.countUnfinished).toBe(2n);
          expect(
            (yield* MutationJobsDB.retrieveUnfinished).map(
              (job) => job[J.JOB_ID],
            ),
          ).toEqual([localJobId(other), mergeJobId]);
          // A late failure or completion from an attempt still in flight
          // cannot bring the row back.
          yield* MutationJobsDB.markFailed(localJobId(removed), "late failure");
          yield* MutationJobsDB.markCompleted(localJobId(removed));
          expect(yield* findJob(localJobId(removed))).toBeUndefined();
          expect(yield* MutationJobsDB.countUnfinished).toBe(2n);
          // A revived journal replays its finalization: start tracks a fresh
          // attempt.
          yield* runningLocalJob(removed);
          const replayed = yield* readJob(localJobId(removed));
          expect(replayed[J.STATUS]).toBe(MutationJobsDB.Status.Running);
          expect(replayed[J.ATTEMPTS]).toBe(1);
        }),
      ),
  );

  it.effect(
    "a running job of a removed block is removed too, and nothing is removed when the journal abandonment fails",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const removed = header("removed-running");
          yield* observedJournal(removed);
          yield* runningLocalJob(removed);
          yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
            removed,
            "ef".repeat(32),
          );
          expect(yield* findJob(localJobId(removed))).toBeUndefined();

          // A journal the correction path may not abandon: the whole
          // transaction fails and the job keeps its failure.
          const unsubmitted = header("unsubmitted");
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(unsubmitted),
          );
          yield* failedLocalJob(unsubmitted, "kept failure");
          const refused = yield* Effect.either(
            PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
              unsubmitted,
              "ef".repeat(32),
            ),
          );
          expect(refused._tag).toBe("Left");
          expect(yield* readJob(localJobId(unsubmitted))).toMatchObject({
            [J.STATUS]: MutationJobsDB.Status.Failed,
            [J.LAST_ERROR]: "kept failure",
          });
          // Abandoning it before submission removes it.
          const { result, logs } = yield* withLogs(
            PendingBlockFinalizationsDB.markUnsubmittedAbandoned(unsubmitted),
          );
          expect(result).toBe(true);
          expect(yield* findJob(localJobId(unsubmitted))).toBeUndefined();
          expect(
            logs.filter((line) =>
              line.includes(
                "reason=pending block journal abandoned before submission",
              ),
            ),
          ).toHaveLength(1);
        }),
      ),
  );

  it.effect(
    "a completed job keeps its record when its journal is abandoned",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const removed = header("completed");
          yield* observedJournal(removed);
          yield* runningLocalJob(removed);
          yield* MutationJobsDB.markCompleted(localJobId(removed));
          yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
            removed,
            "ab".repeat(32),
          );
          expect((yield* readJob(localJobId(removed)))[J.STATUS]).toBe(
            MutationJobsDB.Status.Completed,
          );
        }),
      ),
  );
});
