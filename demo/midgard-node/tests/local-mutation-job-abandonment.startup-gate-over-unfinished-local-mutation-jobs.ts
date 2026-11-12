import "./local-mutation-job-abandonment.local-finalization-job-abandonment.js";

import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { classifyUnfinishedMutationJobOnStartup } from "../src/commands/listen-startup.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  failedLocalJob,
  header,
  isolatedDb,
  J,
  localJobId,
  observedJournal,
  readJob,
  runningLocalJob,
  startupGate,
  Status,
  withLogs,
} from "./local-mutation-job-abandonment.journal-fixture.js";

describe("startup gate over unfinished local mutation jobs", () => {
  it.effect(
    "closes a failed local finalization of a submitted block, which landed-block processing applies, leaving its journal as it is",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const live = header("live");
          yield* observedJournal(live);
          yield* failedLocalJob(live, "delta invalid");
          expect(yield* startupGate).toBeUndefined();
          const job = yield* readJob(localJobId(live));
          expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Completed);
          expect(
            (yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              live,
            )).pipe((journal) =>
              journal._tag === "Some"
                ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
                : undefined,
            ),
          ).toBe(Status.ObservedWaitingStability);
        }),
      ),
  );

  it.effect(
    "closes a running job from a crash for a submitted block, instead of refusing every restart",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const crashed = header("crashed");
          yield* observedJournal(crashed);
          yield* runningLocalJob(crashed);
          expect(yield* startupGate).toBeUndefined();
          expect((yield* readJob(localJobId(crashed)))[J.STATUS]).toBe(
            MutationJobsDB.Status.Completed,
          );
        }),
      ),
  );

  it.effect(
    "refuses a failed local finalization without a journal, closes one of a submitted block, and leaves failed and running merge finalizations to the runtime",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const live = header("live-with-others");
          yield* observedJournal(live);
          yield* failedLocalJob(live);
          const orphan = header("orphan");
          yield* failedLocalJob(orphan);
          const mergeJobId = MutationJobsDB.confirmedMergeFinalizationJobId(
            live.toString("hex"),
          );
          yield* MutationJobsDB.start({
            jobId: mergeJobId,
            kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
          });
          yield* MutationJobsDB.markFailed(mergeJobId, "merge failed");
          const crashedMergeJobId =
            MutationJobsDB.confirmedMergeFinalizationJobId(
              header("crashed-merge").toString("hex"),
            );
          yield* MutationJobsDB.start({
            jobId: crashedMergeJobId,
            kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
          });
          const { result: refused, logs } = yield* withLogs(startupGate);
          expect(refused).toEqual([localJobId(orphan)]);
          expect((yield* readJob(localJobId(live)))[J.STATUS]).toBe(
            MutationJobsDB.Status.Completed,
          );
          const handOffs = logs.filter((line) =>
            line.includes("confirmed-merge finalization job"),
          );
          expect(handOffs).toHaveLength(2);
          expect(handOffs.join("\n")).toContain(
            `failed confirmed-merge finalization job ${mergeJobId}`,
          );
          expect(handOffs.join("\n")).toContain(
            `running confirmed-merge finalization job ${crashedMergeJobId}`,
          );
        }),
      ),
  );

  it.effect("passes once the only unfinished job was abandoned", () =>
    isolatedDb(
      Effect.gen(function* () {
        const removed = header("removed-then-start");
        yield* runningLocalJob(removed);
        expect(yield* startupGate).toEqual([localJobId(removed)]);
        yield* observedJournal(removed);
        yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
          removed,
          "01".repeat(32),
        );
        expect(yield* startupGate).toBeUndefined();
      }),
    ),
  );

  it("leaves merge finalizations to the runtime and closes an unfinished local finalization of a submitted block", () => {
    const job = (
      overrides: Partial<Record<MutationJobsDB.Columns, unknown>>,
    ): MutationJobsDB.Entry =>
      ({
        [J.JOB_ID]: localJobId(header("classified")),
        [J.KIND]: MutationJobsDB.Kind.LocalBlockFinalization,
        [J.STATUS]: MutationJobsDB.Status.Failed,
        [J.PAYLOAD]: {},
        [J.ATTEMPTS]: 1,
        [J.LAST_ERROR]: "failed",
        [J.CREATED_AT]: new Date(0),
        [J.UPDATED_AT]: new Date(0),
        [J.COMPLETED_AT]: null,
        ...overrides,
      }) as MutationJobsDB.Entry;
    // Landed-block processing applies a submitted block when it lands;
    // nothing runs the job any more.
    const complete: readonly PendingBlockFinalizationsDB.Status[] = [
      Status.SubmittedLocalFinalizationPending,
      Status.SubmittedUnconfirmed,
      Status.ObservedWaitingStability,
      Status.LocallyApplied,
    ];
    for (const status of Object.values(Status))
      expect(
        classifyUnfinishedMutationJobOnStartup(job({}), status),
        status,
      ).toBe(complete.includes(status) ? "complete" : "refuse");
    expect(classifyUnfinishedMutationJobOnStartup(job({}), undefined)).toBe(
      "refuse",
    );
    // A running job is a process killed mid-way: the same.
    for (const status of Object.values(Status))
      expect(
        classifyUnfinishedMutationJobOnStartup(
          job({ [J.STATUS]: MutationJobsDB.Status.Running }),
          status,
        ),
        `running/${status}`,
      ).toBe(complete.includes(status) ? "complete" : "refuse");
    expect(
      classifyUnfinishedMutationJobOnStartup(
        job({ [J.STATUS]: MutationJobsDB.Status.Running }),
        undefined,
      ),
    ).toBe("refuse");
    // The merge fiber retries every merge finalization idempotently, failed
    // or interrupted mid-way, whatever its journal records.
    for (const status of [
      MutationJobsDB.Status.Failed,
      MutationJobsDB.Status.Running,
    ])
      for (const journalStatus of [...Object.values(Status), undefined])
        expect(
          classifyUnfinishedMutationJobOnStartup(
            job({
              [J.KIND]: MutationJobsDB.Kind.ConfirmedMergeFinalization,
              [J.JOB_ID]: MutationJobsDB.confirmedMergeFinalizationJobId(
                header("classified").toString("hex"),
              ),
              [J.STATUS]: status,
            }),
            journalStatus,
          ),
          `${status}/${String(journalStatus)}`,
        ).toBe("runtime");
    expect(
      classifyUnfinishedMutationJobOnStartup(
        job({ [J.JOB_ID]: "local_block_finalization:not-a-header" }),
        Status.ObservedWaitingStability,
      ),
    ).toBe("refuse");
  });
});
