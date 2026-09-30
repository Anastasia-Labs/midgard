import "./local-mutation-job-abandonment.local-finalization-job-abandonment.js";

import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { classifyUnfinishedMutationJobOnStartup } from "../src/commands/listen-startup.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { describeLocalFinalizationFailure } from "../src/workers/utils/commit-submission.js";
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
    "hands a failed local finalization of a live submitted block to the runtime without closing it",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const live = header("live");
          yield* observedJournal(live);
          yield* failedLocalJob(live, "delta invalid");
          expect(yield* startupGate).toBeUndefined();
          const job = yield* readJob(localJobId(live));
          expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Failed);
          expect(job[J.LAST_ERROR]).toBe("delta invalid");
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
    "still refuses a running job from a crash, exactly as before, even for a live submitted block",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const crashed = header("crashed");
          yield* observedJournal(crashed);
          yield* runningLocalJob(crashed);
          expect(yield* startupGate).toEqual([localJobId(crashed)]);
          expect((yield* readJob(localJobId(crashed)))[J.STATUS]).toBe(
            MutationJobsDB.Status.Running,
          );
        }),
      ),
  );

  it.effect(
    "refuses a failed local finalization without a journal and leaves failed and running merge finalizations to the runtime",
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
        yield* observedJournal(removed);
        yield* runningLocalJob(removed);
        expect(yield* startupGate).toEqual([localJobId(removed)]);
        yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
          removed,
          "01".repeat(32),
        );
        expect(yield* startupGate).toBeUndefined();
      }),
    ),
  );

  it("classifies merge finalizations and a failed local finalization of a submitted, unfinalized block as runtime-owned", () => {
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
    const runtime: readonly PendingBlockFinalizationsDB.Status[] = [
      Status.SubmittedLocalFinalizationPending,
      Status.SubmittedUnconfirmed,
      Status.ObservedWaitingStability,
    ];
    for (const status of Object.values(Status))
      expect(
        classifyUnfinishedMutationJobOnStartup(job({}), status),
        status,
      ).toBe(runtime.includes(status) ? "runtime" : "refuse");
    expect(classifyUnfinishedMutationJobOnStartup(job({}), undefined)).toBe(
      "refuse",
    );
    expect(
      classifyUnfinishedMutationJobOnStartup(
        job({ [J.STATUS]: MutationJobsDB.Status.Running }),
        Status.ObservedWaitingStability,
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

describe("local finalization failure diagnostics", () => {
  it("includes the DatabaseError's cause chain, bounded and cycle-safe", () => {
    const reason = new Error(
      "ledger delta spends an outref absent from its authenticated base: abab",
    );
    const error = new DatabaseError({
      table: "confirmed_ledger",
      message:
        "Pending-finalization ledger delta is invalid for its authenticated base",
      cause: reason,
    });
    const text = describeLocalFinalizationFailure(error);
    expect(text).toContain(
      "Pending-finalization ledger delta is invalid for its authenticated base",
    );
    expect(text).toContain(
      "cause=Error: ledger delta spends an outref absent from its authenticated base: abab",
    );
    const cyclic = new Error("outer") as Error & { cause?: unknown };
    cyclic.cause = cyclic;
    expect(describeLocalFinalizationFailure(cyclic)).toBe("Error: outer");
    const huge = new DatabaseError({
      table: "t",
      message: "x".repeat(10_000),
      cause: new Error("y".repeat(10_000)),
    });
    expect(describeLocalFinalizationFailure(huge).length).toBe(4000);
    let deep: unknown = new Error("root");
    for (let i = 0; i < 20; i += 1)
      deep = new Error(`level ${i}`, { cause: deep });
    expect(
      describeLocalFinalizationFailure(deep).split("; cause=").length,
    ).toBe(8);
  });
});
