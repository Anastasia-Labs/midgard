import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Logger } from "effect";
import { describe, expect } from "vitest";

import {
  assertStartupMutationJobsRecoverable,
  classifyUnfinishedMutationJobOnStartup,
} from "../src/commands/listen-startup.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { describeLocalFinalizationFailure } from "../src/workers/utils/commit-submission.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

const J = MutationJobsDB.Columns;
const Status = PendingBlockFinalizationsDB.Status;
const STARTUP_REFUSAL =
  "Startup found unfinished local mutation jobs; refusing to serve until recovery is performed";

/** Clears only the tables these tests write. The shared application-table
 * reset replays the schema's seed section, which is currently not
 * replayable. */
const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE TABLE local_mutation_jobs, pending_block_finalizations
        RESTART IDENTITY CASCADE`;
      return yield* effect;
    }),
  );

const header = (label: string) =>
  deterministicFixtureBytes(`local-mutation-job-abandonment:${label}`, 28);

/** An empty-block journal for `headerHash`, ready for preparePendingSubmission. */
const journalFixture = (
  headerHash: Buffer,
): PendingBlockFinalizationsDB.PrepareInput => {
  const blockStartTime = new Date("2026-06-12T00:00:00.000Z");
  const emptyRoots = {
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const expectedRoots = {
    ...emptyRoots,
    transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const expectedCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const blockHeader: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    ...expectedRoots,
    ...expectedCounts,
    startTime: 1n,
    endTime: 2n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  return {
    headerHash,
    headerCbor: Buffer.from(
      LucidData.to(blockHeader as never, SDK.Header as never),
      "hex",
    ),
    metadata: {
      deploymentMarker: makeDeploymentMarker("de".repeat(32)),
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
      stateQueueLeaseToken: "lease-token",
      baseSnapshotId: "snapshot",
      baseTailOutRef: "base#0",
      baseTailHeaderHash: header("base-tail"),
      baseTailDatumCbor: "d87980",
      baseRoots: emptyRoots,
      blockStartTime,
      expectedRoots,
      expectedCounts,
    },
    blockEndTime: new Date(blockStartTime.getTime() + 60_000),
    depositEventIds: [],
    depositEntries: [],
    forcedTransactionEventIds: [],
    forcedTransactionEntries: [],
    withdrawalEventIds: [],
    withdrawalEntries: [],
    mempoolTxIds: [],
    mempoolTxs: [],
    mempoolTxSourceTable: "none",
    transitionTraceMembers: [],
    eventToStepMembers: [],
    validationTraceMembers: [],
    validationTraceWitnessMembers: [],
    ledgerDelta: { spent: [], produced: [] },
  };
};

/** A submitted block awaiting stability, as the live f5215638 journal. */
const observedJournal = (headerHash: Buffer) =>
  Effect.gen(function* () {
    yield* PendingBlockFinalizationsDB.preparePendingSubmission(
      journalFixture(headerHash),
    );
    yield* PendingBlockFinalizationsDB.markSubmitted(
      headerHash,
      Buffer.alloc(32, 7),
    );
    yield* PendingBlockFinalizationsDB.markObservedWaitingStability(
      headerHash,
      1n,
    );
  });

const localJobId = (headerHash: Buffer) =>
  MutationJobsDB.localBlockFinalizationJobId(headerHash.toString("hex"));

const runningLocalJob = (headerHash: Buffer) =>
  MutationJobsDB.start({
    jobId: localJobId(headerHash),
    kind: MutationJobsDB.Kind.LocalBlockFinalization,
    payload: { mempoolTxCount: 2 },
  });

const failedLocalJob = (headerHash: Buffer, error = "finalization failed") =>
  Effect.gen(function* () {
    yield* runningLocalJob(headerHash);
    yield* MutationJobsDB.markFailed(localJobId(headerHash), error);
  });

/** The job row, or none once removed. */
const findJob = (jobId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<MutationJobsDB.Entry>`SELECT * FROM
      local_mutation_jobs WHERE job_id = ${jobId}`;
    expect(rows.length).toBeLessThanOrEqual(1);
    return rows[0];
  });

const readJob = (jobId: string) =>
  Effect.gen(function* () {
    const row = yield* findJob(jobId);
    if (row === undefined) throw new Error(`Missing job ${jobId}`);
    return row;
  });

/** Runs `effect` collecting every log message it emits. */
const withLogs = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const logs: string[] = [];
    const result = yield* effect.pipe(
      Effect.provide(
        Logger.replace(
          Logger.defaultLogger,
          Logger.make(({ message }) => {
            logs.push(
              (Array.isArray(message) ? message : [message])
                .map(String)
                .join(" "),
            );
          }),
        ),
      ),
    );
    return { result, logs };
  });

/** The startup gate's refusal, with the job ids it names; none if it passed. */
const startupGate = Effect.gen(function* () {
  const exit = yield* Effect.exit(assertStartupMutationJobsRecoverable);
  if (Exit.isSuccess(exit)) return undefined;
  const failure = Cause.failureOption(exit.cause);
  if (failure._tag === "None") throw new Error(Cause.pretty(exit.cause));
  const error = failure.value as {
    readonly message: string;
    readonly cause: readonly { readonly jobId: string }[];
  };
  expect(error.message).toBe(STARTUP_REFUSAL);
  return error.cause.map(({ jobId }) => jobId);
});

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
    "refuses a failed merge finalization and a failed local finalization without a journal, naming only those",
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
          const refused = yield* startupGate;
          expect(new Set(refused)).toEqual(
            new Set([localJobId(orphan), mergeJobId]),
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

  it("classifies only a failed local finalization of a submitted, unfinalized block as runtime-owned", () => {
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
    expect(
      classifyUnfinishedMutationJobOnStartup(
        job({
          [J.KIND]: MutationJobsDB.Kind.ConfirmedMergeFinalization,
          [J.JOB_ID]: MutationJobsDB.confirmedMergeFinalizationJobId(
            header("classified").toString("hex"),
          ),
        }),
        Status.ObservedWaitingStability,
      ),
    ).toBe("refuse");
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
