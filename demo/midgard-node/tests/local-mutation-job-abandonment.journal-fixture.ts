import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Logger } from "effect";
import { expect } from "vitest";

import { assertStartupMutationJobsRecoverable } from "../src/commands/listen-startup.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

export const J = MutationJobsDB.Columns;

export const Status = PendingBlockFinalizationsDB.Status;

const STARTUP_REFUSAL =
  "Startup found unfinished local mutation jobs; refusing to serve until recovery is performed";

/** Runs `effect` on a freshly reset database: the fixture producer gate
 * refuses any acquired owner row an earlier file on the same worker shard
 * left behind. */
export const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

export const header = (label: string) =>
  deterministicFixtureBytes(`local-mutation-job-abandonment:${label}`, 28);

/** An empty-block journal for `headerHash`, ready for preparePendingSubmission. */
export const journalFixture = (
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
export const observedJournal = (headerHash: Buffer) =>
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

export const localJobId = (headerHash: Buffer) =>
  MutationJobsDB.localBlockFinalizationJobId(headerHash.toString("hex"));

export const runningLocalJob = (headerHash: Buffer) =>
  MutationJobsDB.start({
    jobId: localJobId(headerHash),
    kind: MutationJobsDB.Kind.LocalBlockFinalization,
    payload: { mempoolTxCount: 2 },
  });

export const failedLocalJob = (
  headerHash: Buffer,
  error = "finalization failed",
) =>
  Effect.gen(function* () {
    yield* runningLocalJob(headerHash);
    yield* MutationJobsDB.markFailed(localJobId(headerHash), error);
  });

/** The job row, or none once removed. */
export const findJob = (jobId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<MutationJobsDB.Entry>`SELECT * FROM
      local_mutation_jobs WHERE job_id = ${jobId}`;
    expect(rows.length).toBeLessThanOrEqual(1);
    return rows[0];
  });

export const readJob = (jobId: string) =>
  Effect.gen(function* () {
    const row = yield* findJob(jobId);
    if (row === undefined) throw new Error(`Missing job ${jobId}`);
    return row;
  });

/** Runs `effect` collecting every log message it emits. */
export const withLogs = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
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
export const startupGate = Effect.gen(function* () {
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
