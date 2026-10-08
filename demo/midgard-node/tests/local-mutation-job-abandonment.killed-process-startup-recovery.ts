import "./local-mutation-job-abandonment.startup-gate-over-unfinished-local-mutation-jobs.js";

import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import * as MempoolDB from "../src/database/mempool.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { Columns as TxColumns } from "../src/database/utils/tx.js";
import { finalizeCommittedBlockLocally } from "../src/workers/utils/commit-submission.finalize-committed-block-locally.js";
import { withLocalBlockFinalizationJob } from "../src/workers/utils/commit-submission.with-local-block-finalization-job.js";
import { makeProofSubmitTx } from "./database.test/fixtures.make-material-proof-submit-tx.js";
import {
  failedLocalJob,
  header,
  isolatedDb,
  J,
  journalFixture,
  localJobId,
  observedJournal,
  readJob,
  runningLocalJob,
  startupGate,
  Status,
  withLogs,
} from "./local-mutation-job-abandonment.journal-fixture.js";

const journalStatus = (headerHash: Buffer) =>
  PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash).pipe(
    Effect.map((journal) =>
      journal._tag === "Some"
        ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
        : undefined,
    ),
  );

/** A submitted journal in `status`, as a killed process can leave it. Each
 * header gets its own submitted tx hash, which the table keeps unique. */
const submittedJournal = (
  headerHash: Buffer,
  status: PendingBlockFinalizationsDB.Status,
) =>
  Effect.gen(function* () {
    yield* PendingBlockFinalizationsDB.preparePendingSubmission(
      journalFixture(headerHash),
    );
    // Leaves the journal submitted_local_finalization_pending.
    yield* PendingBlockFinalizationsDB.markSubmitted(
      headerHash,
      Buffer.concat([headerHash, Buffer.alloc(4)]),
    );
    if (status === Status.SubmittedUnconfirmed) {
      // Its only writer, markLocalFinalizationComplete, has no caller; startup
      // still honours rows written before, so it is set directly.
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE pending_block_finalizations SET status = ${status}
        WHERE header_hash = ${headerHash}`;
    }
    if (
      status === Status.ObservedWaitingStability ||
      status === Status.LocallyApplied
    )
      yield* PendingBlockFinalizationsDB.markObservedWaitingStability(
        headerHash,
        1n,
      );
    if (status === Status.LocallyApplied)
      yield* PendingBlockFinalizationsDB.markFinalized(headerHash);
  });

describe("startup after a process killed mid local finalization", () => {
  for (const status of [
    Status.SubmittedLocalFinalizationPending,
    Status.SubmittedUnconfirmed,
    Status.ObservedWaitingStability,
  ])
    it.effect(
      `hands a running job of a ${status} block to the runtime untouched`,
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const killed = header(`killed-${status}`);
            yield* submittedJournal(killed, status);
            yield* runningLocalJob(killed);
            const { result, logs } = yield* withLogs(startupGate);
            expect(result).toBeUndefined();
            expect(logs.join("\n")).toContain(
              `Startup left running local mutation job ${localJobId(killed)}`,
            );
            const job = yield* readJob(localJobId(killed));
            expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Running);
            expect(job[J.ATTEMPTS]).toBe(1);
            expect(yield* journalStatus(killed)).toBe(status);
          }),
        ),
    );

  it.effect(
    "completes a running or failed job whose journal is already finalized",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const done = header("killed-after-mark-finalized");
          yield* runningLocalJob(done);
          yield* submittedJournal(done, Status.LocallyApplied);
          const failedDone = header("failed-after-mark-finalized");
          yield* failedLocalJob(failedDone, "ack lost");
          yield* submittedJournal(failedDone, Status.LocallyApplied);
          expect(yield* startupGate).toBeUndefined();
          for (const finalized of [done, failedDone]) {
            const job = yield* readJob(localJobId(finalized));
            expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Completed);
            expect(job[J.COMPLETED_AT]).not.toBeNull();
            expect(yield* journalStatus(finalized)).toBe(Status.LocallyApplied);
          }
        }),
      ),
  );

  it.effect(
    "still refuses a running job with no journal, an unsubmitted journal or an abandoned one",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          // One active journal at a time: the abandoned one comes first.
          const removed = header("running-removed");
          yield* submittedJournal(removed, Status.ObservedWaitingStability);
          yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
            removed,
            "01".repeat(32),
          );
          yield* runningLocalJob(removed);
          expect(yield* journalStatus(removed)).toBe(Status.Abandoned);
          const unsubmitted = header("running-unsubmitted");
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(unsubmitted),
          );
          yield* runningLocalJob(unsubmitted);
          const orphan = header("running-orphan");
          yield* runningLocalJob(orphan);
          const refused = yield* startupGate;
          expect([...(refused ?? [])].sort()).toEqual(
            [
              localJobId(orphan),
              localJobId(unsubmitted),
              localJobId(removed),
            ].sort(),
          );
          for (const headerHash of [orphan, unsubmitted, removed])
            expect((yield* readJob(localJobId(headerHash)))[J.STATUS]).toBe(
              MutationJobsDB.Status.Running,
            );
        }),
      ),
  );

  it.effect(
    "lets the runtime retry a finalization killed after its SQL commit without applying the block twice",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`TRUNCATE TABLE mempool, processed_mempool, immutable, blocks
            RESTART IDENTITY CASCADE`;
          const killed = header("killed-after-finalization-sql");
          const headerHex = killed.toString("hex");
          yield* observedJournal(killed);
          const tx = makeProofSubmitTx();
          yield* MempoolDB.insertMultipleCore([
            {
              txId: tx.txId,
              txCbor: tx.txCanonicalCbor,
              spent: [],
              produced: [],
            },
          ]);
          const entry = (yield* MempoolDB.retrievePage({
            limit: 10,
          })).entries.find((row) => row[TxColumns.TX_ID].equals(tx.txId))!;
          const transactionsMpf = {
            resetToEmpty: () => Effect.void,
          } as unknown as Parameters<typeof finalizeCommittedBlockLocally>[0];
          const finalize = finalizeCommittedBlockLocally(
            transactionsMpf,
            [entry],
            [tx.txId],
            headerHex,
            [],
            { useAmbientProcessedMempool: false },
          );
          // The killed attempt: the job started and the finalization SQL
          // committed; the process died before markFinalized/markCompleted.
          yield* MutationJobsDB.start({
            jobId: localJobId(killed),
            kind: MutationJobsDB.Kind.LocalBlockFinalization,
          });
          yield* finalize;

          expect(yield* startupGate).toBeUndefined();

          // The runtime's retry, shaped as successfulLocalFinalizationRecoveryProgram.
          yield* withLocalBlockFinalizationJob(
            {
              headerHash: headerHex,
              mempoolTxCount: 1,
              includedDepositCount: 0,
              includedForcedTransactionCount: 0,
              includedWithdrawalCount: 0,
            },
            finalize.pipe(
              Effect.zipRight(
                PendingBlockFinalizationsDB.markFinalized(killed),
              ),
            ),
          );
          const job = yield* readJob(localJobId(killed));
          expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Completed);
          expect(job[J.ATTEMPTS]).toBe(2);
          expect(yield* journalStatus(killed)).toBe(Status.LocallyApplied);
          const immutable = yield* sql<{ readonly count: string }>`SELECT
            COUNT(*)::text AS count FROM immutable WHERE tx_id = ${tx.txId}`;
          const blocks = yield* sql<{ readonly count: string }>`SELECT
            COUNT(*)::text AS count FROM blocks
            WHERE header_hash = ${killed} AND tx_id = ${tx.txId}`;
          const mempool = yield* sql<{ readonly count: string }>`SELECT
            COUNT(*)::text AS count FROM mempool WHERE tx_id = ${tx.txId}`;
          expect(
            [immutable, blocks, mempool].map((rows) => rows[0]!.count),
          ).toEqual(["1", "1", "0"]);
          expect(yield* startupGate).toBeUndefined();
        }),
      ),
  );
});
