/**
 * The commit worker's submission outcomes that keep the block's payload
 * pending: a deferred commit moves its selected mempool rows to
 * `processed_mempool`, and a refused one keeps its transactions MPF for the
 * retry. A commit that lands is applied by landed-block processing
 * (`landed-blocks/own-blocks.ts`).
 */
import { Effect, Schedule } from "effect";

import {
  ProcessedMempoolDB,
  TxUtils as TxTable,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import type { MidgardMpf } from "../../mpf/index.js";
import { type Database } from "../../services/index.js";
import { batchProgram } from "../../utils.js";
import type {
  CommitSubmitRefusal,
  WorkerOutput,
} from "./commit-block-header.js";

const BATCH_SIZE = 100;

const SKIPPED_SUBMISSION_TRANSFER_RETRIES = 2;

const SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF = "250 millis";

export const skippedSubmissionProgram = (
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length !== mempoolTxHashes.length) {
      return yield* Effect.fail(
        new DatabaseError({
          message:
            "Failed to transfer deferred commit payload: tx metadata length mismatch",
          cause: `mempool_txs=${mempoolTxs.length},mempool_tx_hashes=${mempoolTxHashes.length}`,
          table: "mempool,processed_mempool",
        }),
      );
    }
    yield* batchProgram(
      BATCH_SIZE,
      mempoolTxs.length,
      "skipped-submission-db-transfer",
      (startIndex: number, endIndex: number) =>
        Effect.gen(function* () {
          // The rows move as they are now: a mark a block set after the
          // selection moves with its row.
          yield* ProcessedMempoolDB.moveFromMempool(
            mempoolTxHashes.slice(startIndex, endIndex),
          ).pipe(Effect.withSpan(`mempool-db-move-txs-${startIndex}`));
        }),
      1,
    );
  }).pipe(
    Effect.retry(
      Schedule.compose(
        Schedule.exponential(SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF),
        Schedule.recurs(SKIPPED_SUBMISSION_TRANSFER_RETRIES),
      ),
    ),
  );

export const failedSubmissionProgram = (
  transactionsMpf: MidgardMpf,
  mempoolTxsCount: number,
  sizeOfProcessedTxs: number,
  submitRefusal: CommitSubmitRefusal,
): Effect.Effect<WorkerOutput> =>
  Effect.gen(function* () {
    yield* Effect.logError(
      "🔹 ⚠️  Transactions MPF root marker will be preserved for recovery.",
    );
    const diagnostics = yield* transactionsMpf
      .diagnostics()
      .pipe(Effect.catchAll(() => Effect.succeed({ entries: -1 })));
    yield* Effect.logInfo(
      `🔹 Transactions MPF diagnostics: entries=${diagnostics.entries}`,
    );
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
      submitRefusal,
    };
  });
