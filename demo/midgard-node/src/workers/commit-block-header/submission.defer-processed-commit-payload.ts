import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import { TxUtils as TxTable } from "../../database/index.js";
import type { WorkerOutput } from "../utils/commit-block-header.js";
import { skippedSubmissionProgram } from "../utils/commit-submission.js";

/**
 * Keeps a validated payload that has no commit base to build on: its txs move
 * to `ProcessedMempoolDB` for the next commit attempt.
 */
export const deferProcessedCommitPayloadUntilConfirmation = ({
  processedMempoolTxs,
  mempoolTxHashes,
  mempoolTxsCount,
  sizeOfProcessedTxs,
}: {
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxsCount: number;
  readonly sizeOfProcessedTxs: number;
}) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 No confirmed blocks available. Transferring to ProcessedMempoolDB...",
    );
    const transferResult = yield* Effect.either(
      skippedSubmissionProgram(processedMempoolTxs, mempoolTxHashes),
    );
    if (transferResult._tag === "Left") {
      const detail = formatUnknownError(transferResult.left);
      yield* Effect.logError(
        `🔹 Failed to defer processed txs while waiting for confirmation: ${detail}`,
      );
      return {
        type: "FailureOutput",
        error: `Failed to transfer deferred commit payload to ProcessedMempoolDB: ${detail}`,
      } satisfies WorkerOutput;
    }
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
    } satisfies WorkerOutput;
  });
