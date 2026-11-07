import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Metric } from "effect";

import {
  DepositsDB,
  MempoolLedgerDB,
  MutationJobsDB,
  WithdrawalsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { type Database } from "../../services/index.js";

export const BATCH_SIZE = 100;

export const SKIPPED_SUBMISSION_TRANSFER_RETRIES = 2;

export const SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF = "250 millis";

export const daPayloadBuildDurationTimer = Metric.timer(
  "da_payload_build_duration_ms",
  "Duration of full-state DA materialization and payload encoding before local-finalization SQL",
);

export const localBlockFinalizationTransactionDurationTimer = Metric.timer(
  "local_block_finalization_transaction_duration_ms",
  "Duration of the local-finalization SQL transaction after DA payload bytes are complete",
);

export const uniqueBuffersByHex = (
  buffers: readonly Buffer[],
): readonly Buffer[] => [
  ...new Map(
    buffers.map((buffer) => [buffer.toString("hex"), buffer] as const),
  ).values(),
];

const LOCAL_FINALIZATION_FAILURE_MAX_CAUSES = 8;

const LOCAL_FINALIZATION_FAILURE_MAX_CHARS = 4000;

/**
 * A finalization failure with its cause chain (for a DatabaseError, the
 * underlying reason, e.g. which outref a ledger delta spends that its base
 * lacks). Bounded in depth and length, and cycle-safe; errors carry only
 * messages here, never connection parameters.
 */
export const describeLocalFinalizationFailure = (error: unknown): string => {
  const parts: string[] = [];
  const seen = new Set<unknown>();
  let current: unknown = error;
  while (
    parts.length < LOCAL_FINALIZATION_FAILURE_MAX_CAUSES &&
    current !== undefined &&
    current !== null &&
    !seen.has(current)
  ) {
    seen.add(current);
    parts.push(formatUnknownError(current));
    current =
      typeof current === "object"
        ? (current as { readonly cause?: unknown }).cause
        : undefined;
  }
  return parts.join("; cause=").slice(0, LOCAL_FINALIZATION_FAILURE_MAX_CHARS);
};

export const withLocalBlockFinalizationJob = <A, E, R>(
  input: {
    readonly headerHash: string;
    readonly mempoolTxCount: number;
    readonly includedDepositCount: number;
    readonly includedForcedTransactionCount: number;
    readonly includedWithdrawalCount: number;
  },
  program: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> => {
  const jobId = MutationJobsDB.localBlockFinalizationJobId(input.headerHash);
  return Effect.gen(function* () {
    yield* MutationJobsDB.start({
      jobId,
      kind: MutationJobsDB.Kind.LocalBlockFinalization,
      payload: {
        headerHash: input.headerHash,
        mempoolTxCount: input.mempoolTxCount,
        includedDepositCount: input.includedDepositCount,
        includedForcedTransactionCount: input.includedForcedTransactionCount,
        includedWithdrawalCount: input.includedWithdrawalCount,
      },
    });
    const result = yield* program;
    yield* MutationJobsDB.markCompleted(jobId);
    return result;
  }).pipe(
    Effect.tapError((error) =>
      MutationJobsDB.markFailed(
        jobId,
        describeLocalFinalizationFailure(error),
      ).pipe(Effect.catchAll(() => Effect.void)),
    ),
    Effect.tapError((error) =>
      Effect.logError(
        `🔹 Local block finalization job failed (job=${jobId},error=${describeLocalFinalizationFailure(error)})`,
      ),
    ),
  );
};

export const applyFinalizedWithdrawalLedgerEffects = (
  includedWithdrawalEventIds: readonly Buffer[],
): Effect.Effect<readonly string[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (includedWithdrawalEventIds.length <= 0) {
      return [];
    }
    const uniqueEventIds = uniqueBuffersByHex(includedWithdrawalEventIds);
    const withdrawals = yield* WithdrawalsDB.retrieveByEventIds(uniqueEventIds);
    if (withdrawals.length !== uniqueEventIds.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Failed to apply finalized withdrawal ledger effects because at least one withdrawal row is missing",
          cause: `requested=${uniqueEventIds.length},found=${withdrawals.length}`,
        }),
      );
    }

    const validWithdrawals = withdrawals.filter(
      (entry) =>
        entry[WithdrawalsDB.Columns.VALIDITY] ===
        WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    if (validWithdrawals.length <= 0) {
      return [];
    }

    const consumedOutRefs = yield* Effect.forEach(
      validWithdrawals,
      WithdrawalsDB.toLedgerOutRef,
    );
    const consumedDepositEventIds =
      yield* MempoolLedgerDB.clearUTxOs(consumedOutRefs);
    yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
    return consumedOutRefs.map((outRef) => outRef.toString("hex"));
  });
