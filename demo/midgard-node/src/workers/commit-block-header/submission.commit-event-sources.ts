import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { isSpentInputSubmitRejection } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { assertIncludedEventsDeep } from "../../database/commit-event-depth.js";
import {
  DepositsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import { type UtxoPayloadEntry } from "../../mpf/index.js";
import { HistoryProducer } from "../../services/event-history-producer.js";
import {
  commitEventHorizon,
  type CommitHorizonLag,
} from "../../services/history-commit-window.js";
import { Database } from "../../services/index.js";
import { type TxSubmitError } from "../../transactions/utils.js";

/** True when both lists hold the same ids, each exactly once. */
const sameSourceIdSet = (
  actualIds: readonly string[],
  expectedIds: readonly string[],
): boolean => {
  if (actualIds.length !== expectedIds.length) return false;
  const expected = new Set(expectedIds);
  return (
    expected.size === expectedIds.length &&
    actualIds.every((id) => expected.has(id))
  );
};

export const commitUserEventSourceIdSetsAreExact = ({
  pendingDepositIds,
  includedDepositIds,
  pendingForcedTransactionIds,
  includedForcedTransactionIds,
  pendingWithdrawalIds,
  includedWithdrawalIds,
}: {
  readonly pendingDepositIds: readonly string[];
  readonly includedDepositIds: readonly string[];
  readonly pendingForcedTransactionIds: readonly string[];
  readonly includedForcedTransactionIds: readonly string[];
  readonly pendingWithdrawalIds: readonly string[];
  readonly includedWithdrawalIds: readonly string[];
}): boolean =>
  sameSourceIdSet(pendingDepositIds, includedDepositIds) &&
  sameSourceIdSet(pendingForcedTransactionIds, includedForcedTransactionIds) &&
  sameSourceIdSet(pendingWithdrawalIds, includedWithdrawalIds);

/**
 * Rechecks the final end time against the event horizon, min(journal
 * coverage, follower ingestion, the earliest forced order not yet rebuilt)
 * (E-N1-2 item 3, N10) capped by the horizon lag (`horizonLag`). This is the
 * check that refuses a header end above the lagged cap before submission.
 * Deposits, withdrawals and forced orders are the follower-change driver's:
 * nothing here fetches them. Every runtime commit runs under a history
 * producer; the unowned model fixture (no producer) needs an ingestion but
 * plans its end time past it, as its source polling did before.
 */
export const refreshCommitUserEventSourcesThroughBlockEnd = <
  LE = never,
  LR = never,
>(
  blockEndTimeMs: number,
  horizonLag: CommitHorizonLag<LE, LR>,
) =>
  Effect.gen(function* () {
    const history = yield* Effect.serviceOption(HistoryProducer);
    const horizon = yield* commitEventHorizon(
      Option.isSome(history) ? history.value.coverage : undefined,
      horizonLag,
    );
    // The final header window may differ from the initial plan. Recheck the
    // same immutable owner coverage; polling cannot extend its authority.
    // preparePendingSubmission rechecks the generation and that this coverage
    // is still a journaled prefix (the exact cursor, the anchor, or a
    // canonical earlier block under a newer cursor revision) under the
    // authority row lock, before journal writes.
    if (
      horizon === null ||
      (Option.isSome(history) && blockEndTimeMs > horizon)
    )
      return yield* Effect.fail(
        new DatabaseError({
          table: "follower_event_ingestion",
          message:
            "Final commitment end time exceeds the ingested event horizon",
          cause: `end=${blockEndTimeMs},horizon=${String(horizon)}`,
        }),
      );
  });

/**
 * Inside the journal transaction: the commit's included events are exactly
 * the due set through its end time, and each is at least `lagBlocks` (d)
 * deep below the follower's view (plan §8.1, `assertIncludedEventsDeep`).
 */
export const assertCommitUserEventSourceCompleteness = ({
  blockEndTimeMs,
  lagBlocks,
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
}: {
  readonly blockEndTimeMs: number;
  readonly lagBlocks: number;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const effectiveEndTime = new Date(blockEndTimeMs);

    // This effect runs inside the pending-journal transaction. Lock all three
    // source tables so the exact due set cannot change between this check and
    // assigning the journal projection.
    yield* sql`LOCK TABLE ${sql(DepositsDB.tableName)} IN SHARE MODE`;
    yield* sql`LOCK TABLE ${sql(ForcedTransactionsDB.tableName)} IN SHARE MODE`;
    yield* sql`LOCK TABLE ${sql(WithdrawalsDB.tableName)} IN SHARE MODE`;
    const [pendingDeposits, pendingForcedTransactions, pendingWithdrawals] =
      yield* Effect.all(
        [
          DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
          ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          ),
          WithdrawalsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
        ],
        { concurrency: 1 },
      );

    if (
      !commitUserEventSourceIdSetsAreExact({
        pendingDepositIds: pendingDeposits.map((entry) =>
          entry[DepositsDB.Columns.ID].toString("hex"),
        ),
        includedDepositIds: includedDepositEntries.map((entry) =>
          entry[DepositsDB.Columns.ID].toString("hex"),
        ),
        pendingForcedTransactionIds: pendingForcedTransactions.map((entry) =>
          entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        ),
        includedForcedTransactionIds: includedForcedTransactionEntries.map(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        ),
        pendingWithdrawalIds: pendingWithdrawals.map((entry) =>
          entry[WithdrawalsDB.Columns.ID].toString("hex"),
        ),
        includedWithdrawalIds: includedWithdrawalEntries.map((entry) =>
          entry[WithdrawalsDB.Columns.ID].toString("hex"),
        ),
      })
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Commit user-event source set changed before journal preparation",
          cause: `block_end_time_ms=${blockEndTimeMs.toString()}`,
        }),
      );
    }
    yield* assertIncludedEventsDeep({
      lagBlocks,
      depositIds: includedDepositEntries.map((entry) =>
        Buffer.from(entry[DepositsDB.Columns.ID]),
      ),
      forcedIds: includedForcedTransactionEntries.map((entry) =>
        Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]),
      ),
      withdrawalIds: includedWithdrawalEntries.map((entry) =>
        Buffer.from(entry[WithdrawalsDB.Columns.ID]),
      ),
    });
  }).pipe(
    sqlErrorToDatabaseError(
      PendingBlockFinalizationsDB.tableName,
      "Failed to revalidate commit user-event source completeness",
    ),
  );

export const journalUtxoEntries = (
  entries: readonly UtxoPayloadEntry[],
): readonly PendingBlockFinalizationsDB.UtxoInput[] =>
  entries.map((entry) => ({
    [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: entry.outref,
    [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: entry.output,
  }));

export const submitErrorReferencesOutRef = (
  error: TxSubmitError,
  outRef: string,
): boolean => {
  const [txHash, outputIndex] = outRef.split("#");
  if (txHash === undefined || outputIndex === undefined) {
    return false;
  }
  const detail = formatUnknownError(error, { includeCause: true }).replace(
    /\\"/g,
    '"',
  );
  return (
    isSpentInputSubmitRejection(detail) &&
    detail.includes(txHash) &&
    (detail.includes(`"index":${outputIndex}`) ||
      detail.includes(`"index": ${outputIndex}`) ||
      detail.includes(`#${outputIndex}`))
  );
};

export const isStaleCommitBaseError = (error: unknown): boolean =>
  error instanceof SDK.StateQueueError &&
  formatUnknownError(error, { includeCause: true }).includes(
    "Commit base is stale",
  );
