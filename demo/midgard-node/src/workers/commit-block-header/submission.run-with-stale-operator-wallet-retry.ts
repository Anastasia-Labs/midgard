import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

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
import { fetchAndInsertTxOrderUTxOsForCommitBarrier } from "../../fibers/fetch-and-insert-tx-order-utxos.js";
import { type UtxoPayloadEntry } from "../../mpf/index.js";
import {
  fetchOperatorWalletView,
  type OperatorWalletView,
} from "../../operator-wallet-view.js";
import { HistoryProducer } from "../../services/event-history-producer.js";
import {
  commitEventHorizon,
  type CommitHorizonLag,
} from "../../services/history-commit-window.js";
import {
  ContractDeploymentIdentity,
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../../services/index.js";
import {
  isUnknownOutputReferenceSubmitError,
  TxSubmitError,
} from "../../transactions/utils.js";
import {
  COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES,
  StaleOperatorWalletRetrySignal,
} from "./submission.assert-pre-submit-da-payload-size.js";

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

type CommitUserEventSourceRefreshers<E, R> = {
  readonly txOrder: (upperBound: Date) => Effect.Effect<Date, E, R>;
};

/**
 * Rechecks the final end time against the event horizon, min(journal
 * coverage, follower ingestion) (E-N1-2 item 3) capped by the horizon lag
 * (`horizonLag`), then refreshes tx orders through it. This is the check
 * that refuses a header end above the lagged cap before submission. Deposits and withdrawals are the follower-change driver's:
 * nothing here fetches them. Every runtime commit runs under a history
 * producer; the unowned model fixture (no producer) needs an ingestion but
 * plans its end time past it, as its source polling did before.
 */
export const refreshCommitUserEventSourcesThroughBlockEnd = <
  E = SDK.LucidError | DatabaseError,
  R =
    | ContractDeploymentIdentity
    | Database
    | Lucid
    | MidgardContracts
    | NodeConfig,
  LE = never,
  LR = never,
>(
  blockEndTimeMs: number,
  horizonLag: CommitHorizonLag<LE, LR>,
  refreshers: CommitUserEventSourceRefreshers<E, R> = {
    txOrder: fetchAndInsertTxOrderUTxOsForCommitBarrier,
  } as unknown as CommitUserEventSourceRefreshers<E, R>,
) =>
  Effect.gen(function* () {
    const finalBlockEndTime = new Date(blockEndTimeMs);
    const history = yield* Effect.serviceOption(HistoryProducer);
    const horizon = yield* commitEventHorizon(
      Option.isSome(history) ? history.value.coverage : undefined,
      horizonLag,
    );
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
    // The final header window may differ from the initial plan. Recheck the
    // same immutable owner coverage; polling cannot extend its authority.
    // preparePendingSubmission rechecks the generation and that this coverage
    // is still a journaled prefix (the exact cursor, the anchor, or a
    // canonical earlier block under a newer cursor revision) under the
    // authority row lock after this provider work, before journal writes.
    yield* refreshers.txOrder(finalBlockEndTime);
  });

export const assertCommitUserEventSourceCompleteness = ({
  blockEndTimeMs,
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
}: {
  readonly blockEndTimeMs: number;
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
  }).pipe(
    sqlErrorToDatabaseError(
      PendingBlockFinalizationsDB.tableName,
      "Failed to revalidate commit user-event source completeness",
    ),
  );

export const signalStaleOperatorWalletRetry = ({
  pendingHeaderHash,
  error,
  label,
}: {
  readonly pendingHeaderHash: Buffer;
  readonly error: TxSubmitError;
  readonly label: string;
}) =>
  Effect.gen(function* () {
    yield* Effect.logWarning(
      `🔹 ${label} hit a stale operator-wallet view before submission recovery; retrying with a refreshed wallet view: ${formatUnknownError(
        error,
      )}`,
    );
    yield* PendingBlockFinalizationsDB.discardUnsubmittedPendingSubmission(
      pendingHeaderHash,
    ).pipe(
      Effect.catchAll((cause) =>
        Effect.logWarning(
          `🔹 Failed to discard stale unsubmitted pending journal before retry (${pendingHeaderHash.toString(
            "hex",
          )}): ${formatUnknownError(cause)}`,
        ),
      ),
    );
    return yield* Effect.fail(
      new StaleOperatorWalletRetrySignal({
        pendingHeaderHash,
        txSubmitError: error,
      }),
    );
  });

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
    isUnknownOutputReferenceSubmitError(detail) &&
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

export const runWithStaleOperatorWalletRetry = <A, E, R>({
  label,
  attempt,
}: {
  readonly label: string;
  readonly attempt: (
    initialOperatorWalletView?: OperatorWalletView,
    previousPendingHeaderHash?: Buffer,
  ) => Effect.Effect<A, E | StaleOperatorWalletRetrySignal, R>;
}): Effect.Effect<
  A,
  E | SDK.StateQueueError | DatabaseError | TxSubmitError,
  R | Lucid | Database
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    let previousPendingHeaderHash: Buffer | undefined;
    let lastResult = yield* Effect.either(
      attempt(undefined, previousPendingHeaderHash),
    );
    let retryCount = 0;

    while (
      lastResult._tag === "Left" &&
      lastResult.left instanceof StaleOperatorWalletRetrySignal &&
      retryCount < COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES
    ) {
      const stalePendingHeaderHash = Buffer.from(
        lastResult.left.pendingHeaderHash,
      );
      previousPendingHeaderHash = stalePendingHeaderHash;
      retryCount += 1;
      const refreshed = yield* Effect.either(
        Effect.gen(function* () {
          yield* lucid.switchToOperatorsMainWallet;
          const reloadedOperatorWalletView = yield* Effect.tryPromise({
            try: () => fetchOperatorWalletView(lucid.api),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to reload operator wallet view after stale commit submission",
                cause,
              }),
          });
          yield* Effect.logWarning(
            `${label} hit a stale operator-wallet input class error; reloading wallet view and rebuilding (attempt=${retryCount}/${COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES}).`,
          );
          return reloadedOperatorWalletView;
        }),
      );
      if (refreshed._tag === "Left") {
        yield* PendingBlockFinalizationsDB.markAbandoned(
          stalePendingHeaderHash,
        ).pipe(Effect.catchAll(() => Effect.void));
        return yield* Effect.fail(refreshed.left);
      }
      lastResult = yield* Effect.either(
        attempt(refreshed.right, stalePendingHeaderHash),
      );
    }

    if (lastResult._tag === "Left") {
      if (lastResult.left instanceof StaleOperatorWalletRetrySignal) {
        // The signal already discarded an intent-free journal, so this
        // matches nothing then, and markAbandoned never touches a journal
        // with a signed intent; either refusal must not replace the submit
        // error the commit reports.
        yield* PendingBlockFinalizationsDB.markAbandoned(
          lastResult.left.pendingHeaderHash,
        ).pipe(Effect.catchAll(() => Effect.void));
        return yield* Effect.fail(lastResult.left.txSubmitError);
      }
      if (previousPendingHeaderHash !== undefined) {
        yield* PendingBlockFinalizationsDB.markAbandoned(
          previousPendingHeaderHash,
        ).pipe(Effect.catchAll(() => Effect.void));
      }
      return yield* Effect.fail(lastResult.left);
    }
    return lastResult.right;
  });
