import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { isSpentInputSubmitRejection } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  type CommitAnchor,
  commitAnchorCanonical,
  commitAnchorCapMs,
  commitAnchorHeight,
} from "../../database/commit-anchor.js";
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
import { forcedOrderHorizon } from "../../forced-orders/horizon.js";
import { type UtxoPayloadEntry } from "../../mpf/index.js";
import { requireCandidateView } from "../../services/follower-write-gate.js";
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

export const COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE =
  "Refusing to journal a commit whose end time exceeds its commit anchor's cap";

export const COMMIT_END_ABOVE_FORCED_HORIZON_MESSAGE =
  "Refusing to journal a commit whose end time reaches a forced order not yet rebuilt";

export const COMMIT_ANCHOR_UNAVAILABLE_MESSAGE =
  "Refusing to journal a commit planned without a commit anchor";

export const COMMIT_ANCHOR_NOT_OF_VIEW_MESSAGE =
  "Refusing to journal a commit whose commit anchor is not the block d below its permit's view";

const refuse = (message: string, cause: string) =>
  Effect.fail(
    new DatabaseError({
      table: PendingBlockFinalizationsDB.tableName,
      message,
      cause,
    }),
  );

/**
 * The end-time recheck against the commit anchor (plan §8.1), inside the
 * gated journal transaction. The anchor was read at the permit's view when
 * the end time was planned (`commitEventHorizon`); the gate has just checked
 * that view is still on the follower's chain, so its ancestor at the anchor
 * height is still the anchor. The recheck confirms the anchor is that
 * ancestor (its height, and its block while the follower stores it), and
 * caps the end time at its time + event_wait - 1 and at the forced-order
 * bound. Returns the anchor the journal stores. A model fixture without a
 * permit stores the anchor it was given and is not capped.
 */
const recheckCommitAnchor = (input: {
  readonly blockEndTimeMs: number;
  readonly anchor: CommitAnchor | undefined;
  readonly depth: number;
  readonly slotToUnixTime: (slot: number) => number;
}) =>
  Effect.gen(function* () {
    const permit = yield* requireCandidateView;
    const { anchor } = input;
    if (Option.isNone(permit)) return anchor;
    if (anchor === undefined)
      return yield* refuse(
        COMMIT_ANCHOR_UNAVAILABLE_MESSAGE,
        "the commit was planned without a commit anchor",
      );
    const anchorAt = `anchor_height=${anchor.height.toString()},anchor_slot=${anchor.slot.toString()},view_height=${permit.value.view.height.toString()},d=${input.depth.toString()}`;
    const sql = yield* SqlClient.SqlClient;
    const [check] = yield* sql<{ canonical: boolean }>`
      SELECT ${commitAnchorCanonical(sql, "a")} AS canonical
      FROM (SELECT ${anchor.hash}::bytea AS commit_anchor_hash,
          ${anchor.height}::bigint AS commit_anchor_height,
          ${anchor.slot}::bigint AS commit_anchor_slot) a`;
    if (
      anchor.height !==
        commitAnchorHeight(permit.value.view.height, input.depth) ||
      check?.canonical !== true
    )
      return yield* refuse(COMMIT_ANCHOR_NOT_OF_VIEW_MESSAGE, anchorAt);
    const capMs = commitAnchorCapMs(input.slotToUnixTime(anchor.slot));
    if (!Number.isSafeInteger(capMs) || input.blockEndTimeMs > capMs)
      return yield* refuse(
        COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE,
        `end=${input.blockEndTimeMs.toString()},cap=${String(capMs)},${anchorAt}`,
      );
    const forced = yield* forcedOrderHorizon;
    if (forced !== null && input.blockEndTimeMs > forced)
      return yield* refuse(
        COMMIT_END_ABOVE_FORCED_HORIZON_MESSAGE,
        `end=${input.blockEndTimeMs.toString()},forced_horizon=${forced.toString()}`,
      );
    return anchor;
  });

/**
 * Inside the gated journal transaction: the commit's included events are
 * exactly the due set through its end time, and the end time is within its
 * commit anchor's cap (`recheckCommitAnchor`). Returns the anchor the journal
 * stores (`undefined` only for a model fixture planned without one).
 */
export const assertCommitUserEventSourceCompleteness = ({
  blockEndTimeMs,
  commitAnchor,
  depth,
  slotToUnixTime,
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
}: {
  readonly blockEndTimeMs: number;
  readonly commitAnchor: CommitAnchor | undefined;
  readonly depth: number;
  readonly slotToUnixTime: (slot: number) => number;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
}): Effect.Effect<CommitAnchor | undefined, DatabaseError, Database> =>
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
    return yield* recheckCommitAnchor({
      blockEndTimeMs,
      anchor: commitAnchor,
      depth,
      slotToUnixTime,
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
