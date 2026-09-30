import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../database/utils/tx.js";
import { sameSpeculativeSourceIdSet } from "../fibers/speculative-commit-state.js";
import {
  type CommitStageLedgerRevert,
  revertCommitStageRejectedLedgerEffects,
} from "../mpf/index.js";
import {
  type ContractDeploymentIdentityValue,
  Database,
} from "../services/index.js";
import {
  resolveDepositsRoot,
  resolveForcedTransactionsRoot,
  resolveWithdrawalsRoot,
} from "./commit-block-header/event-roots.js";

export const revalidateAndPersistSpeculativeCandidateSources = ({
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
  selectedMempoolTxs,
  rejectedMempoolTxs,
  mempoolTxSourceTable,
  rejectionEntries,
  ledgerRevert,
  expectedEventRoots,
  candidateEndTime,
  excludedUserEventIds,
  stateQueueLeaseToken,
  activeMpfLeaseOwner,
  consensusProfile,
}: {
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly selectedMempoolTxs: readonly EntryWithTimeStamp[];
  readonly rejectedMempoolTxs: readonly EntryWithTimeStamp[];
  readonly mempoolTxSourceTable: string;
  readonly rejectionEntries: readonly TxRejectionsDB.EntryNoTimestamp[];
  readonly ledgerRevert: CommitStageLedgerRevert;
  readonly expectedEventRoots: {
    readonly deposits: string;
    readonly forcedTransactions: string;
    readonly withdrawals: string;
  };
  readonly candidateEndTime: Date;
  readonly excludedUserEventIds: {
    readonly depositEventIds: ReadonlySet<string>;
    readonly forcedTransactionEventIds: ReadonlySet<string>;
    readonly withdrawalEventIds: ReadonlySet<string>;
  };
  readonly stateQueueLeaseToken: string;
  readonly activeMpfLeaseOwner: string;
  readonly consensusProfile?: ContractDeploymentIdentityValue["consensusProfile"];
}): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const failSnapshot = (table: string, cause: string) =>
      Effect.fail(
        new DatabaseError({
          table,
          message:
            "Speculative candidate source snapshot changed before journal preparation",
          cause,
        }),
      );
    const sameBuffer = (left: Buffer, right: Buffer): boolean =>
      left.equals(right);
    const headerIsUnassigned = (headerHash: Buffer | null): boolean =>
      headerHash === null;

    yield* StateQueueMutationLeasesDB.revalidate(stateQueueLeaseToken);
    yield* MpfEngineStateDB.revalidateLedgerStoreLease(activeMpfLeaseOwner);

    // The speculative count check before the submit tail is only an early
    // rejection. Lock all three event-source tables and repeat an exact ID-set
    // comparison inside the pending-journal transaction so a late ingestion
    // cannot preserve the total count while replacing a candidate member, or
    // arrive between the count check and journal preparation.
    yield* sql`LOCK TABLE ${sql(DepositsDB.tableName)} IN SHARE MODE`;
    yield* sql`LOCK TABLE ${sql(ForcedTransactionsDB.tableName)} IN SHARE MODE`;
    yield* sql`LOCK TABLE ${sql(WithdrawalsDB.tableName)} IN SHARE MODE`;
    const [pendingDeposits, pendingForcedTransactions, pendingWithdrawals] =
      yield* Effect.all(
        [
          DepositsDB.retrievePendingHeaderEntriesUpTo(candidateEndTime),
          ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            candidateEndTime,
          ),
          WithdrawalsDB.retrievePendingHeaderEntriesUpTo(candidateEndTime),
        ],
        { concurrency: 1 },
      );
    const actualDepositIds = pendingDeposits
      .map((entry) => entry[DepositsDB.Columns.ID].toString("hex"))
      .filter((id) => !excludedUserEventIds.depositEventIds.has(id));
    const actualForcedTransactionIds = pendingForcedTransactions
      .map((entry) =>
        entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
      )
      .filter((id) => !excludedUserEventIds.forcedTransactionEventIds.has(id));
    const actualWithdrawalIds = pendingWithdrawals
      .map((entry) => entry[WithdrawalsDB.Columns.ID].toString("hex"))
      .filter((id) => !excludedUserEventIds.withdrawalEventIds.has(id));
    if (
      !sameSpeculativeSourceIdSet(
        actualDepositIds,
        includedDepositEntries.map((entry) =>
          entry[DepositsDB.Columns.ID].toString("hex"),
        ),
      ) ||
      !sameSpeculativeSourceIdSet(
        actualForcedTransactionIds,
        includedForcedTransactionEntries.map((entry) =>
          entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        ),
      ) ||
      !sameSpeculativeSourceIdSet(
        actualWithdrawalIds,
        includedWithdrawalEntries.map((entry) =>
          entry[WithdrawalsDB.Columns.ID].toString("hex"),
        ),
      )
    ) {
      return yield* failSnapshot(
        PendingBlockFinalizationsDB.tableName,
        "pending user-event ID set changed before journal preparation",
      );
    }

    const depositIds = includedDepositEntries.map(
      (entry) => entry[DepositsDB.Columns.ID],
    );
    const currentDeposits =
      depositIds.length === 0
        ? []
        : yield* sql<DepositsDB.Entry>`SELECT *
            FROM ${sql(DepositsDB.tableName)}
            WHERE ${sql(DepositsDB.Columns.ID)} IN ${sql.in(depositIds)}
            FOR UPDATE`;
    if (currentDeposits.length !== includedDepositEntries.length) {
      return yield* failSnapshot(
        DepositsDB.tableName,
        `deposit_count expected=${includedDepositEntries.length.toString()},actual=${currentDeposits.length.toString()}`,
      );
    }
    const currentDepositById = new Map(
      currentDeposits.map((entry) => [
        entry[DepositsDB.Columns.ID].toString("hex"),
        entry,
      ]),
    );
    for (const expected of includedDepositEntries) {
      const id = expected[DepositsDB.Columns.ID].toString("hex");
      const current = currentDepositById.get(id);
      if (
        current === undefined ||
        !sameBuffer(
          current[DepositsDB.Columns.INFO],
          expected[DepositsDB.Columns.INFO],
        ) ||
        current[DepositsDB.Columns.INCLUSION_TIME].getTime() !==
          expected[DepositsDB.Columns.INCLUSION_TIME].getTime() ||
        !sameBuffer(
          current[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
          expected[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
        ) ||
        !sameBuffer(
          current[DepositsDB.Columns.LEDGER_TX_ID],
          expected[DepositsDB.Columns.LEDGER_TX_ID],
        ) ||
        !sameBuffer(
          current[DepositsDB.Columns.LEDGER_OUTPUT],
          expected[DepositsDB.Columns.LEDGER_OUTPUT],
        ) ||
        current[DepositsDB.Columns.LEDGER_ADDRESS] !==
          expected[DepositsDB.Columns.LEDGER_ADDRESS] ||
        !headerIsUnassigned(current[DepositsDB.Columns.PROJECTED_HEADER_HASH])
      ) {
        return yield* failSnapshot(DepositsDB.tableName, `event_id=${id}`);
      }
    }

    const forcedIds = includedForcedTransactionEntries.map(
      (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
    );
    const currentForcedTransactions =
      forcedIds.length === 0
        ? []
        : yield* sql<ForcedTransactionsDB.Entry>`SELECT *
            FROM ${sql(ForcedTransactionsDB.tableName)}
            WHERE ${sql(ForcedTransactionsDB.Columns.TX_ORDER_ID)} IN ${sql.in(forcedIds)}
            FOR UPDATE`;
    if (
      currentForcedTransactions.length !==
      includedForcedTransactionEntries.length
    ) {
      return yield* failSnapshot(
        ForcedTransactionsDB.tableName,
        `forced_count expected=${includedForcedTransactionEntries.length.toString()},actual=${currentForcedTransactions.length.toString()}`,
      );
    }
    const currentForcedById = new Map(
      currentForcedTransactions.map((entry) => [
        entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        entry,
      ]),
    );
    for (const expected of includedForcedTransactionEntries) {
      const id =
        expected[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex");
      const current = currentForcedById.get(id);
      if (
        current === undefined ||
        !sameBuffer(
          current[ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH],
          expected[ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH],
        ) ||
        current[ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX] !==
          expected[ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX] ||
        !sameBuffer(
          current[ForcedTransactionsDB.Columns.ASSET_NAME],
          expected[ForcedTransactionsDB.Columns.ASSET_NAME],
        ) ||
        !sameBuffer(
          current[ForcedTransactionsDB.Columns.RAW_DATUM],
          expected[ForcedTransactionsDB.Columns.RAW_DATUM],
        ) ||
        !sameBuffer(
          current[ForcedTransactionsDB.Columns.TX_ID],
          expected[ForcedTransactionsDB.Columns.TX_ID],
        ) ||
        !sameBuffer(
          current[ForcedTransactionsDB.Columns.TX_COMPACT],
          expected[ForcedTransactionsDB.Columns.TX_COMPACT],
        ) ||
        !sameBuffer(
          current[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
          expected[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
        ) ||
        current[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() !==
          expected[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() ||
        !headerIsUnassigned(
          current[ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH],
        )
      ) {
        return yield* failSnapshot(
          ForcedTransactionsDB.tableName,
          `tx_order_id=${id}`,
        );
      }
    }

    const withdrawalIds = includedWithdrawalEntries.map(
      (entry) => entry[WithdrawalsDB.Columns.ID],
    );
    const currentWithdrawals =
      withdrawalIds.length === 0
        ? []
        : yield* sql<WithdrawalsDB.Entry>`SELECT *
            FROM ${sql(WithdrawalsDB.tableName)}
            WHERE ${sql(WithdrawalsDB.Columns.ID)} IN ${sql.in(withdrawalIds)}
            FOR UPDATE`;
    if (currentWithdrawals.length !== includedWithdrawalEntries.length) {
      return yield* failSnapshot(
        WithdrawalsDB.tableName,
        `withdrawal_count expected=${includedWithdrawalEntries.length.toString()},actual=${currentWithdrawals.length.toString()}`,
      );
    }
    const currentWithdrawalById = new Map(
      currentWithdrawals.map((entry) => [
        entry[WithdrawalsDB.Columns.ID].toString("hex"),
        entry,
      ]),
    );
    for (const expected of includedWithdrawalEntries) {
      const id = expected[WithdrawalsDB.Columns.ID].toString("hex");
      const current = currentWithdrawalById.get(id);
      if (current === undefined) {
        return yield* failSnapshot(WithdrawalsDB.tableName, `event_id=${id}`);
      }
      const currentSettlement =
        current[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
      const expectedSettlement =
        expected[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
      const currentValidity = current[WithdrawalsDB.Columns.VALIDITY];
      const expectedValidity = expected[WithdrawalsDB.Columns.VALIDITY];
      if (
        !sameBuffer(
          current[WithdrawalsDB.Columns.RAW_EVENT_INFO],
          expected[WithdrawalsDB.Columns.RAW_EVENT_INFO],
        ) ||
        current[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() !==
          expected[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH],
          expected[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH],
        ) ||
        current[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX] !==
          expected[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX] ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.ASSET_NAME],
          expected[WithdrawalsDB.Columns.ASSET_NAME],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.L2_OUTREF],
          expected[WithdrawalsDB.Columns.L2_OUTREF],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.L2_OWNER],
          expected[WithdrawalsDB.Columns.L2_OWNER],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.L2_VALUE],
          expected[WithdrawalsDB.Columns.L2_VALUE],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.L1_ADDRESS],
          expected[WithdrawalsDB.Columns.L1_ADDRESS],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.L1_DATUM],
          expected[WithdrawalsDB.Columns.L1_DATUM],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.REFUND_ADDRESS],
          expected[WithdrawalsDB.Columns.REFUND_ADDRESS],
        ) ||
        !sameBuffer(
          current[WithdrawalsDB.Columns.REFUND_DATUM],
          expected[WithdrawalsDB.Columns.REFUND_DATUM],
        ) ||
        (currentSettlement !== null &&
          (expectedSettlement === null ||
            !sameBuffer(currentSettlement, expectedSettlement))) ||
        (currentValidity !== null && currentValidity !== expectedValidity) ||
        !headerIsUnassigned(
          current[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH],
        )
      ) {
        return yield* failSnapshot(WithdrawalsDB.tableName, `event_id=${id}`);
      }
    }

    const candidateTxs = [...selectedMempoolTxs, ...rejectedMempoolTxs];
    const rejectedEntryIds = new Set(
      rejectionEntries.map((entry) =>
        entry[TxRejectionsDB.Columns.TX_ID].toString("hex"),
      ),
    );
    if (
      rejectionEntries.length !== rejectedMempoolTxs.length ||
      rejectedMempoolTxs.some(
        (entry) =>
          !rejectedEntryIds.has(entry[TxColumns.TX_ID].toString("hex")),
      )
    ) {
      return yield* failSnapshot(
        TxRejectionsDB.tableName,
        "rejected transaction snapshot does not match rejection entries",
      );
    }
    if (candidateTxs.length > 0) {
      if (
        mempoolTxSourceTable !== MempoolDB.tableName &&
        mempoolTxSourceTable !== ProcessedMempoolDB.tableName
      ) {
        return yield* failSnapshot(
          mempoolTxSourceTable,
          "candidate tx source table is not a durable mempool table",
        );
      }
      const txIds = candidateTxs.map((entry) => entry[TxColumns.TX_ID]);
      const uniqueTxIds = new Set(txIds.map((txId) => txId.toString("hex")));
      if (uniqueTxIds.size !== txIds.length) {
        return yield* failSnapshot(
          mempoolTxSourceTable,
          "candidate contains duplicate selected/rejected transaction ids",
        );
      }
      const currentTxs = yield* sql<{
        readonly tx_id: Buffer;
        readonly tx: Buffer | null;
        readonly time_stamp_tz: Date;
      }>`SELECT ${sql(TxColumns.TX_ID)}, ${sql(TxColumns.TX)}, ${sql(
        TxColumns.TIMESTAMPTZ,
      )}
          FROM ${sql(mempoolTxSourceTable)}
          WHERE ${sql(TxColumns.TX_ID)} IN ${sql.in(txIds)}
          FOR UPDATE`;
      if (currentTxs.length !== candidateTxs.length) {
        return yield* failSnapshot(
          mempoolTxSourceTable,
          `tx_count expected=${candidateTxs.length.toString()},actual=${currentTxs.length.toString()}`,
        );
      }
      const currentTxById = new Map(
        currentTxs.map((entry) => [entry.tx_id.toString("hex"), entry]),
      );
      const payloadRequiredIds = currentTxs
        .filter((entry) => entry.tx === null)
        .map((entry) => entry.tx_id);
      const acceptedPayloads =
        payloadRequiredIds.length === 0
          ? []
          : yield* sql<{
              readonly tx_id: Buffer;
              readonly tx_canonical_cbor: Buffer;
            }>`SELECT ${sql(TxAdmissionsDB.Columns.TX_ID)}, ${sql(
              TxAdmissionsDB.Columns.TX_CANONICAL_CBOR,
            )}
                FROM ${sql(TxAdmissionsDB.payloadTableName)}
                WHERE ${sql(TxAdmissionsDB.Columns.TX_ID)} IN ${sql.in(
                  payloadRequiredIds,
                )}
                FOR SHARE`;
      if (acceptedPayloads.length !== payloadRequiredIds.length) {
        return yield* failSnapshot(
          TxAdmissionsDB.payloadTableName,
          `accepted_payload_count expected=${payloadRequiredIds.length.toString()},actual=${acceptedPayloads.length.toString()}`,
        );
      }
      const acceptedPayloadById = new Map(
        acceptedPayloads.map((entry) => [
          entry.tx_id.toString("hex"),
          entry.tx_canonical_cbor,
        ]),
      );
      for (const expected of candidateTxs) {
        const id = expected[TxColumns.TX_ID].toString("hex");
        const current = currentTxById.get(id);
        const currentCbor = current?.tx ?? acceptedPayloadById.get(id) ?? null;
        if (
          current === undefined ||
          currentCbor === null ||
          !sameBuffer(currentCbor, expected[TxColumns.TX]) ||
          current.time_stamp_tz.getTime() !==
            expected[TxColumns.TIMESTAMPTZ].getTime()
        ) {
          return yield* failSnapshot(mempoolTxSourceTable, `tx_id=${id}`);
        }
      }
    }

    const [depositsRoot, forcedTransactionsRoot, withdrawalsRoot] =
      yield* Effect.all(
        [
          resolveDepositsRoot(includedDepositEntries),
          resolveForcedTransactionsRoot(
            includedForcedTransactionEntries,
            consensusProfile,
          ),
          resolveWithdrawalsRoot(includedWithdrawalEntries),
        ],
        { concurrency: "unbounded" },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: PendingBlockFinalizationsDB.tableName,
              message: "Failed to revalidate speculative candidate event roots",
              cause,
            }),
        ),
      );
    const actualEventRoots = {
      deposits: Option.getOrElse(
        depositsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      ),
      forcedTransactions: Option.getOrElse(
        forcedTransactionsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      ),
      withdrawals: Option.getOrElse(
        withdrawalsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      ),
    };
    if (
      actualEventRoots.deposits !== expectedEventRoots.deposits ||
      actualEventRoots.forcedTransactions !==
        expectedEventRoots.forcedTransactions ||
      actualEventRoots.withdrawals !== expectedEventRoots.withdrawals
    ) {
      return yield* failSnapshot(
        PendingBlockFinalizationsDB.tableName,
        "candidate event roots changed before journal preparation",
      );
    }

    if (includedDepositEntries.length > 0) {
      const mempoolEntries = yield* Effect.forEach(
        includedDepositEntries,
        DepositsDB.toMempoolLedgerEntry,
      );
      yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
      yield* DepositsDB.markAwaitingAsProjected(
        includedDepositEntries.map((entry) => entry[DepositsDB.Columns.ID]),
      );
    }

    yield* ForcedTransactionsDB.markAwaitingAsProjected(
      includedForcedTransactionEntries.map(
        (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
      ),
    );

    const withdrawalAssignments = yield* Effect.forEach(
      includedWithdrawalEntries,
      (entry) => {
        const settlementEventInfo =
          entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
        const validity = entry[WithdrawalsDB.Columns.VALIDITY];
        if (settlementEventInfo === null || validity === null) {
          return Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Speculative candidate is missing a classified withdrawal settlement",
              cause: entry[WithdrawalsDB.Columns.ID].toString("hex"),
            }),
          );
        }
        return Effect.succeed({
          eventId: entry[WithdrawalsDB.Columns.ID],
          expectedClassificationRevision:
            entry[WithdrawalsDB.Columns.CLASSIFICATION_REVISION],
          settlementEventInfo,
          validity,
          validityDetail: entry[WithdrawalsDB.Columns.VALIDITY_DETAIL],
        });
      },
    );
    yield* WithdrawalsDB.setSettlementInfoForEventIds(withdrawalAssignments);
    yield* WithdrawalsDB.markAwaitingAsProjected(
      includedWithdrawalEntries.map((entry) => ({
        eventId: entry[WithdrawalsDB.Columns.ID],
        expectedClassificationRevision:
          entry[WithdrawalsDB.Columns.CLASSIFICATION_REVISION],
      })),
    );

    if (rejectedMempoolTxs.length > 0) {
      const rejectedTxIds = rejectedMempoolTxs.map(
        (entry) => entry[TxColumns.TX_ID],
      );
      const deleted = yield* sql<{ readonly tx_id: Buffer }>`DELETE FROM ${sql(
        mempoolTxSourceTable,
      )}
          WHERE ${sql(TxColumns.TX_ID)} IN ${sql.in(rejectedTxIds)}
          RETURNING ${sql(TxColumns.TX_ID)}`;
      if (deleted.length !== rejectedTxIds.length) {
        return yield* failSnapshot(
          mempoolTxSourceTable,
          `rejected_delete_count expected=${rejectedTxIds.length.toString()},actual=${deleted.length.toString()}`,
        );
      }
      if (mempoolTxSourceTable === MempoolDB.tableName) {
        yield* MempoolTxDeltasDB.clearTxs(rejectedTxIds);
      }
      yield* TxRejectionsDB.insertMany(rejectionEntries);
    }
    const mempoolLedgerReverted =
      yield* revertCommitStageRejectedLedgerEffects(ledgerRevert);
    yield* StateQueueMutationLeasesDB.revalidate(stateQueueLeaseToken);
    yield* MpfEngineStateDB.revalidateLedgerStoreLease(activeMpfLeaseOwner);
    return mempoolLedgerReverted;
  }).pipe(
    sqlErrorToDatabaseError(
      PendingBlockFinalizationsDB.tableName,
      "Failed to atomically revalidate and persist speculative candidate sources",
    ),
  );
