/**
 * Production block-commit worker entrypoint.
 * This module orchestrates MPF root processing, commit transaction
 * assembly, submission, and recovery by composing the smaller worker helpers.
 */
import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Cause, Data, Effect, Option, pipe, Schedule } from "effect";
import { parentPort, workerData } from "worker_threads";

import {
  CommitBuildCalibrationDB,
  ConfirmedLedgerDB,
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
} from "@/database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "@/database/utils/tx.js";
import { reachPipelinedCommitCrashCheckpoint } from "@/e2e/pipelined-commit-crash-checkpoint.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import { fetchAndInsertTxOrderUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-tx-order-utxos.js";
import { fetchAndInsertWithdrawalUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-withdrawal-utxos.js";
import {
  minimumBarrierWatermarkMs,
  sameSpeculativeSourceIdSet,
} from "@/fibers/speculative-commit-state.js";
import {
  ConfigError,
  ContractDeploymentIdentity,
  type ContractDeploymentIdentityValue,
  Database,
  DatabaseInitializationError,
  Lucid,
  MidgardContracts,
  MidgardContractServices,
  NativeMpfWorkerPortClient,
  NodeConfig,
} from "@/services/index.js";
import { materializeConfirmedLedgerSnapshot } from "@/transactions/state-queue/confirmed-ledger-snapshot.js";
import {
  awaitExactTransactionConfirmation,
  type TxSignError,
  type TxSubmitError,
} from "@/transactions/utils.js";
import { outRefLabel } from "@/tx-context.js";
import { buildUnsignedCommitTx } from "@/workers/commit-block-header/build-unsigned-tx.js";
import {
  resolveDepositsRoot,
  resolveForcedTransactionsRoot,
  resolveWithdrawalsRoot,
} from "@/workers/commit-block-header/event-roots.js";
import {
  fetchLatestCommittedBlockLocal,
  getLatestBlockDatumEndTime,
} from "@/workers/commit-block-header/state-queue.js";
import {
  deferProcessedCommitPayloadUntilConfirmation,
  recoverLocalFinalizationAgainstConfirmedBlock,
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "@/workers/commit-block-header/submission.js";
import { makeEventCommitments } from "@/workers/commit-block-header/transition-commitments.js";
import { reconcileOverdueAwaitingEventsAgainstRetainedForeignTips } from "@/workers/t2-foreign-event-reconciliation.js";
import {
  deserializeStateQueueUTxO,
  type RegisteredDueWorkOutput,
  type SerializedStateQueueUTxO,
  type SpeculativeCandidateInvalidatedOutput,
  type SpeculativeCandidateReadyOutput,
  type SpeculativeCommitWorkerInstruction,
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import {
  calibratedCommitBuildMsPerTx,
  type CommitSchedulerStateQueueEvidence,
  type CurrentOperatorSchedulerWindow,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  type EarliestCommitSchedulerPlan,
  establishEndTimeFromTxRequests,
  planCommitBatchBudgets,
  planSchedulerAwareCommitSelection,
  selectCommitTxCandidates,
  shouldDeferCommitSubmission,
  shouldSkipIdleCommitBehindUnmergedTail,
  updateCommitBuildEwma,
} from "@/workers/utils/commit-block-planner.js";
import {
  COMMIT_MIN_PRE_WITNESS_BUDGET_MS,
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  resolveCommitEndTimeFit,
  resolveExplicitCommitCandidateEndTimeMs,
} from "@/workers/utils/commit-end-time.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  configureCommitMpfRuntime,
  hydrateLedgerMpfFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
  makeMpfs,
  MidgardMpf,
  MpfError,
  type NativeMpfBuildContext,
  type ParkedEventFlatOverlayV1,
  type ParkedMpfOverlayV1,
  processMpfs,
  utxoToLedgerInsertMaterialV1,
  withMpfBlockOverlays,
  withMpfRootTransactions,
} from "@/workers/utils/mpf.js";
import {
  resolveCurrentOperatorSchedulerWindow,
  resolveEarliestCommitSchedulerDueWorkPlan,
} from "@/workers/utils/scheduler-refresh.js";

const EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS = 120_000;
const EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const EXPLICIT_COMMIT_BLOCK_VISIBILITY_DELAY = "5 seconds";
const EXPLICIT_COMMIT_BLOCK_VISIBILITY_RETRIES = 18;

class CommitWorkerInvariantError extends Data.TaggedError(
  "CommitWorkerInvariantError",
)<{
  readonly message: string;
}> {}

export const provideCommitBlockWorkerServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
  >,
): Effect.Effect<A, E | ConfigError | DatabaseInitializationError, never> =>
  pipe(
    effect,
    Effect.provide(MidgardContractServices),
    Effect.provide(Database.workerLayer),
    Effect.provide(NodeConfig.layer),
  );

export type CommitLucidFactory = () => Effect.Effect<Lucid, ConfigError>;

export const defaultCommitLucidFactory: CommitLucidFactory = () =>
  Effect.provide(Lucid, Lucid.Default);

const pendingUserEventCountUpTo = (
  effectiveEndTime: Date,
  excluded?: {
    readonly depositEventIds: ReadonlySet<string>;
    readonly forcedTransactionEventIds: ReadonlySet<string>;
    readonly withdrawalEventIds: ReadonlySet<string>;
  },
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const [depositEntries, forcedTransactionEntries, withdrawalEntries] =
      yield* Effect.all(
        [
          DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
          ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          ),
          WithdrawalsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
        ],
        { concurrency: "unbounded" },
      );
    return (
      depositEntries.filter(
        (entry) =>
          !excluded?.depositEventIds.has(
            entry[DepositsDB.Columns.ID].toString("hex"),
          ),
      ).length +
      forcedTransactionEntries.filter(
        (entry) =>
          !excluded?.forcedTransactionEventIds.has(
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          ),
      ).length +
      withdrawalEntries.filter(
        (entry) =>
          !excluded?.withdrawalEventIds.has(
            entry[WithdrawalsDB.Columns.ID].toString("hex"),
          ),
      ).length
    );
  });

export const revalidateAndPersistSpeculativeCandidateSources = ({
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
  selectedMempoolTxs,
  rejectedMempoolTxs,
  mempoolTxSourceTable,
  rejectionEntries,
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
}): Effect.Effect<void, DatabaseError, Database> =>
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
        current[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY] !==
          expected[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY] ||
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
          settlementEventInfo,
          validity,
          validityDetail: entry[WithdrawalsDB.Columns.VALIDITY_DETAIL],
        });
      },
    );
    yield* WithdrawalsDB.setSettlementInfoForEventIds(withdrawalAssignments);
    yield* WithdrawalsDB.markAwaitingAsProjected(
      includedWithdrawalEntries.map((entry) => entry[WithdrawalsDB.Columns.ID]),
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
    yield* StateQueueMutationLeasesDB.revalidate(stateQueueLeaseToken);
    yield* MpfEngineStateDB.revalidateLedgerStoreLease(activeMpfLeaseOwner);
  }).pipe(
    sqlErrorToDatabaseError(
      PendingBlockFinalizationsDB.tableName,
      "Failed to atomically revalidate and persist speculative candidate sources",
    ),
  );

type ResolvedCommitBaseLedgerEntries = {
  readonly source: string;
  readonly entries?: readonly Ledger.MinimalEntry[];
  readonly root: string;
  readonly utxoPayloadAggregate: SDK.DaPayloadEntrySizeAggregate;
};

export const selectAuthenticatedForeignBaseCandidate = ({
  foreignUtxosRoot,
  requireEntries,
  candidates,
}: {
  readonly foreignUtxosRoot: string;
  readonly requireEntries: boolean;
  readonly candidates: readonly {
    readonly source: string;
    readonly root: string;
    readonly hasEntries: boolean;
  }[];
}):
  | { readonly type: "Ready"; readonly source: string }
  | { readonly type: "AwaitingForeignLedger"; readonly reason: string } => {
  const matching = candidates.find(
    (candidate) =>
      candidate.root === foreignUtxosRoot &&
      (!requireEntries || candidate.hasEntries),
  );
  return matching === undefined
    ? {
        type: "AwaitingForeignLedger",
        reason: candidates.some(
          (candidate) => candidate.root === foreignUtxosRoot,
        )
          ? "matching durable root has no authenticated entry snapshot"
          : "foreign UTxO root differs from every authenticated local base",
      }
    : { type: "Ready", source: matching.source };
};

const resolveCommitBaseLedgerEntries = ({
  availableConfirmedBlock,
  speculativeBase,
  ledgerMpf,
  nativeMpfRoot,
  requireEntries,
}: {
  readonly availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  readonly speculativeBase?: NonNullable<
    WorkerInput["data"]["speculativeBuild"]
  >["base"];
  readonly ledgerMpf?: MidgardMpf;
  readonly nativeMpfRoot?: string;
  readonly requireEntries: boolean;
}): Effect.Effect<
  ResolvedCommitBaseLedgerEntries,
  unknown,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    const currentLedgerRoot = (): Effect.Effect<string, unknown> =>
      ledgerMpf === undefined
        ? nativeMpfRoot === undefined
          ? Effect.fail(
              new Error("Commit base resolution has no ledger root provider"),
            )
          : Effect.succeed(nativeMpfRoot)
        : ledgerMpf.rootHex();
    if (speculativeBase !== undefined) {
      const currentLedgerRootHex = yield* currentLedgerRoot();
      const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(speculativeBase.headerHash, "hex"),
      );
      if (Option.isNone(journal)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Refusing to speculate without the submitted base journal",
            cause: `base_header_hash=${speculativeBase.headerHash}`,
          }),
        );
      }
      if (
        journal.value[
          PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
        ] !== speculativeBase.utxosRoot
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Speculative base root does not match its submitted journal",
            cause: `base_header_hash=${speculativeBase.headerHash},input_root=${speculativeBase.utxosRoot},journal_root=${journal.value[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]}`,
          }),
        );
      }
      if (
        !requireEntries &&
        currentLedgerRootHex === speculativeBase.utxosRoot &&
        journal.value.utxoPayloadAggregate !== undefined
      ) {
        return {
          source: `speculative-parent:${speculativeBase.headerHash}`,
          root: speculativeBase.utxosRoot,
          utxoPayloadAggregate: journal.value.utxoPayloadAggregate,
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
      const snapshot = yield* materializeConfirmedLedgerSnapshot(journal.value);
      if (snapshot.root !== speculativeBase.utxosRoot) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Submitted journal post-state cannot reproduce the speculative base root",
            cause: `base_header_hash=${speculativeBase.headerHash},expected_root=${speculativeBase.utxosRoot},journal_root=${snapshot.root}`,
          }),
        );
      }
      return {
        source: `speculative-journal:${speculativeBase.headerHash}`,
        entries: snapshot.entries,
        root: snapshot.root,
        utxoPayloadAggregate:
          journal.value.utxoPayloadAggregate ??
          ledgerPayloadAggregateFromEntries(snapshot.entries),
      } satisfies ResolvedCommitBaseLedgerEntries;
    }
    if (availableConfirmedBlock !== "") {
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      if (latestBlock.datum.key === "Empty") {
        yield* Effect.logInfo(
          "🔹 Commit base state_queue tip is the confirmed-state root; using confirmed_ledger as base.",
        );
        const confirmedEntriesForGenesis = yield* ConfirmedLedgerDB.retrieve;
        if (confirmedEntriesForGenesis.length === 0) {
          const nodeConfig = yield* NodeConfig;
          const genesisEntries = yield* Effect.forEach(
            nodeConfig.GENESIS_UTXOS,
            (utxo) =>
              utxoToLedgerInsertMaterialV1(utxo).pipe(
                Effect.map(({ ledgerOp, outputCbor }) => ({
                  [Ledger.Columns.OUTREF]: ledgerOp.key,
                  [Ledger.Columns.OUTPUT]: outputCbor,
                })),
              ),
          );
          if (genesisEntries.length > 0) {
            const root =
              yield* computeLedgerMpfRootFromLedgerEntries(genesisEntries);
            yield* Effect.logInfo(
              `🔹 Commit base ledger snapshot resolved from configured genesis UTxOs (entries=${genesisEntries.length.toString()}).`,
            );
            return {
              source: "genesis",
              entries: genesisEntries,
              root,
              utxoPayloadAggregate:
                ledgerPayloadAggregateFromEntries(genesisEntries),
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
        }
      } else {
        const header = yield* SDK.getHeaderV1FromStateQueueDatum(
          latestBlock.datum,
        );
        const currentLedgerRootHex = yield* currentLedgerRoot();
        const headerHash = yield* SDK.hashBlockHeaderV1(header);
        const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(headerHash, "hex"),
        );

        if (Option.isSome(journal)) {
          if (
            !requireEntries &&
            currentLedgerRootHex === header.utxosRoot &&
            journal.value.utxoPayloadAggregate !== undefined
          ) {
            return {
              source: "persistent-ledger-mpf",
              root: header.utxosRoot,
              utxoPayloadAggregate: journal.value.utxoPayloadAggregate,
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
          const snapshot = yield* materializeConfirmedLedgerSnapshot(
            journal.value,
          );
          if (snapshot.root !== header.utxosRoot) {
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Refusing to use pending-finalization journal as commit base because its UTxO snapshot root does not match the state-queue tip",
                cause: `header_hash=${headerHash},journal_root=${snapshot.root},state_queue_root=${header.utxosRoot}`,
              }),
            );
          }
          yield* Effect.logInfo(
            `🔹 Commit base ledger snapshot resolved from pending-finalization journal ${headerHash} (entries=${snapshot.entries.length.toString()}).`,
          );
          return {
            source: `pending-finalization:${headerHash}`,
            entries: snapshot.entries,
            root: snapshot.root,
            utxoPayloadAggregate:
              journal.value.utxoPayloadAggregate ??
              ledgerPayloadAggregateFromEntries(snapshot.entries),
          } satisfies ResolvedCommitBaseLedgerEntries;
        }
        if (!requireEntries && currentLedgerRootHex === header.utxosRoot) {
          const aggregate =
            yield* MpfEngineStateDB.retrieveLedgerPayloadAggregate(
              currentLedgerRootHex,
            );
          if (aggregate !== undefined) {
            return {
              source: "persistent-ledger-mpf",
              root: header.utxosRoot,
              utxoPayloadAggregate: aggregate,
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
        }

        const parentJournal =
          yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
            Buffer.from(header.prevHeaderHash, "hex"),
          );
        const parentSnapshot = Option.isSome(parentJournal)
          ? yield* materializeConfirmedLedgerSnapshot(parentJournal.value)
          : undefined;
        const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
        const confirmedRoot =
          yield* computeLedgerMpfRootFromLedgerEntries(confirmedEntries);
        const selection = selectAuthenticatedForeignBaseCandidate({
          foreignUtxosRoot: header.utxosRoot,
          requireEntries,
          candidates: [
            ...(parentSnapshot === undefined
              ? []
              : [
                  {
                    source: `foreign-parent-journal:${header.prevHeaderHash}`,
                    root: parentSnapshot.root,
                    hasEntries: true,
                  },
                ]),
            {
              source: "confirmed_ledger",
              root: confirmedRoot,
              hasEntries: true,
            },
            {
              source: "persistent-ledger-mpf",
              root: currentLedgerRootHex,
              hasEntries: false,
            },
          ],
        });
        if (selection.type === "Ready") {
          if (selection.source.startsWith("foreign-parent-journal:")) {
            return {
              source: selection.source,
              entries: parentSnapshot!.entries,
              root: parentSnapshot!.root,
              utxoPayloadAggregate:
                parentJournal._tag === "Some" &&
                parentJournal.value.utxoPayloadAggregate !== undefined
                  ? parentJournal.value.utxoPayloadAggregate
                  : ledgerPayloadAggregateFromEntries(parentSnapshot!.entries),
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
          if (selection.source === "confirmed_ledger") {
            return {
              source: selection.source,
              entries: confirmedEntries,
              root: confirmedRoot,
              utxoPayloadAggregate:
                ledgerPayloadAggregateFromEntries(confirmedEntries),
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
          const aggregate =
            yield* MpfEngineStateDB.retrieveLedgerPayloadAggregate(
              currentLedgerRootHex,
            );
          if (aggregate !== undefined) {
            return {
              source: selection.source,
              root: currentLedgerRootHex,
              utxoPayloadAggregate: aggregate,
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
        }
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Awaiting authenticated foreign ledger finalization before rebuilding",
            cause: `foreign_header_hash=${headerHash},foreign_utxos_root=${header.utxosRoot},current_mpf_root=${currentLedgerRootHex},parent_journal_root=${parentSnapshot?.root ?? "missing"},confirmed_ledger_root=${confirmedRoot},reason=${selection.type === "AwaitingForeignLedger" ? selection.reason : "missing persistent aggregate"}`,
          }),
        );
      }
    }

    if (!requireEntries) {
      const currentLedgerRootHex = yield* currentLedgerRoot();
      const aggregate =
        yield* MpfEngineStateDB.retrieveLedgerPayloadAggregate(
          currentLedgerRootHex,
        );
      if (aggregate !== undefined) {
        return {
          source: "persistent-ledger-mpf",
          root: currentLedgerRootHex,
          utxoPayloadAggregate: aggregate,
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
    }

    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    const confirmedRoot =
      yield* computeLedgerMpfRootFromLedgerEntries(confirmedEntries);
    yield* Effect.logInfo(
      `🔹 Commit base ledger snapshot resolved from confirmed_ledger (entries=${confirmedEntries.length.toString()}).`,
    );
    const utxoPayloadAggregate =
      ledgerPayloadAggregateFromEntries(confirmedEntries);
    yield* MpfEngineStateDB.stampLedgerPayloadAggregate({
      rootHex: confirmedRoot,
      aggregate: utxoPayloadAggregate,
    });
    return {
      source: "confirmed_ledger",
      entries: confirmedEntries,
      root: confirmedRoot,
      utxoPayloadAggregate,
    } satisfies ResolvedCommitBaseLedgerEntries;
  });

const alignCommitMpfsToBase = ({
  ledgerMpf,
  nativeMpfRoot,
  transactionsMpf,
  base,
}: {
  readonly ledgerMpf?: MidgardMpf;
  readonly nativeMpfRoot?: string;
  readonly transactionsMpf: MidgardMpf;
  readonly base: ResolvedCommitBaseLedgerEntries;
}): Effect.Effect<readonly Ledger.MinimalEntry[], unknown, never> =>
  Effect.gen(function* () {
    const currentLedgerRoot =
      ledgerMpf === undefined ? nativeMpfRoot : yield* ledgerMpf.rootHex();
    if (currentLedgerRoot === undefined) {
      return yield* Effect.fail(
        new Error("Commit base alignment has no ledger root provider"),
      );
    }
    if (currentLedgerRoot !== base.root) {
      if (ledgerMpf === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Architecture G durable marker differs from the selected commit base",
            cause: `source=${base.source},current_root=${currentLedgerRoot},expected_root=${base.root}`,
          }),
        );
      }
      if (base.entries === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Cannot hydrate a divergent commit ledger MPF without base entries",
            cause: `source=${base.source},current_root=${currentLedgerRoot},expected_root=${base.root}`,
          }),
        );
      }
      const hydratedRoot = yield* hydrateLedgerMpfFromLedgerEntries(
        ledgerMpf,
        base.entries,
      );
      if (hydratedRoot !== base.root) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Refusing to build a block because hydrating the commit ledger MPF did not reproduce the selected base root",
            cause: `source=${base.source},expected_root=${base.root},hydrated_root=${hydratedRoot}`,
          }),
        );
      }
      yield* Effect.logInfo(
        `🔹 Hydrated commit ledger MPF from ${base.source}: previous_root=${currentLedgerRoot},root=${hydratedRoot},entries=${base.entries.length.toString()}.`,
      );
    }

    const transactionsRootIsEmpty = yield* transactionsMpf.rootIsEmpty();
    if (!transactionsRootIsEmpty) {
      yield* transactionsMpf.resetToEmpty();
      yield* Effect.logInfo(
        `🔹 Reset per-block transactions MPF before building on ${base.source}.`,
      );
    }

    return base.entries ?? [];
  });

export const shouldPreserveCommitMpfRoots = (output: WorkerOutput): boolean => {
  switch (output.type) {
    case "SubmittedAwaitingConfirmationOutput":
    case "SubmittedAwaitingLocalFinalizationOutput":
    case "SuccessfulSubmissionOutput":
    case "SkippedSubmissionOutput":
      return true;
    case "SuccessfulLocalFinalizationRecoveryOutput":
      // Local finalization intentionally advances durable DB state and resets
      // the transactions MPF root; rolling back would undo recovery.
      return true;
    case "FailureOutput":
    case "AwaitingForeignDaOutput":
    case "RegisteredDueWorkOutput":
    case "NothingToCommitOutput":
    case "SpeculativeCandidateReadyOutput":
    case "SpeculativeCandidateInvalidatedOutput":
      return false;
  }
};

export const workerPreIngestionDueWorkOutputFromPlan = (
  plan: EarliestCommitSchedulerPlan,
): RegisteredDueWorkOutput | undefined =>
  plan.status === "register_due_work"
    ? {
        type: "RegisteredDueWorkOutput",
        dueWork: plan.dueWork,
      }
    : undefined;

export const shouldShortCircuitIdleCommitAttempt = ({
  candidateTxCount,
  processedPendingTxCount,
  pendingUserEventCount,
  localFinalizationPending,
}: {
  readonly candidateTxCount: number;
  readonly processedPendingTxCount: number;
  readonly pendingUserEventCount: number;
  readonly localFinalizationPending: boolean;
}): boolean =>
  candidateTxCount === 0 &&
  processedPendingTxCount === 0 &&
  pendingUserEventCount === 0 &&
  !localFinalizationPending;

export type ExplicitBlockHeaderCommitParams = {
  readonly utxosRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
  // For fault-proof drills that commit a non-empty transactions root, the
  // header must carry a matching l2_transaction_count (> 0) and, because
  // total_event_count > 0, non-empty transition roots. The transition roots are
  // not checked by the CommitBlockHeader validator, so callers supply arbitrary
  // non-empty values.
  readonly l2TransactionCount?: bigint;
  readonly transitionTraceRoot?: string;
  readonly eventToStepRoot?: string;
  readonly validationTracesRoot?: string;
  readonly validationTraceCount?: bigint;
  readonly endTimeMs?: number;
  readonly awaitConfirmation?: boolean;
};

export type ExplicitBlockHeaderCommitOutput = {
  readonly submittedTxHash: string;
  readonly headerHash: string;
  readonly blockOutRef: string | null;
  readonly txSize: number;
  readonly blockEndTimeMs: number;
  readonly roots: {
    readonly utxosRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
};

const waitForTxConfirmation = (
  lucid: LucidEvolution,
  txHash: string,
): Effect.Effect<void, SDK.LucidError> =>
  Effect.tryPromise({
    try: () =>
      awaitExactTransactionConfirmation(lucid, txHash, {
        timeout: EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS,
        checkInterval: EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS,
      }).then(() => undefined),
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to confirm explicit block-header commit transaction",
        cause,
      }),
  });

const fetchCommittedBlockOutRef = ({
  lucid,
  contracts,
  headerHash,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly headerHash: string;
}): Effect.Effect<string, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const unit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      );
      const utxos = await lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        unit,
      );
      if (utxos.length !== 1) {
        throw new Error(
          `expected exactly one committed state_queue block UTxO for ${headerHash}, found ${utxos.length}`,
        );
      }
      return `${utxos[0].txHash}#${utxos[0].outputIndex}`;
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to resolve committed state_queue block outref",
        cause,
      }),
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(EXPLICIT_COMMIT_BLOCK_VISIBILITY_DELAY),
        Schedule.recurs(EXPLICIT_COMMIT_BLOCK_VISIBILITY_RETRIES),
      ),
    ),
  );

/**
 * Explicit operator command helper for live fault-proof drills. The supplied
 * roots are committed through the same real state_queue, scheduler, and active
 * operator transaction builder used by the production block worker, but no
 * local database finalization is attempted.
 */
export const commitExplicitBlockHeaderProgram = (
  params: ExplicitBlockHeaderCommitParams,
): Effect.Effect<
  ExplicitBlockHeaderCommitOutput,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HeaderTransitionCommitmentsError
  | SDK.LucidError
  | SDK.HashingError
  | TxSignError
  | TxSubmitError,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const lucid = lucidService.api;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    };

    const latestBlock = yield* fetchLatestCommittedBlockLocal(
      lucid,
      fetchConfig,
    );
    const endTime = new Date(
      resolveExplicitCommitCandidateEndTimeMs(params.endTimeMs),
    );
    const transitionCommitments = yield* makeEventCommitments(
      {
        withdrawalsRoot: params.withdrawalsRoot,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: params.transactionsRoot,
        depositsRoot: params.depositsRoot,
        transitionTraceRoot:
          params.transitionTraceRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: params.eventToStepRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
      },
      {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: params.l2TransactionCount ?? 0n,
        depositCount: 0n,
      },
      {
        validationTracesRoot:
          params.validationTracesRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
        validationTraceCount: params.validationTraceCount ?? 0n,
      },
    );
    const explicitBuildResult = yield* buildUnsignedCommitTx(
      contracts,
      latestBlock,
      params.utxosRoot,
      params.transactionsRoot,
      params.depositsRoot,
      params.withdrawalsRoot,
      transitionCommitments,
      contracts.consensusProfile,
      endTime,
    );
    if ("dueWork" in explicitBuildResult) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Explicit block commit discovered scheduler due work instead of a ready transaction",
          cause: `kind=${explicitBuildResult.dueWork.kind},due_slot=${explicitBuildResult.dueWork.dueSlot.toString()},wait_ms=${explicitBuildResult.dueWork.waitMs.toString()}`,
        }),
      );
    }
    const { newHeaderHash, blockEndTimeMs, signAndSubmitProgram, txSize } =
      explicitBuildResult;

    const submittedTxHash = yield* signAndSubmitProgram;
    const shouldAwait = params.awaitConfirmation ?? true;
    if (shouldAwait) {
      yield* waitForTxConfirmation(lucid, submittedTxHash);
    }
    const blockOutRef = shouldAwait
      ? yield* fetchCommittedBlockOutRef({
          lucid,
          contracts,
          headerHash: newHeaderHash,
        })
      : null;

    return {
      submittedTxHash,
      headerHash: newHeaderHash,
      blockOutRef,
      txSize,
      blockEndTimeMs,
      roots: {
        utxosRoot: params.utxosRoot,
        transactionsRoot: params.transactionsRoot,
        depositsRoot: params.depositsRoot,
        withdrawalsRoot: params.withdrawalsRoot,
      },
    };
  });

type AwaitSpeculativeCommitInstruction = (
  candidate: SpeculativeCandidateReadyOutput["candidate"],
) => Effect.Effect<SpeculativeCommitWorkerInstruction, unknown, Database>;

type NotifySpeculativeCommitProgress = (
  output: WorkerOutput,
) => Effect.Effect<void, unknown>;

type SpeculativeMpfArtifacts =
  | {
      readonly engine: "overlay";
      readonly ledger: ParkedMpfOverlayV1;
      readonly transactions: ParkedMpfOverlayV1;
    }
  | {
      readonly engine: "event_flat";
      readonly ledger: ParkedEventFlatOverlayV1;
      readonly transactions: ParkedMpfOverlayV1;
    };

export const parkSpeculativeMpfsForConfirmationWait = ({
  ledgerFork,
  transactionsScratch,
  ledgerParent,
  closeOwningParents,
}: {
  readonly ledgerFork: MidgardMpf;
  readonly transactionsScratch: MidgardMpf;
  readonly ledgerParent: MidgardMpf;
  readonly closeOwningParents: Effect.Effect<void>;
}): Effect.Effect<SpeculativeMpfArtifacts, MpfError> => {
  const cleanup = Effect.gen(function* () {
    yield* ledgerFork
      .discardBlockOverlayIfActive()
      .pipe(Effect.catchAll(() => Effect.void));
    yield* transactionsScratch
      .discardBlockOverlayIfActive()
      .pipe(Effect.catchAll(() => Effect.void));
    yield* ledgerParent
      .discardBlockOverlayIfActive()
      .pipe(Effect.catchAll(() => Effect.void));
    yield* ledgerFork.close().pipe(Effect.catchAll(() => Effect.void));
    yield* transactionsScratch.close().pipe(Effect.catchAll(() => Effect.void));
    yield* closeOwningParents.pipe(Effect.catchAll(() => Effect.void));
  });
  return Effect.gen(function* () {
    const parked = yield* Effect.either(
      Effect.gen(function* () {
        const eventFlat = ledgerFork.usesEventFlatEngine();
        let artifacts: SpeculativeMpfArtifacts;
        if (eventFlat) {
          artifacts = {
            engine: "event_flat",
            ledger: yield* ledgerFork.parkEventFlatOverlayV1(),
            transactions: yield* transactionsScratch.parkBlockOverlay(),
          };
        } else {
          artifacts = {
            engine: "overlay",
            ledger: yield* ledgerFork.parkBlockOverlay(),
            transactions: yield* transactionsScratch.parkBlockOverlay(),
          };
        }
        yield* ledgerParent.discardBlockOverlayIfActive();
        yield* ledgerFork.close();
        yield* transactionsScratch.close();
        yield* closeOwningParents;
        return artifacts;
      }),
    );
    if (parked._tag === "Right") return parked.right;
    yield* cleanup;
    return yield* Effect.fail(parked.left);
  });
};

type ResumeParkedOverlay = (
  trieName: string,
  levelDBFilePath: string | undefined,
  artifact: ParkedMpfOverlayV1,
) => Effect.Effect<MidgardMpf, MpfError>;

type ResumeParkedEventFlatOverlay = (
  trieName: string,
  levelDBFilePath: string | undefined,
  artifact: ParkedEventFlatOverlayV1,
) => Effect.Effect<MidgardMpf, MpfError>;

type OpenLocalFinalizationTransactionsMpf = () => Effect.Effect<
  MidgardMpf,
  MpfError
>;

export const resumeSpeculativeMpfsForSubmission = ({
  artifacts,
  ledgerMpfPath,
  needsLocalFinalizationTransactionsMpf,
  resumeParkedOverlay = MidgardMpf.resumeParkedOverlay,
  resumeParkedEventFlatOverlay = MidgardMpf.resumeParkedEventFlatOverlayV1,
  openLocalFinalizationTransactionsMpf,
}: {
  readonly artifacts: SpeculativeMpfArtifacts;
  readonly ledgerMpfPath: string;
  readonly needsLocalFinalizationTransactionsMpf: boolean;
  readonly resumeParkedOverlay?: ResumeParkedOverlay;
  readonly resumeParkedEventFlatOverlay?: ResumeParkedEventFlatOverlay;
  readonly openLocalFinalizationTransactionsMpf: OpenLocalFinalizationTransactionsMpf;
}): Effect.Effect<
  {
    readonly ledgerMpf: MidgardMpf;
    readonly transactionsMpf: MidgardMpf;
    readonly localFinalizationTransactionsMpf?: MidgardMpf;
  },
  MpfError
> =>
  Effect.gen(function* () {
    let ledgerMpf: MidgardMpf | undefined;
    let transactionsMpf: MidgardMpf | undefined;
    let localFinalizationTransactionsMpf: MidgardMpf | undefined;
    const resumed = yield* Effect.either(
      Effect.gen(function* () {
        ledgerMpf =
          artifacts.engine === "event_flat"
            ? yield* resumeParkedEventFlatOverlay(
                "ledger",
                ledgerMpfPath,
                artifacts.ledger,
              )
            : yield* resumeParkedOverlay(
                "ledger",
                ledgerMpfPath,
                artifacts.ledger,
              );
        transactionsMpf = yield* resumeParkedOverlay(
          "speculative-transactions",
          undefined,
          artifacts.transactions,
        );
        if (needsLocalFinalizationTransactionsMpf) {
          localFinalizationTransactionsMpf =
            yield* openLocalFinalizationTransactionsMpf();
        }
      }),
    );
    if (resumed._tag === "Left") {
      yield* ledgerMpf?.close().pipe(Effect.catchAll(() => Effect.void)) ??
        Effect.void;
      yield* transactionsMpf
        ?.close()
        .pipe(Effect.catchAll(() => Effect.void)) ?? Effect.void;
      yield* localFinalizationTransactionsMpf
        ?.close()
        .pipe(Effect.catchAll(() => Effect.void)) ?? Effect.void;
      return yield* Effect.fail(resumed.left);
    }
    return {
      ledgerMpf: ledgerMpf!,
      transactionsMpf: transactionsMpf!,
      localFinalizationTransactionsMpf,
    };
  });

const databaseOperationsProgram = (
  workerInput: WorkerInput,
  ledgerMpf: MidgardMpf | undefined,
  transactionsMpf: MidgardMpf,
  awaitSpeculativeInstruction?: AwaitSpeculativeCommitInstruction,
  notifySpeculativeProgress?: NotifySpeculativeCommitProgress,
  activeMpfLeaseOwner?: string,
  localFinalizationTransactionsMpf?: MidgardMpf,
  postWaitMpfContext?: () => {
    readonly transactionsMpf: MidgardMpf;
    readonly localFinalizationTransactionsMpf?: MidgardMpf;
  },
  nativeMpfClient?: NativeMpfWorkerPortClient,
  nativeMpfState?: {
    context?: NativeMpfBuildContext;
    preserve: boolean;
  },
  acquireCommitLucid: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
> =>
  Effect.gen(function* () {
    let acquiredCommitLucid: Lucid | undefined;
    const acquireCommitLucidOnce = Effect.suspend(() =>
      acquiredCommitLucid === undefined
        ? acquireCommitLucid().pipe(
            Effect.tap((lucid) =>
              Effect.sync(() => {
                acquiredCommitLucid = lucid;
              }),
            ),
          )
        : Effect.succeed(acquiredCommitLucid),
    );
    const workerStartedAtMs = Date.now();
    let baseHydrationPasses = 0;
    let mpfProcessingPasses = 0;
    let speculativeReadyPassCounts:
      | {
          readonly candidateId: string;
          readonly baseHydrationPasses: number;
          readonly mpfProcessingPasses: number;
        }
      | undefined;
    const attachSpeculativeExecutionEvidence = (
      output: WorkerOutput,
    ): WorkerOutput =>
      speculativeReadyPassCounts !== undefined &&
      output.type === "SubmittedAwaitingConfirmationOutput"
        ? {
            ...output,
            speculativeExecution: {
              candidateId: speculativeReadyPassCounts.candidateId,
              baseHydrationPassesBeforeReady:
                speculativeReadyPassCounts.baseHydrationPasses,
              mpfProcessingPassesBeforeReady:
                speculativeReadyPassCounts.mpfProcessingPasses,
              baseHydrationPassesAfterReady:
                baseHydrationPasses -
                speculativeReadyPassCounts.baseHydrationPasses,
              mpfProcessingPassesAfterReady:
                mpfProcessingPasses -
                speculativeReadyPassCounts.mpfProcessingPasses,
            },
          }
        : output;
    const nodeConfig = yield* NodeConfig;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    if (deploymentIdentity.deploymentMarker === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "V1 commit production requires a verified final deployment marker",
        }),
      );
    }
    const deploymentMarker = deploymentIdentity.deploymentMarker;
    yield* configureCommitMpfRuntime(nodeConfig);
    if (
      nodeConfig.MPF_ENGINE === "architecture_g" &&
      (nativeMpfClient === undefined || workerInput.nativeMpf === undefined)
    ) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Architecture G commit worker is missing its main-owner port/root",
        }),
      );
    }
    if (nodeConfig.MPF_ENGINE !== "legacy") {
      yield* MpfEngineStateDB.assertLedgerAuditHealthy;
    }
    yield* Effect.logInfo(
      `pipeline_trace phase=commit_worker_started at_ms=${workerStartedAtMs.toString()}`,
    );
    const excludedMempoolTxIds = new Set(
      workerInput.data.speculativeBuild?.excludedMempoolTxIds ?? [],
    );
    const retrievedMempoolTxs: EntryWithTimeStamp[] = [];
    let mempoolCursor: MempoolDB.MempoolCursor | undefined;
    while (retrievedMempoolTxs.length < nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE) {
      const page = yield* MempoolDB.retrievePage({
        after: mempoolCursor,
        limit: nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE,
        upTo: new Date(workerStartedAtMs),
      });
      retrievedMempoolTxs.push(
        ...page.entries.filter(
          (entry) =>
            !excludedMempoolTxIds.has(entry[TxColumns.TX_ID].toString("hex")),
        ),
      );
      if (page.nextCursor === null) break;
      mempoolCursor = page.nextCursor;
    }
    if (retrievedMempoolTxs.length > nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE) {
      retrievedMempoolTxs.length = nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE;
    }
    const currentBlockStartTime = new Date(
      workerInput.data.currentBlockStartTimeMs,
    );
    const retrievedProcessedPendingTxs = yield* ProcessedMempoolDB.retrieve;
    const excludeSubmittedBasePayload = (
      entry: (typeof retrievedMempoolTxs)[number],
    ): boolean =>
      !excludedMempoolTxIds.has(entry[TxColumns.TX_ID].toString("hex"));
    const mempoolTxs = retrievedMempoolTxs;
    const processedPendingTxs = retrievedProcessedPendingTxs.filter(
      excludeSubmittedBasePayload,
    );
    const rawCandidateSelection = selectCommitTxCandidates({
      mempoolTxs,
      processedMempoolTxs: processedPendingTxs,
    });
    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const speculativeBuild = workerInput.data.speculativeBuild;
    const availableLocalFinalizationBlock =
      workerInput.data.availableLocalFinalizationBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    const hasAvailableLocalFinalizationBlock =
      availableLocalFinalizationBlock !== "";
    const canBuildOnConfirmedBlock =
      (hasAvailableConfirmedBlock || speculativeBuild !== undefined) &&
      !workerInput.data.localFinalizationPending;
    let latestBlockForSchedulerPlanning: SDK.StateQueueUTxO | undefined;
    let latestEndTimeMsForSchedulerPlanning: number | undefined;
    if (canBuildOnConfirmedBlock) {
      if (speculativeBuild === undefined) {
        if (availableConfirmedBlock === "") {
          return yield* Effect.fail(
            new CommitWorkerInvariantError({
              message:
                "Confirmed commit build is missing its serialized state-queue base",
            }),
          );
        }
        latestBlockForSchedulerPlanning = yield* deserializeStateQueueUTxO(
          availableConfirmedBlock,
        );
        latestEndTimeMsForSchedulerPlanning = Number(
          (yield* getLatestBlockDatumEndTime(
            latestBlockForSchedulerPlanning.datum,
          )).getTime(),
        );
      } else {
        latestEndTimeMsForSchedulerPlanning =
          speculativeBuild.base.blockEndTimeMs;
      }
      const stateQueueEvidence: CommitSchedulerStateQueueEvidence = {
        tailCommitBaseOutRef:
          speculativeBuild === undefined
            ? outRefLabel(latestBlockForSchedulerPlanning!.utxo)
            : `${speculativeBuild.base.submittedTxHash}#0`,
        tailBlockEndTimeMs: latestEndTimeMsForSchedulerPlanning,
        stateQueueHasUnmergedTail:
          workerInput.data.stateQueueHasUnmergedTail ?? false,
      };
      if (speculativeBuild === undefined) {
        const contracts = yield* MidgardContracts;
        const lucid = yield* acquireCommitLucidOnce;
        yield* lucid.switchToOperatorsMainWallet;
        const preIngestionPlan = yield* Effect.either(
          resolveEarliestCommitSchedulerDueWorkPlan({
            lucid: lucid.api,
            contracts,
            submitSlotSnapshot: lucid.submitSlotSnapshot,
            stateQueueEvidence,
            localFinalizationPending: workerInput.data.localFinalizationPending,
            callerLabel: "commit-scheduler-worker-pre-ingestion",
            discoveryStage: "worker_pre_ingestion",
          }),
        );
        if (preIngestionPlan._tag === "Right") {
          const output = workerPreIngestionDueWorkOutputFromPlan(
            preIngestionPlan.right,
          );
          if (output !== undefined) {
            yield* Effect.logInfo(
              `🔹 Registered slot-aware due work before commit ingestion barriers discovery_stage=worker_pre_ingestion (kind=${output.dueWork.kind},key=${output.dueWork.key},current_slot=${output.dueWork.observedSlot.toString()},due_slot=${output.dueWork.dueSlot.toString()},due_at_ms=${output.dueWork.dueAtMs.toString()},wait_ms=${output.dueWork.waitMs.toString()},slot_source=${output.dueWork.slotSource},dependency_key=${output.dueWork.dependencyKey}).`,
            );
            return output;
          }
        } else {
          yield* Effect.logWarning(
            `🔹 Worker pre-ingestion scheduler due-work preflight failed; continuing to full planner: ${String(preIngestionPlan.left)}`,
          );
        }
      }
    }
    const depositIngestionBarrierTime =
      speculativeBuild === undefined
        ? yield* acquireCommitLucidOnce.pipe(
            Effect.flatMap((lucid) =>
              fetchAndInsertDepositUTxOsForCommitBarrier(new Date()).pipe(
                Effect.provideService(Lucid, lucid),
              ),
            ),
          )
        : new Date(speculativeBuild.watermarks.depositMs);
    const withdrawalIngestionBarrierTime =
      speculativeBuild === undefined
        ? yield* acquireCommitLucidOnce.pipe(
            Effect.flatMap((lucid) =>
              fetchAndInsertWithdrawalUTxOsForCommitBarrier(
                depositIngestionBarrierTime,
              ).pipe(Effect.provideService(Lucid, lucid)),
            ),
          )
        : new Date(speculativeBuild.watermarks.withdrawalMs);
    const txOrderIngestionBarrierTime =
      speculativeBuild === undefined
        ? yield* acquireCommitLucidOnce.pipe(
            Effect.flatMap((lucid) =>
              fetchAndInsertTxOrderUTxOsForCommitBarrier(
                withdrawalIngestionBarrierTime,
              ).pipe(Effect.provideService(Lucid, lucid)),
            ),
          )
        : new Date(speculativeBuild.watermarks.txOrderMs);
    const userEventOnlyEndTime = [
      depositIngestionBarrierTime,
      withdrawalIngestionBarrierTime,
      txOrderIngestionBarrierTime,
    ].reduce((earliest, candidate) =>
      candidate.getTime() < earliest.getTime() ? candidate : earliest,
    );

    let currentSchedulerWindow: CurrentOperatorSchedulerWindow | undefined;
    let currentWindowCommitEndTimeFit:
      | ReturnType<typeof resolveCommitEndTimeFit>
      | undefined;
    const schedulerPlanningNowMs = Date.now();
    if (canBuildOnConfirmedBlock && speculativeBuild === undefined) {
      const contracts = yield* MidgardContracts;
      const lucid = yield* acquireCommitLucidOnce;
      yield* lucid.switchToOperatorsMainWallet;
      currentSchedulerWindow = yield* resolveCurrentOperatorSchedulerWindow(
        lucid.api,
        contracts,
      );
      if (currentSchedulerWindow !== undefined) {
        const latestEndTimeMs = Number(latestEndTimeMsForSchedulerPlanning);
        const txBackedCandidateEndTime = establishEndTimeFromTxRequests(
          rawCandidateSelection.candidateTxs,
        );
        const candidateEndTimeMs = Option.isSome(txBackedCandidateEndTime)
          ? txBackedCandidateEndTime.value.getTime()
          : userEventOnlyEndTime.getTime();
        currentWindowCommitEndTimeFit = resolveCommitEndTimeFit({
          lucid: lucid.api,
          latestEndTime: latestEndTimeMs,
          candidateEndTime: candidateEndTimeMs,
          nowMs: schedulerPlanningNowMs,
          minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
          maximumEndTimeMs: currentSchedulerWindow.endTimeMs,
        });
      }
    }
    const schedulerAwareCommitSelection = planSchedulerAwareCommitSelection({
      candidateSelection: rawCandidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      currentBlockStartTimeMs: currentBlockStartTime.getTime(),
      nowMs: schedulerPlanningNowMs,
      minimumCurrentWindowBudgetMs: COMMIT_MIN_PRE_WITNESS_BUDGET_MS,
      productionMinimumFutureBufferMs:
        COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      currentWindowCommitEndTimeFit,
    });
    if (
      schedulerAwareCommitSelection.status === "using_current_scheduler_window"
    ) {
      yield* Effect.logInfo(
        `🔹 Scheduler-aware commit planner using current scheduler window (${schedulerAwareCommitSelection.reason},user_event_end_time=${schedulerAwareCommitSelection.userEventOnlyEndTime.toISOString()}).`,
      );
    } else if (
      schedulerAwareCommitSelection.status ===
        "current_scheduler_budget_too_low" ||
      schedulerAwareCommitSelection.status ===
        "current_scheduler_end_time_floor_exceeds_window"
    ) {
      yield* Effect.logInfo(
        `🔹 Scheduler-aware commit planner will not cap to current scheduler window (${schedulerAwareCommitSelection.reason}).`,
      );
    }
    const calibration =
      nodeConfig.COMMIT_BUILD_COST_MODEL === "ewma"
        ? yield* CommitBuildCalibrationDB.retrieve
        : undefined;
    const estimatedCommitBuildMsPerTx =
      calibration === undefined
        ? DEFAULT_COMMIT_BATCH_BUDGET_LIMITS.estimatedCommitBuildMsPerTx
        : calibratedCommitBuildMsPerTx({
            msPerTxEwma: calibration.msPerTxEwma,
            safetyFactor: nodeConfig.COMMIT_BUILD_EWMA_SAFETY_FACTOR,
          });
    const budgetedCommitSelection = planCommitBatchBudgets({
      candidateSelection: schedulerAwareCommitSelection.candidateSelection,
      limits: {
        ...DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
        maxL2TxCount: nodeConfig.COMMIT_MAX_L2_TX_COUNT,
        maxLedgerOpCount: nodeConfig.COMMIT_MAX_LEDGER_OP_COUNT,
        maxTransitionStepCount: nodeConfig.COMMIT_MAX_TRANSITION_STEP_COUNT,
        estimatedCommitBuildMsPerTx,
      },
    });
    const batchSelectedAtMs = Date.now();
    if (
      budgetedCommitSelection.plan.selectedTxCount > 0 ||
      budgetedCommitSelection.prunedTxCount > 0
    ) {
      yield* Effect.logInfo(
        `🔹 Commit batch planner selected tx_count=${budgetedCommitSelection.plan.selectedTxCount.toString()}, tx_bytes=${budgetedCommitSelection.plan.selectedTxBytes.toString()}, estimated_da_payload_bytes=${budgetedCommitSelection.plan.estimatedDaPayloadBytes.toString()}, estimated_commit_build_ms=${budgetedCommitSelection.plan.estimatedCommitBuildMs.toString()}, stop_reason=${budgetedCommitSelection.plan.stopReason}, pruned_tx_count=${budgetedCommitSelection.prunedTxCount.toString()}.`,
      );
    }
    const candidateSelection = budgetedCommitSelection.candidateSelection;
    yield* Effect.logInfo(
      `pipeline_trace phase=batch_selected at_ms=${batchSelectedAtMs.toString()} elapsed_ms=${Math.max(0, batchSelectedAtMs - workerStartedAtMs).toString()} selected_tx_count=${budgetedCommitSelection.plan.selectedTxCount.toString()} selected_tx_bytes=${budgetedCommitSelection.plan.selectedTxBytes.toString()} stop_reason=${budgetedCommitSelection.plan.stopReason}`,
    );
    const effectiveUserEventOnlyEndTime =
      schedulerAwareCommitSelection.userEventOnlyEndTime;
    const blockEndTimeCapMs = schedulerAwareCommitSelection.blockEndTimeCapMs;
    if (
      shouldDeferCommitSubmission({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        hasAvailableConfirmedBlock: hasAvailableLocalFinalizationBlock,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 Local finalization pending and no recoverable confirmed block is available yet; deferring new submission.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const recoverableLocalFinalizationBlock =
      workerInput.data.localFinalizationPending &&
      hasAvailableLocalFinalizationBlock
        ? availableLocalFinalizationBlock
        : undefined;
    if (recoverableLocalFinalizationBlock !== undefined) {
      const recoverableConfirmedBlock = yield* deserializeStateQueueUTxO(
        recoverableLocalFinalizationBlock,
      );
      const lucid = yield* acquireCommitLucidOnce;
      return yield* recoverLocalFinalizationAgainstConfirmedBlock({
        latestBlock: recoverableConfirmedBlock,
        transactionsMpf,
        processedMempoolTxs: [],
        mempoolTxHashes: [],
        workerInput,
        sizeOfProcessedTxs: 0,
        consensusProfile: deploymentIdentity.consensusProfile,
      }).pipe(Effect.provideService(Lucid, lucid));
    }

    if (speculativeBuild === undefined) {
      const foreignEventResolution =
        yield* reconcileOverdueAwaitingEventsAgainstRetainedForeignTips();
      if (foreignEventResolution.type === "AwaitingForeignDa") {
        return {
          type: "AwaitingForeignDaOutput",
          foreignHeaderHash: foreignEventResolution.foreignHeaderHash,
          reason: `${foreignEventResolution.reason}:${foreignEventResolution.detail}`,
        } satisfies WorkerOutput;
      }
    }

    const pendingUserEventCount = yield* pendingUserEventCountUpTo(
      effectiveUserEventOnlyEndTime,
    );
    if (
      shouldSkipIdleCommitBehindUnmergedTail({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        stateQueueHasUnmergedTail:
          workerInput.data.stateQueueHasUnmergedTail ?? false,
        mempoolTxCount: mempoolTxs.length,
        processedTxCount: processedPendingTxs.length,
        pendingUserEventCount,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 State queue has an unmerged tail and no pending tx/user-event work; waiting for merge before the next commit attempt.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    if (
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: candidateSelection.candidateTxs.length,
        processedPendingTxCount: processedPendingTxs.length,
        pendingUserEventCount,
        localFinalizationPending: workerInput.data.localFinalizationPending,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 No pending tx/user-event work for block commitment; skipping commit base hydration.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const baseHydrationStartedAtMs = Date.now();
    baseHydrationPasses += 1;
    const commitBase = yield* resolveCommitBaseLedgerEntries({
      availableConfirmedBlock,
      speculativeBase: speculativeBuild?.base,
      ledgerMpf,
      nativeMpfRoot: workerInput.nativeMpf?.durableRoot,
      requireEntries:
        nodeConfig.MPF_PAYLOAD_ROOT_CHECK === "every_block" ||
        nodeConfig.MPF_RECORD_CORPUS.trim().length > 0,
    });
    const initialLedgerEntries = yield* alignCommitMpfsToBase({
      ledgerMpf,
      nativeMpfRoot: workerInput.nativeMpf?.durableRoot,
      transactionsMpf,
      base: commitBase,
    });
    const nativeMpfContext: NativeMpfBuildContext | undefined =
      nativeMpfClient === undefined
        ? undefined
        : {
            client: nativeMpfClient,
            handle: yield* Effect.tryPromise({
              try: () => nativeMpfClient.fork(commitBase.root),
              catch: (cause) =>
                new CommitWorkerInvariantError({
                  message: `Architecture G fork failed: ${String(cause)}`,
                }),
            }),
            ownerBinarySha256: workerInput.nativeMpf!.ownerBinarySha256,
          };
    if (nativeMpfState !== undefined) {
      nativeMpfState.context = nativeMpfContext;
    }
    yield* Effect.logInfo(
      `🔹 Commit base hydration phase completed duration_ms=${Math.max(
        0,
        Date.now() - baseHydrationStartedAtMs,
      ).toString()},source=${commitBase.source},base_entry_count=${initialLedgerEntries.length.toString()}`,
    );
    if (speculativeBuild !== undefined) {
      yield* reachPipelinedCommitCrashCheckpoint("speculative_mid_build");
    }
    const mpfProcessingStartedAtMs = Date.now();
    mpfProcessingPasses += 1;
    const proofValidationLucid = (yield* acquireCommitLucidOnce).api;
    const processed = yield* processMpfs(
      ledgerMpf,
      transactionsMpf,
      candidateSelection.candidateTxs,
      {
        currentBlockStartTime: canBuildOnConfirmedBlock
          ? currentBlockStartTime
          : undefined,
        processedOnlyEndTime:
          candidateSelection.sourceTable === ProcessedMempoolDB.tableName
            ? candidateSelection.candidateTxs[0]?.[TxColumns.TIMESTAMPTZ]
            : undefined,
        depositVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? depositIngestionBarrierTime
          : undefined,
        withdrawalVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? withdrawalIngestionBarrierTime
          : undefined,
        txOrderVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? txOrderIngestionBarrierTime
          : undefined,
        depositOnlyEndTime: canBuildOnConfirmedBlock
          ? effectiveUserEventOnlyEndTime
          : undefined,
        initialLedgerEntries,
        consensusProfile: deploymentIdentity.consensusProfile,
        forcedValidation: {
          expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
          bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
          slotForUnixTime: (unixTimeMs) =>
            BigInt(proofValidationLucid.unixTimeToSlot(unixTimeMs)),
        },
        selectedBaseUtxoRoot: commitBase.root,
        payloadRootCheck: nodeConfig.MPF_PAYLOAD_ROOT_CHECK,
        baseUtxoPayloadAggregate: commitBase.utxoPayloadAggregate,
        recordCorpusPath: nodeConfig.MPF_RECORD_CORPUS,
        excludedDepositEventIds:
          speculativeBuild === undefined
            ? undefined
            : new Set(speculativeBuild.excludedDepositEventIds),
        excludedForcedTransactionEventIds:
          speculativeBuild === undefined
            ? undefined
            : new Set(speculativeBuild.excludedForcedTransactionEventIds),
        excludedWithdrawalEventIds:
          speculativeBuild === undefined
            ? undefined
            : new Set(speculativeBuild.excludedWithdrawalEventIds),
        deferDatabaseWrites: speculativeBuild !== undefined,
        nativeMpf: nativeMpfContext,
      },
    );
    const mpfProcessingFinishedAtMs = Date.now();
    yield* Effect.logInfo(
      `pipeline_trace phase=mpf_processing_finished at_ms=${mpfProcessingFinishedAtMs.toString()} duration_ms=${Math.max(0, mpfProcessingFinishedAtMs - mpfProcessingStartedAtMs).toString()}`,
    );

    const {
      utxoRoot,
      rawTxRoot,
      txRoot,
      transitionTraceRoot,
      eventToStepRoot,
      validationTracesRoot,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
      transitionStepCount,
      validationTraceCount,
      utxoPayloadEntries,
      ledgerDelta,
      utxoPayloadAggregate,
      rejectedMempoolTxsCount,
      rejectedMempoolTxHashes,
      rejectionEntries,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedForcedTransactionEntriesCount,
      includedForcedTransactionEntries,
      includedForcedTransactionEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
      nativeMpfReplay,
      nativeMpfHandle,
    } = processed;

    const attachNativeMpfPromotion = (output: WorkerOutput): WorkerOutput => {
      if (
        nativeMpfHandle === undefined ||
        (output.type !== "SubmittedAwaitingConfirmationOutput" &&
          output.type !== "SubmittedAwaitingLocalFinalizationOutput" &&
          output.type !== "SuccessfulSubmissionOutput")
      ) {
        return output;
      }
      if (nativeMpfState !== undefined) nativeMpfState.preserve = true;
      return {
        ...output,
        nativeMpfPromotion: { handle: nativeMpfHandle },
      };
    };

    const processedMempoolTxs = processed.processedMempoolTxs;
    const mempoolTxHashes = processed.mempoolTxHashes;
    const rejectedMempoolTxIdSet = new Set(
      rejectedMempoolTxHashes.map((txId) => txId.toString("hex")),
    );
    const rejectedMempoolTxs = candidateSelection.candidateTxs.filter((entry) =>
      rejectedMempoolTxIdSet.has(entry[TxColumns.TX_ID].toString("hex")),
    );
    const sizeOfProcessedTxs = processed.sizeOfProcessedTxs;
    const mempoolTxSourceTable =
      candidateSelection.sourceTable === "processed_mempool"
        ? ProcessedMempoolDB.tableName
        : candidateSelection.sourceTable === "mempool"
          ? MempoolDB.tableName
          : "none";
    const recordSuccessfulBuildCalibration = (output: WorkerOutput) =>
      Effect.gen(function* () {
        if (
          calibration === undefined ||
          processedMempoolTxs.length === 0 ||
          !shouldPreserveCommitMpfRoots(output)
        ) {
          return;
        }
        const measuredBuildMs = Math.max(
          0,
          mpfProcessingFinishedAtMs - mpfProcessingStartedAtMs,
        );
        const nextEwma = updateCommitBuildEwma({
          previousMsPerTx: calibration.msPerTxEwma,
          measuredBuildMs,
          processedTxCount: processedMempoolTxs.length,
          alpha: nodeConfig.COMMIT_BUILD_EWMA_ALPHA,
        });
        const updated = yield* CommitBuildCalibrationDB.update(nextEwma);
        yield* Effect.logInfo(
          `commit_build_calibration measured_ms_per_tx=${(
            measuredBuildMs / processedMempoolTxs.length
          ).toString()} ewma_ms_per_tx=${updated.msPerTxEwma.toString()} sample_count=${updated.sampleCount.toString()}`,
        );
      });
    if (candidateSelection.sourceTable === "processed_mempool") {
      yield* Effect.logWarning(
        `🔹 Prioritizing ${processedPendingTxs.length.toString()} deferred processed tx(s) before newer mempool tx(s).`,
      );
    }

    if (rejectedMempoolTxsCount > 0) {
      yield* Effect.logWarning(
        `Rejected ${rejectedMempoolTxsCount} malformed tx(s) during commitment preprocessing.`,
      );
    }
    if (includedDepositEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedDepositEntriesCount} due deposit UTxO(s).`,
      );
    }
    if (includedWithdrawalEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedWithdrawalEntriesCount} due withdrawal event(s).`,
      );
    }
    if (includedForcedTransactionEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment source set includes ${includedForcedTransactionEntriesCount} due tx-order event(s).`,
      );
    }

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime = establishEndTimeFromTxRequests(processedMempoolTxs);
    let submitAvailableConfirmedBlock = availableConfirmedBlock;
    let submitWorkerInput = workerInput;
    let beforePendingJournalInsert:
      | Effect.Effect<void, DatabaseError, Database>
      | undefined;

    if (
      submitAvailableConfirmedBlock === "" &&
      speculativeBuild !== undefined
    ) {
      if (awaitSpeculativeInstruction === undefined) {
        return yield* Effect.fail(
          new CommitWorkerInvariantError({
            message: "Speculative commit build requires an instruction channel",
          }),
        );
      }
      const candidateEndTime = Option.isSome(optEndTime)
        ? optEndTime.value
        : effectiveUserEventOnlyEndTime;
      const candidateId = randomUUID();
      const [optDepositsRoot, optForcedTransactionsRoot, optWithdrawalsRoot] =
        yield* Effect.all(
          [
            resolveDepositsRoot(includedDepositEntries),
            resolveForcedTransactionsRoot(
              includedForcedTransactionEntries,
              deploymentIdentity.consensusProfile,
            ),
            resolveWithdrawalsRoot(includedWithdrawalEntries),
          ],
          { concurrency: "unbounded" },
        );
      const depositsRoot = Option.getOrElse(
        optDepositsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      const forcedTransactionsRoot = Option.getOrElse(
        optForcedTransactionsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      const withdrawalsRoot = Option.getOrElse(
        optWithdrawalsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      const candidate = {
        candidateId,
        baseHeaderHash: speculativeBuild.base.headerHash,
        endTimeMs: candidateEndTime.getTime(),
        builtAtMs: Date.now(),
        buildDurationMs: Math.max(0, Date.now() - workerStartedAtMs),
        invalidationKey: `${speculativeBuild.base.headerHash}:${candidateEndTime.getTime().toString()}:${minimumBarrierWatermarkMs(speculativeBuild.watermarks).toString()}`,
        watermarks: speculativeBuild.watermarks,
        expectedUserEventCounts: {
          deposits: includedDepositEventIds.length,
          forcedTransactions: includedForcedTransactionEventIds.length,
          withdrawals: includedWithdrawalEventIds.length,
        },
        expectedL2TransactionCount: processedMempoolTxs.length,
        roots: {
          utxos: utxoRoot,
          rawTransactions: rawTxRoot,
          transactions: txRoot,
          deposits: depositsRoot,
          forcedTransactions: forcedTransactionsRoot,
          withdrawals: withdrawalsRoot,
          transitionTrace: transitionTraceRoot,
          eventToStep: eventToStepRoot,
        },
      } satisfies SpeculativeCandidateReadyOutput["candidate"];
      yield* Effect.logInfo(
        `pipeline_trace phase=candidate_ready candidate_id=${candidateId} base_header_hash=${candidate.baseHeaderHash} build_duration_ms=${candidate.buildDurationMs.toString()} invalidation_key=${candidate.invalidationKey}`,
      );
      yield* reachPipelinedCommitCrashCheckpoint("candidate_ready_unconfirmed");
      speculativeReadyPassCounts = {
        candidateId,
        baseHydrationPasses,
        mpfProcessingPasses,
      };
      const instruction = yield* awaitSpeculativeInstruction(candidate);
      if (instruction.type === "InvalidateSpeculativeCandidate") {
        return {
          type: "SpeculativeCandidateInvalidatedOutput",
          candidateId,
          reason: instruction.reason,
        } satisfies SpeculativeCandidateInvalidatedOutput;
      }
      // Candidate construction remains provider-free. The parent acquires the
      // L1 control-plane permit before sending this submit instruction, so
      // provider-backed scheduler revalidation starts only after CandidateReady.
      const speculativeContracts = yield* MidgardContracts;
      const speculativeLucid = yield* acquireCommitLucidOnce;
      const resumedMpfContext = postWaitMpfContext?.();
      const confirmedBlock = yield* deserializeStateQueueUTxO(
        instruction.confirmedBlock,
      );
      const confirmedHeaderHash =
        confirmedBlock.datum.key === "Empty"
          ? undefined
          : yield* SDK.getHeaderV1FromStateQueueDatum(
              confirmedBlock.datum,
            ).pipe(Effect.flatMap(SDK.hashBlockHeaderV1));
      if (confirmedHeaderHash !== speculativeBuild.base.headerHash) {
        return {
          type: "SpeculativeCandidateInvalidatedOutput",
          candidateId,
          reason: "T2",
        } satisfies SpeculativeCandidateInvalidatedOutput;
      }
      if (instruction.localFinalizationBlock !== undefined) {
        if (activeMpfLeaseOwner === undefined) {
          return yield* Effect.fail(
            new CommitWorkerInvariantError({
              message:
                "Speculative local finalization requires an active MPF lease owner",
            }),
          );
        }
        const recoveryOutput =
          yield* recoverLocalFinalizationAgainstConfirmedBlock({
            latestBlock: yield* deserializeStateQueueUTxO(
              instruction.localFinalizationBlock,
            ),
            transactionsMpf:
              resumedMpfContext?.localFinalizationTransactionsMpf ??
              localFinalizationTransactionsMpf ??
              transactionsMpf,
            processedMempoolTxs: [],
            mempoolTxHashes: [],
            workerInput,
            sizeOfProcessedTxs: 0,
            consensusProfile: deploymentIdentity.consensusProfile,
            beforeTransactionsMpfReset: Effect.all(
              [
                StateQueueMutationLeasesDB.revalidate(
                  instruction.stateQueueLeaseToken,
                ),
                MpfEngineStateDB.revalidateLedgerStoreLease(
                  activeMpfLeaseOwner,
                ),
              ],
              { discard: true },
            ),
          }).pipe(Effect.provideService(Lucid, speculativeLucid));
        if (
          recoveryOutput.type !== "SuccessfulLocalFinalizationRecoveryOutput"
        ) {
          return recoveryOutput;
        }
        if (notifySpeculativeProgress !== undefined) {
          yield* notifySpeculativeProgress(recoveryOutput);
        }
      }
      const excludedUserEventIds = {
        depositEventIds: new Set(speculativeBuild.excludedDepositEventIds),
        forcedTransactionEventIds: new Set(
          speculativeBuild.excludedForcedTransactionEventIds,
        ),
        withdrawalEventIds: new Set(
          speculativeBuild.excludedWithdrawalEventIds,
        ),
      };
      const submitPendingUserEventCount = yield* pendingUserEventCountUpTo(
        candidateEndTime,
        excludedUserEventIds,
      );
      const expectedUserEventCount =
        candidate.expectedUserEventCounts.deposits +
        candidate.expectedUserEventCounts.forcedTransactions +
        candidate.expectedUserEventCounts.withdrawals;
      if (submitPendingUserEventCount !== expectedUserEventCount) {
        return {
          type: "SpeculativeCandidateInvalidatedOutput",
          candidateId,
          reason: "T3",
        } satisfies SpeculativeCandidateInvalidatedOutput;
      }
      yield* speculativeLucid.switchToOperatorsMainWallet;
      const submitSchedulerWindow =
        yield* resolveCurrentOperatorSchedulerWindow(
          speculativeLucid.api,
          speculativeContracts,
        );
      if (submitSchedulerWindow !== undefined) {
        const confirmedEndTimeMs = Number(
          (yield* getLatestBlockDatumEndTime(confirmedBlock.datum)).getTime(),
        );
        const submitFit = resolveCommitEndTimeFit({
          lucid: speculativeLucid.api,
          latestEndTime: confirmedEndTimeMs,
          candidateEndTime: candidateEndTime.getTime(),
          nowMs: Date.now(),
          minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
          maximumEndTimeMs: submitSchedulerWindow.endTimeMs,
        });
        if (submitFit.status === "exceeds_cap") {
          return {
            type: "SpeculativeCandidateInvalidatedOutput",
            candidateId,
            reason: "T4",
          } satisfies SpeculativeCandidateInvalidatedOutput;
        }
      }
      if (activeMpfLeaseOwner === undefined) {
        return yield* Effect.fail(
          new CommitWorkerInvariantError({
            message:
              "Speculative journal preparation requires an active MPF lease owner",
          }),
        );
      }
      // The speculative builder is read-only. Journal preparation invokes
      // this effect only after confirmation and inside the journal SQL
      // transaction, while both state-queue and MPF leases are held.
      beforePendingJournalInsert =
        revalidateAndPersistSpeculativeCandidateSources({
          includedDepositEntries,
          includedForcedTransactionEntries,
          includedWithdrawalEntries,
          selectedMempoolTxs: processedMempoolTxs,
          rejectedMempoolTxs,
          mempoolTxSourceTable,
          rejectionEntries,
          expectedEventRoots: {
            deposits: candidate.roots.deposits,
            forcedTransactions: candidate.roots.forcedTransactions,
            withdrawals: candidate.roots.withdrawals,
          },
          candidateEndTime,
          excludedUserEventIds,
          stateQueueLeaseToken: instruction.stateQueueLeaseToken,
          activeMpfLeaseOwner,
          consensusProfile: deploymentIdentity.consensusProfile,
        });
      submitAvailableConfirmedBlock = instruction.confirmedBlock;
      submitWorkerInput = {
        data: {
          ...workerInput.data,
          availableConfirmedBlock: instruction.confirmedBlock,
          localFinalizationPending: false,
          stateQueueLeaseToken: instruction.stateQueueLeaseToken,
          baseSnapshotId: instruction.baseSnapshotId,
          stateQueueHasUnmergedTail: instruction.stateQueueHasUnmergedTail,
          speculativeBuild: undefined,
        },
      };
    }

    const submissionContracts = yield* MidgardContracts;
    const submissionLucid = yield* acquireCommitLucidOnce;

    if (submitAvailableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
      if (mempoolTxSourceTable === ProcessedMempoolDB.tableName) {
        yield* Effect.logInfo(
          "🔹 No confirmed block available and selected tx payload is already durable in ProcessedMempoolDB; preserving it for the next commit attempt.",
        );
        return {
          type: "SkippedSubmissionOutput",
          mempoolTxsCount: 0,
          sizeOfProcessedTxs: 0,
        } satisfies WorkerOutput;
      }
      const output = yield* deferProcessedCommitPayloadUntilConfirmation({
        processedMempoolTxs,
        mempoolTxHashes,
        mempoolTxsCount,
        sizeOfProcessedTxs,
      });
      yield* recordSuccessfulBuildCalibration(output);
      return output;
    } else {
      yield* Effect.logInfo(
        "🔹 Previous submitted block is now confirmed, deserializing...",
      );
      const latestBlock = yield* deserializeStateQueueUTxO(
        submitAvailableConfirmedBlock,
      );

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const output = yield* submitDepositOnlyCommit({
          contracts: submissionContracts,
          consensusProfile: deploymentIdentity.consensusProfile,
          deploymentMarker,
          latestBlock,
          endTime: effectiveUserEventOnlyEndTime,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          workerInput: submitWorkerInput,
          blockEndTimeCapMs,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          validationTracesRoot,
          transitionTraceMembers,
          eventToStepMembers,
          validationTraceMembers,
          transitionStepCount,
          validationTraceCount,
          utxoPayloadEntries,
          ledgerDelta,
          utxoPayloadAggregate,
          selectedBaseUtxosRoot: commitBase.root,
          implicitGenesisEntries:
            commitBase.source === "genesis" ? initialLedgerEntries : [],
          beforePendingJournalInsert,
          nativeMpfReplay,
        }).pipe(Effect.provideService(Lucid, submissionLucid));
        return attachNativeMpfPromotion(
          attachSpeculativeExecutionEvidence(output),
        );
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. Use the shared max-candidate timestamp rule as the upper
        // bound of the block we are about to submit.
        const endTime = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const output = yield* submitTxBackedCommit({
          contracts: submissionContracts,
          consensusProfile: deploymentIdentity.consensusProfile,
          deploymentMarker,
          latestBlock,
          endTime,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          validationTracesRoot,
          transitionTraceMembers,
          eventToStepMembers,
          validationTraceMembers,
          transitionStepCount,
          validationTraceCount,
          utxoPayloadEntries,
          ledgerDelta,
          utxoPayloadAggregate,
          selectedBaseUtxosRoot: commitBase.root,
          implicitGenesisEntries:
            commitBase.source === "genesis" ? initialLedgerEntries : [],
          transactionsMpf:
            postWaitMpfContext?.().transactionsMpf ?? transactionsMpf,
          processedMempoolTxs,
          mempoolTxHashes,
          mempoolTxSourceTable,
          workerInput: submitWorkerInput,
          sizeOfProcessedTxs,
          blockEndTimeCapMs,
          beforePendingJournalInsert,
          nativeMpfReplay,
        }).pipe(Effect.provideService(Lucid, submissionLucid));
        yield* recordSuccessfulBuildCalibration(output);
        return attachNativeMpfPromotion(
          attachSpeculativeExecutionEvidence(output),
        );
      }
    }
  });

// Export the production commit worker core so emulator tests can exercise the
// exact same effect graph without going through a worker-thread bootstrap.
export const runCommitBlockHeaderWorkerProgram = (
  workerInput: WorkerInput,
  awaitSpeculativeInstruction?: AwaitSpeculativeCommitInstruction,
  notifySpeculativeProgress?: NotifySpeculativeCommitProgress,
  commitLucidFactory: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const nodeConfig = yield* NodeConfig;
    const stateQueueLeaseToken = workerInput.data.stateQueueLeaseToken;
    if (stateQueueLeaseToken !== undefined) {
      // Production lock order is state-queue mutation lease, then MPF store
      // lease. The parent owns/renews the former while this worker runs.
      yield* StateQueueMutationLeasesDB.revalidate(stateQueueLeaseToken);
    }
    const leaseOwner = workerInput.data.ledgerStoreLeaseOwner;
    if (
      !/^commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/.test(
        leaseOwner,
      )
    ) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Commit worker requires a unique parent-generated ledger MPF lease owner",
        }),
      );
    }
    const leasedResult = yield* MpfEngineStateDB.tryWithLedgerStoreLease(
      leaseOwner,
      (activeLeaseOwner) =>
        Effect.gen(function* () {
          const nativeMpfClient =
            workerInput.nativeMpf === undefined
              ? undefined
              : new NativeMpfWorkerPortClient(workerInput.nativeMpf.port);
          const nativeMpfState: {
            context?: NativeMpfBuildContext;
            preserve: boolean;
          } = { preserve: false };
          const opened =
            nodeConfig.MPF_ENGINE === "architecture_g"
              ? {
                  ledgerMpf: undefined,
                  transactionsMpf: yield* MidgardMpf.createScratch(
                    "architecture-g-transactions",
                    { engine: "overlay" },
                  ),
                }
              : yield* makeMpfs;
          const { ledgerMpf, transactionsMpf } = opened;
          let owningParentsClosed = false;
          const closeMpfs = Effect.suspend(() => {
            if (owningParentsClosed) return Effect.void;
            owningParentsClosed = true;
            return Effect.all(
              [
                ledgerMpf?.close().pipe(Effect.catchAll(() => Effect.void)) ??
                  Effect.void,
                transactionsMpf
                  .close()
                  .pipe(Effect.catchAll(() => Effect.void)),
              ],
              { discard: true },
            );
          });

          const mpfs = [ledgerMpf!, transactionsMpf] as const;
          let speculativeStateQueueLeaseToken: string | undefined;
          const runProgram = (
            activeLedgerMpf: MidgardMpf | undefined,
            activeTransactionsMpf: MidgardMpf,
            activeAwaitSpeculativeInstruction?: AwaitSpeculativeCommitInstruction,
            localFinalizationTransactionsMpf?: MidgardMpf,
            postWaitMpfContext?: () => {
              readonly transactionsMpf: MidgardMpf;
              readonly localFinalizationTransactionsMpf?: MidgardMpf;
            },
          ) =>
            databaseOperationsProgram(
              workerInput,
              activeLedgerMpf,
              activeTransactionsMpf,
              activeAwaitSpeculativeInstruction,
              notifySpeculativeProgress,
              activeLeaseOwner,
              localFinalizationTransactionsMpf,
              postWaitMpfContext,
              nativeMpfClient,
              nativeMpfState,
              commitLucidFactory,
            ).pipe(
              Effect.tap((output) =>
                shouldPreserveCommitMpfRoots(output)
                  ? Effect.gen(function* () {
                      const activeStateQueueLeaseToken =
                        stateQueueLeaseToken ?? speculativeStateQueueLeaseToken;
                      if (activeStateQueueLeaseToken !== undefined) {
                        yield* StateQueueMutationLeasesDB.revalidate(
                          activeStateQueueLeaseToken,
                        );
                      }
                      yield* MpfEngineStateDB.revalidateLedgerStoreLease(
                        activeLeaseOwner,
                      );
                    })
                  : Effect.void,
              ),
            );
          const program =
            nodeConfig.MPF_ENGINE === "architecture_g"
              ? Effect.gen(function* () {
                  yield* transactionsMpf.beginBlockOverlay();
                  const awaitInstructionWithRetainedNativeHandle =
                    workerInput.data.speculativeBuild === undefined ||
                    awaitSpeculativeInstruction === undefined
                      ? undefined
                      : (
                          candidate: SpeculativeCandidateReadyOutput["candidate"],
                        ) =>
                          Effect.gen(function* () {
                            yield* MpfEngineStateDB.releaseLedgerStoreLease(
                              activeLeaseOwner,
                            );
                            const instruction =
                              yield* awaitSpeculativeInstruction(candidate);
                            if (
                              instruction.type !== "SubmitSpeculativeCandidate"
                            ) {
                              return instruction;
                            }
                            yield* StateQueueMutationLeasesDB.revalidate(
                              instruction.stateQueueLeaseToken,
                            );
                            let reacquired = false;
                            for (let attempt = 0; attempt < 200; attempt += 1) {
                              reacquired =
                                yield* MpfEngineStateDB.acquireLedgerStoreLease(
                                  {
                                    owner: activeLeaseOwner,
                                    ttlMs: 10 * 60 * 1000,
                                  },
                                );
                              if (reacquired) break;
                              yield* Effect.sleep("50 millis");
                            }
                            if (!reacquired) {
                              return yield* Effect.fail(
                                new CommitWorkerInvariantError({
                                  message:
                                    "Timed out reacquiring the logical MPF lease for Architecture G speculative submission",
                                }),
                              );
                            }
                            return instruction;
                          });
                  const result = yield* Effect.either(
                    runProgram(
                      undefined,
                      transactionsMpf,
                      awaitInstructionWithRetainedNativeHandle,
                      transactionsMpf,
                      () => ({ transactionsMpf }),
                    ),
                  );
                  yield* transactionsMpf.discardBlockOverlayIfActive();
                  if (result._tag === "Left") {
                    return yield* Effect.fail(result.left);
                  }
                  return result.right;
                })
              : nodeConfig.MPF_ENGINE !== "legacy" &&
                  workerInput.data.speculativeBuild !== undefined
                ? Effect.gen(function* () {
                    yield* ledgerMpf!.beginBlockOverlay();
                    const speculativeLedgerMpf =
                      yield* ledgerMpf!.forkBlockOverlay();
                    const speculativeTransactionsMpf =
                      yield* MidgardMpf.createScratch(
                        "speculative-transactions",
                        { engine: "overlay" },
                      );
                    yield* speculativeTransactionsMpf.beginBlockOverlay();
                    let activeSpeculativeLedgerMpf = speculativeLedgerMpf;
                    let activeSpeculativeTransactionsMpf =
                      speculativeTransactionsMpf;
                    let activeLocalFinalizationTransactionsMpf:
                      | MidgardMpf
                      | undefined = transactionsMpf;
                    let parkedForConfirmationWait = false;
                    const closeActiveSpeculativeMpfs = Effect.suspend(() =>
                      Effect.all(
                        [
                          activeSpeculativeLedgerMpf
                            .close()
                            .pipe(Effect.catchAll(() => Effect.void)),
                          activeSpeculativeTransactionsMpf
                            .close()
                            .pipe(Effect.catchAll(() => Effect.void)),
                          activeLocalFinalizationTransactionsMpf !==
                            undefined &&
                          activeLocalFinalizationTransactionsMpf !==
                            transactionsMpf &&
                          activeLocalFinalizationTransactionsMpf !==
                            activeSpeculativeTransactionsMpf
                            ? activeLocalFinalizationTransactionsMpf
                                .close()
                                .pipe(Effect.catchAll(() => Effect.void))
                            : Effect.void,
                        ],
                        { discard: true },
                      ),
                    );
                    const awaitInstructionWithParkedMpfs =
                      awaitSpeculativeInstruction === undefined
                        ? undefined
                        : (
                            candidate: SpeculativeCandidateReadyOutput["candidate"],
                          ) =>
                            Effect.gen(function* () {
                              const parked =
                                yield* parkSpeculativeMpfsForConfirmationWait({
                                  ledgerFork: speculativeLedgerMpf,
                                  transactionsScratch:
                                    speculativeTransactionsMpf,
                                  ledgerParent: ledgerMpf!,
                                  closeOwningParents: closeMpfs,
                                });
                              parkedForConfirmationWait = true;
                              // No live Trie/Store/Level reference survives the
                              // ReadyCandidate boundary. Only after both
                              // authenticated artifacts are parked and both
                              // owning Level handles are closed may the logical
                              // MPF lease be released and confirmation proceed.
                              yield* MpfEngineStateDB.releaseLedgerStoreLease(
                                activeLeaseOwner,
                              );
                              const instruction =
                                yield* awaitSpeculativeInstruction(candidate);
                              if (
                                instruction.type !==
                                "SubmitSpeculativeCandidate"
                              ) {
                                return instruction;
                              }

                              speculativeStateQueueLeaseToken =
                                instruction.stateQueueLeaseToken;
                              // The submitter acquires the state-queue lease
                              // before posting this instruction. Revalidate it
                              // before restoring the state_queue -> MPF lock
                              // order and reopening any parked stores.
                              yield* StateQueueMutationLeasesDB.revalidate(
                                instruction.stateQueueLeaseToken,
                              );
                              let reacquired = false;
                              for (
                                let attempt = 0;
                                attempt < 200;
                                attempt += 1
                              ) {
                                reacquired =
                                  yield* MpfEngineStateDB.acquireLedgerStoreLease(
                                    {
                                      owner: activeLeaseOwner,
                                      ttlMs: 10 * 60 * 1000,
                                    },
                                  );
                                if (reacquired) break;
                                yield* Effect.sleep("50 millis");
                              }
                              if (!reacquired) {
                                return yield* Effect.fail(
                                  new CommitWorkerInvariantError({
                                    message:
                                      "Timed out reacquiring the ledger MPF lease for speculative submission",
                                  }),
                                );
                              }
                              const resumed =
                                yield* resumeSpeculativeMpfsForSubmission({
                                  artifacts: parked,
                                  ledgerMpfPath: nodeConfig.LEDGER_MPF_DB_PATH,
                                  needsLocalFinalizationTransactionsMpf:
                                    instruction.localFinalizationBlock !==
                                    undefined,
                                  openLocalFinalizationTransactionsMpf: () =>
                                    MidgardMpf.create(
                                      "transactions",
                                      nodeConfig.TRANSACTIONS_MPF_DB_PATH,
                                      {
                                        engine:
                                          nodeConfig.MPF_ENGINE ===
                                          "architecture_g"
                                            ? "overlay"
                                            : nodeConfig.MPF_ENGINE,
                                        spillThresholdBytes:
                                          nodeConfig.MPF_OVERLAY_SPILL_BYTES,
                                      },
                                    ),
                                });
                              activeSpeculativeLedgerMpf = resumed.ledgerMpf;
                              activeSpeculativeTransactionsMpf =
                                resumed.transactionsMpf;
                              activeLocalFinalizationTransactionsMpf =
                                resumed.localFinalizationTransactionsMpf;
                              parkedForConfirmationWait = false;
                              return instruction;
                            });
                    const result = yield* Effect.either(
                      runProgram(
                        speculativeLedgerMpf,
                        speculativeTransactionsMpf,
                        awaitInstructionWithParkedMpfs,
                        transactionsMpf,
                        () => ({
                          transactionsMpf: activeSpeculativeTransactionsMpf,
                          localFinalizationTransactionsMpf:
                            activeLocalFinalizationTransactionsMpf,
                        }),
                      ),
                    );
                    const discardForksAndParents = Effect.suspend(() =>
                      parkedForConfirmationWait
                        ? Effect.void
                        : Effect.gen(function* () {
                            yield* activeSpeculativeLedgerMpf.discardBlockOverlayIfActive();
                            yield* ledgerMpf!.discardBlockOverlayIfActive();
                            yield* activeSpeculativeTransactionsMpf.discardBlockOverlayIfActive();
                            yield* closeActiveSpeculativeMpfs;
                          }),
                    );
                    if (result._tag === "Left") {
                      yield* discardForksAndParents;
                      return yield* Effect.fail(result.left);
                    }
                    if (!shouldPreserveCommitMpfRoots(result.right)) {
                      yield* discardForksAndParents;
                      return result.right;
                    }
                    // Only the ledger child owns durable candidate state. Its
                    // promotion occurs after the submit path and both leases are
                    // revalidated above. The per-block transaction tree is
                    // scratch-only; the journal is its recovery source of truth.
                    const speculativeLedgerRoot =
                      yield* activeSpeculativeLedgerMpf.root();
                    yield* activeSpeculativeLedgerMpf.flushBlockOverlay(
                      speculativeLedgerRoot,
                    );
                    yield* activeSpeculativeTransactionsMpf.discardBlockOverlayIfActive();
                    yield* closeActiveSpeculativeMpfs;
                    return result.right;
                  })
                : nodeConfig.MPF_ENGINE !== "legacy"
                  ? withMpfBlockOverlays(
                      mpfs,
                      runProgram(ledgerMpf!, transactionsMpf, undefined),
                      shouldPreserveCommitMpfRoots,
                    )
                  : withMpfRootTransactions(
                      mpfs,
                      runProgram(ledgerMpf!, transactionsMpf, undefined),
                      shouldPreserveCommitMpfRoots,
                    );
          const finalizeNativeGenerationLease = Effect.suspend(() => {
            const context = nativeMpfState.context;
            if (context === undefined || nativeMpfClient === undefined) {
              return Effect.void;
            }
            if (nativeMpfState.preserve) {
              return Effect.promise(() =>
                nativeMpfClient.retainForJournal(context.handle),
              ).pipe(Effect.orDie);
            }
            return Effect.promise(() =>
              nativeMpfClient.discard(context.handle),
            ).pipe(Effect.catchAll(() => Effect.void));
          });
          return yield* program.pipe(
            Effect.ensuring(finalizeNativeGenerationLease),
            Effect.ensuring(closeMpfs),
            Effect.ensuring(Effect.sync(() => nativeMpfClient?.close())),
          );
        }),
    );
    if (leasedResult._tag === "Busy") {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message: "Ledger MPF store is busy with an audit or another commit",
        }),
      );
    }
    const result = leasedResult.value;
    if (result === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Block commitment worker completed without producing a worker output",
        }),
      );
    }
    return result;
  });

/**
 * Runs the exact production speculative commit-candidate path and stops at the
 * candidate-ready boundary. This is the only benchmark-safe build-only entry:
 * it never advances to source revalidation, journal preparation, signing, or
 * L1 submission, and the ordinary worker cleanup discards the native
 * generation after the typed invalidation instruction.
 */
export const runCommitBlockHeaderCandidateBuildProgram = (
  workerInput: WorkerInput,
  commitLucidFactory: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  SpeculativeCandidateReadyOutput["candidate"],
  unknown,
  MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
> =>
  Effect.gen(function* () {
    if (workerInput.data.speculativeBuild === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Commit-candidate build-only program requires speculativeBuild input",
        }),
      );
    }
    let captured: SpeculativeCandidateReadyOutput["candidate"] | undefined;
    const output = yield* runCommitBlockHeaderWorkerProgram(
      workerInput,
      (candidate) =>
        Effect.sync(() => {
          captured = candidate;
          return {
            type: "InvalidateSpeculativeCandidate",
            reason: "T1",
          } satisfies SpeculativeCommitWorkerInstruction;
        }),
      undefined,
      commitLucidFactory,
    );
    if (
      captured === undefined ||
      output.type !== "SpeculativeCandidateInvalidatedOutput" ||
      output.candidateId !== captured.candidateId
    ) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message: `Commit-candidate build-only program did not stop at the candidate-ready boundary (output=${output.type})`,
        }),
      );
    }
    return captured;
  });

if (parentPort !== null) {
  const workerParentPort = parentPort;
  const inputData = workerData as WorkerInput;

  const awaitSpeculativeInstruction: AwaitSpeculativeCommitInstruction = (
    candidate,
  ) =>
    Effect.async((resume) => {
      const onInstruction = (
        instruction: SpeculativeCommitWorkerInstruction,
      ) => {
        workerParentPort.off("message", onInstruction);
        resume(Effect.succeed(instruction));
      };
      workerParentPort.on("message", onInstruction);
      workerParentPort.postMessage({
        type: "SpeculativeCandidateReadyOutput",
        candidate,
      } satisfies SpeculativeCandidateReadyOutput);
      return Effect.sync(() => workerParentPort.off("message", onInstruction));
    });

  const notifySpeculativeProgress: NotifySpeculativeCommitProgress = (output) =>
    Effect.sync(() => workerParentPort.postMessage(output));

  const program = provideCommitBlockWorkerServices(
    runCommitBlockHeaderWorkerProgram(
      inputData,
      inputData.data.speculativeBuild === undefined
        ? undefined
        : awaitSpeculativeInstruction,
      inputData.data.speculativeBuild === undefined
        ? undefined
        : notifySpeculativeProgress,
    ),
  );

  Effect.runPromise(
    program.pipe(
      Effect.catchAllCause((cause) =>
        Effect.succeed({
          type: "FailureOutput",
          error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
        }),
      ),
    ),
  ).then((output) => {
    Effect.runSync(
      Effect.logInfo(
        `👷 Block commitment work completed (${JSON.stringify(output)}).`,
      ),
    );
    parentPort?.postMessage(output);
  });
}
