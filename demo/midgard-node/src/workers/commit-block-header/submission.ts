import {
  encodeMidgardCekProgramMaterialDaValue,
  mergeMidgardCekProgramMaterialSidecars,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
  projectDaPayloadSizes,
} from "@al-ft/midgard-core/da-payload-sizing";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { fromHex } from "@lucid-evolution/lucid";
import { Data, Effect, Option } from "effect";

import { readDaHardeningConfig } from "../../da/hardening-config.js";
import {
  DepositsDB,
  ForcedTransactionsDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "../../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import type * as Ledger from "../../database/utils/ledger.js";
import { Columns as TxColumns } from "../../database/utils/tx.js";
import { reachPipelinedCommitCrashCheckpoint } from "../../e2e/pipelined-commit-crash-checkpoint.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "../../fibers/fetch-and-insert-deposit-utxos.js";
import { fetchAndInsertTxOrderUTxOsForCommitBarrier } from "../../fibers/fetch-and-insert-tx-order-utxos.js";
import { fetchAndInsertWithdrawalUTxOsForCommitBarrier } from "../../fibers/fetch-and-insert-withdrawal-utxos.js";
import { sameSpeculativeSourceIdSet } from "../../fibers/speculative-commit-state.js";
import {
  emptyRootHexProgram,
  encodeTransactionRootValue,
  type LedgerDelta,
  type MidgardMpf,
  type NativeMpfReplayBuild,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type RetainedValidationTraceMember,
  type UtxoPayloadEntry,
  type UtxoPayloadSizeAggregate,
} from "../../mpf/index.js";
import {
  fetchOperatorWalletView,
  isPotentiallyStaleOperatorWalletViewError,
  type OperatorWalletView,
} from "../../operator-wallet-view.js";
import {
  ContractDeploymentIdentity,
  type ContractDeploymentIdentityValue,
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../../services/index.js";
import {
  isUnknownOutputReferenceSubmitError,
  TxSignError,
  TxSubmitError,
} from "../../transactions/utils.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "../utils/commit-block-header.js";
import { selectCommitRoots } from "../utils/commit-block-planner.js";
import {
  failedSubmissionProgram,
  recoverSubmittedTxHashByHeaderProgram,
  skippedSubmissionProgram,
  successfulLocalFinalizationRecoveryProgram,
} from "../utils/commit-submission.js";
import { buildUnsignedCommitTx } from "./build-unsigned-tx.js";
import {
  resolveDepositsRoot,
  resolveForcedTransactionsRoot,
  resolveWithdrawalsRoot,
} from "./event-roots.js";
import {
  assertLiveTailCommitBase,
  assertPendingJournalCompleteness,
  buildPendingJournalMetadata,
  resolveLiveTailCommitBase,
  resolvePendingJournalLedgerState,
  revalidateStateQueueLease,
} from "./pending-journal.js";
import {
  getHeaderFromStateQueueDatumLocal,
  hashBlockHeaderLocal,
  stateQueueOutRef,
} from "./state-queue.js";
import { makeEventCommitments } from "./transition-commitments.js";

const COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES = 1;

const daEntry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const forcedProgramMaterialSidecars = (
  entries: readonly ForcedTransactionsDB.Entry[],
): readonly Buffer[] =>
  entries.map((entry) => {
    const sidecar =
      entry[ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR];
    if (sidecar == null || sidecar.length === 0) {
      throw new Error(
        `V1 forced transaction ${entry[
          ForcedTransactionsDB.Columns.TX_ORDER_ID
        ].toString("hex")} is missing canonical CEK program material`,
      );
    }
    return Buffer.from(sidecar);
  });

const daProgramMaterialFromSidecars = (
  sidecars: Iterable<Uint8Array>,
): readonly SDK.DaPayloadEntry[] =>
  mergeMidgardCekProgramMaterialSidecars(sidecars).map((entry) =>
    daEntry(
      Buffer.from(entry.root),
      encodeMidgardCekProgramMaterialDaValue(entry),
    ),
  );

export const assertPreSubmitDaPayloadSize = ({
  headerHash,
  header,
  utxoPayloadAggregate,
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
  processedMempoolTxs,
  transitionTraceMembers,
  eventToStepMembers,
  validationTraceMembers,
  cekProgramMaterial,
  envelopeMode = readDaHardeningConfig().envelopeMode,
}: {
  readonly headerHash: string;
  readonly header: SDK.Header;
  readonly utxoPayloadAggregate: UtxoPayloadSizeAggregate;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly cekProgramMaterial: readonly SDK.DaPayloadEntry[];
  readonly envelopeMode?: DaPayloadEmissionMode;
}): Effect.Effect<number, DatabaseError> =>
  Effect.gen(function* () {
    const transactionSources = yield* Effect.try({
      try: () =>
        processedMempoolTxs.map((entry) =>
          daEntry(
            entry[TxColumns.TX_ID],
            encodeTransactionRootValue(
              entry[TxColumns.TX],
              MIDGARD_CONSENSUS_PROFILE,
            ),
          ),
        ),
      catch: (cause) =>
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: "Failed to derive V1 transaction sources for DA sizing",
          cause,
        }),
    });
    const forcedTransactionPreimages = includedForcedTransactionEntries.map(
      (entry) =>
        daEntry(
          entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR],
        ),
    );
    const withdrawals = yield* Effect.forEach(
      includedWithdrawalEntries,
      (entry) => {
        const value = entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
        return value === null
          ? Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Cannot size DA payload for an unclassified withdrawal",
                cause: `withdrawal_event_id=${entry[
                  WithdrawalsDB.Columns.ID
                ].toString("hex")}`,
              }),
            )
          : Effect.succeed(
              daEntry(entry[WithdrawalsDB.Columns.ID], Buffer.from(value)),
            );
      },
    );
    const commonBody = {
      header_hash: headerHash,
      // The exact aggregate replaces this list in the sizing function.
      utxos: [],
      withdrawals,
      forced_transactions: includedForcedTransactionEntries.map((entry) =>
        daEntry(
          entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
        ),
      ),
      transactions: transactionSources,
      deposits: includedDepositEntries.map((entry) =>
        daEntry(entry[DepositsDB.Columns.ID], entry[DepositsDB.Columns.INFO]),
      ),
      transition_trace: transitionTraceMembers.map((entry) =>
        daEntry(entry.keyCbor, entry.valueCbor),
      ),
      event_to_step: eventToStepMembers.map((entry) =>
        daEntry(entry.keyCbor, entry.valueCbor),
      ),
      validation_trace_witnesses: validationTraceMembers.flatMap(
        (entry) => entry.witnesses,
      ),
    };
    const encodedBytes = SDK.daPayloadEncodedSizeFromUtxoAggregate(
      {
        version: SDK.DA_PAYLOAD_VERSION,
        block_body: {
          ...commonBody,
          header,
          transaction_preimages: processedMempoolTxs.map((entry) =>
            daEntry(entry[TxColumns.TX_ID], entry[TxColumns.TX]),
          ),
          forced_transaction_preimages: forcedTransactionPreimages,
          cek_program_material: [...cekProgramMaterial],
          validation_traces: validationTraceMembers.map((entry) =>
            daEntry(entry.keyCbor, entry.valueCbor),
          ),
          counts: {
            withdrawalCount: header.withdrawalCount,
            forcedTransactionCount: header.forcedTransactionCount,
            l2TransactionCount: header.l2TransactionCount,
            depositCount: header.depositCount,
            totalEventCount: header.totalEventCount,
            transitionStepCount: header.transitionStepCount,
            validationTraceCount: header.validationTraceCount,
          },
        },
      },
      utxoPayloadAggregate,
    );
    const projection = projectDaPayloadSizes(encodedBytes, envelopeMode);
    const effectiveInnerLimit = maxDaPayloadInnerBytes(envelopeMode);
    if (
      encodedBytes > effectiveInnerLimit ||
      projection.storedBytesUpperBound > DA_TRANSPORT_LIMITS.maxPayloadBytes ||
      projection.requestBytesUpperBound > DA_TRANSPORT_LIMITS.maxPayloadBytes
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to prepare or submit a block whose DA payload cannot fit the V1 submit frame",
          cause: `header_hash=${headerHash},envelope_mode=${envelopeMode},inner_bytes=${encodedBytes.toString()},stored_bytes_upper_bound=${projection.storedBytesUpperBound.toString()},request_bytes_upper_bound=${projection.requestBytesUpperBound.toString()},effective_inner_limit=${effectiveInnerLimit.toString()},max_frame_bytes=${DA_TRANSPORT_LIMITS.maxPayloadBytes.toString()},utxo_entry_count=${utxoPayloadAggregate.entryCount.toString()},utxo_encoded_tuple_bytes=${utxoPayloadAggregate.encodedTupleBytes.toString()}`,
        }),
      );
    }
    yield* Effect.logInfo(
      `da_payload_pre_submit_inner_bytes=${encodedBytes.toString()} da_payload_stored_bytes_upper_bound=${projection.storedBytesUpperBound.toString()} da_payload_request_bytes_upper_bound=${projection.requestBytesUpperBound.toString()} da_payload_effective_inner_limit=${effectiveInnerLimit.toString()} da_payload_envelope_mode=${envelopeMode} da_payload_frame_limit_bytes=${DA_TRANSPORT_LIMITS.maxPayloadBytes.toString()} utxo_entry_count=${utxoPayloadAggregate.entryCount.toString()} utxo_encoded_tuple_bytes=${utxoPayloadAggregate.encodedTupleBytes.toString()}`,
    );
    return encodedBytes;
  });

class StaleOperatorWalletRetrySignal extends Data.TaggedError(
  "StaleOperatorWalletRetrySignal",
)<{
  readonly pendingHeaderHash: Buffer;
  readonly txSubmitError: TxSubmitError;
}> {}

const maybeAbandonPreviousStaleAttempt = (
  previousPendingHeaderHash: Buffer | undefined,
  nextHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  previousPendingHeaderHash === undefined ||
  previousPendingHeaderHash.equals(nextHeaderHash)
    ? Effect.void
    : PendingBlockFinalizationsDB.markAbandoned(previousPendingHeaderHash).pipe(
        Effect.catchAll((cause) =>
          Effect.logWarning(
            `🔹 Previous stale pending journal was already cleared before retry (header=${previousPendingHeaderHash.toString(
              "hex",
            )}): ${formatUnknownError(cause)}`,
          ),
        ),
      );

const assertCommitInputsWithinBlockEndTime = ({
  blockEndTimeMs,
  processedMempoolTxs = [],
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
}: {
  readonly blockEndTimeMs: number;
  readonly processedMempoolTxs?: readonly TxTable.EntryWithTimeStamp[];
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const violations = [
      ...processedMempoolTxs
        .filter(
          (entry) => entry[TxColumns.TIMESTAMPTZ].getTime() > blockEndTimeMs,
        )
        .map(
          (entry) =>
            `tx:${entry[TxColumns.TX_ID].toString("hex")}@${entry[
              TxColumns.TIMESTAMPTZ
            ].toISOString()}`,
        ),
      ...includedDepositEntries
        .filter(
          (entry) =>
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime() > blockEndTimeMs,
        )
        .map(
          (entry) =>
            `deposit:${entry[DepositsDB.Columns.ID].toString("hex")}@${entry[
              DepositsDB.Columns.INCLUSION_TIME
            ].toISOString()}`,
        ),
      ...includedForcedTransactionEntries
        .filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() >
            blockEndTimeMs,
        )
        .map(
          (entry) =>
            `forced:${entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
              "hex",
            )}@${entry[
              ForcedTransactionsDB.Columns.INCLUSION_TIME
            ].toISOString()}`,
        ),
      ...includedWithdrawalEntries
        .filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() >
            blockEndTimeMs,
        )
        .map(
          (entry) =>
            `withdrawal:${entry[WithdrawalsDB.Columns.ID].toString(
              "hex",
            )}@${entry[WithdrawalsDB.Columns.INCLUSION_TIME].toISOString()}`,
        ),
    ];
    if (violations.length > 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to prepare a pending commit journal with inputs after the block end-time",
          cause: `block_end_time_ms=${blockEndTimeMs.toString()},violations=${violations.join(",")}`,
        }),
      );
    }
  });

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
  sameSpeculativeSourceIdSet(pendingDepositIds, includedDepositIds) &&
  sameSpeculativeSourceIdSet(
    pendingForcedTransactionIds,
    includedForcedTransactionIds,
  ) &&
  sameSpeculativeSourceIdSet(pendingWithdrawalIds, includedWithdrawalIds);

type CommitUserEventSourceRefreshers<E, R> = {
  readonly deposit: (upperBound: Date) => Effect.Effect<Date, E, R>;
  readonly withdrawal: (upperBound: Date) => Effect.Effect<Date, E, R>;
  readonly txOrder: (upperBound: Date) => Effect.Effect<Date, E, R>;
};

export const refreshCommitUserEventSourcesThroughBlockEnd = <
  E = SDK.LucidError | DatabaseError,
  R =
    | ContractDeploymentIdentity
    | Database
    | Lucid
    | MidgardContracts
    | NodeConfig,
>(
  blockEndTimeMs: number,
  refreshers: CommitUserEventSourceRefreshers<E, R> = {
    deposit: fetchAndInsertDepositUTxOsForCommitBarrier,
    withdrawal: fetchAndInsertWithdrawalUTxOsForCommitBarrier,
    txOrder: fetchAndInsertTxOrderUTxOsForCommitBarrier,
  } as unknown as CommitUserEventSourceRefreshers<E, R>,
) =>
  Effect.gen(function* () {
    const finalBlockEndTime = new Date(blockEndTimeMs);
    yield* refreshers.deposit(finalBlockEndTime);
    yield* refreshers.withdrawal(finalBlockEndTime);
    yield* refreshers.txOrder(finalBlockEndTime);
  });

const assertCommitUserEventSourceCompleteness = ({
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

const signalStaleOperatorWalletRetry = ({
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

const journalUtxoEntries = (
  entries: readonly UtxoPayloadEntry[],
): readonly PendingBlockFinalizationsDB.UtxoInput[] =>
  entries.map((entry) => ({
    [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: entry.outref,
    [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: entry.output,
  }));

const submitErrorReferencesOutRef = (
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

const isStaleCommitBaseError = (error: unknown): boolean =>
  error instanceof SDK.StateQueueError &&
  formatUnknownError(error, { includeCause: true }).includes(
    "Commit base is stale",
  );

const runWithStaleOperatorWalletRetry = <A, E, R>({
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
        yield* PendingBlockFinalizationsDB.markAbandoned(
          lastResult.left.pendingHeaderHash,
        );
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

export const submitDepositOnlyCommit = ({
  contracts,
  consensusProfile,
  deploymentMarker,
  latestBlock,
  endTime,
  includedDepositEntries,
  includedDepositEventIds,
  includedForcedTransactionEntries,
  includedForcedTransactionEventIds,
  includedWithdrawalEntries,
  includedWithdrawalEventIds,
  workerInput,
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
  selectedBaseUtxosRoot,
  implicitGenesisEntries,
  beforePendingJournalInsert,
  nativeMpfReplay,
}: {
  readonly contracts: SDK.MidgardValidators;
  readonly consensusProfile: ContractDeploymentIdentityValue["consensusProfile"];
  readonly deploymentMarker: NonNullable<
    ContractDeploymentIdentityValue["deploymentMarker"]
  >;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedForcedTransactionEventIds: readonly Buffer[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly includedWithdrawalEventIds: readonly Buffer[];
  readonly workerInput: WorkerInput;
  readonly blockEndTimeCapMs?: number;
  readonly utxoRoot: string;
  readonly txRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly transitionStepCount: number;
  readonly validationTraceCount: number;
  readonly utxoPayloadEntries: readonly UtxoPayloadEntry[];
  readonly ledgerDelta: LedgerDelta;
  readonly utxoPayloadAggregate: UtxoPayloadSizeAggregate;
  readonly selectedBaseUtxosRoot: string;
  readonly implicitGenesisEntries: readonly Ledger.MinimalEntry[];
  readonly nativeMpfReplay?: NativeMpfReplayBuild;
  readonly beforePendingJournalInsert?: (
    blockEndTimeMs: number,
  ) => Effect.Effect<void, DatabaseError, Database>;
}) =>
  Effect.gen(function* () {
    const [optDepositsRoot, optForcedTransactionsRoot, optWithdrawalsRoot] =
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
      );
    if (
      Option.isNone(optDepositsRoot) &&
      Option.isNone(optForcedTransactionsRoot) &&
      Option.isNone(optWithdrawalsRoot)
    ) {
      yield* Effect.logInfo("🔹 Nothing to commit.");
      return {
        type: "NothingToCommitOutput",
      } as WorkerOutput;
    }

    const emptyRoot = yield* emptyRootHexProgram;
    const depositsRoot = Option.isSome(optDepositsRoot)
      ? optDepositsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    const withdrawalsRoot = Option.isSome(optWithdrawalsRoot)
      ? optWithdrawalsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    const forcedTransactionsRoot = Option.isSome(optForcedTransactionsRoot)
      ? optForcedTransactionsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    yield* Effect.logInfo(
      `🔹 Forced transactions root is: ${forcedTransactionsRoot}`,
    );
    yield* Effect.logInfo(`🔹 Withdrawals root is: ${withdrawalsRoot}`);
    const submittedAwaitingConfirmationOutput = (
      submittedTxHash: string,
      txSize: number,
      blockEndTimeMs: number,
      submittedHeaderHash: string,
    ) =>
      Effect.succeed({
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash,
        txSize,
        mempoolTxsCount: workerInput.data.mempoolTxsCountSoFar,
        sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
        blockEndTimeMs,
        submittedHeaderHash,
        submittedUtxosRoot: roots.utxoRoot,
      } satisfies WorkerOutput);
    const roots = selectCommitRoots({
      hasTxRequests: false,
      computedUtxoRoot: utxoRoot,
      computedTxRoot: txRoot,
      emptyRoot,
    });
    yield* assertPendingJournalCompleteness({
      utxoRoot: roots.utxoRoot,
      utxoMemberCount: utxoPayloadEntries.length,
      hasLedgerDelta: true,
      txRoot: roots.txRoot,
      emptyTxRoot: emptyRoot,
      txMemberCount: 0,
      depositsRoot,
      depositMemberCount: includedDepositEventIds.length,
      forcedTransactionsRoot,
      forcedTransactionMemberCount: includedForcedTransactionEventIds.length,
      withdrawalsRoot,
      withdrawalMemberCount: includedWithdrawalEventIds.length,
      transitionTraceRoot,
      transitionTraceMemberCount: transitionTraceMembers.length,
      eventToStepRoot,
      eventToStepMemberCount: eventToStepMembers.length,
      validationTracesRoot,
      validationTraceMemberCount: validationTraceMembers.length,
      expectedValidationTraceCount: validationTraceCount,
    });
    const transitionCommitments = yield* makeEventCommitments(
      {
        withdrawalsRoot,
        forcedTransactionsRoot,
        transactionsRoot: roots.txRoot,
        depositsRoot,
        transitionTraceRoot,
        eventToStepRoot,
      },
      {
        withdrawalCount: BigInt(includedWithdrawalEventIds.length),
        forcedTransactionCount: BigInt(
          includedForcedTransactionEventIds.length,
        ),
        l2TransactionCount: 0n,
        depositCount: BigInt(includedDepositEventIds.length),
        transitionStepCount: BigInt(transitionStepCount),
      },
      {
        validationTracesRoot,
        validationTraceCount: BigInt(validationTraceCount),
      },
    );

    const submitCommitAttempt = (
      initialOperatorWalletView?: OperatorWalletView,
      previousPendingHeaderHash?: Buffer,
    ) =>
      revalidateStateQueueLease(workerInput).pipe(
        Effect.andThen(
          resolveLiveTailCommitBase(contracts, latestBlock, consensusProfile),
        ),
        Effect.flatMap((commitBaseTail) =>
          buildUnsignedCommitTx(
            contracts,
            commitBaseTail,
            roots.utxoRoot,
            roots.txRoot,
            depositsRoot,
            withdrawalsRoot,
            transitionCommitments,
            consensusProfile,
            endTime,
            initialOperatorWalletView,
            blockEndTimeCapMs,
          ).pipe(
            Effect.flatMap((buildResult) => {
              if ("dueWork" in buildResult) {
                const output: WorkerOutput = {
                  type: "RegisteredDueWorkOutput",
                  dueWork: buildResult.dueWork,
                };
                return Effect.succeed(output);
              }
              const {
                newHeaderHash,
                newHeader,
                newHeaderCbor,
                blockEndTimeMs,
                signAndSubmitProgram,
                txSize,
              } = buildResult;
              return Effect.gen(function* () {
                yield* refreshCommitUserEventSourcesThroughBlockEnd(
                  blockEndTimeMs,
                );
                const headerHashBuffer = Buffer.from(fromHex(newHeaderHash));
                const cekProgramMaterial = yield* Effect.try({
                  try: () =>
                    daProgramMaterialFromSidecars(
                      forcedProgramMaterialSidecars(
                        includedForcedTransactionEntries,
                      ),
                    ),
                  catch: (cause) =>
                    new DatabaseError({
                      table: ForcedTransactionsDB.tableName,
                      message:
                        "Cannot build V1 DA from missing or conflicting forced-transaction program material",
                      cause,
                    }),
                });
                yield* assertPreSubmitDaPayloadSize({
                  headerHash: newHeaderHash,
                  header: newHeader,
                  utxoPayloadAggregate,
                  includedDepositEntries,
                  includedForcedTransactionEntries,
                  includedWithdrawalEntries,
                  processedMempoolTxs: [],
                  transitionTraceMembers,
                  eventToStepMembers,
                  validationTraceMembers,
                  cekProgramMaterial,
                });
                yield* maybeAbandonPreviousStaleAttempt(
                  previousPendingHeaderHash,
                  headerHashBuffer,
                );
                yield* MpfEngineStateDB.stampLedgerPayloadAggregate({
                  rootHex: roots.utxoRoot,
                  aggregate: utxoPayloadAggregate,
                });
                yield* assertCommitInputsWithinBlockEndTime({
                  blockEndTimeMs,
                  includedDepositEntries,
                  includedForcedTransactionEntries,
                  includedWithdrawalEntries,
                });
                const metadata = yield* buildPendingJournalMetadata({
                  latestBlock: commitBaseTail,
                  workerInput,
                  blockEndTimeMs,
                  expectedRoots: {
                    utxosRoot: roots.utxoRoot,
                    forcedTransactionsRoot,
                    transactionsRoot: roots.txRoot,
                    depositsRoot,
                    withdrawalsRoot,
                    transitionTraceRoot:
                      transitionCommitments.transitionTraceRoot,
                    eventToStepRoot: transitionCommitments.eventToStepRoot,
                    validationTracesRoot,
                  },
                  expectedCounts: {
                    withdrawalCount: transitionCommitments.withdrawalCount,
                    forcedTransactionCount:
                      transitionCommitments.forcedTransactionCount,
                    l2TransactionCount:
                      transitionCommitments.l2TransactionCount,
                    depositCount: transitionCommitments.depositCount,
                    totalEventCount: transitionCommitments.totalEventCount,
                    transitionStepCount:
                      transitionCommitments.transitionStepCount,
                    validationTraceCount: BigInt(validationTraceCount),
                  },
                  consensusProfile,
                  deploymentMarker,
                });
                const journalLedgerState =
                  yield* resolvePendingJournalLedgerState({
                    recordedBaseUtxosRoot: metadata.baseRoots.utxosRoot,
                    selectedBaseUtxosRoot,
                    expectedFinalUtxosRoot: roots.utxoRoot,
                    expectedFinalEntryCount: utxoPayloadAggregate.entryCount,
                    implicitGenesisEntries,
                    transitionDelta: ledgerDelta,
                  });
                const beforeJournalInsert =
                  beforePendingJournalInsert?.(blockEndTimeMs) ??
                  assertCommitUserEventSourceCompleteness({
                    blockEndTimeMs,
                    includedDepositEntries,
                    includedForcedTransactionEntries,
                    includedWithdrawalEntries,
                  });
                return yield* PendingBlockFinalizationsDB.preparePendingSubmission(
                  {
                    headerHash: headerHashBuffer,
                    headerCbor: newHeaderCbor,
                    metadata,
                    blockEndTime: new Date(blockEndTimeMs),
                    depositEventIds: includedDepositEventIds,
                    depositEntries: includedDepositEntries,
                    forcedTransactionEventIds:
                      includedForcedTransactionEventIds,
                    forcedTransactionEntries: includedForcedTransactionEntries,
                    withdrawalEventIds: includedWithdrawalEventIds,
                    withdrawalEntries: includedWithdrawalEntries,
                    mempoolTxIds: [],
                    mempoolTxs: [],
                    mempoolTxProgramMaterialSidecars: [],
                    mempoolTxSourceTable: "none",
                    transitionTraceMembers,
                    eventToStepMembers,
                    validationTraceMembers,
                    validationTraceWitnessMembers:
                      validationTraceMembers.flatMap((entry) =>
                        entry.witnesses.map(([key, value]) => ({
                          keyCbor: Buffer.from(key, "hex"),
                          valueCbor: Buffer.from(value, "hex"),
                        })),
                      ),
                    ledgerDelta: {
                      spent: journalLedgerState.ledgerDelta.spent,
                      produced: journalUtxoEntries(
                        journalLedgerState.ledgerDelta.produced,
                      ),
                    },
                    utxoPayloadAggregate,
                    nativeMpfReplay,
                  },
                  { beforeJournalInsert },
                ).pipe(
                  Effect.andThen(
                    reachPipelinedCommitCrashCheckpoint(
                      "journal_prepared_before_submit",
                    ),
                  ),
                  Effect.andThen(
                    Effect.matchEffect(
                      revalidateStateQueueLease(workerInput).pipe(
                        Effect.andThen(
                          assertLiveTailCommitBase(contracts, commitBaseTail),
                        ),
                        Effect.andThen(signAndSubmitProgram),
                      ),
                      {
                        onFailure: (error) =>
                          handleDepositOnlySubmissionFailure({
                            error,
                            headerHashBuffer,
                            expectedTailOutRef:
                              stateQueueOutRef(commitBaseTail),
                          }),
                        onSuccess: (txHash) =>
                          PendingBlockFinalizationsDB.markSubmitted(
                            headerHashBuffer,
                            Buffer.from(fromHex(txHash)),
                          ).pipe(
                            Effect.andThen(
                              submittedAwaitingConfirmationOutput(
                                txHash,
                                txSize,
                                blockEndTimeMs,
                                newHeaderHash,
                              ),
                            ),
                          ),
                      },
                    ),
                  ),
                );
              });
            }),
          ),
        ),
      );

    const handleDepositOnlySubmissionFailure = ({
      error,
      headerHashBuffer,
      expectedTailOutRef,
    }: {
      readonly error: unknown;
      readonly headerHashBuffer: Buffer;
      readonly expectedTailOutRef: string;
    }): Effect.Effect<
      WorkerOutput,
      StaleOperatorWalletRetrySignal,
      Database
    > =>
      error instanceof TxSubmitError &&
      isPotentiallyStaleOperatorWalletViewError(error)
        ? signalStaleOperatorWalletRetry({
            pendingHeaderHash: headerHashBuffer,
            error,
            label: "User-event-only commit submission",
          })
        : error instanceof TxSubmitError &&
            submitErrorReferencesOutRef(error, expectedTailOutRef)
          ? Effect.gen(function* () {
              yield* PendingBlockFinalizationsDB.markAbandoned(
                headerHashBuffer,
              ).pipe(Effect.catchAll(() => Effect.void));
              yield* Effect.logWarning(
                `🔹 User-event-only commit submission hit stale state-queue tail ${expectedTailOutRef}; the next worker tick will rebuild against the refreshed live tail.`,
              );
              return {
                type: "NothingToCommitOutput",
              } satisfies WorkerOutput;
            })
          : isStaleCommitBaseError(error)
            ? Effect.gen(function* () {
                yield* PendingBlockFinalizationsDB.markAbandoned(
                  headerHashBuffer,
                ).pipe(Effect.catchAll(() => Effect.void));
                yield* Effect.logWarning(
                  `🔹 User-event-only commit base ${expectedTailOutRef} became stale before submission; rolling back local roots for a rebuild on the next worker tick.`,
                );
                return {
                  type: "NothingToCommitOutput",
                } satisfies WorkerOutput;
              })
            : Effect.gen(function* () {
                yield* PendingBlockFinalizationsDB.markAbandoned(
                  headerHashBuffer,
                ).pipe(Effect.catchAll(() => Effect.void));
                const detail = formatUnknownError(error);
                yield* Effect.logError(
                  `🔹 User-event-only commit submission failed: ${detail}`,
                );
                return {
                  type: "FailureOutput",
                  error: `User-event-only commit submission failed: ${detail}`,
                } satisfies WorkerOutput;
              });

    return yield* runWithStaleOperatorWalletRetry({
      label: "User-event-only commit submission",
      attempt: submitCommitAttempt,
    });
  });

export const submitTxBackedCommit = ({
  contracts,
  consensusProfile,
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
  selectedBaseUtxosRoot,
  implicitGenesisEntries,
  transactionsMpf,
  processedMempoolTxs,
  mempoolTxHashes,
  mempoolTxSourceTable,
  workerInput,
  sizeOfProcessedTxs,
  blockEndTimeCapMs,
  beforePendingJournalInsert,
  nativeMpfReplay,
}: {
  readonly contracts: SDK.MidgardValidators;
  readonly consensusProfile: ContractDeploymentIdentityValue["consensusProfile"];
  readonly deploymentMarker: NonNullable<
    ContractDeploymentIdentityValue["deploymentMarker"]
  >;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedForcedTransactionEventIds: readonly Buffer[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly includedWithdrawalEventIds: readonly Buffer[];
  readonly utxoRoot: string;
  readonly txRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly transitionStepCount: number;
  readonly validationTraceCount: number;
  readonly utxoPayloadEntries: readonly UtxoPayloadEntry[];
  readonly ledgerDelta: LedgerDelta;
  readonly utxoPayloadAggregate: UtxoPayloadSizeAggregate;
  readonly selectedBaseUtxosRoot: string;
  readonly implicitGenesisEntries: readonly Ledger.MinimalEntry[];
  readonly nativeMpfReplay?: NativeMpfReplayBuild;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxSourceTable: string;
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
  readonly blockEndTimeCapMs?: number;
  readonly beforePendingJournalInsert?: (
    blockEndTimeMs: number,
  ) => Effect.Effect<void, DatabaseError, Database>;
}) =>
  Effect.gen(function* () {
    const emptyRoot = yield* emptyRootHexProgram;
    const [optDepositsRoot, optForcedTransactionsRoot, optWithdrawalsRoot] =
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
      );
    const depositsRoot = Option.isSome(optDepositsRoot)
      ? optDepositsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    const withdrawalsRoot = Option.isSome(optWithdrawalsRoot)
      ? optWithdrawalsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    const forcedTransactionsRoot = Option.isSome(optForcedTransactionsRoot)
      ? optForcedTransactionsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    const currentBlockMempoolTxsCount = processedMempoolTxs.length;
    if (currentBlockMempoolTxsCount <= 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit a tx-backed commit with an empty pending tx journal",
          cause: `tx_root=${txRoot},deposits=${includedDepositEventIds.length},withdrawals=${includedWithdrawalEventIds.length}`,
        }),
      );
    }
    yield* assertPendingJournalCompleteness({
      utxoRoot,
      utxoMemberCount: utxoPayloadEntries.length,
      hasLedgerDelta: true,
      txRoot,
      emptyTxRoot: emptyRoot,
      txMemberCount: currentBlockMempoolTxsCount,
      depositsRoot,
      depositMemberCount: includedDepositEventIds.length,
      forcedTransactionsRoot,
      forcedTransactionMemberCount: includedForcedTransactionEventIds.length,
      withdrawalsRoot,
      withdrawalMemberCount: includedWithdrawalEventIds.length,
      transitionTraceRoot,
      transitionTraceMemberCount: transitionTraceMembers.length,
      eventToStepRoot,
      eventToStepMemberCount: eventToStepMembers.length,
      validationTracesRoot,
      validationTraceMemberCount: validationTraceMembers.length,
      expectedValidationTraceCount: validationTraceCount,
    });
    const transitionCommitments = yield* makeEventCommitments(
      {
        withdrawalsRoot,
        forcedTransactionsRoot,
        transactionsRoot: txRoot,
        depositsRoot,
        transitionTraceRoot,
        eventToStepRoot,
      },
      {
        withdrawalCount: BigInt(includedWithdrawalEventIds.length),
        forcedTransactionCount: BigInt(
          includedForcedTransactionEventIds.length,
        ),
        l2TransactionCount: BigInt(currentBlockMempoolTxsCount),
        depositCount: BigInt(includedDepositEventIds.length),
        transitionStepCount: BigInt(transitionStepCount),
      },
      {
        validationTracesRoot,
        validationTraceCount: BigInt(validationTraceCount),
      },
    );
    const submittedAwaitingConfirmationOutput = (
      submittedTxHash: string,
      txSize: number,
      blockEndTimeMs: number,
      submittedHeaderHash: string,
    ) =>
      Effect.succeed({
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash,
        txSize,
        mempoolTxsCount:
          currentBlockMempoolTxsCount + workerInput.data.mempoolTxsCountSoFar,
        sizeOfBlocksTxs:
          sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
        blockEndTimeMs,
        submittedHeaderHash,
        submittedUtxosRoot: utxoRoot,
      } satisfies WorkerOutput);
    const selectedPayloadAlreadyDeferred =
      mempoolTxSourceTable === ProcessedMempoolDB.tableName;
    const newlyDeferredTxsCount = selectedPayloadAlreadyDeferred
      ? 0
      : currentBlockMempoolTxsCount;
    const newlyDeferredTxsSize = selectedPayloadAlreadyDeferred
      ? 0
      : sizeOfProcessedTxs;
    const preserveTxPayloadForRetryAfterSubmitFailure = (
      error: TxSubmitError,
    ): Effect.Effect<WorkerOutput, never, Database> =>
      Effect.gen(function* () {
        if (selectedPayloadAlreadyDeferred) {
          yield* Effect.logWarning(
            "🔹 Commit submission failed while the selected tx payload was already in ProcessedMempoolDB; preserving durable retry rows without another transfer.",
          );
          return yield* failedSubmissionProgram(
            transactionsMpf,
            newlyDeferredTxsCount,
            newlyDeferredTxsSize,
            error,
          );
        }

        const transferResult = yield* Effect.either(
          skippedSubmissionProgram(processedMempoolTxs, mempoolTxHashes),
        );
        if (transferResult._tag === "Left") {
          const detail = formatUnknownError(transferResult.left);
          yield* Effect.logError(
            `🔹 Commit submission failed and deferred transfer failed: submit=${formatUnknownError(
              error,
            )}; transfer=${detail}`,
          );
          return {
            type: "FailureOutput",
            error: `Commit submission failed and deferred transfer failed: submit=${formatUnknownError(
              error,
            )}; transfer=${detail}`,
          } satisfies WorkerOutput;
        }

        return yield* failedSubmissionProgram(
          transactionsMpf,
          newlyDeferredTxsCount,
          newlyDeferredTxsSize,
          error,
        );
      });

    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    yield* Effect.logInfo(
      `🔹 Forced transactions root is: ${forcedTransactionsRoot}`,
    );
    yield* Effect.logInfo(`🔹 Withdrawals root is: ${withdrawalsRoot}`);

    const submitCommitAttempt = (
      initialOperatorWalletView?: OperatorWalletView,
      previousPendingHeaderHash?: Buffer,
    ) =>
      revalidateStateQueueLease(workerInput).pipe(
        Effect.andThen(
          resolveLiveTailCommitBase(contracts, latestBlock, consensusProfile),
        ),
        Effect.flatMap((commitBaseTail) =>
          buildUnsignedCommitTx(
            contracts,
            commitBaseTail,
            utxoRoot,
            txRoot,
            depositsRoot,
            withdrawalsRoot,
            transitionCommitments,
            consensusProfile,
            endTime,
            initialOperatorWalletView,
            blockEndTimeCapMs,
          ).pipe(
            Effect.flatMap((buildResult) => {
              if ("dueWork" in buildResult) {
                const output: WorkerOutput = {
                  type: "RegisteredDueWorkOutput",
                  dueWork: buildResult.dueWork,
                };
                return Effect.succeed(output);
              }
              const {
                newHeaderHash,
                newHeader,
                newHeaderCbor,
                blockEndTimeMs,
                signAndSubmitProgram,
                txSize,
              } = buildResult;
              return Effect.gen(function* () {
                yield* refreshCommitUserEventSourcesThroughBlockEnd(
                  blockEndTimeMs,
                );
                const headerHashBuffer = Buffer.from(fromHex(newHeaderHash));
                const mempoolTxProgramMaterialSidecars =
                  yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(
                    processedMempoolTxs.map((entry) => entry[TxColumns.TX_ID]),
                  );
                if (
                  mempoolTxProgramMaterialSidecars.length !==
                  processedMempoolTxs.length
                ) {
                  return yield* Effect.fail(
                    new DatabaseError({
                      table: TxAdmissionsDB.payloadTableName,
                      message:
                        "Cannot build V1 block without one durable program-material sidecar per normal transaction",
                      cause: `transactions=${processedMempoolTxs.length.toString()},sidecars=${mempoolTxProgramMaterialSidecars.length.toString()}`,
                    }),
                  );
                }
                const cekProgramMaterial = yield* Effect.try({
                  try: () =>
                    daProgramMaterialFromSidecars([
                      ...mempoolTxProgramMaterialSidecars.map(
                        (entry) => entry.sidecarCbor,
                      ),
                      ...forcedProgramMaterialSidecars(
                        includedForcedTransactionEntries,
                      ),
                    ]),
                  catch: (cause) =>
                    new DatabaseError({
                      table: TxAdmissionsDB.payloadTableName,
                      message:
                        "Cannot build V1 block from conflicting program-material sidecars",
                      cause,
                    }),
                });
                yield* assertPreSubmitDaPayloadSize({
                  headerHash: newHeaderHash,
                  header: newHeader,
                  utxoPayloadAggregate,
                  includedDepositEntries,
                  includedForcedTransactionEntries,
                  includedWithdrawalEntries,
                  processedMempoolTxs,
                  transitionTraceMembers,
                  eventToStepMembers,
                  validationTraceMembers,
                  cekProgramMaterial,
                });
                yield* maybeAbandonPreviousStaleAttempt(
                  previousPendingHeaderHash,
                  headerHashBuffer,
                );
                yield* MpfEngineStateDB.stampLedgerPayloadAggregate({
                  rootHex: utxoRoot,
                  aggregate: utxoPayloadAggregate,
                });
                yield* assertCommitInputsWithinBlockEndTime({
                  blockEndTimeMs,
                  processedMempoolTxs,
                  includedDepositEntries,
                  includedForcedTransactionEntries,
                  includedWithdrawalEntries,
                });
                const metadata = yield* buildPendingJournalMetadata({
                  latestBlock: commitBaseTail,
                  workerInput,
                  blockEndTimeMs,
                  expectedRoots: {
                    utxosRoot: utxoRoot,
                    forcedTransactionsRoot,
                    transactionsRoot: txRoot,
                    depositsRoot,
                    withdrawalsRoot,
                    transitionTraceRoot:
                      transitionCommitments.transitionTraceRoot,
                    eventToStepRoot: transitionCommitments.eventToStepRoot,
                    validationTracesRoot,
                  },
                  expectedCounts: {
                    withdrawalCount: transitionCommitments.withdrawalCount,
                    forcedTransactionCount:
                      transitionCommitments.forcedTransactionCount,
                    l2TransactionCount:
                      transitionCommitments.l2TransactionCount,
                    depositCount: transitionCommitments.depositCount,
                    totalEventCount: transitionCommitments.totalEventCount,
                    transitionStepCount:
                      transitionCommitments.transitionStepCount,
                    validationTraceCount: BigInt(validationTraceCount),
                  },
                  consensusProfile,
                  deploymentMarker,
                });
                const journalLedgerState =
                  yield* resolvePendingJournalLedgerState({
                    recordedBaseUtxosRoot: metadata.baseRoots.utxosRoot,
                    selectedBaseUtxosRoot,
                    expectedFinalUtxosRoot: utxoRoot,
                    expectedFinalEntryCount: utxoPayloadAggregate.entryCount,
                    implicitGenesisEntries,
                    transitionDelta: ledgerDelta,
                  });
                const beforeJournalInsert =
                  beforePendingJournalInsert?.(blockEndTimeMs) ??
                  assertCommitUserEventSourceCompleteness({
                    blockEndTimeMs,
                    includedDepositEntries,
                    includedForcedTransactionEntries,
                    includedWithdrawalEntries,
                  });
                return yield* PendingBlockFinalizationsDB.preparePendingSubmission(
                  {
                    headerHash: headerHashBuffer,
                    headerCbor: newHeaderCbor,
                    metadata,
                    blockEndTime: new Date(blockEndTimeMs),
                    depositEventIds: includedDepositEventIds,
                    depositEntries: includedDepositEntries,
                    forcedTransactionEventIds:
                      includedForcedTransactionEventIds,
                    forcedTransactionEntries: includedForcedTransactionEntries,
                    withdrawalEventIds: includedWithdrawalEventIds,
                    withdrawalEntries: includedWithdrawalEntries,
                    mempoolTxIds: processedMempoolTxs.map(
                      (entry) => entry[TxColumns.TX_ID],
                    ),
                    mempoolTxs: processedMempoolTxs,
                    mempoolTxProgramMaterialSidecars,
                    mempoolTxSourceTable,
                    transitionTraceMembers,
                    eventToStepMembers,
                    validationTraceMembers,
                    validationTraceWitnessMembers:
                      validationTraceMembers.flatMap((entry) =>
                        entry.witnesses.map(([key, value]) => ({
                          keyCbor: Buffer.from(key, "hex"),
                          valueCbor: Buffer.from(value, "hex"),
                        })),
                      ),
                    ledgerDelta: {
                      spent: journalLedgerState.ledgerDelta.spent,
                      produced: journalUtxoEntries(
                        journalLedgerState.ledgerDelta.produced,
                      ),
                    },
                    utxoPayloadAggregate,
                    nativeMpfReplay,
                  },
                  { beforeJournalInsert },
                ).pipe(
                  Effect.andThen(
                    reachPipelinedCommitCrashCheckpoint(
                      "journal_prepared_before_submit",
                    ),
                  ),
                  Effect.andThen(
                    Effect.matchEffect(
                      revalidateStateQueueLease(workerInput).pipe(
                        Effect.andThen(
                          assertLiveTailCommitBase(contracts, commitBaseTail),
                        ),
                        Effect.andThen(signAndSubmitProgram),
                      ),
                      {
                        onFailure: (error) => {
                          if (error instanceof TxSignError) {
                            return Effect.gen(function* () {
                              yield* PendingBlockFinalizationsDB.markAbandoned(
                                headerHashBuffer,
                              ).pipe(Effect.catchAll(() => Effect.void));
                              const detail = formatUnknownError(error);
                              yield* Effect.logError(
                                `🔹 Commit signing failed: ${detail}`,
                              );
                              return {
                                type: "FailureOutput",
                                error: `Commit signing failed: ${detail}`,
                              } satisfies WorkerOutput;
                            });
                          }

                          return Effect.gen(function* () {
                            if (
                              error instanceof TxSubmitError &&
                              isPotentiallyStaleOperatorWalletViewError(error)
                            ) {
                              return yield* signalStaleOperatorWalletRetry({
                                pendingHeaderHash: headerHashBuffer,
                                error,
                                label: "Tx-backed commit submission",
                              });
                            }

                            if (
                              error instanceof TxSubmitError &&
                              submitErrorReferencesOutRef(
                                error,
                                stateQueueOutRef(commitBaseTail),
                              )
                            ) {
                              yield* PendingBlockFinalizationsDB.markAbandoned(
                                headerHashBuffer,
                              ).pipe(Effect.catchAll(() => Effect.void));
                              yield* Effect.logWarning(
                                `🔹 Tx-backed commit submission hit stale state-queue tail ${stateQueueOutRef(
                                  commitBaseTail,
                                )}; preserving tx payload for rebuild against the refreshed live tail.`,
                              );
                              return yield* preserveTxPayloadForRetryAfterSubmitFailure(
                                error,
                              );
                            }

                            if (!(error instanceof TxSubmitError)) {
                              yield* PendingBlockFinalizationsDB.markAbandoned(
                                headerHashBuffer,
                              ).pipe(Effect.catchAll(() => Effect.void));
                              if (isStaleCommitBaseError(error)) {
                                yield* Effect.logWarning(
                                  `🔹 Tx-backed commit base ${stateQueueOutRef(
                                    commitBaseTail,
                                  )} became stale before submission; rolling back local roots for a rebuild on the next worker tick.`,
                                );
                                return {
                                  type: "NothingToCommitOutput",
                                } satisfies WorkerOutput;
                              }
                              const detail = formatUnknownError(error);
                              yield* Effect.logError(
                                `🔹 Commit aborted before submission: ${detail}`,
                              );
                              return {
                                type: "FailureOutput",
                                error: `Commit aborted before submission: ${detail}`,
                              } satisfies WorkerOutput;
                            }

                            const recoveredTxHash =
                              yield* recoverSubmittedTxHashByHeaderProgram(
                                contracts.stateQueue,
                                newHeaderHash,
                              );
                            if (Option.isSome(recoveredTxHash)) {
                              return yield* PendingBlockFinalizationsDB.markSubmitted(
                                headerHashBuffer,
                                Buffer.from(fromHex(recoveredTxHash.value)),
                              ).pipe(
                                Effect.andThen(
                                  submittedAwaitingConfirmationOutput(
                                    recoveredTxHash.value,
                                    txSize,
                                    blockEndTimeMs,
                                    newHeaderHash,
                                  ),
                                ),
                              );
                            }

                            yield* PendingBlockFinalizationsDB.markAbandoned(
                              headerHashBuffer,
                            ).pipe(Effect.catchAll(() => Effect.void));
                            return yield* preserveTxPayloadForRetryAfterSubmitFailure(
                              error,
                            );
                          });
                        },
                        onSuccess: (txHash) =>
                          PendingBlockFinalizationsDB.markSubmitted(
                            headerHashBuffer,
                            Buffer.from(fromHex(txHash)),
                          ).pipe(
                            Effect.andThen(
                              submittedAwaitingConfirmationOutput(
                                txHash,
                                txSize,
                                blockEndTimeMs,
                                newHeaderHash,
                              ),
                            ),
                          ),
                      },
                    ),
                  ),
                );
              });
            }),
          ),
        ),
      );

    return yield* runWithStaleOperatorWalletRetry({
      label: "Tx-backed commit submission",
      attempt: submitCommitAttempt,
    });
  });

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

export const recoverLocalFinalizationAgainstConfirmedBlock = ({
  latestBlock,
  transactionsMpf,
  processedMempoolTxs,
  mempoolTxHashes,
  workerInput,
  sizeOfProcessedTxs,
  beforeTransactionsMpfReset,
  consensusProfile,
}: {
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
  readonly beforeTransactionsMpfReset?: Effect.Effect<
    void,
    DatabaseError,
    Database
  >;
  readonly consensusProfile: ContractDeploymentIdentityValue["consensusProfile"];
}): Effect.Effect<WorkerOutput, unknown, Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 Attempting local finalization recovery against confirmed block roots...",
    );
    if (latestBlock.datum.key === "Empty") {
      return {
        type: "FailureOutput",
        error:
          "Confirmed block datum does not contain a recoverable header for local finalization",
      } satisfies WorkerOutput;
    }
    const confirmedHeader = yield* getHeaderFromStateQueueDatumLocal(
      latestBlock.datum,
    );
    const confirmedHeaderHash = yield* hashBlockHeaderLocal(confirmedHeader);
    const pendingRecord =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(fromHex(confirmedHeaderHash)),
      );
    if (Option.isNone(pendingRecord)) {
      return {
        type: "FailureOutput",
        error:
          "Local finalization recovery aborted: no durable pending journal exists for the confirmed block",
      } satisfies WorkerOutput;
    }
    const record = pendingRecord.value;
    const rootsMatch =
      record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID] ===
        consensusProfile.profileId &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT] ===
        confirmedHeader.utxosRoot &&
      record[
        PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT
      ] === confirmedHeader.forcedTransactionsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT] ===
        confirmedHeader.transactionsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT] ===
        confirmedHeader.depositsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT] ===
        confirmedHeader.withdrawalsRoot &&
      record[
        PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT
      ] === confirmedHeader.validationTracesRoot &&
      record[
        PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT
      ] === confirmedHeader.validationTraceCount;
    if (!rootsMatch) {
      return {
        type: "FailureOutput",
        error:
          "Local finalization recovery aborted: journal expected roots do not match the confirmed block header",
      } satisfies WorkerOutput;
    }
    return yield* successfulLocalFinalizationRecoveryProgram(
      transactionsMpf,
      processedMempoolTxs,
      mempoolTxHashes,
      confirmedHeaderHash,
      workerInput,
      sizeOfProcessedTxs,
      beforeTransactionsMpfReset,
    );
  });
