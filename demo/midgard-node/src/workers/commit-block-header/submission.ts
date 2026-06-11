import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Data, Effect, Option } from "effect";

import {
  DepositsDB,
  PendingBlockFinalizationsDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  fetchOperatorWalletView,
  isPotentiallyStaleOperatorWalletViewError,
  type OperatorWalletView,
} from "@/operator-wallet-view.js";
import type { Database } from "@/services/index.js";
import { Lucid } from "@/services/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { selectCommitRoots } from "@/workers/utils/commit-block-planner.js";
import {
  failedSubmissionProgram,
  recoverSubmittedTxHashByHeaderProgram,
  skippedSubmissionProgram,
  successfulLocalFinalizationRecoveryProgram,
} from "@/workers/utils/commit-submission.js";
import { emptyRootHexProgram, type MidgardMpf } from "@/workers/utils/mpf.js";

import { buildUnsignedCommitTx } from "./build-unsigned-tx.js";
import { resolveDepositsRoot, resolveWithdrawalsRoot } from "./event-roots.js";
import {
  assertLiveTailCommitBase,
  assertPendingJournalCompleteness,
  buildPendingJournalMetadata,
  revalidateStateQueueLease,
} from "./pending-journal.js";
import {
  getHeaderFromStateQueueDatumLocal,
  hashBlockHeaderLocal,
} from "./state-queue.js";

const COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES = 1;

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
    : PendingBlockFinalizationsDB.markAbandoned(previousPendingHeaderHash);

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
    return yield* Effect.fail(
      new StaleOperatorWalletRetrySignal({
        pendingHeaderHash,
        txSubmitError: error,
      }),
    );
  });

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
  latestBlock,
  endTime,
  includedDepositEntries,
  includedDepositEventIds,
  includedWithdrawalEntries,
  includedWithdrawalEventIds,
  workerInput,
  utxoRoot,
  txRoot,
}: {
  readonly contracts: SDK.MidgardValidators;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly includedWithdrawalEventIds: readonly Buffer[];
  readonly workerInput: WorkerInput;
  readonly utxoRoot: string;
  readonly txRoot: string;
}) =>
  Effect.gen(function* () {
    const optDepositsRoot = yield* resolveDepositsRoot(includedDepositEntries);
    const optWithdrawalsRoot = yield* resolveWithdrawalsRoot(
      includedWithdrawalEntries,
    );
    if (Option.isNone(optDepositsRoot) && Option.isNone(optWithdrawalsRoot)) {
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
    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    yield* Effect.logInfo(`🔹 Withdrawals root is: ${withdrawalsRoot}`);
    const submittedAwaitingConfirmationOutput = (
      submittedTxHash: string,
      txSize: number,
      blockEndTimeMs: number,
    ) =>
      Effect.succeed({
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash,
        txSize,
        mempoolTxsCount: workerInput.data.mempoolTxsCountSoFar,
        sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
        blockEndTimeMs,
      } satisfies WorkerOutput);
    const roots = selectCommitRoots({
      hasTxRequests: false,
      computedUtxoRoot: utxoRoot,
      computedTxRoot: txRoot,
      emptyRoot,
    });
    yield* assertPendingJournalCompleteness({
      txRoot: roots.txRoot,
      emptyTxRoot: emptyRoot,
      txMemberCount: 0,
      depositsRoot,
      depositMemberCount: includedDepositEventIds.length,
      withdrawalsRoot,
      withdrawalMemberCount: includedWithdrawalEventIds.length,
    });

    const submitCommitAttempt = (
      initialOperatorWalletView?: OperatorWalletView,
      previousPendingHeaderHash?: Buffer,
    ) =>
      revalidateStateQueueLease(workerInput).pipe(
        Effect.andThen(assertLiveTailCommitBase(contracts, latestBlock)),
        Effect.andThen(
          buildUnsignedCommitTx(
            contracts,
            latestBlock,
            roots.utxoRoot,
            roots.txRoot,
            depositsRoot,
            withdrawalsRoot,
            endTime,
            initialOperatorWalletView,
          ),
        ),
        Effect.flatMap(
          ({ newHeaderHash, blockEndTimeMs, signAndSubmitProgram, txSize }) =>
            Effect.gen(function* () {
              const headerHashBuffer = Buffer.from(fromHex(newHeaderHash));
              yield* maybeAbandonPreviousStaleAttempt(
                previousPendingHeaderHash,
                headerHashBuffer,
              );
              return yield* PendingBlockFinalizationsDB.preparePendingSubmission(
                {
                  headerHash: headerHashBuffer,
                  metadata: yield* buildPendingJournalMetadata({
                    latestBlock,
                    workerInput,
                    blockEndTimeMs,
                    expectedRoots: {
                      utxosRoot: roots.utxoRoot,
                      transactionsRoot: roots.txRoot,
                      depositsRoot,
                      withdrawalsRoot,
                    },
                  }),
                  blockEndTime: new Date(blockEndTimeMs),
                  depositEventIds: includedDepositEventIds,
                  depositEntries: includedDepositEntries,
                  withdrawalEventIds: includedWithdrawalEventIds,
                  withdrawalEntries: includedWithdrawalEntries,
                  mempoolTxIds: [],
                  mempoolTxs: [],
                  mempoolTxSourceTable: "none",
                },
              ).pipe(
                Effect.andThen(
                  Effect.matchEffect(
                    revalidateStateQueueLease(workerInput).pipe(
                      Effect.andThen(
                        assertLiveTailCommitBase(contracts, latestBlock),
                      ),
                      Effect.andThen(signAndSubmitProgram),
                    ),
                    {
                      onFailure: (error) =>
                        handleDepositOnlySubmissionFailure({
                          error,
                          headerHashBuffer,
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
                            ),
                          ),
                        ),
                    },
                  ),
                ),
              );
            }),
        ),
      );

    const handleDepositOnlySubmissionFailure = ({
      error,
      headerHashBuffer,
    }: {
      readonly error: unknown;
      readonly headerHashBuffer: Buffer;
    }) =>
      error instanceof TxSubmitError &&
      isPotentiallyStaleOperatorWalletViewError(error)
        ? signalStaleOperatorWalletRetry({
            pendingHeaderHash: headerHashBuffer,
            error,
            label: "User-event-only commit submission",
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
  latestBlock,
  endTime,
  includedDepositEntries,
  includedDepositEventIds,
  includedWithdrawalEntries,
  includedWithdrawalEventIds,
  utxoRoot,
  txRoot,
  transactionsMpf,
  processedMempoolTxs,
  mempoolTxHashes,
  mempoolTxSourceTable,
  workerInput,
  sizeOfProcessedTxs,
}: {
  readonly contracts: SDK.MidgardValidators;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly includedWithdrawalEventIds: readonly Buffer[];
  readonly utxoRoot: string;
  readonly txRoot: string;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxSourceTable: string;
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
}) =>
  Effect.gen(function* () {
    const emptyRoot = yield* emptyRootHexProgram;
    const optDepositsRoot = yield* resolveDepositsRoot(includedDepositEntries);
    const optWithdrawalsRoot = yield* resolveWithdrawalsRoot(
      includedWithdrawalEntries,
    );
    const depositsRoot = Option.isSome(optDepositsRoot)
      ? optDepositsRoot.value
      : SDK.EMPTY_MERKLE_TREE_ROOT;
    const withdrawalsRoot = Option.isSome(optWithdrawalsRoot)
      ? optWithdrawalsRoot.value
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
      txRoot,
      emptyTxRoot: emptyRoot,
      txMemberCount: currentBlockMempoolTxsCount,
      depositsRoot,
      depositMemberCount: includedDepositEventIds.length,
      withdrawalsRoot,
      withdrawalMemberCount: includedWithdrawalEventIds.length,
    });
    const submittedAwaitingConfirmationOutput = (
      submittedTxHash: string,
      txSize: number,
      blockEndTimeMs: number,
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
      } satisfies WorkerOutput);

    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    yield* Effect.logInfo(`🔹 Withdrawals root is: ${withdrawalsRoot}`);

    const submitCommitAttempt = (
      initialOperatorWalletView?: OperatorWalletView,
      previousPendingHeaderHash?: Buffer,
    ) =>
      revalidateStateQueueLease(workerInput).pipe(
        Effect.andThen(assertLiveTailCommitBase(contracts, latestBlock)),
        Effect.andThen(
          buildUnsignedCommitTx(
            contracts,
            latestBlock,
            utxoRoot,
            txRoot,
            depositsRoot,
            withdrawalsRoot,
            endTime,
            initialOperatorWalletView,
          ),
        ),
        Effect.flatMap(
          ({ newHeaderHash, blockEndTimeMs, signAndSubmitProgram, txSize }) =>
            Effect.gen(function* () {
              const headerHashBuffer = Buffer.from(fromHex(newHeaderHash));
              yield* maybeAbandonPreviousStaleAttempt(
                previousPendingHeaderHash,
                headerHashBuffer,
              );
              return yield* PendingBlockFinalizationsDB.preparePendingSubmission(
                {
                  headerHash: headerHashBuffer,
                  metadata: yield* buildPendingJournalMetadata({
                    latestBlock,
                    workerInput,
                    blockEndTimeMs,
                    expectedRoots: {
                      utxosRoot: utxoRoot,
                      transactionsRoot: txRoot,
                      depositsRoot,
                      withdrawalsRoot,
                    },
                  }),
                  blockEndTime: new Date(blockEndTimeMs),
                  depositEventIds: includedDepositEventIds,
                  depositEntries: includedDepositEntries,
                  withdrawalEventIds: includedWithdrawalEventIds,
                  withdrawalEntries: includedWithdrawalEntries,
                  mempoolTxIds: processedMempoolTxs.map(
                    (entry) => entry[TxColumns.TX_ID],
                  ),
                  mempoolTxs: processedMempoolTxs,
                  mempoolTxSourceTable,
                },
              ).pipe(
                Effect.andThen(
                  Effect.matchEffect(
                    revalidateStateQueueLease(workerInput).pipe(
                      Effect.andThen(
                        assertLiveTailCommitBase(contracts, latestBlock),
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

                          if (!(error instanceof TxSubmitError)) {
                            yield* PendingBlockFinalizationsDB.markAbandoned(
                              headerHashBuffer,
                            ).pipe(Effect.catchAll(() => Effect.void));
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
                                ),
                              ),
                            );
                          }

                          yield* PendingBlockFinalizationsDB.markAbandoned(
                            headerHashBuffer,
                          ).pipe(Effect.catchAll(() => Effect.void));
                          const transferResult = yield* Effect.either(
                            skippedSubmissionProgram(
                              processedMempoolTxs,
                              mempoolTxHashes,
                            ),
                          );
                          if (transferResult._tag === "Left") {
                            const detail = formatUnknownError(
                              transferResult.left,
                            );
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
                            currentBlockMempoolTxsCount,
                            sizeOfProcessedTxs,
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
                            ),
                          ),
                        ),
                    },
                  ),
                ),
              );
            }),
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
}: {
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
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
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT] ===
        confirmedHeader.utxosRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT] ===
        confirmedHeader.transactionsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT] ===
        confirmedHeader.depositsRoot &&
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT] ===
        confirmedHeader.withdrawalsRoot;
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
    );
  });
