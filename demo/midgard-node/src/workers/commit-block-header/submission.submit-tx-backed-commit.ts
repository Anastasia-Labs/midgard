import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

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
import { DatabaseError } from "../../database/utils/common.js";
import type * as Ledger from "../../database/utils/ledger.js";
import { Columns as TxColumns } from "../../database/utils/tx.js";
import { reachCommitCrashCheckpoint } from "../../e2e/commit-crash-checkpoint.js";
import {
  emptyRootHexProgram,
  type LedgerDelta,
  type MidgardMpf,
  type NativeMpfReplayBuild,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type RetainedValidationTraceMember,
  type UtxoPayloadEntry,
  type UtxoPayloadSizeAggregate,
} from "../../mpf/index.js";
import { configuredCommitHorizonLag } from "../../services/history-commit-window.js";
import {
  type ContractDeploymentIdentityValue,
  Database,
} from "../../services/index.js";
import { TxSignError, TxSubmitError } from "../../transactions/utils.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "../utils/commit-block-header.js";
import {
  failedSubmissionProgram,
  recoverSubmittedTxHashByHeaderProgram,
  skippedSubmissionProgram,
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
import { stateQueueOutRef } from "./state-queue.js";
import {
  assertCommitInputsWithinBlockEndTime,
  assertPreSubmitDaPayloadSize,
  daProgramMaterialFromSidecars,
  forcedProgramMaterialSidecars,
} from "./submission.assert-pre-submit-da-payload-size.js";
import {
  assertCommitUserEventSourceCompleteness,
  isStaleCommitBaseError,
  journalUtxoEntries,
  refreshCommitUserEventSourcesThroughBlockEnd,
  submitErrorReferencesOutRef,
} from "./submission.commit-event-sources.js";
import {
  awaitNextCommitWindow,
  retainedIntentFailure,
  submitWithDurableIntent,
} from "./submission.submit-with-durable-intent.js";
import { type CommitSubmissionHooks } from "./submission-hooks.js";
import { makeEventCommitments } from "./transition-commitments.js";

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
  afterDaFrameAccepted,
  nativeMpfReplay,
}: CommitSubmissionHooks & {
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
  readonly nativeMpfReplay: NativeMpfReplayBuild;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxSourceTable: string;
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
  readonly blockEndTimeCapMs?: number;
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

    const submitCommitAttempt = () =>
      revalidateStateQueueLease(workerInput).pipe(
        Effect.zipRight(
          PendingBlockFinalizationsDB.assertNoUnreconciledSignedSubmission,
        ),
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
                preparedTxHash,
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
                  yield* configuredCommitHorizonLag,
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
                yield* afterDaFrameAccepted ?? Effect.void;
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
                  assertCommitUserEventSourceCompleteness({
                    blockEndTimeMs,
                    lagBlocks: (yield* configuredCommitHorizonLag).lagBlocks,
                    includedDepositEntries,
                    includedForcedTransactionEntries,
                    includedWithdrawalEntries,
                  });
                const prepared =
                  yield* PendingBlockFinalizationsDB.preparePendingSubmission(
                    {
                      headerHash: headerHashBuffer,
                      preparedTxHash: Buffer.from(preparedTxHash, "hex"),
                      headerCbor: newHeaderCbor,
                      metadata,
                      blockEndTime: new Date(blockEndTimeMs),
                      depositEventIds: includedDepositEventIds,
                      depositEntries: includedDepositEntries,
                      forcedTransactionEventIds:
                        includedForcedTransactionEventIds,
                      forcedTransactionEntries:
                        includedForcedTransactionEntries,
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
                  );
                if (prepared.kind === "held")
                  return yield* awaitNextCommitWindow(prepared.heldHeaderHash);
                yield* reachCommitCrashCheckpoint(
                  "journal_prepared_before_submit",
                );
                return yield* Effect.matchEffect(
                  revalidateStateQueueLease(workerInput).pipe(
                    Effect.andThen(
                      assertLiveTailCommitBase(contracts, commitBaseTail),
                    ),
                    Effect.andThen(
                      submitWithDurableIntent(
                        headerHashBuffer,
                        signAndSubmitProgram,
                      ),
                    ),
                  ),
                  {
                    onFailure: (error) =>
                      Effect.gen(function* () {
                        const retained = yield* retainedIntentFailure(
                          headerHashBuffer,
                          error,
                        );
                        if (retained !== undefined) return retained;
                        if (error instanceof TxSignError) {
                          return yield* Effect.gen(function* () {
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

                        return yield* Effect.gen(function* () {
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
                );
              });
            }),
          ),
        ),
      );

    return yield* submitCommitAttempt();
  });
