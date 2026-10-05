import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import type * as Ledger from "../../database/utils/ledger.js";
import { reachPipelinedCommitCrashCheckpoint } from "../../e2e/pipelined-commit-crash-checkpoint.js";
import {
  emptyRootHexProgram,
  type LedgerDelta,
  type NativeMpfReplayBuild,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type RetainedValidationTraceMember,
  type UtxoPayloadEntry,
  type UtxoPayloadSizeAggregate,
} from "../../mpf/index.js";
import {
  isPotentiallyStaleOperatorWalletViewError,
  type OperatorWalletView,
} from "../../operator-wallet-view.js";
import {
  type ContractDeploymentIdentityValue,
  Database,
} from "../../services/index.js";
import { TxSubmitError } from "../../transactions/utils.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "../utils/commit-block-header.js";
import { selectCommitRoots } from "../utils/commit-block-planner.js";
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
  maybeAbandonPreviousStaleAttempt,
  StaleOperatorWalletRetrySignal,
} from "./submission.assert-pre-submit-da-payload-size.js";
import {
  assertCommitUserEventSourceCompleteness,
  isStaleCommitBaseError,
  journalUtxoEntries,
  refreshCommitUserEventSourcesThroughBlockEnd,
  runWithStaleOperatorWalletRetry,
  signalStaleOperatorWalletRetry,
  submitErrorReferencesOutRef,
} from "./submission.run-with-stale-operator-wallet-retry.js";
import {
  retainedIntentFailure,
  submitWithDurableIntent,
} from "./submission.submit-with-durable-intent.js";
import { type CommitSubmissionHooks } from "./submission-hooks.js";
import { makeEventCommitments } from "./transition-commitments.js";

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
  afterPendingJournalPrepared,
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
  readonly nativeMpfReplay: NativeMpfReplayBuild;
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
                yield* afterDaFrameAccepted ?? Effect.void;
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
                    preparedTxHash: Buffer.from(preparedTxHash, "hex"),
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
                  Effect.tap(() => afterPendingJournalPrepared ?? Effect.void),
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
                            return yield* handleDepositOnlySubmissionFailure({
                              error,
                              headerHashBuffer,
                              expectedTailOutRef:
                                stateQueueOutRef(commitBaseTail),
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
