import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Data, Effect, Match, Option, pipe } from "effect";
import {
  WorkerInput,
  WorkerOutput,
  deserializeStateQueueUTxO,
} from "@/workers/utils/commit-block-header.js";
import {
  ConfigError,
  Database,
  Lucid,
  AlwaysSucceedsContract,
  NodeConfig,
  DatabaseInitializationError,
} from "@/services/index.js";
import {
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  ProcessedMempoolDB,
  DepositsDB,
  TxUtils as TxTable,
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  LucidEvolution,
  fromHex,
} from "@lucid-evolution/lucid";
import {
  MidgardMpt,
  MptError,
  emptyRootHexProgram,
  keyValueMptRoot,
  makeMpts,
  processMpts,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import { FileSystemError, batchProgram } from "@/utils.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import {
  buildSuccessfulCommitBatches,
  rootsMatchConfirmedHeader,
  shouldAttemptLocalFinalizationRecovery,
  shouldDeferCommitSubmission,
  selectCommitRoots,
} from "@/workers/utils/commit-block-planner.js";
import {
  Columns as UserEventsColumns,
  retrieveTimeBoundEntries,
} from "@/database/utils/user-events.js";
import { DatabaseError } from "@/database/utils/common.js";

const BATCH_SIZE = 100;

class LocalFinalizationPendingError extends Data.TaggedError(
  "LocalFinalizationPendingError",
)<{
  readonly submittedTxHash: string;
  readonly txSize: number;
  readonly mempoolTxsCount: number;
  readonly sizeOfBlocksTxs: number;
  readonly cause: unknown;
}> {}

const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    return `${error.name}: ${error.message}`;
  }
  if (typeof error === "string") {
    return error;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};

const getLatestBlockDatumEndTime = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<Date, SDK.DataCoercionError, never> =>
  Effect.gen(function* () {
    let endTimeBigInt: bigint;
    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* SDK.getConfirmedStateFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = confirmedState.endTime;
    } else {
      const latestHeader =
        yield* SDK.getHeaderFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = latestHeader.endTime;
    }
    return new Date(Number(endTimeBigInt));
  });

const getLatestBlockHeaderRoots = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<
  Option.Option<{ readonly utxoRoot: string; readonly txRoot: string }>,
  SDK.DataCoercionError,
  never
> =>
  Effect.gen(function* () {
    if (latestBlocksDatum.key === "Empty") {
      return Option.none();
    }
    const latestHeader =
      yield* SDK.getHeaderFromStateQueueDatum(latestBlocksDatum);
    return Option.some({
      utxoRoot: latestHeader.utxosRoot,
      txRoot: latestHeader.transactionsRoot,
    });
  });

const establishEndTimeFromTxRequests = (
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
): Effect.Effect<Option.Option<Date>, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length <= 0) {
      yield* Effect.logInfo(
        "🔹 No transactions were found in MempoolDB, checking ProcessedMempoolDB...",
      );

      const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;

      if (processedMempoolTxs.length <= 0) {
        // No transaction requests are available for inclusion in a block. By
        // setting `endTime` to `undefined` here, the code below can decide
        // whether it can stop if no
        return Option.none();
      } else {
        // No new transactions received, but there are uncommitted transactions
        // in the MPT. So its root must be used to submit a new block, and if
        // successful, `ProcessedMempoolDB` must be cleared. Following functions
        // should work fine with 0 mempool txs.
        return Option.some(processedMempoolTxs[0][TxColumns.TIMESTAMPTZ]);
      }
    } else {
      yield* Effect.logInfo(`🔹 ${mempoolTxs.length} retrieved.`);
      return Option.some(mempoolTxs[0][TxColumns.TIMESTAMPTZ]);
    }
  });

// TODO: Application of user events will likely affect this function as well.
const finalizeCommittedBlockLocally = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  newHeaderHash: string,
): Effect.Effect<void, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    const newHeaderHashBuffer = Buffer.from(fromHex(newHeaderHash));

    const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;
    const batches = buildSuccessfulCommitBatches(
      mempoolTxs,
      mempoolTxHashes,
      processedMempoolTxs,
      Math.floor(BATCH_SIZE / 2),
    );

    yield* Effect.logInfo(
      "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing all the processed txs from MempoolDB and ProcessedMempoolDB, and deleting mempool LevelDB...",
    );
    yield* Effect.all(
      [
        Effect.forEach(
          batches,
          (batch, i) => {
            const clearMempoolProgram =
              batch.clearMempoolTxHashes.length === 0
                ? Effect.void
                : MempoolDB.clearTxs([...batch.clearMempoolTxHashes]).pipe(
                    Effect.withSpan(`mempool-db-clear-txs-batch-${i}`),
                  );
            return Effect.all(
              [
                ImmutableDB.insertTxs([...batch.txsToInsertImmutable]).pipe(
                  Effect.withSpan(`immutable-db-insert-batch-${i}`),
                ),
                BlocksDB.insert(newHeaderHashBuffer, [...batch.blockTxHashes]).pipe(
                  Effect.withSpan(`blocks-db-insert-batch-${i}`),
                ),
                clearMempoolProgram,
              ],
              { concurrency: "unbounded" },
            );
          },
          {
            concurrency: "unbounded",
          },
        ),
        ProcessedMempoolDB.clear, // uses `TRUNCATE` so no need for batching.
        mempoolTrie.delete(),
      ],
      { concurrency: "unbounded" },
    );
  });

const successfulSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  newHeaderHash: string,
  workerInput: WorkerInput,
  txSize: number,
  sizeOfProcessedTxs: number,
  txHash: string,
): Effect.Effect<WorkerOutput, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    yield* finalizeCommittedBlockLocally(
      mempoolTrie,
      mempoolTxs,
      mempoolTxHashes,
      newHeaderHash,
    );

    return {
      type: "SuccessfulSubmissionOutput",
      submittedTxHash: txHash,
      txSize,
      mempoolTxsCount:
        mempoolTxs.length + workerInput.data.mempoolTxsCountSoFar,
      sizeOfBlocksTxs:
        sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
    };
  });

const successfulLocalFinalizationRecoveryProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  confirmedHeaderHash: string,
  workerInput: WorkerInput,
  sizeOfProcessedTxs: number,
): Effect.Effect<
  WorkerOutput,
  DatabaseError | FileSystemError,
  Database
> =>
  Effect.gen(function* () {
    yield* finalizeCommittedBlockLocally(
      mempoolTrie,
      mempoolTxs,
      mempoolTxHashes,
      confirmedHeaderHash,
    );
    return {
      type: "SuccessfulLocalFinalizationRecoveryOutput",
      mempoolTxsCount:
        mempoolTxs.length + workerInput.data.mempoolTxsCountSoFar,
      sizeOfBlocksTxs:
        sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
    };
  });

const skippedSubmissionProgram = (
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
) =>
  batchProgram(
    BATCH_SIZE,
    mempoolTxs.length,
    "skipped-submission-db-transfer",
    (startIndex: number, endIndex: number) => {
      const batchTxs = mempoolTxs.slice(startIndex, endIndex);
      const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);
      return Effect.all(
        [
          ProcessedMempoolDB.insertTxs(batchTxs).pipe(
            Effect.withSpan(`processed-mempool-db-insert-${startIndex}`),
          ),
          MempoolDB.clearTxs(batchHashes).pipe(
            Effect.withSpan(`mempool-db-clear-txs-${startIndex}`),
          ),
        ],
        { concurrency: "unbounded" },
      );
    },
  );

const failedSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxsCount: number,
  sizeOfProcessedTxs: number,
  err: TxSubmitError,
): Effect.Effect<WorkerOutput> =>
  Effect.gen(function* () {
    yield* Effect.logError(`🔹 ⚠️  Tx submit failed: ${err}`);
    yield* Effect.logError(
      "🔹 ⚠️  Mempool trie will be preserved, but db will be cleared.",
    );
    yield* Effect.logInfo("🔹 Mempool Trie stats:");
    console.dir(mempoolTrie.databaseStats(), { depth: null });
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
    };
  });

const recoverSubmittedTxHashByHeaderProgram = (
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
  expectedHeaderHash: string,
): Effect.Effect<Option.Option<string>, never, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    const latestBlock = yield* SDK.fetchLatestCommittedBlockProgram(
      lucid.api,
      fetchConfig,
    );
    const latestHeader =
      yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum);
    const latestHeaderHash = yield* SDK.hashBlockHeader(latestHeader);
    if (latestHeaderHash === expectedHeaderHash) {
      yield* Effect.logWarning(
        `🔹 Submit errored but on-chain header already advanced to ${expectedHeaderHash}; recovering submission state.`,
      );
      return Option.some(latestBlock.utxo.txHash);
    }
    return Option.none();
  }).pipe(
    Effect.catchAll((error) =>
      Effect.gen(function* () {
        yield* Effect.logWarning(
          `🔹 Could not verify submit recovery on-chain: ${formatUnknownError(error)}`,
        );
        return Option.none<string>();
      }),
    ),
  );

/**
 * Given the target user event table, this helper finds all the events falling
 * in the given time range and if any was found, returns an `Effect` that finds
 * the MPT root of those events.
 */
const userEventsProgram = (
  tableName: string,
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  Option.Option<Effect.Effect<string, MptError>>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const events = yield* retrieveTimeBoundEntries(
      tableName,
      startDate,
      endDate,
    );

    if (events.length <= 0) {
      yield* Effect.logInfo(
        `🔹 No events found in ${tableName} table between ${startDate.getTime()} and ${endDate.getTime()}.`,
      );
      return Option.none();
    } else {
      yield* Effect.logInfo(
        `🔹 ${events.length} event(s) found in ${tableName} table between ${startDate.getTime()} and ${endDate.getTime()}.`,
      );
      const eventIDs = events.map((event) => event[UserEventsColumns.ID]);
      const eventInfos = events.map((event) => event[UserEventsColumns.INFO]);
      return Option.some(keyValueMptRoot(eventIDs, eventInfos));
    }
  });

const buildUnsignedTx = (
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
  latestBlock: SDK.StateQueueUTxO,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  endDate: Date,
) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
    yield* lucid.switchToOperatorsMainWallet;
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid.api,
        latestBlock.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        "00".repeat(32),
        BigInt(endDate.getTime()),
      );

    const newHeaderHash = yield* SDK.hashBlockHeader(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

    // Build commitment block
    const commitBlockParams: SDK.StateQueueCommitBlockParams = {
      anchorUTxO: latestBlock,
      updatedAnchorDatum: updatedNodeDatum,
      newHeader: newHeader,
      stateQueueSpendingScript: stateQueueAuthValidator.spendingScript,
      policyId: stateQueueAuthValidator.policyId,
      stateQueueMintingScript: stateQueueAuthValidator.mintingScript,
    };

    yield* Effect.logInfo("🔹 Building block commitment transaction...");
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    yield* lucid.switchToOperatorsMainWallet;
    const commitTx = yield* SDK.incompleteCommitBlockHeaderTxProgram(
      lucid.api,
      fetchConfig,
      commitBlockParams,
    );
    const txBuilder = yield* Effect.tryPromise({
      try: () =>
        commitTx.complete({
          localUPLCEval: false,
        }),
      catch: (e) =>
        new SDK.StateQueueError({
          message: `Failed to build block header commitment transaction: ${e}`,
          cause: e,
        }),
    });

    const txSize = txBuilder.toCBOR().length / 2;
    yield* Effect.logInfo(`🔹 Transaction built successfully. Size: ${txSize}`);

    const signAndSubmitProgram = handleSignSubmitNoConfirmation(
      lucid.api,
      txBuilder,
    ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

    return {
      newHeaderHash,
      signAndSubmitProgram,
      txSize,
    };
  });

const databaseOperationsProgram = (
  workerInput: WorkerInput,
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
): Effect.Effect<
  WorkerOutput,
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | FileSystemError
  | LocalFinalizationPendingError
  | MptError,
  AlwaysSucceedsContract | Database | Lucid
> =>
  Effect.gen(function* () {
    const mempoolTxs = yield* MempoolDB.retrieve;

    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    if (
      shouldDeferCommitSubmission({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        hasAvailableConfirmedBlock,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 Local finalization pending and no confirmed block available yet; deferring new submission.",
      );
      return {
        type: "NothingToCommitOutput",
      };
    }

    const {
      utxoRoot,
      txRoot,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount,
    } = yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs);

    if (rejectedMempoolTxsCount > 0) {
      yield* Effect.logWarning(
        `Rejected ${rejectedMempoolTxsCount} malformed tx(s) during commitment preprocessing.`,
      );
    }

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime: Option.Option<Date> =
      yield* establishEndTimeFromTxRequests(processedMempoolTxs);

    const { stateQueue: stateQueueAuthValidator } =
      yield* AlwaysSucceedsContract;

    if (availableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
      //
      // TODO: Handle failures properly.
      yield* Effect.logInfo(
        "🔹 No confirmed blocks available. Transferring to ProcessedMempoolDB...",
      );
      yield* skippedSubmissionProgram(processedMempoolTxs, mempoolTxHashes);
      return {
        type: "SkippedSubmissionOutput",
        mempoolTxsCount,
        sizeOfProcessedTxs,
      };
    } else {
      yield* Effect.logInfo(
        "🔹 Previous submitted block is now confirmed, deserializing...",
      );
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );

      if (
        shouldAttemptLocalFinalizationRecovery({
          localFinalizationPending: workerInput.data.localFinalizationPending,
          hasAvailableConfirmedBlock: true,
        })
      ) {
        yield* Effect.logInfo(
          "🔹 Attempting local finalization recovery against confirmed block roots...",
        );
        const optHeaderRoots = yield* getLatestBlockHeaderRoots(latestBlock.datum);
        if (Option.isNone(optHeaderRoots)) {
          return {
            type: "FailureOutput",
            error:
              "Confirmed block datum does not contain a recoverable header for local finalization",
          };
        }
        const rootsMatch = rootsMatchConfirmedHeader({
          computedUtxoRoot: utxoRoot,
          computedTxRoot: txRoot,
          confirmedUtxoRoot: optHeaderRoots.value.utxoRoot,
          confirmedTxRoot: optHeaderRoots.value.txRoot,
        });
        if (!rootsMatch) {
          return {
            type: "FailureOutput",
            error:
              "Local finalization recovery aborted: computed roots do not match the confirmed block header",
          };
        }
        const confirmedHeader =
          yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum);
        const confirmedHeaderHash = yield* SDK.hashBlockHeader(confirmedHeader);
        return yield* successfulLocalFinalizationRecoveryProgram(
          mempoolTrie,
          processedMempoolTxs,
          mempoolTxHashes,
          confirmedHeaderHash,
          workerInput,
          sizeOfProcessedTxs,
        );
      }

      const startTime = yield* getLatestBlockDatumEndTime(latestBlock.datum);

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        const endDate = new Date();
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const optDepositsRootProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startTime,
          endDate,
        );
        if (Option.isNone(optDepositsRootProgram)) {
          yield* Effect.logInfo("🔹 Nothing to commit.");
          return {
            type: "NothingToCommitOutput",
          } as WorkerOutput;
        } else {
          const depositsRootFiber = yield* Effect.fork(
            optDepositsRootProgram.value,
          );
          const depositsRoot = yield* depositsRootFiber;
          yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
          const emptyRoot = yield* emptyRootHexProgram;
          const roots = selectCommitRoots({
            hasTxRequests: false,
            computedUtxoRoot: utxoRoot,
            computedTxRoot: txRoot,
            emptyRoot,
          });
          const { signAndSubmitProgram, txSize } = yield* buildUnsignedTx(
            stateQueueAuthValidator,
            latestBlock,
            roots.utxoRoot,
            roots.txRoot,
            depositsRoot,
            endDate,
          );

          return yield* Effect.matchEffect(signAndSubmitProgram, {
            onFailure: (_) => {
              // For now we'll assume the deposit events will be small in
              // number, so we won't concern ourselves with wasted work. That
              // is, we won't do anything and just report back with failure.
              const failureOutput: WorkerOutput = {
                type: "FailureOutput",
                error:
                  "Something went wrong the transaction. No tx requests were present, only deposits.",
              };

              return Effect.succeed(failureOutput);
            },
            onSuccess: (txHash) =>
              Effect.succeed({
                type: "SuccessfulSubmissionOutput",
                submittedTxHash: txHash,
                txSize,
                mempoolTxsCount: 0,
                sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
              } as WorkerOutput),
          });
        }
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. We use the latest transaction's timestamp as the upper
        // bound of the block we are about to submit.
        const endTime = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const optDepositsRootProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startTime,
          endTime,
        );

        let depositsRoot: string = yield* emptyRootHexProgram;
        if (Option.isSome(optDepositsRootProgram)) {
          const depositsRootFiber = yield* Effect.fork(
            optDepositsRootProgram.value,
          );
          depositsRoot = yield* depositsRootFiber;
        }
        yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);

        const { newHeaderHash, signAndSubmitProgram, txSize } =
          yield* buildUnsignedTx(
            stateQueueAuthValidator,
            latestBlock,
            utxoRoot,
            txRoot,
            depositsRoot,
            endTime,
          );

        return yield* Effect.matchEffect(signAndSubmitProgram, {
          // TODO: Separating the behavior between these two failures seems
          //       unnecessary.
          onFailure: Match.valueTags({
            TxSignError: (_) => {
              const failureOutput: WorkerOutput = {
                type: "FailureOutput",
                error: "Something went wrong at signing the transaction",
              };
              return Effect.succeed(failureOutput);
            },
            TxSubmitError: (e) =>
              Effect.gen(function* () {
                const recoveredTxHash = yield* recoverSubmittedTxHashByHeaderProgram(
                  stateQueueAuthValidator,
                  newHeaderHash,
                );
                if (Option.isSome(recoveredTxHash)) {
                  return yield* successfulSubmissionProgram(
                    mempoolTrie,
                    processedMempoolTxs,
                    mempoolTxHashes,
                    newHeaderHash,
                    workerInput,
                    txSize,
                    sizeOfProcessedTxs,
                    recoveredTxHash.value,
                  ).pipe(
                    Effect.mapError(
                      (cause) =>
                        new LocalFinalizationPendingError({
                          submittedTxHash: recoveredTxHash.value,
                          txSize,
                          mempoolTxsCount:
                            processedMempoolTxs.length +
                            workerInput.data.mempoolTxsCountSoFar,
                          sizeOfBlocksTxs:
                            sizeOfProcessedTxs +
                            workerInput.data.sizeOfProcessedTxsSoFar,
                          cause,
                        }),
                    ),
                  );
                }

                // With a failed tx submission, we need to carry out the same db
                // logic as the case where no confirmed blocks are available.
                //
                // TODO: Handle failures properly.
                yield* skippedSubmissionProgram(
                  processedMempoolTxs,
                  mempoolTxHashes,
                );

                return yield* failedSubmissionProgram(
                  mempoolTrie,
                  mempoolTxsCount,
                  sizeOfProcessedTxs,
                  e,
                );
              }),
          }),
          onSuccess: (txHash) =>
            successfulSubmissionProgram(
              mempoolTrie,
              processedMempoolTxs,
              mempoolTxHashes,
              newHeaderHash,
              workerInput,
              txSize,
              sizeOfProcessedTxs,
              txHash,
            ).pipe(
              Effect.mapError(
                (cause) =>
                  new LocalFinalizationPendingError({
                    submittedTxHash: txHash,
                    txSize,
                    mempoolTxsCount:
                      processedMempoolTxs.length +
                      workerInput.data.mempoolTxsCountSoFar,
                    sizeOfBlocksTxs:
                      sizeOfProcessedTxs +
                      workerInput.data.sizeOfProcessedTxsSoFar,
                    cause,
                  }),
              ),
            ),
        });
      }
    }
  });

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput | undefined,
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | ConfigError
  | DatabaseInitializationError
  | DatabaseError
  | FileSystemError
  | LocalFinalizationPendingError
  | TxSignError
  | MptError,
  AlwaysSucceedsContract | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const { ledgerTrie, mempoolTrie } = yield* makeMpts;

    const result: void | WorkerOutput = yield* withTrieTransaction(
      ledgerTrie,
      databaseOperationsProgram(workerInput, ledgerTrie, mempoolTrie),
    ).pipe(
      Effect.catchAll((error) =>
        error._tag === "LocalFinalizationPendingError"
          ? Effect.succeed({
              type: "SubmittedAwaitingLocalFinalizationOutput",
              submittedTxHash: error.submittedTxHash,
              txSize: error.txSize,
              mempoolTxsCount: error.mempoolTxsCount,
              sizeOfBlocksTxs: error.sizeOfBlocksTxs,
              error: formatUnknownError(error.cause),
            } as WorkerOutput)
          : Effect.fail(error),
      ),
    );
    if (result) {
      return result;
    } else {
      return undefined;
    }
  });

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
  Effect.provide(AlwaysSucceedsContract.Default),
  Effect.provide(Database.layer),
  Effect.provide(Lucid.Default),
  Effect.provide(NodeConfig.layer),
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
