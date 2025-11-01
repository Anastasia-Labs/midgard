import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Match, pipe } from "effect";
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
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { fromHex } from "@lucid-evolution/lucid";
import {
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
  Columns as UserEventsColumns,
  retrieveTimeBoundEntries,
} from "@/database/utils/user-events.js";
import { DatabaseError } from "@/database/utils/common.js";
import { RuntimeFiber } from "effect/Fiber";

const BATCH_SIZE = 100;

export enum UserEventTables {
  DEPOSITS = DepositsDB.tableName,
}

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
  | TxSignError
  | MptError,
  AlwaysSucceedsContract | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;

    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const mempoolTxs = yield* MempoolDB.retrieve;
    const mempoolTxsCount = mempoolTxs.length;
    let endTime: Date | undefined = undefined;

    if (mempoolTxsCount === 0) {
      yield* Effect.logInfo(
        "🔹 No transactions were found in MempoolDB, checking ProcessedMempoolDB...",
      );

      const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;

      if (processedMempoolTxs.length === 0) {
        // No transaction requests are available for inclusion in a block. By
        // setting `endTime` to `undefined` here, the code below can decide
        // whether it can stop if no
        endTime = undefined;
      } else {
        // No new transactions received, but there are uncommitted transactions
        // in the MPT. So its root must be used to submit a new block, and if
        // successful, `ProcessedMempoolDB` must be cleared. Following functions
        // should work fine with 0 mempool txs.
        endTime = processedMempoolTxs[0][TxColumns.TIMESTAMPTZ];
      }
    } else {
      yield* Effect.logInfo("🔹 Transactions were found in MempoolDB");
      endTime = mempoolTxs[0][TxColumns.TIMESTAMPTZ];
    }

    yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

    const { ledgerTrie, mempoolTrie } = yield* makeMpts;

    const databaseOperationsProgram: Effect.Effect<
      WorkerOutput,
      | SDK.CborDeserializationError
      | SDK.CmlUnexpectedError
      | SDK.DataCoercionError
      | SDK.HashingError
      | SDK.LucidError
      | SDK.StateQueueError
      | DatabaseError
      | FileSystemError
      | MptError,
      AlwaysSucceedsContract | Database | NodeConfig
    > = Effect.gen(function* () {
      const { utxoRoot, txRoot, mempoolTxHashes, sizeOfProcessedTxs } =
        yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs);

      const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

      const skippedSubmissionProgram = batchProgram(
        BATCH_SIZE,
        mempoolTxsCount,
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

      if (workerInput.data.availableConfirmedBlock === "") {
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
        yield* skippedSubmissionProgram;
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
          workerInput.data.availableConfirmedBlock,
        );
        yield* Effect.logInfo(
          "🔹 Finding updated block datum and new header...",
        );

        const startTime = yield* getLatestBlockDatumEndTime(latestBlock.datum);

        const userEventsProgram = (
          tableName: string,
          endDate: Date,
        ): Effect.Effect<
          Effect.Effect<string, MptError> | undefined,
          DatabaseError,
          Database
        > =>
          Effect.gen(function* () {
            const events = yield* retrieveTimeBoundEntries(
              tableName,
              startTime,
              endDate,
            );

            if (events.length <= 0) {
              yield* Effect.logInfo(
                `🔹 No events found in ${tableName} table.`,
              );
              return undefined;
            } else {
              const eventIDs = events.map(
                (event) => event[UserEventsColumns.ID],
              );
              const eventInfos = events.map(
                (event) => event[UserEventsColumns.INFO],
              );
              return keyValueMptRoot(eventIDs, eventInfos);
            }
          });

        let depositsRootFiber: RuntimeFiber<string, MptError> | undefined =
          undefined;

        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        if (endTime === undefined) {
          const depositsRootProgram = yield* userEventsProgram(
            DepositsDB.tableName,
            new Date(),
          );
          if (depositsRootProgram === undefined) {
            yield* Effect.logInfo("🔹 Nothing to commit.");
            return {
              type: "NothingToCommitOutput",
            } as WorkerOutput;
          } else {
            depositsRootFiber = yield* Effect.fork(depositsRootProgram);
          }
        }

        const depositsRoot = depositsRootFiber
          ? yield* depositsRootFiber
          : yield* emptyRootHexProgram;

        const { nodeDatum: updatedNodeDatum, header: newHeader } =
          yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
            lucid.api,
            latestBlock.datum,
            utxoRoot,
            txRoot,
            depositsRoot,
            "00".repeat(32),
            BigInt(Number(endTime)),
          );

        const newHeaderHash = yield* SDK.hashHeader(newHeader);
        yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

        // Build commitment block
        const commitBlockParams: SDK.StateQueueCommitBlockParams = {
          anchorUTxO: latestBlock,
          updatedAnchorDatum: updatedNodeDatum,
          newHeader: newHeader,
          stateQueueSpendingScript: stateQueueAuthValidator.spendScript,
          policyId: stateQueueAuthValidator.policyId,
          stateQueueMintingScript: stateQueueAuthValidator.mintScript,
        };

        const aoUpdateCommitmentTimeParams = {};

        yield* Effect.logInfo("🔹 Building block commitment transaction...");
        const fetchConfig: SDK.StateQueueFetchConfig = {
          stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
          stateQueuePolicyId: stateQueueAuthValidator.policyId,
        };
        yield* lucid.switchToOperatorsMainWallet;
        const txBuilder = yield* SDK.unsignedCommitBlockHeaderTxProgram(
          lucid.api,
          fetchConfig,
          commitBlockParams,
          aoUpdateCommitmentTimeParams,
        );
        const txSize = txBuilder.toCBOR().length / 2;
        yield* Effect.logInfo(
          `🔹 Transaction built successfully. Size: ${txSize}`,
        );

        const failedSubmissionProgram = (
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

        const successfulSubmissionProgram = (
          txHash: string,
        ): Effect.Effect<
          WorkerOutput,
          DatabaseError | FileSystemError,
          Database | NodeConfig
        > =>
          Effect.gen(function* () {
            const newHeaderHashBuffer = Buffer.from(fromHex(newHeaderHash));

            const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;

            yield* Effect.logInfo(
              "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing all the processed txs from MempoolDB and ProcessedMempoolDB, and deleting mempool LevelDB...",
            );
            yield* Effect.all(
              [
                batchProgram(
                  Math.floor(BATCH_SIZE / 2),
                  mempoolTxsCount,
                  "successful-commit",
                  (startIndex: number, endIndex: number) => {
                    const batchTxs = mempoolTxs.slice(startIndex, endIndex);
                    const batchHashes = mempoolTxHashes.slice(
                      startIndex,
                      endIndex,
                    );
                    const batchHashesForBlocks = [...batchHashes];

                    const batchProcessedTxs = processedMempoolTxs.slice(
                      startIndex,
                      endIndex,
                    );

                    for (let i = 0; i < batchProcessedTxs.length; i++) {
                      const txPair = batchProcessedTxs[i];
                      batchTxs.push(txPair);
                      batchHashesForBlocks.push(txPair[TxColumns.TX_ID]);
                    }

                    return Effect.all(
                      [
                        ImmutableDB.insertTxs(batchTxs).pipe(
                          Effect.withSpan(`immutable-db-insert-${startIndex}`),
                        ),
                        BlocksDB.insert(
                          newHeaderHashBuffer,
                          batchHashesForBlocks,
                        ).pipe(
                          Effect.withSpan(`blocks-db-insert-${startIndex}`),
                        ),
                        MempoolDB.clearTxs(batchHashes).pipe(
                          Effect.withSpan(`mempool-db-clear-txs-${startIndex}`),
                        ),
                      ],
                      { concurrency: "unbounded" },
                    );
                  },
                ),
                ProcessedMempoolDB.clear, // uses `TRUNCATE` so no need for batching.
                mempoolTrie.delete(),
              ],
              { concurrency: "unbounded" },
            );

            return {
              type: "SuccessfulSubmissionOutput",
              submittedTxHash: txHash,
              txSize,
              mempoolTxsCount:
                mempoolTxsCount + workerInput.data.mempoolTxsCountSoFar,
              sizeOfBlocksTxs:
                sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
            };
          });

        const signAndSubmitProgram = handleSignSubmitNoConfirmation(
          lucid.api,
          txBuilder,
        ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

        return yield* Effect.matchEffect(signAndSubmitProgram, {
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
                // With a failed tx submission, we need to carry out the same db
                // logic as the case where no confirmed blocks are available.
                //
                // TODO: Handle failures properly.
                yield* skippedSubmissionProgram;
                return yield* failedSubmissionProgram(e);
              }),
          }),
          onSuccess: successfulSubmissionProgram,
        });
      }
    });

    const result: void | WorkerOutput = yield* withTrieTransaction(
      ledgerTrie,
      databaseOperationsProgram,
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
