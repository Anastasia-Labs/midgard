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
  DepositsDB,
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { Data, fromHex } from "@lucid-evolution/lucid";
import {
  MptError,
  keyValueMptRoot,
  makeMpts,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import { FileSystemError, batchProgram } from "@/utils.js";
import {
  EntryWithTimeStamp as TxEntry,
  Columns as TxColumns,
} from "@/database/utils/tx.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  processDepositEvent,
  processTxOrderEvent,
  processTxRequestEvent,
} from "./utils/events.js";

const BATCH_SIZE = 100;

const getLatestBlockDatumEndTime = (
  latestBlocksDatum: SDK.TxBuilder.StateQueue.Datum,
): Effect.Effect<Date, SDK.Utils.DataCoercionError, never> =>
  Effect.gen(function* () {
    let endTimeBigInt: bigint;
    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* SDK.Utils.getConfirmedStateFromStateQueueDatum(
          latestBlocksDatum,
        );
      endTimeBigInt = confirmedState.endTime;
    } else {
      const latestHeader =
        yield* SDK.Utils.getHeaderFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = latestHeader.endTime;
    }
    return new Date(Number(endTimeBigInt));
  });

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput | undefined,
  | SDK.Utils.CborDeserializationError
  | SDK.Utils.CmlUnexpectedError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.HashingError
  | SDK.Utils.LucidError
  | SDK.Utils.StateQueueError
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
    let latestTxEntry: undefined | TxEntry;

    if (mempoolTxsCount === 0) {
      yield* Effect.logInfo(
        "🔹 No transactions were found in MempoolDB",
      );
    } else {
      yield* Effect.logInfo(
        `🔹 ${mempoolTxsCount} transactions were found in MempoolDB`,
      );
      latestTxEntry = mempoolTxs[0];
    }

    const { ledgerTrie, mempoolTrie } = yield* makeMpts;

    const databaseOperationsProgram: Effect.Effect<
      WorkerOutput,
      | SDK.Utils.CborDeserializationError
      | SDK.Utils.CmlUnexpectedError
      | SDK.Utils.DataCoercionError
      | SDK.Utils.HashingError
      | SDK.Utils.LucidError
      | SDK.Utils.StateQueueError
      | DatabaseError
      | FileSystemError
      | MptError,
      AlwaysSucceedsContract | Database | NodeConfig
    > = Effect.gen(function* () {
      const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

      if (workerInput.data.availableConfirmedBlock === "") {
        // The tx confirmation worker has not yet confirmed a previously
        // submitted tx
        // TODO: Handle failures properly.
        yield* Effect.logInfo(
          "🔹 No confirmed blocks available. Skipping submission.",
        );
        return {
          type: "SkippedSubmissionOutput",
          mempoolTxsCount,
          sizeOfProcessedTxs: 0,
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

        yield* lucid.switchToOperatorsMainWallet;

        const startTime = yield* getLatestBlockDatumEndTime(latestBlock.datum);
        const endTime =
          latestTxEntry === undefined
            ? new Date()
            : latestTxEntry[TxColumns.TIMESTAMPTZ];
        // TODO: with small modification of `processTxRequestEvent` we probably can process all 3 tries in parallel
        const sizeOfTxOrderTxs = yield* processTxOrderEvent(
          startTime,
          endTime,
          ledgerTrie,
        );
        const { mempoolTxHashes, sizeOfTxRequestTxs } =
          yield* processTxRequestEvent(ledgerTrie, mempoolTrie, mempoolTxs);
        const { depositRoot, sizeOfDepositTxs } = yield* processDepositEvent(
          startTime,
          endTime,
        );
        const sizeOfProcessedTxs =
          sizeOfTxOrderTxs + sizeOfTxRequestTxs + sizeOfDepositTxs;

        if (sizeOfProcessedTxs === 0)
          return {
            type: "NothingToCommitOutput",
          } as WorkerOutput;

        const { nodeDatum: updatedNodeDatum, header: newHeader } =
          yield* SDK.Utils.updateLatestBlocksDatumAndGetTheNewHeader(
            lucid.api,
            latestBlock.datum,
            yield* ledgerTrie.getRootHex(),
            yield* mempoolTrie.getRootHex(),
            depositRoot,
            "00".repeat(32),
            BigInt(Number(endTime)),
          );

        const newHeaderHash = yield* SDK.Utils.hashHeader(newHeader);
        yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

        // Build commitment block
        const commitBlockParams: SDK.TxBuilder.StateQueue.CommitBlockParams = {
          anchorUTxO: latestBlock,
          updatedAnchorDatum: updatedNodeDatum,
          newHeader: newHeader,
          stateQueueSpendingScript: stateQueueAuthValidator.spendScript,
          policyId: stateQueueAuthValidator.policyId,
          stateQueueMintingScript: stateQueueAuthValidator.mintScript,
        };

        const aoUpdateCommitmentTimeParams = {};

        yield* Effect.logInfo("🔹 Building block commitment transaction...");
        const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
          stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
          stateQueuePolicyId: stateQueueAuthValidator.policyId,
        };
        yield* lucid.switchToOperatorsMainWallet;
        const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
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

            yield* Effect.logInfo(
              "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing all the processed txs from MempoolDB, and deleting mempool LevelDB...",
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
