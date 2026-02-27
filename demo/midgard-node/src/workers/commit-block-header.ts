import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Match, Option, pipe } from "effect";
import {
  applyMempoolToLedger,
  applyDepositsToLedger,
  buildUnsignedBlockCommitmentTx,
  deserializeStateQueueUTxO,
  establishEndDateFromTxRequests,
  getBlockHeadersEndDate,
  WorkerInput,
  WorkerOutput,
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
  MempoolLedgerDB,
  LedgerUtils as LedgerTable,
  AddressHistoryDB,
  UserEventsUtils,
  WithdrawalsDB,
  TxOrdersDB,
} from "@/database/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import { CML, fromHex } from "@lucid-evolution/lucid";
import {
  MidgardMpt,
  MptError,
  emptyRootHexProgram,
  makeMpts,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import {
  FileSystemError,
  batchProgram,
  trivialTransactionFromCMLUnspentOutput,
} from "@/utils.js";
import { DatabaseError } from "@/database/utils/common.js";
import { userEventsProgram } from "./utils/commit-block-header.js";

// Batch size for database operations.
const BATCH_SIZE = 100;

const addDepositUTxOsToDatabases = (
  insertedDepositUTxOs: {
    utxo: CML.TransactionUnspentOutput;
    inclusionTime: Date;
  }[],
): Effect.Effect<void, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 Inserting included deposits into ImmutableDB, MempoolLedgerDB and AddressHistoryDB",
    );

    yield* batchProgram(
      Math.floor(BATCH_SIZE),
      insertedDepositUTxOs.length,
      "inserting-deposits-to-databases",
      (startIndex: number, endIndex: number) =>
        Effect.gen(function* () {
          const batchInsertedDepositUTxOs = insertedDepositUTxOs.slice(
            startIndex,
            endIndex,
          );
          const ledgerTableBatch: LedgerTable.EntryWithTimeStamp[] =
            batchInsertedDepositUTxOs.map(({ utxo, inclusionTime }) => ({
              [LedgerTable.Columns.TX_ID]: Buffer.from(
                utxo.input().transaction_id().to_raw_bytes(),
              ),
              [LedgerTable.Columns.OUTREF]: Buffer.from(
                utxo.input().to_cbor_bytes(),
              ),
              [LedgerTable.Columns.OUTPUT]: Buffer.from(
                utxo.output().to_cbor_bytes(),
              ),
              [LedgerTable.Columns.ADDRESS]: utxo.output().address().to_hex(),
              [LedgerTable.Columns.TIMESTAMPTZ]: inclusionTime,
            }));

          const txTableBatch: TxTable.EntryWithTimeStamp[] =
            yield* Effect.forEach(
              batchInsertedDepositUTxOs,
              ({ utxo, inclusionTime }) =>
                Effect.gen(function* () {
                  const tx =
                    yield* trivialTransactionFromCMLUnspentOutput(utxo);
                  return {
                    [TxTable.Columns.TX_ID]: Buffer.from(
                      utxo.input().transaction_id().to_raw_bytes(),
                    ),
                    [TxTable.Columns.TX]: Buffer.from(tx.to_cbor_bytes()),
                    [TxTable.Columns.TIMESTAMPTZ]: inclusionTime,
                  };
                }),
              { concurrency: "unbounded" },
            );

          const addressTableBatch: AddressHistoryDB.Entry[] =
            batchInsertedDepositUTxOs.map(({ utxo }) => ({
              [LedgerTable.Columns.TX_ID]: Buffer.from(
                utxo.input().transaction_id().to_raw_bytes(),
              ),
              [LedgerTable.Columns.ADDRESS]: utxo.output().address().to_hex(),
            }));

          return Effect.all(
            [
              MempoolLedgerDB.insert(ledgerTableBatch).pipe(
                Effect.withSpan(`mempool-ledger-db-insert-${startIndex}`),
              ),
              ImmutableDB.insertTxs(txTableBatch).pipe(
                Effect.withSpan(`immutable-db-insert-${startIndex}`),
              ),
              AddressHistoryDB.insertEntries(addressTableBatch).pipe(
                Effect.withSpan(`address-history-db-insert-${startIndex}`),
              ),
            ],
            { concurrency: "unbounded" },
          );
        }),
    );
  });

/**
 * If any deposits were added to `ledgerTrie`, remove them and return the
 * provided output. If the removal fails, return the provided failure output.
 */
const withDepositsReverted = (
  ledgerTrie: MidgardMpt,
  depositEventEntries: readonly UserEventsUtils.Entry[],
  outputIfReversionSucceeds: WorkerOutput,
  outputIfReversionFails: WorkerOutput,
): Effect.Effect<WorkerOutput, never, NodeConfig | AlwaysSucceedsContract> => {
  if (depositEventEntries.length <= 0) {
    return Effect.succeed(outputIfReversionSucceeds);
  }
  // TODO: Handle this failure properly.
  return applyDepositsToLedger("remove", ledgerTrie, depositEventEntries).pipe(
    Effect.as(outputIfReversionSucceeds),
    Effect.catchAllCause((_cause) => Effect.succeed(outputIfReversionFails)),
  );
};

const successfulSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  insertedDepositUTxOs: {
    utxo: CML.TransactionUnspentOutput;
    inclusionTime: Date;
  }[],
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  newHeaderHash: string,
  workerInput: WorkerInput,
  txSize: number,
  sizeOfProcessedTxs: number,
  txHash: string,
): Effect.Effect<WorkerOutput, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    const newHeaderHashBuffer = Buffer.from(fromHex(newHeaderHash));

    const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;

    yield* Effect.logInfo(
      "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing all the processed txs from MempoolDB and ProcessedMempoolDB, and deleting mempool LevelDB...",
    );
    yield* Effect.all(
      [
        batchProgram(
          Math.floor(BATCH_SIZE),
          mempoolTxs.length,
          "successful-commit",
          (startIndex: number, endIndex: number) => {
            const batchTxs = mempoolTxs.slice(startIndex, endIndex);
            const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);
            const batchHashesForBlocks = [...batchHashes];

            const batchProcessedTxs = processedMempoolTxs.slice(
              startIndex,
              endIndex,
            );

            for (let i = 0; i < batchProcessedTxs.length; i++) {
              const txPair = batchProcessedTxs[i];
              batchTxs.push(txPair);
              batchHashesForBlocks.push(txPair[TxTable.Columns.TX_ID]);
            }

            return Effect.all(
              [
                addDepositUTxOsToDatabases(insertedDepositUTxOs),
                ImmutableDB.insertTxs(batchTxs).pipe(
                  Effect.withSpan(`immutable-db-insert-${startIndex}`),
                ),
                BlocksDB.insert(newHeaderHashBuffer, batchHashesForBlocks).pipe(
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
      type: "SuccessfulCommitmentOutput",
      submittedTxHash: txHash,
      txSize,
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
  | MptError,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const mempoolTxs = yield* MempoolDB.retrieve;
    const mempoolTxsCount = mempoolTxs.length;

    const { mempoolTxHashes, sizeOfProcessedTxs } = yield* applyMempoolToLedger(
      ledgerTrie,
      mempoolTrie,
      mempoolTxs,
    );

    const { stateQueue: stateQueueAuthValidator } =
      yield* AlwaysSucceedsContract;

    if (workerInput.data.availableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
      //
      // We are ignoring user events here because we don't have a confirmed
      // block to extract the start time of the inclusion time window for them.
      //
      // TODO: Handle failures properly.
      yield* Effect.logInfo(
        "🔹 No confirmed blocks available. Transferring to ProcessedMempoolDB...",
      );
      yield* skippedSubmissionProgram(mempoolTxs, mempoolTxHashes);
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

      const startDate = yield* getBlockHeadersEndDate(latestBlock.datum);

      const optEndTime: Option.Option<Date> =
        yield* establishEndDateFromTxRequests(mempoolTxs);

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startDate` and current moment.
        const endDate = new Date();
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const optDepositsProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startDate,
          endDate,
        );
        if (Option.isNone(optDepositsProgram)) {
          yield* Effect.logInfo("🔹 Nothing to commit.");
          const workerOutput: WorkerOutput = { type: "NothingToCommitOutput" };
          return workerOutput;
        } else {
          // Here there are no tx requests, but deposits are slated for
          // inclusion.
          const depositEventEntries = optDepositsProgram.value.retreivedEvents;
          const insertedDepositUTxOs = yield* applyDepositsToLedger(
            "add",
            ledgerTrie,
            depositEventEntries,
          );

          const depositsRoot = yield* optDepositsProgram.value.mptRoot;
          const utxoRoot = yield* ledgerTrie.getRootHex();
          const txRoot = yield* mempoolTrie.getRootHex();

          yield* Effect.logInfo(`🔹 Deposits root found: ${depositsRoot}`);
          yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
          yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);

          const { signAndSubmitProgram, txSize } =
            yield* buildUnsignedBlockCommitmentTx(
              stateQueueAuthValidator,
              latestBlock,
              utxoRoot,
              txRoot,
              depositsRoot,
              endDate,
            );

          return yield* Effect.matchEffect(signAndSubmitProgram, {
            onFailure: (_) => {
              // For now we'll assume the deposit events will be small in
              // number, so we won't concern ourselves with wasted work. That
              // is, we just revert their addition to the ledger trie.

              const outputIfReversionSucceeds: WorkerOutput = {
                type: "FailureOutput",
                error:
                  "Block commitment tx failed (no tx requests, only deposits).",
              };

              const outputIfReversionFails: WorkerOutput = {
                type: "FailureOutput",
                error:
                  "Block commitment tx failed (no tx requests, only deposits). The ledger trie reversal also failed.",
              };

              // Removing the added deposits is necessary here because we are
              // treating `ledgerTrie` as the source of truth. Hence not
              // removing them would allow tx requests/orders in next batch that
              // would spend the resulting UTxOs, which is invalid as deposits
              // are the last events to be applied to the ledger (Fig. 1.1.1 in
              // specs).
              return withDepositsReverted(
                ledgerTrie,
                depositEventEntries,
                outputIfReversionSucceeds,
                outputIfReversionFails,
              );
            },
            onSuccess: (txHash) =>
              addDepositUTxOsToDatabases(insertedDepositUTxOs).pipe(
                Effect.andThen((_) => {
                  const successOutput: WorkerOutput = {
                    type: "SuccessfulCommitmentOutput",
                    submittedTxHash: txHash,
                    txSize,
                    mempoolTxsCount: 0,
                    sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
                  };
                  return Effect.succeed(successOutput);
                }),
                Effect.catchAllCause((_cause) => {
                  // TODO: Handle this failure properly.
                  const failureOutput: WorkerOutput = {
                    type: "FailureOutput",
                    error:
                      "Block commitment with 0 txs and only deposit events went through successfully, but addition of the deposit events to the corresponding db tables failed.",
                  };
                  return Effect.succeed(failureOutput);
                }),
              ),
          });
        }
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. We use the latest transaction's timestamp as the upper
        // bound of the block we are about to submit.
        const endDate = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const optDepositsProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startDate,
          endDate,
        );

        let depositsRoot: string = yield* emptyRootHexProgram;
        let insertedDepositUTxOs: {
          utxo: CML.TransactionUnspentOutput;
          inclusionTime: Date;
        }[] = [];
        const depositEventEntries = Option.isSome(optDepositsProgram)
          ? optDepositsProgram.value.retreivedEvents
          : [];
        if (Option.isSome(optDepositsProgram)) {
          insertedDepositUTxOs = yield* applyDepositsToLedger(
            "add",
            ledgerTrie,
            depositEventEntries,
          );
          depositsRoot = yield* optDepositsProgram.value.mptRoot;
        }
        const utxoRoot = yield* ledgerTrie.getRootHex();
        const txRoot = yield* mempoolTrie.getRootHex();

        yield* Effect.logInfo(`🔹 Deposits root found: ${depositsRoot}`);
        yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
        yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);

        const { newHeaderHash, signAndSubmitProgram, txSize } =
          yield* buildUnsignedBlockCommitmentTx(
            stateQueueAuthValidator,
            latestBlock,
            utxoRoot,
            txRoot,
            depositsRoot,
            endDate,
          );

        return yield* Effect.matchEffect(signAndSubmitProgram, {
          // TODO: Separating the behavior between these two failures seems
          //       unnecessary.
          onFailure: Match.valueTags({
            TxSignError: (_) => {
              const outputIfReversionSucceeds: WorkerOutput = {
                type: "FailureOutput",
                error: "Something went wrong at signing the transaction",
              };
              const outputIfReversionFails: WorkerOutput = {
                type: "FailureOutput",
                error:
                  "Block commitment tx signing failed. The ledger trie reversal of added deposits also failed.",
              };
              return withDepositsReverted(
                ledgerTrie,
                depositEventEntries,
                outputIfReversionSucceeds,
                outputIfReversionFails,
              );
            },
            TxSubmitError: (e) =>
              Effect.gen(function* () {
                // With a failed tx submission, we need to carry out the same db
                // logic as the case where no confirmed blocks are available.
                //
                // TODO: Handle failures properly.
                yield* skippedSubmissionProgram(mempoolTxs, mempoolTxHashes);

                const skippedOutput = yield* failedSubmissionProgram(
                  mempoolTrie,
                  mempoolTxsCount,
                  sizeOfProcessedTxs,
                  e,
                );
                const ledgerFailureOutput: WorkerOutput = {
                  type: "FailureOutput",
                  error:
                    "Block commitment tx submission failed and rollback of added deposits from ledger trie also failed.",
                };
                return yield* withDepositsReverted(
                  ledgerTrie,
                  depositEventEntries,
                  skippedOutput,
                  ledgerFailureOutput,
                );
              }),
          }),
          onSuccess: (txHash) =>
            successfulSubmissionProgram(
              mempoolTrie,
              insertedDepositUTxOs,
              mempoolTxs,
              mempoolTxHashes,
              newHeaderHash,
              workerInput,
              txSize,
              sizeOfProcessedTxs,
              txHash,
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
