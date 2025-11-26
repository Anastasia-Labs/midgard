import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Match, Option, pipe } from "effect";
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
  AuthenticatedValidator,
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
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { CML, fromHex } from "@lucid-evolution/lucid";
import {
  MidgardMpt,
  MptError,
  emptyRootHexProgram,
  keyValueMptRoot,
  makeMpts,
  processMpts,
  applyDepositsToLedger,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import {
  FileSystemError,
  batchProgram,
  trivialTransactionFromCMLUnspentOutput,
} from "@/utils.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import {
  Columns as UserEventsColumns,
  retrieveTimeBoundEntries,
} from "@/database/utils/user-events.js";
import { DatabaseError } from "@/database/utils/common.js";
import { RuntimeFiber } from "effect/Fiber";

const BATCH_SIZE = 100;

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
          const batchInsertedDepositUTxOs = insertedDepositUTxOs.slice(startIndex, endIndex);
          const ledgerTableBatch: LedgerTable.EntryWithTimeStamp[] =
            batchInsertedDepositUTxOs.map(({utxo, inclusionTime}) => ({
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
            yield* Effect.forEach(batchInsertedDepositUTxOs, ({utxo, inclusionTime}) =>
              Effect.gen(function* () {
                const tx = yield* trivialTransactionFromCMLUnspentOutput(utxo);
                return {
                  [TxTable.Columns.TX_ID]: Buffer.from(
                    utxo.input().transaction_id().to_raw_bytes(),
                  ),
                  [TxTable.Columns.TX]: Buffer.from(tx.to_cbor_bytes()),
                  [TxTable.Columns.TIMESTAMPTZ]: inclusionTime,
                };
              }),
              { concurrency: "unbounded"}
            );

          const addressTableBatch: AddressHistoryDB.Entry[] = batchInsertedDepositUTxOs.map(({ utxo }) =>
                ({
                  [LedgerTable.Columns.TX_ID]: Buffer.from(
                    utxo.input().transaction_id().to_raw_bytes(),
                  ),
                  [LedgerTable.Columns.ADDRESS]: utxo
                    .output()
                    .address()
                    .to_hex(),
                })
            );

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
          Math.floor(BATCH_SIZE / 2),
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
              batchHashesForBlocks.push(txPair[TxColumns.TX_ID]);
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
      type: "SuccessfulSubmissionOutput",
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

/**
 * Given the target user event table, this helper finds all the events falling
 * in the given time range and if any was found, returns an `Effect` that finds
 * the MPT root of those events with the retrieved event entries.
 */
const userEventsProgram = (
  tableName: string,
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  Option.Option<{mptRoot: Effect.Effect<string, MptError>, retreivedEvents: readonly UserEventsUtils.Entry[]}>,
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
      return Option.some({ mptRoot: keyValueMptRoot(eventIDs, eventInfos), retreivedEvents: events });
    }
  });

const buildUnsignedTx = (
  stateQueueAuthValidator: AuthenticatedValidator,
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
  | MptError,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const mempoolTxs = yield* MempoolDB.retrieve;
    const mempoolTxsCount = mempoolTxs.length;

    const { mempoolTxHashes, sizeOfProcessedTxs } = yield* processMpts(
      ledgerTrie,
      mempoolTrie,
      mempoolTxs,
    );

    const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

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

      const startTime = yield* getLatestBlockDatumEndTime(latestBlock.datum);

      const optEndTime: Option.Option<Date> =
        yield* establishEndTimeFromTxRequests(mempoolTxs);

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        const endTime = new Date();

        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const optUserEventsProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startTime,
          endTime,
        );
        if (Option.isNone(optUserEventsProgram)) {
          yield* Effect.logInfo("🔹 Nothing to commit.");
          return {
            type: "NothingToCommitOutput",
          } as WorkerOutput;
        } else {
          const insertedDepositUTxOs = yield* applyDepositsToLedger(
            ledgerTrie,
            optUserEventsProgram.value.retreivedEvents
          );

          const depositsRootFiber: RuntimeFiber<string, MptError> =
            yield* Effect.fork(optUserEventsProgram.value.mptRoot);
          const depositsRoot = yield* depositsRootFiber;
          yield* Effect.logInfo(`🔹 New deposits root found: ${depositsRoot}`);

          const utxoRoot = yield* ledgerTrie.getRootHex();
          const txRoot = yield* mempoolTrie.getRootHex();

          yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
          yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);

          const { signAndSubmitProgram, txSize } = yield* buildUnsignedTx(
            stateQueueAuthValidator,
            latestBlock,
            utxoRoot,
            txRoot,
            depositsRoot,
            endTime,
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
              Effect.gen(function* () {
                yield* addDepositUTxOsToDatabases(
                  insertedDepositUTxOs,
                );
                return {
                  type: "SuccessfulSubmissionOutput",
                  submittedTxHash: txHash,
                  txSize,
                  mempoolTxsCount: 0,
                  sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
                } as WorkerOutput;
              }),
          });
        }
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. We use the latest transaction's timestamp as the upper
        // bound of the block we are about to submit.
        const endTime = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const optUserEventsProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startTime,
          endTime,
        );

        let depositsRoot: string = yield* emptyRootHexProgram;
        let insertedDepositUTxOs: {
            utxo: CML.TransactionUnspentOutput;
            inclusionTime: Date;
          }[] = []
        if (Option.isSome(optUserEventsProgram)) {
          insertedDepositUTxOs = yield* applyDepositsToLedger(
            ledgerTrie,
            optUserEventsProgram.value.retreivedEvents
          );

          const depositsRootFiber = yield* Effect.fork(
            optUserEventsProgram.value.mptRoot,
          );
          depositsRoot = yield* depositsRootFiber;
        }
        yield* Effect.logInfo(`🔹 New deposits root found: ${depositsRoot}`);

        const utxoRoot = yield* ledgerTrie.getRootHex();
        const txRoot = yield* mempoolTrie.getRootHex();

        yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
        yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);

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
                // With a failed tx submission, we need to carry out the same db
                // logic as the case where no confirmed blocks are available.
                //
                // TODO: Handle failures properly.
                yield* skippedSubmissionProgram(mempoolTxs, mempoolTxHashes);

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
