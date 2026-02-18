import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Match, Metric, pipe } from "effect";
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
  MempoolLedgerDB,
  MempoolDB,
  ProcessedMempoolDB,
  TxPoolDB,
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { fromHex } from "@lucid-evolution/lucid";
import {
  MptError,
  makeMpts,
  processMpts,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import { FileSystemError, batchProgram } from "@/utils.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  PhaseAAccepted,
  QueuedTx,
  RejectCode,
  RejectedTx,
  runPhaseAValidation,
  runPhaseBValidation,
} from "@/validation/index.js";

const BATCH_SIZE = 100;

const validationPhaseALatencyGauge = Metric.gauge("validation_phase_a_latency_ms", {
  description: "Phase-A validation latency in milliseconds",
});
const validationPhaseBLatencyGauge = Metric.gauge("validation_phase_b_latency_ms", {
  description: "Phase-B validation latency in milliseconds",
});
const validationBatchSizeGauge = Metric.gauge("validation_batch_size", {
  description: "Number of queued txs fetched for a validation batch",
  bigint: true,
});
const validationAcceptCounter = Metric.counter("validation_accept_count", {
  description: "Total number of txs accepted by phase-1 validation",
  bigint: true,
  incremental: true,
});
const validationRejectCounter = Metric.counter("validation_reject_count", {
  description: "Total number of txs rejected by phase-1 validation",
  bigint: true,
  incremental: true,
});
const validationQueueDepthGauge = Metric.gauge("validation_queue_depth", {
  description: "Current number of queued transactions awaiting validation",
  bigint: true,
});
const validationWorkerUtilizationGauge = Metric.gauge("validation_worker_utilization", {
  description: "Fraction of configured batch capacity used in the latest validation batch (0-1)",
});

const summarizeRejections = (rejected: readonly RejectedTx[]): string => {
  if (rejected.length === 0) {
    return "none";
  }
  const perCode = rejected.reduce(
    (acc, r) => {
      const count = acc.get(r.code) ?? 0;
      acc.set(r.code, count + 1);
      return acc;
    },
    new Map<RejectCode, number>(),
  );
  return Array.from(perCode.entries())
    .map(([code, count]) => `${code}:${count}`)
    .join(", ");
};

const markRejectedTxs = (rejected: readonly RejectedTx[]) =>
  Effect.forEach(
    rejected,
    (tx) => TxPoolDB.updateRejected(tx.txId, tx.code, tx.detail),
    { concurrency: "unbounded", discard: true },
  );

const queuedToValidationInput = (queued: readonly TxPoolDB.QueuedEntry[]): QueuedTx[] =>
  queued.map((entry) => ({
    txId: entry.tx_id,
    txCbor: entry.tx,
    arrivalSeq: entry.arrival_seq,
    createdAt: entry.created_at,
  }));

const validateQueuedAndPopulateMempool = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const queuedCount = yield* TxPoolDB.retrieveCountByStatus("queued");
  yield* validationQueueDepthGauge(Effect.succeed(queuedCount));

  const queuedRows = yield* TxPoolDB.retrieveQueued(nodeConfig.VALIDATION_BATCH_SIZE);
  yield* validationBatchSizeGauge(Effect.succeed(BigInt(queuedRows.length)));

  if (queuedRows.length === 0) {
    yield* validationWorkerUtilizationGauge(Effect.succeed(0));
    return;
  }

  const phaseAStart = Date.now();
  const phaseA = yield* runPhaseAValidation(queuedToValidationInput(queuedRows), {
    expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
    minFeeA: nodeConfig.MIN_FEE_A,
    minFeeB: nodeConfig.MIN_FEE_B,
    concurrency: nodeConfig.VALIDATION_PHASE_A_CONCURRENCY,
    strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
  });
  yield* validationPhaseALatencyGauge(Effect.succeed(Date.now() - phaseAStart));

  const phaseBStart = Date.now();
  const preState = yield* MempoolLedgerDB.retrieve;
  const phaseB = runPhaseBValidation(phaseA.accepted, preState, {
    nowMillis: BigInt(Date.now()),
  });
  yield* validationPhaseBLatencyGauge(Effect.succeed(Date.now() - phaseBStart));

  const allRejected = [...phaseA.rejected, ...phaseB.rejected];

  yield* markRejectedTxs(allRejected);
  yield* Metric.incrementBy(validationRejectCounter, BigInt(allRejected.length));

  if (phaseB.accepted.length > 0) {
    const acceptedIds = phaseB.accepted.map((tx) => tx.txId);
    const acceptedProcessed = phaseB.accepted.map(
      (tx: PhaseAAccepted) => tx.processedTx,
    );
    yield* MempoolDB.insertMultiple(acceptedProcessed);
    yield* TxPoolDB.updateToAccepted(acceptedIds);
    yield* Metric.incrementBy(
      validationAcceptCounter,
      BigInt(phaseB.accepted.length),
    );
  }

  const utilization =
    nodeConfig.VALIDATION_BATCH_SIZE <= 0
      ? 1
      : queuedRows.length / nodeConfig.VALIDATION_BATCH_SIZE;
  yield* validationWorkerUtilizationGauge(Effect.succeed(utilization));
  yield* Effect.logInfo(
    `🔹 Validation batch done. queued=${queuedRows.length}, accepted=${phaseB.accepted.length}, rejected=${allRejected.length}, rejected_by_code=[${summarizeRejections(allRejected)}]`,
  );
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
    yield* Effect.logInfo("🔹 Running phase-1 validation on queued txs...");
    yield* validateQueuedAndPopulateMempool.pipe(
      Effect.withSpan("phase1-validation"),
    );

    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const mempoolTxs = yield* MempoolDB.retrieve;
    const endTime = Date.now();
    const mempoolTxsCount = mempoolTxs.length;

    if (mempoolTxsCount === 0) {
      yield* Effect.logInfo(
        "🔹 No transactions were found in MempoolDB, checking ProcessedMempoolDB...",
      );

      const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;

      if (processedMempoolTxs.length === 0) {
        yield* Effect.logInfo("🔹 Nothing to commit.");
        return {
          type: "NothingToCommitOutput",
        } as WorkerOutput;
      }
      // No new transactions received, but there are uncommitted transactions in
      // the MPT. So its root must be used to submit a new block, and if
      // successful, `ProcessedMempoolDB` must be cleared. Following functions
      // should work fine with 0 mempool txs.
    }

    yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

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
      const { utxoRoot, txRoot, mempoolTxHashes, sizeOfProcessedTxs } =
        yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs);

      const { policyId, spendScript, spendScriptAddress, mintScript } =
        yield* AlwaysSucceedsContract;

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

        yield* lucid.switchToOperatorsMainWallet;

        const { nodeDatum: updatedNodeDatum, header: newHeader } =
          yield* SDK.Utils.updateLatestBlocksDatumAndGetTheNewHeader(
            lucid.api,
            latestBlock.datum,
            utxoRoot,
            txRoot,
            BigInt(endTime),
          );

        const newHeaderHash = yield* SDK.Utils.hashHeader(newHeader);
        yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

        // Build commitment block
        const commitBlockParams: SDK.TxBuilder.StateQueue.CommitBlockParams = {
          anchorUTxO: latestBlock,
          updatedAnchorDatum: updatedNodeDatum,
          newHeader: newHeader,
          stateQueueSpendingScript: spendScript,
          policyId,
          stateQueueMintingScript: mintScript,
        };

        const aoUpdateCommitmentTimeParams = {};

        yield* Effect.logInfo("🔹 Building block commitment transaction...");
        const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
          stateQueueAddress: spendScriptAddress,
          stateQueuePolicyId: policyId,
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

        // let output: WorkerOutput | undefined = undefined;

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
                        TxPoolDB.updateCommitted(batchHashesForBlocks).pipe(
                          Effect.withSpan(`tx-pool-commit-status-${startIndex}`),
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

const safeProgram = program.pipe(
  Effect.catchAllCause((cause) =>
    Effect.succeed({
      type: "FailureOutput",
      error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
    }),
  ),
) as Effect.Effect<
  WorkerOutput | { type: string; error: string } | undefined,
  never,
  never
>;

Effect.runPromise(
  safeProgram as Effect.Effect<
    WorkerOutput | { type: string; error: string } | undefined,
    never,
    never
  >,
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `👷 Block commitment work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
