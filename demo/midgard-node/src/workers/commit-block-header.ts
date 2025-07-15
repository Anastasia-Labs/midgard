import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule, pipe } from "effect";
import { WorkerInput, WorkerOutput } from "@/utils.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { BlocksDB, ImmutableDB, MempoolDB } from "@/database/index.js";
import {
  ConfirmError,
  handleSignSubmit,
  SubmitError,
} from "@/transactions/utils.js";
import { fromHex } from "@lucid-evolution/lucid";
import { makeMpts, processMpts, withTrieTransaction } from "./db.js";
import { NodeConfig, User } from "@/config.js";
import { Database } from "@/services/database.js";

const emptyOutput: WorkerOutput = {
  txSize: 0,
  mempoolTxsCount: 0,
  sizeOfBlocksTxs: 0,
};

const wrapper = (
  _input: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User | Database> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;

    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");
    const mempoolTxs = yield* MempoolDB.retrieve();
    const mempoolTxsCount = mempoolTxs.length;

    if (mempoolTxsCount === 0) {
      yield* Effect.logInfo("🔹 No transactions were found in MempoolDB");
      return emptyOutput;
    }
    const endTime = Date.now();
    yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

    const { ledgerTrie, mempoolTrie } = yield* makeMpts();

    lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);

    return yield* withTrieTransaction(
      ledgerTrie,
      Effect.gen(function* () {
        const { utxoRoot, txRoot, mempoolTxHashes, sizeOfBlocksTxs } =
          yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs);
        const { policyId, spendScript, spendScriptAddress, mintScript } =
          yield* makeAlwaysSucceedsServiceFn(nodeConfig);
        const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
          stateQueueAddress: spendScriptAddress,
          stateQueuePolicyId: policyId,
        };
        const retryPolicy = Schedule.exponential("100 millis").pipe(
          Schedule.compose(Schedule.recurs(4)),
        );
        yield* Effect.logInfo("🔹 Fetching latest commited block...");
        const latestBlock =
          yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
            lucid,
            fetchConfig,
          ).pipe(
            Effect.retry(retryPolicy),
            Effect.withSpan("fetchLatestCommittedBlockProgram"),
          );
        yield* Effect.logInfo(
          "🔹 Finding updated block datum and new header...",
        );
        const { nodeDatum: updatedNodeDatum, header: newHeader } =
          yield* SDK.Utils.updateLatestBlocksDatumAndGetTheNewHeader(
            lucid,
            latestBlock,
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
        const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
          lucid,
          fetchConfig,
          commitBlockParams,
          aoUpdateCommitmentTimeParams,
        );
        const txSize = txBuilder.toCBOR().length / 2;
        yield* Effect.logInfo(
          `🔹 Transaction built successfully. Size: ${txSize}`,
        );

        // Using sign and submit helper with confirmation so that databases are
        // only updated after a successful on-chain registration of the block.
        const onSubmitFailure = (err: SubmitError) =>
          Effect.gen(function* () {
            yield* Effect.logError(`Sumbit tx error: ${err}`);
            yield* Effect.fail(err.err);
          });
        const onConfirmFailure = (err: ConfirmError) =>
          Effect.logError(`Confirm tx error: ${err}`);
        yield* handleSignSubmit(
          lucid,
          txBuilder,
          onSubmitFailure,
          onConfirmFailure,
        ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

        const batchSize = 100;

        yield* Effect.logInfo(
          "🔹 Inserting included transactions into ImmutableDB and BlocksDB...",
        );

        const batchIndices = Array.from(
          { length: Math.ceil(mempoolTxsCount / batchSize) },
          (_, i) => i * batchSize,
        );
        yield* Effect.forEach(
          batchIndices,
          (startIndex) => {
            const endIndex = startIndex + batchSize;
            const batchTxs = mempoolTxs.slice(startIndex, endIndex);
            const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);

            return pipe(
              Effect.all(
                [
                  ImmutableDB.insertTxs(batchTxs).pipe(
                    Effect.withSpan(`immutable-db-insert-${startIndex}`),
                  ),
                  BlocksDB.insert(fromHex(newHeaderHash), batchHashes).pipe(
                    Effect.withSpan(`blocks-db-insert-${startIndex}`),
                  ),
                ],
                { concurrency: 2 },
              ),
              Effect.withSpan(`batch-insert-${startIndex}-${endIndex}`),
            );
          },
          { concurrency: batchIndices.length },
        );

        yield* Effect.logInfo(
          "🔹 Clearing included transactions from MempoolDB...",
        );
        yield* MempoolDB.clearTxs(mempoolTxHashes).pipe(
          Effect.withSpan("clear mempool"),
        );

        const output: WorkerOutput = {
          txSize,
          mempoolTxsCount,
          sizeOfBlocksTxs,
        };
        return output;
      }),
    );
  });

if (parentPort === null)
  throw new Error("MPT computation must be run as a worker");

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
  Effect.provide(Database.layer),
  Effect.provide(User.layer),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAll((e) =>
      Effect.succeed({
        error: e instanceof Error ? e.message : "Unknown error from MPT worker",
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(`👷 Work completed (${JSON.stringify(output)}).`),
  );
  parentPort?.postMessage(output);
});
