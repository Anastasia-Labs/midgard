import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule, pipe } from "effect";
import {
  WorkerInput,
  WorkerOutput,
  findSpentAndProducedUTxOs,
} from "@/utils.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { BlocksDB, ImmutableDB, MempoolDB } from "@/database/index.js";
import { handleSignSubmit } from "@/transactions/utils.js";
import { fromHex, toHex } from "@lucid-evolution/lucid";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { PostgresCheckpointDB } from "./db.js";
import { NodeConfig, User } from "@/config.js";
import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";

// Key of the row which its value is the persisted trie root.
const rootKey = ETH.ROOT_DB_KEY;

const wrapper = (
  _input: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User | Database> =>
  Effect.gen(function* () {
    const { user: lucid } = yield* User;
    const sqlClient = yield* SqlClient.SqlClient;
    const nodeConfig = yield* NodeConfig;

    return yield* sqlClient.withTransaction(
      Effect.gen(function* () {
        const mempoolDB = new PostgresCheckpointDB(sqlClient, "mempool");
        const ledgerDB = new PostgresCheckpointDB(sqlClient, "latest_ledger");

        yield* Effect.all([mempoolDB.openEffect(), ledgerDB.openEffect()], {
          concurrency: 2,
        });

        const mempoolTxs = yield* mempoolDB
          .getAllEffect()
          .pipe(Effect.withSpan("retrieve mempool transaction"));
        const mempoolTxsCount = mempoolTxs.length;

        if (mempoolTxsCount === 0) {
          yield* Effect.logInfo("🔹 No transactions were found in MempoolDB");
          const output: WorkerOutput = {
            txSize: 0,
            mempoolTxsCount: 0,
            sizeOfBlocksTxs: 0,
          };
          return output;
        }

        const endTime = Date.now();
        yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

        const mempoolTrie = yield* Effect.tryPromise(() =>
          ETH.createMPT({
            db: mempoolDB.db,
            valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
          }),
        );

        const ledgerTrie = yield* Effect.tryPromise(() =>
          ETH.createMPT({
            db: ledgerDB.db,
            useRootPersistence: true,
            valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
          }),
        );

        const ledgerRootBeforeMempoolTxs = yield* Effect.tryPromise({
          try: () => ledgerTrie.get(rootKey),
          catch: (e) => new Error(`${e}`),
        }).pipe(
          Effect.orElse(() => Effect.succeed(ledgerTrie.EMPTY_TRIE_ROOT)),
        );

        // Ensuring persisted root is stored in trie's private property. Looking
        // at `mpt`'s source code, initializing an MPT does NOT seem to
        // automatically pull a previously stored root from the database.
        yield* Effect.sync(() => ledgerTrie.root(ledgerRootBeforeMempoolTxs));

        const mempoolTxHashes: Uint8Array[] = [];
        let sizeOfBlocksTxs = 0;

        yield* Effect.logInfo(
          "🔹 Going through mempool txs and finding roots...",
        );
        yield* Effect.forEach(mempoolTxs, ({ key: txHash, value: txCbor }) =>
          Effect.gen(function* () {
            mempoolTxHashes.push(txHash);

            sizeOfBlocksTxs += txCbor.length;

            yield* Effect.tryPromise({
              try: () => mempoolTrie.put(txHash, txCbor),
              catch: (e) => new Error(`${e}`),
            });

            const { spent, produced } = yield* findSpentAndProducedUTxOs(
              txCbor,
            ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));

            const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef) => ({
              type: "del",
              key: outRef,
            }));

            const putOps: ETH_UTILS.BatchDBOp[] = produced.map(
              ({ key: outputReference, value: output }) => ({
                type: "put",
                key: outputReference,
                value: output,
              }),
            );

            const batchDBOps: ETH_UTILS.BatchDBOp[] = [...delOps, ...putOps];

            yield* Effect.tryPromise({
              try: () => ledgerTrie.batch(batchDBOps),
              catch: (e) => new Error(`${e}`),
            });
          }),
        );

        const utxoRoot = toHex(ledgerTrie.root());
        const txRoot = toHex(mempoolTrie.root());

        yield* Effect.logInfo(`🔹 Mempool tx root found: ${txRoot}`);
        yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

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
        yield* handleSignSubmit(lucid, txBuilder).pipe(
          Effect.withSpan("handleSignSubmit-commit-block"),
        );

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
if (parentPort === null) {
  throw new Error("MPT computation must be run as a worker");
}

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
