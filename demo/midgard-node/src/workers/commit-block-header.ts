import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule, pipe } from "effect";
import {
  WorkerInput,
  WorkerOutput,
  findSpentAndProducedUTxOs,
} from "@/utils.js";
import { NodeConfig, User } from "@/config.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  BlocksDB,
  ImmutableDB,
  LatestLedgerCloneDB,
  LatestLedgerDB,
  MempoolDB,
} from "@/database/index.js";
import { handleSignSubmit } from "@/transactions/utils.js";
import { fromHex } from "@lucid-evolution/lucid";
import postgres from "postgres";

const wrapper = (
  _input: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User> =>
  // ) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;

    const db = postgres({
      host: nodeConfig.POSTGRES_HOST,
      username: nodeConfig.POSTGRES_USER,
      password: nodeConfig.POSTGRES_PASSWORD,
      database: nodeConfig.POSTGRES_DB,
      max: 20,
      idle_timeout: 30,
      connect_timeout: 2,
    });

    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");
    const mempoolTxs = yield* Effect.tryPromise({
      try: () => MempoolDB.retrieve(db),
      catch: (e) => new Error(`${e}`),
    }).pipe(Effect.withSpan("retrieve mempool transaction"));

    const mempoolTxsCount = mempoolTxs.length;

    if (mempoolTxsCount > 0) {
      const endTime = Date.now();
      yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

      // const latestLedgerUTxOs = yield* Effect.tryPromise(() =>
      //   LatestLedgerDB.retrieve(pool),
      // ).pipe(Effect.withSpan("retrieve latest ledger utxo list"));

      // let updatedLatestLedgerUTxOs = latestLedgerUTxOs;

      const utxoStore = new Store("utxosStore");
      const txsStore = new Store("txsStore");
      yield* Effect.tryPromise({
        try: () => utxoStore.ready(),
        catch: (e) => new Error(`${e}`),
      });
      yield* Effect.tryPromise({
        try: () => txsStore.ready(),
        catch: (e) => new Error(`${e}`),
      });
      const utxoTrie = new Trie(utxoStore);
      const txsTrie = new Trie(txsStore);

      yield* Effect.tryPromise({
        try: () => LatestLedgerCloneDB.clear(db),
        catch: (e) => new Error(`${e}`),
      });
      yield* Effect.tryPromise({
        try: () => db`
          INSERT INTO ${db(LatestLedgerCloneDB.tableName)}
          SELECT * FROM ${db(LatestLedgerCloneDB.tableName)}
        `,
        catch: (e) => new Error(`${e}`),
      });

      const mempoolTxHashes: Uint8Array[] = [];
      let sizeOfBlocksTxs = 0;

      yield* Effect.logInfo(
        "🔹 Going through mempool txs and finding roots...",
      );
      yield* Effect.forEach(mempoolTxs, ({ txHash, txCbor }) =>
        Effect.gen(function* () {
          mempoolTxHashes.push(txHash);
          // mempoolTxCbors.push(txCbor);

          sizeOfBlocksTxs += txCbor.length;

          yield* Effect.tryPromise({
            try: () => txsTrie.insert(Buffer.from(txHash), Buffer.from(txCbor)),
            catch: (e) => new Error(`${e}`),
          });

          const { spent, produced } = yield* findSpentAndProducedUTxOs(
            txCbor,
          ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));

          yield* Effect.tryPromise({
            try: () => LatestLedgerCloneDB.clearUTxOs(db, spent),
            catch: (e) => new Error(`${e}`),
          });
          yield* Effect.tryPromise({
            try: () => LatestLedgerCloneDB.insert(db, produced),
            catch: (e) => new Error(`${e}`),
          });
        }),
      );

      const updatedLatestLedgerUTxOs = yield* Effect.tryPromise({
        try: () => LatestLedgerCloneDB.retrieve(db),
        catch: (e) => new Error(`${e}`),
      });

      const txRoot = txsTrie.hash.toString("hex");

      yield* Effect.forEach(
        updatedLatestLedgerUTxOs,
        ({ outputReference, output }) =>
          Effect.tryPromise({
            try: () =>
              utxoTrie.insert(
                Buffer.from(outputReference),
                Buffer.from(output),
              ),
            catch: (e) => new Error(`${e}`),
          }),
      );

      const utxoRoot = utxoTrie.hash.toString("hex");

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
      const latestBlock = yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
        lucid,
        fetchConfig,
      ).pipe(
        Effect.retry(retryPolicy),
        Effect.withSpan("fetchLatestCommittedBlockProgram"),
      );

      yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
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

      yield* Effect.logInfo("🔹 Clearing LatestLedgerDB...");
      yield* Effect.tryPromise({
        try: () => LatestLedgerDB.clear(db),
        catch: (e) => new Error(`${e}`),
      });

      yield* Effect.logInfo("🔹 Inserting updated UTxO set LatestLedgerDB...");
      yield* Effect.tryPromise({
        try: () => db`
          INSERT INTO ${db(LatestLedgerCloneDB.tableName)}
          SELECT * FROM ${db(LatestLedgerCloneDB.tableName)}
        `,
        catch: (e) => new Error(`${e}`),
      });

      yield* Effect.logInfo(
        "🔹 Inserting included transactions into ImmutableDB and BlocksDB...",
      );
      for (let i = 0; i < mempoolTxsCount; i += batchSize) {
        yield* Effect.tryPromise({
          try: () =>
            ImmutableDB.insertTxs(db, mempoolTxs.slice(i, i + batchSize)),
          catch: (e) => new Error(`${e}`),
        }).pipe(Effect.withSpan(`immutable-db-insert-${i}`));

        yield* Effect.tryPromise({
          try: () =>
            BlocksDB.insert(
              db,
              fromHex(newHeaderHash),
              mempoolTxHashes.slice(i, i + batchSize),
            ),
          catch: (e) => new Error(`${e}`),
        }).pipe(Effect.withSpan(`immutable-db-insert-${i}`));
      }

      yield* Effect.logInfo(
        "🔹 Clearing included transactions from MempoolDB...",
      );
      yield* Effect.tryPromise({
        try: () => MempoolDB.clearTxs(db, mempoolTxHashes),
        catch: (e) => new Error(`${e}`),
      }).pipe(Effect.withSpan("clear mempool"));

      const output: WorkerOutput = {
        txSize,
        mempoolTxsCount,
        sizeOfBlocksTxs,
      };

      return output;
    } else {
      yield* Effect.logInfo("🔹 No transactions were found in MempoolDB.");
      const output: WorkerOutput = {
        txSize: 0,
        mempoolTxsCount: 0,
        sizeOfBlocksTxs: 0,
      };
      return output;
    }
  });

if (parentPort === null) {
  throw new Error("MPT computation must be run as a worker");
}

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
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
