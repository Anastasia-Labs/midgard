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
import pg from "pg";
import {
  BlocksDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  UtilsDB,
} from "@/database/index.js";
import { handleSignSubmit } from "@/transactions/utils.js";
import { fromHex } from "@lucid-evolution/lucid";

const wrapper = (
  _input: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User> =>
  // ) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;

    const pool = new pg.Pool({
      host: nodeConfig.POSTGRES_HOST,
      user: nodeConfig.POSTGRES_USER,
      password: nodeConfig.POSTGRES_PASSWORD,
      database: nodeConfig.POSTGRES_DB,
      max: 20,
      idleTimeoutMillis: 30000,
      connectionTimeoutMillis: 2000,
    });

    const client = yield* Effect.tryPromise({
      try: () => pool.connect(),
      catch: (e) => new Error(`${e}`),
    });

    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");
    const mempoolTxs = yield* Effect.tryPromise({
      try: () => MempoolDB.retrieve(pool),
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
      const tempTableName = `temp_${LatestLedgerDB.tableName}`;
      yield* Effect.tryPromise({
        try: () =>
          client.query(`
CREATE TEMPORARY TABLE ${tempTableName}
AS
SELECT * FROM ${LatestLedgerDB.tableName}`),
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
            try: () => UtilsDB.clearUTxOs(client, tempTableName, spent),
            catch: (e) => new Error(`${e}`),
          });
          yield* Effect.tryPromise({
            try: () => UtilsDB.insertUTxOsCBOR(client, tempTableName, produced),
            catch: (e) => new Error(`${e}`),
          });

          // updatedLatestLedgerUTxOs = [...updatedLatestLedgerUTxOs.filter(
          //   (utxo) => !spent.some((spent) => utxo.outputReference == spent),
          // ), ...produced];
        }),
      );

      const updatedLatestLedgerUTxOs = yield* Effect.tryPromise({
        try: () => UtilsDB.retrieveUTxOsCBOR(client, tempTableName),
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
        try: () => UtilsDB.clearTable(client, LatestLedgerDB.tableName),
        catch: (e) => new Error(`${e}`),
      });

      yield* Effect.logInfo("🔹 Inserting updated UTxO set LatestLedgerDB...");
      yield* Effect.tryPromise({
        try: () =>
          client.query(`
INSERT INTO ${LatestLedgerDB.tableName}
SELECT * FROM ${tempTableName}
`),
        catch: (e) => new Error(`${e}`),
      });

      yield* Effect.logInfo(
        "🔹 Inserting included transactions into ImmutableDB and BlocksDB...",
      );
      for (let i = 0; i < mempoolTxsCount; i += batchSize) {
        yield* Effect.tryPromise({
          try: () =>
            ImmutableDB.insertTxs(pool, mempoolTxs.slice(i, i + batchSize)),
          catch: (e) => new Error(`${e}`),
        }).pipe(Effect.withSpan(`immutable-db-insert-${i}`));

        yield* Effect.tryPromise({
          try: () =>
            BlocksDB.insert(
              pool,
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
        try: () => MempoolDB.clearTxs(pool, mempoolTxHashes),
        catch: (e) => new Error(`${e}`),
      }).pipe(Effect.withSpan("clear mempool"));

      client.release();

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
