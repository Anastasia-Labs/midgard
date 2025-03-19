// Build a tx Merkle root with all the mempool txs

import { makeConfig } from "@/config.js";
import {
  BlocksDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
} from "@/database/index.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import {
  WorkerInput,
  WorkerOutput,
  findAllSpentAndProducedUTxOs,
} from "@/utils.js";
import { UtilsTx } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, utxoToCore } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";
import pg from "pg";
import { handleSignSubmit } from "../utils.js";
import { Worker } from "worker_threads";
import path from "path";

const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in the commit block",
});

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in the commit block",
  bigint: true,
  incremental: true,
});

const commitBlockTxSizeGauge = Metric.gauge("commit_block_tx_size", {
  description: "A gauge for tracking the size of the commit block transaction",
});

// Apply mempool txs to LatestLedgerDB, and find the new UTxO set

// Update LatestLedgerDB to store this updated set

// Clear included transactions from MempoolDB, and inject them into ImmutableDB

// Build a Merkle root using this updated UTxO set

// Build and submit the commitment block using these 2 roots

export const buildAndSubmitCommitmentBlock = (
  lucid: LucidEvolution,
  db: pg.Pool,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  endTime: number,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");
    const mempoolTxs = yield* Effect.tryPromise(() =>
      MempoolDB.retrieve(db),
    ).pipe(Effect.withSpan("retrieve mempool transaction"));
    const mempoolTxsCount = BigInt(mempoolTxs.length);

    if (mempoolTxsCount > 0n) {
      yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

      const mempoolTxCbors = mempoolTxs.map((tx) => tx.txCbor);
      const mempoolTxHashes = mempoolTxs.map((tx) => tx.txHash);

      const { spent: spentList, produced: producedList } =
        yield* findAllSpentAndProducedUTxOs(mempoolTxCbors).pipe(
          Effect.withSpan("findAllSpentAndProducedUTxOs"),
        );

      const latestLedgerUTxOs = yield* Effect.tryPromise(() =>
        LatestLedgerDB.retrieve(db),
      ).pipe(Effect.withSpan("retrieve latest ledger utxo list"));

      // Remove spent UTxOs from latestLedgerUTxOs
      const filteredUTxOList = latestLedgerUTxOs.filter(
        (utxo) =>
          !spentList.some((spent) => UtilsTx.outRefsAreEqual(utxo, spent)),
      );

      // Merge filtered latestLedgerUTxOs with producedList
      const newLatestLedger = [...filteredUTxOList, ...producedList];

      const workerHelper = (input: WorkerInput) =>
        Effect.async<string, Error, never>((resume) => {
          Effect.runSync(Effect.logInfo("👷 Starting worker..."));
          const worker = new Worker(new URL("./mpt.js", import.meta.url), {
            workerData: input,
          });
          worker.on("message", (output: WorkerOutput) => {
            if ("error" in output) {
              resume(
                Effect.fail(new Error(`Error in worker: ${output.error}`)),
              );
            } else {
              resume(Effect.succeed(output.root));
            }
            worker.terminate();
          });
          worker.on("error", (e: Error) => {
            resume(Effect.fail(new Error(`Error in worker: ${e}`)));
            worker.terminate();
          });
          worker.on("exit", (code: number) => {
            if (code !== 0) {
              resume(
                Effect.fail(new Error(`Worker exited with code: ${code}`)),
              );
            }
          });
          return Effect.sync(() => {
            worker.terminate();
          });
        });

      const txRootWorkerProgram = workerHelper({
        data: {
          items: mempoolTxs,
          itemsType: "txs",
        },
      }).pipe(Effect.withSpan("txRootWorkerProgram"));

      const utxoRootWorkerProgram = workerHelper({
        data: {
          items: newLatestLedger.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
          itemsType: "utxos",
        },
      }).pipe(Effect.withSpan("utxoRootWorkerProgram"));

      yield* Effect.logInfo("🔹 Building MPT roots...");
      const [txRoot, utxoRoot] = yield* Effect.all(
        [txRootWorkerProgram, utxoRootWorkerProgram],
        { concurrency: 2 },
      );

      yield* Effect.logInfo(`🔹 Mempool tx root found: ${txRoot}`);
      yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

      const nodeConfig = yield* makeConfig;

      const { policyId, spendScript, mintScript } =
        yield* makeAlwaysSucceedsServiceFn(nodeConfig);

      yield* Effect.logInfo("🔹 Fetching latest commited block...");
      const latestBlock = yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
        lucid,
        fetchConfig,
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

      const totalTxSize = mempoolTxCbors.reduce(
        (acc, cbor) => acc + cbor.length / 2,
        0,
      );

      yield* commitBlockTxSizeGauge(Effect.succeed(txSize));
      yield* commitBlockNumTxGauge(Effect.succeed(mempoolTxsCount));
      yield* Metric.increment(commitBlockCounter);
      yield* Metric.incrementBy(commitBlockTxCounter, mempoolTxsCount);
      yield* totalTxSizeGauge(Effect.succeed(totalTxSize));

      const batchSize = 100;

      yield* Effect.logInfo("🔹 Clearing spent UTxOs from LatestLedgerDB...");
      for (let i = 0; i < spentList.length; i += batchSize) {
        yield* Effect.tryPromise(() =>
          LatestLedgerDB.clearUTxOs(db, spentList.slice(i, i + batchSize)),
        ).pipe(Effect.withSpan(`latest-ledger-clearUTxOs-${i}`));
      }

      yield* Effect.logInfo(
        "🔹 Inserting produced UTxOs into LatestLedgerDB...",
      );
      for (let i = 0; i < producedList.length; i += batchSize) {
        yield* Effect.tryPromise(() =>
          LatestLedgerDB.insert(db, producedList.slice(i, i + batchSize)),
        ).pipe(Effect.withSpan(`latest-ledger-insert-${i}`));
      }

      yield* Effect.logInfo(
        "🔹 Inserting included transactions into ImmutableDB and BlocksDB...",
      );
      for (let i = 0; i < mempoolTxsCount; i += batchSize) {
        yield* Effect.tryPromise(() =>
          ImmutableDB.insertTxs(db, mempoolTxs.slice(i, i + batchSize)),
        ).pipe(Effect.withSpan(`immutable-db-insert-${i}`));

        yield* Effect.tryPromise(() =>
          BlocksDB.insert(
            db,
            newHeaderHash,
            mempoolTxHashes.slice(i, i + batchSize),
          ),
        ).pipe(Effect.withSpan(`immutable-db-insert-${i}`));
      }

      yield* Effect.logInfo(
        "🔹 Clearing included transactions from MempoolDB...",
      );
      yield* Effect.tryPromise(() =>
        MempoolDB.clearTxs(db, mempoolTxHashes),
      ).pipe(Effect.withSpan("clear mempool"));

      yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
    } else {
      yield* Effect.logInfo("🔹 No transactions were found in MempoolDB.");
    }
  });
