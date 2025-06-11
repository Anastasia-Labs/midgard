import { Effect } from "effect";
import { Database } from "../src/services/database.js";
import { performance } from "perf_hooks";
import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { NodeConfig, User } from "../src/config.js";
import {
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
} from "../src/database/index.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { fromHex } from "@lucid-evolution/lucid";
import dotenv from "dotenv";
import { initializeDb } from "@/database/init.js";
import path from "path";
import { Worker } from "worker_threads";
import { fileURLToPath } from "url";
import fs from "fs";
import { mkMPTs } from "@/workers/db.js";
dotenv.config({ path: ".env" });

const NUM_OF_BLOCKS = 5;

const runMptWorker = (
  workerInput: number,
): Effect.Effect<
  { txSize: number; mempoolTxsCount: number; sizeOfBlocksTxs: number },
  Error
> =>
  Effect.async<
    { txSize: number; mempoolTxsCount: number; sizeOfBlocksTxs: number },
    Error
  >((resume) => {
    // IMPORTANT: The path must point to the COMPILED JavaScript file in your 'dist' directory.
    const __filename = fileURLToPath(import.meta.url);
    const __dirname = path.dirname(__filename);
    const workerPath = path.join(
      __dirname,
      "../dist/commit-block-header.js", // Adjust path as needed
    );

    const worker = new Worker(workerPath);

    // Listen for the final message from the worker
    worker.on("message", (output) => {
      resume(Effect.succeed(output));
    });

    // Handle errors from the worker
    worker.on("error", (err) => {
      resume(Effect.fail(err));
    });

    // Handle unexpected exits
    worker.on("exit", (code) => {
      if (code !== 0) {
        resume(Effect.fail(new Error(`Worker stopped with exit code ${code}`)));
      }
    });

    // Send the input data to start the worker's job
    worker.postMessage(workerInput);

    // This is the cleanup function that Effect will call if the fiber is interrupted.
    return Effect.sync(() => {
      worker.terminate();
    });
  });

describe("Commit Block Header Worker", () => {
  it.effect(`should measure performance of MPTs`, (_) =>
    Effect.gen(function* () {
      yield* initializeDb();
      yield* flushDb;

      const blocksTxs = yield* loadTxs;
      const startTime = performance.now();
      console.log("Building trees...");
      for (let blockNumber = 0; blockNumber < NUM_OF_BLOCKS; blockNumber++) {
        yield* mkMPTs(blocksTxs[blockNumber]);
      }
      const totalTime = (performance.now() - startTime) / 1000;
      const avgTime = totalTime / (NUM_OF_BLOCKS * 2);
      console.log(`It took ${totalTime.toFixed(2)}s to create`);
      console.log(
        `${NUM_OF_BLOCKS} ledger and mempool tries with 1000 one-to-one txs each.`,
      );
      console.log(`Average time per trie: ${avgTime.toFixed(2)}s`);
      expect(avgTime).toBeLessThan(10);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(AlwaysSucceedsContract.layer),
      Effect.provide(User.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

  it.effect(
    `should create ${NUM_OF_BLOCKS} blocks sequentially`,
    (_) =>
      Effect.gen(function* () {
        yield* flushDb;
        const blocksTxs = yield* loadTxs;
        const blocksData: any[] = [];
        let totalTransactions = 0;
        const startTime = performance.now();

        // Iteratively put txs in mempool and run worker until all blocks are done
        for (let blockNumber = 0; blockNumber < NUM_OF_BLOCKS; blockNumber++) {
          console.log(`\n📦 Processing block ${blockNumber + 1}...`);
          const blockStartTime = performance.now();
          const transactions = blocksTxs[blockNumber].slice(100);
          totalTransactions += transactions.length;

          // Sequentially insert transactions into mempool
          for (const [_, tx] of transactions.entries()) {
            yield* MempoolDB.insert(tx.key, tx.value);
          }

          // Execute worker to commit the block
          console.log(`🔨 Executing worker for block ${blockNumber + 1}...`);
          const workerStartTime = performance.now();

          try {
            const workerOutput = yield* runMptWorker(blockNumber);
            const workerEndTime = performance.now();
            console.log(
              `⏱️ Worker execution time for block ${blockNumber + 1}: ${workerEndTime - workerStartTime}ms`,
            );
            console.log(`📊 Block ${blockNumber + 1} output:`, {
              mempoolTxsCount: workerOutput.mempoolTxsCount,
              txSize: workerOutput.txSize,
              sizeOfBlocksTxs: workerOutput.sizeOfBlocksTxs,
            });

            // Store block data for analysis
            blocksData.push({
              blockNumber: blockNumber + 1,
              mempoolTxsCount: workerOutput.mempoolTxsCount,
              txSize: workerOutput.txSize,
              sizeOfBlocksTxs: workerOutput.sizeOfBlocksTxs,
              executionTime: workerEndTime - workerStartTime,
            });

            expect(workerOutput.mempoolTxsCount).toBeGreaterThan(0);
          } catch (error) {
            console.error(
              `❌ Worker failed for block ${blockNumber + 1}:`,
              error,
            );
            throw error;
          }

          const blockEndTime = performance.now();
          console.log(
            `⏱️ Total time for block ${blockNumber + 1}: ${((blockEndTime - blockStartTime) / 1000).toFixed(2)}s`,
          );
        }

        const totalEndTime = performance.now();
        const totalTime = totalEndTime - startTime;

        console.log("\n📈 Performance Summary:");
        console.log(
          `⏱️ Total test execution time: ${(totalTime / 1000).toFixed(2)}s`,
        );
        console.log(`📦 Total blocks created: ${blocksData.length}`);
        console.log(` Total transactions processed: ${totalTransactions}`);
        console.log(
          `⚡ Average time per block: ${(totalTime / blocksData.length / 1000).toFixed(2)}s`,
        );
      }).pipe(
        Effect.provide(Database.layer),
        Effect.provide(AlwaysSucceedsContract.layer),
        Effect.provide(User.layer),
        Effect.provide(NodeConfig.layer),
      ),
    { timeout: 6000_000 },
  );
});

const flushDb = Effect.gen(function* () {
  yield* Effect.all(
    [
      MempoolDB.clear(),
      MempoolLedgerDB.clear(),
      BlocksDB.clear(),
      ImmutableDB.clear(),
      LatestLedgerDB.clear(),
      ConfirmedLedgerDB.clear(),
    ],
    { discard: true },
  );
});

const loadTxs = Effect.gen(function* () {
  const blocksTxs = [];
  for (let blockNumber = 0; blockNumber < NUM_OF_BLOCKS; blockNumber++) {
    const txsPath = path.resolve(__dirname, `txs/txs_${blockNumber}.json`);
    const txs: { cborHex: string; txId: string }[] = JSON.parse(
      fs.readFileSync(txsPath, "utf-8"),
    );
    blocksTxs.push(
      txs.map((tx) => ({ key: fromHex(tx.txId), value: fromHex(tx.cborHex) })),
    );
  }
  return blocksTxs;
});
