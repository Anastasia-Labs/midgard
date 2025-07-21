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
import { fromHex, toHex } from "@lucid-evolution/lucid";
import dotenv from "dotenv";
import { initializeDb } from "../src/database/init.js";
import path from "path";
import { Worker } from "worker_threads";
import { fileURLToPath } from "url";
import fs from "fs";
import {
  LevelDB,
  makeMpts,
  processMpts,
  withTrieTransaction,
} from "../src/workers/db.js";
import { SqlClient } from "@effect/sql";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { Level } from "level";
import { rm } from "fs/promises";
dotenv.config({ path: ".env" });

const NUM_OF_BLOCKS = 5;

const TxRoots = [
  "60ad6944514513cf9a8765be1c170ffa6d3ba5c833b2c0b49bfc5ac2705a3640",
  "f87f4343f08a37c457fec3fa2336d57cbfda804aefb03ff5dafcfbdb42fe65a3",
  "a86b120fa8fb642584b9e4b780ea15fab00affc599b22830925e723a3c4fcee3",
  "3e1e1827c99c228e986d4f4bd8adf26291874ce16c6b80eeecd84c158a1dc2ce",
  "a4932ac746e5c098160ca4fddd34a428b5f9c9daf59b9e855f4c52a211e812db",
];
const UtxoRoots = [
  "06bc3f80780264d6b824ab4e60021aaadaf89333ac37fb388ddba7d56fb8384f",
  "296aedcf0d98fb9907139b2f53198d6bd265e98051b7892b374c85258d45ce96",
  "a77a669b61a8f40c28c93272632de3831f8837ff2170d3956923eb8952604d6b",
  "a3fa49d4ec584d5e764ef61c9ea7eb41f3e6d3a472bfa3fd3e93b92e0e73d726",
  "073b9b40d33068452794b851126c4132888b0c2f7a17cac9241aa01c97a3f4a4",
];

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

describe("CheckpointDB", () => {
  it.effect("Checkpoints, commits and reverts", (_) =>
    Effect.gen(function* () {
      yield* initializeDb();
      yield* flushDb;
      const sql = yield* SqlClient.SqlClient;
      const levelDb = new Level("./test-trie-db", { valueEncoding: "binary" });
      const ledgerTrie = yield* Effect.tryPromise({
        try: () =>
          ETH.createMPT({
            db: new LevelDB(levelDb),
            useRootPersistence: true,
            valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
          }),
        catch: (e) => new Error(`${e}`),
      });
      const root1 = yield* Effect.sync(() => ledgerTrie.root());
      yield* Effect.sync(() => ledgerTrie.checkpoint());
      expect(toHex(root1)).toEqual(toHex(ledgerTrie.EMPTY_TRIE_ROOT));
      // Direct update
      yield* Effect.tryPromise(() =>
        ledgerTrie.put(Buffer.from("aa", "hex"), Buffer.from("bb", "hex")),
      );
      const root2 = yield* Effect.sync(() => ledgerTrie.root());
      yield* Effect.tryPromise(() => ledgerTrie.commit());
      expect(toHex(root2)).toEqual(
        "2569f13bc09fa69b2e624d31238ad9dc3b7be864d593d6eea9aaa1f8565e645e",
      );
      yield* Effect.sync(() => ledgerTrie.checkpoint());
      yield* Effect.tryPromise(() =>
        ledgerTrie.put(Buffer.from("cc", "hex"), Buffer.from("dd", "hex")),
      );
      const root3 = yield* Effect.sync(() => ledgerTrie.root());
      expect(toHex(root3)).toEqual(
        "1dcb2196504dbd63c5b9f7578517267ebdc6267c4a53f45768cee1d1a60eff59",
      );
      yield* Effect.tryPromise(() => ledgerTrie.revert());
      const root4 = yield* Effect.sync(() => ledgerTrie.root());
      expect(toHex(root4)).toEqual(toHex(root2));

      // Transaction update
      yield* Effect.gen(function* () {
        yield* Effect.sync(() => ledgerTrie.checkpoint());
        yield* sql.withTransaction(
          Effect.gen(function* () {
            yield* MempoolDB.insert(
              Buffer.from("ee", "hex"),
              Buffer.from("ff", "hex"),
            );
            yield* Effect.tryPromise(() =>
              ledgerTrie.put(
                Buffer.from("ee", "hex"),
                Buffer.from("ff", "hex"),
              ),
            );
            const root5 = yield* Effect.sync(() => ledgerTrie.root());
            expect(toHex(root5)).toEqual(
              "c2f0030bf8657330fde77f508dea41b4fedf03467add56e6b212548cacf19114",
            );
            yield* Effect.fail(new Error("test"));
          }),
        );
      }).pipe(
        Effect.catchAll((e) =>
          Effect.gen(function* () {
            yield* Effect.tryPromise(() => ledgerTrie.revert());
            Effect.succeed(Effect.void);
          }),
        ),
      );
      const mempoolDB = yield* MempoolDB.retrieve();
      const root5 = yield* Effect.sync(() => ledgerTrie.root());
      expect(toHex(root5)).toEqual(toHex(root2));
      expect(mempoolDB).toEqual([]);

      // Tranasction combinator update
      yield* withTrieTransaction(
        ledgerTrie,
        Effect.gen(function* () {
          yield* MempoolDB.insert(
            Buffer.from("ee", "hex"),
            Buffer.from("ff", "hex"),
          );
          yield* Effect.tryPromise(() =>
            ledgerTrie.put(Buffer.from("ee", "hex"), Buffer.from("ff", "hex")),
          );
          const root5 = yield* Effect.sync(() => ledgerTrie.root());
          expect(toHex(root5)).toEqual(
            "c2f0030bf8657330fde77f508dea41b4fedf03467add56e6b212548cacf19114",
          );
          yield* Effect.fail(new Error("test"));
        }),
      ).pipe(Effect.catchAll((e) => Effect.succeed(Effect.void)));
      const mempoolDBCombinator = yield* MempoolDB.retrieve();
      const root5Combinator = yield* Effect.sync(() => ledgerTrie.root());
      expect(toHex(root5Combinator)).toEqual(toHex(root2));
      expect(mempoolDBCombinator).toEqual([]);
      yield* Effect.tryPromise(() => levelDb.close());
      yield* Effect.tryPromise(async () =>
        rm("./test-trie-db", { recursive: true, force: true }),
      );
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(User.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
});

describe("Commit Block Header Worker", () => {
  it.effect(`should measure performance of MPTs single 5000txs`, (_) =>
    Effect.gen(function* () {
      yield* initializeDb();
      yield* flushDb;

      const blocksTxs = yield* loadTxs;
      let totalTimeMs = 0;

      console.log("Building trees...");
      const transactions = blocksTxs.flat();
      const startTime = performance.now();
      const { ledgerTrie, mempoolTrie } = yield* makeMpts();
      const { utxoRoot, txRoot } = yield* processMpts(
        ledgerTrie,
        mempoolTrie,
        transactions,
      );
      totalTimeMs += performance.now() - startTime;
      yield* MempoolDB.clear();
      const totalTimeSec = totalTimeMs / 1000;
      console.log(`It took ${totalTimeSec.toFixed(2)}s to create`);
      console.log(
        `ledger and mempool trie with ${transactions.length} one-to-one txs each.`,
      );
      expect(utxoRoot).toEqual(
        "2fbd2397169c17c9621387ba47fb2a2858f580e28d2750feb5eec8b371b4f9ea",
      );
      expect(txRoot).toEqual(
        "992bf3cbb70bf1841575b24e8ce5d419c3bb770cac45f944cc653717734e8da7",
      );
      yield* Effect.tryPromise(async () =>
        rm("./midgard-mpt-db", { recursive: true, force: true }),
      );
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(User.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
  //   it.effect(`should measure performance of MPTs 1000 txs 5x`, (_) =>
  //     Effect.gen(function* () {
  //       yield* initializeDb();
  //       yield* flushDb;

  //       const blocksTxs = yield* loadTxs;
  //       let totalTimeMs = 0;
  //       console.log("Building trees...");
  //       for (let blockNumber = 0; blockNumber < NUM_OF_BLOCKS; blockNumber++) {
  //         const transactions = blocksTxs[blockNumber];
  //         for (const [_, tx] of transactions.entries()) {
  //           yield* MempoolDB.insert(tx.key, tx.value);
  //         }
  //         const startTime = performance.now();
  //         const { ledgerTrie, mempoolTrie } = yield* makeMpts();
  //         const {utxoRoot, txRoot} = yield* processMpts(ledgerTrie, mempoolTrie, blocksTxs[blockNumber])
  //         totalTimeMs += performance.now() - startTime;
  //         yield* MempoolDB.clear();
  //         expect(utxoRoot).toEqual(UtxoRoots[blockNumber]);
  //         expect(txRoot).toEqual(TxRoots[blockNumber]);
  //       }
  //       const totalTimeSec = totalTimeMs / 1000;
  //       const avgTimeSec = totalTimeSec / (NUM_OF_BLOCKS * 2);
  //       console.log(`It took ${totalTimeSec.toFixed(2)}s to create`);
  //       console.log(
  //         `${NUM_OF_BLOCKS} ledger and mempool tries with ${blocksTxs[0].length} one-to-one txs each.`,
  //       );
  //       console.log(`Average time per trie: ${avgTimeSec.toFixed(2)}s`);

  //   }).pipe(
  //       Effect.provide(Database.layer),
  //       Effect.provide(User.layer),
  //       Effect.provide(NodeConfig.layer),
  //     )
  // )

  // it.effect(
  //   `should create ${NUM_OF_BLOCKS} blocks sequentially`,
  //   (_) =>
  //     Effect.gen(function* () {
  //       yield* flushDb;
  //       const blocksTxs = yield* loadTxs;
  //       const blocksData: any[] = [];
  //       let totalTransactions = 0;
  //       const startTime = performance.now();

  //       // Iteratively put txs in mempool and run worker until all blocks are done
  //       for (let blockNumber = 0; blockNumber < NUM_OF_BLOCKS; blockNumber++) {
  //         console.log(`\n📦 Processing block ${blockNumber + 1}...`);
  //         const blockStartTime = performance.now();
  //         const transactions = blocksTxs[blockNumber];
  //         totalTransactions += transactions.length;

  //         // Sequentially insert transactions into mempool
  //         for (const [_, tx] of transactions.entries()) {
  //           yield* MempoolDB.insert(tx.key, tx.value);
  //         }

  //         // Execute worker to commit the block
  //         console.log(`🔨 Executing worker for block ${blockNumber + 1}...`);
  //         const workerStartTime = performance.now();

  //         try {
  //           const workerOutput = yield* runMptWorker(blockNumber);
  //           const workerEndTime = performance.now();
  //           console.log(
  //             `⏱️ Worker execution time for block ${blockNumber + 1}: ${workerEndTime - workerStartTime}ms`,
  //           );
  //           console.log(`📊 Block ${blockNumber + 1} output:`, {
  //             mempoolTxsCount: workerOutput.mempoolTxsCount,
  //             txSize: workerOutput.txSize,
  //             sizeOfBlocksTxs: workerOutput.sizeOfBlocksTxs,
  //           });

  //           // Store block data for analysis
  //           blocksData.push({
  //             blockNumber: blockNumber + 1,
  //             mempoolTxsCount: workerOutput.mempoolTxsCount,
  //             txSize: workerOutput.txSize,
  //             sizeOfBlocksTxs: workerOutput.sizeOfBlocksTxs,
  //             executionTime: workerEndTime - workerStartTime,
  //           });
  //         } catch (error) {
  //           console.error(
  //             `❌ Worker failed for block ${blockNumber + 1}:`,
  //             error,
  //           );
  //           throw error;
  //         }

  //         const blockEndTime = performance.now();
  //         console.log(
  //           `⏱️ Total time for block ${blockNumber + 1}: ${((blockEndTime - blockStartTime) / 1000).toFixed(2)}s`,
  //         );
  //       }

  //       const totalEndTime = performance.now();
  //       const totalTime = totalEndTime - startTime;

  //       console.log("\n📈 Performance Summary:");
  //       console.log(
  //         `⏱️ Total test execution time: ${(totalTime / 1000).toFixed(2)}s`,
  //       );
  //       console.log(`📦 Total blocks created: ${blocksData.length}`);
  //       console.log(` Total transactions processed: ${totalTransactions}`);
  //       console.log(
  //         `⚡ Average time per block: ${(totalTime / blocksData.length / 1000).toFixed(2)}s`,
  //       );
  //     }).pipe(
  //       Effect.provide(Database.layer),
  //       Effect.provide(User.layer),
  //       Effect.provide(NodeConfig.layer),
  //     ),
  //   { timeout: 6000_000 },
  // );
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

const loadTxs: Effect.Effect<
  { key: Uint8Array; value: Uint8Array }[][],
  never,
  never
> = Effect.gen(function* () {
  const blocksTxs: { key: Uint8Array; value: Uint8Array }[][] = [];
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

const showKVs = (
  arg: { key: Uint8Array; value: Uint8Array }[],
): { key: string; value: string }[] => {
  return arg.map((a) => ({ key: toHex(a.key), value: toHex(a.value) }));
};
