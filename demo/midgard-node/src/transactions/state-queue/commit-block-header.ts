// Build a tx Merkle root with all the mempool txs

import { WorkerOutput } from "@/utils.js";
import { Effect, Metric } from "effect";
import { Worker } from "worker_threads";

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

export const buildAndSubmitCommitmentBlock = () =>
  Effect.gen(function* () {
    // Toggle global flag to prevent wasted work in case of a merge tx occurring
    // in a conflicting manner.
    global.BLOCK_SUBMISSION_IN_PROGRESS = true;

    const worker = Effect.async<WorkerOutput, Error, never>((resume) => {
      Effect.runSync(Effect.logInfo(`👷 Starting worker...`));
      const worker = new Worker(
        new URL("./commit-block-header.js", import.meta.url),
        {
          workerData: { data: { command: "start" } },
        },
      );
      worker.on("message", (output: WorkerOutput) => {
        if ("error" in output) {
          resume(Effect.fail(new Error(`Error in worker: ${output.error}`)));
        } else {
          resume(Effect.succeed(output));
        }
        worker.terminate();
      });
      worker.on("error", (e: Error) => {
        resume(Effect.fail(new Error(`Error in worker: ${e}`)));
        worker.terminate();
      });
      worker.on("exit", (code: number) => {
        if (code !== 0) {
          resume(Effect.fail(new Error(`Worker exited with code: ${code}`)));
        }
      });
      return Effect.sync(() => {
        worker.terminate();
      });
    });

    const { txSize, mempoolTxsCount, sizeOfBlocksTxs } = yield* worker;

    if (txSize > 0) {
      global.BLOCKS_IN_QUEUE += 1;

      yield* commitBlockTxSizeGauge(Effect.succeed(txSize));
      yield* commitBlockNumTxGauge(Effect.succeed(BigInt(mempoolTxsCount)));
      yield* Metric.increment(commitBlockCounter);
      yield* Metric.incrementBy(commitBlockTxCounter, BigInt(mempoolTxsCount));
      yield* totalTxSizeGauge(Effect.succeed(sizeOfBlocksTxs));

      yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
    }

    // Toggling the flag back to `false` so that merges can continue.
    global.BLOCK_SUBMISSION_IN_PROGRESS = false;
  });
