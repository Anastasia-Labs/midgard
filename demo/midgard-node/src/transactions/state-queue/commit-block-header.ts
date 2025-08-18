import { WorkerOutput } from "@/workers/utils/commit-block-header.js";
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

export const buildAndSubmitCommitmentBlock = () =>
  Effect.gen(function* () {
    if (global.RESET_IN_PROGRESS) {
      return yield* Effect.logInfo("🔹 Reset in progress...");
    }
    const worker = Effect.async<WorkerOutput, Error, never>((resume) => {
      Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
      const worker = new Worker(
        new URL("./commit-block-header.js", import.meta.url),
        {
          workerData: {
            data: {
              availableConfirmedBlock: global.AVAILABLE_CONFIRMED_BLOCK,
              mempoolTxsCountSoFar: global.PROCESSED_UNSUBMITTED_TXS_COUNT,
              sizeOfProcessedTxsSoFar: global.PROCESSED_UNSUBMITTED_TXS_SIZE,
            },
          },
        },
      );
      worker.on("message", (output: WorkerOutput) => {
        if (output.type === "FailureOutput") {
          resume(
            Effect.fail(
              new Error(`Error in commitment worker: ${output.error}`),
            ),
          );
        } else {
          resume(Effect.succeed(output));
        }
        worker.terminate();
      });
      worker.on("error", (e: Error) => {
        resume(Effect.fail(new Error(`Error in commitment worker: ${e}`)));
        worker.terminate();
      });
      worker.on("exit", (code: number) => {
        if (code !== 0) {
          resume(
            Effect.fail(
              new Error(`Commitment worker exited with code: ${code}`),
            ),
          );
        }
      });
      return Effect.sync(() => {
        worker.terminate();
      });
    });

    const workerOutput: WorkerOutput = yield* worker;

    switch (workerOutput.type) {
      case "SuccessfulSubmissionOutput": {
        global.BLOCKS_IN_QUEUE += 1;
        global.AVAILABLE_CONFIRMED_BLOCK = "";
        global.UNCONFIRMED_SUBMITTED_BLOCK = workerOutput.submittedTxHash;
        global.PROCESSED_UNSUBMITTED_TXS_COUNT = 0;
        global.PROCESSED_UNSUBMITTED_TXS_SIZE = 0;
        yield* commitBlockTxSizeGauge(Effect.succeed(workerOutput.txSize));
        yield* commitBlockNumTxGauge(
          Effect.succeed(BigInt(workerOutput.mempoolTxsCount)),
        );
        yield* Metric.increment(commitBlockCounter);
        yield* Metric.incrementBy(
          commitBlockTxCounter,
          BigInt(workerOutput.mempoolTxsCount),
        );
        yield* totalTxSizeGauge(Effect.succeed(workerOutput.sizeOfBlocksTxs));
        yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
        break;
      }
      case "SkippedSubmissionOutput": {
        global.PROCESSED_UNSUBMITTED_TXS_COUNT += workerOutput.mempoolTxsCount;
        global.PROCESSED_UNSUBMITTED_TXS_SIZE +=
          workerOutput.sizeOfProcessedTxs;
        break;
      }
      case "EmptyMempoolOutput": {
        break;
      }
      case "FailureOutput": {
        break;
      }
    }
  });
