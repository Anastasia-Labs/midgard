import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { Effect, Metric, Ref } from "effect";
import { Worker } from "worker_threads";
import { WorkerError } from "@/workers/utils/common.js";
import { Globals } from "@/services/globals.js";

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
    const globals = yield* Globals;
    const AVAILABLE_CONFIRMED_BLOCK = yield* globals.AVAILABLE_CONFIRMED_BLOCK;
    const PROCESSED_UNSUBMITTED_TXS_COUNT =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_COUNT;
    const PROCESSED_UNSUBMITTED_TXS_SIZE =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_SIZE;

    const worker = Effect.async<WorkerOutput, WorkerError, never>((resume) => {
      Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
      const worker = new Worker(
        new URL("./commit-block-header.js", import.meta.url),
        {
          workerData: {
            data: {
              availableConfirmedBlock: AVAILABLE_CONFIRMED_BLOCK,
              mempoolTxsCountSoFar: PROCESSED_UNSUBMITTED_TXS_COUNT,
              sizeOfProcessedTxsSoFar: PROCESSED_UNSUBMITTED_TXS_SIZE,
            },
          } as WorkerInput, // TODO: Consider other approaches to avoid type assertion here.
        },
      );
      worker.on("message", (output: WorkerOutput) => {
        if (output.type === "FailureOutput") {
          resume(
            Effect.fail(
              new WorkerError({
                worker: "commit-block-header",
                message: `Commitment worker failed`,
                cause: output.error,
              }),
            ),
          );
        } else {
          resume(Effect.succeed(output));
        }
        worker.terminate();
      });
      worker.on("error", (e: Error) => {
        resume(
          Effect.fail(
            new WorkerError({
              worker: "commit-block-header",
              message: `Error in commitment worker: ${e}`,
              cause: e,
            }),
          ),
        );
        worker.terminate();
      });
      worker.on("exit", (code: number) => {
        if (code !== 0) {
          resume(
            Effect.fail(
              new WorkerError({
                worker: "commit-block-header",
                message: `Commitment worker exited with code: ${code}`,
                cause: `exit code ${code}`,
              }),
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
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);

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
        yield* Ref.update(
          globals.PROCESSED_UNSUBMITTED_TXS_COUNT,
          (n) => n + workerOutput.mempoolTxsCount,
        );
        yield* Ref.update(
          globals.PROCESSED_UNSUBMITTED_TXS_SIZE,
          (n) => n + workerOutput.sizeOfProcessedTxs,
        );
        break;
      }
      case "NothingToCommitOutput": {
        break;
      }
      case "FailureOutput": {
        break;
      }
    }
  });
