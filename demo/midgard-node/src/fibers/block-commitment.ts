import { Globals } from "@/services/index.js";
import { Duration, Effect, Ref, Schedule } from "effect";
import { WorkerError } from "@/workers/utils/common.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { Metric } from "effect";
import { Worker } from "worker_threads";
import { emitQueueStateMetrics } from "./queue-metrics.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";

/**
 * Background block-commitment loop that packages processed L2 transactions into
 * L1 commitment transactions.
 *
 * The fiber relies on a worker thread for the heavy lifting, then translates
 * the worker's outcome into updates of the node's shared global state and
 * operational metrics.
 */
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

const commitWorkerDurationTimer = Metric.timer(
  "commit_worker_duration",
  "Duration of one block commitment worker attempt in milliseconds",
);

/**
 * Launches one commitment worker, applies its result to global node state, and
 * updates the block-commitment metrics.
 */
export const buildAndSubmitCommitmentBlockAction = () =>
  Effect.gen(function* () {
    const workerStartedAt = Date.now();
    const globals = yield* Globals;
    const AVAILABLE_CONFIRMED_BLOCK = yield* globals.AVAILABLE_CONFIRMED_BLOCK;
    const AVAILABLE_LOCAL_FINALIZATION_BLOCK =
      yield* globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK;
    const CURRENT_BLOCK_START_TIME_MS =
      yield* globals.LATEST_LOCAL_BLOCK_END_TIME_MS;
    const LOCAL_FINALIZATION_PENDING =
      yield* globals.LOCAL_FINALIZATION_PENDING;
    const PROCESSED_UNSUBMITTED_TXS_COUNT =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_COUNT;
    const PROCESSED_UNSUBMITTED_TXS_SIZE =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_SIZE;

    const worker = Effect.async<WorkerOutput, WorkerError, never>((resume) => {
      Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
      const worker = new Worker(
        resolveWorkerEntry(import.meta.url, "commit-block-header.js"),
        {
          workerData: {
            data: {
              availableConfirmedBlock: AVAILABLE_CONFIRMED_BLOCK,
              availableLocalFinalizationBlock:
                AVAILABLE_LOCAL_FINALIZATION_BLOCK,
              currentBlockStartTimeMs: CURRENT_BLOCK_START_TIME_MS,
              localFinalizationPending: LOCAL_FINALIZATION_PENDING,
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
    yield* commitWorkerDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - workerStartedAt)),
    );

    switch (workerOutput.type) {
      case "SuccessfulSubmissionOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          workerOutput.blockEndTimeMs,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
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
      case "SubmittedAwaitingLocalFinalizationOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          workerOutput.blockEndTimeMs,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
        yield* Effect.logWarning(
          `🔹 Block submitted but local finalization is pending recovery: ${workerOutput.error}`,
        );
        break;
      }
      case "SubmittedAwaitingConfirmationOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          workerOutput.blockEndTimeMs,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
        yield* Effect.logInfo(
          "🔹 Block submitted; local finalization is intentionally deferred until L1 confirmation.",
        );
        break;
      }
      case "SuccessfulLocalFinalizationRecoveryOutput": {
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);
        yield* Effect.logInfo(
          "🔹 ☑️  Local finalization recovery completed for confirmed block.",
        );
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

    yield* emitQueueStateMetrics;
  });

/**
 * Single scheduled commitment tick with a guard that prevents overlapping
 * commitment workers.
 */
export const blockCommitmentAction: Effect.Effect<void, WorkerError, Globals> =
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.HEARTBEAT_BLOCK_COMMITMENT, Date.now());
    const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (!RESET_IN_PROGRESS) {
      const acquired = yield* Ref.modify(
        globals.COMMIT_WORKER_ACTIVE,
        (active) => (active ? [false, true] : [true, true]),
      );
      if (!acquired) {
        yield* Effect.logInfo(
          "🔹 Skipping block commitment trigger because a worker is already active.",
        );
        return;
      }

      yield* Effect.logInfo("🔹 New block commitment process started.");
      yield* buildAndSubmitCommitmentBlockAction().pipe(
        Effect.withSpan("buildAndSubmitCommitmentBlockAction"),
        Effect.ensuring(Ref.set(globals.COMMIT_WORKER_ACTIVE, false)),
      );
    }
  });

/**
 * Fiber wrapper that repeats block-commitment work on the provided schedule.
 */
export const blockCommitmentFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fiber started.");
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
