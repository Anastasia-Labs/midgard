import { Globals } from "@/services/index.js";
import { Effect, Ref, Schedule } from "effect";
import { WorkerError } from "@/workers/utils/common.js";
import { WorkerInput, WorkerOutput } from "@/workers/utils/block-commitment.js";
import { Metric } from "effect";
import { Worker } from "worker_threads";
import { BlocksDB } from "@/database/index.js";

const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in committed blocks",
});

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in committed blocks",
  bigint: true,
  incremental: true,
});

const blockCommitmentUserEventsCountGauge = Metric.gauge(
  "block_total_user_events_count",
  {
    description:
      "A gauge for tracking the number of user events (deposits, withdrawals and tx orders) in committed blocks",
  },
);

export const buildAndSubmitCommitmentBlockAction = () =>
  Effect.gen(function* () {
    const globals = yield* Globals;

    const worker = Effect.async<WorkerOutput, WorkerError, never>((resume) => {
      Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
      const workerInputData: WorkerInput = { data: {} };
      const worker = new Worker(
        new URL("./block-commitment.js", import.meta.url),
        { workerData: workerInputData },
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
      case "SuccessfulCommitmentOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);

        yield* blockCommitmentUserEventsCountGauge(
          Effect.succeed(
            workerOutput.stats[BlocksDB.Columns.DEPOSITS_COUNT] +
              workerOutput.stats[BlocksDB.Columns.WITHDRAWALS_COUNT] +
              workerOutput.stats[BlocksDB.Columns.TX_ORDERS_COUNT],
          ),
        );
        yield* commitBlockNumTxGauge(
          Effect.succeed(
            BigInt(workerOutput.stats[BlocksDB.Columns.TX_REQUESTS_COUNT]),
          ),
        );
        yield* Metric.increment(commitBlockCounter);
        yield* Metric.incrementBy(
          commitBlockTxCounter,
          BigInt(workerOutput.stats[BlocksDB.Columns.TX_REQUESTS_COUNT]),
        );
        yield* totalTxSizeGauge(
          Effect.succeed(
            workerOutput.stats[BlocksDB.Columns.TOTAL_EVENTS_SIZE],
          ),
        );
        yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
        break;
      }
      case "SeededOutput": {
        yield* Effect.logInfo(
          "🔹 ✅ BlocksDB seeded from chain. Will commit on next cycle.",
        );
        break;
      }
      case "FailureOutput": {
        break;
      }
    }
  });

export const blockCommitmentAction: Effect.Effect<void, WorkerError, Globals> =
  Effect.gen(function* () {
    const globals = yield* Globals;
    const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (!RESET_IN_PROGRESS) {
      yield* Effect.logInfo("🔹 New block commitment process started.");
      yield* buildAndSubmitCommitmentBlockAction().pipe(
        Effect.withSpan("buildAndSubmitCommitmentBlockAction"),
      );
    }
  });

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
