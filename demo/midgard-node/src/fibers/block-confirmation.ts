import { Globals } from "@/services/index.js";
import { Duration, Effect, Ref, Schedule } from "effect";
import { Worker } from "worker_threads";
import {
  WorkerInput as BlockConfirmationWorkerInput,
  WorkerOutput as BlockConfirmationWorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { WorkerError } from "@/workers/utils/common.js";

const blockConfirmationAction: Effect.Effect<void, WorkerError, Globals> =
  Effect.gen(function* () {
    const globals = yield* Globals;
    const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (!RESET_IN_PROGRESS) {
      const UNCONFIRMED_SUBMITTED_BLOCK_HASH = yield* Ref.get(
        globals.UNCONFIRMED_SUBMITTED_BLOCK_HASH,
      );
      const AVAILABLE_CONFIRMED_BLOCK = yield* Ref.get(
        globals.AVAILABLE_CONFIRMED_BLOCK,
      );
      yield* Effect.logInfo("🔍 New block confirmation process started.");
      const worker = Effect.async<
        BlockConfirmationWorkerOutput,
        WorkerError,
        never
      >((resume) => {
        Effect.runSync(
          Effect.logInfo(`🔍 Starting block confirmation worker...`),
        );
        const worker = new Worker(
          new URL("./confirm-block-commitments.js", import.meta.url),
          {
            workerData: {
              data: {
                firstRun:
                  UNCONFIRMED_SUBMITTED_BLOCK_HASH === "" &&
                  AVAILABLE_CONFIRMED_BLOCK === "",
                unconfirmedSubmittedBlock: UNCONFIRMED_SUBMITTED_BLOCK_HASH,
              },
            } as BlockConfirmationWorkerInput, // TODO: Consider other approaches to avoid type assertion here.
          },
        );
        worker.on("message", (output: BlockConfirmationWorkerOutput) => {
          if (output.type === "FailedConfirmationOutput") {
            resume(
              Effect.fail(
                new WorkerError({
                  worker: "confirm-block-commitments",
                  message: `Confirmation worker failed.`,
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
                worker: "confirm-block-commitments",
                message: `Error in confirmation worker: ${e}`,
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
                  worker: "confirm-block-commitments",
                  message: `Confirmation worker exited with code: ${code}`,
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
      const workerOutput: BlockConfirmationWorkerOutput = yield* worker;
      switch (workerOutput.type) {
        case "SuccessfulConfirmationOutput": {
          yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_HASH, "");
          yield* Ref.set(
            globals.AVAILABLE_CONFIRMED_BLOCK,
            workerOutput.blocksUTxO,
          );
          yield* Effect.logInfo("🔍 ☑️  Submitted block confirmed.");
          break;
        }
        case "NoTxForConfirmationOutput": {
          break;
        }
        case "FailedConfirmationOutput": {
          break;
        }
      }
    }
  });

export const blockConfirmationFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟫 Block confirmation fork started.");
    const action = blockConfirmationAction.pipe(
      Effect.withSpan("block-confirmation-fork"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
