import { Globals } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import { Effect, Ref, Schedule } from "effect";
import { WorkerError } from "@/workers/utils/common.js";

export const blockCommitmentAction: Effect.Effect<void, WorkerError, Globals> =
  Effect.gen(function* () {
    const globals = yield* Globals;
    const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (!RESET_IN_PROGRESS) {
      yield* Effect.logInfo("🔹 New block commitment process started.");
      yield* StateQueueTx.buildAndSubmitCommitmentBlock().pipe(
        Effect.withSpan("buildAndSubmitCommitmentBlock"),
      );
    }
  });

export const blockCommitmentFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fork started.");
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fork"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
