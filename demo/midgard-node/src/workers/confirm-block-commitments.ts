import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import { Cause, Data, Effect, Option, pipe } from "effect";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { Lucid } from "@/services/lucid.js";
import { ConfigError, NodeConfig } from "@/services/config.js";
import {
  fetchLatestCommittedStateQueueBlock,
  fetchSortedCommittedStateQueueBlocks,
  findCommittedStateQueueBlockByHeaderHash,
  latestCommittedStateQueueBlockFromSorted,
  resolveStateQueueBlockEndTimeMs,
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { LucidEvolution } from "@lucid-evolution/lucid";
import type * as SDK from "@al-ft/midgard-sdk";

type StateQueueAuthValidator = SDK.MidgardValidators["stateQueue"];

class TxConfirmAwaitError extends Data.TaggedError("TxConfirmAwaitError")<{
  readonly message: string;
  readonly headerHash: string;
  readonly cause: string;
}> {}

const awaitPendingBlockResolution = (
  lucid: LucidEvolution,
  stateQueueAuthValidator: StateQueueAuthValidator,
  pendingBlock: NonNullable<WorkerInput["data"]["pendingBlock"]>,
  timeoutMs: number,
) =>
  Effect.gen(function* () {
    const startedAt = Date.now();
    const pollIntervalMs = 2_000;
    while (Date.now() - startedAt < timeoutMs) {
      const sortedBlocks = yield* fetchSortedCommittedStateQueueBlocks(
        lucid,
        stateQueueAuthValidator,
      );
      const latestBlock =
        yield* latestCommittedStateQueueBlockFromSorted(sortedBlocks);
      const latestEndTimeMs =
        yield* resolveStateQueueBlockEndTimeMs(latestBlock);
      const matchedPendingBlock =
        yield* findCommittedStateQueueBlockByHeaderHash(
          sortedBlocks,
          pendingBlock.expectedHeaderHash,
        );
      if (Option.isSome(matchedPendingBlock)) {
        return {
          latestBlock,
          matchedPendingBlock: matchedPendingBlock.value,
        };
      }
      if (latestEndTimeMs >= pendingBlock.blockEndTimeMs) {
        return {
          latestBlock,
          matchedPendingBlock: null,
        };
      }
      yield* Effect.sleep(`${pollIntervalMs} millis`);
    }
    return yield* Effect.fail(
      new TxConfirmAwaitError({
        message:
          "Timed out waiting for canonical state_queue resolution of pending block header",
        headerHash: pendingBlock.expectedHeaderHash,
        cause: `timeout_ms=${timeoutMs}`,
      }),
    );
  });

const resolveStateQueueAuthValidator = (): Effect.Effect<
  StateQueueAuthValidator,
  never,
  MidgardContracts
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    // Effect.Service DTS generation currently widens this worker entry; keep
    // the runtime contract shape explicit at the boundary we actually need.
    return (
      contracts as unknown as {
        readonly stateQueue: StateQueueAuthValidator;
      }
    ).stateQueue;
  });

const provideConfirmationWorkerServices = <A, E>(
  effect: Effect.Effect<A, E, MidgardContracts | Lucid | NodeConfig>,
): Effect.Effect<A, E | ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(MidgardContracts.Default),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );

export const runConfirmBlockCommitmentsWorkerProgram = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const stateQueueAuthValidator = yield* resolveStateQueueAuthValidator();

    /**
     * Recovers confirmation processing by replaying from the latest known block.
     */
    const recoverWithLatestBlock = (
      stalePendingHeaderHash: string,
      staleSubmittedTxHash: "" | string,
    ) =>
      Effect.gen(function* () {
        const latestBlock = yield* fetchLatestCommittedStateQueueBlock(
          lucid.api,
          stateQueueAuthValidator,
        );
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        return {
          type: "StaleUnconfirmedRecoveryOutput",
          stalePendingHeaderHash,
          staleSubmittedTxHash,
          latestBlocksUTxO: serializedUTxO,
        } satisfies WorkerOutput;
      });

    if (workerInput.data.firstRun) {
      yield* Effect.logInfo("🔍 First run. Fetching the latest block...");
      const latestBlock = yield* fetchLatestCommittedStateQueueBlock(
        lucid.api,
        stateQueueAuthValidator,
      );
      const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
      return {
        type: "SuccessfulConfirmationOutput",
        latestBlocksUTxO: serializedUTxO,
        matchedPendingBlocksUTxO: null,
      } satisfies WorkerOutput;
    } else if (workerInput.data.pendingBlock === null) {
      return {
        type: "NoTxForConfirmationOutput",
      } satisfies WorkerOutput;
    } else {
      const pendingBlock = workerInput.data.pendingBlock;
      const pendingAgeMs = Math.max(0, Date.now() - pendingBlock.updatedAtMs);
      yield* Effect.logInfo(
        `🔍 Resolving pending block header ${pendingBlock.expectedHeaderHash} (submitted_tx=${pendingBlock.submittedTxHash || "unknown"}, age_ms=${pendingAgeMs}).`,
      );
      const confirmationResult = yield* Effect.either(
        awaitPendingBlockResolution(
          lucid.api,
          stateQueueAuthValidator,
          pendingBlock,
          nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS,
        ),
      );

      if (confirmationResult._tag === "Left") {
        yield* Effect.logWarning(
          `🔍 Pending block header ${pendingBlock.expectedHeaderHash} not resolved yet (submitted_tx=${pendingBlock.submittedTxHash || "unknown"}, age_ms=${pendingAgeMs}, timeout_ms=${nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS}).`,
        );
        if (pendingAgeMs >= nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS) {
          yield* Effect.logWarning(
            `🔍 Pending block header ${pendingBlock.expectedHeaderHash} exceeded warning age (${nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS}ms) without deterministic chain resolution.`,
          );
        }
        return {
          type: "NoTxForConfirmationOutput",
        } satisfies WorkerOutput;
      }

      const resolved = confirmationResult.right;
      if (resolved.matchedPendingBlock !== null) {
        yield* Effect.logInfo(
          `🔍 Pending block header ${pendingBlock.expectedHeaderHash} is present in canonical state_queue.`,
        );
        const latestSerializedUTxO = yield* serializeStateQueueUTxO(
          resolved.latestBlock,
        );
        const matchedSerializedUTxO = yield* serializeStateQueueUTxO(
          resolved.matchedPendingBlock,
        );
        yield* Effect.logInfo("🔍 Done.");
        return {
          type: "SuccessfulConfirmationOutput",
          latestBlocksUTxO: latestSerializedUTxO,
          matchedPendingBlocksUTxO: matchedSerializedUTxO,
        } satisfies WorkerOutput;
      }

      yield* Effect.logWarning(
        `🔍 Canonical state_queue advanced past pending block header ${pendingBlock.expectedHeaderHash} without including it; abandoning that pending submission.`,
      );
      return yield* recoverWithLatestBlock(
        pendingBlock.expectedHeaderHash,
        pendingBlock.submittedTxHash,
      );
    }
  });

if (parentPort !== null) {
  const inputData = workerData as WorkerInput;
  const program = pipe(
    runConfirmBlockCommitmentsWorkerProgram(inputData),
    provideConfirmationWorkerServices,
  );

  Effect.runPromise(
    program.pipe(
      Effect.catchAllCause((cause) =>
        Effect.succeed({
          type: "FailedConfirmationOutput",
          error: `Tx confirmation worker failure: ${Cause.pretty(cause)}`,
        }),
      ),
    ),
  ).then((output) => {
    Effect.runSync(
      Effect.logInfo(
        `🔍 Confirmation work completed (${JSON.stringify(output)}).`,
      ),
    );
    parentPort?.postMessage(output);
  });
}
