import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import { Cause, Data, Effect, Schedule, pipe } from "effect";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { Lucid } from "@/services/lucid.js";
import { ConfigError, NodeConfig } from "@/services/config.js";
import {
  fetchLatestCommittedStateQueueBlock,
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { LucidEvolution } from "@lucid-evolution/lucid";
import type * as SDK from "@al-ft/midgard-sdk";

const inputData = workerData as WorkerInput;

type StateQueueAuthValidator = SDK.MidgardValidators["stateQueue"];

class TxConfirmAwaitError extends Data.TaggedError("TxConfirmAwaitError")<{
  readonly message: string;
  readonly txHash: string;
  readonly cause: string;
}> {}

const awaitSubmittedBlockConfirmation = (
  lucid: LucidEvolution,
  stateQueueAuthValidator: StateQueueAuthValidator,
  txHash: string,
  timeoutMs: number,
  retries: number,
) =>
  Effect.retry(
    Effect.gen(function* () {
      const startedAt = Date.now();
      const pollIntervalMs = 2_000;
      while (Date.now() - startedAt < timeoutMs) {
        const latest = yield* Effect.either(
          fetchLatestCommittedStateQueueBlock(lucid, stateQueueAuthValidator),
        );
        if (
          latest._tag === "Right" &&
          latest.right.utxo.txHash === txHash
        ) {
          return;
        }
        yield* Effect.sleep(`${pollIntervalMs} millis`);
      }
      yield* Effect.fail(
        new TxConfirmAwaitError({
          message:
            "Timed out waiting for state-queue tip to confirm submitted transaction",
          txHash,
          cause: `timeout_ms=${timeoutMs}`,
        }),
      );
    }),
    Schedule.recurs(Math.max(0, retries)),
  );

const resolveStateQueueAuthValidator = (): Effect.Effect<
  StateQueueAuthValidator,
  never,
  MidgardContracts
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    // Effect.Service DTS generation currently widens this worker entry; keep
    // the runtime contract shape explicit at the boundary we actually need.
    return (contracts as unknown as {
      readonly stateQueue: StateQueueAuthValidator;
    }).stateQueue;
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

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<WorkerOutput, unknown, MidgardContracts | Lucid | NodeConfig> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const stateQueueAuthValidator = yield* resolveStateQueueAuthValidator();

    /**
     * Recovers confirmation processing by replaying from the latest known block.
     */
    const recoverWithLatestBlock = (staleTxHash: string) =>
      Effect.gen(function* () {
        const latestBlock = yield* fetchLatestCommittedStateQueueBlock(
          lucid.api,
          stateQueueAuthValidator,
        );
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        return {
          type: "StaleUnconfirmedRecoveryOutput",
          staleTxHash,
          blocksUTxO: serializedUTxO,
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
        blocksUTxO: serializedUTxO,
      } satisfies WorkerOutput;
    } else if (workerInput.data.unconfirmedSubmittedBlock === "") {
      return {
        type: "NoTxForConfirmationOutput",
      } satisfies WorkerOutput;
    } else {
      const targetTxHash = workerInput.data.unconfirmedSubmittedBlock;
      yield* Effect.logInfo(`🔍 Confirming tx: ${targetTxHash}`);
      const confirmationResult = yield* Effect.either(
        awaitSubmittedBlockConfirmation(
          lucid.api,
          stateQueueAuthValidator,
          targetTxHash,
          nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS,
          nodeConfig.BLOCK_CONFIRMATION_AWAIT_RETRIES,
        ),
      );

      if (confirmationResult._tag === "Left") {
        const pendingAgeMs =
          workerInput.data.unconfirmedSubmittedBlockSinceMs > 0
            ? Date.now() - workerInput.data.unconfirmedSubmittedBlockSinceMs
            : 0;
        yield* Effect.logWarning(
          `🔍 Pending submitted block ${targetTxHash} not confirmed yet (age_ms=${pendingAgeMs}, timeout_ms=${nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS}, retries=${nodeConfig.BLOCK_CONFIRMATION_AWAIT_RETRIES}).`,
        );

        if (pendingAgeMs >= nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS) {
          yield* Effect.logWarning(
            `🔍 Pending submitted block ${targetTxHash} exceeded max age (${nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS}ms); recovering from latest chain tip.`,
          );
          return yield* recoverWithLatestBlock(targetTxHash);
        }

        return {
          type: "NoTxForConfirmationOutput",
        } satisfies WorkerOutput;
      }

      yield* Effect.logInfo("🔍 Tx confirmed. Fetching the block...");
      const latestBlock = yield* fetchLatestCommittedStateQueueBlock(
        lucid.api,
        stateQueueAuthValidator,
      );
      if (latestBlock.utxo.txHash === targetTxHash) {
        yield* Effect.logInfo("🔍 Serializing state queue UTxO...");
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        yield* Effect.logInfo("🔍 Done.");
        return {
          type: "SuccessfulConfirmationOutput",
          blocksUTxO: serializedUTxO,
        } satisfies WorkerOutput;
      }

      yield* Effect.logWarning(
        `🔍 Confirmed tx ${targetTxHash}, but latest state-queue tip is ${latestBlock.utxo.txHash}; recovering from latest chain tip.`,
      );
      return yield* recoverWithLatestBlock(targetTxHash);
    }
  });

const program = pipe(
  wrapper(inputData),
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
