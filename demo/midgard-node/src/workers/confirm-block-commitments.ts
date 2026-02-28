import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Schedule, pipe } from "effect";
import { MidgardContracts, Lucid, NodeConfig } from "@/services/index.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { TxConfirmError } from "@/transactions/utils.js";

const inputData = workerData as WorkerInput;

const fetchLatestBlock = (
  lucid: LucidEvolution,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError | SDK.LucidError,
  MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const { stateQueue: stateQueueAuthValidator } =
      yield* MidgardContracts;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    return yield* SDK.fetchLatestCommittedBlockProgram(lucid, fetchConfig);
  });

const awaitSubmittedBlockConfirmation = (
  lucid: LucidEvolution,
  txHash: string,
  timeoutMs: number,
  retries: number,
): Effect.Effect<void, TxConfirmError, MidgardContracts | NodeConfig> =>
  Effect.retry(
    Effect.gen(function* () {
      const startedAt = Date.now();
      const pollIntervalMs = 2_000;
      while (Date.now() - startedAt < timeoutMs) {
        const latest = yield* Effect.either(fetchLatestBlock(lucid));
        if (
          latest._tag === "Right" &&
          latest.right.utxo.txHash === txHash
        ) {
          return;
        }
        yield* Effect.sleep(`${pollIntervalMs} millis`);
      }
      return yield* Effect.fail(
        new TxConfirmError({
          message: `Timed out waiting for state-queue tip to confirm submitted transaction`,
          txHash,
          cause: `timeout_ms=${timeoutMs}`,
        }),
      );
    }),
    Schedule.recurs(Math.max(0, retries)),
  );

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError,
  MidgardContracts | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;

    const recoverWithLatestBlock = (
      staleTxHash: string,
    ): Effect.Effect<
      WorkerOutput,
      | SDK.CborSerializationError
      | SDK.CmlUnexpectedError
      | SDK.LucidError
      | SDK.StateQueueError,
      MidgardContracts | NodeConfig
    > =>
      Effect.gen(function* () {
        const latestBlock = yield* fetchLatestBlock(lucid.api);
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        return {
          type: "StaleUnconfirmedRecoveryOutput",
          staleTxHash,
          blocksUTxO: serializedUTxO,
        };
      });

    if (workerInput.data.firstRun) {
      yield* Effect.logInfo("🔍 First run. Fetching the latest block...");
      const latestBlock = yield* fetchLatestBlock(lucid.api);
      const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
      return {
        type: "SuccessfulConfirmationOutput",
        blocksUTxO: serializedUTxO,
      };
    } else if (workerInput.data.unconfirmedSubmittedBlock === "") {
      return {
        type: "NoTxForConfirmationOutput",
      };
    } else {
      const targetTxHash = workerInput.data.unconfirmedSubmittedBlock;
      yield* Effect.logInfo(`🔍 Confirming tx: ${targetTxHash}`);
      const confirmationResult = yield* Effect.either(
        awaitSubmittedBlockConfirmation(
          lucid.api,
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
        };
      }

      yield* Effect.logInfo("🔍 Tx confirmed. Fetching the block...");
      const latestBlock = yield* fetchLatestBlock(lucid.api);
      if (latestBlock.utxo.txHash === targetTxHash) {
        yield* Effect.logInfo("🔍 Serializing state queue UTxO...");
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        yield* Effect.logInfo("🔍 Done.");
        return {
          type: "SuccessfulConfirmationOutput",
          blocksUTxO: serializedUTxO,
        };
      }

      yield* Effect.logWarning(
        `🔍 Confirmed tx ${targetTxHash}, but latest state-queue tip is ${latestBlock.utxo.txHash}; recovering from latest chain tip.`,
      );
      return yield* recoverWithLatestBlock(targetTxHash);
    }
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(MidgardContracts.Default),
  Effect.provide(Lucid.Default),
  Effect.provide(NodeConfig.layer),
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
