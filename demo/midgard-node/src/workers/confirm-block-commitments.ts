import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Schedule, pipe } from "effect";
import {
  AlwaysSucceedsContract,
  AuthenticatedValidator,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
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
  SDK.TxBuilder.StateQueue.StateQueueUTxO,
  SDK.Utils.StateQueueError | SDK.Utils.LucidError,
  AlwaysSucceedsContract | NodeConfig
> =>
  Effect.gen(function* () {
    const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    return yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
      lucid,
      fetchConfig,
    );
  });

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  | SDK.Utils.CborSerializationError
  | SDK.Utils.CmlUnexpectedError
  | SDK.Utils.LucidError
  | SDK.Utils.StateQueueError
  | TxConfirmError,
  AlwaysSucceedsContract | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
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
      yield* Effect.retry(
        Effect.tryPromise({
          try: () => lucid.api.awaitTx(targetTxHash),
          catch: (e) =>
            new TxConfirmError({
              message: `Failed to confirm transaction`,
              txHash: targetTxHash,
              cause: e,
            }),
        }),
        Schedule.recurs(4),
      );
      yield* Effect.logInfo("🔍 Tx confirmed. Fetching the block...");
      const latestBlock = yield* fetchLatestBlock(lucid.api);
      if (latestBlock.utxo.txHash == targetTxHash) {
        yield* Effect.logInfo("🔍 Serializing state queue UTxO...");
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        yield* Effect.logInfo("🔍 Done.");
        return {
          type: "SuccessfulConfirmationOutput",
          blocksUTxO: serializedUTxO,
        };
      } else {
        yield* Effect.logInfo(
          "🔍 ⚠️  Latest block's txHash doesn't match the confirmed tx.",
        );
        return {
          type: "FailedConfirmationOutput",
          error:
            "Tx confirmed, but fetching the latest block did not yield a UTxO with the same txHash",
        };
      }
    }
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(AlwaysSucceedsContract.Default),
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
