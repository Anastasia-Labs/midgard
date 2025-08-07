import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule, pipe } from "effect";
import { NodeConfig, User } from "@/config.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";

const inputData = workerData as WorkerInput;

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;
    if (workerInput.data.unconfirmedSubmittedBlock === "") {
      return {
        type: "NoTxForConfirmationOutput",
      };
    } else {
      const targetTxHash = workerInput.data.unconfirmedSubmittedBlock;
      yield* Effect.logInfo(`🟤 Confirming tx: ${targetTxHash}`);
      yield* Effect.retry(
        Effect.tryPromise({
          try: () => lucid.awaitTx(targetTxHash),
          catch: (e) => new Error(`${e}`),
        }),
        Schedule.recurs(4),
      );
      yield* Effect.logInfo("🟤 Tx confirmed. Fetching the block...");
      const { policyId, spendScriptAddress } =
        yield* makeAlwaysSucceedsServiceFn(nodeConfig);
      const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
        stateQueueAddress: spendScriptAddress,
        stateQueuePolicyId: policyId,
      };
      const latestBlock = yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
        lucid,
        fetchConfig,
      );
      if (latestBlock.utxo.txHash == targetTxHash) {
        yield* Effect.logInfo("🟤 Serializing state queue UTxO...");
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        yield* Effect.logInfo("🟤 Done.");
        return {
          type: "SuccessfulConfirmationOutput",
          blocksUTxO: serializedUTxO,
        };
      } else {
        yield* Effect.logInfo(
          "🟤 ⚠️  Latest block's txHash doesn't match the confirmed tx.",
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
  Effect.provide(User.layer),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAll((e) =>
      Effect.succeed({
        type: "FailedConfirmationOutput",
        error:
          e instanceof Error
            ? e.message
            : "Unknown error from tx confirmation worker",
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `👷 Confirmation work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
