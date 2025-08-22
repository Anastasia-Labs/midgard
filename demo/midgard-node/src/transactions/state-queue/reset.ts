import * as SDK from "@al-ft/midgard-sdk";
import {
  Address,
  Assets,
  Data,
  LucidEvolution,
  Script,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceeds } from "@/services/index.js";
import { NodeConfig, User } from "@/config.js";
import { Effect } from "effect";
import { ConfirmError, handleSignSubmit, SubmitError } from "../utils.js";
import { batchProgram } from "@/utils.js";

const collectAndBurnStateQueueNodesProgram = (
  lucid: LucidEvolution,
  l1OperatorAddress: Address,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  stateQueueSpendingScript: Script,
  stateQueueMintingScript: Script,
  stateQueueUTxOs: SDK.TxBuilder.StateQueue.StateQueueUTxO[],
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    global.RESET_IN_PROGRESS = true;
    const tx = lucid.newTx().addSigner(l1OperatorAddress);
    const assetsToBurn: Assets = {};
    stateQueueUTxOs.map(({ utxo, assetName }) => {
      const unit = toUnit(fetchConfig.stateQueuePolicyId, assetName);
      if (assetsToBurn[unit] !== undefined) {
        assetsToBurn[unit] -= 1n;
      } else {
        assetsToBurn[unit] = -1n;
      }
      tx.collectFrom([utxo], Data.void());
    });
    tx.mintAssets(assetsToBurn, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    const completed = yield* tx.completeProgram();
    const onSubmitFailure = (err: SubmitError) =>
      Effect.gen(function* () {
        yield* Effect.logError(`Sumbit tx error: ${err}`);
        yield* Effect.fail(err.err);
      });
    const onConfirmFailure = (err: ConfirmError) =>
      Effect.logError(`Confirm tx error: ${err}`);
    const txHash = yield* handleSignSubmit(
      lucid,
      completed,
      onSubmitFailure,
      onConfirmFailure,
    );
    global.RESET_IN_PROGRESS = false;
    return txHash;
  });

export const resetStateQueue = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { user: lucid } = yield* User;
  const alwaysSucceeds = yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.policyId,
    stateQueueAddress: alwaysSucceeds.spendScriptAddress,
  };

  yield* Effect.logInfo("🚧 Fetching state queue UTxOs...");
  const allStateQueueUTxOs =
    yield* SDK.Endpoints.fetchUnsortedStateQueueUTxOsProgram(
      lucid,
      fetchConfig,
    );

  if (allStateQueueUTxOs.length <= 0) {
    yield* Effect.logInfo(`🚧 No state queue UTxOs were found.`);
  }

  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);

  const l1OperatorAddr = yield* Effect.tryPromise({
    try: () => lucid.wallet().address(),
    catch: (e) => new Error(`${e}`),
  });

  // Collect and burn 40 UTxOs and asset names at a time:
  const batchSize = 40;
  yield* batchProgram(
    batchSize,
    allStateQueueUTxOs.length,
    "resetStateQueue",
    (startIndex, endIndex) =>
      Effect.gen(function* () {
        const batch = allStateQueueUTxOs.slice(startIndex, endIndex);
        yield* Effect.logInfo(`🚧 Batch ${startIndex}-${endIndex}`);
        yield* collectAndBurnStateQueueNodesProgram(
          lucid,
          l1OperatorAddr,
          fetchConfig,
          alwaysSucceeds.spendScript,
          alwaysSucceeds.mintScript,
          batch,
        );
      }).pipe(Effect.tapError((e) => Effect.logError(e))),
    1,
  );
  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH = Date.now();
  global.BLOCKS_IN_QUEUE = 0;
  yield* Effect.logInfo(`🚧 Done.`);
});
