import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  Script,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceeds } from "@/services/index.js";
import { NodeConfig, User } from "@/config.js";
import { Effect, Ref } from "effect";
import { TxConfirmError, handleSignSubmit, TxSubmitError } from "../utils.js";
import { Globals } from "@/globals.js";

const collectAndBurnStateQueueNodesProgram = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  stateQueueSpendingScript: Script,
  stateQueueMintingScript: Script,
  stateQueueUTxOs: SDK.TxBuilder.StateQueue.StateQueueUTxO[],
): Effect.Effect<void, Error, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.RESET_IN_PROGRESS, true);
    const tx = lucid.newTx();
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
    const onSubmitFailure = (err: TxSubmitError | { _tag: "TxSubmitError" }) =>
      Effect.gen(function* () {
        yield* Effect.logError(`Submit tx error: ${err}`);
        yield* Effect.fail(
          new TxSubmitError({
            message: "failed to submit a state queue reset tx",
            cause: err,
          }),
        );
      });
    const onConfirmFailure = (err: TxConfirmError) =>
      Effect.logError(`Confirm tx error: ${err}`);
    const txHash = yield* handleSignSubmit(
      lucid,
      completed,
      onSubmitFailure,
      onConfirmFailure,
    );
    yield* Ref.set(globals.RESET_IN_PROGRESS, false);
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

  const allStateQueueUTxOs =
    yield* SDK.Endpoints.fetchUnsortedStateQueueUTxOsProgram(
      lucid,
      fetchConfig,
    );

  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);

  // Collect and burn 10 UTxOs and asset names at a time:
  const batchSize = 40;
  for (let i = 0; i < allStateQueueUTxOs.length; i += batchSize) {
    const batch = allStateQueueUTxOs.slice(i, i + batchSize);
    yield* collectAndBurnStateQueueNodesProgram(
      lucid,
      fetchConfig,
      alwaysSucceeds.spendScript,
      alwaysSucceeds.mintScript,
      batch,
    );
  }
  const globals = yield* Globals;

  yield* Ref.set(globals.LATEST_SYNC_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
});
