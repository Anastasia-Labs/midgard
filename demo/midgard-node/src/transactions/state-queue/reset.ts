import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  Script,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceeds } from "@/services/index.js";
import { User } from "@/config.js";
import { Effect } from "effect";
import { ConfirmError, handleSignSubmit, SubmitError } from "../utils.js";

const collectAndBurnStateQueueNodesProgram = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  stateQueueSpendingScript: Script,
  stateQueueMintingScript: Script,
  utxosAndAssetNames: { utxo: UTxO; assetName: string }[],
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    const tx = lucid.newTx();
    const assetsToBurn: Assets = {};
    utxosAndAssetNames.map(({ utxo, assetName }) => {
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
    return yield* handleSignSubmit(
      lucid,
      completed,
      onSubmitFailure,
      onConfirmFailure,
    );
  });

export const resetStateQueue = Effect.gen(function* () {
  const { user: lucid } = yield* User;
  const alwaysSucceeds = yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.policyId,
    stateQueueAddress: alwaysSucceeds.spendScriptAddress,
  };

  const allUTxOsAndAssetNames =
    yield* SDK.Endpoints.fetchAllStateQueueUTxOsProgram(lucid, fetchConfig);

  // Collect and burn 10 UTxOs and asset names at a time:
  const batchSize = 40;
  for (let i = 0; i < allUTxOsAndAssetNames.length; i += batchSize) {
    const batch = allUTxOsAndAssetNames.slice(i, i + batchSize);
    yield* collectAndBurnStateQueueNodesProgram(
      lucid,
      fetchConfig,
      alwaysSucceeds.spendScript,
      alwaysSucceeds.mintScript,
      batch,
    );
  }
});
