import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, Script, UTxO, toUnit } from "@lucid-evolution/lucid";
import { AlwaysSucceeds } from "@/services/index.js";
import { User } from "@/config.js";
import { Effect } from "effect";
import { handleSignSubmit } from "../utils.js";

const collectAndBurnStateQueueNodesProgram = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  stateQueueSpendingScript: Script,
  stateQueueMintingScript: Script,
  utxosAndAssetNames: { utxo: UTxO; assetName: string }[],
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    const tx = lucid.newTx();
    utxosAndAssetNames.map(({ utxo, assetName }) => {
      tx.collectFrom([utxo], "d87980") // TODO: Placeholder redeemer.
        .mintAssets(
          { [toUnit(fetchConfig.stateQueuePolicyId, assetName)]: -1n },
          "d87980",
        );
    });
    tx.attach
      .Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    const completed = yield* tx.completeProgram();
    return yield* handleSignSubmit(lucid, completed);
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
  const batchSize = 1;
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
