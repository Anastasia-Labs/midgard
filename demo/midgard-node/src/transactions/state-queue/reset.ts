import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  Script,
  TxSignBuilder,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract, Globals, Lucid } from "@/services/index.js";
import { Effect, Ref } from "effect";
import {
  TxConfirmError,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "../utils.js";

const collectAndBurnStateQueueNodesProgram = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  stateQueueSpendingScript: Script,
  stateQueueMintingScript: Script,
  stateQueueUTxOs: SDK.TxBuilder.StateQueue.StateQueueUTxO[],
): Effect.Effect<
  void,
  SDK.Utils.LucidError | TxSignError | TxSubmitError,
  Globals
> =>
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
    const completed: TxSignBuilder = yield* tx.completeProgram().pipe(
      Effect.mapError(
        (e) =>
          new SDK.Utils.LucidError({
            message: "Failed to finalize the reset transaction",
            cause: e,
          }),
      ),
    );
    const onSubmitFailure = (err: TxSubmitError | { _tag: "TxSubmitError" }) =>
      Effect.gen(function* () {
        yield* Effect.logError(`Submit tx error: ${err}`);
        yield* Effect.fail(
          new TxSubmitError({
            message: "failed to submit a state queue reset tx",
            cause: err,
            txHash: completed.toHash(),
          }),
        );
      });
    const onConfirmFailure = (err: TxConfirmError) =>
      Effect.logError(`Confirm tx error: ${err}`);
    const txHash = yield* handleSignSubmit(lucid, completed).pipe(
      Effect.catchTag("TxSubmitError", onSubmitFailure),
      Effect.catchTag("TxConfirmError", onConfirmFailure),
    );
    yield* Ref.set(globals.RESET_IN_PROGRESS, false);
    return txHash;
  });

export const resetStateQueue: Effect.Effect<
  void,
  SDK.Utils.LucidError | TxSubmitError | TxSignError,
  AlwaysSucceedsContract | Lucid | Globals
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const alwaysSucceeds = yield* AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.stateQueueAuthValidator.policyId,
    stateQueueAddress:
      alwaysSucceeds.stateQueueAuthValidator.spendScriptAddress,
  };
  yield* lucid.switchToOperatorsMainWallet;
  const allStateQueueUTxOs =
    yield* SDK.Endpoints.fetchUnsortedStateQueueUTxOsProgram(
      lucid.api,
      fetchConfig,
    );

  yield* lucid.switchToOperatorsMainWallet;

  // Collect and burn 10 UTxOs and asset names at a time:
  const batchSize = 40;
  for (let i = 0; i < allStateQueueUTxOs.length; i += batchSize) {
    const batch = allStateQueueUTxOs.slice(i, i + batchSize);
    yield* collectAndBurnStateQueueNodesProgram(
      lucid.api,
      fetchConfig,
      alwaysSucceeds.stateQueueAuthValidator.spendScript,
      alwaysSucceeds.stateQueueAuthValidator.mintScript,
      batch,
    );
  }
  const globals = yield* Globals;

  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
});
