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
} from "@/transactions/utils.js";
import { batchProgram } from "@/utils.js";

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
    const onSubmitFailure = (err: TxSubmitError) =>
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

  yield* Effect.logInfo("🚧 Fetching state queue UTxOs...");

  const allStateQueueUTxOs =
    yield* SDK.Endpoints.StateQueue.fetchUnsortedStateQueueUTxOsProgram(
      lucid.api,
      fetchConfig,
    );

  if (allStateQueueUTxOs.length <= 0) {
    yield* Effect.logInfo(`🚧 No state queue UTxOs were found.`);
  }

  yield* lucid.switchToOperatorsMainWallet;

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
          lucid.api,
          fetchConfig,
          alwaysSucceeds.stateQueueAuthValidator.spendScript,
          alwaysSucceeds.stateQueueAuthValidator.mintScript,
          batch,
        );
      }).pipe(Effect.tapError((e) => Effect.logError(e))),
    1,
  );
  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  const globals = yield* Globals;
  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
  yield* Effect.logInfo(`🚧 Done.`);
});
