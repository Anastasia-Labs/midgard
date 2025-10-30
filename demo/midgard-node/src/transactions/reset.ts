import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  PolicyId,
  Script,
  TxSignBuilder,
  UTxO,
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

const collectAndBurnUTxOsProgram = (
  lucid: LucidEvolution,
  policyID: PolicyId,
  spendingScript: Script,
  mintingScript: Script,
  assetUTxOs: {
    utxo: UTxO;
    assetName: string;
  }[],
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
    assetUTxOs.map(({ utxo, assetName }) => {
      const unit = toUnit(policyID, assetName);
      if (assetsToBurn[unit] !== undefined) {
        assetsToBurn[unit] -= 1n;
      } else {
        assetsToBurn[unit] = -1n;
      }
      tx.collectFrom([utxo], Data.void());
    });
    tx.mintAssets(assetsToBurn, Data.void())
      .attach.Script(spendingScript)
      .attach.Script(mintingScript);
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
            message: "failed to submit a utxos reset tx",
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
  const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

  yield* lucid.switchToOperatorsMainWallet;

  yield* Effect.logInfo("🚧 Fetching state queue UTxOs...");

  const allStateQueueUTxOs =
    yield* SDK.Endpoints.StateQueue.fetchUnsortedStateQueueUTxOsProgram(
      lucid.api,
      {
        stateQueuePolicyId: stateQueueAuthValidator.policyId,
        stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
      },
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
        yield* collectAndBurnUTxOsProgram(
          lucid.api,
          stateQueueAuthValidator.policyId,
          stateQueueAuthValidator.spendScript,
          stateQueueAuthValidator.mintScript,
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
  yield* Effect.logInfo(`🚧 Done resetting state queue UTxOs.`);
});

export const resetDeposits: Effect.Effect<
  void,
  SDK.Utils.LucidError | TxSubmitError | TxSignError,
  AlwaysSucceedsContract | Lucid | Globals
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const { depositAuthValidator } = yield* AlwaysSucceedsContract;

  yield* lucid.switchToOperatorsMainWallet;

  yield* Effect.logInfo("🚧 Fetching deposit UTxOs...");

  const allDepositUTxOs =
    yield* SDK.Endpoints.UserEvents.Deposit.fetchAllDepositUTxOsProgram(
      lucid.api,
      {
        depositAddress: depositAuthValidator.spendScriptAddress,
        depositPolicyId: depositAuthValidator.policyId,
      },
    );

  if (allDepositUTxOs.length <= 0) {
    yield* Effect.logInfo(`🚧 No deposit UTxOs were found.`);
  }

  yield* lucid.switchToOperatorsMainWallet;

  // Collect and burn 40 UTxOs and asset names at a time:
  const batchSize = 40;
  yield* batchProgram(
    batchSize,
    allDepositUTxOs.length,
    "resetStateQueue",
    (startIndex, endIndex) =>
      Effect.gen(function* () {
        const batch = allDepositUTxOs.slice(startIndex, endIndex);
        yield* Effect.logInfo(`🚧 Batch ${startIndex}-${endIndex}`);
        yield* collectAndBurnUTxOsProgram(
          lucid.api,
          depositAuthValidator.policyId,
          depositAuthValidator.spendScript,
          depositAuthValidator.mintScript,
          batch,
        );
      }).pipe(Effect.tapError((e) => Effect.logError(e))),
    1,
  );

  yield* Effect.logInfo(`🚧 Done resetting deposit UTxOs.`);
});
