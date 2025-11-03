import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  PolicyId,
  Script,
  TxBuilder,
  TxSignBuilder,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract, AuthenticatedValidator, Globals, Lucid } from "@/services/index.js";
import { Effect, Ref } from "effect";
import {
  TxConfirmError,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "@/transactions/utils.js";
import { batchProgram } from "@/utils.js";

const collectAndBurnUTxOsTx = (
  lucid: LucidEvolution,
  authValidator: AuthenticatedValidator,
  assetUTxOs: {
    utxo: UTxO;
    assetName: string;
  }[],
) =>
  Effect.gen(function* () {
    const tx = lucid.newTx();
    const assetsToBurn: Assets = {};
    assetUTxOs.map(({ utxo, assetName }) => {
      const unit = toUnit(authValidator.policyId, assetName);
      if (assetsToBurn[unit] !== undefined) {
        assetsToBurn[unit] -= 1n;
      } else {
        assetsToBurn[unit] = -1n;
      }
      tx.collectFrom([utxo], Data.void());
    });
    tx.mintAssets(assetsToBurn, Data.void())
      .attach.Script(authValidator.spendScript)
      .attach.Script(authValidator.mintScript);
    return tx;
  })

const completeResetTxProgram = (
  lucid: LucidEvolution,
  tx: TxBuilder,
): Effect.Effect<void, SDK.LucidError | TxSignError | TxSubmitError, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.RESET_IN_PROGRESS, true);
    const completed: TxSignBuilder = yield* tx.completeProgram().pipe(
      Effect.mapError(
        (e) =>
          new SDK.LucidError({
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

export const resetAll: Effect.Effect<
  void,
  SDK.LucidError | TxSubmitError | TxSignError,
  AlwaysSucceedsContract | Lucid | Globals
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const { stateQueueAuthValidator, depositAuthValidator } = yield* AlwaysSucceedsContract;

  yield* lucid.switchToOperatorsMainWallet;

  yield* Effect.logInfo("🚧 Fetching UTxOs...");

  const allStateQueueUTxOs = yield* SDK.fetchUnsortedStateQueueUTxOsProgram(
    lucid.api,
    {
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
      stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
    },
  );

  const allDepositUTxOs = yield* SDK.fetchDepositUTxOsProgram(lucid.api, {
    depositAddress: depositAuthValidator.spendScriptAddress,
    depositPolicyId: depositAuthValidator.policyId,
  });

  if (allStateQueueUTxOs.length <= 0 && allDepositUTxOs.length <= 0) {
    yield* Effect.logInfo(`🚧 No UTxOs were found.`);
  }

  const batchSize = 40;

  const stateQueuePartialBatch = allStateQueueUTxOs.splice(Math.floor(allStateQueueUTxOs.length / batchSize), allStateQueueUTxOs.length % batchSize)
  const depositPartialBatch = allDepositUTxOs.splice(Math.floor(allDepositUTxOs.length / batchSize), allDepositUTxOs.length % batchSize)

  yield* lucid.switchToOperatorsMainWallet;

  const partialBatchTx: TxBuilder = yield* Effect.gen(function* () {
    const stateQueuePartialTx = yield* collectAndBurnUTxOsTx(lucid.api, stateQueueAuthValidator, stateQueuePartialBatch);
    const depositPartialTx = yield* collectAndBurnUTxOsTx(lucid.api, depositAuthValidator, depositPartialBatch);
    return stateQueuePartialTx.compose(depositPartialTx)
  })

  // Collect and burn UTxOs, that didn't fill a full batch:
  yield* Effect.logInfo(`🚧 Mixed Batch 0 - ${stateQueuePartialBatch.length + depositPartialBatch.length}`);
  yield* completeResetTxProgram(lucid.api, partialBatchTx).pipe(Effect.tapError((e) => Effect.logError(e)));

  // Collect and burn full batches of UTxOs at a time:

  yield* batchProgram(
    batchSize,
    allStateQueueUTxOs.length,
    "resetStateQueue",
    (startIndex, endIndex) =>
      Effect.gen(function* () {
        const batch = allStateQueueUTxOs.slice(startIndex, endIndex);
        yield* Effect.logInfo(`🚧 StateQueue Batch ${startIndex}-${endIndex}`);
        const tx = yield* collectAndBurnUTxOsTx(lucid.api, stateQueueAuthValidator, batch);
        yield* completeResetTxProgram(lucid.api, tx);
      }).pipe(Effect.tapError((e) => Effect.logError(e))),
    1,
  );

  yield* batchProgram(
    batchSize,
    allDepositUTxOs.length,
    "resetDeposit",
    (startIndex, endIndex) =>
      Effect.gen(function* () {
        const batch = allDepositUTxOs.slice(startIndex, endIndex);
        yield* Effect.logInfo(`🚧 Deposit Batch ${startIndex}-${endIndex}`);
        const tx = yield* collectAndBurnUTxOsTx(lucid.api, depositAuthValidator, batch);
        yield* completeResetTxProgram(lucid.api, tx);
      }).pipe(Effect.tapError((e) => Effect.logError(e))),
    1,
  );

  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  const globals = yield* Globals;
  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
  yield* Effect.logInfo(`🚧 Done resetting UTxOs.`);
});
