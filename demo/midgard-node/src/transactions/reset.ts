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
import { Effect, Option, Ref } from "effect";
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

type ValidatorUTxOsPair = {
    authValidator: AuthenticatedValidator,
    assetUTxOs: {
      utxo: UTxO;
      assetName: string;
    }[],
}

const stringifyBigInt = (obj: any) => JSON.stringify(obj, (_, v) => typeof v === 'bigint' ? v.toString() : v)

const constructBatchTx = (
  lucid: LucidEvolution,
  validatorUTxOsPairs: ValidatorUTxOsPair[],
  batchSize: number,
): Effect.Effect<Option.Option<{batchTx: TxBuilder, restPairs: ValidatorUTxOsPair[]}>> =>
  Effect.gen(function* () {
    yield* Effect.log(`Call constructBatchTx with batchSize: ${batchSize}`)

    const validatorUTxOs = validatorUTxOsPairs.pop()
    if (validatorUTxOs === undefined) {
      yield* Effect.log(`No more validator UTxOs`)
      return Option.none()
    }
    if (batchSize === 0) {
      yield* Effect.log(`No more batch size left`)
      return Option.none()
    }

    const partialBatch = validatorUTxOs.assetUTxOs.slice(0, batchSize)
    const leftFromPartialBatch = validatorUTxOs.assetUTxOs.slice(batchSize +1)
    if (leftFromPartialBatch.length > 0) {
      validatorUTxOsPairs.push({authValidator: validatorUTxOs.authValidator, assetUTxOs: leftFromPartialBatch})
    }

    const partialBatchTx = yield* collectAndBurnUTxOsTx(lucid, validatorUTxOs.authValidator, partialBatch)
    const batchSizeLeft = batchSize - partialBatch.length

    const optRestBatchTx = yield* constructBatchTx(lucid, validatorUTxOsPairs, batchSizeLeft)
    return Option.match(optRestBatchTx, {
      onNone: () => Option.some({batchTx: partialBatchTx, restPairs: validatorUTxOsPairs}),
      onSome: ({batchTx, restPairs}) => Option.some({batchTx: partialBatchTx.compose(batchTx), restPairs: restPairs})
    })
  })

const constructBatchTxs = (
  lucid: LucidEvolution,
  validatorUTxOsPairs: ValidatorUTxOsPair[],
  batchSize: number,
): Effect.Effect<TxBuilder[]> =>
  Effect.gen(function* () {
    let accTransactions: TxBuilder[] = []
    let currValidatorUTxOsPairs = validatorUTxOsPairs
    while (currValidatorUTxOsPairs.length >= 0) {
      const optBatchTx = yield* constructBatchTx(lucid, validatorUTxOsPairs, batchSize)
      const batchTx = Option.getOrElse(optBatchTx, () => ({
        batchTx: lucid.newTx(),
        restPairs: [],
      }))

      accTransactions.push(batchTx.batchTx)
      currValidatorUTxOsPairs = batchTx.restPairs
    }

    return accTransactions
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

  const validatorUTxOPairs: ValidatorUTxOsPair[] = [
    {
      authValidator: stateQueueAuthValidator,
      assetUTxOs: allStateQueueUTxOs,
    },
    {
      authValidator: depositAuthValidator,
      assetUTxOs: allDepositUTxOs,
    },
  ]

  const batchSize = 40;
  const optBatchTransaction = yield* constructBatchTx(lucid.api, validatorUTxOPairs, batchSize)
  const batchTransaction = Option.getOrThrow(optBatchTransaction)

  yield* Effect.logInfo(`🚧 StateQueue Batch`);
  yield* completeResetTxProgram(lucid.api, batchTransaction.batchTx);

  // const batchTransactions = yield* constructBatchTxs(lucid.api, validatorUTxOPairs, batchSize)
  // yield* Effect.forEach(batchTransactions, (tx, i) =>
  //   Effect.gen(function* () {
  //     yield* Effect.logInfo(`🚧 StateQueue Batch ${i}`);
  //     yield* completeResetTxProgram(lucid.api, tx);
  // }).pipe(Effect.tapError((e) => Effect.logError(e))))


  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  const globals = yield* Globals;
  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
  yield* Effect.logInfo(`🚧 Done resetting UTxOs.`);
});
