import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceedsContract, Lucid, NodeConfig } from "@/services/index.js";
import {
  TxConfirmError,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "../utils.js";

export const stateQueueInit: Effect.Effect<
  string | void,
  TxSubmitError | TxSignError | SDK.Utils.LucidError,
  Lucid | NodeConfig | AlwaysSucceedsContract
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;
  const initParams: SDK.TxBuilder.StateQueue.InitParams = {
    address: stateQueueAuthValidator.spendScriptAddress,
    policyId: stateQueueAuthValidator.policyId,
    stateQueueMintingScript: stateQueueAuthValidator.mintScript,
  };
  yield* lucid.switchToOperatorsMainWallet;
  const txBuilderProgram = SDK.Endpoints.StateQueue.initTxProgram(
    lucid.api,
    initParams,
  );
  const txBuilder = yield* txBuilderProgram;
  const onSubmitFailure = (err: TxSubmitError | { _tag: "TxSubmitError" }) =>
    Effect.gen(function* () {
      yield* Effect.logError(`Submit tx error: ${err}`);
      yield* Effect.fail(
        new TxSubmitError({
          message: "Failed to submit the state queue initiation tx",
          cause: err,
          txHash: txBuilder.toHash(),
        }),
      );
    });
  const onConfirmFailure = (err: TxConfirmError) =>
    Effect.logError(`Confirm tx error: ${err}`);
  return yield* handleSignSubmit(lucid.api, txBuilder).pipe(
    Effect.catchTag("TxSubmitError", onSubmitFailure),
    Effect.catchTag("TxConfirmError", onConfirmFailure),
  );
});
