import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceeds } from "@/services/index.js";
import { NodeConfig, User } from "@/config.js";
import { TxConfirmError, handleSignSubmit, TxSubmitError } from "../utils.js";

export const stateQueueInit = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { user: lucid } = yield* User;
  const { stateQueueAuthValidator, depositAuthValidator } =
    yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const initParams: SDK.TxBuilder.StateQueue.InitParams = {
    address: stateQueueAuthValidator.spendScriptAddress,
    policyId: stateQueueAuthValidator.policyId,
    stateQueueMintingScript: stateQueueAuthValidator.mintScript,
  };
  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
  const txBuilderProgram = SDK.Endpoints.initTxProgram(lucid, initParams);
  const txBuilder = yield* txBuilderProgram;
  const onSubmitFailure = (err: TxSubmitError | { _tag: "TxSubmitError" }) =>
    Effect.gen(function* () {
      yield* Effect.logError(`Submit tx error: ${err}`);
      yield* Effect.fail(
        new TxSubmitError({
          message: "failed to submit the state queue initiation tx",
          cause: err,
        }),
      );
    });
  const onConfirmFailure = (err: TxConfirmError) =>
    Effect.logError(`Confirm tx error: ${err}`);
  return yield* handleSignSubmit(
    lucid,
    txBuilder,
    onSubmitFailure,
    onConfirmFailure,
  );
});
