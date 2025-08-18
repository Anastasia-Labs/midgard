import { Effect } from "effect";
import { ConfirmError, handleSignSubmit, SubmitError } from "../utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceeds } from "@/services/index.js";
import { NodeConfig, User } from "@/config.js";

export const stateQueueInit = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { user: lucid } = yield* User;
  const { spendScriptAddress, mintScript, policyId } =
    yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const initParams: SDK.TxBuilder.StateQueue.InitParams = {
    address: spendScriptAddress,
    policyId: policyId,
    stateQueueMintingScript: mintScript,
  };
  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
  const txBuilder = yield* SDK.Endpoints.initTxProgram(lucid, initParams);
  const onSubmitFailure = (err: SubmitError) =>
    Effect.gen(function* () {
      yield* Effect.logError(`Sumbit tx error: ${err}`);
      yield* Effect.fail(err.err);
    });
  const onConfirmFailure = (err: ConfirmError) =>
    Effect.logError(`Confirm tx error: ${err}`);
  return yield* handleSignSubmit(
    lucid,
    txBuilder,
    onSubmitFailure,
    onConfirmFailure,
  );
});
