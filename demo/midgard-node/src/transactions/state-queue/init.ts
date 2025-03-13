import { Effect } from "effect";
import { handleSignSubmit } from "../utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceeds } from "@/services/index.js";
import { User } from "@/config.js";

export const stateQueueInit = Effect.gen(function* () {
  const { user: lucid } = yield* User;
  const { spendScriptAddress, mintScript, policyId } =
    yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const initParams: SDK.TxBuilder.StateQueue.InitParams = {
    address: spendScriptAddress,
    policyId: policyId,
    stateQueueMintingScript: mintScript,
  };
  const txBuilder = yield* SDK.Endpoints.initTxProgram(lucid, initParams);
  return yield* handleSignSubmit(lucid, txBuilder);
});
