import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "./utils.js";

export const initializeMidgard = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  //TODO: Move to SDK
  const initParams: SDK.InitializationParams = {
    midgardValidators: contracts,
  };

  const unsignedTx = yield* SDK.unsignedInitializationTxProgram(
    lucid,
    initParams,
  );
  yield* handleSignSubmit(lucid, unsignedTx);
});
