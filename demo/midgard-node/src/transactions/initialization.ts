import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "./utils.js";

import {
  MptError,
  keyValueMptRoot,
} from "../workers/utils/mpt.js";
import { getGenesisScriptInputs } from "../genesis.js";

const computeGenesisMptRoot = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<string, MptError | SDK.HashingError> =>
  Effect.gen(function* () {
    const { keys, values } = yield* getGenesisScriptInputs(contracts);
    return yield* keyValueMptRoot(keys, values);
  });

export const initializeMidgard = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const mptRootHash = yield* computeGenesisMptRoot(contracts);

  //TODO: Move to SDK
  const initParams: SDK.InitializationParams = {
    midgardValidators: contracts,
    mptRootHash: mptRootHash,
  };

  const unsignedTx = yield* SDK.unsignedInitializationTxProgram(
    lucid,
    initParams,
  );
  const txHash = yield* handleSignSubmit(lucid, unsignedTx);

  return txHash;
});
