import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "./utils.js";

import {
  MidgardMpt,
  MptError,
  utxoToBatchOps,
  computeRootFromUtxos,
} from "../workers/utils/mpt.js";
import { getGenesisUtxosFromValidators } from "../genesis.js";
import { UTxO, Script } from "@lucid-evolution/lucid";
import * as ETH_UTILS from "@ethereumjs/util";

const computeGenesisMptRoot = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  string,
  MptError | SDK.HashingError | SDK.DataCoercionError
> =>
  Effect.gen(function* () {
    const utxos = yield* getGenesisUtxosFromValidators(contracts);
    return yield* computeRootFromUtxos(utxos);
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
