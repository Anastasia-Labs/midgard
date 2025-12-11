import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "./utils.js";

export const initializeMidgard = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const genesisTime = BigInt(Date.now());

  const operatorAddress = yield* Effect.promise(() => lucid.wallet().address());
  const operatorCredential = paymentCredentialOf(operatorAddress);
  const initialOperator = operatorCredential.hash;

  //TODO: Move to SDK
  const initParams: SDK.InitializationParams = {
    genesisTime,
    initialOperator,
    hubOracle: contracts.hubOracleMintValidator,
    stateQueue: contracts.stateQueueAuthValidator,
    registeredOperators: contracts.registeredOperatorsAuthValidator,
    activeOperators: contracts.activeOperatorsAuthValidator,
    retiredOperators: contracts.retiredOperatorsAuthValidator,
    scheduler: contracts.schedulerAuthValidator,
    fraudProofCatalogue: contracts.fraudProofCatalogueAuthValidator,
    fraudProof: contracts.fraudProofAuthValidator,
  };

  const unsignedTx = yield* SDK.unsignedInitializationTxProgram(
    lucid,
    initParams,
  );
  yield* handleSignSubmit(lucid, unsignedTx);
});
