import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import {
  HubOracleDatum,
  unsignedInitializationTxProgram,
  InitializationParams,
  fromAddress,
  GENESIS_HASH_32,
} from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "./utils.js";
import { genesisTime, stateQueueData } from "./state-queue/init.js";

/**
 * Initializes all Midgard contracts in a single transaction.
 * Uses tx-builders from midgard-sdk.
 */
export const initializeMidgard = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const operatorAddress = yield* Effect.promise(() => lucid.wallet().address());
  const operatorCredential = paymentCredentialOf(operatorAddress);
  const initialOperator = operatorCredential.hash;

  const [
    registeredOperatorsAddr,
    activeOperatorsAddr,
    retiredOperatorsAddr,
    schedulerAddr,
    stateQueueAddr,
    fraudProofCatalogueAddr,
    fraudProofAddr,
  ] = yield* Effect.all([
    fromAddress(contracts.registeredOperatorsAuthValidator.spendScriptAddress),
    fromAddress(contracts.activeOperatorsAuthValidator.spendScriptAddress),
    fromAddress(contracts.retiredOperatorsAuthValidator.spendScriptAddress),
    fromAddress(contracts.schedulerAuthValidator.spendScriptAddress),
    fromAddress(contracts.stateQueueAuthValidator.spendScriptAddress),
    fromAddress(contracts.fraudProofCatalogueAuthValidator.spendScriptAddress),
    fromAddress(contracts.fraudProofAuthValidator.spendScriptAddress),
  ]);

  const hubOracleDatum: HubOracleDatum = {
    registeredOperators: contracts.registeredOperatorsAuthValidator.policyId,
    activeOperators: contracts.activeOperatorsAuthValidator.policyId,
    retiredOperators: contracts.retiredOperatorsAuthValidator.policyId,
    scheduler: contracts.schedulerAuthValidator.policyId,
    stateQueue: contracts.stateQueueAuthValidator.policyId,
    fraudProofCatalogue: contracts.fraudProofCatalogueAuthValidator.policyId,
    fraudProof: contracts.fraudProofAuthValidator.policyId,

    registeredOperatorsAddr,
    activeOperatorsAddr,
    retiredOperatorsAddr,
    schedulerAddr,
    stateQueueAddr,
    fraudProofCatalogueAddr,
    fraudProofAddr,
  };

  const initParams: InitializationParams = {
    genesisTime,
    initialOperator,

    hubOracle: {
      validator: contracts.hubOracleAuthValidator,
      datum: hubOracleDatum,
    },

    stateQueue: {
      validator: contracts.stateQueueAuthValidator,
      data: stateQueueData,
    },

    registeredOperators: {
      validator: contracts.registeredOperatorsAuthValidator,
      data: [],
    },

    activeOperators: {
      validator: contracts.activeOperatorsAuthValidator,
      data: [],
    },

    retiredOperators: {
      validator: contracts.retiredOperatorsAuthValidator,
      data: [],
    },

    scheduler: {
      validator: contracts.schedulerAuthValidator,
      operator: initialOperator,
      startTime: genesisTime,
    },

    fraudProofCatalogue: {
      validator: contracts.fraudProofCatalogueAuthValidator,
      mptRootHash: GENESIS_HASH_32,
    },
  };

  const unsignedTx = yield* unsignedInitializationTxProgram(lucid, initParams);
  yield* handleSignSubmit(lucid, unsignedTx);
});
