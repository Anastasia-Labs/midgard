import { Effect } from "effect";
import { Lucid } from "../services/lucid.js";
import { AlwaysSucceedsContract } from "../services/always-succeeds.js";
import {
  paymentCredentialOf,
  getAddressDetails,
  Address,
} from "@lucid-evolution/lucid";
import {
  HubOracleDatum,
  InitializationParams,
  ConfirmedState,
  EmptyRootData,
  unsignedInitializationProgram,
  AddressData,
} from "@al-ft/midgard-sdk";

/**
 * Initializes all Midgard contracts in a single transaction.
 * Uses tx-builders from midgard-sdk.
 */
export const initializeMidgard = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  // Switch to operator wallet
  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  // Get nonce UTxO
  const utxos = yield* Effect.tryPromise(() => lucid.wallet().getUtxos());
  if (utxos.length === 0) {
    return yield* Effect.fail(new Error("No UTxOs available for nonce"));
  }

  const genesisTime = BigInt(Date.now());

  // Get operator PKH
  const operatorAddress = yield* Effect.promise(() => lucid.wallet().address());
  const operatorCredential = paymentCredentialOf(operatorAddress);
  const initialOperator = operatorCredential.hash;

  function fromAddress(address: Address): AddressData {
    // We do not support pointer addresses!

    const { paymentCredential, stakeCredential } = getAddressDetails(address);

    if (!paymentCredential) throw new Error("Not a valid payment address.");

    return {
      paymentCredential:
        paymentCredential?.type === "Key"
          ? {
              PublicKeyCredential: [paymentCredential.hash],
            }
          : { ScriptCredential: [paymentCredential.hash] },
      stakeCredential: stakeCredential
        ? {
            Inline: [
              stakeCredential.type === "Key"
                ? {
                    PublicKeyCredential: [stakeCredential.hash],
                  }
                : { ScriptCredential: [stakeCredential.hash] },
            ],
          }
        : null,
    };
  }

  // Build hub oracle datum
  const hubOracleDatum: HubOracleDatum = {
    registeredOperators: contracts.registeredOperatorsAuthValidator.policyId,
    activeOperators: contracts.activeOperatorsAuthValidator.policyId,
    retiredOperators: contracts.retiredOperatorsAuthValidator.policyId,
    scheduler: contracts.schedulerAuthValidator.policyId,
    stateQueue: contracts.stateQueueAuthValidator.policyId,
    fraudProofCatalogue: contracts.fraudProofCatalogueAuthValidator.policyId,
    fraudProof: contracts.fraudProofAuthValidator.policyId,

    registeredOperatorsAddr: fromAddress(
      contracts.registeredOperatorsAuthValidator.spendScriptAddress,
    ),
    activeOperatorsAddr: fromAddress(
      contracts.activeOperatorsAuthValidator.spendScriptAddress,
    ),
    retiredOperatorsAddr: fromAddress(
      contracts.retiredOperatorsAuthValidator.spendScriptAddress,
    ),
    schedulerAddr: fromAddress(
      contracts.schedulerAuthValidator.spendScriptAddress,
    ),
    stateQueueAddr: fromAddress(
      contracts.stateQueueAuthValidator.spendScriptAddress,
    ),
    fraudProofCatalogueAddr: fromAddress(
      contracts.fraudProofCatalogueAuthValidator.spendScriptAddress,
    ),
    fraudProofAddr: fromAddress(
      contracts.fraudProofAuthValidator.spendScriptAddress,
    ),
  };

  // Build initialization params - uses SDK types!
  const initParams: InitializationParams = {
    nonceUtxo: utxos[0],
    genesisTime,
    initialOperator,

    hubOracle: {
      policyId: contracts.hubOracleAuthValidator.policyId,
      address: contracts.hubOracleAuthValidator.spendScriptAddress,
      mintScript: contracts.hubOracleAuthValidator.mintScript,
      datum: hubOracleDatum,
    },

    stateQueue: {
      policyId: contracts.stateQueueAuthValidator.policyId,
      address: contracts.stateQueueAuthValidator.spendScriptAddress,
      mintScript: contracts.stateQueueAuthValidator.mintScript,
      dataSchema: ConfirmedState,
      data: {
        headerHash: "00".repeat(28),
        prevHeaderHash: "00".repeat(28),
        utxoRoot: "00".repeat(32),
        startTime: BigInt(Date.now()),
        endTime: BigInt(Date.now()),
        protocolVersion: 0n,
      },
    },

    settlementQueue: {
      policyId: contracts.settlementQueueAuthValidator.policyId,
      address: contracts.settlementQueueAuthValidator.spendScriptAddress,
      mintScript: contracts.settlementQueueAuthValidator.mintScript,
      dataSchema: EmptyRootData,
      data: [],
    },

    registeredOperators: {
      policyId: contracts.registeredOperatorsAuthValidator.policyId,
      address: contracts.registeredOperatorsAuthValidator.spendScriptAddress,
      mintScript: contracts.registeredOperatorsAuthValidator.mintScript,
      dataSchema: EmptyRootData,
      data: [],
    },

    activeOperators: {
      policyId: contracts.activeOperatorsAuthValidator.policyId,
      address: contracts.activeOperatorsAuthValidator.spendScriptAddress,
      mintScript: contracts.activeOperatorsAuthValidator.mintScript,
      dataSchema: EmptyRootData,
      data: [],
    },

    retiredOperators: {
      policyId: contracts.retiredOperatorsAuthValidator.policyId,
      address: contracts.retiredOperatorsAuthValidator.spendScriptAddress,
      mintScript: contracts.retiredOperatorsAuthValidator.mintScript,
      dataSchema: EmptyRootData,
      data: [],
    },

    scheduler: {
      policyId: contracts.schedulerAuthValidator.policyId,
      address: contracts.schedulerAuthValidator.spendScriptAddress,
      mintScript: contracts.schedulerAuthValidator.mintScript,
      dataSchema: EmptyRootData,
      data: [],
    },

    fraudProofCatalogue: {
      policyId: contracts.fraudProofCatalogueAuthValidator.policyId,
      address: contracts.fraudProofCatalogueAuthValidator.spendScriptAddress,
      mintScript: contracts.fraudProofCatalogueAuthValidator.mintScript,
      mptRootHash: "00".repeat(32),
    },
  };

  // Use SDK function to build the transaction!
  const unsignedTx = yield* unsignedInitializationProgram(lucid, initParams);

  // Sign and submit
  const signedTx = yield* Effect.tryPromise({
    try: () => unsignedTx.sign.withWallet().complete(),
    catch: (e) => new Error(`Failed to sign initialization: ${e}`),
  });

  const txHash = yield* Effect.tryPromise({
    try: () => signedTx.submit(),
    catch: (e) => new Error(`Failed to submit initialization: ${e}`),
  });

  console.log(`Submitting ...`);
  yield* Effect.tryPromise({
    try: () => lucid.awaitTx(txHash),
    catch: (e) => new Error(`Failed to await transaction: ${e}`),
  });

  console.log(`✅ Midgard initialized! TxHash: ${txHash}`);
  return txHash;
});
