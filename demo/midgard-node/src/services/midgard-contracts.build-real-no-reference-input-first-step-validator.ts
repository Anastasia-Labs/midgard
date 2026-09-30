import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { expectDerivedScriptHash } from "./midgard-contracts.build-real-validation-trace-dispute-validator.js";
import { loadRealBlueprint } from "./midgard-contracts.load-reference-script-auth-validator.js";

export const buildRealNoReferenceInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const noReferenceInputContracts =
      yield* SDK.buildNoReferenceInputFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      noReferenceInputContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      noReferenceInputContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      noReferenceInputContracts.fraudProof.spendingScriptHash,
    );

    return noReferenceInputContracts.noReferenceInput.firstStep;
  });

export const buildRealReferenceInputNoIdxFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const referenceInputNoIdxContracts =
      yield* SDK.buildReferenceInputNoIdxFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      referenceInputNoIdxContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      referenceInputNoIdxContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      referenceInputNoIdxContracts.fraudProof.spendingScriptHash,
    );

    return referenceInputNoIdxContracts.referenceInputNoIdx.firstStep;
  });

export const buildRealInvalidSignatureFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const invalidSignatureContracts =
      yield* SDK.buildInvalidSignatureFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      invalidSignatureContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      invalidSignatureContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      invalidSignatureContracts.fraudProof.spendingScriptHash,
    );

    return invalidSignatureContracts.invalidSignature.firstStep;
  });

/**
 * Builds the real state-queue authenticated validator.
 */
export const buildRealStateQueueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicyId: string,
): Effect.Effect<SDK.StateQueueValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* SDK.buildStateQueueValidator({
      blueprint,
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      correctionLockScriptHash: contracts.correctionLock.spendingScriptHash,
      activeOperatorsPolicyId: contracts.activeOperators.policyId,
      activeOperatorsAddress: contracts.activeOperators.spendingScriptAddress,
      retiredOperatorsPolicyId: contracts.retiredOperators.policyId,
      schedulerPolicyId: contracts.scheduler.policyId,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      settlementPolicyId: contracts.settlement.policyId,
      daAttestationPolicyId: contracts.daAttestation.policyId,
      availabilityChallengePolicyId: contracts.availabilityChallenge.policyId,
      referenceScriptAuthPolicyId,
    });
  });

export const buildRealCorrectionLockValidator = (
  network: Network,
  hubOraclePolicyId: string,
  availabilityChallengePolicyId: string,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* SDK.buildCorrectionLockValidator({
      blueprint,
      network,
      hubOraclePolicyId,
      availabilityChallengePolicyId,
    });
  });

/**
 * Builds the real registered-operators authenticated validator.
 */
export const buildRealRegisteredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildRegisteredOperatorsValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build RegisteredOperatorsValidator", { cause }),
    });
  });

/**
 * Builds the real active-operators authenticated validator.
 */
export const buildRealActiveOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildActiveOperatorsValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build ActiveOperatorsValidator", { cause }),
    });
  });

/**
 * Builds the real retired-operators authenticated validator.
 */
export const buildRealRetiredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildRetiredOperatorsValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build RetiredOperatorsValidator", { cause }),
    });
  });

/**
 * Builds the real scheduler authenticated validator.
 */
export const buildRealSchedulerValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildSchedulerValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build SchedulerValidator", { cause }),
    });
  });

export type TxOrderContracts = {
  readonly txOrder: SDK.AuthenticatedValidator;
  readonly fieldPreimageCertificate: SDK.SpendingValidator &
    SDK.MintingValidator;
  readonly cekProgramMaterial: SDK.SpendingValidator;
};

/** Derives tx-order and its certificate/material dependencies with the SDK recipe. */
export const buildRealTxOrderContracts = (
  network: Network,
  hubOraclePolicyId: string,
): Effect.Effect<TxOrderContracts, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildTxOrderValidators({ blueprint, network, hubOraclePolicyId }),
      catch: (cause) =>
        new Error("Failed to derive tx-order validators", { cause }),
    });
  });

export const buildRealSettlementValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildSettlementValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build SettlementValidator", { cause }),
    });
  });

export const buildRealReserveValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.SpendingValidator & SDK.WithdrawalValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildReserveValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build ReserveValidator", { cause }),
    });
  });

export const buildRealPayoutValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildPayoutValidator(blueprint, network, contracts),
      catch: (cause) => new Error("Failed to build PayoutValidator", { cause }),
    });
  });
