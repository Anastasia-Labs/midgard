import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { loadRealBlueprint } from "./midgard-contracts.load-reference-script-auth-validator.js";

export const buildRealAvailabilityChallengeValidator = (
  network: Network,
  hubOraclePolicyId: string,
  referenceScriptAuthPolicyId: string,
  daBondPoolPolicyId: string,
  parameters: SDK.DaAvailabilityParameters,
): Effect.Effect<SDK.AvailabilityChallengeValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildAvailabilityChallengeValidator(
          blueprint,
          network,
          hubOraclePolicyId,
          referenceScriptAuthPolicyId,
          daBondPoolPolicyId,
          parameters,
        ),
      catch: (cause) =>
        new Error("Failed to build AvailabilityChallengeValidator", { cause }),
    });
  });

export const expectDerivedScriptHash = (
  label: string,
  expected: string,
  actual: string,
): Effect.Effect<void, Error> =>
  expected === actual
    ? Effect.void
    : Effect.fail(
        new Error(
          `${label} mismatch while deriving real fault-proof contracts: expected=${expected}, actual=${actual}`,
        ),
      );

export const buildRealFaultProofContracts = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
  eventHistoryBounds: SDK.EventHistoryPayloadBounds,
): Effect.Effect<SDK.FaultProofContractChains, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const derived = yield* SDK.buildFaultProofContracts({
      eventHistoryBounds,
      blueprint,
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
    });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      derived.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      derived.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      derived.fraudProof.spendingScriptHash,
    );

    return derived;
  });

export const buildRealDoubleSpendFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const doubleSpendContracts = yield* SDK.buildDoubleSpendFaultProofContracts(
      {
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      },
    );

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      doubleSpendContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      doubleSpendContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      doubleSpendContracts.fraudProof.spendingScriptHash,
    );

    return doubleSpendContracts.doubleSpend.firstStep;
  });

export const buildRealTransitionTraceProofValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const transitionTraceContracts =
      yield* SDK.buildTransitionTraceFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        eventHistoryBounds:
          contracts.fraudProofContracts.transitionTrace.history,
        referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      transitionTraceContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      transitionTraceContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      transitionTraceContracts.fraudProof.spendingScriptHash,
    );

    return transitionTraceContracts.transitionTrace.firstStep;
  });

export const buildRealValidationTraceDisputeValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.ValidationTraceDisputeValidators, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const validationTraceContracts =
      yield* SDK.buildValidationTraceDisputeFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
        referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      validationTraceContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      validationTraceContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      validationTraceContracts.fraudProof.spendingScriptHash,
    );

    const chain = validationTraceContracts.validationTraceDispute;
    return {
      ...chain.opener,
      source: chain.source,
      game: chain.game,
      boundary: chain.boundary,
      timeout: chain.timeout,
      award: chain.award,
    };
  });

export const buildRealNonExistentInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const nonExistentInputContracts =
      yield* SDK.buildNonExistentInputFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      nonExistentInputContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      nonExistentInputContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      nonExistentInputContracts.fraudProof.spendingScriptHash,
    );

    return nonExistentInputContracts.nonExistentInput.firstStep;
  });

export const buildRealZeroInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const zeroInputContracts = yield* SDK.buildZeroInputFaultProofContracts({
      blueprint,
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
    });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      zeroInputContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      zeroInputContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      zeroInputContracts.fraudProof.spendingScriptHash,
    );

    return zeroInputContracts.zeroInput.firstStep;
  });

export const buildRealDaHashPreimageFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const daHashPreimageContracts =
      yield* SDK.buildDaHashPreimageFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      daHashPreimageContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      daHashPreimageContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      daHashPreimageContracts.fraudProof.spendingScriptHash,
    );

    return daHashPreimageContracts.daHashPreimage.firstStep;
  });
