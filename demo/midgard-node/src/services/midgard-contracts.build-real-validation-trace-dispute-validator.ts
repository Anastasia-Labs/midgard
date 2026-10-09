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
