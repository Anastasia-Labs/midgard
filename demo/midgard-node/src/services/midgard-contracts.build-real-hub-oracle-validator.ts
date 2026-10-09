import { normalizeOutRef } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Network,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { loadRealBlueprint } from "./midgard-contracts.load-reference-script-auth-validator.js";

/**
 * One-shot outref used to parameterize the real hub-oracle policy.
 */
export type HubOracleOneShotOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type RealContractDeploymentParameters = {
  readonly referenceScriptAuth: SDK.MintingValidator;
  readonly availabilityChallengeParameters: SDK.DaAvailabilityParameters;
  readonly eventHistoryBounds: SDK.EventHistoryPayloadBounds;
  readonly eventHistoryProtectionDurationMs: bigint;
  readonly daParamsGovernorInitOutRef?: HubOracleOneShotOutRef;
  readonly daParamsMaxCommitteeSize?: number;
  readonly daParamsMaxOwnerCount?: number;
};

/**
 * Normalizes the configured one-shot outref used to parameterize the real
 * hub-oracle policy.
 */
export const normalizeHubOracleOneShotOutRef = (
  outRef: HubOracleOneShotOutRef,
): Effect.Effect<HubOracleOneShotOutRef, Error> =>
  Effect.try({
    try: () => normalizeOutRef(outRef),
    catch: (cause) =>
      new Error(`Invalid hub-oracle one-shot outref: ${String(cause)}`),
  });

/**
 * Builds the real hub-oracle minting validator parameterized by the configured
 * one-shot outref.
 */
export const buildRealHubOracleValidator = (
  network: Network,
  fallbackSpendingValidator: SDK.SpendingValidator,
  oneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const { mintingScriptCBOR, mintingScript, policyId } = yield* Effect.try({
      try: () =>
        SDK.buildHubOracleMintingValidator({ blueprint, oneShotOutRef }),
      catch: (cause) =>
        new Error("Failed to derive hub-oracle minting validator", { cause }),
    });
    return {
      spendingScriptCBOR: fallbackSpendingValidator.spendingScriptCBOR,
      spendingScript: fallbackSpendingValidator.spendingScript,
      // The canonical Aiken tree only ships the one-shot mint policy. The
      // witness UTxO lives at the script credential derived from that policy id.
      spendingScriptHash: policyId,
      spendingScriptAddress: credentialToAddress(
        network,
        scriptHashToCredential(policyId),
      ),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

export const buildRealFraudProofCatalogueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildFraudProofCatalogueValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build FraudProofCatalogueValidator", { cause }),
    });
  });

export const buildRealComputationThreadValidator = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.MintingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildComputationThreadValidator(blueprint, contracts),
      catch: (cause) =>
        new Error("Failed to build ComputationThreadValidator", { cause }),
    });
  });

export const buildRealFraudProofValidator = (
  network: Network,
  computationThread: SDK.MintingValidator,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildFraudProofValidator(blueprint, network, computationThread),
      catch: (cause) =>
        new Error("Failed to build FraudProofValidator", { cause }),
    });
  });

export const buildRealFraudProofSharedWithdrawalValidators = (): Effect.Effect<
  Readonly<{
    chunkedVerify: SDK.WithdrawalValidator;
    pexcludes: SDK.WithdrawalValidator;
  }>,
  Error
> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildFraudProofSharedWithdrawalValidators(blueprint),
      catch: (cause) =>
        new Error("Failed to build FraudProofSharedWithdrawalValidators", {
          cause,
        }),
    });
  });

export const buildRealDaParamsGovernorValidator = (
  network: Network,
  initOutRef: HubOracleOneShotOutRef,
  maxCommitteeSize: number,
  maxOwnerCount: number,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildDaParamsGovernorValidator(
          blueprint,
          network,
          initOutRef,
          maxCommitteeSize,
          maxOwnerCount,
        ),
      catch: (cause) =>
        new Error("Failed to build DaParamsGovernorValidator", { cause }),
    });
  });

export const buildRealDaAttestationValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicyId: string,
  availabilityParameters: SDK.DaAvailabilityParameters,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildDaAttestationValidator(
          blueprint,
          network,
          contracts,
          referenceScriptAuthPolicyId,
          availabilityParameters,
        ),
      catch: (cause) =>
        new Error("Failed to build DaAttestationValidator", { cause }),
    });
  });

export const buildRealDaBondPoolValidator = (
  network: Network,
  initOutRef: HubOracleOneShotOutRef,
  hubOraclePolicyId: string,
  daParamsPolicyId: string,
  parameters: SDK.DaAvailabilityParameters,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildDaBondPoolValidator(
          blueprint,
          network,
          initOutRef,
          hubOraclePolicyId,
          daParamsPolicyId,
          parameters,
        ),
      catch: (cause) =>
        new Error("Failed to build DaBondPoolValidator", { cause }),
    });
  });
