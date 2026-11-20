import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { normalizeOutRef } from "@al-ft/midgard-core/out-ref";
import type * as SDKCore from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import * as SDKDeployment from "@al-ft/midgard-sdk/deployment";
import * as SDK from "@al-ft/midgard-sdk/users";
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
  readonly referenceScriptAuth: SDKCore.MintingValidator;
  readonly availabilityChallengeParameters: SDKDa.DaAvailabilityParameters;
  readonly eventHistoryBounds: SDK.EventHistoryPayloadBounds;
  readonly eventHistoryProtectionDurationMs: bigint;
  /** The deployment's L1 slot mapping, a validation-trace validator parameter. */
  readonly l1SlotConfig: L1SlotConfig;
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
  oneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDKCore.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const { mintingScriptCBOR, mintingScript, policyId } = yield* Effect.try({
      try: () =>
        SDK.buildHubOracleMintingValidator({ blueprint, oneShotOutRef }),
      catch: (cause) =>
        new Error("Failed to derive hub-oracle minting validator", { cause }),
    });
    return {
      // The witness credential already uses this mint policy's hash. Its
      // else handler fails for spending, so the hub witness stays immutable.
      spendingScriptCBOR: mintingScriptCBOR,
      spendingScript: mintingScript,
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
  contracts: SDKCore.MidgardValidators,
): Effect.Effect<SDKCore.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildFraudProofCatalogueValidator(
          blueprint,
          network,
          contracts,
        ),
      catch: (cause) =>
        new Error("Failed to build FraudProofCatalogueValidator", { cause }),
    });
  });

export const buildRealComputationThreadValidator = (
  contracts: SDKCore.MidgardValidators,
): Effect.Effect<SDKCore.MintingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildComputationThreadValidator(blueprint, contracts),
      catch: (cause) =>
        new Error("Failed to build ComputationThreadValidator", { cause }),
    });
  });

export const buildRealFraudProofValidator = (
  network: Network,
  computationThread: SDKCore.MintingValidator,
): Effect.Effect<SDKCore.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildFraudProofValidator(
          blueprint,
          network,
          computationThread,
        ),
      catch: (cause) =>
        new Error("Failed to build FraudProofValidator", { cause }),
    });
  });

export const buildRealFraudProofSharedWithdrawalValidators = (): Effect.Effect<
  Readonly<{
    chunkedVerify: SDKCore.WithdrawalValidator;
    pexcludes: SDKCore.WithdrawalValidator;
  }>,
  Error
> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildFraudProofSharedWithdrawalValidators(blueprint),
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
): Effect.Effect<SDKCore.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildDaParamsGovernorValidator(
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
  contracts: SDKCore.MidgardValidators,
  referenceScriptAuthPolicyId: string,
  availabilityParameters: SDKDa.DaAvailabilityParameters,
): Effect.Effect<SDKCore.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildDaAttestationValidator(
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
  parameters: SDKDa.DaAvailabilityParameters,
): Effect.Effect<SDKCore.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDKDeployment.buildDaBondPoolValidator(
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
