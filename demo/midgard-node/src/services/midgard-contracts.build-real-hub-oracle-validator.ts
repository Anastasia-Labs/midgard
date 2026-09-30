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
 * Blueprint titles for the real state-queue scripts.
 */
export const REAL_STATE_QUEUE_SCRIPT_TITLES = SDK.STATE_QUEUE_SCRIPT_TITLES;

export const REAL_CORRECTION_LOCK_SCRIPT_TITLES =
  SDK.CORRECTION_LOCK_SCRIPT_TITLES;

export const REAL_DA_PARAMS_GOVERNOR_SCRIPT_TITLES =
  SDK.DA_PARAMS_GOVERNOR_SCRIPT_TITLES;

export const REAL_DA_ATTESTATION_SCRIPT_TITLES =
  SDK.DA_ATTESTATION_SCRIPT_TITLES;

export const REAL_DA_BOND_POOL_SCRIPT_TITLES = SDK.DA_BOND_POOL_SCRIPT_TITLES;

export const REAL_AVAILABILITY_CHALLENGE_SCRIPT_TITLES =
  SDK.AVAILABILITY_CHALLENGE_SCRIPT_TITLES;

/**
 * Blueprint titles for the real hub-oracle scripts.
 */
export const REAL_HUB_ORACLE_SCRIPT_TITLES =
  SDK.USER_EVENT_CONTRACT_TITLES.hubOracle;

/**
 * Blueprint titles for the real registered-operators scripts.
 */
export const REAL_REGISTERED_OPERATORS_SCRIPT_TITLES =
  SDK.REGISTERED_OPERATORS_SCRIPT_TITLES;

/**
 * Blueprint titles for the real active-operators scripts.
 */
export const REAL_ACTIVE_OPERATORS_SCRIPT_TITLES =
  SDK.ACTIVE_OPERATORS_SCRIPT_TITLES;

/**
 * Blueprint titles for the real retired-operators scripts.
 */
export const REAL_RETIRED_OPERATORS_SCRIPT_TITLES =
  SDK.RETIRED_OPERATORS_SCRIPT_TITLES;

/**
 * Blueprint titles for the real scheduler scripts.
 */
export const REAL_SCHEDULER_SCRIPT_TITLES = SDK.SCHEDULER_SCRIPT_TITLES;

/**
 * Blueprint titles for the real deposit scripts.
 */
export const REAL_DEPOSIT_SCRIPT_TITLES = SDK.EVENT_HISTORY_CONTRACT_TITLES;

/**
 * Blueprint titles for the real tx-order scripts.
 */
export const REAL_TX_ORDER_SCRIPT_TITLES =
  SDK.USER_EVENT_CONTRACT_TITLES.txOrder;

/**
 * Blueprint titles for the real withdrawal scripts.
 */
export const REAL_WITHDRAWAL_SCRIPT_TITLES = SDK.EVENT_HISTORY_CONTRACT_TITLES;

/**
 * Blueprint titles for the real settlement scripts.
 */
export const REAL_SETTLEMENT_SCRIPT_TITLES = SDK.SETTLEMENT_SCRIPT_TITLES;

/**
 * Blueprint titles for the real reserve scripts.
 */
export const REAL_RESERVE_SCRIPT_TITLES = SDK.RESERVE_SCRIPT_TITLES;

/**
 * Blueprint titles for the real payout scripts.
 */
export const REAL_PAYOUT_SCRIPT_TITLES = SDK.PAYOUT_SCRIPT_TITLES;

export const REAL_FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES =
  SDK.FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES;

export const REAL_COMPUTATION_THREAD_SCRIPT_TITLES =
  SDK.COMPUTATION_THREAD_SCRIPT_TITLES;

export const REAL_FRAUD_PROOF_SCRIPT_TITLES = SDK.FRAUD_PROOF_SCRIPT_TITLES;

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
