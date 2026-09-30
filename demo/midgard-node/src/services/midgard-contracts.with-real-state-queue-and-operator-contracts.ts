import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildRealComputationThreadValidator,
  buildRealDaAttestationValidator,
  buildRealDaBondPoolValidator,
  buildRealDaParamsGovernorValidator,
  buildRealFraudProofCatalogueValidator,
  buildRealFraudProofSharedWithdrawalValidators,
  buildRealFraudProofValidator,
  buildRealHubOracleValidator,
  type HubOracleOneShotOutRef,
  normalizeHubOracleOneShotOutRef,
  type RealContractDeploymentParameters,
} from "./midgard-contracts.build-real-hub-oracle-validator.js";
import {
  buildRealActiveOperatorsValidator,
  buildRealCorrectionLockValidator,
  buildRealPayoutValidator,
  buildRealRegisteredOperatorsValidator,
  buildRealReserveValidator,
  buildRealRetiredOperatorsValidator,
  buildRealSchedulerValidator,
  buildRealSettlementValidator,
  buildRealStateQueueValidator,
  buildRealTxOrderContracts,
} from "./midgard-contracts.build-real-no-reference-input-first-step-validator.js";
import {
  buildRealAvailabilityChallengeValidator,
  buildRealFaultProofContracts,
} from "./midgard-contracts.build-real-validation-trace-dispute-validator.js";
import { loadRealBlueprint } from "./midgard-contracts.load-reference-script-auth-validator.js";

/**
 * Replaces hub-oracle, deposit, operator-list, scheduler, and state-queue
 * contracts with their real blueprint-derived counterparts.
 */
export const withRealStateQueueAndOperatorContracts = (
  network: Network,
  baseContracts: SDK.MidgardValidators,
  hubOracleOneShotOutRef: HubOracleOneShotOutRef,
  deploymentParameters: RealContractDeploymentParameters,
): Effect.Effect<SDK.MidgardValidators, Error> =>
  Effect.gen(function* () {
    const normalizedOneShotOutRef = yield* normalizeHubOracleOneShotOutRef(
      hubOracleOneShotOutRef,
    );
    const daParamsGovernorInitOutRef = yield* normalizeHubOracleOneShotOutRef(
      deploymentParameters.daParamsGovernorInitOutRef ??
        normalizedOneShotOutRef,
    );
    const daParamsMaxCommitteeSize =
      deploymentParameters.daParamsMaxCommitteeSize ?? 256;
    const daParamsMaxOwnerCount =
      deploymentParameters.daParamsMaxOwnerCount ?? 16;

    const realHubOracle = yield* buildRealHubOracleValidator(
      network,
      baseContracts.hubOracle,
      normalizedOneShotOutRef,
    );
    // Build order: DA params governor -> DA bond pool -> availability
    // challenge -> correction lock -> (later) DA attestation. Each of these
    // takes parameters only from contracts built before it: the pool reads the
    // DA params policy, the availability timeout yield reads the pool policy,
    // `correction_lock.spend` reads the availability policy, and the
    // attestation reads the governor, hub and pool policies.
    const realDaParamsGovernor = yield* buildRealDaParamsGovernorValidator(
      network,
      daParamsGovernorInitOutRef,
      daParamsMaxCommitteeSize,
      daParamsMaxOwnerCount,
    );
    // The pool is a one-shot on the hub nonce, which the atomic protocol init
    // spends while it mints the pool NFT.
    const realDaBondPool = yield* buildRealDaBondPoolValidator(
      network,
      normalizedOneShotOutRef,
      realHubOracle.policyId,
      realDaParamsGovernor.policyId,
      deploymentParameters.availabilityChallengeParameters,
    );
    const realAvailabilityChallenge =
      yield* buildRealAvailabilityChallengeValidator(
        network,
        realHubOracle.policyId,
        deploymentParameters.referenceScriptAuth.policyId,
        realDaBondPool.policyId,
        deploymentParameters.availabilityChallengeParameters,
      );
    const realCorrectionLock = yield* buildRealCorrectionLockValidator(
      network,
      realHubOracle.policyId,
      realAvailabilityChallenge.policyId,
    );
    const withRealHubOracle: SDK.MidgardValidators = {
      ...baseContracts,
      referenceScriptAuth: deploymentParameters.referenceScriptAuth,
      hubOracle: realHubOracle,
      daParamsGovernor: realDaParamsGovernor,
      daBondPool: realDaBondPool,
      correctionLock: realCorrectionLock,
      availabilityChallenge: realAvailabilityChallenge,
    };

    const realFraudProofCatalogue =
      yield* buildRealFraudProofCatalogueValidator(network, withRealHubOracle);
    const withRealFraudProofCatalogue: SDK.MidgardValidators = {
      ...withRealHubOracle,
      fraudProofCatalogue: realFraudProofCatalogue,
    };

    const realComputationThread = yield* buildRealComputationThreadValidator(
      withRealFraudProofCatalogue,
    );
    const realFraudProof = yield* buildRealFraudProofValidator(
      network,
      realComputationThread,
    );
    const realFraudProofSharedWithdrawals =
      yield* buildRealFraudProofSharedWithdrawalValidators();
    const realFaultProofContracts = yield* buildRealFaultProofContracts(
      network,
      withRealFraudProofCatalogue,
      realComputationThread,
      realFraudProof,
      deploymentParameters.eventHistoryBounds,
    );
    const withRealFraudProof: SDK.MidgardValidators = {
      ...withRealFraudProofCatalogue,
      computationThread: realComputationThread,
      fraudProof: realFraudProof,
      ...realFraudProofSharedWithdrawals,
      fraudProofContracts: realFaultProofContracts,
      fraudProofs: SDK.fraudProofContractsToFirstSteps(realFaultProofContracts),
    };

    const realRetiredOperators = yield* buildRealRetiredOperatorsValidator(
      network,
      withRealFraudProof,
    );
    const withRealRetiredOperators: SDK.MidgardValidators = {
      ...withRealFraudProof,
      retiredOperators: realRetiredOperators,
    };

    const realRegisteredOperators =
      yield* buildRealRegisteredOperatorsValidator(
        network,
        withRealRetiredOperators,
      );
    const withRealRegisteredOperators: SDK.MidgardValidators = {
      ...withRealRetiredOperators,
      registeredOperators: realRegisteredOperators,
    };

    const realActiveOperators = yield* buildRealActiveOperatorsValidator(
      network,
      withRealRegisteredOperators,
    );
    const withRealOperatorSets: SDK.MidgardValidators = {
      ...withRealRegisteredOperators,
      activeOperators: realActiveOperators,
    };

    const historyBlueprint = yield* loadRealBlueprint();
    const eventHistory = yield* Effect.try({
      try: () =>
        SDK.buildEventHistoryDeployments({
          blueprint: historyBlueprint,
          network,
          hubOraclePolicyId: withRealOperatorSets.hubOracle.policyId,
          initializationNonce: hubOracleOneShotOutRef,
          protectionDurationMs:
            deploymentParameters.eventHistoryProtectionDurationMs,
          bounds: deploymentParameters.eventHistoryBounds,
        }),
      catch: (cause) =>
        new Error("Failed to derive authenticated event history", { cause }),
    });
    const withRealHubOracleAndDeposit: SDK.MidgardValidators = {
      ...withRealOperatorSets,
      eventHistory,
      deposit: eventHistory.deposit.list,
      withdrawal: eventHistory.withdrawal.list,
    };

    const realTxOrderContracts = yield* buildRealTxOrderContracts(
      network,
      withRealHubOracleAndDeposit.hubOracle.policyId,
    );
    const withRealHubOracleDepositAndTxOrder: SDK.MidgardValidators = {
      ...withRealHubOracleAndDeposit,
      txOrder: realTxOrderContracts.txOrder,
      // #579 ruling A. The real certificate has to be propagated, not left as
      // the always-succeeds stand-in it inherits from the base set: the tx-order
      // mint above is parameterized by THIS policy id, so a set that reported
      // the stand-in would describe a door the deployed script does not consult.
      fieldPreimageCertificate: realTxOrderContracts.fieldPreimageCertificate,
      cekProgramMaterial: realTxOrderContracts.cekProgramMaterial,
    };

    const withRealUserEvents = withRealHubOracleDepositAndTxOrder;

    const realScheduler = yield* buildRealSchedulerValidator(
      network,
      withRealUserEvents,
    );
    const withRealScheduler: SDK.MidgardValidators = {
      ...withRealUserEvents,
      scheduler: realScheduler,
    };

    const realSettlement = yield* buildRealSettlementValidator(
      network,
      withRealScheduler,
    );
    const withRealSettlement: SDK.MidgardValidators = {
      ...withRealScheduler,
      settlement: realSettlement,
    };

    const realDaAttestation = yield* buildRealDaAttestationValidator(
      network,
      withRealSettlement,
      deploymentParameters.referenceScriptAuth.policyId,
      deploymentParameters.availabilityChallengeParameters,
    );
    const withRealDaAttestation: SDK.MidgardValidators = {
      ...withRealSettlement,
      daAttestation: realDaAttestation,
    };

    const realStateQueue = yield* buildRealStateQueueValidator(
      network,
      withRealDaAttestation,
      deploymentParameters.referenceScriptAuth.policyId,
    );

    const withRealStateQueue: SDK.MidgardValidators = {
      ...withRealDaAttestation,
      stateQueue: realStateQueue,
    };

    const realPayout = yield* buildRealPayoutValidator(
      network,
      withRealStateQueue,
    );
    const withRealPayout: SDK.MidgardValidators = {
      ...withRealStateQueue,
      payout: realPayout,
    };

    const realReserve = yield* buildRealReserveValidator(
      network,
      withRealPayout,
    );
    return {
      ...withRealPayout,
      reserve: realReserve,
    };
  });
