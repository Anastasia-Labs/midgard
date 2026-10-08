import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  type Network,
  type TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { loadPhasMembershipWithdrawalScript } from "../phas-membership.js";
import { NodeConfig } from "../services/config.js";
import { type IntentJournal, openPlan } from "../services/intent-journal.js";
import { Lucid } from "../services/lucid.js";
import {
  type ContractDeploymentIdentityValue,
  MidgardContracts,
} from "../services/midgard-contracts.js";
import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "./availability-challenge-registration.js";
import {
  type AtomicProtocolInitReferenceScripts,
  buildFraudProofCatalogueDeploymentInfo,
  DEPLOYMENT_VISIBILITY_REFRESH_DELAY,
  DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES,
  fraudProofsToIndexedValidators,
} from "./initialization.atomic-protocol-init-reference-scripts-from-publications.js";
import {
  deriveOperatorDaParams,
  ensureAtomicProtocolInitReferenceScriptsProgram,
  fetchCorrectionLockWitness,
  fetchHubOracleWitness,
  isNodeSetInitialized,
  isSchedulerInitialized,
} from "./initialization.derive-operator-da-params.js";
import {
  completeAndSubmit,
  fetchConfiguredNonceUtxo,
  fetchHistoryRootState,
  isDaBondPoolInitialized,
  isDaParamsInitialized,
  isStateQueueInitialized,
  makePartialProtocolDeploymentError,
  type ProtocolDeploymentStatus,
  resolveDefaultDeploymentDeadline,
  resolveDeploymentValidityBounds,
} from "./initialization.fetch-configured-nonce-utxo.js";
import { ensurePhasMembershipRewardAccountRegisteredProgram } from "./phas-membership-registration.js";
import {
  ensureEventHistoryRewardAccountsRegisteredProgram,
  ensureRuntimeRewardAccountsRegisteredProgram,
} from "./script-reward-registration.js";

/**
 * Queries the current deployment state of the protocol contracts.
 */
export const fetchProtocolDeploymentStatus = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const history = SDK.requireEventHistoryContracts(contracts);
    const depositHistory = yield* fetchHistoryRootState(lucid, history.deposit);
    const withdrawalHistory = yield* fetchHistoryRootState(
      lucid,
      history.withdrawal,
    );
    const hubOracleWitness = yield* fetchHubOracleWitness(lucid, contracts);
    const correctionLockWitness = yield* fetchCorrectionLockWitness(
      lucid,
      contracts,
    );
    const stateQueueInitialized = yield* isStateQueueInitialized(
      lucid,
      contracts.stateQueue,
    );
    const daParamsInitialized = yield* isDaParamsInitialized(
      lucid,
      contracts.daParamsGovernor,
    );
    const daBondPoolInitialized = yield* isDaBondPoolInitialized(
      lucid,
      contracts.daBondPool,
    );
    const schedulerInitialized = yield* isSchedulerInitialized(
      lucid,
      contracts.scheduler,
    );
    const registeredOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.registeredOperators,
    );
    const activeOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.activeOperators,
    );
    const retiredOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.retiredOperators,
    );
    const fraudProofCatalogueInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.fraudProofCatalogue,
    );
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Failed to resolve network while building PHAS deployment identity",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const phasMembershipScript = loadPhasMembershipWithdrawalScript();
    const phasMembershipReward = SDK.phasMembershipIdentity(
      network,
      phasMembershipScript,
    );
    const missingComponents = [
      ...(!depositHistory.initialized ? ["deposit-history"] : []),
      ...(!withdrawalHistory.initialized ? ["withdrawal-history"] : []),
      ...(hubOracleWitness === null ? ["hub-oracle"] : []),
      ...(correctionLockWitness === null ? ["correction-lock"] : []),
      ...(!daParamsInitialized ? ["da-params"] : []),
      ...(!stateQueueInitialized ? ["state-queue"] : []),
      ...(!schedulerInitialized ? ["scheduler"] : []),
      ...(!registeredOperatorsInitialized ? ["registered-operators"] : []),
      ...(!activeOperatorsInitialized ? ["active-operators"] : []),
      ...(!retiredOperatorsInitialized ? ["retired-operators"] : []),
      ...(!fraudProofCatalogueInitialized ? ["fraud-proof-catalogue"] : []),
      ...(!daBondPoolInitialized ? ["da-bond-pool"] : []),
    ] as const;
    const complete =
      depositHistory.initialized &&
      withdrawalHistory.initialized &&
      hubOracleWitness !== null &&
      correctionLockWitness !== null &&
      daParamsInitialized &&
      stateQueueInitialized &&
      schedulerInitialized &&
      registeredOperatorsInitialized &&
      activeOperatorsInitialized &&
      retiredOperatorsInitialized &&
      fraudProofCatalogueInitialized &&
      daBondPoolInitialized;
    const empty =
      depositHistory.empty &&
      withdrawalHistory.empty &&
      hubOracleWitness === null &&
      correctionLockWitness === null &&
      !daParamsInitialized &&
      !stateQueueInitialized &&
      !schedulerInitialized &&
      !registeredOperatorsInitialized &&
      !activeOperatorsInitialized &&
      !retiredOperatorsInitialized &&
      !fraudProofCatalogueInitialized &&
      !daBondPoolInitialized;

    return {
      depositHistoryInitialized: depositHistory.initialized,
      withdrawalHistoryInitialized: withdrawalHistory.initialized,
      hubOracleWitness,
      correctionLockWitness,
      stateQueueInitialized,
      daParamsInitialized,
      daBondPoolInitialized,
      schedulerInitialized,
      registeredOperatorsInitialized,
      activeOperatorsInitialized,
      retiredOperatorsInitialized,
      fraudProofCatalogueInitialized,
      phasMembershipRewardAddress: phasMembershipReward.rewardAddress,
      phasMembershipScriptHash: phasMembershipReward.scriptHash,
      complete,
      empty,
      missingComponents,
    };
  });

const waitForAtomicInitializationVisibility = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const status = yield* fetchProtocolDeploymentStatus(lucid, contracts);
    if (status.complete) {
      return status;
    }
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Atomic initialization transaction is confirmed but not yet fully visible through the provider",
        cause: `missing_components=[${status.missingComponents.join(",")}]`,
      }),
    );
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(DEPLOYMENT_VISIBILITY_REFRESH_DELAY),
        Schedule.recurs(DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES),
      ),
    ),
  );

export const buildAtomicProtocolInitTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  nodeConfig: {
    HUB_ORACLE_ONE_SHOT_TX_HASH: string;
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
    L1_OPERATOR_SEED_PHRASE: string;
    NETWORK: Network;
    DA_COMMITTEE_HEX?: string;
    DA_THRESHOLD?: bigint | null;
    DA_COSIGNER_SEED_PHRASE?: string;
    DA_OWNERS_HEX?: string;
  },
  fraudProofCatalogueMerkleRoot: string,
  validTo?: bigint,
  referenceScripts?: AtomicProtocolInitReferenceScripts,
  consensusProfile: ContractDeploymentIdentityValue["consensusProfile"] = MIDGARD_CONSENSUS_PROFILE,
): Effect.Effect<
  TxBuilder,
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.UnspecifiedNetworkError
  | SDK.HashingError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const validityRange = resolveDeploymentValidityBounds(lucid, validTo);
    const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
    const daParams = yield* deriveOperatorDaParams(nodeConfig);
    return yield* SDK.incompleteInitializationTxProgram(lucid, {
      midgardValidators: contracts,
      consensusProfile,
      fraudProofCatalogueMerkleRoot,
      daParams,
      oneShotNonceUTxO: nonceUtxo,
      validityRange,
      referenceScripts,
    });
  });

/**
 * End-to-end protocol initialization program.
 *
 * The flow performs exactly one atomic bootstrap. Partial real deployment is
 * fatal because canonical Init validators depend on the hub-oracle NFT being
 * minted in the same transaction as every protocol root.
 */
export const program: Effect.Effect<
  string,
  unknown,
  Lucid | MidgardContracts | NodeConfig | IntentJournal
> = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const indexedFraudProofs = fraudProofsToIndexedValidators(
    contracts.fraudProofs,
  );
  const fraudProofCatalogueDeploymentInfo =
    yield* buildFraudProofCatalogueDeploymentInfo(indexedFraudProofs);
  yield* Effect.logInfo(
    `Fraud proof catalogue root prepared for initialization: ${fraudProofCatalogueDeploymentInfo.root}`,
  );

  // S5: the plan opens before the deployment-status read the init rests on.
  const plan = yield* openPlan;
  const status = yield* fetchProtocolDeploymentStatus(lucid, contracts);
  if (status.complete) {
    yield* ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    );
    const phasRegistration =
      yield* ensurePhasMembershipRewardAccountRegisteredProgram(lucid);
    yield* ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts);
    yield* Effect.logInfo(
      `PHAS membership reward-account registration status: status=${phasRegistration.status},scriptHash=${phasRegistration.scriptHash},rewardAddress=${phasRegistration.rewardAddress},txHash=${phasRegistration.txHash ?? "already-registered"}`,
    );
    return "already-initialized";
  }

  if (!status.empty) {
    return yield* Effect.fail(makePartialProtocolDeploymentError(status));
  }

  const referenceScripts =
    yield* ensureAtomicProtocolInitReferenceScriptsProgram(
      lucidService.referenceScriptsApi,
      contracts,
      lucid,
      lucidService.referenceScriptsAddress,
    );
  yield* ensureEventHistoryRewardAccountsRegisteredProgram(
    lucidService.referenceScriptsApi,
    contracts,
  );
  const initDeadline = resolveDefaultDeploymentDeadline(lucid);
  const txHash = yield* completeAndSubmit(
    lucid,
    yield* buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      nodeConfig,
      fraudProofCatalogueDeploymentInfo.root,
      initDeadline,
      referenceScripts,
      contracts.consensusProfile,
    ),
    "Failed to build atomic real protocol initialization transaction",
    {
      txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
      outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    },
    plan,
  );
  yield* Effect.logInfo(
    `Atomic real protocol initialization submitted: txHash=${txHash}`,
  );
  yield* waitForAtomicInitializationVisibility(lucid, contracts);
  yield* ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
    lucid,
    contracts,
  );
  const phasRegistration =
    yield* ensurePhasMembershipRewardAccountRegisteredProgram(lucid);
  yield* ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts);
  yield* Effect.logInfo(
    `PHAS membership reward-account registration status: status=${phasRegistration.status},scriptHash=${phasRegistration.scriptHash},rewardAddress=${phasRegistration.rewardAddress},txHash=${phasRegistration.txHash ?? "already-registered"}`,
  );
  return txHash;
});
