import * as SDK from "@al-ft/midgard-sdk";

import type { WatcherConfig } from "../config.js";
import { type DaAttestationChainReader } from "../l1/da-attestation-reader.js";
import { daAttestationValidatorsFromDeployment } from "../l1/deployment.js";
import { lucidFromProviderUrl } from "../l1/lucid.js";
import { fetchDaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import {
  assertL1SubmitterWalletPreflight,
  type L1SubmitterPreflightOptions,
  type L1SubmitterPreflightResult,
  preflightL1SubmitterWallet,
  selectL1SubmitterWallet,
} from "../l1/submitter.js";
import type { WatcherStore } from "../store.js";
import { normalizeHex } from "../utils/hex.js";
import { LucidDaAttestationSubmitter } from "./lucid-submitter.js";
import { OnChainLifecycleCoordinator } from "./on-chain.js";

type OnChainCoordinatorFactoryDeps = {
  readonly lucidFromProviderUrl: typeof lucidFromProviderUrl;
  readonly selectL1SubmitterWallet: typeof selectL1SubmitterWallet;
  readonly assertL1SubmitterWalletPreflight: typeof assertL1SubmitterWalletPreflight;
  readonly preflightL1SubmitterWallet: typeof preflightL1SubmitterWallet;
  readonly fetchDaAttestationReferenceScripts: typeof fetchDaAttestationReferenceScripts;
};

const defaultDeps: OnChainCoordinatorFactoryDeps = {
  lucidFromProviderUrl,
  selectL1SubmitterWallet,
  assertL1SubmitterWalletPreflight,
  preflightL1SubmitterWallet,
  fetchDaAttestationReferenceScripts,
};

export const onChainCoordinatorFromConfig = async (
  config: WatcherConfig,
  chainReader?: DaAttestationChainReader,
  store?: Pick<
    WatcherStore,
    "saveDaAttestationCandidate" | "saveL1Submission" | "listDaSignatures"
  >,
  deps: OnChainCoordinatorFactoryDeps = defaultDeps,
): Promise<OnChainLifecycleCoordinator> => {
  if (config.l1SubmitterKeySource === undefined) {
    throw new Error("L1_SUBMITTER_KEY_SOURCE is required for L1 submission");
  }
  if (chainReader === undefined) {
    throw new Error(
      "L1 submission requires the canonical configured DA chain reader",
    );
  }
  const { lucid } = await deps.lucidFromProviderUrl(
    config.cardanoProviderUrls[0]!,
    config.network,
  );
  await deps.selectL1SubmitterWallet(lucid, config.l1SubmitterKeySource);
  if (config.l1SubmitterPreflight.enabled) {
    await deps.assertL1SubmitterWalletPreflight(
      lucid,
      l1SubmitterPreflightOptionsFromConfig(config),
    );
  }
  const referenceScripts = await deps.fetchDaAttestationReferenceScripts(
    lucid,
    config.midgardNodeDeployment,
  );
  const contracts = daAttestationValidatorsFromDeployment(
    config.midgardNodeDeployment,
  );
  const contractDaAttestationPolicyId = normalizeHex(
    contracts.daAttestation.policyId,
    {
      fieldName: "contract deployment DA attestation policy id",
      byteLength: 28,
    },
  );
  if (contractDaAttestationPolicyId !== config.daAttestationPolicyId) {
    throw new Error(
      "configured DA attestation policy id does not match Midgard node deployment-info",
    );
  }
  const submitter = new LucidDaAttestationSubmitter({
    lucid,
    contracts,
    referenceScripts,
    availabilityParameters: SDK.daAvailabilityParametersV1({
      responseGeometry: SDK.availabilityResponseGeometryV1(
        config.availabilityChallenge.responseGeometry,
      ),
      daBondLovelace: BigInt(config.availabilityChallenge.daBondLovelace),
      challengerBondLovelace: BigInt(
        config.availabilityChallenge.challengerBondLovelace,
      ),
      maxOpenFeeLovelace: BigInt(
        config.availabilityChallenge.maxOpenFeeLovelace,
      ),
      maxPublicationFeeLovelace: BigInt(
        config.availabilityChallenge.maxPublicationFeeLovelace,
      ),
      maxSettlementFeeLovelace: BigInt(
        config.availabilityChallenge.maxSettlementFeeLovelace,
      ),
      maxCloseFeeLovelace: BigInt(
        config.availabilityChallenge.maxCloseFeeLovelace,
      ),
      maxTimeoutFeeLovelace: BigInt(
        config.availabilityChallenge.maxTimeoutFeeLovelace,
      ),
    }),
  });
  return new OnChainLifecycleCoordinator({
    chainReader,
    submitter,
    threshold: config.daParams.threshold,
    recordCandidate:
      store === undefined
        ? undefined
        : (record) => store.saveDaAttestationCandidate(record),
    recordSubmission:
      store === undefined
        ? undefined
        : (record) => store.saveL1Submission(record),
    peerSignaturesFor:
      store === undefined
        ? undefined
        : (headerHash) => store.listDaSignatures(headerHash),
    submitterSignerIndexes: config.l1SubmitterSignerIndexes,
    l1SubmitterId: config.l1SubmitterId,
    l1SubmitterIds: config.l1SubmitterIds,
    l1LeaderFailoverMs: config.l1LeaderFailoverMs,
  });
};

export const l1SubmitterWalletPreflightFromConfig = async (
  config: WatcherConfig,
  deps: Pick<
    OnChainCoordinatorFactoryDeps,
    | "lucidFromProviderUrl"
    | "selectL1SubmitterWallet"
    | "preflightL1SubmitterWallet"
  > = defaultDeps,
): Promise<L1SubmitterPreflightResult> => {
  if (!config.l1SubmissionEnabled) {
    throw new Error(
      "L1 wallet preflight requires DA_L1_SUBMISSION_ENABLED=true",
    );
  }
  if (config.l1SubmitterKeySource === undefined) {
    throw new Error(
      "L1_SUBMITTER_KEY_SOURCE is required for L1 wallet preflight",
    );
  }
  const { lucid } = await deps.lucidFromProviderUrl(
    config.cardanoProviderUrls[0]!,
    config.network,
  );
  await deps.selectL1SubmitterWallet(lucid, config.l1SubmitterKeySource);
  return deps.preflightL1SubmitterWallet(
    lucid,
    l1SubmitterPreflightOptionsFromConfig(config),
  );
};

const l1SubmitterPreflightOptionsFromConfig = (
  config: WatcherConfig,
): L1SubmitterPreflightOptions => {
  if (config.l1SubmitterKeySource === undefined) {
    throw new Error(
      "L1_SUBMITTER_KEY_SOURCE is required for L1 wallet preflight",
    );
  }
  return {
    submitterKeySource: config.l1SubmitterKeySource,
    minPlainAdaLovelace: config.l1SubmitterPreflight.minPlainAdaLovelace,
    minCollateralLovelace: config.l1SubmitterPreflight.minCollateralLovelace,
    minSpendableUtxoCount: config.l1SubmitterPreflight.minSpendableUtxoCount,
    ...(config.l1SubmitterPreflight.autoFundKeySource === undefined
      ? {}
      : { autoFundKeySource: config.l1SubmitterPreflight.autoFundKeySource }),
    autoFundBufferLovelace: config.l1SubmitterPreflight.autoFundBufferLovelace,
    retryCount: config.l1SubmitterPreflight.retryCount,
    retryDelayMs: config.l1SubmitterPreflight.retryDelayMs,
  };
};
