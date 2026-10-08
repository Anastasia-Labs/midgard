import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import {
  requireRetentionAlertThresholdMs,
  resolveL1ViewFatalMs,
} from "@al-ft/midgard-core";
import { parseDaLibp2pRuntimeManifest } from "@al-ft/midgard-core/da-transport";
import { parseL1Origin } from "@al-ft/midgard-core/l1-origin";
import * as SDK from "@al-ft/midgard-sdk";

import {
  type Env,
  type LoadedCommitteeConfig,
} from "./config.committee-config.js";
import {
  availabilityJournalPath,
  contractDeploymentManifestConfig,
  l1SubmitterPreflightConfig,
  localState,
  nonNegativeInt,
  optionalSignerConfig,
  parseNativeLedgerConfig,
  positiveInt,
} from "./config.l1-submitter-preflight-config.js";
import {
  daParamsConfig,
  libp2pDaTransportConfig,
  libp2pPrivateKeySourceConfig,
  parseJsonObject,
  parseL1SubmitterSignerIndexes,
  rejectPublicRetainedDaCoHosting,
  validateLibp2pCommitteeMatchesDaParams,
} from "./config.libp2p-da-transport-config.js";
import {
  booleanEnv,
  optionalKeySource,
  optionalNonEmpty,
  optionalSplitList,
  requireEnv,
} from "./config.operational-provider-identity.js";
import { cardanoL1SourceConfig } from "./config.parse-l1-source-config.js";
import {
  assertLibp2pDaRetentionDays,
  deploymentFingerprintConfig,
} from "./config.parse-libp2p-da-committee-peers.js";
import { committeePromiseAdoptionConfig } from "./config.promise-admission.js";
import { parseMidgardNodeDeploymentInfo } from "./l1/deployment.js";
import { normalizeHex } from "./utils/hex.js";

export const loadCommitteeConfig = async (
  env: Env = process.env,
): Promise<LoadedCommitteeConfig> => {
  const deploymentManifestPath = requireEnv(
    env,
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  );
  const contractDeploymentInfoPath = requireEnv(
    env,
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
  );
  const deploymentManifestRaw = await readFile(deploymentManifestPath, "utf8");
  const contractDeploymentInfoRaw = await readFile(
    contractDeploymentInfoPath,
    "utf8",
  );
  const deploymentManifest = parseJsonObject(
    deploymentManifestRaw,
    deploymentManifestPath,
  );
  const runtimeManifest = parseDaLibp2pRuntimeManifest(deploymentManifest);
  const contractDeploymentInfo = parseJsonObject(
    contractDeploymentInfoRaw,
    contractDeploymentInfoPath,
  );
  const deploymentManifestSha256 = createHash("sha256")
    .update(deploymentManifestRaw)
    .digest("hex");
  const contractDeploymentInfoSha256 = createHash("sha256")
    .update(contractDeploymentInfoRaw)
    .digest("hex");
  const {
    manifestId: contractDeploymentManifestId,
    consensusProfile,
    network: contractDeploymentNetwork,
    daRetentionDays: manifestDaRetentionDays,
    finalityDepth: manifestFinalityDepth,
    automaticRecoveryMaxDepth,
    availabilityChallenge: manifestAvailabilityChallenge,
  } = contractDeploymentManifestConfig(contractDeploymentInfo);
  const network = runtimeManifest.network;
  if (network !== contractDeploymentNetwork) {
    throw new Error(
      `DA runtime manifest network must exactly match contract deployment manifest network: runtime=${network}, contract=${contractDeploymentNetwork}`,
    );
  }
  if (env.MIDGARD_NETWORK !== undefined && env.MIDGARD_NETWORK !== network) {
    throw new Error(
      `MIDGARD_NETWORK must exactly match runtime manifest network ${network}`,
    );
  }
  const deploymentFingerprint = deploymentFingerprintConfig(
    runtimeManifest,
    contractDeploymentManifestId,
  );
  const libp2pDaTransport = libp2pDaTransportConfig({
    env,
    runtimeManifest,
    deploymentFingerprint,
  });
  assertLibp2pDaRetentionDays({
    runtimeRetentionDays: libp2pDaTransport.retentionDays,
    manifestRetentionDays: manifestDaRetentionDays,
  });
  const libp2pPrivateKeySource = libp2pPrivateKeySourceConfig(env);
  rejectPublicRetainedDaCoHosting(env);
  const midgardNodeDeployment = parseMidgardNodeDeploymentInfo(
    contractDeploymentInfo,
    network,
  );

  const daAttestationPolicyId = midgardNodeDeployment.daAttestation.policyId;
  const daAttestationAddress =
    midgardNodeDeployment.daAttestation.spendingScriptAddress;
  const daParamsGovernorPolicyId =
    midgardNodeDeployment.daParamsGovernor.policyId;
  const daParamsGovernorAddress =
    midgardNodeDeployment.daParamsGovernor.spendingScriptAddress;
  const stateQueuePolicyId = midgardNodeDeployment.stateQueue.policyId;
  const stateQueueAddress =
    midgardNodeDeployment.stateQueue.spendingScriptAddress;
  const hubOraclePolicyId = midgardNodeDeployment.hubOraclePolicyId;
  const correctionLockAddress = midgardNodeDeployment.correctionLockAddress;
  const fraudProofPolicyId = midgardNodeDeployment.fraudProof.policyId;
  const fraudProofAddress =
    midgardNodeDeployment.fraudProof.spendingScriptAddress;
  const l1SubmitterKeySource = optionalKeySource(
    env.L1_SUBMITTER_KEY_SOURCE,
    "L1_SUBMITTER_KEY_SOURCE",
  );
  // The deployment's L1 origin point until the redeploy carries it in the
  // manifest: `<slot>.<block hash>`, as `midgard-l1-follower find-origin`
  // prints it. Absent when unset.
  const l1OriginText = optionalNonEmpty(env.L1_ORIGIN);
  const l1Origin =
    l1OriginText === undefined
      ? undefined
      : parseL1Origin(l1OriginText, "L1_ORIGIN");
  const l1SubmitterId = optionalNonEmpty(env.DA_L1_SUBMITTER_ID);
  const l1SubmitterIds = optionalSplitList(env.DA_L1_SUBMITTER_IDS);
  if (
    l1SubmitterIds.length > 0 &&
    (l1SubmitterId === undefined || !l1SubmitterIds.includes(l1SubmitterId))
  ) {
    throw new Error(
      "DA_L1_SUBMITTER_ID must be present in DA_L1_SUBMITTER_IDS",
    );
  }
  const cardanoL1Source = cardanoL1SourceConfig({ env, network });
  const nativeLedger = parseNativeLedgerConfig(env);
  const daCommitteeMembers = libp2pDaTransport.peers.map((member) => ({
    index: member.signerIndex,
    vkey: member.daVkey,
    canSubmitL1: member.roles.includes("coordinator"),
  }));
  const l1SubmissionEnabled = booleanEnv(
    env.DA_L1_SUBMISSION_ENABLED,
    l1SubmitterKeySource !== undefined,
  );
  if (l1SubmissionEnabled && l1SubmitterKeySource === undefined) {
    throw new Error(
      "L1_SUBMITTER_KEY_SOURCE is required when DA_L1_SUBMISSION_ENABLED=true",
    );
  }
  const l1SubmitterPreflight = l1SubmitterPreflightConfig({
    env,
    l1SubmissionEnabled,
    l1SubmitterKeySource,
  });
  const maybeSigner = optionalSignerConfig(env);
  const daParams = daParamsConfig(env, runtimeManifest, daCommitteeMembers);
  validateLibp2pCommitteeMatchesDaParams(libp2pDaTransport, daParams);
  const l1SubmitterSignerIndexes = parseL1SubmitterSignerIndexes(
    env,
    daCommitteeMembers,
  );
  const configuredFinalityDepth = nonNegativeInt(
    requireEnv(env, "CARDANO_FINALITY_DEPTH"),
    "CARDANO_FINALITY_DEPTH",
  );
  if (configuredFinalityDepth !== manifestFinalityDepth) {
    throw new Error(
      `CARDANO_FINALITY_DEPTH must exactly equal the verified deployment manifest l1Finality.confirmationDepth: runtime=${configuredFinalityDepth.toString()}, manifest=${manifestFinalityDepth.toString()}`,
    );
  }
  const pollIntervalMs = positiveInt(
    env.DA_COMMITTEE_POLL_INTERVAL_MS ?? "15000",
    "DA_COMMITTEE_POLL_INTERVAL_MS",
  );
  // A committee that cannot read L1 for the attestation timeout has already
  // failed the duty that timeout exists for, so that is the default deadline.
  const l1ViewFatalMs = resolveL1ViewFatalMs({
    value: env.L1_VIEW_FATAL_MS,
    defaultMs: Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    pollIntervalMs,
    fieldName: "L1_VIEW_FATAL_MS",
  });
  const retentionAlertThresholdRaw = optionalNonEmpty(
    env.DA_RETENTION_ALERT_THRESHOLD_MS,
  );

  return {
    network,
    cardanoL1Source,
    deploymentManifestPath,
    contractDeploymentInfoPath,
    deploymentFingerprint,
    deploymentManifestSha256,
    contractDeploymentInfoSha256,
    deploymentManifestRaw,
    deploymentManifest,
    contractDeploymentInfo,
    availabilityChallenge: manifestAvailabilityChallenge,
    consensusProfile,
    midgardNodeDeployment,
    ...(nativeLedger === undefined ? {} : { nativeLedger }),
    ...(l1Origin === undefined ? {} : { l1Origin }),
    finalityDepth: configuredFinalityDepth,
    automaticRecoveryMaxDepth,
    daTransport: libp2pDaTransport,
    libp2pPrivateKeySource,
    ...maybeSigner,
    l1SubmitterKeySource,
    l1SubmissionEnabled,
    availabilityPromiseAdoption: committeePromiseAdoptionConfig(env),
    availabilityJournalPath: availabilityJournalPath(env),
    availabilitySubmitterKeySource: optionalNonEmpty(
      env.DA_AVAILABILITY_SUBMITTER_KEY_SOURCE,
    ),
    l1SubmitterPreflight,
    ...(l1SubmitterId === undefined ? {} : { l1SubmitterId }),
    l1SubmitterIds:
      l1SubmitterIds.length > 0
        ? l1SubmitterIds
        : l1SubmitterId === undefined
          ? []
          : [l1SubmitterId],
    l1LeaderFailoverMs: nonNegativeInt(
      env.DA_L1_LEADER_FAILOVER_MS ?? "15000",
      "DA_L1_LEADER_FAILOVER_MS",
    ),
    localState: localState(env),
    daParams,
    daCommitteeMembers,
    l1SubmitterSignerIndexes,
    daAttestationPolicyId: normalizeHex(daAttestationPolicyId, {
      fieldName: "DA attestation policy id",
      byteLength: 28,
    }),
    daAttestationAddress,
    daParamsGovernorPolicyId: normalizeHex(daParamsGovernorPolicyId, {
      fieldName: "DA params governor policy id",
      byteLength: 28,
    }),
    daParamsGovernorAddress,
    stateQueuePolicyId: normalizeHex(stateQueuePolicyId, {
      fieldName: "state queue policy id",
      byteLength: 28,
    }),
    stateQueueAddress,
    hubOraclePolicyId,
    correctionLockAddress,
    fraudProofPolicyId,
    fraudProofAddress,
    peerRequestTimeoutMs: positiveInt(
      env.DA_PEER_REQUEST_TIMEOUT_MS ?? "5000",
      "DA_PEER_REQUEST_TIMEOUT_MS",
    ),
    peerReplayWindowMs: positiveInt(
      env.DA_PEER_REPLAY_WINDOW_MS ?? "300000",
      "DA_PEER_REPLAY_WINDOW_MS",
    ),
    peerMaxBodyBytes: positiveInt(
      env.DA_PEER_MAX_BODY_BYTES ?? "1048576",
      "DA_PEER_MAX_BODY_BYTES",
    ),
    peerRetryInitialDelayMs: positiveInt(
      env.DA_PEER_RETRY_INITIAL_DELAY_MS ?? "1000",
      "DA_PEER_RETRY_INITIAL_DELAY_MS",
    ),
    peerRetryMaxDelayMs: positiveInt(
      env.DA_PEER_RETRY_MAX_DELAY_MS ?? "60000",
      "DA_PEER_RETRY_MAX_DELAY_MS",
    ),
    peerRetryMaxAttempts: positiveInt(
      env.DA_PEER_RETRY_MAX_ATTEMPTS ?? "12",
      "DA_PEER_RETRY_MAX_ATTEMPTS",
    ),
    peerRateLimitWindowMs: positiveInt(
      env.DA_PEER_RATE_LIMIT_WINDOW_MS ?? "60000",
      "DA_PEER_RATE_LIMIT_WINDOW_MS",
    ),
    peerRateLimitMaxRequests: positiveInt(
      env.DA_PEER_RATE_LIMIT_MAX_REQUESTS ?? "120",
      "DA_PEER_RATE_LIMIT_MAX_REQUESTS",
    ),
    apiHost: env.DA_COMMITTEE_API_HOST ?? "127.0.0.1",
    apiPort: positiveInt(
      env.DA_COMMITTEE_API_PORT ?? "8787",
      "DA_COMMITTEE_API_PORT",
    ),
    pollIntervalMs,
    l1ViewFatalMs,
    ...(retentionAlertThresholdRaw === undefined
      ? {}
      : {
          retentionAlertThresholdMs: requireRetentionAlertThresholdMs(
            nonNegativeInt(
              retentionAlertThresholdRaw,
              "DA_RETENTION_ALERT_THRESHOLD_MS",
            ),
            "DA_RETENTION_ALERT_THRESHOLD_MS",
          ),
        }),
  };
};
