import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { parseDaLibp2pRuntimeManifest } from "@al-ft/midgard-core/da-transport";

import {
  type Env,
  type LoadedCommitteeConfig,
} from "./config.committee-config.js";
import { contractDeploymentManifestConfig } from "./config.l1-submitter-preflight-config.js";
import {
  daParamsConfig,
  libp2pDaTransportConfig,
  parseJsonObject,
  parseL1SubmitterSignerIndexes,
  validateLibp2pCommitteeMatchesDaParams,
} from "./config.libp2p-da-transport-config.js";
import { parseCommitteeEnvironment } from "./config.parse-committee-environment.js";
import { cardanoL1SourceConfig } from "./config.parse-l1-source-config.js";
import {
  assertLibp2pDaRetentionDays,
  deploymentFingerprintConfig,
} from "./config.parse-libp2p-da-committee-peers.js";
import { parseMidgardNodeDeploymentInfo } from "./l1/deployment.js";
import { normalizeHex } from "./utils/hex.js";

export const loadCommitteeConfig = async (
  env: Env = process.env,
): Promise<LoadedCommitteeConfig> => {
  // Everything the environment alone fixes, refused before either file is read.
  const settings = parseCommitteeEnvironment(env);
  const { deploymentManifestPath, contractDeploymentInfoPath } = settings;
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
    l1Origin: recordedL1Origin,
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
  const cardanoL1Source = cardanoL1SourceConfig({ env, network });
  const daCommitteeMembers = libp2pDaTransport.peers.map((member) => ({
    index: member.signerIndex,
    vkey: member.daVkey,
    canSubmitL1: member.roles.includes("coordinator"),
  }));
  const daParams = daParamsConfig(env, runtimeManifest, daCommitteeMembers);
  validateLibp2pCommitteeMatchesDaParams(libp2pDaTransport, daParams);
  const l1SubmitterSignerIndexes = parseL1SubmitterSignerIndexes(
    env,
    daCommitteeMembers,
  );
  if (settings.finalityDepth !== manifestFinalityDepth) {
    throw new Error(
      `CARDANO_FINALITY_DEPTH must exactly equal the verified deployment manifest l1Finality.confirmationDepth: runtime=${settings.finalityDepth.toString()}, manifest=${manifestFinalityDepth.toString()}`,
    );
  }

  return {
    ...settings,
    network,
    cardanoL1Source,
    deploymentFingerprint,
    deploymentManifestSha256,
    contractDeploymentInfoSha256,
    deploymentManifestRaw,
    deploymentManifest,
    contractDeploymentInfo,
    availabilityChallenge: manifestAvailabilityChallenge,
    ...(recordedL1Origin === undefined ? {} : { recordedL1Origin }),
    consensusProfile,
    midgardNodeDeployment,
    automaticRecoveryMaxDepth,
    daTransport: libp2pDaTransport,
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
  };
};
