import { paymentCredentialOf } from "@lucid-evolution/lucid";

import { type CommitteeConfig, l1SourceAuthorityDigest } from "../config.js";
import { ownWalletAddress } from "../l1/follower/committee-follower-config.js";
import type { CommitteeRetirementBinding } from "../store/retirement-model.js";

/** The retirement binding of this member's promise runtime as `actorId`. */
export const committeeRetirementBinding = (
  config: Pick<
    CommitteeConfig,
    | "deploymentFingerprint"
    | "deploymentManifestSha256"
    | "contractDeploymentInfo"
    | "daParams"
    | "daTransport"
    | "automaticRecoveryMaxDepth"
    | "network"
    | "nativeLedger"
  >,
  actorId: string,
): CommitteeRetirementBinding => ({
  deploymentFingerprint: config.deploymentFingerprint,
  manifestSha256: config.deploymentManifestSha256,
  contractManifestId: String(config.contractDeploymentInfo.manifestId),
  committeeSignersHash: config.daParams.committeeSignersHash,
  actorId,
  sourceAuthoritySha256: l1SourceAuthorityDigest(config),
  peerIds: config.daTransport.peers.map((peer) => peer.peerId),
  retentionDays: config.daTransport.retentionDays,
  recoveryDepth: config.automaticRecoveryMaxDepth,
  maximumRecords: 512,
  maximumEncodedBytes: 8388608,
});

/**
 * The retirement binding the store open re-binds a stored floor to: only
 * for a member whose promise profile is adopted, the actor being its
 * availability submitter's payment key. Undefined otherwise; the open then
 * leaves any floor as it is.
 */
export const configuredRetirementBinding = async (
  config: CommitteeConfig,
): Promise<CommitteeRetirementBinding | undefined> => {
  if (
    config.availabilityPromiseAdoption === undefined ||
    config.availabilitySubmitterKeySource === undefined
  )
    return undefined;
  const actor = paymentCredentialOf(
    await ownWalletAddress(
      config.availabilitySubmitterKeySource,
      config.network,
    ),
  );
  return actor.type === "Key"
    ? committeeRetirementBinding(config, actor.hash)
    : undefined;
};
