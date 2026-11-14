import {
  type FraudProofReleaseEconomicsAuthority,
  type FraudProofReleaseFinalityAuthority,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";

import { fail } from "./deployment-identity.catalogue-category-to-contract.js";
import {
  appliedScriptHashesByDeploymentIdentity,
  authenticatedWatcherAvailabilityChallengeAuthorities,
  authenticatedWatcherProtocolParameterAuthorities,
  availabilityChallengeAuthorityByDeploymentIdentity,
  protocolParameterAuthorityByDeploymentIdentity,
  releaseEconomicsAuthorityByDeploymentIdentity,
  releaseFinalityAuthorityByDeploymentIdentity,
  releaseFinalityByDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  type WatcherDeploymentAvailabilityChallengeAuthority,
  type WatcherDeploymentProtocolParameterAuthority,
} from "./deployment-identity.parse-trust-roots.js";
import { assertVerifiedWatcherDeploymentIdentity } from "./deployment-identity.verify-watcher-user-event-script-binding.js";

/** Exact applied scripts from the already verified deployment manifest. */
export const watcherDeploymentAppliedScriptHashes = (
  identity: VerifiedWatcherDeploymentIdentity,
): Readonly<Record<string, string>> => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    appliedScriptHashesByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.appliedScriptHashes")
  );
};

export const assertWatcherDeploymentProtocolParameterAuthority = (
  authority: WatcherDeploymentProtocolParameterAuthority,
): void => {
  if (!authenticatedWatcherProtocolParameterAuthorities.has(authority)) {
    fail("invalid_field", "$.protocolParameterAuthority");
  }
};

export const watcherDeploymentProtocolParameterAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): WatcherDeploymentProtocolParameterAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    protocolParameterAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.protocolParameters")
  );
};

export const assertWatcherDeploymentAvailabilityChallengeAuthority = (
  authority: WatcherDeploymentAvailabilityChallengeAuthority,
): void => {
  if (!authenticatedWatcherAvailabilityChallengeAuthorities.has(authority)) {
    fail("invalid_field", "$.availabilityChallengeAuthority");
  }
};

/**
 * Returns the exact Q58 geometry, bonds, owner, response classes and every
 * lifecycle fee ceiling admitted by the signed deployment manifest. A plain
 * object with the same fields is not production authority.
 */
export const watcherDeploymentAvailabilityChallengeAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): WatcherDeploymentAvailabilityChallengeAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    availabilityChallengeAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.availabilityChallenge")
  );
};

/**
 * Returns the release-finality authority minted by the signed deployment
 * verifier. The authority method authenticates its receiver, so spreading or
 * structurally copying the object cannot preserve workflow authority.
 */
export const watcherDeploymentReleaseFinalityAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): FraudProofReleaseFinalityAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    releaseFinalityAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.releaseFinality")
  );
};

/**
 * Returns the release-finality policy the signed deployment verifier admitted,
 * for synchronous consumers that bind L1 sources and runtime configuration to
 * the deployment's confirmation depth.
 */
export const watcherDeploymentReleaseFinalityPolicy = (
  identity: VerifiedWatcherDeploymentIdentity,
): VerifiedFraudProofReleaseFinalityPolicy => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    releaseFinalityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.releaseFinality")
  );
};

/**
 * Returns the exact release-economics authority minted by signed deployment
 * verification. Structural copies cannot select a different collateral floor
 * or other F04 amount after launch.
 */
export const watcherDeploymentReleaseEconomicsAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): FraudProofReleaseEconomicsAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    releaseEconomicsAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.releaseEconomics")
  );
};
