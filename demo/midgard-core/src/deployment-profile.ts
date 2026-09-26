import { canonicalJson } from "./canonical-json.js";
import {
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "./generated-deployment-profiles.js";

export * from "./generated-deployment-profiles.js";

export type DeploymentProfileName = keyof typeof DEPLOYMENT_PROFILES;
export type DeploymentProfile =
  (typeof DEPLOYMENT_PROFILES)[DeploymentProfileName];

export const requireSelectedDeploymentProfile = (name: string | undefined) => {
  if (name !== SELECTED_DEPLOYMENT_PROFILE.name) {
    throw new Error(
      `MIDGARD_DEPLOYMENT_PROFILE must match the compiled profile ${SELECTED_DEPLOYMENT_PROFILE.name}; rebuild to select another profile`,
    );
  }
  return SELECTED_DEPLOYMENT_PROFILE;
};

export const verifyDeploymentProfileBinding = (
  profile: unknown,
  digest: unknown,
  network: unknown,
): void => {
  if (
    digest !== SELECTED_DEPLOYMENT_PROFILE_DIGEST ||
    canonicalJson(profile, "Deployment profile") !==
      canonicalJson(SELECTED_DEPLOYMENT_PROFILE, "Deployment profile") ||
    network !== SELECTED_DEPLOYMENT_PROFILE.network
  ) {
    throw new Error(
      "Deployment profile, digest, and network must match the compiled profile",
    );
  }
};
