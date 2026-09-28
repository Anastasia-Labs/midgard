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

/**
 * The closed set of profiles whose fraud-provability rests on non-interactive
 * proofs alone: their maturity is shorter than one interactive dispute, so the
 * on-chain `can_open_before_maturity` guard refuses every interactive opening.
 * Twin: `nonInteractiveTesting` in demo/scripts/deployment-profiles.mjs, which
 * holds the same two names. Keep both lists closed and identical; never derive
 * membership from a name suffix, the network or the economics profile.
 */
const NON_INTERACTIVE_TESTING_PROFILE_NAMES: ReadonlySet<string> = new Set([
  "preprod-testing",
  "local-devnet-testing",
]);

export const isNonInteractiveTestingProfile = (name: string): boolean =>
  NON_INTERACTIVE_TESTING_PROFILE_NAMES.has(name);

export const requireSelectedDeploymentProfile = (name: string | undefined) => {
  if (name !== SELECTED_DEPLOYMENT_PROFILE.name) {
    throw new Error(
      `MIDGARD_DEPLOYMENT_PROFILE must match the compiled profile ${SELECTED_DEPLOYMENT_PROFILE.name}; rebuild to select another profile`,
    );
  }
  return SELECTED_DEPLOYMENT_PROFILE;
};

/**
 * A profile's pooled DA committee bond amounts, keyed as a deployment
 * manifest's `availabilityChallenge` section and the availability
 * `ParametersV1` builders key them. Off-chain consumers must carry exactly
 * these values; the generator already validated their relations.
 */
export type DaBondManifestAmounts = Readonly<{
  daBondLovelace: number;
  daSlashPenaltyLovelace: number;
  daBondMinTopUpLovelace: number;
  daBondPoolFloorLovelace: number;
  challengeRecordLovelace: number;
}>;

export const daBondManifestAmounts = (
  profile: DeploymentProfile = SELECTED_DEPLOYMENT_PROFILE,
): DaBondManifestAmounts =>
  Object.freeze({
    daBondLovelace: profile.da_bond.da_bond_lovelace,
    daSlashPenaltyLovelace: profile.da_bond.da_slash_penalty_lovelace,
    daBondMinTopUpLovelace: profile.da_bond.da_bond_min_top_up_lovelace,
    daBondPoolFloorLovelace: profile.da_bond.da_bond_pool_floor_lovelace,
    challengeRecordLovelace: profile.da_bond.challenge_record_lovelace,
  });

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
