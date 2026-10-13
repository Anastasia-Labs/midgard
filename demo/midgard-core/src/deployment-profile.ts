import { canonicalJson } from "./canonical-json.js";
import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestFraudProofCatalogueCategory,
} from "./deployment-manifest-identity/catalogue-roles.js";
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

const catalogueCategories: ReadonlySet<string> = new Set(
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
);
for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
  for (const category of profile.excluded_fraud_proof_categories) {
    if (!catalogueCategories.has(category)) {
      throw new Error(
        `Deployment profile ${profile.name} excludes ${category}, which is not a fraud-proof catalogue category`,
      );
    }
  }
}

/**
 * The fraud-proof catalogue a deployment carries: every catalogue category
 * the profile does not exclude, in the catalogue's presentation order. The
 * atomic init builds the catalogue MPF from exactly this set, so an excluded
 * category has no membership proof and Computation-Thread Init refuses it.
 */
export const deploymentFraudProofCatalogueCategories = (
  profile: DeploymentProfile = SELECTED_DEPLOYMENT_PROFILE,
): readonly DeploymentManifestFraudProofCatalogueCategory[] => {
  const excluded: ReadonlySet<string> = new Set(
    profile.excluded_fraud_proof_categories,
  );
  return Object.freeze(
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
      (category) => !excluded.has(category),
    ),
  );
};

/** A stage-0 allow-list, keyed as the profile's `allow_lists`. */
export type AllowListRole = "operators" | "fraud_provers" | "da_challengers";

/** Admitted payment key hashes, or `null` to admit every key. */
export type AllowList = readonly string[] | null;

const ALLOW_LIST_ACTIONS: Readonly<Record<AllowListRole, string>> = {
  operators: "register as an operator",
  fraud_provers: "initialise a fraud-proof computation thread",
  da_challengers: "open a DA availability challenge",
};

/** The profile's list for `role`; the validators compile the same list. */
export const deploymentAllowList = (
  role: AllowListRole,
  profile: DeploymentProfile = SELECTED_DEPLOYMENT_PROFILE,
): AllowList => profile.allow_lists[role];

/** Whether `list` admits `keyHash`, as the on-chain `allow_list.admits`. */
export const allowListAdmits = (list: AllowList, keyHash: string): boolean =>
  list === null || list.includes(keyHash.toLowerCase());

/** A key the deployment's allow-list leaves out, refused before any build. */
export class AllowListRefusal extends Error {
  override readonly name = "AllowListRefusal";
  constructor(
    readonly role: AllowListRole,
    readonly keyHash: string,
  ) {
    super(
      `key ${keyHash} is not on the deployment's ${role} allow-list, so it cannot ${ALLOW_LIST_ACTIONS[role]}`,
    );
  }
}

/** Throws `AllowListRefusal` unless `list` (the profile's by default) admits `keyHash`. */
export const requireAllowListed = (
  role: AllowListRole,
  keyHash: string,
  list: AllowList = deploymentAllowList(role),
): void => {
  if (!allowListAdmits(list, keyHash))
    throw new AllowListRefusal(role, keyHash);
};
