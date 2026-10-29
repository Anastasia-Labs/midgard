import { canonicalJson } from "./canonical-json.js";
import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestFraudProofCatalogueCategory,
} from "./deployment-manifest-identity/catalogue-roles.js";
import {
  DEPLOYMENT_PROFILES,
  PRODUCTION_DEPLOYMENT_PROFILE_NAMES,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "./generated-deployment-profiles.js";

export * from "./generated-deployment-profiles.js";

export type DeploymentProfileName = keyof typeof DEPLOYMENT_PROFILES;
export type DeploymentProfile =
  (typeof DEPLOYMENT_PROFILES)[DeploymentProfileName];

/** A profile deployed as a public release: provenance, redaction, public HTTP. */
export type ProductionDeploymentProfileName =
  (typeof PRODUCTION_DEPLOYMENT_PROFILE_NAMES)[number];

export const isProductionDeploymentProfileName = (
  name: string,
): name is ProductionDeploymentProfileName =>
  (PRODUCTION_DEPLOYMENT_PROFILE_NAMES as readonly string[]).includes(name);

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

type DisputeTimingProfile = Readonly<{
  timing: Readonly<{
    block_maturity_ms: number;
    max_validity_range_ms: number;
    da_challenge_window_ms: number;
    da_full_response_window_ms: number;
    dispute_side_clock_ms: number;
    dispute_max_validity_range_ms: number;
    dispute_resolution_allowance_ms: number;
  }>;
  limits: Readonly<{ max_bisection_rounds: number }>;
  excluded_fraud_proof_categories: readonly string[];
}>;

/**
 * D_case of the Plutus execution dispute (validationTraceDispute, 00000006),
 * as onchain/aiken/lib/midgard/validation-dispute-v1.ak derives it: a thread
 * charges at most 2 * max_bisection_rounds + 3 moves
 * (`max_thread_charged_moves`), and D_case = 2 C + 2 * moves * L_d + R
 * (`max_dispute_duration_milliseconds`). Twin of
 * `validationDisputeCaseDurationMs` in demo/scripts/deployment-profile-budgets.mjs.
 */
export const validationDisputeCaseDurationMs = ({
  timing,
  limits,
}: DisputeTimingProfile): number =>
  2 * timing.dispute_side_clock_ms +
  2 *
    (2 * limits.max_bisection_rounds + 3) *
    timing.dispute_max_validity_range_ms +
  timing.dispute_resolution_allowance_ms;

/**
 * The least block maturity that leaves a case its post-game allowance
 * P = M - M / 2 - D_case > 0 (validation-case-v1.ak): 2 D_case + 1.
 */
export const minValidationDisputeMaturityMs = (
  profile: DisputeTimingProfile,
): number => 2 * validationDisputeCaseDurationMs(profile) + 1;

/** Whether the profile's catalogue carries the execution dispute. */
export const validationTraceDisputeInstalled = (
  profile: Pick<DisputeTimingProfile, "excluded_fraud_proof_categories">,
): boolean =>
  !profile.excluded_fraud_proof_categories.includes("validationTraceDispute");

/**
 * Fail-closed load-time check of a profile that installs the execution
 * dispute (twin of the profile generator's rule). A case opens by
 * block_end + M / 2 (`case_can_open`) and may need the latest DA answer
 * first (challenge window + maximum validity range + full response window,
 * strictly before that deadline), and maturity must be at least
 * 2 D_case + 1. A profile that excludes the dispute is not checked here.
 */
export const assertValidationDisputeFitsProfile = (
  name: string,
  profile: DisputeTimingProfile,
): void => {
  if (!validationTraceDisputeInstalled(profile)) return;
  const { timing } = profile;
  const latestDaAnswerMs =
    timing.da_challenge_window_ms +
    timing.max_validity_range_ms +
    timing.da_full_response_window_ms;
  if (latestDaAnswerMs >= Math.floor(timing.block_maturity_ms / 2)) {
    throw new Error(
      `Deployment profile ${name} installs validationTraceDispute, but its latest DA answer (${String(latestDaAnswerMs)} ms) does not land before the case open deadline (half of block_maturity_ms=${String(timing.block_maturity_ms)})`,
    );
  }
  const minimumMs = minValidationDisputeMaturityMs(profile);
  if (timing.block_maturity_ms < minimumMs) {
    throw new Error(
      `Deployment profile ${name} installs validationTraceDispute, but block_maturity_ms=${String(timing.block_maturity_ms)} is below 2 D_case + 1 = ${String(minimumMs)}`,
    );
  }
};

for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
  assertValidationDisputeFitsProfile(profile.name, profile);
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

/**
 * A stage-0 allow-list, keyed as the profile's `allow_lists`. Only operators
 * are listed; fault provers and DA challengers are permissionless.
 */
export type AllowListRole = "operators";

/** Admitted payment key hashes, or `null` to admit every key. */
export type AllowList = readonly string[] | null;

const ALLOW_LIST_ACTIONS: Readonly<Record<AllowListRole, string>> = {
  operators: "register as an operator",
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
