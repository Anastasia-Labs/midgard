import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  type DeploymentManifestEconomicsProfile,
} from "@al-ft/midgard-core/deployment-manifest-identity";

/**
 * The node's typed environment edge for the readers that are not Effect
 * `Config` descriptors.
 *
 * `src/services/config.ts` is the edge for everything that runs under a
 * `NodeConfig` layer, and stays the primary one. What it cannot cover are the
 * plain functions — commands, workers, fibers, blueprint loaders — that read a
 * handful of variables directly on the way past. Those reads had drifted into
 * several near-copies of the same parse: two copies of the eleven-variable
 * DA-availability block (with the same validation under two different error
 * strings), three of the deployment-manifest path, two of the blueprint path
 * override, two of the bond-owner credential, and a third copy of the
 * economics-profile check that `config.ts` already performs.
 *
 * This module holds one definition of each. Every function here reads
 * `process.env` and validates; nothing here caches, so a test that mutates the
 * environment sees the change on the next call. Readers that already take an
 * injected `env` parameter (`da/hardening-config.ts`, `e2e/env.ts`,
 * `commands/command-utils.ts`, and the rest) are deliberately left alone —
 * injection is the better shape and they already have it.
 */

/** `undefined` for unset *and* for present-but-blank, which callers all treat alike. */
export const trimmedEnvironmentValue = (name: string): string | undefined => {
  const raw = process.env[name]?.trim();
  return raw === undefined || raw.length === 0 ? undefined : raw;
};

/**
 * A positive decimal integer that must be spelled explicitly — no default, no
 * leading zero, no sign, and inside the safe-integer range.
 *
 * `describeMissing` exists because the two DA-availability call sites report a
 * missing value differently: the script-derivation path names the reason it
 * needs the value, the manifest path states the required shape. Both messages
 * are preserved rather than merged, so nothing that greps logs or asserts on
 * them changes.
 */
export const requiredPositiveIntegerEnvironmentValue = (
  name: string,
  describeMissing: (name: string) => string,
): number => {
  const raw = process.env[name]?.trim();
  if (raw === undefined || !/^[1-9][0-9]*$/u.test(raw)) {
    throw new Error(describeMissing(name));
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error(`${name} must fit a JavaScript safe integer`);
  }
  return parsed;
};

/** Exactly 28 lowercase hex bytes — the on-chain credential encoding. */
export const daAvailabilityBondOwnerCredential = (): string => {
  const value =
    process.env.MIDGARD_DA_AVAILABILITY_BOND_OWNER_CREDENTIAL?.trim();
  if (value === undefined || !/^[0-9a-f]{56}$/u.test(value)) {
    throw new Error(
      "MIDGARD_DA_AVAILABILITY_BOND_OWNER_CREDENTIAL must be exactly 28 lowercase hex bytes",
    );
  }
  return value;
};

/**
 * The fixed response classes both DA-availability call sites pin. They are not
 * configurable: they are the deployment's published windows, repeated in the
 * two call sites before this module existed.
 */
const DA_AVAILABILITY_RESPONSE_CLASSES = {
  smallPayloadMaxBytes: 65_536,
  smallResponseWindowMs: 3_600_000,
  fullPayloadMaxBytes: 67_108_864,
  fullResponseWindowMs: 172_800_000,
} as const;

/**
 * The eleven `MIDGARD_DA_AVAILABILITY_*` variables, parsed into the shape
 * `parseDeploymentManifestAvailabilityChallenge` accepts.
 *
 * Returned unparsed so each caller keeps its own downstream conversion — one
 * builds `SDK.DaAvailabilityParameters`, the other a manifest section — while
 * the environment reading and validation happen once.
 */
export const daAvailabilityChallengeEnvironmentInput = (
  describeMissingInteger: (name: string) => string,
) => {
  const integer = (name: string): number =>
    requiredPositiveIntegerEnvironmentValue(name, describeMissingInteger);
  return {
    responseClasses: DA_AVAILABILITY_RESPONSE_CLASSES,
    responseGeometry: {
      chunkByteLength: integer("MIDGARD_DA_AVAILABILITY_CHUNK_BYTE_LENGTH"),
      trancheByteLength: integer("MIDGARD_DA_AVAILABILITY_TRANCHE_BYTE_LENGTH"),
      maxTrancheCount: integer("MIDGARD_DA_AVAILABILITY_MAX_TRANCHE_COUNT"),
    },
    daBondLovelace: integer("MIDGARD_DA_AVAILABILITY_BOND_LOVELACE"),
    challengerBondLovelace: integer(
      "MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE",
    ),
    maxOpenFeeLovelace: integer(
      "MIDGARD_DA_AVAILABILITY_MAX_OPEN_FEE_LOVELACE",
    ),
    maxPublicationFeeLovelace: integer(
      "MIDGARD_DA_AVAILABILITY_MAX_PUBLICATION_FEE_LOVELACE",
    ),
    maxSettlementFeeLovelace: integer(
      "MIDGARD_DA_AVAILABILITY_MAX_SETTLEMENT_FEE_LOVELACE",
    ),
    maxCloseFeeLovelace: integer(
      "MIDGARD_DA_AVAILABILITY_MAX_CLOSE_FEE_LOVELACE",
    ),
    maxTimeoutFeeLovelace: integer(
      "MIDGARD_DA_AVAILABILITY_MAX_TIMEOUT_FEE_LOVELACE",
    ),
    bondOwnerCredential: daAvailabilityBondOwnerCredential(),
  };
};

/**
 * The profile check `config.ts` runs inside its `Config` descriptor and
 * `commands/contract-deployment-info.ts` runs on a bare read. Same two accepted
 * spellings, same message, one definition.
 */
export const parseDeploymentEconomicsProfile = (
  value: string | undefined,
): DeploymentManifestEconomicsProfile => {
  if (
    value !== "public-preprod-launch-v1" &&
    value !== "bounded-acceptance-v1"
  ) {
    throw new Error(
      "MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE must explicitly equal public-preprod-launch-v1 or bounded-acceptance-v1",
    );
  }
  return value;
};

export const deploymentEconomicsProfileFromEnvironment =
  (): DeploymentManifestEconomicsProfile =>
    parseDeploymentEconomicsProfile(
      process.env.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE?.trim(),
    );

export const deploymentEconomicsFromEnvironment = () =>
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[
    deploymentEconomicsProfileFromEnvironment()
  ];

/**
 * Override for the deployment manifest written by `contract-deployment-info`.
 * Three readers, three different fallbacks — the default output path, a hard
 * failure, and "no manifest configured" — so this returns the override only.
 */
export const contractDeploymentInfoPathOverride = (): string | undefined =>
  trimmedEnvironmentValue("MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH");

/**
 * Override for the Aiken blueprint. Both readers fall back to their own
 * candidate list, which legitimately differs by module depth, so only the
 * override is shared.
 */
export const realBlueprintPathOverride = (): string | undefined =>
  trimmedEnvironmentValue("MIDGARD_REAL_BLUEPRINT_PATH");

export const attestationTimeoutJournalPathOverride = (): string | undefined =>
  trimmedEnvironmentValue("MIDGARD_ATTESTATION_TIMEOUT_JOURNAL_PATH");
