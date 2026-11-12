import type { DeploymentManifestEconomicsProfile } from "@al-ft/midgard-core/deployment-manifest-identity";
import { requireSelectedDeploymentProfile } from "@al-ft/midgard-core/deployment-profile";
import { REFERENCE_SCRIPT_AUTH_TIMELOCK_MS } from "@al-ft/midgard-sdk";
import { Config, Option } from "effect";

import {
  isStrictlyAscending,
  splitPackedHex,
  VERIFICATION_KEY_HASH_HEX_LENGTH,
  VERIFICATION_KEY_HEX_LENGTH,
} from "../da/local-signers.js";
import {
  assertDistinctOperationalWallets,
  requiredSeedPhrase,
  seedPhraseAddress,
} from "./config.operational-wallets.js";
import { positiveSafeIntegerConfig, stringConfig } from "./config.parse.js";

/**
 * Validates the *encoding* of one of the DA key sets (`DA_COMMITTEE_HEX`,
 * `DA_OWNERS_HEX`) at config load, returning the normalized packed hex.
 *
 * Encoding only — deliberately not policy. `DA_COMMITTEE_HEX`, `DA_OWNERS_HEX`
 * and `DA_THRESHOLD` are read by exactly one consumer,
 * `deriveOperatorDaParams`, and every other subsystem that loads `NodeConfig`
 * ignores them. Enforcing the Q63 governed floors here would let a stale
 * deployment value (say the pre-Q63 `DA_THRESHOLD=1` still sitting in a
 * checkout's `.env`) fail config load for the whole process, surfacing as an
 * opaque error inside subsystems that never touch DA. The floors are instead
 * enforced in `deriveOperatorDaParams`, at the one point where a
 * governor-invalid datum would actually be written, where the real committee
 * length is known even when it is derived from local signers rather than
 * configured.
 *
 * What stays here is what is unambiguously wrong regardless of policy: a value
 * that is not hex, is not a whole number of elements, or is not the
 * sorted-unique ascending order the governor's walkers require.
 */
const daKeySetConfig = (name: string, chunkHexLength: number, shape: string) =>
  stringConfig(name, "").pipe(
    Config.mapAttempt((value) => {
      const normalized = value.trim().toLowerCase();
      if (normalized.length === 0) {
        return "";
      }
      let elements: readonly string[];
      try {
        elements = splitPackedHex(normalized, chunkHexLength, name);
      } catch {
        throw new Error(`${name} must be ${shape} as hex`);
      }
      if (!isStrictlyAscending(elements)) {
        throw new Error(
          `${name} must be sorted ascending with no duplicates, matching the governor's sorted-unique encoding`,
        );
      }
      return normalized;
    }),
  );

/** A lovelace amount the deployment profile fixes; unset takes the profile's. */
const profileLovelace = (name: string) =>
  Config.option(Config.string(name).pipe(Config.mapAttempt(BigInt)));

/**
 * The deployment the node runs in: the network and the compiled deployment
 * profile, the profile's economics, the operator's wallets and
 * reference-script addresses, and the DA committee it attests with.
 */
export const deploymentConfig = Config.all({
  NETWORK: Config.literal("Mainnet", "Preprod", "Preview", "Custom")("NETWORK"),
  profile: Config.string("MIDGARD_DEPLOYMENT_PROFILE").pipe(
    Config.mapAttempt(requireSelectedDeploymentProfile),
  ),
  L1_OPERATOR_SEED_PHRASE: requiredSeedPhrase("L1_OPERATOR_SEED_PHRASE"),
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: requiredSeedPhrase(
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  ),
  L1_SETTLEMENT_SEED_PHRASE: stringConfig("L1_SETTLEMENT_SEED_PHRASE", ""),
  L1_REFERENCE_SCRIPT_SEED_PHRASE: requiredSeedPhrase(
    "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  ),
  L1_REFERENCE_SCRIPT_ADDRESS: stringConfig("L1_REFERENCE_SCRIPT_ADDRESS", ""),
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: Config.option(
    Config.string("L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS"),
  ),
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS: positiveSafeIntegerConfig(
    "REFERENCE_SCRIPT_AUTH_TIMELOCK_MS",
    REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  ),
  OPERATOR_REQUIRED_BOND_LOVELACE: profileLovelace(
    "OPERATOR_REQUIRED_BOND_LOVELACE",
  ),
  OPERATOR_SLASHING_PENALTY_LOVELACE: profileLovelace(
    "OPERATOR_SLASHING_PENALTY_LOVELACE",
  ),
  // Q63 (F04 §4) governed floors are enforced in `deriveOperatorDaParams`,
  // not here. Config load only rejects values that are malformed no matter
  // what the policy is.
  DA_COMMITTEE_HEX: daKeySetConfig(
    "DA_COMMITTEE_HEX",
    VERIFICATION_KEY_HEX_LENGTH,
    "packed 32-byte verification keys",
  ),
  DA_THRESHOLD: stringConfig("DA_THRESHOLD", "").pipe(
    Config.mapAttempt((value) => {
      const trimmed = value.trim();
      if (trimmed.length === 0) {
        return null;
      }
      const threshold = BigInt(trimmed);
      if (threshold <= 0n) {
        throw new Error(
          `DA_THRESHOLD must be a positive integer, received ${threshold.toString()}`,
        );
      }
      return threshold;
    }),
  ),
  DA_OWNERS_HEX: daKeySetConfig(
    "DA_OWNERS_HEX",
    VERIFICATION_KEY_HASH_HEX_LENGTH,
    "packed 28-byte payment key hashes",
  ),
  DA_COSIGNER_SEED_PHRASE: stringConfig("DA_COSIGNER_SEED_PHRASE", "").pipe(
    Config.map((value) => value.trim()),
  ),
}).pipe(
  Config.mapAttempt(
    ({
      profile,
      L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: deployAddress,
      OPERATOR_REQUIRED_BOND_LOVELACE: bond,
      OPERATOR_SLASHING_PENALTY_LOVELACE: penalty,
      L1_SETTLEMENT_SEED_PHRASE: settlementSeedPhrase,
      ...settings
    }) => {
      const network = settings.NETWORK;
      if (network !== profile.network) {
        throw new Error("NETWORK must match the compiled deployment profile");
      }
      const L1_REFERENCE_SCRIPT_ADDRESS =
        settings.L1_REFERENCE_SCRIPT_ADDRESS.trim() ||
        seedPhraseAddress(settings.L1_REFERENCE_SCRIPT_SEED_PHRASE, network);
      const L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS = Option.getOrElse(
        deployAddress,
        () => L1_REFERENCE_SCRIPT_ADDRESS,
      );
      assertDistinctOperationalWallets({
        seeds: {
          ...settings,
          L1_SETTLEMENT_SEED_PHRASE: settlementSeedPhrase,
        },
        referenceScriptDeployAddress: L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
        network,
      });
      const economics = profile.economics;
      // Typed as any profile's, not the compiled one's: the type is every
      // node config's, whichever profile loaded it.
      const widened: {
        /** Required by listen; optional for read-only and deployment commands. */
        L1_SETTLEMENT_SEED_PHRASE?: string;
        DEPLOYMENT_ECONOMICS_PROFILE: DeploymentManifestEconomicsProfile;
      } = {
        L1_SETTLEMENT_SEED_PHRASE: settlementSeedPhrase,
        DEPLOYMENT_ECONOMICS_PROFILE: economics.profile,
      };
      const profileAmount = (
        name: string,
        configured: Option.Option<bigint>,
        expected: bigint,
      ) => {
        const amount = Option.getOrElse(configured, () => expected);
        if (amount !== expected) {
          throw new Error(
            `${name} must equal ${economics.profile} profile economics ${expected.toString()}`,
          );
        }
        return amount;
      };
      const OPERATOR_REQUIRED_BOND_LOVELACE = profileAmount(
        "OPERATOR_REQUIRED_BOND_LOVELACE",
        bond,
        BigInt(economics.requiredBondLovelace),
      );
      const OPERATOR_SLASHING_PENALTY_LOVELACE = profileAmount(
        "OPERATOR_SLASHING_PENALTY_LOVELACE",
        penalty,
        BigInt(economics.slashingPenaltyLovelace),
      );
      if (settings.DA_COSIGNER_SEED_PHRASE.length > 0) {
        // Derive once here so a malformed seed fails config load with a
        // ConfigError, the way the operator and reference-script seeds
        // already do, rather than surfacing later as an untyped defect inside
        // bootstrap or attestation signing.
        try {
          seedPhraseAddress(settings.DA_COSIGNER_SEED_PHRASE, network);
        } catch (cause) {
          throw new Error(
            `DA_COSIGNER_SEED_PHRASE is not a valid wallet seed phrase: ${String(cause)}`,
          );
        }
      }
      return {
        ...settings,
        ...widened,
        L1_REFERENCE_SCRIPT_ADDRESS,
        L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
        OPERATOR_REQUIRED_BOND_LOVELACE,
        OPERATOR_SLASHING_PENALTY_LOVELACE,
      };
    },
  ),
);
