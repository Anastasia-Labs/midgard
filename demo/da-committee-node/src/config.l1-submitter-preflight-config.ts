import "./config.parse-l1-source-config.js";

import { isAbsolute, normalize } from "node:path";

import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import {
  type DeploymentManifestAvailabilityChallenge,
  parseDeploymentManifestAvailabilityChallenge,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  type CommitteeConfig,
  DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE,
  DA_L1_SUBMITTER_FEE_HEADROOM_LOVELACE,
  DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
  type Env,
  type L1SubmitterPreflightConfig,
  type LocalStateConfig,
  type NativeLedgerConfig,
} from "./config.committee-config.js";
import {
  booleanEnv,
  optionalKeySource,
  optionalNonEmpty,
} from "./config.operational-provider-identity.js";

export const nonNegativeInt = (value: string, name: string): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${name} must be a non-negative integer`);
  }
  return parsed;
};

export const positiveInt = (value: string, name: string): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${name} must be a positive integer`);
  }
  return parsed;
};

const positiveLovelace = (value: string, name: string): bigint => {
  const normalized = value.trim().replaceAll("_", "");
  if (!/^[0-9]+$/.test(normalized)) {
    throw new Error(`${name} must be a positive lovelace integer`);
  }
  const parsed = BigInt(normalized);
  if (parsed <= 0n) {
    throw new Error(`${name} must be a positive lovelace integer`);
  }
  return parsed;
};

export const l1SubmitterPreflightConfig = ({
  env,
  l1SubmissionEnabled,
  l1SubmitterKeySource,
}: {
  readonly env: Env;
  readonly l1SubmissionEnabled: boolean;
  readonly l1SubmitterKeySource?: string;
}): L1SubmitterPreflightConfig => {
  const autoFundKeySource = optionalKeySource(
    env.DA_L1_AUTO_FUND_KEY_SOURCE,
    "DA_L1_AUTO_FUND_KEY_SOURCE",
  );
  if (
    autoFundKeySource !== undefined &&
    l1SubmitterKeySource !== undefined &&
    autoFundKeySource === l1SubmitterKeySource
  ) {
    throw new Error(
      "DA_L1_AUTO_FUND_KEY_SOURCE must not equal L1_SUBMITTER_KEY_SOURCE",
    );
  }
  const minPlainAdaLovelace =
    env.DA_L1_MIN_PLAIN_ADA_LOVELACE === undefined
      ? DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE
      : positiveLovelace(
          env.DA_L1_MIN_PLAIN_ADA_LOVELACE,
          "DA_L1_MIN_PLAIN_ADA_LOVELACE",
        );
  if (minPlainAdaLovelace < DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE) {
    throw new Error(
      `DA_L1_MIN_PLAIN_ADA_LOVELACE must be at least ${DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE.toString()} (${DA_L1_SUBMITTER_FEE_HEADROOM_LOVELACE.toString()} fee headroom + ${DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE.toString()} attestation min-ADA)`,
    );
  }
  return {
    enabled:
      l1SubmissionEnabled && booleanEnv(env.DA_L1_PREFLIGHT_ENABLED, true),
    minPlainAdaLovelace,
    minCollateralLovelace: positiveLovelace(
      env.DA_L1_MIN_COLLATERAL_LOVELACE ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.minCollateralLovelace.toString(),
      "DA_L1_MIN_COLLATERAL_LOVELACE",
    ),
    minSpendableUtxoCount: positiveInt(
      env.DA_L1_MIN_SPENDABLE_UTXO_COUNT ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.minSpendableUtxoCount.toString(),
      "DA_L1_MIN_SPENDABLE_UTXO_COUNT",
    ),
    ...(autoFundKeySource === undefined ? {} : { autoFundKeySource }),
    autoFundBufferLovelace: positiveLovelace(
      env.DA_L1_AUTO_FUND_BUFFER_LOVELACE ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.autoFundBufferLovelace.toString(),
      "DA_L1_AUTO_FUND_BUFFER_LOVELACE",
    ),
    retryCount: positiveInt(
      env.DA_L1_PREFLIGHT_RETRY_COUNT ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.retryCount.toString(),
      "DA_L1_PREFLIGHT_RETRY_COUNT",
    ),
    retryDelayMs: positiveInt(
      env.DA_L1_PREFLIGHT_RETRY_DELAY_MS ??
        DEFAULT_L1_SUBMITTER_PREFLIGHT.retryDelayMs.toString(),
      "DA_L1_PREFLIGHT_RETRY_DELAY_MS",
    ),
  };
};

export const signerIndex = (value: string): number => {
  const parsed = nonNegativeInt(value, "DA_SIGNER_INDEX");
  if (parsed > 255) {
    throw new Error("DA_SIGNER_INDEX must fit in one byte");
  }
  return parsed;
};

export const optionalSignerConfig = (
  env: Env,
): Pick<CommitteeConfig, "signerIndex" | "signerKeySource"> => {
  const configuredMode = optionalNonEmpty(env.DA_MODE);
  if (configuredMode !== undefined) {
    throw new Error("DA_MODE has been removed and must be omitted");
  }
  const index = optionalNonEmpty(env.DA_SIGNER_INDEX);
  const indexedSources = Object.keys(env).filter((name) =>
    name.startsWith("DA_SIGNER_KEY_SOURCE_"),
  );
  if (indexedSources.length > 0) {
    if (env.DA_SIGNER_KEY_SOURCE !== undefined) {
      throw new Error(
        "Use either DA_SIGNER_KEY_SOURCE or indexed DA_SIGNER_KEY_SOURCE_<index> settings, not both",
      );
    }
    for (const name of indexedSources) {
      const suffix = name.slice("DA_SIGNER_KEY_SOURCE_".length);
      if (
        !/^(0|[1-9][0-9]{0,2})$/u.test(suffix) ||
        Number(suffix) > 255 ||
        optionalNonEmpty(env[name]) === undefined
      ) {
        throw new Error(
          "Indexed DA signer sources require canonical indices from 0 to 255 and nonempty values",
        );
      }
    }
    if (index === undefined) {
      throw new Error(
        "DA_SIGNER_INDEX is required to select an indexed signer",
      );
    }
    const selectedIndex = signerIndex(index);
    const selectedSource = optionalNonEmpty(
      env[`DA_SIGNER_KEY_SOURCE_${selectedIndex}`],
    );
    if (selectedSource === undefined) {
      throw new Error("No indexed DA signer source matches DA_SIGNER_INDEX");
    }
    return { signerIndex: selectedIndex, signerKeySource: selectedSource };
  }
  const keySource = optionalNonEmpty(env.DA_SIGNER_KEY_SOURCE);
  if (index === undefined && keySource === undefined) {
    return {};
  }
  if (index === undefined || keySource === undefined) {
    throw new Error(
      "DA_SIGNER_INDEX and DA_SIGNER_KEY_SOURCE must be set together",
    );
  }
  return {
    signerIndex: signerIndex(index),
    signerKeySource: keySource,
  };
};

const RETIRED_WATCHER_ENV_NAMES: Readonly<Record<string, string>> = {
  WATCHER_API_HOST: "DA_COMMITTEE_API_HOST",
  WATCHER_API_PORT: "DA_COMMITTEE_API_PORT",
  WATCHER_POLL_INTERVAL_MS: "DA_COMMITTEE_POLL_INTERVAL_MS",
  WATCHER_DB_PATH: "DA_COMMITTEE_DB_PATH",
  WATCHER_DATABASE_URL: "DA_COMMITTEE_DATABASE_URL",
};

/**
 * The committee node read `WATCHER_*` variables before the DA committee role
 * was split out of the watcher.  Refuse them rather than silently ignoring a
 * setting the operator believes is in effect.
 */
export const rejectRetiredWatcherEnvNames = (env: Env): void => {
  const present = Object.keys(RETIRED_WATCHER_ENV_NAMES).filter(
    (name) => env[name] !== undefined,
  );
  if (present.length > 0) {
    throw new Error(
      `retired environment variable(s) ${present.join(", ")}: the DA committee node is not the watcher; use ${present
        .map((name) => RETIRED_WATCHER_ENV_NAMES[name])
        .join(", ")}`,
    );
  }
};

export const localState = (env: Env): LocalStateConfig => {
  rejectRetiredWatcherEnvNames(env);
  const dbPath = optionalNonEmpty(env.DA_COMMITTEE_DB_PATH);
  const databaseUrl = optionalNonEmpty(env.DA_COMMITTEE_DATABASE_URL);
  if (dbPath !== undefined && databaseUrl !== undefined) {
    throw new Error(
      "set only one of DA_COMMITTEE_DB_PATH or DA_COMMITTEE_DATABASE_URL",
    );
  }
  if (dbPath !== undefined) {
    return { kind: "file", path: dbPath };
  }
  if (databaseUrl !== undefined) {
    return { kind: "database", url: databaseUrl };
  }
  throw new Error(
    "DA_COMMITTEE_DB_PATH or DA_COMMITTEE_DATABASE_URL is required",
  );
};

export const availabilityJournalPath = (env: Env): string | undefined => {
  const path = optionalNonEmpty(env.DA_AVAILABILITY_JOURNAL_PATH);
  if (path !== undefined && !isAbsolute(path)) {
    throw new Error(
      "DA_AVAILABILITY_JOURNAL_PATH must be an absolute durable file path",
    );
  }
  return path;
};

const NATIVE_LEDGER_PATH_SETTINGS = [
  ["socketPath", "CARDANO_LOCAL_NODE_SOCKET_PATH"],
  ["nodeConfigPath", "CARDANO_LOCAL_NODE_CONFIG_PATH"],
  ["binaryPath", "CARDANO_NATIVE_CHAIN_SYNC_BINARY_PATH"],
] as const;

export const DEFAULT_NATIVE_LEDGER_AUTHORITY_ID = "local-cardano-node";

export const NATIVE_LEDGER_AUTHORITY_ID =
  /^[a-z0-9](?:[a-z0-9._-]{0,62}[a-z0-9])?$/u;

/**
 * The local node ledger settings are all-or-none and allowed in both source
 * modes: the reward-account gap is in Ogmios, not in the source mode. Paths
 * must be lexically canonical here; symlinks are refused when the authority is
 * resolved against the filesystem.
 */
export const parseNativeLedgerConfig = (
  env: Env,
): NativeLedgerConfig | undefined => {
  const values = NATIVE_LEDGER_PATH_SETTINGS.map(
    ([field, name]) => [field, name, optionalNonEmpty(env[name])] as const,
  );
  const missing = values.filter(([, , value]) => value === undefined);
  if (missing.length === values.length) {
    return undefined;
  }
  if (missing.length > 0) {
    throw new Error(
      `Local node ledger settings are all-or-none; missing ${missing.map(([, name]) => name).join(", ")}`,
    );
  }
  for (const [, name, value] of values) {
    if (
      !isAbsolute(value!) ||
      normalize(value!) !== value ||
      (value !== "/" && value!.endsWith("/"))
    ) {
      throw new Error(`${name} must be an absolute canonical path`);
    }
  }
  const authorityNodeId =
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_AUTHORITY_ID) ??
    DEFAULT_NATIVE_LEDGER_AUTHORITY_ID;
  if (!NATIVE_LEDGER_AUTHORITY_ID.test(authorityNodeId)) {
    throw new Error(
      "CARDANO_LOCAL_NODE_AUTHORITY_ID must be a native ledger authority id (lowercase letters, digits, '.', '_' or '-', at most 64 characters, alphanumeric at both ends) when the local node ledger is configured",
    );
  }
  const [socketPath, nodeConfigPath, binaryPath] = values.map(
    ([, , value]) => value!,
  );
  return {
    authorityNodeId,
    socketPath: socketPath!,
    nodeConfigPath: nodeConfigPath!,
    binaryPath: binaryPath!,
  };
};

export const contractDeploymentManifestConfig = (
  contractDeploymentInfo: Record<string, unknown>,
): {
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly network: string;
  readonly daRetentionDays: number;
  readonly finalityDepth: number;
  readonly automaticRecoveryMaxDepth: number;
  readonly availabilityChallenge: DeploymentManifestAvailabilityChallenge;
} => {
  const verified = verifyFinalizedDeploymentManifest(contractDeploymentInfo);
  return {
    manifestId: verified.manifestId,
    consensusProfile: verified.consensusProfile,
    network: verified.network,
    // The retention window is part of the verified deployment identity.
    daRetentionDays: verified.da.transportProfile.retentionDays,
    finalityDepth: verified.l1Finality.confirmationDepth,
    automaticRecoveryMaxDepth: verified.l1Finality.automaticRecoveryMaxDepth,
    // Preserve the parser's independently owned, frozen configuration value.
    availabilityChallenge: parseDeploymentManifestAvailabilityChallenge(
      verified.availabilityChallenge,
    ),
  };
};
