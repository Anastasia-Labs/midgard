import type { Env } from "./config.committee-config.js";
import { rejectRetiredWatcherEnvNames } from "./config.l1-submitter-preflight-config.js";
import { normalizeHex } from "./utils/hex.js";

/** The public reader's settings its environment alone fixes. */
export type PublicRetainedDaEnvironmentConfig = {
  readonly deploymentManifestPath: string;
  readonly contractDeploymentInfoPath: string;
  readonly privateKeySource: string;
  readonly databaseUrl: string;
  readonly databaseRole: string;
};

/**
 * Parses every public-reader setting that needs neither deployment file: the
 * part of `loadPublicRetainedDaRuntimeConfig` that runs before it reads them,
 * so a deployment tool can check the reader's environment before the
 * deployment exists.
 */
export const parsePublicRetainedDaEnvironment = (
  env: Env,
): PublicRetainedDaEnvironmentConfig => {
  if (!booleanEnv(env.DA_PUBLIC_RETAINED_DA_ENABLED, false)) {
    throw new Error(
      "DA_PUBLIC_RETAINED_DA_ENABLED=true is required for the public retained-DA process",
    );
  }
  rejectRetiredWatcherEnvNames(env);
  if (env.DA_COMMITTEE_DATABASE_URL !== undefined) {
    throw new Error(
      "public retained-DA process requires DA_PUBLIC_RETAINED_DA_DATABASE_URL and must not receive DA_COMMITTEE_DATABASE_URL",
    );
  }
  const deploymentManifestPath = requireEnv(
    env,
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  );
  const contractDeploymentInfoPath = requireEnv(
    env,
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
  );
  const privateKeySource = requireEnv(
    env,
    "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE",
  );
  validateLibp2pPrivateKeySource(privateKeySource);
  const databaseUrl = requireEnv(env, "DA_PUBLIC_RETAINED_DA_DATABASE_URL");
  const databaseRole = requireEnv(env, "DA_PUBLIC_RETAINED_DA_DATABASE_ROLE");
  if (databaseRole.trim() !== databaseRole || databaseRole.length === 0) {
    throw new Error(
      "DA_PUBLIC_RETAINED_DA_DATABASE_ROLE must be a non-empty exact PostgreSQL role name",
    );
  }
  return {
    deploymentManifestPath,
    contractDeploymentInfoPath,
    privateKeySource,
    databaseUrl,
    databaseRole,
  };
};

const validateLibp2pPrivateKeySource = (source: string): void => {
  if (source.startsWith("seed:")) {
    normalizeHex(source.slice("seed:".length), {
      fieldName: "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE seed",
      byteLength: 32,
    });
    return;
  }
  if (source.startsWith("hex:")) {
    const encoded = source.slice("hex:".length);
    if (encoded.length === 0) {
      throw new Error(
        "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE must include a hex key",
      );
    }
    normalizeHex(encoded, {
      fieldName: "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE protobuf key",
    });
    return;
  }
  if (source.startsWith("file:")) {
    if (source.slice("file:".length).length === 0) {
      throw new Error(
        "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE file path is required",
      );
    }
    return;
  }
  throw new Error(
    "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE must use seed:, hex:, or file:",
  );
};

const booleanEnv = (
  value: string | undefined,
  defaultValue: boolean,
): boolean => {
  const normalized = value?.trim().toLowerCase();
  if (normalized === undefined || normalized === "") return defaultValue;
  if (["1", "true", "yes", "on"].includes(normalized)) return true;
  if (["0", "false", "no", "off"].includes(normalized)) return false;
  throw new Error("boolean environment values must be true or false");
};

const requireEnv = (env: Env, name: string): string => {
  const value = env[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`${name} is required`);
  }
  return value;
};
