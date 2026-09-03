import { readFile } from "node:fs/promises";

import {
  assertMidgardConsensusReleaseReady,
  isMidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  normalizeDaDeploymentFingerprintHex,
  parseDaLibp2pRuntimeManifest,
} from "@al-ft/midgard-core/da-transport";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity-v1";

import type { PublicRetainedDaRuntimeConfig } from "./config.js";
import { normalizeHex } from "./utils/hex.js";

type Env = Record<string, string | undefined>;

/**
 * The dedicated public-reader configuration path. It intentionally has no
 * dependency on the committee config loader, signer, provider, submitter, or
 * mutable store configuration.
 */
export const loadPublicRetainedDaRuntimeConfig = async (
  env: Env = process.env,
): Promise<PublicRetainedDaRuntimeConfig> => {
  if (!booleanEnv(env.DA_PUBLIC_RETAINED_DA_ENABLED, false)) {
    throw new Error(
      "DA_PUBLIC_RETAINED_DA_ENABLED=true is required for the public retained-DA process",
    );
  }
  if (
    env.WATCHER_DB_PATH !== undefined ||
    env.WATCHER_DATABASE_URL !== undefined
  ) {
    throw new Error(
      "public retained-DA process requires DA_PUBLIC_RETAINED_DA_DATABASE_URL and must not receive WATCHER_DB_PATH or WATCHER_DATABASE_URL",
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
  const runtimeManifest = parseDaLibp2pRuntimeManifest(
    parseJsonObject(
      await readFile(deploymentManifestPath, "utf8"),
      deploymentManifestPath,
    ),
  );
  if (runtimeManifest.runtime_topology.target !== "watcher") {
    throw new Error("runtime_topology.target must be watcher");
  }
  const contractDeployment = verifyContractDeployment(
    parseJsonObject(
      await readFile(contractDeploymentInfoPath, "utf8"),
      contractDeploymentInfoPath,
    ),
    contractDeploymentInfoPath,
  );
  if (runtimeManifest.network !== contractDeployment.network) {
    throw new Error(
      "public retained-DA runtime manifest network must match contract deployment manifest network",
    );
  }
  if (
    env.MIDGARD_NETWORK !== undefined &&
    env.MIDGARD_NETWORK !== runtimeManifest.network
  ) {
    throw new Error(
      `MIDGARD_NETWORK must exactly match runtime manifest network ${runtimeManifest.network}`,
    );
  }
  if (
    runtimeManifest.deployment.contract_deployment_manifest_id !==
    contractDeployment.manifestId
  ) {
    throw new Error(
      "deployment.contract_deployment_manifest_id does not match contract deployment manifestId",
    );
  }
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
  const profile = runtimeManifest.public_retained_da;
  return {
    deploymentFingerprint: runtimeManifest.deployment.fingerprint,
    publicRetainedDa: {
      peerId: profile.peer_id,
      privateKeySource,
      listenMultiaddrs: [...profile.listen_multiaddrs],
      announceMultiaddrs: [...profile.announce_multiaddrs],
      protocols: [...profile.protocols],
      limits: {
        maxStreamsPerPeer: profile.limits.max_streams_per_peer,
        maxInflightRequests: profile.limits.max_inflight_requests,
        maxInflightRequestsPerPeer:
          profile.limits.max_inflight_requests_per_peer,
        maxInflightProofRequests: profile.limits.max_inflight_proof_requests,
        requestTimeoutMs: profile.limits.request_timeout_ms,
      },
    },
    dataLimits: {
      maxPayloadBytes: runtimeManifest.da_transport.limits.max_payload_bytes,
      maxInlineResponseBytes:
        runtimeManifest.da_transport.limits.max_inline_response_bytes,
      maxChunkBytes: runtimeManifest.da_transport.limits.max_chunk_bytes,
      maxStreamsPerPeer:
        runtimeManifest.da_transport.limits.max_streams_per_peer,
      requestTimeoutMs: runtimeManifest.da_transport.limits.request_timeout_ms,
    },
    databaseUrl,
    databaseRole,
  };
};

const verifyContractDeployment = (
  value: Record<string, unknown>,
  path: string,
): { readonly manifestId: string; readonly network: string } => {
  const verified = verifyFinalizedDeploymentManifest(value);
  if (!isMidgardConsensusProfile(verified.consensusProfile)) {
    throw new Error(`${path} does not contain the exact V1 consensus profile`);
  }
  assertMidgardConsensusReleaseReady();
  if (typeof verified.network !== "string" || verified.network.length === 0) {
    throw new Error(`${path} does not contain a deployment network`);
  }
  if (typeof verified.manifestId !== "string") {
    throw new Error(`${path} does not contain a deployment manifestId`);
  }
  return {
    manifestId: normalizeDaDeploymentFingerprintHex(verified.manifestId),
    network: verified.network,
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

const parseJsonObject = (
  raw: string,
  path: string,
): Record<string, unknown> => {
  let value: unknown;
  try {
    value = JSON.parse(raw);
  } catch (cause) {
    throw new Error(`${path} is not valid JSON`, { cause });
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${path} must be a JSON object`);
  }
  return value as Record<string, unknown>;
};
