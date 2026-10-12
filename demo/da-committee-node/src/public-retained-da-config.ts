import { readFile } from "node:fs/promises";

import { parseDaLibp2pRuntimeManifest } from "@al-ft/midgard-core/da-transport";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  parsePublicRetainedDaEnvironment,
  type PublicRetainedDaRuntimeConfig,
} from "./config.js";

type Env = Record<string, string | undefined>;

/**
 * The dedicated public-reader configuration path. It intentionally has no
 * dependency on the committee config loader, signer, provider, submitter, or
 * mutable store configuration.
 */
export const loadPublicRetainedDaRuntimeConfig = async (
  env: Env = process.env,
): Promise<PublicRetainedDaRuntimeConfig> => {
  // Everything the environment alone fixes, refused before either file is read.
  const {
    deploymentManifestPath,
    contractDeploymentInfoPath,
    privateKeySource,
    databaseUrl,
    databaseRole,
  } = parsePublicRetainedDaEnvironment(env);
  const runtimeManifest = parseDaLibp2pRuntimeManifest(
    parseJsonObject(
      await readFile(deploymentManifestPath, "utf8"),
      deploymentManifestPath,
    ),
  );
  if (runtimeManifest.runtime_topology.target !== "committee") {
    throw new Error("runtime_topology.target must be committee");
  }
  const contractDeployment = verifyContractDeployment(
    parseJsonObject(
      await readFile(contractDeploymentInfoPath, "utf8"),
      contractDeploymentInfoPath,
    ),
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
): { readonly manifestId: string; readonly network: string } => {
  const verified = verifyFinalizedDeploymentManifest(value);
  return {
    manifestId: verified.manifestId,
    network: verified.network,
  };
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
