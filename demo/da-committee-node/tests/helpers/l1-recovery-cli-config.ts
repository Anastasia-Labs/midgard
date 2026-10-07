// Uses the same public deployment/runtime fixture as config.load-committee-config.ts.
import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  LIBP2P_DA_TRANSPORT_LIMITS,
} from "../../src/config.js";
import { readDaDeploymentFixture } from "./deployment-fixture.js";
const LIBP2P_PEER_ID_A = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

const LIBP2P_PEER_ID_B = "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47";

const LIBP2P_PEER_ID_PUBLIC =
  "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";

const LIBP2P_PRIVATE_KEY_SOURCE = `seed:${"00".repeat(31)}01`;

const canonicalDeploymentManifest = await readDaDeploymentFixture();

export const DEPLOYMENT_MANIFEST_ID = canonicalDeploymentManifest.manifestId;
if (typeof DEPLOYMENT_MANIFEST_ID !== "string") {
  throw new Error("Canonical deployment fixture is missing manifestId");
}

export const libp2pManifest = (
  member: string,
  roles: readonly string[] = ["committee", "retrieval"],
  deploymentManifestId = DEPLOYMENT_MANIFEST_ID,
): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  network: "Preprod",
  deployment: {
    fingerprint: deploymentManifestId.toUpperCase(),
    contract_deployment_manifest_id: deploymentManifestId,
    contract_deployment_info_sha256: "cd".repeat(32),
    identity_source: "contract_deployment_manifest_id",
  },
  runtime_topology: {
    target: "committee",
    profile: "public",
    producer_peer_id: LIBP2P_PEER_ID_B,
    local_signer_index: 0,
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
    announce_multiaddrs: [
      `/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`,
    ],
    bootstrap_multiaddrs: [
      `/dns4/bootstrap.example/tcp/4001/p2p/${LIBP2P_PEER_ID_B}`,
    ],
    retention_days: LIBP2P_DA_MIN_RETENTION_DAYS,
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
    },
    limits: {
      max_payload_bytes: LIBP2P_DA_TRANSPORT_LIMITS.maxPayloadBytes,
      max_inline_response_bytes:
        LIBP2P_DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      max_chunk_bytes: LIBP2P_DA_TRANSPORT_LIMITS.maxChunkBytes,
      max_streams_per_peer: LIBP2P_DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      request_timeout_ms: LIBP2P_DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  },
  public_retained_da: {
    profile: "public-retained-da-v1",
    access_policy: "any_noise_authenticated_peer",
    peer_id: LIBP2P_PEER_ID_PUBLIC,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
    announce_multiaddrs: [
      `/dns4/public-da.example/tcp/4002/p2p/${LIBP2P_PEER_ID_PUBLIC}`,
    ],
    protocols: [
      "capabilities",
      "payload-by-header",
      "payload-chunk",
      "metadata-by-header",
      "proof-bundle-by-header",
      "trace-step-by-index",
      "event-to-step-by-event",
    ],
    limits: {
      max_streams_per_peer: 4,
      max_inflight_requests: 32,
      max_inflight_requests_per_peer: 2,
      max_inflight_proof_requests: 1,
      request_timeout_ms: LIBP2P_DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  },
  da_committee: {
    threshold: 1,
    members: [
      {
        signer_index: 0,
        da_vkey: member,
        peer_id: LIBP2P_PEER_ID_A,
        multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`],
        roles,
      },
    ],
  },
});

export const writeConfigFiles = async (
  dir: string,
  manifest: Record<string, unknown>,
): Promise<{
  readonly manifestPath: string;
  readonly deploymentInfoPath: string;
}> => {
  const manifestPath = join(dir, "manifest.json");
  const deploymentInfoPath = join(dir, "deployment.json");
  await writeFile(manifestPath, JSON.stringify(manifest));
  await writeMinimalDeploymentInfo(deploymentInfoPath);
  return { manifestPath, deploymentInfoPath };
};

const writeMinimalDeploymentInfo = async (
  path: string,
  manifestId = DEPLOYMENT_MANIFEST_ID,
): Promise<void> => {
  const fixture = await readDaDeploymentFixture();
  await writeFile(
    path,
    JSON.stringify({
      ...fixture,
      manifestId,
    }),
  );
};

export const libp2pConfigEnv = (
  dir: string,
  manifestPath: string,
  deploymentInfoPath: string,
): Record<string, string> => ({
  MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
  CARDANO_L1_SOURCE_MODE: "local_node",
  CARDANO_LOCAL_NODE_AUTHORITY_ID: "test-cardano-node",
  CARDANO_L1_TEST_MODE: "true",
  CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: "chain-sync:fixture:/tmp/state.json",
  CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH: join(
    dir,
    "chain-sync-cursor.json",
  ),
  CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
  CARDANO_FINALITY_DEPTH: String(
    DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  ),
  DA_LIBP2P_PRIVATE_KEY_SOURCE: LIBP2P_PRIVATE_KEY_SOURCE,
  DA_COMMITTEE_DATABASE_URL: "postgresql://unused.invalid/committee",
});
