import { writeFile } from "node:fs/promises";

import { DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION } from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  LIBP2P_DA_TRANSPORT_LIMITS,
  loadWatcherConfig,
} from "../src/config.js";
import { tempDir } from "./helpers.js";
import {
  readDaDeploymentFixture,
  writeDaDeploymentFixture,
} from "./helpers/deployment-fixture.js";

const committeePeerId = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const producerPeerId = "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47";
const publicRetainedDaPeerId =
  "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";

describe("canonical V1 startup release gate", () => {
  it("fails closed while validator-hash-bound release evidence is unavailable", async () => {
    const dir = await tempDir();
    const manifestPath = `${dir}/runtime.json`;
    const deploymentInfoPath = `${dir}/deployment.json`;
    const deployment = await readDaDeploymentFixture();
    const deploymentManifestId = String(deployment.manifestId);
    await writeFile(
      manifestPath,
      JSON.stringify({
        schemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        network: "Preview",
        deployment: {
          fingerprint: deploymentManifestId,
          contract_deployment_manifest_id: deploymentManifestId,
          contract_deployment_info_sha256: "cd".repeat(32),
          identity_source: "contract_deployment_manifest_id",
        },
        runtime_topology: {
          target: "watcher",
          profile: "public",
          producer_peer_id: producerPeerId,
          local_signer_index: 0,
        },
        da_transport: {
          kind: "libp2p",
          no_http_da_transport: true,
          listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
          announce_multiaddrs: [
            `/dns4/da.example/tcp/4001/p2p/${committeePeerId}`,
          ],
          bootstrap_multiaddrs: [
            `/dns4/producer.example/tcp/4001/p2p/${producerPeerId}`,
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
          peer_id: publicRetainedDaPeerId,
          listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
          announce_multiaddrs: [
            `/dns4/public-da.example/tcp/4002/p2p/${publicRetainedDaPeerId}`,
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
              da_vkey: "01".repeat(32),
              peer_id: committeePeerId,
              multiaddrs: [`/dns4/da.example/tcp/4001/p2p/${committeePeerId}`],
              roles: ["committee", "retrieval"],
            },
          ],
        },
      }),
    );
    await writeDaDeploymentFixture(deploymentInfoPath);

    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      }),
    ).rejects.toThrow(/not activated/u);
  });
});
