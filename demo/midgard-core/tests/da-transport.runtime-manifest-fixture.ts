import { expect } from "vitest";

import {
  DA_TRANSPORT_LIMITS,
  type DaPayloadChunkManifest,
} from "../src/da-transport.js";

export const h = (byte: string, count: number): string => byte.repeat(count);

export const b = (byte: number, count: number): Buffer =>
  Buffer.alloc(count, byte);

export const deployment = b(0x01, 32);

export const header = b(0x02, 28);

export const payload = b(0x03, 32);

export const chunkManifest: DaPayloadChunkManifest = {
  payloadHash: payload,
  totalBytes: 5,
  chunkSize: 2,
  chunkHashes: [b(0x04, 32), b(0x05, 32)],
};

export const assertVector = <T>(
  label: string,
  encode: (value: T) => Buffer,
  decode: (bytes: Uint8Array) => T,
  value: T,
  hex: string,
): void => {
  const encoded = encode(value);
  expect(encoded.toString("hex"), label).toBe(hex);
  expect(decode(encoded)).toEqual(value);
};

export const runtimeManifestFixture = (): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  network: "Preview",
  deployment: {
    fingerprint: h("AB", 32),
    contract_deployment_manifest_id: h("ab", 32),
    contract_deployment_info_sha256: h("cd", 32),
    identity_source: "contract_deployment_manifest_id",
  },
  runtime_topology: {
    target: "producer",
    profile: "public",
    producer_peer_id: "peer-producer",
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/39002"],
    announce_multiaddrs: ["/dns4/producer.example/tcp/39002/p2p/peer-producer"],
    bootstrap_multiaddrs: [],
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
    },
    limits: {
      max_payload_bytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      max_inline_response_bytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      max_chunk_bytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
      max_streams_per_peer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
    retention_days: DA_TRANSPORT_LIMITS.minimumRetentionDays,
  },
  public_retained_da: {
    profile: "public-retained-da-v1",
    access_policy: "any_noise_authenticated_peer",
    peer_id: "peer-public",
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/39003"],
    announce_multiaddrs: ["/dns4/public.example/tcp/39003/p2p/peer-public"],
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
      request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  },
  da_committee: {
    threshold: 1,
    members: [
      {
        signer_index: 0,
        da_vkey: h("01", 32),
        peer_id: "peer-a",
        multiaddrs: ["/dns4/da-a.example/tcp/39001/p2p/peer-a"],
        roles: ["committee"],
      },
    ],
  },
});
