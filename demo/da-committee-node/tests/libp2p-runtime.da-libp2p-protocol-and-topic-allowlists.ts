import { randomBytes } from "node:crypto";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DaGossipTopic,
  daGossipTopic,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it, vi } from "vitest";

import type { PublicRetainedDaConfig } from "../src/config.js";
import { DaGossip } from "../src/da/libp2p/DaGossip.js";
import { createDaProtocolAllowlist } from "../src/da/libp2p/DaProtocols.js";
import { createDaTopicAllowlist } from "../src/da/libp2p/DaTopics.js";
import {
  DEPLOYMENT_FINGERPRINT,
  libp2pConfig,
} from "./libp2p-runtime.da-libp2p-stream-framing.js";

describe("DA libp2p protocol and topic allowlists", () => {
  it("derives allowlisted IDs from the Phase 0/1 core transport module", () => {
    const protocols = createDaProtocolAllowlist(DEPLOYMENT_FINGERPRINT);
    const topics = createDaTopicAllowlist(DEPLOYMENT_FINGERPRINT);
    const protocolId = daRequestResponseProtocolId(
      DEPLOYMENT_FINGERPRINT,
      DaRequestResponseProtocol.payloadByHeader,
    );
    const topicId = daGossipTopic(
      DEPLOYMENT_FINGERPRINT,
      DaGossipTopic.payloadAnnouncements,
    );

    expect(protocols.hasProtocolId(protocolId)).toBe(true);
    expect(protocols.requireProtocolId(protocolId)).toBe("payload-by-header");
    expect(() =>
      protocols.requireProtocolId("/midgard/wrong/da/payload/1"),
    ).toThrow(/unsupported/);
    expect(topics.hasTopicId(topicId)).toBe(true);
    expect(topics.requireTopicId(topicId)).toBe("payload-announcements");
    expect(() => topics.requireTopicId("/midgard/wrong/da/topic/1")).toThrow(
      /unsupported/,
    );
  });

  it("publishes only allowlisted topics within gossip bounds", async () => {
    const published: { readonly topic: string; readonly data: Uint8Array }[] =
      [];
    const gossip = new DaGossip({
      pubsub: {
        publish: async (topic, data) => {
          published.push({ topic, data });
        },
        subscribe: vi.fn(),
      },
      topics: createDaTopicAllowlist(DEPLOYMENT_FINGERPRINT),
      config: libp2pConfig(),
    });

    await gossip.publish(DaGossipTopic.attestations, Buffer.from("ok"));

    expect(published).toEqual([
      {
        topic: daGossipTopic(
          DEPLOYMENT_FINGERPRINT,
          DaGossipTopic.attestations,
        ),
        data: Buffer.from("ok"),
      },
    ]);
    await expect(
      gossip.publish("/not/allowed", Buffer.from("x")),
    ).rejects.toThrow(/unsupported/);
    await expect(
      gossip.publish(DaGossipTopic.conflicts, Buffer.alloc(65_537)),
    ).rejects.toThrow(/exceeds 65536 bytes/);
  });
});

/** An identity-encoded V1 envelope of exactly `envelopeBytes` bytes. */
export const realisticIdentityEnvelope = async (
  envelopeBytes: number,
): Promise<Buffer> =>
  // 47 bytes of envelope framing for a body between 64 KiB and 4 GiB.
  wrapDaPayload(randomBytes(envelopeBytes - 47), { mode: "identity" });

export const publicRetainedDaConfig = (
  peerId: string,
): PublicRetainedDaConfig => ({
  peerId,
  privateKeySource: `seed:${"5a".repeat(32)}`,
  listenMultiaddrs: ["/ip4/127.0.0.1/tcp/0"],
  announceMultiaddrs: [],
  protocols: DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  limits: {
    maxStreamsPerPeer: 4,
    maxInflightRequests: 8,
    maxInflightRequestsPerPeer: 2,
    maxInflightProofRequests: 1,
    requestTimeoutMs: 2_000,
  },
});
