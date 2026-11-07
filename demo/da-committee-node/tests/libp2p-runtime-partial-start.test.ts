import { createServer } from "node:net";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { createLibp2p } from "libp2p";
import { describe, expect, it } from "vitest";

import type { Libp2pDaTransportConfig } from "../src/config.js";
import {
  DaLibp2pNode,
  type DaLibp2pRuntimeNode,
} from "../src/da/libp2p/DaLibp2pNode.js";
import { reserveLoopbackPort } from "./libp2p-attestation-exchange.state-queue-record.js";

const DEPLOYMENT_FINGERPRINT = "ab".repeat(32);
const KEY_SOURCE = `seed:${"00".repeat(31)}b1`;
/** A bootstrap peer that never answers: libp2p needs a non-empty list. */
const BOOTSTRAP_KEY_SOURCE = `seed:${"00".repeat(31)}b2`;

/** Resolves to the bind error code, or "free" after binding and releasing. */
const probePort = (port: number): Promise<string> =>
  new Promise((resolve) => {
    const server = createServer();
    server.once("error", (error: NodeJS.ErrnoException) =>
      resolve(error.code ?? error.message),
    );
    server.listen(port, "127.0.0.1", () => {
      server.close(() => resolve("free"));
    });
  });

const loopbackConfig = async (
  port: number,
): Promise<Libp2pDaTransportConfig> => {
  const { peerId } = await loadDaLibp2pIdentity(KEY_SOURCE);
  const bootstrap = await loadDaLibp2pIdentity(BOOTSTRAP_KEY_SOURCE);
  const bootstrapPort = await reserveLoopbackPort();
  const multiaddr = `/ip4/127.0.0.1/tcp/${port.toString()}/p2p/${peerId}`;
  return {
    kind: "libp2p",
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    noHttpDaTransport: true,
    threshold: 1,
    listenMultiaddrs: [`/ip4/127.0.0.1/tcp/${port.toString()}`],
    announceMultiaddrs: [multiaddr],
    bootstrapMultiaddrs: [
      `/ip4/127.0.0.1/tcp/${bootstrapPort.toString()}/p2p/${bootstrap.peerId}`,
    ],
    gossip: {
      strictSign: true,
      emitSelf: false,
      allowedTopicsOnly: true,
      maxGossipMessageBytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
    },
    limits: {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
      maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
    retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
    peers: [
      {
        signerIndex: 0,
        daVkey: "01".repeat(32),
        peerId,
        multiaddrs: [multiaddr],
        roles: ["committee", "retrieval"],
      },
    ],
  };
};

describe("DA libp2p committee node partial start", () => {
  it("releases the bound listen socket when start fails after the bind, so a retry binds the same port", async () => {
    const port = await reserveLoopbackPort();
    const config = await loopbackConfig(port);
    const failing = new DaLibp2pNode({
      config,
      privateKeySource: KEY_SOURCE,
      libp2pFactory: async (options) => {
        const runtime = await createLibp2p(options);
        const pubsub = (
          runtime.services as unknown as {
            pubsub: { subscribe: (topic: string) => void };
          }
        ).pubsub;
        pubsub.subscribe = () => {
          throw new Error("gossip subscription refused");
        };
        return runtime as unknown as DaLibp2pRuntimeNode;
      },
    });

    await expect(failing.start()).rejects.toThrow(
      /gossip subscription refused/u,
    );
    expect(failing.isStarted()).toBe(false);
    expect(await probePort(port)).toBe("free");
    // The runtime still calls stop() on a failed start; it changes nothing.
    await failing.stop();

    const retried = new DaLibp2pNode({
      config,
      privateKeySource: KEY_SOURCE,
    });
    await retried.start();
    try {
      expect(retried.isStarted()).toBe(true);
      expect(await probePort(port)).toBe("EADDRINUSE");
    } finally {
      await retried.stop();
    }
    expect(await probePort(port)).toBe("free");
  });
});
