import {
  DA_TRANSPORT_LIMITS_V1,
  DaGossipTopic,
  daGossipTopic,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { multiaddr } from "@multiformats/multiaddr";
import { describe, expect, it, vi } from "vitest";

import type { Libp2pDaTransportConfig } from "../src/config.js";
import { DaGossip } from "../src/da/libp2p/DaGossip.js";
import {
  DaLibp2pNode,
  type DaLibp2pRuntimeNode,
} from "../src/da/libp2p/DaLibp2pNode.js";
import {
  DaPeerRegistry,
  peerIdFromMultiaddrString,
} from "../src/da/libp2p/DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "../src/da/libp2p/DaProtocols.js";
import {
  decodeDaStreamFrames,
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "../src/da/libp2p/DaStreamCodec.js";
import { createDaTopicAllowlist } from "../src/da/libp2p/DaTopics.js";
import { createDaConnectionGater } from "../src/da/libp2p/index.js";
import { DaLibp2pPayloadSource } from "../src/da/libp2p/payload-source.js";

const DEPLOYMENT_FINGERPRINT = "ab".repeat(32);
const PEER_ID_A = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const PEER_ID_B = "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47";
const PEER_ID_UNKNOWN = "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";

describe("DA libp2p peer registry", () => {
  it("indexes only manifest committee and bootstrap peers", () => {
    const registry = DaPeerRegistry.fromConfig(libp2pConfig());

    expect(registry.size).toBe(2);
    expect(registry.isKnownPeerId(PEER_ID_A)).toBe(true);
    expect(registry.isKnownPeerId(PEER_ID_B)).toBe(true);
    expect(registry.isKnownPeerId(PEER_ID_UNKNOWN)).toBe(false);
    expect(registry.getBySignerIndex(0)?.peerId).toBe(PEER_ID_A);
    expect(
      registry.peersForRole("retrieval").map((entry) => entry.peerId),
    ).toEqual([PEER_ID_A]);
    expect(
      peerIdFromMultiaddrString(`/dns4/da-a.example/tcp/4001/p2p/${PEER_ID_A}`),
    ).toBe(PEER_ID_A);
  });

  it("includes bootstrap producer peers in payload retrieval attempts", async () => {
    const config = libp2pConfig();
    const registry = DaPeerRegistry.fromConfig(config);
    const requestedPeerIds: string[] = [];
    const source = new DaLibp2pPayloadSource({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      node: {
        request: async ({ peer }: { peer: { peerId: string } }) => {
          requestedPeerIds.push(peer.peerId);
          throw new Error("peer offline");
        },
      } as unknown as DaLibp2pNode,
      registry,
      limits: config.limits,
    });

    const result = await source.fetchPayloadCandidates("00".repeat(28));

    expect(result.ok).toBe(false);
    expect(requestedPeerIds).toEqual([PEER_ID_A, PEER_ID_B]);
    expect(result.attempts.map((attempt) => attempt.sourcePeerId)).toEqual([
      PEER_ID_A,
      PEER_ID_B,
    ]);
  });
});

describe("DA libp2p connection gater", () => {
  it("denies unmanifested peers and unmanifested multiaddrs", () => {
    const registry = DaPeerRegistry.fromConfig(libp2pConfig());
    const gater = createDaConnectionGater(registry);
    const knownPeer = peerId(PEER_ID_A);
    const unknownPeer = peerId(PEER_ID_UNKNOWN);
    const knownAddr = multiaddr(`/dns4/da-a.example/tcp/4001/p2p/${PEER_ID_A}`);
    const wrongAddr = multiaddr(
      `/dns4/other.example/tcp/4001/p2p/${PEER_ID_A}`,
    );

    expect(gater.denyDialPeer?.(knownPeer)).toBe(false);
    expect(gater.denyDialPeer?.(unknownPeer)).toBe(true);
    expect(gater.denyDialMultiaddr?.(knownAddr)).toBe(false);
    expect(gater.denyDialMultiaddr?.(wrongAddr)).toBe(true);
    expect(
      gater.denyInboundEncryptedConnection?.(unknownPeer, {} as never),
    ).toBe(true);
    expect(gater.filterMultiaddrForPeer?.(knownPeer, knownAddr)).toBe(true);
    expect(gater.filterMultiaddrForPeer?.(knownPeer, wrongAddr)).toBe(true);
    expect(
      gater.filterMultiaddrForPeer?.(
        knownPeer,
        multiaddr("/dns4/da-a.example/tcp/4001"),
      ),
    ).toBe(false);
  });
});

describe("DA libp2p stream framing", () => {
  it("round-trips bounded length-prefixed frames across chunk boundaries", async () => {
    const frame = encodeDaStreamFrame(Buffer.from("payload"), {
      maxFrameBytes: 16,
    });
    const decoded = await collect(
      decodeDaStreamFrames([frame.subarray(0, 2), frame.subarray(2)], {
        maxFrameBytes: 16,
      }),
    );

    expect(decoded.map((entry) => entry.toString("utf8"))).toEqual(["payload"]);
    await expect(
      readSingleDaStreamFrame([frame], { maxFrameBytes: 16 }),
    ).resolves.toEqual(Buffer.from("payload"));
  });

  it("assembles fragmented and adjacent frames with one bounded destination each", async () => {
    const timing = vi.fn();
    let now = 0;
    const wire = Buffer.concat([
      encodeDaStreamFrame(Buffer.alloc(257, 0xa1), { maxFrameBytes: 512 }),
      encodeDaStreamFrame(Buffer.from("second"), { maxFrameBytes: 512 }),
    ]);
    const fragments = Array.from(wire, (byte) => Buffer.from([byte]));
    const decoded = await collect(
      decodeDaStreamFrames(fragments, {
        maxFrameBytes: 512,
        timing: {
          monotonicNow: () => {
            now += 1;
            return now;
          },
          onStageTiming: timing,
        },
      }),
    );

    expect(decoded).toEqual([Buffer.alloc(257, 0xa1), Buffer.from("second")]);
    expect(timing).toHaveBeenCalledTimes(2);
    expect(timing.mock.calls.map(([stage]) => stage)).toEqual([
      "frame_receive",
      "frame_receive",
    ]);
  });

  it("reports backpressure-aware frame writes without changing drain ordering", async () => {
    const order: string[] = [];
    const timings: Array<{
      readonly stage: string;
      readonly durationMs: number;
    }> = [];
    let now = 20;
    await writeDaStreamFrame(
      {
        send: () => {
          order.push("send");
          return false;
        },
        onDrain: async () => {
          order.push("drain");
        },
        close: () => {
          order.push("close");
        },
      },
      Buffer.from("response"),
      {
        close: true,
        timing: {
          monotonicNow: () => {
            now += 3;
            return now;
          },
          onStageTiming: (stage, durationMs) =>
            timings.push({ stage, durationMs }),
        },
      },
    );

    expect(order).toEqual(["send", "drain", "close"]);
    expect(timings).toEqual([{ stage: "frame_write", durationMs: 3 }]);
  });

  it("keeps framing semantics when the optional timing clock throws", async () => {
    const frame = encodeDaStreamFrame(Buffer.from("payload"), {
      maxFrameBytes: 16,
    });
    const timing = {
      monotonicNow: () => {
        throw new Error("clock unavailable");
      },
      onStageTiming: () => {
        throw new Error("sink unavailable");
      },
    };
    await expect(
      collect(decodeDaStreamFrames([frame], { maxFrameBytes: 16, timing })),
    ).resolves.toEqual([Buffer.from("payload")]);
    await expect(
      writeDaStreamFrame({ send: () => true }, Buffer.from("payload"), {
        maxFrameBytes: 16,
        timing,
      }),
    ).resolves.toBeUndefined();
  });

  it("rejects oversized, empty, and incomplete frames", async () => {
    expect(() =>
      encodeDaStreamFrame(Buffer.alloc(17), { maxFrameBytes: 16 }),
    ).toThrow(/exceeds 16 bytes/);
    expect(() =>
      encodeDaStreamFrame(Buffer.alloc(0), { maxFrameBytes: 16 }),
    ).toThrow(/must not be empty/);

    const oversized = Buffer.alloc(4);
    oversized.writeUInt32BE(17, 0);
    await expect(
      collect(decodeDaStreamFrames([oversized], { maxFrameBytes: 16 })),
    ).rejects.toThrow(/exceeds 16 bytes/);
    await expect(
      collect(
        decodeDaStreamFrames([Buffer.from([0, 0])], { maxFrameBytes: 16 }),
      ),
    ).rejects.toThrow(/incomplete/);

    const incompleteBody = Buffer.alloc(6);
    incompleteBody.writeUInt32BE(4, 0);
    await expect(
      collect(decodeDaStreamFrames([incompleteBody], { maxFrameBytes: 16 })),
    ).rejects.toThrow(/incomplete/);

    const valid = encodeDaStreamFrame(Buffer.from("payload"), {
      maxFrameBytes: 16,
    });
    await expect(
      readSingleDaStreamFrame([valid, valid], { maxFrameBytes: 16 }),
    ).rejects.toThrow(/exactly one/u);
  });
});

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

describe("DA libp2p runtime service lifecycle", () => {
  it("enforces one absolute deadline across stream write and close", async () => {
    const config = libp2pConfig();
    const protocolId = daRequestResponseProtocolId(
      DEPLOYMENT_FINGERPRINT,
      DaRequestResponseProtocol.payloadByHeader,
    );
    const abort = vi.fn();
    const stream = {
      send: () => true,
      close: () => new Promise<void>(() => undefined),
      abort,
      async *[Symbol.asyncIterator]() {
        await new Promise<void>(() => undefined);
      },
    };
    const runtime: DaLibp2pRuntimeNode = {
      start: vi.fn(),
      stop: vi.fn(),
      handle: vi.fn(),
      unhandle: vi.fn(),
      dialProtocol: vi.fn(async () => stream),
    };
    const service = new DaLibp2pNode({
      config,
      libp2pFactory: async () => runtime,
    });
    await service.start();
    const peer = service.registry.getBySignerIndex(0);
    if (peer === undefined) throw new Error("missing fixture peer");

    await expect(
      service.request({
        peer,
        protocolId,
        payload: Buffer.from("request"),
        timeoutMs: 10,
      }),
    ).rejects.toThrow(/exceeded the 10ms deadline/);
    expect(abort).toHaveBeenCalledOnce();
    await service.stop();
  });

  it("builds the pinned stack and gracefully starts and stops mocked libp2p", async () => {
    const config = libp2pConfig();
    const protocolId = daRequestResponseProtocolId(
      DEPLOYMENT_FINGERPRINT,
      DaRequestResponseProtocol.payloadByHeader,
    );
    const handler = vi.fn();
    const handled: {
      readonly protocol: string;
      readonly handler: (
        stream: unknown,
        connection: unknown,
      ) => Promise<void> | void;
      readonly options: unknown;
    }[] = [];
    const unhandled: string[] = [];
    const subscribed: string[] = [];
    const unsubscribed: string[] = [];
    let capturedOptions: unknown;
    const runtime: DaLibp2pRuntimeNode = {
      services: {
        pubsub: {
          publish: vi.fn(),
          subscribe: vi.fn((topic: string) => {
            subscribed.push(topic);
          }),
          unsubscribe: vi.fn((topic: string) => {
            unsubscribed.push(topic);
          }),
        },
      },
      start: vi.fn(),
      stop: vi.fn(),
      handle: vi.fn((protocol, streamHandler, options) => {
        handled.push({ protocol, handler: streamHandler, options });
      }),
      unhandle: vi.fn((protocol) => {
        unhandled.push(protocol);
      }),
    };
    const service = new DaLibp2pNode({
      config,
      requestHandlers: new Map([[protocolId, handler]]),
      libp2pFactory: async (options) => {
        capturedOptions = options;
        return runtime;
      },
    });

    await service.start();
    await handled[0]!.handler({}, { remotePeer: peerId(PEER_ID_A) });

    expect(service.isStarted()).toBe(true);
    expect(capturedOptions).toMatchObject({
      start: false,
      addresses: {
        listen: config.listenMultiaddrs,
        announce: config.announceMultiaddrs,
      },
    });
    expect(stackLengths(capturedOptions)).toEqual({
      transports: 1,
      connectionEncrypters: 1,
      streamMuxers: 1,
      peerDiscovery: 1,
    });
    expect(
      Object.keys((capturedOptions as { services: object }).services),
    ).toEqual(["identify", "pubsub"]);
    expect(handled[0]!.protocol).toBe(protocolId);
    expect(handled[0]!.options).toMatchObject({
      maxInboundStreams: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
      maxOutboundStreams: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
      runOnLimitedConnection: false,
    });
    expect(handler).toHaveBeenCalledWith(
      expect.objectContaining({
        protocolId,
        protocolName: DaRequestResponseProtocol.payloadByHeader,
        remotePeerId: PEER_ID_A,
      }),
    );
    expect(subscribed).toEqual(
      createDaTopicAllowlist(DEPLOYMENT_FINGERPRINT).topicIds,
    );

    await service.stop();

    expect(unsubscribed).toEqual(subscribed);
    expect(unhandled).toEqual([protocolId]);
    expect(runtime.stop).toHaveBeenCalledOnce();
    expect(service.isStarted()).toBe(false);
  });

  it("dispatches conflicts only from strictly signed authenticated gossip messages", async () => {
    const config = libp2pConfig();
    const conflictHandler = vi.fn();
    const gossipErrors: unknown[] = [];
    let messageListener: ((event: Event) => void) | undefined;
    const removeEventListener = vi.fn();
    const runtime: DaLibp2pRuntimeNode = {
      services: {
        pubsub: {
          publish: vi.fn(),
          subscribe: vi.fn(),
          unsubscribe: vi.fn(),
          addEventListener: vi.fn((_type, listener) => {
            messageListener = listener;
          }),
          removeEventListener,
        },
      },
      start: vi.fn(),
      stop: vi.fn(),
      handle: vi.fn(),
      unhandle: vi.fn(),
    };
    const service = new DaLibp2pNode({
      config,
      gossipHandlers: new Map([[DaGossipTopic.conflicts, conflictHandler]]),
      onGossipMessageError: (error) => gossipErrors.push(error),
      libp2pFactory: async () => runtime,
    });
    await service.start();
    if (messageListener === undefined) {
      throw new Error("missing gossip message listener");
    }
    const topicId = daGossipTopic(
      DEPLOYMENT_FINGERPRINT,
      DaGossipTopic.conflicts,
    );
    messageListener({
      detail: {
        type: "signed",
        from: peerId(PEER_ID_A),
        topic: topicId,
        data: Buffer.from("conflict"),
      },
    } as CustomEvent);
    await vi.waitFor(() => {
      expect(conflictHandler).toHaveBeenCalledWith({
        topicId,
        topicName: DaGossipTopic.conflicts,
        data: Buffer.from("conflict"),
        remotePeerId: PEER_ID_A,
      });
    });

    messageListener({
      detail: {
        type: "unsigned",
        topic: topicId,
        data: Buffer.from("forged"),
      },
    } as CustomEvent);
    await vi.waitFor(() => {
      expect(gossipErrors).toHaveLength(1);
    });
    expect(conflictHandler).toHaveBeenCalledOnce();

    await service.stop();
    expect(removeEventListener).toHaveBeenCalledWith(
      "message",
      expect.any(Function),
    );
  });
});

const collect = async (
  frames: AsyncIterable<Buffer>,
): Promise<readonly Buffer[]> => {
  const output: Buffer[] = [];
  for await (const frame of frames) {
    output.push(frame);
  }
  return output;
};

const peerId = (value: string): never =>
  ({
    toString: () => value,
  }) as never;

const stackLengths = (
  options: unknown,
): {
  readonly transports: number;
  readonly connectionEncrypters: number;
  readonly streamMuxers: number;
  readonly peerDiscovery: number;
} => {
  const candidate = options as {
    readonly transports?: readonly unknown[];
    readonly connectionEncrypters?: readonly unknown[];
    readonly streamMuxers?: readonly unknown[];
    readonly peerDiscovery?: readonly unknown[];
  };
  return {
    transports: candidate.transports?.length ?? 0,
    connectionEncrypters: candidate.connectionEncrypters?.length ?? 0,
    streamMuxers: candidate.streamMuxers?.length ?? 0,
    peerDiscovery: candidate.peerDiscovery?.length ?? 0,
  };
};

const libp2pConfig = (): Libp2pDaTransportConfig => ({
  kind: "libp2p",
  deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
  noHttpDaTransport: true,
  threshold: 1,
  listenMultiaddrs: ["/ip4/0.0.0.0/tcp/0"],
  announceMultiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${PEER_ID_A}`],
  bootstrapMultiaddrs: [`/dns4/bootstrap.example/tcp/4001/p2p/${PEER_ID_B}`],
  gossip: {
    strictSign: true,
    emitSelf: false,
    allowedTopicsOnly: true,
    maxGossipMessageBytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
  },
  limits: {
    maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    maxInlineResponseBytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
    maxChunkBytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
    maxStreamsPerPeer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
    requestTimeoutMs: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
  },
  retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
  peers: [
    {
      signerIndex: 0,
      daVkey: "01".repeat(32),
      peerId: PEER_ID_A,
      multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${PEER_ID_A}`],
      roles: ["committee", "retrieval"],
    },
  ],
});
