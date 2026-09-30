import "./libp2p-runtime.public-retained-da-listener.js";

import {
  DA_TRANSPORT_LIMITS,
  DaGossipTopic,
  daGossipTopic,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it, vi } from "vitest";

import {
  DaLibp2pNode,
  type DaLibp2pRuntimeNode,
} from "../src/da/libp2p/DaLibp2pNode.js";
import { createDaTopicAllowlist } from "../src/da/libp2p/DaTopics.js";
import {
  DEPLOYMENT_FINGERPRINT,
  libp2pConfig,
  PEER_ID_A,
  peerId,
} from "./libp2p-runtime.da-libp2p-stream-framing.js";

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
      maxInboundStreams: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      maxOutboundStreams: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
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
