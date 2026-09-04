import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { withDaRequestDeadline } from "@al-ft/midgard-core/da-request-deadline";
import type { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import { bootstrap } from "@libp2p/bootstrap";
import { gossipsub, StrictSign } from "@libp2p/gossipsub";
import { identify } from "@libp2p/identify";
import { tcp } from "@libp2p/tcp";
import { multiaddr } from "@multiformats/multiaddr";
import { createLibp2p, type Libp2pOptions } from "libp2p";

import type { Libp2pDaTransportConfig } from "../../config.js";
import { createDaConnectionGater } from "./DaConnectionGater.js";
import {
  DaGossip,
  type DaGossipMessageHandler,
  type DaPubsubService,
} from "./DaGossip.js";
import { DaPeerRegistry, type DaPeerRegistryEntry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";
import {
  type DaStreamChunk,
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "./DaStreamCodec.js";
import { createDaTopicAllowlist } from "./DaTopics.js";

export type DaLibp2pStream = AsyncIterable<DaStreamChunk> & {
  send(data: Uint8Array): boolean;
  onDrain?(): Promise<void>;
  close?(): Promise<void> | void;
  abort?(error: Error): void;
};

export type DaLibp2pConnection = {
  readonly remotePeer?: { toString(): string };
};

export type DaLibp2pStreamHandlerContext = {
  readonly protocolId: string;
  readonly protocolName: string;
  readonly stream: DaLibp2pStream;
  readonly connection: DaLibp2pConnection;
  readonly remotePeerId?: string;
};

export type DaLibp2pStreamHandler = (
  context: DaLibp2pStreamHandlerContext,
) => Promise<void> | void;

export type DaLibp2pRuntimeNode = {
  readonly services?: { readonly pubsub?: DaPubsubService };
  start(): Promise<void> | void;
  stop(): Promise<void> | void;
  handle(
    protocol: string,
    handler: (stream: unknown, connection: unknown) => Promise<void> | void,
    options?: {
      readonly maxInboundStreams?: number;
      readonly maxOutboundStreams?: number;
      readonly runOnLimitedConnection?: boolean;
    },
  ): Promise<void> | void;
  unhandle(protocol: string): Promise<void> | void;
  getProtocols?(): readonly string[];
  getMultiaddrs?(): readonly unknown[];
  dialProtocol?(
    peer: unknown,
    protocol: string,
    options?: { readonly signal?: AbortSignal },
  ): Promise<DaLibp2pStream>;
};

export type DaLibp2pFactory = (
  options: Libp2pOptions,
) => Promise<DaLibp2pRuntimeNode>;

export type DaLibp2pNodeOptions = {
  readonly config: Libp2pDaTransportConfig;
  readonly registry?: DaPeerRegistry;
  readonly privateKeySource?: string;
  readonly requestHandlers?: ReadonlyMap<string, DaLibp2pStreamHandler>;
  readonly gossipHandlers?: ReadonlyMap<
    DaGossipTopic | string,
    DaGossipMessageHandler
  >;
  readonly onGossipMessageError?: (error: unknown) => void;
  readonly libp2pFactory?: DaLibp2pFactory;
};

export class DaLibp2pNode {
  readonly config: Libp2pDaTransportConfig;
  readonly registry: DaPeerRegistry;
  readonly protocols;
  readonly topics;

  private readonly requestHandlers = new Map<string, DaLibp2pStreamHandler>();
  private readonly gossipHandlers = new Map<string, DaGossipMessageHandler>();
  private readonly libp2pFactory: DaLibp2pFactory;
  private readonly privateKeySource?: string;
  private readonly onGossipMessageError?: (error: unknown) => void;
  private node?: DaLibp2pRuntimeNode;
  private gossip?: DaGossip;
  private started = false;

  constructor(options: DaLibp2pNodeOptions) {
    this.config = options.config;
    this.registry =
      options.registry ?? DaPeerRegistry.fromConfig(options.config);
    this.protocols = createDaProtocolAllowlist(
      options.config.deploymentFingerprint,
    );
    this.topics = createDaTopicAllowlist(options.config.deploymentFingerprint);
    this.libp2pFactory = options.libp2pFactory ?? defaultDaLibp2pFactory;
    this.privateKeySource = options.privateKeySource;
    this.onGossipMessageError = options.onGossipMessageError;
    for (const [protocolId, handler] of options.requestHandlers ?? []) {
      this.setRequestHandler(protocolId, handler);
    }
    for (const [topic, handler] of options.gossipHandlers ?? []) {
      this.setGossipHandler(topic, handler);
    }
  }

  isStarted(): boolean {
    return this.started;
  }

  setRequestHandler(protocolId: string, handler: DaLibp2pStreamHandler): void {
    this.protocols.requireProtocolId(protocolId);
    this.requestHandlers.set(protocolId, handler);
  }

  setGossipHandler(
    topic: DaGossipTopic | string,
    handler: DaGossipMessageHandler,
  ): void {
    if (this.started) {
      throw new Error("cannot change DA gossip handlers after startup");
    }
    const topicId = this.topics.hasTopicName(topic)
      ? this.topics.topicIdByName.get(topic)
      : topic;
    if (topicId === undefined) {
      throw new Error(`unsupported DA libp2p gossip topic ${topic}`);
    }
    this.topics.requireTopicId(topicId);
    this.gossipHandlers.set(topicId, handler);
  }

  async start(): Promise<void> {
    if (this.started) {
      return;
    }
    const identity =
      this.config.kind === "libp2p" && this.privateKeySource !== undefined
        ? await loadDaLibp2pIdentity(this.privateKeySource)
        : undefined;
    if (identity !== undefined) {
      this.registry.requireKnownPeer(identity.peerId);
    }
    const node = await this.libp2pFactory(
      createDaLibp2pOptions({
        config: this.config,
        registry: this.registry,
        privateKey: identity?.privateKey,
      }),
    );
    this.node = node;
    for (const [protocolId, handler] of this.requestHandlers) {
      const protocolName = this.protocols.requireProtocolId(protocolId);
      await node.handle(
        protocolId,
        async (stream, connection) => {
          const typedConnection = connection as DaLibp2pConnection;
          await handler({
            protocolId,
            protocolName,
            stream: stream as DaLibp2pStream,
            connection: typedConnection,
            remotePeerId: typedConnection.remotePeer?.toString(),
          });
        },
        {
          maxInboundStreams: this.config.limits.maxStreamsPerPeer,
          maxOutboundStreams: this.config.limits.maxStreamsPerPeer,
          runOnLimitedConnection: false,
        },
      );
    }
    await node.start();
    const pubsub = node.services?.pubsub;
    if (pubsub !== undefined) {
      this.gossip = new DaGossip({
        pubsub,
        topics: this.topics,
        config: this.config,
        messageHandlers: this.gossipHandlers,
        onMessageError: this.onGossipMessageError,
      });
      await this.gossip.subscribeAllowedTopics();
    }
    this.started = true;
  }

  async stop(): Promise<void> {
    if (!this.started || this.node === undefined) {
      return;
    }
    await this.gossip?.unsubscribeAllowedTopics();
    for (const protocolId of this.requestHandlers.keys()) {
      await this.node.unhandle(protocolId);
    }
    await this.node.stop();
    this.gossip = undefined;
    this.node = undefined;
    this.started = false;
  }

  async request({
    peer,
    protocolId,
    payload,
    timeoutMs = this.config.limits.requestTimeoutMs,
  }: {
    readonly peer: DaPeerRegistryEntry;
    readonly protocolId: string;
    readonly payload: Uint8Array;
    readonly timeoutMs?: number;
  }): Promise<Buffer> {
    this.protocols.requireProtocolId(protocolId);
    if (!this.started || this.node === undefined) {
      throw new Error("DA libp2p node is not started");
    }
    if (this.node.dialProtocol === undefined) {
      throw new Error("DA libp2p node does not support request/response dials");
    }
    const node = this.node;
    return withDaRequestDeadline({
      timeoutMs,
      open: (signal) =>
        node.dialProtocol!(
          peer.multiaddrs.map((addr) => multiaddr(addr)),
          protocolId,
          { signal },
        ),
      run: async (stream) => {
        await writeDaStreamFrame(stream, payload, {
          maxFrameBytes: this.config.limits.maxPayloadBytes,
        });
        await stream.close?.();
        return readSingleDaStreamFrame(stream, {
          maxFrameBytes: this.config.limits.maxPayloadBytes,
        });
      },
      abort: (stream, error) => stream.abort?.(error),
    });
  }

  async publishGossip(
    topic: DaGossipTopic | string,
    data: Uint8Array,
  ): Promise<void> {
    if (!this.started || this.gossip === undefined) {
      throw new Error("DA libp2p gossip is not started");
    }
    await this.gossip.publish(topic, data);
  }
}

export const createDaLibp2pOptions = ({
  config,
  registry = DaPeerRegistry.fromConfig(config),
  privateKey,
}: {
  readonly config: Libp2pDaTransportConfig;
  readonly registry?: DaPeerRegistry;
  readonly privateKey?: Libp2pOptions["privateKey"];
}): Libp2pOptions => ({
  start: false,
  ...(privateKey === undefined ? {} : { privateKey }),
  addresses: {
    listen: [...config.listenMultiaddrs],
    announce: [...config.announceMultiaddrs],
  },
  transports: [tcp()],
  connectionEncrypters: [noise()],
  streamMuxers: [
    yamux({
      maxInboundStreams: config.limits.maxStreamsPerPeer,
      maxOutboundStreams: config.limits.maxStreamsPerPeer,
      maxMessageSize: config.limits.maxChunkBytes,
    }),
  ],
  peerDiscovery: [
    bootstrap({
      list: [...config.bootstrapMultiaddrs],
    }),
  ],
  connectionGater: createDaConnectionGater(registry),
  services: {
    identify: identify(),
    pubsub: gossipsub({
      globalSignaturePolicy: StrictSign,
      emitSelf: false,
      allowPublishToZeroTopicPeers: false,
      allowedTopics: new Set(
        createDaTopicAllowlist(config.deploymentFingerprint).topicIds,
      ),
      maxOutboundBufferSize: config.gossip.maxGossipMessageBytes,
    }),
  },
});

const defaultDaLibp2pFactory: DaLibp2pFactory = async (
  options: Libp2pOptions,
): Promise<DaLibp2pRuntimeNode> =>
  createLibp2p(options) as Promise<DaLibp2pRuntimeNode>;
