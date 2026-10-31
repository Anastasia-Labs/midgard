import type { DaGossipTopic as DaGossipTopicName } from "@al-ft/midgard-core/da-transport";

import type { Libp2pDaTransportConfig } from "../../config.js";
import type { DaTopicAllowlist } from "./DaTopics.js";

export type DaPubsubService = {
  publish(topic: string, data: Uint8Array): Promise<unknown>;
  subscribe(topic: string): Promise<unknown> | unknown;
  unsubscribe?(topic: string): Promise<unknown> | unknown;
  addEventListener?(type: "message", listener: (event: Event) => void): void;
  removeEventListener?(type: "message", listener: (event: Event) => void): void;
};

export type DaSignedPubsubMessage = {
  readonly type: "signed";
  readonly from: { toString(): string };
  readonly topic: string;
  readonly data: Uint8Array;
};

export type DaUnsignedPubsubMessage = {
  readonly type: "unsigned";
  readonly topic: string;
  readonly data: Uint8Array;
};

export type DaPubsubMessage = DaSignedPubsubMessage | DaUnsignedPubsubMessage;

export type DaGossipMessageHandlerContext = {
  readonly topicId: string;
  readonly topicName: DaGossipTopicName;
  readonly data: Buffer;
  readonly remotePeerId: string;
};

export type DaGossipMessageHandler = (
  context: DaGossipMessageHandlerContext,
) => Promise<void> | void;

export class DaGossip {
  private readonly pubsub: DaPubsubService;
  private readonly topics: DaTopicAllowlist;
  private readonly maxMessageBytes: number;
  private readonly messageHandlers: ReadonlyMap<string, DaGossipMessageHandler>;
  private readonly onMessageError?: (error: unknown) => void;
  private listening = false;
  private readonly messageListener = (event: Event): void => {
    const detail = (event as CustomEvent<DaPubsubMessage>).detail;
    void this.handleInboundMessage(detail).catch((error: unknown) => {
      this.onMessageError?.(error);
    });
  };

  constructor({
    pubsub,
    topics,
    config,
    messageHandlers = new Map(),
    onMessageError,
  }: {
    readonly pubsub: DaPubsubService;
    readonly topics: DaTopicAllowlist;
    readonly config: Pick<Libp2pDaTransportConfig, "gossip">;
    readonly messageHandlers?: ReadonlyMap<string, DaGossipMessageHandler>;
    readonly onMessageError?: (error: unknown) => void;
  }) {
    this.pubsub = pubsub;
    this.topics = topics;
    this.maxMessageBytes = config.gossip.maxGossipMessageBytes;
    this.messageHandlers = messageHandlers;
    this.onMessageError = onMessageError;
  }

  async subscribeAllowedTopics(): Promise<void> {
    if (this.messageHandlers.size > 0) {
      if (this.pubsub.addEventListener === undefined) {
        throw new Error(
          "DA libp2p pubsub does not expose authenticated message events",
        );
      }
      this.pubsub.addEventListener("message", this.messageListener);
      this.listening = true;
    }
    for (const topic of this.topics.topicIds) {
      await this.pubsub.subscribe(topic);
    }
  }

  async unsubscribeAllowedTopics(): Promise<void> {
    for (const topic of this.topics.topicIds) {
      await this.pubsub.unsubscribe?.(topic);
    }
    if (this.listening) {
      this.pubsub.removeEventListener?.("message", this.messageListener);
      this.listening = false;
    }
  }

  async publish(
    topic: DaGossipTopicName | string,
    data: Uint8Array,
  ): Promise<void> {
    const topicId = this.topicId(topic);
    if (data.byteLength > this.maxMessageBytes) {
      throw new Error(
        `DA libp2p gossip message exceeds ${this.maxMessageBytes.toString()} bytes`,
      );
    }
    await this.pubsub.publish(topicId, data);
  }

  async handleInboundMessage(message: DaPubsubMessage): Promise<boolean> {
    const topicName = this.topics.requireTopicId(message.topic);
    const handler = this.messageHandlers.get(message.topic);
    if (handler === undefined) {
      return false;
    }
    if (message.type !== "signed") {
      throw new Error("DA libp2p gossip message must be strictly signed");
    }
    const remotePeerId = message.from.toString();
    if (remotePeerId.length === 0) {
      throw new Error("DA libp2p gossip message has no authenticated peer");
    }
    if (message.data.byteLength > this.maxMessageBytes) {
      throw new Error(
        `DA libp2p gossip message exceeds ${this.maxMessageBytes.toString()} bytes`,
      );
    }
    await handler({
      topicId: message.topic,
      topicName,
      data: Buffer.from(message.data),
      remotePeerId,
    });
    return true;
  }

  topicId(topic: DaGossipTopicName | string): string {
    if (this.topics.hasTopicName(topic)) {
      const topicId = this.topics.topicIdByName.get(topic);
      if (topicId !== undefined) {
        return topicId;
      }
    }
    this.topics.requireTopicId(topic);
    return topic;
  }
}
