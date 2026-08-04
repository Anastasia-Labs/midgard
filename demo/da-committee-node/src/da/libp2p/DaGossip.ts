import type { DaGossipTopic as DaGossipTopicName } from "@al-ft/midgard-core/da-transport";

import type { Libp2pDaTransportConfig } from "../../config.js";
import type { DaTopicAllowlist } from "./DaTopics.js";

export type DaPubsubService = {
  publish(topic: string, data: Uint8Array): Promise<unknown>;
  subscribe(topic: string): Promise<unknown> | unknown;
  unsubscribe?(topic: string): Promise<unknown> | unknown;
};

export class DaGossip {
  private readonly pubsub: DaPubsubService;
  private readonly topics: DaTopicAllowlist;
  private readonly maxMessageBytes: number;

  constructor({
    pubsub,
    topics,
    config,
  }: {
    readonly pubsub: DaPubsubService;
    readonly topics: DaTopicAllowlist;
    readonly config: Pick<Libp2pDaTransportConfig, "gossip">;
  }) {
    this.pubsub = pubsub;
    this.topics = topics;
    this.maxMessageBytes = config.gossip.maxGossipMessageBytes;
  }

  async subscribeAllowedTopics(): Promise<void> {
    for (const topic of this.topics.topicIds) {
      await this.pubsub.subscribe(topic);
    }
  }

  async unsubscribeAllowedTopics(): Promise<void> {
    for (const topic of this.topics.topicIds) {
      await this.pubsub.unsubscribe?.(topic);
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
