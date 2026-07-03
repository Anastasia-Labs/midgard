import {
  DaGossipTopic,
  type DaGossipTopic as DaGossipTopicName,
  daGossipTopic,
} from "@al-ft/midgard-core/da-transport";

export const DA_GOSSIP_TOPICS = Object.freeze(
  Object.values(DaGossipTopic),
) as readonly DaGossipTopicName[];

export type DaTopicAllowlist = {
  readonly deploymentFingerprint: string;
  readonly topicIds: readonly string[];
  readonly topicIdByName: ReadonlyMap<DaGossipTopicName, string>;
  readonly topicNameById: ReadonlyMap<string, DaGossipTopicName>;
  hasTopicName(topic: string): topic is DaGossipTopicName;
  hasTopicId(topicId: string): boolean;
  requireTopicId(topicId: string): DaGossipTopicName;
};

export const createDaTopicAllowlist = (
  deploymentFingerprint: string,
): DaTopicAllowlist => {
  const topicIdByName = new Map<DaGossipTopicName, string>();
  const topicNameById = new Map<string, DaGossipTopicName>();
  for (const topic of DA_GOSSIP_TOPICS) {
    const topicId = daGossipTopic(deploymentFingerprint, topic);
    topicIdByName.set(topic, topicId);
    topicNameById.set(topicId, topic);
  }
  const topicIds = Object.freeze([...topicNameById.keys()]);
  return {
    deploymentFingerprint,
    topicIds,
    topicIdByName,
    topicNameById,
    hasTopicName(topic): topic is DaGossipTopicName {
      return topicIdByName.has(topic as DaGossipTopicName);
    },
    hasTopicId(topicId): boolean {
      return topicNameById.has(topicId);
    },
    requireTopicId(topicId): DaGossipTopicName {
      const topic = topicNameById.get(topicId);
      if (topic === undefined) {
        throw new Error(`unsupported DA libp2p gossip topic ${topicId}`);
      }
      return topic;
    },
  };
};
