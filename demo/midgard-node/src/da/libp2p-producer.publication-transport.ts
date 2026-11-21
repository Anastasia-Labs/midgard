import { createDaLibp2pProducerTransport } from "./libp2p-producer.create-da-libp2p-producer-transport.js";
import {
  type DaProducerPublicationManifest,
  type DaProducerTransport,
} from "./libp2p-producer.parse-committee-peers.js";
import { publicationTransportKey } from "./libp2p-producer.publish-da-payload-insert.js";

let cachedPublicationTransport:
  | Promise<{
      readonly key: string;
      readonly transport: DaProducerTransport;
    }>
  | undefined;

export const getPublicationTransport = async (
  manifest: DaProducerPublicationManifest,
): Promise<DaProducerTransport> => {
  const key = publicationTransportKey(manifest);
  const existing = cachedPublicationTransport;
  if (existing !== undefined) {
    const resolved = await existing;
    if (resolved.key === key) {
      return resolved.transport;
    }
    await resolved.transport.close?.();
  }
  const created = createDaLibp2pProducerTransport(manifest, {
    mode: "dial-only",
  }).then((transport) => ({ key, transport }));
  cachedPublicationTransport = created;
  try {
    return (await created).transport;
  } catch (error) {
    if (cachedPublicationTransport === created) {
      cachedPublicationTransport = undefined;
    }
    throw error;
  }
};

export const closeDaLibp2pPublicationTransport = async (): Promise<void> => {
  const cached = cachedPublicationTransport;
  cachedPublicationTransport = undefined;
  const resolved = await cached?.catch(() => undefined);
  await resolved?.transport.close?.();
};

export const getDaPublicationTransportForTest = getPublicationTransport;
