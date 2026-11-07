import { withDaRequestDeadline } from "@al-ft/midgard-core/da-request-deadline";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_PROTOCOL_VERSION,
  daDeploymentFingerprintFromHex,
  DaGossipTopic,
  daGossipTopic,
  DaTransportSigningDomain,
  encodeDaPayloadAnnouncementCbor,
} from "@al-ft/midgard-core/da-transport";

import { DaPayloadsDB } from "../database/index.js";
import {
  createDaLibp2pRetainedPayloadRequestHandlers,
  loadAndValidateProducerIdentity,
} from "./libp2p-producer.create-da-libp2p-retained-payload-request-handlers.js";
import {
  type DaProducerAnnouncementResult,
  type DaProducerPublicationManifest,
  type DaProducerStream,
  type DaProducerTransport,
  type DaProducerTransportOptions,
  type DaRetainedPayloadLookup,
  type DaRetainedPayloadServer,
} from "./libp2p-producer.parse-committee-peers.js";
import {
  loadDaProducerPublicationManifestFromEnv,
  rootSummaryHash,
} from "./libp2p-producer.parse-da-producer-publication-manifest.js";

export const startDaLibp2pRetainedPayloadServerFromEnv = async ({
  retrieveByHeaderHash,
  env = process.env,
}: {
  readonly retrieveByHeaderHash: DaRetainedPayloadLookup;
  readonly env?: NodeJS.ProcessEnv;
}): Promise<DaRetainedPayloadServer> => {
  const manifest = await loadDaProducerPublicationManifestFromEnv(env);
  if (manifest === null) {
    return {
      configured: false,
      reason: "no libp2p DA manifest configured",
    };
  }
  const requestHandlers = createDaLibp2pRetainedPayloadRequestHandlers({
    manifest,
    retrieveByHeaderHash,
  });
  const transport = await createDaLibp2pProducerTransport(manifest, {
    mode: "bind-listen",
    requestHandlers,
  });
  let localPeerId: string;
  try {
    localPeerId = await transport.localPeerId();
  } catch (error) {
    await transport.close?.().catch(() => undefined);
    throw error;
  }
  return {
    configured: true,
    deploymentFingerprint: manifest.deploymentFingerprint,
    localPeerId,
    listenMultiaddrs: manifest.listenMultiaddrs,
    announceMultiaddrs: manifest.announceMultiaddrs,
    close: async () => {
      await transport.close?.();
    },
  };
};

export const publishDaPayloadAnnouncement = async ({
  insert,
  manifest,
  transport,
  announcedAtSlot = 0,
}: {
  readonly insert: DaPayloadsDB.InsertInput;
  readonly manifest: DaProducerPublicationManifest;
  readonly transport: DaProducerTransport;
  readonly announcedAtSlot?: number;
}): Promise<DaProducerAnnouncementResult> => {
  const headerHash = insert[DaPayloadsDB.Columns.HEADER_HASH];
  const payloadBytes = insert[DaPayloadsDB.Columns.PAYLOAD_CBOR];
  const payloadHash = verifyPayloadHash(insert);
  const deploymentFingerprint = daDeploymentFingerprintFromHex(
    manifest.deploymentFingerprint,
  );
  const topic = daGossipTopic(
    manifest.deploymentFingerprint,
    DaGossipTopic.payloadAnnouncements,
  );
  const localPeerId = await transport.localPeerId();
  const announcementWithoutSignature = {
    deploymentFingerprint,
    headerHash,
    payloadHash,
    payloadSchemaVersion: insert[DaPayloadsDB.Columns.VERSION],
    payloadBytes: payloadBytes.length,
    chunkSize: payloadBytes.length,
    chunkCount: 1,
    rootSummaryHash: rootSummaryHash(insert),
    announcedByPeerId: localPeerId,
    announcedAtSlot,
  };
  const signature = Buffer.from(
    await transport.sign(
      payloadAnnouncementSigningPreimage(announcementWithoutSignature),
    ),
  );
  const announcementBytes = encodeDaPayloadAnnouncementCbor({
    ...announcementWithoutSignature,
    signature,
  });
  if (announcementBytes.length > manifest.maxGossipMessageBytes) {
    throw new Error(
      `DA payload announcement is ${announcementBytes.length.toString()} bytes, exceeding max_gossip_message_bytes=${manifest.maxGossipMessageBytes.toString()}`,
    );
  }
  const published = await transport.publish(topic, announcementBytes);
  return {
    topic,
    payloadHash: payloadHash.toString("hex"),
    recipients: published.recipients,
  };
};

export const createDaLibp2pProducerTransport = async (
  manifest: DaProducerPublicationManifest,
  { mode = "bind-listen", requestHandlers }: DaProducerTransportOptions = {},
): Promise<DaProducerTransport> => {
  const [
    { createLibp2p },
    { tcp },
    { noise },
    { yamux },
    { bootstrap },
    { identify },
    { gossipsub, StrictSign },
    { multiaddr },
  ] = await Promise.all([
    import("libp2p"),
    import("@libp2p/tcp"),
    import("@chainsafe/libp2p-noise"),
    import("@chainsafe/libp2p-yamux"),
    import("@libp2p/bootstrap"),
    import("@libp2p/identify"),
    import("@libp2p/gossipsub"),
    import("@multiformats/multiaddr"),
  ]);
  const allowedTopic = daGossipTopic(
    manifest.deploymentFingerprint,
    DaGossipTopic.payloadAnnouncements,
  );
  const identity = await loadAndValidateProducerIdentity(manifest);
  const privateKey = identity.privateKey;
  const localPeerId = identity.peerId;
  const node = await createLibp2p({
    privateKey,
    addresses: {
      listen: mode === "bind-listen" ? [...manifest.listenMultiaddrs] : [],
      announce: mode === "bind-listen" ? [...manifest.announceMultiaddrs] : [],
    },
    transports: [tcp()],
    connectionEncrypters: [noise()],
    streamMuxers: [yamux()],
    peerDiscovery:
      manifest.bootstrapMultiaddrs.length === 0
        ? []
        : [bootstrap({ list: [...manifest.bootstrapMultiaddrs] })],
    services: {
      identify: identify(),
      pubsub: gossipsub({
        globalSignaturePolicy: StrictSign,
        allowPublishToZeroTopicPeers: true,
        allowedTopics: new Set([allowedTopic]),
        maxInboundDataLength: manifest.maxGossipMessageBytes,
      }),
    },
  });
  // createLibp2p has already bound the listen address. Any failure from here
  // on must release it, or every start retry hits EADDRINUSE on our own socket.
  try {
    for (const [protocolId, handler] of requestHandlers ?? []) {
      await node.handle(
        protocolId,
        async (stream) => {
          await handler(stream as DaProducerStream);
        },
        {
          maxInboundStreams: manifest.maxStreamsPerPeer,
          maxOutboundStreams: manifest.maxStreamsPerPeer,
          runOnLimitedConnection: false,
        },
      );
    }
  } catch (error) {
    await Promise.resolve()
      .then(() => node.stop())
      .catch(() => undefined);
    throw error;
  }
  return {
    localPeerId: () => localPeerId,
    sign: (message) => privateKey.sign(message),
    request: async (peer, protocolId, payload, timeoutMs) =>
      withDaRequestDeadline({
        timeoutMs,
        open: (signal) =>
          node.dialProtocol(
            peer.multiaddrs.map((address) => multiaddr(address)),
            protocolId,
            { signal },
          ),
        run: async (stream) => {
          await writeDaStreamFrame(stream, payload, {
            maxFrameBytes: manifest.maxPayloadBytes,
            close: true,
          });
          return readSingleDaStreamFrame(stream, {
            maxFrameBytes: manifest.maxPayloadBytes,
          });
        },
        abort: (stream, error) => stream.abort?.(error),
      }),
    requestFramed: async (peer, protocolId, frame, timeoutMs, maxChunkBytes) =>
      withDaRequestDeadline({
        timeoutMs,
        open: (signal) =>
          node.dialProtocol(
            peer.multiaddrs.map((address) => multiaddr(address)),
            protocolId,
            { signal },
          ),
        run: async (stream) => {
          await writeSharedFrameChunks(stream, frame, maxChunkBytes);
          await stream.close();
          return readSingleDaStreamFrame(stream, {
            maxFrameBytes: manifest.maxPayloadBytes,
          });
        },
        abort: (stream, error) => stream.abort?.(error),
      }),
    publish: async (topic, payload) => {
      const result = await node.services.pubsub.publish(topic, payload);
      return {
        recipients: result.recipients.map((peerId) => peerId.toString()),
      };
    },
    close: async () => {
      for (const protocolId of requestHandlers?.keys() ?? []) {
        await node.unhandle(protocolId);
      }
      await node.stop();
    },
  };
};

const payloadAnnouncementSigningPreimage = (message: {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly payloadSchemaVersion: 1;
  readonly payloadBytes: number;
  readonly chunkSize: number;
  readonly chunkCount: number;
  readonly rootSummaryHash: Buffer;
  readonly announcedByPeerId: string;
  readonly announcedAtSlot: number;
}): Buffer =>
  Buffer.concat([
    Buffer.from(DaTransportSigningDomain.payloadAnnouncement, "utf8"),
    Buffer.from([0]),
    Buffer.from([DA_TRANSPORT_PROTOCOL_VERSION]),
    encodeDaPayloadAnnouncementCbor({
      ...message,
      signature: Buffer.alloc(0),
    }),
  ]);

export const verifyPayloadHash = (insert: DaPayloadsDB.InsertInput): Buffer => {
  const expected = insert[DaPayloadsDB.Columns.PAYLOAD_SHA256];
  const actual = computeDaSha256Hash(insert[DaPayloadsDB.Columns.PAYLOAD_CBOR]);
  if (!expected.equals(actual)) {
    throw new Error(
      `DA payload hash mismatch for ${insert[
        DaPayloadsDB.Columns.HEADER_HASH
      ].toString("hex")}`,
    );
  }
  return expected;
};

export const writeSharedFrameChunks = async (
  stream: DaProducerStream,
  frame: Uint8Array,
  maxChunkBytes: number,
): Promise<void> => {
  if (!Number.isSafeInteger(maxChunkBytes) || maxChunkBytes <= 0) {
    throw new Error("maxChunkBytes must be a positive safe integer");
  }
  for (let offset = 0; offset < frame.length; offset += maxChunkBytes) {
    const accepted = stream.send(
      frame.subarray(offset, Math.min(offset + maxChunkBytes, frame.length)),
    );
    if (!accepted) {
      if (stream.onDrain === undefined) {
        throw new Error("DA stream backpressure requires onDrain support");
      }
      await stream.onDrain();
    }
  }
};
