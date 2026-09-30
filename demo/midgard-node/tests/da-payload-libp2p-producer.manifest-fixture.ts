import { createServer, type Server } from "node:net";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import {
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";

import {
  createDaLibp2pRetainedPayloadRequestHandlers,
  type DaProducerStream,
  parseDaProducerPublicationManifest,
} from "../src/da/libp2p-producer.js";
import { DaPayloadsDB } from "../src/database/index.js";

export const PEER_A = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

export const PEER_B = "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47";

export const PEER_C = "12D3KooWKf1kXPQFRZ6SR6WQF1Z7gqDRUjUe7S4hSm8LRmSk5kvA";

// A dedicated, non-committee Noise identity for the public retained-DA
// plane, as parseDaLibp2pRuntimeManifest requires.
const PEER_RETAINED = "12D3KooWQYV9dGMFoRzNStwpXztXaBUjtPqi6aU76ZgUriHhKust";

export const DEPLOYMENT = "ab".repeat(32);

export const HEADER_HASH = Buffer.alloc(28, 0x02);

export const PAYLOAD_CBOR = Buffer.from("d87980", "hex");

export const PAYLOAD_HASH = computeDaSha256Hash(PAYLOAD_CBOR);

export const PRODUCER_PRIVATE_KEY_SOURCE = `seed:${"00".repeat(31)}01`;

export const manifestFixture = (): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  network: "Preview",
  deployment: {
    fingerprint: DEPLOYMENT.toUpperCase(),
    contract_deployment_manifest_id: DEPLOYMENT,
    contract_deployment_info_sha256: "cd".repeat(32),
    identity_source: "contract_deployment_manifest_id",
  },
  runtime_topology: {
    target: "producer",
    profile: "public",
    producer_peer_id: PEER_C,
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
    announce_multiaddrs: [`/dns4/producer.example/tcp/4001/p2p/${PEER_C}`],
    bootstrap_multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${PEER_A}`],
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
    },
    limits: {
      max_payload_bytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      max_inline_response_bytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      max_chunk_bytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
      max_streams_per_peer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
    retention_days: DA_TRANSPORT_LIMITS.minimumRetentionDays,
  },
  public_retained_da: {
    profile: "public-retained-da-v1",
    access_policy: "any_noise_authenticated_peer",
    peer_id: PEER_RETAINED,
    listen_multiaddrs: ["/ip4/127.0.0.1/tcp/0"],
    announce_multiaddrs: [`/dns4/public.example/tcp/4003/p2p/${PEER_RETAINED}`],
    protocols: [
      "capabilities",
      "payload-by-header",
      "payload-chunk",
      "metadata-by-header",
      "proof-bundle-by-header",
      "trace-step-by-index",
      "event-to-step-by-event",
    ],
    limits: {
      max_streams_per_peer: 4,
      max_inflight_requests: 32,
      max_inflight_requests_per_peer: 2,
      max_inflight_proof_requests: 1,
      request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  },
  da_committee: {
    threshold: 2,
    members: [
      {
        signer_index: 0,
        da_vkey: "01".repeat(32),
        peer_id: PEER_A,
        multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${PEER_A}`],
        roles: ["committee", "retrieval"],
      },
      {
        signer_index: 1,
        da_vkey: "02".repeat(32),
        peer_id: PEER_B,
        multiaddrs: [`/dns4/da-b.example/tcp/4001/p2p/${PEER_B}`],
        roles: ["committee"],
      },
      {
        signer_index: 2,
        da_vkey: "03".repeat(32),
        peer_id: PEER_C,
        multiaddrs: [`/dns4/watcher.example/tcp/4001/p2p/${PEER_C}`],
        roles: ["watcher"],
      },
    ],
  },
});

export const runtimeManifestFixture = (
  producerPeerId: string,
  listenPort: number,
): Record<string, unknown> => ({
  ...manifestFixture(),
  da_transport: {
    ...(manifestFixture().da_transport as Record<string, unknown>),
    listen_multiaddrs: [`/ip4/127.0.0.1/tcp/${listenPort.toString()}`],
    announce_multiaddrs: [
      `/ip4/127.0.0.1/tcp/${listenPort.toString()}/p2p/${producerPeerId}`,
    ],
    bootstrap_multiaddrs: [],
  },
});

export const parseManifestFixture = () =>
  parseDaProducerPublicationManifest(manifestFixture(), {
    DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
  });

export const parseThreeCommitteePeerManifest = () => {
  const fixture = manifestFixture();
  const committee = fixture.da_committee as Record<string, unknown>;
  const members = committee.members as Record<string, unknown>[];
  members[2] = { ...members[2], roles: ["committee", "watcher"] };
  const parsed = parseDaProducerPublicationManifest(fixture, {
    DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
  });
  return parsed;
};

export const listenOnLoopback = (): Promise<Server> =>
  new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      server.off("error", reject);
      resolve(server);
    });
  });

export const closeServer = (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    server.close((error) => (error === undefined ? resolve() : reject(error)));
  });

export const serverPort = (server: Server): number => {
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("expected TCP server address");
  }
  return address.port;
};

export const insertFixture = (): DaPayloadsDB.InsertInput => ({
  [DaPayloadsDB.Columns.HEADER_HASH]: HEADER_HASH,
  [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
  [DaPayloadsDB.Columns.VERSION]: 1,
  [DaPayloadsDB.Columns.PAYLOAD_CBOR]: PAYLOAD_CBOR,
  [DaPayloadsDB.Columns.PAYLOAD_SHA256]: PAYLOAD_HASH,
  [DaPayloadsDB.Columns.UTXOS_ROOT]: "10".repeat(32),
  [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]: "11".repeat(32),
  [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: "12".repeat(32),
  [DaPayloadsDB.Columns.DEPOSITS_ROOT]: "13".repeat(32),
  [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: "14".repeat(32),
  [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: "15".repeat(32),
  [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: "16".repeat(32),
  [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: EMPTY_MERKLE_TREE_ROOT,
  [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
  [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
  [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
  [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
  [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
  [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
  [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
  [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date("2026-06-21T00:00:00Z"),
  [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date("2026-06-21T00:00:01Z"),
});

export const rowFixture = (): DaPayloadsDB.Row => ({
  ...insertFixture(),
  [DaPayloadsDB.Columns.CREATED_AT]: new Date("2026-06-21T00:00:02Z"),
  [DaPayloadsDB.Columns.UPDATED_AT]: new Date("2026-06-21T00:00:03Z"),
});

export const callRetainedPayloadHandler = async ({
  handlers,
  protocol,
  request,
}: {
  readonly handlers: ReturnType<
    typeof createDaLibp2pRetainedPayloadRequestHandlers
  >;
  readonly protocol: DaRequestResponseProtocol;
  readonly request: Uint8Array;
}): Promise<Buffer> => {
  const protocolId = daRequestResponseProtocolId(DEPLOYMENT, protocol);
  const handler = handlers.get(protocolId);
  if (handler === undefined) {
    throw new Error(`missing handler for ${protocol}`);
  }
  const requestFrame = encodeDaStreamFrame(request, {
    maxFrameBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
  });
  let responseFrame: Buffer | undefined;
  const stream: DaProducerStream = {
    async *[Symbol.asyncIterator]() {
      yield requestFrame;
    },
    send: (data) => {
      responseFrame = Buffer.from(data);
      return true;
    },
    close: async () => {},
  };

  await handler(stream);
  if (responseFrame === undefined) {
    throw new Error("retained payload handler did not send a response");
  }
  return readSingleDaStreamFrame([responseFrame]);
};
