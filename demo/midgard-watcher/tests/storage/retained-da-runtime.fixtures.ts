import { type Mock, vi } from "vitest";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { WatcherPublicDaRequest } from "../../src/storage/public-da-client.js";
import {
  encodeWatcherPublicDaFrame,
  WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";

export const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";

export const rawConfig = (
  multiaddr = `/dns4/da-a.example/tcp/443/p2p/${PEER_ID}`,
) =>
  ({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: "/etc/cardano/node-config.json",
          genesisIdentitySha256: "33".repeat(32),
        },
      },
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
      finality: { depth: 30 },
    },
    da: {
      peers: [{ identity: "da-peer-a", multiaddr }],
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
    },
    storage: {
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      },
    },
    proverWallet: {
      keySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      },
    },
    deadlines: {
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    },
  }) as const;

export const transportFactory = () => {
  const request = vi.fn(
    async (_request: WatcherPublicDaRequest) => new Uint8Array([0xf6]),
  );
  const stop = vi.fn(async () => undefined);
  const transport = Object.create(
    WatcherPublicDaLibp2pTransport.prototype,
  ) as WatcherPublicDaLibp2pTransport;
  Object.defineProperties(transport, {
    request: { value: request },
    stop: { value: stop },
  });
  return { request, stop, factory: vi.fn(async () => transport) };
};

/**
 * The real libp2p transport over a fake node that answers every dial with
 * `response` and reports `peerId` as the authenticated remote peer until
 * `changeRemotePeer` swaps it.
 */
export const libp2pTransportFixture = (
  peerId: string,
  response = Buffer.from([0xf6]),
): Readonly<{
  transport: WatcherPublicDaLibp2pTransport;
  dialProtocol: Mock;
  abort: Mock;
  changeRemotePeer: () => void;
}> => {
  let authenticatedPeer = peerId;
  const abort = vi.fn();
  const dialProtocol = vi.fn(async () => ({
    send: () => true,
    close: async () => undefined,
    abort,
    async *[Symbol.asyncIterator]() {
      yield encodeWatcherPublicDaFrame(response);
    },
  }));
  const transport = new WatcherPublicDaLibp2pTransport({
    libp2pFactory: async () => ({
      start: async () => undefined,
      stop: async () => undefined,
      dialProtocol,
      getConnections: () => [
        { remotePeer: { toString: () => authenticatedPeer } },
      ],
    }),
  });
  return {
    transport,
    dialProtocol,
    abort,
    changeRemotePeer: () => {
      authenticatedPeer = PEER_ID;
    },
  };
};
