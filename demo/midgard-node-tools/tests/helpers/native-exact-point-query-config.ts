import { createHash } from "node:crypto";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "midgard-watcher";

// A local-node watcher configuration for exact-point query lifecycle tests.
export const GENESIS_BYTES = JSON.stringify({ networkMagic: 1 });
export const GENESIS = createHash("sha256").update(GENESIS_BYTES).digest("hex");
export const exactPointWatcherConfig = (
  NODE_CONFIG_PATH: string,
  GENESIS_CONFIG_PATH: string,
) =>
  Object.freeze({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: Object.freeze({
      source: Object.freeze({
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: Object.freeze({
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: NODE_CONFIG_PATH,
          genesisConfigPath: GENESIS_CONFIG_PATH,
          genesisIdentitySha256: GENESIS,
        }),
      }),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
      finality: Object.freeze({ depth: 30 }),
    }),
    da: Object.freeze({
      peers: Object.freeze([
        {
          identity: "da-peer-a",
          multiaddr:
            "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ]),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
    }),
    storage: Object.freeze({
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      }),
    }),
    proverWallet: Object.freeze({
      keySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      }),
    }),
    deadlines: Object.freeze({
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    }),
  });
