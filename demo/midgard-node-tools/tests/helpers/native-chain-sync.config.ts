import { createHash } from "node:crypto";

import {
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "midgard-watcher";

export const NODE_CONFIG_PATH = "/etc/cardano/node-config.json";

export const GENESIS_CONFIG_PATH = "/etc/cardano/shelley-genesis.json";

export const NODE_CONFIG_BYTES = new TextEncoder().encode(
  JSON.stringify({ ShelleyGenesisFile: GENESIS_CONFIG_PATH }),
);

export const GENESIS_CONFIG_BYTES = new TextEncoder().encode(
  JSON.stringify({ networkMagic: 1 }),
);

export const GENESIS = createHash("sha256")
  .update(GENESIS_CONFIG_BYTES)
  .digest("hex");

export const INTERSECTION = Object.freeze({
  blockHash: "aa".repeat(32),
  kind: "point" as const,
  slot: "100",
});

export const config = (): WatcherConfig =>
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
      peers: Object.freeze([]),
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

export const readIdentityFixture = async (
  path: string,
): Promise<Uint8Array> => {
  if (path === NODE_CONFIG_PATH) return NODE_CONFIG_BYTES;
  if (path === GENESIS_CONFIG_PATH) return GENESIS_CONFIG_BYTES;
  throw new Error("unexpected native identity fixture path");
};

export const waitFor = async (predicate: () => boolean): Promise<void> => {
  const deadline = Date.now() + 2_000;
  while (!predicate()) {
    if (Date.now() >= deadline) throw new Error("native fixture timed out");
    await new Promise<void>((resolve) => setTimeout(resolve, 5));
  }
};
