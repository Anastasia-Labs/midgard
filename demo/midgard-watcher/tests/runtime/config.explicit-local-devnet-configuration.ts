import { describe, expect, it } from "vitest";

import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
  WatcherConfigError,
  type WatcherConfigErrorCode,
} from "../../src/runtime/config.js";

export const PEER_A =
  "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345";

export const OPERATOR_ID_A = "11".repeat(32);

const OPERATOR_ID_B = "22".repeat(32);

const GENESIS_ID = "33".repeat(32);

export const validConfig = () => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: OPERATOR_ID_A,
          endpoint: "https://cardano-a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: OPERATOR_ID_B,
          endpoint: "https://cardano-b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 15,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 15,
      },
    },
  },
  da: {
    peers: [{ identity: "da-peer-a", multiaddr: PEER_A }],
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
});

export const validLocalNodeConfig = () => {
  const common = validConfig();
  return {
    ...common,
    l1: {
      ...common.l1,
      source: {
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: "/etc/cardano/node-config.json",
          genesisConfigPath: "/etc/cardano/shelley-genesis.json",
          genesisIdentitySha256: GENESIS_ID,
        },
        queryServices: [
          {
            kind: "ogmios",
            identity: "local-ogmios",
            endpoint: "ws://127.0.0.1:1337",
          },
          {
            kind: "kupo",
            identity: "local-kupo",
            endpoint: "http://127.0.0.1:1442",
          },
          {
            kind: "db_sync",
            identity: "local-db-sync",
            endpoint: "postgresql://127.0.0.1:5432/cexplorer",
          },
        ],
      },
    },
  };
};

describe("explicit local devnet configuration", () => {
  const customNetwork = {
    networkMagic: 424242,
    slotConfig: { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 },
  };

  it("admits a custom chain only with an explicit identity and clock", () => {
    const config = parseWatcherConfig({
      ...validLocalNodeConfig(),
      targetNetwork: "Custom",
      customNetwork,
    });
    expect(config).toMatchObject({ targetNetwork: "Custom", customNetwork });
    expect(() =>
      parseWatcherConfig({
        ...validLocalNodeConfig(),
        targetNetwork: "Custom",
      }),
    ).toThrow();
  });

  it("admits direct local DA transport only for an explicit custom devnet", () => {
    const input = validLocalNodeConfig();
    input.da.peers[0]!.multiaddr =
      "/ip4/127.0.0.1/tcp/4141/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
    expect(
      parseWatcherConfig({ ...input, targetNetwork: "Custom", customNetwork })
        .da.peers[0]!.multiaddr,
    ).toBe(input.da.peers[0]!.multiaddr);
    expect(() => parseWatcherConfig(input)).toThrow();
    input.da.peers[0]!.multiaddr =
      "/ip4/999.0.0.1/tcp/4141/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
    expect(() =>
      parseWatcherConfig({ ...input, targetNetwork: "Custom", customNetwork }),
    ).toThrow();
  });

  it("refuses custom metadata on a named network, public magic and external authority", () => {
    expect(() =>
      parseWatcherConfig({ ...validLocalNodeConfig(), customNetwork }),
    ).toThrow();
    for (const networkMagic of [1, 2, 764824073, -1, 2 ** 32]) {
      expect(() =>
        parseWatcherConfig({
          ...validLocalNodeConfig(),
          targetNetwork: "Custom",
          customNetwork: { ...customNetwork, networkMagic },
        }),
      ).toThrow();
    }
    expect(() =>
      parseWatcherConfig({
        ...validConfig(),
        targetNetwork: "Custom",
        customNetwork,
      }),
    ).toThrow();
  });
});

export const rejected = (
  action: () => unknown,
  code: WatcherConfigErrorCode,
  path?: string,
): WatcherConfigError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherConfigError);
    const configError = error as WatcherConfigError;
    expect(configError.code).toBe(code);
    if (path !== undefined) {
      expect(configError.path).toBe(path);
    }
    return configError;
  }
  throw new Error("Expected watcher configuration rejection");
};
