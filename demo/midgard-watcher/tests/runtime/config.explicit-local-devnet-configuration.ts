import { describe, expect, it } from "vitest";

import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
  WatcherConfigError,
  type WatcherConfigErrorCode,
} from "../../src/runtime/config.js";

export const PEER_A =
  "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345";

const GENESIS_ID = "33".repeat(32);

export const validConfig = () => ({
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
        genesisConfigPath: "/etc/cardano/shelley-genesis.json",
        genesisIdentitySha256: GENESIS_ID,
      },
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 15,
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

describe("explicit local devnet configuration", () => {
  const customNetwork = {
    networkMagic: 424242,
    slotConfig: { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 },
  };

  it("admits a custom chain only with an explicit identity and clock", () => {
    const config = parseWatcherConfig({
      ...validConfig(),
      targetNetwork: "Custom",
      customNetwork,
    });
    expect(config).toMatchObject({ targetNetwork: "Custom", customNetwork });
    expect(() =>
      parseWatcherConfig({
        ...validConfig(),
        targetNetwork: "Custom",
      }),
    ).toThrow();
  });

  it("admits direct local DA transport only for an explicit custom devnet", () => {
    const input = validConfig();
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

  it("refuses custom metadata on a named network and public magic", () => {
    expect(() =>
      parseWatcherConfig({ ...validConfig(), customNetwork }),
    ).toThrow();
    for (const networkMagic of [1, 2, 764824073, -1, 2 ** 32]) {
      expect(() =>
        parseWatcherConfig({
          ...validConfig(),
          targetNetwork: "Custom",
          customNetwork: { ...customNetwork, networkMagic },
        }),
      ).toThrow();
    }
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
